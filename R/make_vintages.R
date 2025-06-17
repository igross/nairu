# make_vintages.R -----------------------------------------------------------
library(tidyverse);           library(lubridate);    library(zoo)
library(readabs);             library(readrba);      library(rstan)
library(glue);                library(here);         library(purrr)

# ---------------------------------------------------------------------------
# 1.  Build the release-day calendar  (last Wed Jan/Apr/Jul/Oct; first Wed Mar/Jun/Sep/Dec)
# ---------------------------------------------------------------------------
this_year   <- year(Sys.Date())
start_year  <- this_year - 10                      # 10-year window
years_vec   <- start_year:this_year

last_wed  <- function(y, m) lubridate::rollback(ymd(sprintf("%04d-%02d-01", y, m)) + months(1),
                                               roll_to_first = TRUE) - 
                              days(wday(rollback(ymd(sprintf("%04d-%02d-01", y, m)) + months(1),
                                                   roll_to_first = TRUE)) %% 7)
first_wed <- function(y, m) {
  d <- ymd(sprintf("%04d-%02d-01", y, m))
  d + days((3 - wday(d) + 7) %% 7)   # 3 = Wednesday
}

cpi_dates <- map2(years_vec, rep(c(1,4,7,10), each = length(years_vec)),
                  last_wed)  |> unlist() |> as_date()
na_dates  <- map2(years_vec, rep(c(3,6,9,12), each = length(years_vec)),
                  first_wed) |> unlist() |> as_date()

release_calendar <- sort(c(cpi_dates, na_dates)) |>
                    keep(~ .x < Sys.Date())      # only past dates

# ---------------------------------------------------------------------------
# 2.  Pull the full (current) datasets ONCE
# ---------------------------------------------------------------------------
abs_5206 <- read_abs(series_id = c("A2304402X","A2302915V"))
abs_6202 <- read_abs(series_id = c("A84423043C","A84423047L"))
abs_6457 <- read_abs(series_id = c("A2298279F"))
abs_6345 <- read_abs(series_id = c("A2713849C"))
rba_g3   <- read_rba(series_id = "GBONYLD")
rba_g1   <- read_abs(series_id = c("A3604510W","A2330530C","A2330575J"))

pie_rbaq <- read_csv(file.path("inputs","PIE_RBAQ.CSV")) |>
            rename(date = OBS) |>
            mutate(date = as.yearqtr(date))

# ---- helper that recreates est_data up to a cutoff quarter ---------------
make_est_data <- function(cutoff_qtr) {

  R_5206 <- abs_5206 |>
    filter(series_id %in% c("A2304402X","A2302915V"),
           date <= as.Date(cutoff_qtr)) |>
    mutate(date = as.yearqtr(date)) |>
    select(date, series_id, value) |> distinct() |>
    pivot_wider(names_from = series_id, values_from = value) |>
    mutate(NULC   = A2302915V / A2304402X,
           DLNULC = 100 * (log(NULC) - log(lag(NULC)))) |>
    select(date, DLNULC)

  R_6345 <- abs_6345 |>
    filter(series_id=="A2713849C", date <= as.Date(cutoff_qtr)) |>
    mutate(date = as.yearqtr(date)) |>
    distinct(date,.keep_all=TRUE) |>
    pivot_wider(names_from = series_id, values_from = value) |>
    mutate(DLWPI = 100*(log(A2713849C)-log(lag(A2713849C,1)))) |>
    select(date, DLWPI)

  R_6457 <- abs_6457 |>
    filter(series_id=="A2298279F", date <= as.Date(cutoff_qtr)) |>
    mutate(date = as.yearqtr(date),
           dl4pmcg = 100*(log(value)-log(lag(value,4)))) |>
    select(date, dl4pmcg)

  R_6202 <- abs_6202 |>
    filter(series_id %in% c("A84423043C","A84423047L"),
           date <= as.Date(cutoff_qtr)) |>
    select(date, series_id, value) |>
    distinct() |> pivot_wider(names_from = series_id, values_from = value) |>
    group_by(date = floor_date(date, "quarter")) |>
    summarise(A84423043C = mean(A84423043C, na.rm=TRUE),
              A84423047L = mean(A84423047L, na.rm=TRUE)) |>
    mutate(date = as.yearqtr(date),
           LUR  = 100*(1 - A84423043C/A84423047L)) |>
    select(date, LUR)

  R_g1 <- rba_g1 |>
    filter(series_id=="A3604510W", date <= as.Date(cutoff_qtr)) |>
    mutate(date = as.yearqtr(date), DLPTM = value) |>
    select(date, DLPTM)

  R_g3 <- rba_g3 |>
    filter(series_id=="GBONYLD", date <= as.Date(cutoff_qtr)) |>
    mutate(date = as.yearqtr(date),
           pie_bondq = ((1+value/100)^(1/4)-1)*100) |>
    select(date, pie_bondq)

  pie_cut <- pie_rbaq |>
    filter(date <= cutoff_qtr)

  # ---- merge everything ---------------------------------------------------
  data_set <- list(R_5206,R_6457,R_6202,R_g1,pie_cut) |>
    reduce(full_join, by="date") |>
    arrange(date) |>
    filter(!is.na(date))

  est_data <- data_set |>
    filter(date > as.yearqtr("1997 Q3"), date <= cutoff_qtr) |>
    mutate(dummy1 = ifelse(date >= as.yearqtr("2021 Q3") & date <= as.yearqtr("2023 Q1"),1,0),
           dummy2 = ifelse(date >= as.yearqtr("2022 Q1") & date <= as.yearqtr("2022 Q4"),1,0),
           dummy3 = ifelse(date == as.yearqtr("2020 Q2"),1,0),
           dummy4 = ifelse(date == as.yearqtr("2020 Q3"),1,0))

  return(est_data)
}

# ---------------------------------------------------------------------------
# 3.  Prepare Stan model once
# ---------------------------------------------------------------------------
options(mc.cores = max(1, parallel::detectCores() - 1))
Sys.setenv(MAKEFLAGS = paste0("-j", max(1, parallel::detectCores() - 1)))

compiled_model <- stan_model(file = file.path("stan","NAIRU_baseline.stan"))

# ---------------------------------------------------------------------------
# 4.  Loop through every release date
# ---------------------------------------------------------------------------
root        <- here::here()
out_dir     <- file.path(root,"output"); dir.create(out_dir, showWarnings = FALSE)
vintage_dir <- file.path(out_dir,"vintages"); dir.create(vintage_dir, showWarnings = FALSE)

run_one_vintage <- function(rel_date) {

  # which quarter is fully observed on rel_date?
  cutoff_qtr <- as.yearqtr(rel_date) - 0.25

  message(glue("▶  {rel_date}: computing vintage using data up to {cutoff_qtr}…"))

  est_data <- make_est_data(cutoff_qtr)
  stan_Y   <- as.matrix(est_data[,-1])        # drop date column

  data_list <- list(T = nrow(stan_Y),
                    J = ncol(stan_Y),
                    Y = stan_Y)

  fit <- sampling(compiled_model,
                  data    = data_list,
                  chains  = 4,
                  iter    = 1000,
                  control = list(max_treedepth = 12),
                  refresh = 0)

  summarised <- as.data.frame(fit) |>
    select(contains("NAIRU")) |>
    pivot_longer(everything()) |>
    group_by(name) |>
    summarise(median = median(value),
              lower5 = quantile(value,0.05),
              upper95= quantile(value,0.95),
              .groups="drop") |>
    mutate(vintage = rel_date)

  out_file <- file.path(vintage_dir,
                        glue("{format(rel_date,'%Y-%m-%d')}.csv"))
  write_csv(summarised, out_file)

  message(glue("✔  saved → {basename(out_file)}"))
}

# walk all release dates (newest → oldest) ----------------------------
release_calendar |>
  keep(~ .x >= min(release_calendar)) |>
  walk(run_one_vintage)

message("🏁  All done.")
