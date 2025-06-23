################################################################################
#  National + State NAIRU Estimation Script (AUS, NSW, VIC, QLD, SA, WA, TAS)
#  --------------------------------------------------------------------------
#  • Runs automatically only on ABS CPI or WPI release days
#  • Uses headline CPI (All-groups index), SA unemployment rate,
#    and WPI hourly rates excl. bonuses
#  • Estimates the same Stan NAIRU model for each region
#  • Saves one CSV per region plus a combined long panel + vintage copy
#
#  Dependencies:   readabs   readrba   rstan   dplyr   tidyr   purrr   glue
#                  zoo  lubridate  here  readr
################################################################################

# ---- libraries ------------------------------------------------------------
suppressPackageStartupMessages({
  library(readabs); library(readrba)
  library(dplyr);   library(tidyr);  library(purrr)
  library(zoo);     library(lubridate); library(readr); library(here)
  library(rstan);   library(glue)
})

# ---- 1. ABS quarterly release calendars ----------------------------------

## 1a. CPI – last Wed of Jan/Apr/Jul/Oct
cpi_dates <- as.Date(c(
  "2025-01-29","2025-04-30","2025-07-30","2025-10-29",
  "2026-01-28","2026-04-29","2026-07-29","2026-10-28",
  "2027-01-27","2027-04-28","2027-07-28","2027-10-27"
))

## 1b. WPI – 3rd Wed Feb / 2nd Wed May / 2nd Wed Aug / 3rd Wed Nov
wpi_dates <- as.Date(c(
  "2025-02-19","2025-05-14","2025-08-13","2025-11-19",
  "2026-02-18","2026-05-13","2026-08-12","2026-11-18",
  "2027-02-17","2027-05-12","2027-08-11","2027-11-17"
))

## 1c. Trigger
release_calendar <- c(cpi_dates, wpi_dates)

if (!Sys.Date() %in% release_calendar) {
  message(glue("⏩ {Sys.Date()} is not a CPI or WPI release day – skipping refresh."))
 # quit(save = "no")   # graceful exit for CI pipelines
}

# ---- 2. Constants: ABS series IDs ----------------------------------------

## CPI headline index (original)
cpi_ids <- c(
  AUS = "A2325846C",        # Weighted-average 8 capitals
  NSW = "A2325806K", VIC = "A2325811C", QLD = "A2325816R",
  SA  = "A2325821J", WA  = "A2325826V", TAS = "A2325831L"
)

## Unemployment rate, persons, SA
ur_ids  <- c(
  AUS = "A84423050A",
  NSW = "A84423270C", VIC = "A84423354L", QLD = "A84423284T",
  SA  = "A84423368A", WA  = "A84423326C", TAS = "A84423298F"
)

## WPI index, hourly rates excl. bonuses (original)
wpi_ids <- c(
  AUS = "A2603609J",
  NSW = "A2599619A", VIC = "A2608929K", QLD = "A2600949X",
  SA  = "A2610259A", WA  = "A2607599L", TAS = "A2602279K"
)

regions <- names(cpi_ids)   # "AUS" "NSW" … "TAS"

abs_5206 <- read_abs(series_id = c("A2304402X", "A2302915V"),
                     check_local = FALSE)

abs_6202 <- read_abs(series_id = c("A84423043C", "A84423047L"),
                     check_local = FALSE)

abs_6457 <- read_abs(series_id = "A2298279F",
                     check_local = FALSE)

abs_6345 <- read_abs(series_id = "A2713849C",
                     check_local = FALSE)

rba_g3 <- read_rba(series_id = c("GBONYLD")) 



# ---- 3. Paths -------------------------------------------------------------
root        <- Sys.getenv("GITHUB_WORKSPACE", unset = here::here())
out_dir     <- file.path(root, "output");      dir.create(out_dir, TRUE)
vintage_dir <- file.path(out_dir, "vintages"); dir.create(vintage_dir, TRUE)
setwd(root)

# ---- 4. Download ABS & RBA data ------------------------------------------
cpi_raw <- read_abs(series_id = cpi_ids)
ur_raw  <- read_abs(series_id = ur_ids)
wpi_raw <- read_abs(series_id = wpi_ids)


## ---- 4a.  EXTRA: PMCG import-price index ---------------------------------
pmcg_id  <- "A2298279F"                          # ABS 6457.0, Table 8
pmcg_raw <- read_abs(series_id = pmcg_id)

dl4pmcg <- pmcg_raw %>% 
  mutate(date = zoo::as.yearqtr(date),
         dl4pmcg = 100 * (log(value) - log(lag(value, 4)))) %>%
  select(date, dl4pmcg)

## ---- 4b.  Bond-market inflation expectations -----------------------------
bond_raw <- read_rba(series_id = "GBONYLD")          # 10-year indexed, daily
pie_bondq <- bond_raw %>%
  mutate(date = zoo::as.yearqtr(date),
         PIE_BONDQ = ( (1 + value/100)^(1/4) - 1 ) * 100 ) %>%   # quarterly %
  group_by(date) %>%                                            # average within qtr
  summarise(PIE_BONDQ = mean(PIE_BONDQ, na.rm = TRUE), .groups = "drop")

# ---- 4c.  RBA survey inflation expectations ------------------------------
pie_rbaq <- read_csv("inputs/PIE_RBAQ.CSV") %>%      # same path you used before
  rename(date = OBS) %>%
  mutate(date = zoo::as.yearqtr(date))

# forward-fill to 2025 Q1 (same logic you used earlier)
latest_q   <- max(pie_rbaq$date)
latest_end <- as.yearqtr("2025 Q1")

if (latest_q < latest_end) {
  new_dates <- seq(from = latest_q + 0.25, to = latest_end, by = 0.25)
  last_row  <- pie_rbaq %>% filter(date == latest_q)
  pie_rbaq  <- bind_rows(
                 pie_rbaq,
                 map_dfr(new_dates, ~ last_row %>% mutate(date = .x))
               )
}

# ---- 5. Helper to reshape -------------------------------------------------
make_wide <- function(df, id_vec, stub, diff = FALSE) {

  out <- df %>%
    filter(series_id %in% id_vec) %>%
    mutate(region = names(id_vec)[match(series_id, id_vec)],
           date   = zoo::as.yearqtr(date)) %>%
    group_by(date, region) %>%                 # ← NEW: collapse duplicates
    summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = region, values_from = value) %>%
    arrange(date)

  if (diff) {
    out <- out %>%
      mutate(across(-date, ~ 100 * (log(.x) - log(lag(.x)))))
  }

  out %>% rename_with(~ paste0(stub, "_", .x), -date)
}



cpi_dln <- make_wide(cpi_raw, cpi_ids, "DLCPI", diff = TRUE)   # inflation
ur_sa   <- make_wide(ur_raw , ur_ids , "UR",   diff = FALSE)
wpi_dln <- make_wide(wpi_raw, wpi_ids, "DLWPI", diff = TRUE)  # wage growth

# ---- 6. Merge & sample window --------------------------------------------
panel <- list(
  wpi_dln,
  dl4pmcg,
  ur_sa,
  cpi_dln,
  pie_rbaq
) %>%
  purrr::reduce(dplyr::left_join, by = "date") %>%
  filter(date >= "1997 Q3") %>%
  mutate(dummy1 = ifelse(date >= "2021Q3" & date <= "2023Q1", 1, 0),
         dummy2 = ifelse(date >= "2022Q1" & date <= "2022Q4", 1, 0),
         dummy3 = ifelse(date == "2020Q2", 1, 0),
         dummy4 = ifelse(date == "2020Q3", 1, 0))




# ---- 7. Long format for looping ------------------------------------------
panel_long <- panel %>%
  pivot_longer(-date,
               names_to = c(".value","region"),
               names_pattern = "(DLPI|UR|DLWPI)_(.*)") %>%
  filter(region %in% regions) %>%
  arrange(region, date)

readr::write_csv(panel_long, file.path(out_dir, "est_data_state_long.csv"))

# ---- 8. Stan NAIRU estimation loop ---------------------------------------
options(mc.cores = parallel::detectCores())
Sys.setenv(MAKEFLAGS = "-j4")

compiled <- stan_model(file.path("stan", "NAIRU_baseline.stan"))

all_summ <- vector("list", length(regions)); names(all_summ) <- regions

print(panel)

for (r in regions) {

 col_order <- c(
    paste0("DLWPI_",   r),
       "dl4pmcg",
    paste0("UR_",     r),
    paste0("DLCPI_",  r),

    "PIE_RBAQ",
    "dummy1",
    "dummy2",
    "dummy3",
    "dummy4"
  )

  df_r <- panel %>%
    select(date, all_of(col_order)) %>%
    drop_na()           # only drop rows with missing in these eight

print(df_r)
  
  Y <- as.matrix(df_r %>% select(-date))  # now a clean T×8 matrix

  ## 4.  feed Stan
  fit <- sampling(
    compiled,
    data = list(T = nrow(Y), J = ncol(Y), Y = Y),
    chains = 4, iter = 100,
    control = list(max_treedepth = 15)
  )

  ## 5.  summarise NAIRU draws
  summ <- as.data.frame(fit) %>%
    select(contains("NAIRU")) %>%
    pivot_longer(everything(), names_to = "draw", values_to = "value") %>%
    summarise(median  = median(value),
              lower90 = quantile(value, .05),
              upper90 = quantile(value, .95)) %>%
    mutate(region = r)

  all_summ[[r]] <- summ
  readr::write_csv(summ, file.path(out_dir, glue("NAIRU_{tolower(r)}.csv")))
}


# ---- 9. Combine & save vintage -------------------------------------------
nairu_all <- bind_rows(all_summ)
readr::write_csv(nairu_all, file.path(out_dir, "NAIRU_all_regions.csv"))

run_stamp   <- format(Sys.Date(), "%Y-%m-%d")
vint_file   <- file.path(vintage_dir, glue("{run_stamp}.csv"))
if (!file.exists(vint_file))
  file.copy(file.path(out_dir, "NAIRU_all_regions.csv"), vint_file)

message("✔  Finished – outputs written to: ", out_dir)
