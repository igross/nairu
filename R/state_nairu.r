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
#  quit(save = "no")   # graceful exit for CI pipelines
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



cpi_dln <- make_wide(cpi_raw, cpi_ids, "DLPI", diff = TRUE)   # inflation
ur_sa   <- make_wide(ur_raw , ur_ids , "UR",   diff = FALSE)
wpi_dln <- make_wide(wpi_raw, wpi_ids, "DLWPI", diff = TRUE)  # wage growth

# ---- 6. Merge & sample window --------------------------------------------
panel <- reduce(
           list(cpi_dln, ur_sa, wpi_dln, dl4pmcg, pie_bondq),   # + bond qtrs
           left_join, by = "date") %>%
  filter(date >= "1997 Q3", date <= "2025 Q1") %>%
  mutate(
    dummy1 = ifelse(date >= as.yearqtr("2021 Q3") & date <= as.yearqtr("2023 Q1"), 1, 0),
    dummy2 = ifelse(date >= as.yearqtr("2022 Q1") & date <= as.yearqtr("2022 Q4"), 1, 0)
  )





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

for (r in regions) {

  df_r <- panel %>% 
    transmute(
      date,
      DLPI      = .[[paste0("DLPI_",  r)]],
      UR        = .[[paste0("UR_",    r)]],
      DLWPI     = .[[paste0("DLWPI_", r)]],
      DL4PMCG,                        # ← col 4
      PIE_BONDQ,                      # ← col 5  (new)
      dummy1,                         # ← col 6
      dummy2                          # ← col 7
    ) %>% 
    drop_na()

  ymat <- as.matrix(df_r %>% select(-date))   # 7-column matrix

  fit <- sampling(
    compiled,
    data = list(T = nrow(ymat),
                J = ncol(ymat),   # = 7
                Y = ymat),
    chains = 4, iter = 100,
    control = list(max_treedepth = 15)
  )

 

  # summarise draws ---------------------------------------------------------
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
