# ---- libraries ------------------------------------------------------------
library(ggthemes); library(reshape2); library(readabs);  library(dplyr)
library(ggplot2);   library(zoo);      library(rstan);   library(readrba)
library(lubridate); library(readr);    library(here)


# ---- ABS quarterly release timetable -------------------------------------------------
cpi_dates <- as.Date(c(           # CPI  – last Wed of Jan/Apr/Jul/Oct
  "2025-01-29","2025-04-30","2025-07-30","2025-10-29",
  "2026-01-28","2026-04-29","2026-07-29","2026-10-28",
  "2027-01-27","2027-04-28","2027-07-28","2027-10-27"
))

na_dates  <- as.Date(c(           # Nat. Accounts – first Wed of Mar/Jun/Sep/Dec
  "2025-03-05","2025-06-04","2025-09-03","2025-12-03",
  "2026-03-04","2026-06-03","2026-09-02","2026-12-02",
  "2027-03-03","2027-06-02","2027-09-01","2027-12-01"
))

release_calendar <- c(cpi_dates, na_dates)

# ---- Short-circuit if today isn’t a release day --------------------------------------
if (!Sys.Date() %in% release_calendar) {
  message(
    glue::glue("⏩ {Sys.Date()} is not an ABS CPI/National-Accounts release day – skipping refresh.")
  )
 # quit(save = "no")   # graceful, zero-exit termination
}



options(mc.cores = parallel::detectCores())
Sys.setenv(MAKEFLAGS = "-j4")          # speed up C++ build

# ---- paths ----------------------------------------------------------------
root    <- Sys.getenv("GITHUB_WORKSPACE", unset = here::here())
out_dir <- file.path(root, "output")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
setwd(root)

vintage_dir <- file.path(out_dir, "vintages")
dir.create(vintage_dir, showWarnings = FALSE, recursive = TRUE)



#---------------------------------------------------------------------------------------------------------
# Download Most Recent ABS and RBA Data
#---------------------------------------------------------------------------------------------------------
# Import Data from ABS Website
abs_5206 <- read_abs(series_id = c("A2304402X", "A2302915V"))
abs_6202 <- read_abs(series_id = c("A84423043C", "A84423047L"))
abs_6457 <- read_abs(series_id = c("A2298279F"))
abs_6345 <- read_abs(series_id = c("A2713849C"))
rba_g3 <- read_rba(series_id = c("GBONYLD")) 
#rba_g1 <- read_rba(series_id = c("GCPIOCPMTMQP","GCPITIQP","GCPINTIQP")) 
rba_g1 <- read_abs(series_id = c("A3604510W","A2330530C","A2330575J")) 

#---------------------------------------------------------------------------------------------------------
# Cleanup ABS Spreadsheets
#---------------------------------------------------------------------------------------------------------
# 5206.0 Australian National Accounts
R_5206 <- abs_5206 %>%
  filter(series_id %in% c("A2304402X", "A2302915V")) %>%
  mutate(date = zoo::as.yearqtr(date)) %>%
  dplyr::select(date, series_id, value) %>%
  distinct(date, series_id, .keep_all = TRUE) %>%
  dcast(date ~ series_id) %>%
  mutate(NULC = A2302915V / A2304402X,
         DLNULC = 100 * (log(NULC) - log(lag(NULC, 1)))) %>%
  select(date, DLNULC)

# 6345.0 WPI Data
R_6345 <- abs_6345 %>%
  filter(series_id == "A2713849C") %>%
  mutate(date = zoo::as.yearqtr(date)) %>%
  distinct(date, series_id, .keep_all = TRUE) %>%
  dcast(date ~ series_id) %>%
  mutate(DLWPI = 100 * (log(A2713849C) - log(lag(A2713849C, 1)))) %>%
  select(date, DLWPI)

# 6457.0 International Trade Price Indexes
R_6457 <- abs_6457 %>%
  filter(series_id == "A2298279F") %>%
  mutate(date = zoo::as.yearqtr(date),
         dl4pmcg = 100 * (log(value) - log(lag(value, 4)))) %>%
  select(date, dl4pmcg)

# 6202.0 Labour Force Data
R_6202 <- abs_6202 %>%
  filter(series_id %in% c("A84423043C", "A84423047L")) %>%
  select(date, series_id, value) %>%
  distinct(date, series_id, .keep_all = TRUE) %>%
  dcast(date ~ series_id) %>%
  group_by(date = floor_date(date, "quarter")) %>%
  summarize(A84423043C = mean(A84423043C, na.rm = TRUE),
            A84423047L = mean(A84423047L, na.rm = TRUE)) %>%
  mutate(date = zoo::as.yearqtr(date),
         LUR = 100 * (1 - A84423043C / A84423047L)) %>%
  select(date, LUR)

R_g1 <- rba_g1 %>%
  filter(series_id %in% c("A3604510W")) %>%
  mutate(date = zoo::as.yearqtr(date)) %>%
  rename(DLPTM = value) %>%
  select(date, DLPTM)


#Bond-market inflation expectations
R_g3 <- rba_g3 %>%
  filter(series_id %in% c("GBONYLD")) %>%
  mutate(date = zoo::as.yearqtr(date)) %>%
  mutate(pie_bondq = ((1+value/100)^(1/4)-1)*100) %>%
  select(date, pie_bondq)


#RBA inflation expectationsMore actions
myfile <- file.path("inputs", "PIE_RBAQ.CSV")
pie_rbaq <- read_csv(myfile)
pie_rbaq <- pie_rbaq %>%
  rename(date=OBS) %>%
  mutate(date = zoo::as.yearqtr(date))


  latest_date_df1 <- max(R_5206$date)
  latest_date_df2 <- max(R_g1$date)

  # Check if the latest date in df2 is one day less than in df1
  if (latest_date_df2 > latest_date_df1) {
    # Get the most recent point from df2
    recent_date <- R_g1 %>% filter(date == latest_date_df2) %>% select(date) 
    recent_point <- R_5206 %>% filter(date == latest_date_df1) %>% select(DLNULC) 

    combined_df <- merge(recent_date, recent_point, all = TRUE)

    # Append the recent point to df2
    R_5206 <- bind_rows(R_5206, combined_df)

}


# ── Extend pie_rbaq forward to latest_date_df2 ────────────────────────────────
latest_pie_date <- max(pie_rbaq$date)

if (latest_date_df2 > latest_pie_date) {

  # Quarters we still need (as yearqtr objects)
  new_dates <- seq(from = latest_pie_date + 0.25,  # next quarter
                   to   = latest_date_df2,
                   by   = 0.25)

  # Grab the last observed row (all columns) and duplicate for each new date
  last_row  <- pie_rbaq %>% filter(date == latest_pie_date)
  new_rows  <- purrr::map_dfr(new_dates, ~ last_row %>% mutate(date = .x))

  # Append and keep chronological order
  pie_rbaq  <- bind_rows(pie_rbaq, new_rows) %>% arrange(date)
}




data_set <- list(R_5206, R_6457, R_6202, R_g1, pie_rbaq) %>%
  Reduce(function(dtf1,dtf2) left_join(dtf1,dtf2,by="date"), .)

#data_set$pie_bondq <- replace(data_set$pie_bondq,is.na(data_set$pie_bondq),2.5/4)


data_set <- data_set %>%
  filter(!is.na(date))

# Pick Sample
est_data <- data_set %>%
  filter(date > "1997q3" & date < "2025q1") %>%
  mutate(dummy1 = ifelse(date >= "2021Q3" & date <= "2023Q1", 1, 0),
         dummy2 = ifelse(date >= "2022Q1" & date <= "2022Q4", 1, 0),
         dummy3 = ifelse(date == "2020Q2", 1, 0),
         dummy4 = ifelse(date == "2020Q3", 1, 0))

print(out_dir)
         
csv_path <- file.path(out_dir, "est_data.csv")
readr::write_csv(est_data, csv_path)

myfile <- file.path(out_dir, "est_data.csv")
test <- read_csv(myfile)
         
# Subset Data for Stan
stan_data <- as.matrix(est_data[ , -1])   # drop the yearqtr column

data_list <- list(
  T = nrow(stan_data),
  J = ncol(stan_data),
  Y = stan_data
)

# Compile The Model
compiled_model <- stan_model(file = file.path("stan", "NAIRU_baseline.stan"))
         
sampled_model_baseline <- sampling(compiled_model, data = data_list, chains=10,iter = 200, control = list(max_treedepth = 15))




summarised_state_baseline <- as.data.frame(sampled_model_baseline) %>% 
  select(contains("NAIRU")) %>%
  melt() %>% 
  group_by(variable) %>% 
  summarise(median = median(value),
            lowera = quantile(value, 0.05),
            uppera = quantile(value, 0.95),
            lowerb = quantile(value, 0.15),
            upperb = quantile(value, 0.85)) %>%
  mutate(date = as.Date(est_data$date)) %>%
  mutate(date = zoo::as.yearqtr(date)) %>%
  mutate(LUR = est_data$LUR)  %>%
  mutate(dl4pmcg = est_data$dl4pmcg)

print(summarised_state_baseline)

         
csv_path <- file.path(out_dir, "NAIRU_baseline.csv")
readr::write_csv(summarised_state_baseline, csv_path)

         # ---- sanity check ---------------------------------------------------------
if (file.exists(csv_path)) {
  message("✔  File written: ", csv_path)
} else {
  stop("✖  Failed to write: ", csv_path)
}

# optional: list everything so the workflow log shows it
message("Contents of output/ after write:")
print(list.files(out_dir, full.names = TRUE))

         # ---- save vintage -------------------------------------------------
run_stamp   <- format(Sys.Date(), "%Y-%m-%d")      # e.g. "2025-05-20"
vintage_out <- file.path(vintage_dir, paste0(run_stamp, ".csv"))

if (!file.exists(vintage_out)) {
  file.copy(csv_path, vintage_out)
  message("✔  Vintage saved: ", vintage_out)
} else {
  message("ℹ︎  Vintage for ", run_stamp, " already exists; not overwritten.")
}







