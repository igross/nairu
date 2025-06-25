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
         
sampled_model_baseline <- sampling(compiled_model, data = data_list, chains=6,iter = 1000, control = list(max_treedepth = 15))




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

# ─────────────────────────────────────────────────────────────────────────────
#  REBUILD BOTH DECOMPOSITIONS (π & ULC) FROM POSTERIOR MEDIANS
#  • Assumes `sampled_model_baseline` (rstan fit) and `est_data`
#    (date + 9-column design matrix) already exist.
#  • Writes two tidy CSVs:
#        infl_pi_decomp.csv   – inflation components
#        ulc_decomp.csv       – ULC components
# ─────────────────────────────────────────────────────────────────────────────
library(rstan);   library(dplyr);   library(tibble);  library(readr)

# ---------------------------------------------------------------------------
# 0.  Housekeeping
# ---------------------------------------------------------------------------
Y_mat  <- as.matrix(est_data[, -1])     # remove date col
dates  <- est_data$date                 # zoo::yearqtr vector
Tn     <- nrow(Y_mat)

# demeaned series identical to Stan's transformed data
Y2_demeaned <- Y_mat[, 2] - mean(Y_mat[, 2])
Y1_demeaned <- Y_mat[, 1] - mean(Y_mat[, 1])

# ---------------------------------------------------------------------------
# 1.  Extract posterior medians we need
# ---------------------------------------------------------------------------
draws <- rstan::extract(sampled_model_baseline)

# NAIRU
nairu_med <- apply(draws$NAIRU, 2, median)

# π-side coefficients
delta_pt_0  <- median(draws$delta_pt_0)
delta_pt_l  <- apply(draws$delta_pt_lag, 2, median)
phi_pt_0    <- median(draws$phi_pt_0)
phi_pt_l    <- apply(draws$phi_pt_lag, 2, median)
gamma_pt_0  <- median(draws$gamma_pt_0)
gamma_pt_l  <- apply(draws$gamma_pt_lag, 2, median)
lambda_pt_0 <- median(draws$lambda_pt_0)
lambda_pt_l <- apply(draws$lambda_pt_lag, 2, median)
alpha_pt_0  <- median(draws$alpha_pt_0)
alpha_pt_l  <- apply(draws$alpha_pt_lag, 2, median)
xi_pt_med   <- apply(draws$xi_pt, 2, median)

# ULC-side coefficients
# ── pull ULC-side parameters that *do* exist -------------------------------
delta_pu_0  <- median(draws$delta_pu_0)                # scalar
delta_pu_l  <- apply(draws$delta_pu_lag, 2, median)    # length 2
gamma_pu_0  <- median(draws$gamma_pu_0)
gamma_pu_l  <- apply(draws$gamma_pu_lag, 2, median)
lambda_pu_0 <- median(draws$lambda_pu_0)
lambda_pu_l <- apply(draws$lambda_pu_lag, 2, median)
xi_pu_med   <- apply(draws$xi_pu, 2, median)           # length 2


# initialise only what we will keep
pi_exp <- pi_ulc <- pi_ugap <- pi_mom <- pi_imp <- pi_resid <- pi_dum <- rep(NA, Tn)
pu_dum <- pu_ugap <- pu_mom <- pu_exp <- pu_resid <- rep(NA, Tn)

for (t in 6:Tn) {

  ## ── Inflation (π) components (unchanged) ────────────────────────────────
  pi_exp[t] <- delta_pt_0 * Y_mat[t, 5] +
               sum(delta_pt_l * Y_mat[t - (1:3), 5])

  pi_imp[t] <- alpha_pt_0 * (Y2_demeaned[t-1] - Y2_demeaned[t-2]) +
               sum(alpha_pt_l *
                   (Y2_demeaned[t-(2:4)] - Y2_demeaned[t-(3:5)]))

  pi_ugap[t] <- gamma_pt_0 * ((Y_mat[t,3] - nairu_med[t]) / Y_mat[t,3]) +
                sum(gamma_pt_l *
                    ((Y_mat[t-(1:3),3] - nairu_med[t-(1:3)]) /
                      Y_mat[t-(1:3),3]))

  pi_mom[t]  <- lambda_pt_0 * (Y_mat[t-1,3] - Y_mat[t-2,3]) / Y_mat[t,3] +
                sum(lambda_pt_l *
                    ((Y_mat[t-(2:4),3] - Y_mat[t-(3:5),3]) /
                      Y_mat[t-(1:3),3]))

  pi_ulc[t]  <- phi_pt_0 * Y1_demeaned[t-1] +
                sum(phi_pt_l * Y1_demeaned[t-(2:4)])

  pi_dum[t]  <- xi_pt_med[1]*Y_mat[t,6] + xi_pt_med[2]*Y_mat[t,7]

  deterministic_pi <- pi_exp[t] + pi_imp[t] + pi_ugap[t] +
                      pi_mom[t] + pi_ulc[t] + pi_dum[t]

  pi_resid[t] <- Y_mat[t,4] - deterministic_pi


  ## ── ULC components (NO lagged-π term) ───────────────────────────────────
  pu_dum[t] <- xi_pu_med[1]*Y_mat[t,8] + xi_pu_med[2]*Y_mat[t,9]

  pu_ugap[t] <- gamma_pu_0 * (1 - nairu_med[t]/Y_mat[t,3]) +
                sum(gamma_pu_l * (1 - nairu_med[t-(1:2)] / Y_mat[t-(1:2),3]))

  pu_mom[t]  <- lambda_pu_0 * (Y_mat[t-1,3] - Y_mat[t-2,3]) / Y_mat[t,3] +
                sum(lambda_pu_l *
                    ((Y_mat[t-(2:3),3] - Y_mat[t-(3:4),3]) /
                      Y_mat[t-(1:2),3]))

  pu_exp[t]  <- delta_pu_0 * Y_mat[t,5] +
                sum(delta_pu_l * Y_mat[t-(1:2),5])

  deterministic_pu <- pu_dum[t] + pu_ugap[t] + pu_mom[t] + pu_exp[t]
  pu_resid[t]      <- Y_mat[t,1] - deterministic_pu
}

# build tidy data frames ------------------------------------------------------
infl_pi_decomp <- tibble(
  date_qtr      = dates,
  expectations  = pi_exp,
  import_price  = pi_imp,
  unemp_gap     = pi_ugap,
  momentum      = pi_mom,
  ulc_demeaned  = pi_ulc,
  dummies = pi_dum,
  residuals     = pi_resid
)

ulc_decomp <- tibble(
  date_qtr      = dates,
  dummies       = pu_dum,
  unemp_gap     = pu_ugap,
  momentum      = pu_mom,
  expectations  = pu_exp,
  residuals     = pu_resid
)

write_csv(infl_pi_decomp, file.path(out_dir, "infl_pi_decomp.csv"))
write_csv(ulc_decomp,       file.path(out_dir, "ulc_decomp.csv"))
message("✔  Saved updated CSVs without lagged-π term in ULC block")

