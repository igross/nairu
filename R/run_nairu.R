# ---- libraries ------------------------------------------------------------
library(ggthemes); library(reshape2); library(readabs);  library(dplyr)
library(ggplot2);   library(zoo);      library(rstan);   library(readrba)
library(lubridate); library(readr);    library(here);     library(stringr); library(tidyr)


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
override_flag <- tolower(Sys.getenv("FORCE_REFRESH", unset = ""))
refresh_override <- override_flag %in% c("1", "true", "yes", "force")

if (!refresh_override && !Sys.Date() %in% release_calendar) {
  message(
    glue::glue("⏩ {Sys.Date()} is not an ABS CPI/National-Accounts release day – skipping refresh."),
    glue::glue(" (set FORCE_REFRESH=true to override)" )
  )
 #  quit(save = "no")   # graceful, zero-exit termination
}

if (refresh_override && !Sys.Date() %in% release_calendar) {
  message(
    glue::glue("🔁 FORCE_REFRESH override active – continuing refresh even though {Sys.Date()} is not a scheduled release day.")
  )
}



options(mc.cores = parallel::detectCores())
Sys.setenv(MAKEFLAGS = "-j4")          # speed up C++ build

# ---- paths ----------------------------------------------------------------
root    <- Sys.getenv("GITHUB_WORKSPACE", unset = here::here())
out_dir <- file.path(root, "docs")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
setwd(root)

vintage_dir <- file.path(out_dir, "vintages")
dir.create(vintage_dir, showWarnings = FALSE, recursive = TRUE)



#---------------------------------------------------------------------------------------------------------
# Download Most Recent ABS and RBA Data
#---------------------------------------------------------------------------------------------------------
# Import Data from ABS Website
abs_5206 <- read_abs(series_id = c("A2304402X", "A2302915V"))
abs_6202 <- read_abs(series_id = c("A84423050A"))          # ABS 6202.0 Table 1 – unemployment rate (SA)
abs_6457 <- read_abs(series_id = c("A2298279F"))
abs_6345 <- read_abs(series_id = c("A2713849C"))
rba_g3 <- read_rba(series_id = c("GBONYLD"))
#rba_g1 <- read_rba(series_id = c("GCPIOCPMTMQP","GCPITIQP","GCPINTIQP"))
abs_trimmed_mean <- read_abs(series_id = c("A3604510W"))   # ABS 6401.0 Table 7 – trimmed mean CPI (q/q %)

# print(abs_trimmed_mean, n = Inf, width = Inf)

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
  filter(series_id == "A84423050A") %>%
  mutate(date = zoo::as.yearqtr(date)) %>%
  group_by(date) %>%
  summarise(LUR = mean(value, na.rm = TRUE), .groups = "drop")

R_trimmed_mean <- abs_trimmed_mean %>%
  filter(series_id == "A3604510W") %>%
  mutate(date = zoo::as.yearqtr(date)) %>%
  rename(DLPTM = value) %>%
  select(date, DLPTM)


#Bond-market inflation expectations
R_g3 <- rba_g3 %>%
  filter(series_id %in% c("GBONYLD")) %>%
  mutate(date = zoo::as.yearqtr(date)) %>%
  mutate(pie_bondq = ((1+value/100)^(1/4)-1)*100) %>%
  select(date, pie_bondq)


# RBA inflation expectations (quarterly)
myfile <- file.path("inputs", "PIE_RBAQ.CSV")
pie_rbaq <- read_csv(myfile)
pie_rbaq <- pie_rbaq %>%
  rename(date=OBS) %>%
  mutate(date = zoo::as.yearqtr(date))

transformed_inputs <- list(
  R_5206,
  R_6345,
  R_6457,
  R_6202,
  R_trimmed_mean,
  pie_rbaq
) %>%
  Reduce(function(dtf1, dtf2) full_join(dtf1, dtf2, by = "date"), .) %>%
  arrange(date)

transformed_plot_data <- transformed_inputs %>%
  tidyr::pivot_longer(-date, names_to = "series", values_to = "value") %>%
  filter(!is.na(value)) %>%
  mutate(date_plot = as.Date(date))

transformed_plot <- ggplot(
  transformed_plot_data,
  aes(x = date_plot, y = value, colour = series)
) +
  geom_line(linewidth = 0.6, alpha = 0.9) +
  labs(
    title = "Transformed macroeconomic series",
    x     = "Date",
    y     = "Value",
    colour= "Series"
  ) +
  theme_minimal(base_size = 11)

plot_path <- file.path(out_dir, "transformed_inputs.png")

ggsave(
  plot_path,
  transformed_plot,
  width = 10,
  height = 6,
  dpi = 300
)

message(glue::glue("💾 Saved transformed series plot to {plot_path}"))


# ── Extend pie_rbaq forward to latest_trimmed_mean_date ───────────────────────
latest_trimmed_mean_date <- max(R_trimmed_mean$date)
latest_pie_date <- max(pie_rbaq$date)

if (latest_trimmed_mean_date > latest_pie_date) {

  # Quarters we still need (as yearqtr objects)
  new_dates <- seq(from = latest_pie_date + 0.25,  # next quarter
                   to   = latest_trimmed_mean_date,
                   by   = 0.25)

  # Grab the last observed row (all columns) and duplicate for each new date
  last_row  <- pie_rbaq %>% filter(date == latest_pie_date)
  new_rows  <- purrr::map_dfr(new_dates, ~ last_row %>% mutate(date = .x))

  # Append and keep chronological order
  pie_rbaq  <- bind_rows(pie_rbaq, new_rows) %>% arrange(date)
}




data_set <- list(R_5206, R_6457, R_6202, R_trimmed_mean, pie_rbaq, R_6345) %>%
  Reduce(function(dtf1, dtf2) full_join(dtf1, dtf2, by = "date"), .)

# print(as_tibble(data_set), n = Inf, width = Inf)

#data_set$pie_bondq <- replace(data_set$pie_bondq,is.na(data_set$pie_bondq),2.5/4)


data_set <- data_set %>%
  filter(!is.na(date)) %>%
  arrange(date)

filled_data_set <- data_set %>%
  mutate(across(-date, ~ na.locf(.x, na.rm = FALSE)))

latest_data_date <- max(data_set$date, na.rm = TRUE)
wage_columns <- c("DLNULC", "DLWPI")

for (w_col in wage_columns) {
  if (w_col %in% names(filled_data_set)) {
    reset_mask <- is.na(data_set[[w_col]]) & data_set$date == latest_data_date
    filled_data_set[[w_col]][reset_mask] <- NA_real_
  }
}

data_set <- filled_data_set

data_set <- data_set %>%
  mutate(dl4pmcg = ifelse(is.na(dl4pmcg) & !is.na(DLPTM), 
                          mean(dl4pmcg, na.rm = TRUE), 
                          dl4pmcg))
         
# Pick Sample
est_data <- data_set %>%
  filter(date > "1997q3" ) %>%
  mutate(dummy1 = ifelse(date >= "2021Q3" & date <= "2023Q1", 1, 0),
         dummy2 = ifelse(date >= "2022Q1" & date <= "2022Q4", 1, 0),
         dummy3 = ifelse(date == "2020Q2", 1, 0),
         dummy4 = ifelse(date == "2020Q3", 1, 0))

         

# print(as_tibble(est_data), n = Inf, width = Inf)
         
csv_path <- file.path(out_dir, "est_data.csv")
readr::write_csv(est_data, csv_path)

myfile <- file.path(out_dir, "est_data.csv")
test <- read_csv(myfile)
         
# Subset Data for Stan
assert_complete_nonwage <- function(design_df, wage_col, context_label) {
  non_wage_cols <- setdiff(names(design_df), c("date", wage_col))
  if (length(non_wage_cols) == 0) {
    return(invisible(NULL))
  }

  completeness_matrix <- design_df %>%
    select(all_of(non_wage_cols)) %>%
    as.data.frame()

  missing_counts <- vapply(completeness_matrix, function(col) sum(is.na(col)), numeric(1))
  missing_cols <- names(missing_counts[missing_counts > 0])

  if (length(missing_cols) > 0) {
    stop(
      glue::glue(
        "{context_label}: missing values detected in {paste(missing_cols, collapse = ', ')}. The Stan models require these series to be fully observed when estimating with partial wage data."
      ),
      call. = FALSE
    )
  }
  invisible(NULL)
}

prepare_single_wage_design <- function(est_df, wage_col, context_label) {
  required_cols <- c(
    wage_col,
    "dl4pmcg",
    "LUR",
    "DLPTM",
    "PIE_RBAQ",
    "dummy1",
    "dummy2",
    "dummy3",
    "dummy4"
  )
  design <- est_df %>%
    select(date, all_of(required_cols))

  assert_complete_nonwage(design, wage_col, context_label)
  design
}

run_single_wage_inflation_model <- function(
  est_df,
  wage_col,
  wage_label,
  compiled_model,
  file_stubs,
  variant_label,
  obs_field,
  missing_index_field,
  missing_param,
  wage_component_col
) {
  design <- prepare_single_wage_design(est_df, wage_col, variant_label)
  wage_values <- design[[wage_col]]
  obs <- ifelse(is.na(wage_values), 0L, 1L)
  missing_index <- if (any(obs == 0L)) tail(which(obs == 0L), 1) else 0L

  if (sum(obs == 0L) > 1L) {
    stop(
      glue::glue(
        "{variant_label}: Stan model expects at most one missing {wage_label} observation, but found {sum(obs == 0L)}."
      ),
      call. = FALSE
    )
  }

  if (missing_index > 0L && missing_index != nrow(design)) {
    stop(
      glue::glue(
        "{variant_label}: missing {wage_label} must be the latest quarter (row {nrow(design)}); found NA at row {missing_index}."
      ),
      call. = FALSE
    )
  }

  if (missing_index > 0L) {
    message(glue::glue(
      "ℹ {variant_label}: missing {wage_label} for the most recent quarter; estimating jointly (row {missing_index})."
    ))
  }

  design[[wage_col]] <- ifelse(is.na(wage_values), 0, wage_values)
  stan_matrix <- design %>%
    select(-date) %>%
    as.matrix()

  data_list <- list(
    T = nrow(stan_matrix),
    J = ncol(stan_matrix),
    Y = stan_matrix
  )
  data_list[[obs_field]] <- as.integer(obs)
  data_list[[missing_index_field]] <- as.integer(missing_index)

  fit <- sampling(
    compiled_model,
    data = data_list,
    chains = 4,     iter = 100,
    control = list(max_treedepth = 15)
  )

  draws <- rstan::extract(fit)
  wage_missing_median <- median(draws[[missing_param]])

  if (missing_index > 0L) {
    latest_date <- design$date[missing_index]
    message(glue::glue(
      "🔍 {variant_label}: posterior median estimate for {wage_label} in {latest_date} is {round(wage_missing_median, 2)}."
    ))
  }

  summarised_state <- as.data.frame(fit) %>%
    select(contains("NAIRU")) %>%
    melt() %>%
    group_by(variable) %>%
    summarise(
      median = median(value),
      lowera = quantile(value, 0.05),
      uppera = quantile(value, 0.95),
      lowerb = quantile(value, 0.15),
      upperb = quantile(value, 0.85),
      .groups = "drop"
    ) %>%
    mutate(
      date = est_df$date,
      LUR = est_df$LUR,
      dl4pmcg = est_df$dl4pmcg
    )

  wage_estimates <- ifelse(is.na(wage_values), wage_missing_median, wage_values)
  summarised_state[[paste0(wage_col, "_EST")]] <- wage_estimates

  readr::write_csv(
    summarised_state,
    file.path(out_dir, paste0(file_stubs$nairu, ".csv"))
  )

  Y_mat <- stan_matrix
  if (missing_index > 0L) {
    Y_mat[missing_index, 1] <- wage_missing_median
  }

  Y2_demeaned <- Y_mat[, 2] - mean(Y_mat[, 2])
  Y1_demeaned <- Y_mat[, 1] - mean(Y_mat[, 1])

  nairu_med <- apply(draws$NAIRU, 2, median)

  delta_pt_0  <- median(draws$delta_pt_0)
  phi_pt_0    <- median(draws$phi_pt_0)
  phi_pt_l    <- apply(draws$phi_pt_lag, 2, median)
  gamma_pt_0  <- median(draws$gamma_pt_0)
  gamma_pt_l  <- apply(draws$gamma_pt_lag, 2, median)
  lambda_pt_0 <- median(draws$lambda_pt_0)
  alpha_pt_0  <- median(draws$alpha_pt_0)
  alpha_pt_l  <- apply(draws$alpha_pt_lag, 2, median)
  xi_pt_med   <- apply(draws$xi_pt, 2, median)

  delta_pu_0  <- median(draws$delta_pu_0)
  gamma_pu_0  <- median(draws$gamma_pu_0)
  gamma_pu_l  <- apply(draws$gamma_pu_lag, 2, median)
  lambda_pu_0 <- median(draws$lambda_pu_0)
  xi_pu_med   <- apply(draws$xi_pu, 2, median)

  Tn <- nrow(Y_mat)

  pi_exp <- pi_imp <- pi_ugap <- pi_mom <- pi_wage <- pi_resid <- pi_dum <- rep(NA, Tn)
  pu_dum <- pu_ugap <- pu_mom <- pu_exp <- pu_resid <- rep(NA, Tn)

  for (t in 6:Tn) {
    if (t >= 8) {
      pi_exp[t] <- delta_pt_0 * Y_mat[t, 5]

      pi_imp[t] <- alpha_pt_0 * (Y2_demeaned[t-1] - Y2_demeaned[t-2]) +
        sum(alpha_pt_l *
              (Y2_demeaned[t-(2:4)] - Y2_demeaned[t-(3:5)]))

      pi_ugap[t] <- gamma_pt_0 * ((Y_mat[t, 3] - nairu_med[t]) / Y_mat[t, 3]) +
        sum(gamma_pt_l *
              ((Y_mat[t-(1:3), 3] - nairu_med[t-(1:3)]) /
                 Y_mat[t-(1:3), 3]))

      pi_mom[t]  <- lambda_pt_0 * (Y_mat[t-1, 3] - Y_mat[t-2, 3]) / Y_mat[t, 3]

      pi_wage[t]  <- phi_pt_0 * Y1_demeaned[t-1] +
        sum(phi_pt_l * Y1_demeaned[t-(2:4)])

      pi_dum[t]  <- xi_pt_med[1] * Y_mat[t, 6] + xi_pt_med[2] * Y_mat[t, 7]

      deterministic_pi <- pi_exp[t] + pi_imp[t] + pi_ugap[t] +
        pi_mom[t] + pi_wage[t] + pi_dum[t]

      pi_resid[t] <- Y_mat[t, 4] - deterministic_pi
    }

    pu_dum[t] <- xi_pu_med[1] * Y_mat[t, 8] + xi_pu_med[2] * Y_mat[t, 9]

    pu_ugap[t] <- gamma_pu_0 * (1 - nairu_med[t] / Y_mat[t, 3]) +
      sum(gamma_pu_l * (1 - nairu_med[t-(1:2)] / Y_mat[t-(1:2), 3]))

    pu_mom[t]  <- lambda_pu_0 * (Y_mat[t-1, 3] - Y_mat[t-2, 3]) / Y_mat[t, 3]

    pu_exp[t]  <- delta_pu_0 * Y_mat[t, 5]

    deterministic_pu <- pu_dum[t] + pu_ugap[t] + pu_mom[t] + pu_exp[t]
    pu_resid[t]      <- Y_mat[t, 1] - deterministic_pu
  }

  infl_pi_decomp <- tibble::tibble(
    date_qtr      = est_df$date,
    expectations  = pi_exp,
    import_price  = pi_imp,
    unemp_gap     = pi_ugap,
    momentum      = pi_mom,
    dummies       = pi_dum,
    residuals     = pi_resid,
    wage_component = pi_wage
  )
  wage_component_sym <- rlang::sym(wage_component_col)
  infl_pi_decomp <- infl_pi_decomp %>%
    dplyr::rename(!!wage_component_sym := wage_component)

  wage_decomp <- tibble::tibble(
    date_qtr      = est_df$date,
    dummies       = pu_dum,
    unemp_gap     = pu_ugap,
    momentum      = pu_mom,
    expectations  = pu_exp,
    residuals     = pu_resid
  )

  readr::write_csv(
    infl_pi_decomp,
    file.path(out_dir, paste0(file_stubs$inflation, ".csv"))
  )

  readr::write_csv(
    wage_decomp,
    file.path(out_dir, paste0(file_stubs$wage, ".csv"))
  )

  param_draws <- as.data.frame(fit) %>%
    select(-starts_with("NAIRU"), -lp__)

  param_summary <- param_draws %>%
    pivot_longer(
      everything(),
      names_to  = "parameter",
      values_to = "value"
    ) %>%
    group_by(parameter) %>%
    summarise(
      mean    = mean(value),
      median  = median(value),
      sd      = sd(value),
      lower5  = quantile(value, 0.05),
      lower15 = quantile(value, 0.15),
      upper85 = quantile(value, 0.85),
      upper95 = quantile(value, 0.95),
      .groups = "drop"
    )

  readr::write_csv(
    param_summary,
    file.path(out_dir, paste0(file_stubs$params, ".csv"))
  )

  message(glue::glue(
    "✔ {variant_label}: parameter draws and summaries written to the output directory."
  ))
}

run_dual_wage_model <- function(
  est_df,
  wage_cols,
  compiled_model,
  file_stubs,
  variant_label
) {
  design <- est_df %>%
    select(
      date,
      all_of(c(
        wage_cols,
        "dl4pmcg",
        "LUR",
        "DLPTM",
        "PIE_RBAQ",
        "dummy1",
        "dummy2",
        "dummy3",
        "dummy4"
      ))
    )

  Tn <- nrow(design)
  wage_obs <- matrix(1L, nrow = Tn, ncol = length(wage_cols))
  missing_index <- integer(length(wage_cols))
  wage_values <- vector("list", length(wage_cols))

  for (j in seq_along(wage_cols)) {
    w <- design[[wage_cols[j]]]
    wage_values[[j]] <- w
    obs <- ifelse(is.na(w), 0L, 1L)
    wage_obs[, j] <- obs
    missing_index[j] <- if (any(obs == 0L)) tail(which(obs == 0L), 1) else 0L
    if (missing_index[j] > 0L) {
      message(glue::glue(
        "ℹ {variant_label}: missing {wage_cols[j]} for the most recent quarter; estimating jointly (row {missing_index[j]})."
      ))
    }
    design[[wage_cols[j]]] <- ifelse(is.na(w), 0, w)
  }

  storage.mode(wage_obs) <- "integer"
  missing_index <- as.integer(missing_index)

  stan_matrix <- design %>%
    select(-date) %>%
    as.matrix()

  data_list <- list(
    T = nrow(stan_matrix),
    J = ncol(stan_matrix),
    Y = stan_matrix,
    wage_obs = wage_obs,
    missing_wage_index = missing_index
  )

  fit <- sampling(
    compiled_model,
    data = data_list,
    chains = 4,     iter = 100,
    control = list(max_treedepth = 15)
  )

  draws <- rstan::extract(fit)
  wage_missing_median <- apply(draws$wage_missing, 2, median)

  for (j in seq_along(wage_cols)) {
    if (missing_index[j] > 0L) {
      latest_date <- design$date[missing_index[j]]
      message(glue::glue(
        "🔍 {variant_label}: posterior median estimate for {wage_cols[j]} in {latest_date} is {round(wage_missing_median[j], 2)}."
      ))
    }
  }

  summarised_state <- as.data.frame(fit) %>%
    select(contains("NAIRU")) %>%
    melt() %>%
    group_by(variable) %>%
    summarise(
      median = median(value),
      lowera = quantile(value, 0.05),
      uppera = quantile(value, 0.95),
      lowerb = quantile(value, 0.15),
      upperb = quantile(value, 0.85),
      .groups = "drop"
    ) %>%
    mutate(
      date = est_df$date,
      LUR = est_df$LUR,
      dl4pmcg = est_df$dl4pmcg
    )

  for (j in seq_along(wage_cols)) {
    est_values <- ifelse(is.na(wage_values[[j]]), wage_missing_median[j], wage_values[[j]])
    summarised_state[[paste0(wage_cols[j], "_EST")]] <- est_values
  }

  readr::write_csv(
    summarised_state,
    file.path(out_dir, paste0(file_stubs$nairu, ".csv"))
  )

  param_draws <- as.data.frame(fit) %>%
    select(-starts_with("NAIRU"), -lp__)

  param_summary <- param_draws %>%
    pivot_longer(
      everything(),
      names_to  = "parameter",
      values_to = "value"
    ) %>%
    group_by(parameter) %>%
    summarise(
      mean    = mean(value),
      median  = median(value),
      sd      = sd(value),
      lower5  = quantile(value, 0.05),
      lower15 = quantile(value, 0.15),
      upper85 = quantile(value, 0.85),
      upper95 = quantile(value, 0.95),
      .groups = "drop"
    )

  readr::write_csv(
    param_summary,
    file.path(out_dir, paste0(file_stubs$params, ".csv"))
  )

  message(glue::glue(
    "✔ {variant_label}: parameter draws and summaries written to the output directory."
  ))
}

run_wage_no_inflation_model <- function(
  est_df,
  wage_col,
  compiled_model,
  file_stubs,
  variant_label
) {
  design <- est_df %>%
    select(
      date,
      all_of(c(wage_col, "LUR", "PIE_RBAQ", "dummy3", "dummy4"))
    )

  assert_complete_nonwage(design, wage_col, variant_label)

  wage_values <- design[[wage_col]]
  obs <- ifelse(is.na(wage_values), 0L, 1L)
  missing_index <- if (any(obs == 0L)) tail(which(obs == 0L), 1) else 0L

  if (sum(obs == 0L) > 1L) {
    stop(
      glue::glue(
        "{variant_label}: Stan model expects at most one missing {wage_col} observation, but found {sum(obs == 0L)}."
      ),
      call. = FALSE
    )
  }

  if (missing_index > 0L && missing_index != nrow(design)) {
    stop(
      glue::glue(
        "{variant_label}: missing {wage_col} must be the latest quarter (row {nrow(design)}); found NA at row {missing_index}."
      ),
      call. = FALSE
    )
  }

  if (missing_index > 0L) {
    message(glue::glue(
      "ℹ {variant_label}: missing {wage_col} for the most recent quarter; estimating jointly (row {missing_index})."
    ))
  }

  design[[wage_col]] <- ifelse(is.na(wage_values), 0, wage_values)

  stan_matrix <- design %>%
    select(-date) %>%
    as.matrix()

  data_list <- list(
    T = nrow(stan_matrix),
    J = ncol(stan_matrix),
    Y = stan_matrix,
    wage_obs = as.integer(obs),
    missing_wage_index = as.integer(missing_index)
  )

  fit <- sampling(
    compiled_model,
    data = data_list,
    chains = 4,     iter = 100,
    control = list(max_treedepth = 15)
  )

  draws <- rstan::extract(fit)
  wage_missing_median <- median(draws$wage_missing)

  if (missing_index > 0L) {
    latest_date <- design$date[missing_index]
    message(glue::glue(
      "🔍 {variant_label}: posterior median estimate for {wage_col} in {latest_date} is {round(wage_missing_median, 2)}."
    ))
  }

  summarised_state <- as.data.frame(fit) %>%
    select(contains("NAIRU")) %>%
    melt() %>%
    group_by(variable) %>%
    summarise(
      median = median(value),
      lowera = quantile(value, 0.05),
      uppera = quantile(value, 0.95),
      lowerb = quantile(value, 0.15),
      upperb = quantile(value, 0.85),
      .groups = "drop"
    ) %>%
    mutate(
      date = est_df$date,
      LUR = est_df$LUR
    )

  wage_estimates <- ifelse(is.na(wage_values), wage_missing_median, wage_values)
  summarised_state[[paste0(wage_col, "_EST")]] <- wage_estimates

  readr::write_csv(
    summarised_state,
    file.path(out_dir, paste0(file_stubs$nairu, ".csv"))
  )

  param_draws <- as.data.frame(fit) %>%
    select(-starts_with("NAIRU"), -lp__)

  param_summary <- param_draws %>%
    pivot_longer(
      everything(),
      names_to  = "parameter",
      values_to = "value"
    ) %>%
    group_by(parameter) %>%
    summarise(
      mean    = mean(value),
      median  = median(value),
      sd      = sd(value),
      lower5  = quantile(value, 0.05),
      lower15 = quantile(value, 0.15),
      upper85 = quantile(value, 0.85),
      upper95 = quantile(value, 0.95),
      .groups = "drop"
    )

  readr::write_csv(
    param_summary,
    file.path(out_dir, paste0(file_stubs$params, ".csv"))
  )

  message(glue::glue(
    "✔ {variant_label}: parameter draws and summaries written to the output directory."
  ))
}

compiled_models <- list(
  cpi_ulc = stan_model(file = file.path("stan", "NAIRU_cpi_ulc.stan")),
  cpi_wpi = stan_model(file = file.path("stan", "NAIRU_cpi_wpi.stan")),
  wpi_only = stan_model(file = file.path("stan", "NAIRU_wpi_only.stan"))
)

single_wage_variants <- list(
  list(
    wage_col = "DLNULC",
    wage_label = "DLNULC",
    file_stubs = list(
      nairu = "NAIRU_baseline",
      inflation = "infl_pi_decomp",
      wage = "ulc_decomp",
      params = "posterior_summary_params"
    ),
    variant_label = "CPI & ULC model",
    model_key = "cpi_ulc",
    obs_field = "ulc_obs",
    missing_index_field = "missing_ulc_index",
    missing_param = "ulc_missing",
    wage_component_col = "ulc_demeaned"
  ),
  list(
    wage_col = "DLWPI",
    wage_label = "DLWPI",
    file_stubs = list(
      nairu = "NAIRU_wpi",
      inflation = "infl_pi_decomp_wpi",
      wage = "wage_decomp_wpi",
      params = "posterior_summary_params_wpi"
    ),
    variant_label = "CPI & WPI model",
    model_key = "cpi_wpi",
    obs_field = "wpi_obs",
    missing_index_field = "missing_wpi_index",
    missing_param = "wpi_missing",
    wage_component_col = "wpi_demeaned"
  )
)

for (variant in single_wage_variants) {
  run_single_wage_inflation_model(
    est_df = est_data,
    wage_col = variant$wage_col,
    wage_label = variant$wage_label,
    compiled_model = compiled_models[[variant$model_key]],
    file_stubs = variant$file_stubs,
    variant_label = variant$variant_label,
    obs_field = variant$obs_field,
    missing_index_field = variant$missing_index_field,
    missing_param = variant$missing_param,
    wage_component_col = variant$wage_component_col
  )
}

#run_wage_no_inflation_model(
#  est_df = est_data,
#  wage_col = "DLWPI",
#  compiled_model = compiled_models$wpi_only,
#  file_stubs = list(
#    nairu = "NAIRU_wpi_no_inflation",
#    params = "posterior_summary_params_wpi_no_inflation"
#  ),
#  variant_label = "WPI no-inflation model"
#)
