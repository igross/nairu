# nairu_realtime.R -----------------------------------------------------------
# Build realtime NAIRU baseline estimates using vintage ABS releases.

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(glue)
  library(here)
  library(lubridate)
  library(purrr)
  library(readabs)
  library(readr)
  library(rstan)
  library(tidyr)
  library(zoo)
})

# ----------------------------------------------------------------------------
# Config and paths
# ----------------------------------------------------------------------------
options(mc.cores = max(1, parallel::detectCores() - 1))
Sys.setenv(MAKEFLAGS = paste0("-j", max(1, parallel::detectCores() - 1)))

root_dir   <- here::here()
out_dir    <- file.path(root_dir, "output", "realtime")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

csv_out    <- file.path(out_dir, "nairu_realtime_estimates.csv")
plot_out   <- file.path(out_dir, "nairu_realtime_estimates.png")

# ----------------------------------------------------------------------------
# Helpers
# ----------------------------------------------------------------------------
fetch_abs_series <- function(series_ids, release_date) {
  tryCatch(
    read_abs(series_id = series_ids, release_date = release_date),
    error = function(err) {
      message(
        glue("⚠️ read_abs failed for {paste(series_ids, collapse = ', ')} @ {release_date}: {conditionMessage(err)}")
      )
      NULL
    }
  )
}

prepare_vintage_est_data <- function(release_date, expectations_df) {
  release_date <- as.Date(release_date)
  cutoff_qtr <- zoo::as.yearqtr(release_date) - 0.25

  abs_5206 <- fetch_abs_series(c("A2304402X", "A2302915V"), release_date)
  abs_6457 <- fetch_abs_series("A2298279F", release_date)
  abs_6202 <- fetch_abs_series("A84423050A", release_date)
  abs_trimmed_mean <- fetch_abs_series("A3604510W", release_date)

  if (any(map_lgl(list(abs_5206, abs_6457, abs_6202, abs_trimmed_mean), is.null))) {
    return(NULL)
  }

  R_5206 <- abs_5206 %>%
    filter(series_id %in% c("A2304402X", "A2302915V"), date <= as.Date(cutoff_qtr)) %>%
    mutate(date = zoo::as.yearqtr(date)) %>%
    select(date, series_id, value) %>%
    distinct(date, series_id, .keep_all = TRUE) %>%
    tidyr::pivot_wider(names_from = series_id, values_from = value) %>%
    mutate(
      NULC = A2302915V / A2304402X,
      DLNULC = 100 * (log(NULC) - log(lag(NULC, 1)))
    ) %>%
    select(date, DLNULC)

  R_6457 <- abs_6457 %>%
    filter(series_id == "A2298279F", date <= as.Date(cutoff_qtr)) %>%
    mutate(
      date = zoo::as.yearqtr(date),
      dl4pmcg = 100 * (log(value) - log(lag(value, 4)))
    ) %>%
    select(date, dl4pmcg)

  R_6202 <- abs_6202 %>%
    filter(series_id == "A84423050A", date <= as.Date(cutoff_qtr)) %>%
    mutate(date = zoo::as.yearqtr(date)) %>%
    group_by(date) %>%
    summarise(LUR = mean(value, na.rm = TRUE), .groups = "drop")

  R_trimmed_mean <- abs_trimmed_mean %>%
    filter(series_id == "A3604510W", date <= as.Date(cutoff_qtr)) %>%
    mutate(date = zoo::as.yearqtr(date)) %>%
    rename(DLPTM = value) %>%
    select(date, DLPTM)

  pie_rbaq <- expectations_df %>%
    filter(date <= cutoff_qtr)

  data_set <- list(R_5206, R_6457, R_6202, R_trimmed_mean, pie_rbaq) %>%
    reduce(full_join, by = "date") %>%
    arrange(date) %>%
    filter(!is.na(date))

  latest_data_date <- max(data_set$date, na.rm = TRUE)
  latest_ulc_missing <- FALSE

  if ("DLNULC" %in% names(data_set)) {
    latest_ulc_missing <- data_set %>%
      filter(date == latest_data_date) %>%
      summarise(flag = all(is.na(DLNULC))) %>%
      pull(flag)
  }

  data_set <- data_set %>%
    mutate(across(-date, ~ zoo::na.locf(.x, na.rm = FALSE)))

  if (isTRUE(latest_ulc_missing)) {
    data_set$DLNULC[data_set$date == latest_data_date] <- NA_real_
  }

  data_set <- data_set %>%
    mutate(
      dl4pmcg = ifelse(is.na(dl4pmcg) & !is.na(DLPTM), mean(dl4pmcg, na.rm = TRUE), dl4pmcg)
    )

  est_data <- data_set %>%
    filter(date >= as.yearqtr("2000 Q1"), date <= cutoff_qtr) %>%
    mutate(
      dummy1 = ifelse(date >= as.yearqtr("2021 Q3") & date <= as.yearqtr("2023 Q1"), 1, 0),
      dummy2 = ifelse(date >= as.yearqtr("2022 Q1") & date <= as.yearqtr("2022 Q4"), 1, 0),
      dummy3 = ifelse(date == as.yearqtr("2020 Q2"), 1, 0),
      dummy4 = ifelse(date == as.yearqtr("2020 Q3"), 1, 0)
    )

  if (nrow(est_data) < 12) {
    message(glue("⚠️ Not enough data to estimate for {release_date} (only {nrow(est_data)} rows)."))
    return(NULL)
  }

  est_data
}

run_baseline_model <- function(est_df, compiled_model, release_date) {
  design <- est_df %>%
    select(date, DLNULC, dl4pmcg, LUR, DLPTM, PIE_RBAQ, dummy1, dummy2, dummy3, dummy4)

  wage_obs <- ifelse(is.na(design$DLNULC), 0L, 1L)
  missing_index <- if (any(wage_obs == 0L)) tail(which(wage_obs == 0L), 1) else 0L

  design <- design %>%
    mutate(DLNULC = ifelse(is.na(DLNULC), 0, DLNULC))

  stan_matrix <- design %>%
    select(-date) %>%
    as.matrix()

  data_list <- list(
    T = nrow(stan_matrix),
    J = ncol(stan_matrix),
    Y = stan_matrix,
    ulc_obs = as.integer(wage_obs),
    missing_ulc_index = as.integer(missing_index)
  )

  fit <- sampling(
    compiled_model,
    data = data_list,
    chains = 2,
    iter = 200,
    refresh = 0,
    control = list(max_treedepth = 10)
  )

  summaries <- as.data.frame(fit) %>%
    select(matches("^NAIRU\\[")) %>%
    pivot_longer(
      everything(),
      names_to = "state",
      values_to = "value"
    ) %>%
    mutate(index = readr::parse_number(state)) %>%
    group_by(index) %>%
    summarise(
      median = median(value),
      lower5 = quantile(value, 0.05),
      upper95 = quantile(value, 0.95),
      .groups = "drop"
    ) %>%
    mutate(
      date_qtr = est_df$date[as.integer(index)],
      date = as.Date(date_qtr, frac = 0.5),
      release_date = as.Date(release_date),
      release_year = year(release_date)
    ) %>%
    select(release_year, release_date, date_qtr, date, median, lower5, upper95)

  summaries
}

# ----------------------------------------------------------------------------
# Load expectations data and run vintages
# ----------------------------------------------------------------------------
expectations <- readr::read_csv(file.path(root_dir, "inputs", "PIE_RBAQ.CSV"), show_col_types = FALSE) %>%
  rename(date = OBS) %>%
  mutate(date = zoo::as.yearqtr(date)) %>%
  select(date, PIE_RBAQ)

compiled_model <- stan_model(file.path(root_dir, "stan", "NAIRU_cpi_ulc.stan"))

years_to_run <- 2000:year(Sys.Date())
release_dates <- as.Date(paste0(years_to_run, "-12-01"))

vintage_results <- map_dfr(release_dates, function(rel_date) {
  message(glue("▶ Running baseline vintage for {rel_date}"))
  est_df <- prepare_vintage_est_data(rel_date, expectations)
  if (is.null(est_df)) {
    return(NULL)
  }
  run_baseline_model(est_df, compiled_model, rel_date)
})

if (nrow(vintage_results) == 0) {
  stop("No realtime NAIRU estimates were produced.")
}

readr::write_csv(vintage_results, csv_out)
message(glue("💾 Saved realtime NAIRU estimates to {csv_out}"))

# ----------------------------------------------------------------------------
# Plot realtime NAIRU paths
# ----------------------------------------------------------------------------
realtime_plot <- ggplot(vintage_results, aes(x = date, y = median, colour = factor(release_year), group = release_year)) +
  geom_line(alpha = 0.8, linewidth = 0.9) +
  labs(
    title = "Realtime baseline NAIRU estimates by release year",
    x = "Date",
    y = "NAIRU (median)",
    colour = "Release year"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  filename = plot_out,
  plot = realtime_plot,
  width = 10,
  height = 6,
  dpi = 300
)

message(glue("💾 Saved realtime NAIRU plot to {plot_out}"))
