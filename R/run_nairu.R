# ---- libraries ------------------------------------------------------------
library(ggthemes); library(reshape2); library(readabs);  library(dplyr)
library(ggplot2);   library(zoo);      library(rstan);   library(readrba)
library(lubridate); library(readr);    library(here);     library(stringr)


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
   quit(save = "no")   # graceful, zero-exit termination
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
abs_6202 <- read_abs(series_id = c("A84423043C", "A84423047L"))
abs_6457 <- read_abs(series_id = c("A2298279F"))
abs_6345 <- read_abs(series_id = c("A2713849C"))
rba_g3 <- read_rba(series_id = c("GBONYLD"))
#rba_g1 <- read_rba(series_id = c("GCPIOCPMTMQP","GCPITIQP","GCPINTIQP"))
rba_g1 <- read_abs(series_id = c("A3604510W","A2330530C","A2330575J"))

rba_table_cache <- new.env(parent = emptyenv())

get_rba_table <- function(table_no) {
  if (!exists(table_no, envir = rba_table_cache, inherits = FALSE)) {
    tbl <- tryCatch(
      read_rba(table_no = table_no),
      error = function(e) {
        message(glue::glue("⚠️ Unable to download RBA table {table_no}: {e$message}"))
        NULL
      }
    )
    assign(table_no, tbl, envir = rba_table_cache)
  }
  get(table_no, envir = rba_table_cache, inherits = FALSE)
}

default_rba_tables <- c("A1", "A2", "B1", "B2", "C1", "C2", "D1", "D2",
                        "D3", "D4", "D5", "D6", "E1", "E2", "F1", "F2",
                        "G1", "G2", "G3", "G4", "G5", "G6", "G7", "G8",
                        "H1", "H2", "H3", "H4", "H5", "I1", "I2")

lookup_rba_series <- function(keywords, table = NULL, frequency = NULL) {
  keywords <- str_to_lower(as.vector(keywords))
  tables_to_scan <- if (is.null(table) || length(table) == 0) default_rba_tables else table

  table_data <- purrr::map_dfr(tables_to_scan, function(tbl) {
    df <- get_rba_table(tbl)
    if (is.null(df) || nrow(df) == 0) {
      return(tibble::tibble())
    }
    df$table_no <- tbl
    df
  })

  if (nrow(table_data) == 0) {
    stop("Unable to download any RBA tables for lookup.")
  }

  search_cols <- intersect(
    c("series", "description", "title", "series_title", "series_description", "label", "units"),
    names(table_data)
  )

  if (length(search_cols) == 0) {
    stop("Unable to construct search metadata for RBA tables.")
  }

  table_data <- table_data %>%
    mutate(
      search_text = str_to_lower(
        purrr::pmap_chr(
          dplyr::select(., dplyr::all_of(search_cols)),
          ~ paste(c(...), collapse = " ")
        )
      )
    )

  if (!is.null(table) && "table_no" %in% names(table_data)) {
    table_data <- table_data %>% filter(table_no %in% table)
  }

  if (!is.null(frequency) && "frequency" %in% names(table_data)) {
    table_data <- table_data %>% filter(frequency %in% frequency)
  }

  for (kw in keywords) {
    table_data <- table_data %>% filter(str_detect(search_text, kw))
  }

  if (nrow(table_data) == 0) {
    stop(sprintf("Unable to find RBA series for keywords: %s", paste(keywords, collapse = ", ")))
  }

  available_ids <- unique(stats::na.omit(table_data$series_id))

  if (length(available_ids) == 0) {
    stop(sprintf("Unable to find RBA series for keywords: %s", paste(keywords, collapse = ", ")))
  }

  available_ids[[1]]
}

quarterly_average <- function(df) {
  df %>%
    mutate(date = zoo::as.yearqtr(date)) %>%
    group_by(date) %>%
    summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
}

log_diff_transform <- function(df, new_name, lag_n = 1) {
  df <- df %>% arrange(date)
  lagged <- dplyr::lag(df$value, lag_n)
  valid <- !is.na(df$value) & !is.na(lagged) & df$value > 0 & lagged > 0
  df[[new_name]] <- NA_real_
  df[[new_name]][valid] <- 100 * (log(df$value[valid]) - log(lagged[valid]))
  df[, c("date", new_name)]
}

underlying_series_id <- lookup_rba_series(
  c("trimmed mean", "inflation", "year-ended"),
  table     = c("G1", "G3"),
  frequency = "Quarterly"
)
underlying_inflation <- read_rba(series_id = underlying_series_id) %>%
  quarterly_average() %>%
  rename(UNDERLYING_INFLATION = value)

aena_series_id <- lookup_rba_series(
  c("average", "earnings", "national accounts"),
  frequency = "Quarterly"
)
aena_quarterly <- read_rba(series_id = aena_series_id) %>%
  quarterly_average()
R_aena <- log_diff_transform(aena_quarterly, "DLAENA")

productivity_series_id <- lookup_rba_series(
  c("labour", "productivity"),
  frequency = "Quarterly"
)
productivity_quarterly <- read_rba(series_id = productivity_series_id) %>%
  quarterly_average()
R_productivity <- log_diff_transform(productivity_quarterly, "DLPRODUCTIVITY")

underutilisation_series_id <- lookup_rba_series(c("underutilisation rate"))
R_underutilisation <- read_rba(series_id = underutilisation_series_id) %>%
  quarterly_average() %>%
  rename(UNDERUTILISATION_RATE = value)

unemployment_series_id <- lookup_rba_series(c("unemployment rate"))
R_unemployment <- read_rba(series_id = unemployment_series_id) %>%
  quarterly_average() %>%
  rename(UNEMPLOYMENT_RATE = value)

labour_series_id <- lookup_rba_series(c("participation rate"))
R_labour <- read_rba(series_id = labour_series_id) %>%
  quarterly_average() %>%
  rename(LABOUR_PARTICIPATION = value)

capacity_series_id <- lookup_rba_series(c("capacity utilisation"))
R_capacity <- read_rba(series_id = capacity_series_id) %>%
  quarterly_average() %>%
  rename(CAPACITY_UTILISATION = value)

jobs_ads_series_id <- lookup_rba_series(c("job", "advertisements"))
jobs_ads_quarterly <- read_rba(series_id = jobs_ads_series_id) %>%
  quarterly_average()
R_job_ads <- log_diff_transform(jobs_ads_quarterly, "DLJOBADS")

vacancies_series_id <- lookup_rba_series(
  c("job", "vacancies"),
  frequency = "Quarterly"
)
vacancies_quarterly <- read_rba(series_id = vacancies_series_id) %>%
  quarterly_average()
R_vacancies <- log_diff_transform(vacancies_quarterly, "DLVACANCIES")

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
  R_g1,
  pie_rbaq,
  underlying_inflation,
  R_aena,
  R_productivity,
  R_underutilisation,
  R_unemployment,
  R_labour,
  R_capacity,
  R_job_ads,
  R_vacancies
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


# ── Extend pie_rbaq forward to latest_date_df2 ────────────────────────────────
latest_date_df2 <- max(R_g1$date)
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




data_set <- list(R_5206, R_6457, R_6202, R_g1, pie_rbaq, R_aena, R_6345) %>%
  Reduce(function(dtf1,dtf2) left_join(dtf1,dtf2,by="date"), .)

#data_set$pie_bondq <- replace(data_set$pie_bondq,is.na(data_set$pie_bondq),2.5/4)


data_set <- data_set %>%
  filter(!is.na(date)) %>%
  arrange(date)

filled_data_set <- data_set %>%
  mutate(across(-date, ~ na.locf(.x, na.rm = FALSE)))

latest_data_date <- max(data_set$date, na.rm = TRUE)
wage_columns <- c("DLNULC", "DLAENA", "DLWPI")

for (w_col in wage_columns) {
  if (w_col %in% names(filled_data_set)) {
    reset_mask <- is.na(data_set[[w_col]]) & data_set$date == latest_data_date
    filled_data_set[[w_col]][reset_mask] <- NA_real_
  }
}

data_set <- filled_data_set

# Pick Sample
est_data <- data_set %>%
  filter(date > "1997q3" ) %>%
  mutate(dummy1 = ifelse(date >= "2021Q3" & date <= "2023Q1", 1, 0),
         dummy2 = ifelse(date >= "2022Q1" & date <= "2022Q4", 1, 0),
         dummy3 = ifelse(date == "2020Q2", 1, 0),
         dummy4 = ifelse(date == "2020Q3", 1, 0))

         

print(as_tibble(est_data), n = Inf, width = Inf)
         
csv_path <- file.path(out_dir, "est_data.csv")
readr::write_csv(est_data, csv_path)

myfile <- file.path(out_dir, "est_data.csv")
test <- read_csv(myfile)
         
# Subset Data for Stan
prepare_single_wage_design <- function(est_df, wage_col) {
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
  est_df %>%
    select(date, all_of(required_cols))
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
  design <- prepare_single_wage_design(est_df, wage_col)
  wage_values <- design[[wage_col]]
  obs <- ifelse(is.na(wage_values), 0L, 1L)
  missing_index <- if (any(obs == 0L)) tail(which(obs == 0L), 1) else 0L

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
    chains = 10,
    iter = 10000,
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
    chains = 10,
    iter = 10000,
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

  wage_values <- design[[wage_col]]
  obs <- ifelse(is.na(wage_values), 0L, 1L)
  missing_index <- if (any(obs == 0L)) tail(which(obs == 0L), 1) else 0L

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
    chains = 10,
    iter = 10000,
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
  cpi_aena = stan_model(file = file.path("stan", "NAIRU_cpi_aena.stan")),
  cpi_wpi = stan_model(file = file.path("stan", "NAIRU_cpi_wpi.stan")),
  cpi_aena_wpi = stan_model(file = file.path("stan", "NAIRU_cpi_aena_wpi.stan")),
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
    wage_col = "DLAENA",
    wage_label = "DLAENA",
    file_stubs = list(
      nairu = "NAIRU_aena",
      inflation = "infl_pi_decomp_aena",
      wage = "wage_decomp_aena",
      params = "posterior_summary_params_aena"
    ),
    variant_label = "CPI & AENA model",
    model_key = "cpi_aena",
    obs_field = "aena_obs",
    missing_index_field = "missing_aena_index",
    missing_param = "aena_missing",
    wage_component_col = "aena_demeaned"
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

run_dual_wage_model(
  est_df = est_data,
  wage_cols = c("DLAENA", "DLWPI"),
  compiled_model = compiled_models$cpi_aena_wpi,
  file_stubs = list(
    nairu = "NAIRU_aena_wpi",
    params = "posterior_summary_params_aena_wpi"
  ),
  variant_label = "AENA & WPI wage model"
)

run_wage_no_inflation_model(
  est_df = est_data,
  wage_col = "DLWPI",
  compiled_model = compiled_models$wpi_only,
  file_stubs = list(
    nairu = "NAIRU_wpi_no_inflation",
    params = "posterior_summary_params_wpi_no_inflation"
  ),
  variant_label = "WPI no-inflation model"
)
