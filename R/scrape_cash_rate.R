# ---- Libraries --------------------------------------------------------------
library(dplyr)
library(readr)
library(purrr)
library(lubridate)
library(fs)
library(glue)
library(here)

# ---- Helper functions -------------------------------------------------------
coalesce_datetime <- function(...) {
  candidates <- list(...)
  stopifnot(length(candidates) > 0)
  result <- candidates[[1]]
  if (length(candidates) == 1) {
    return(result)
  }

  for (candidate in candidates[-1]) {
    if (is.null(candidate)) {
      next
    }
    if (!inherits(candidate, c("POSIXct", "POSIXt"))) {
      stop("All inputs to coalesce_datetime() must be POSIXct vectors or NULL")
    }
    if (is.null(result)) {
      result <- candidate
      next
    }

    replacement_idx <- is.na(result) & !is.na(candidate)
    if (!length(replacement_idx)) {
      next
    }
    result[replacement_idx] <- candidate[replacement_idx]
  }

  result
}

parse_scrape_date <- function(x) {
  if (is.null(x)) {
    return(as.Date(rep(NA_real_, length.out = 1)))
  }

  if (inherits(x, "Date")) {
    return(x)
  }

  parsed <- suppressWarnings(lubridate::ymd(x, quiet = TRUE))
  parsed <- dplyr::coalesce(parsed, suppressWarnings(lubridate::dmy(x, quiet = TRUE)))

  parsed
}

parse_scrape_time <- function(scrape_time, scrape_date, tz = "Australia/Melbourne") {
  if (is.null(scrape_time)) {
    scrape_time <- NA_character_
  }

  scrape_time <- na_if(trimws(scrape_time), "")
  if (all(is.na(scrape_time))) {
    return(as.POSIXct(paste(scrape_date, "12:00:00"), tz = tz))
  }

  orders <- c(
    "Y-m-d H:M:S",
    "Y-m-d H:M",
    "Ymd HMS",
    "Ymd HM",
    "Ymd HMSz",
    "Ymd HMz",
    "Ymd HMSO",
    "Ymd HMO",
    "Y-m-dTH:M:OS",
    "Y-m-dTH:M:OSz",
    "Y-m-dTH:M:OSO",
    "d/m/Y H:M:S",
    "d/m/Y H:M",
    "d-b-Y H:M:S",
    "d-b-Y H:M"
  )

  parsed_candidates <- purrr::map(
    orders,
    ~ suppressWarnings(lubridate::parse_date_time(scrape_time, orders = .x, tz = tz, quiet = TRUE))
  )

  parsed_candidates <- purrr::discard(parsed_candidates, is.null)
  if (!length(parsed_candidates)) {
    parsed_time <- as.POSIXct(rep(NA, length(scrape_time)), tz = tz, origin = "1970-01-01")
  } else {
    parsed_time <- purrr::reduce(parsed_candidates, coalesce_datetime)
  }

  fallback_time <- suppressWarnings(as.POSIXct(paste(scrape_date, "12:00:00"), tz = tz))
  parsed_time <- dplyr::coalesce(parsed_time, fallback_time)

  parsed_time
}

# ---- Main script ------------------------------------------------------------
root_dir <- here::here()
daily_dir <- fs::path(root_dir, "daily_data")
combined_dir <- fs::path(root_dir, "combined_data")
fs::dir_create(daily_dir, recurse = TRUE)
fs::dir_create(combined_dir, recurse = TRUE)

csv_files <- fs::dir_ls(daily_dir, glob = "*.csv", type = "file")

if (!length(csv_files)) {
  rlang::inform("No daily CSV files were found – skipping aggregation.")
  quit(save = "no")
}

read_daily_file <- function(path) {
  readr::read_csv(
    path,
    show_col_types = FALSE,
    col_types = cols(.default = col_character())
  ) %>%
    mutate(source_file = fs::path_file(path))
}

df_list <- purrr::map(csv_files, read_daily_file)

all_data <- bind_rows(df_list) %>%
  mutate(
    date = parse_scrape_date(date),
    cash_rate = readr::parse_double(cash_rate),
    scrape_date = parse_scrape_date(scrape_date)
  ) %>%
  mutate(
    scrape_time = parse_scrape_time(scrape_time, scrape_date)
  )

if ("meeting_date" %in% names(all_data)) {
  all_data <- all_data %>%
    mutate(meeting_date = parse_scrape_date(meeting_date)) %>%
    arrange(scrape_time, meeting_date)
} else {
  all_data <- all_data %>% arrange(scrape_time)
}

readr::write_rds(all_data, fs::path(combined_dir, "all_data.Rds"))

rlang::inform(glue::glue("Processed {length(csv_files)} daily files; combined dataset has {nrow(all_data)} rows."))
