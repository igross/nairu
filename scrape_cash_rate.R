library(conflicted)
conflict_prefer_all("dplyr", quiet = TRUE)
library(tidyverse)
library(jsonlite)
library(lubridate)

# list all CSVs in daily_data
old_csvs <- list.files(
  path       = "daily_data",
  pattern    = "\\.csv$",
  full.names = TRUE
)

dates_in_name <- str_match(basename(old_csvs),
                           "scraped_cash_rate_(\\d{4}-\\d{2}-\\d{2})")[,2]

# 3. parse to Date
file_dates <- as.Date(dates_in_name, format = "%Y-%m-%d")

# 4. compute cutoff
cutoff_date <- Sys.Date() - weeks(8)

is_old       <- !is.na(file_dates) & file_dates < cutoff_date
is_weekend   <- !is.na(file_dates) & lubridate::wday(file_dates) %in% c(1, 7)
to_remove    <- old_csvs[is_old | is_weekend]

 # 6. remove them (if any)
 if (length(to_remove) > 0) {
   file.remove(to_remove)
 }

json_url <- "https://asx.api.markitdigital.com/asx-research/1.0/derivatives/interest-rate/IB/futures?days=1&height=179&width=179"

json_file <- tempfile()

# download and parse
download.file(json_url, json_file)
new_data <- fromJSON(json_file) %>%
  pluck("data", "items") %>%
  as_tibble() %>%
  mutate(
    # define `date` straight away, floor to start of month
    date         = ymd(dateExpiry) %>% floor_date("month"),
    scrape_date  = ymd(datePreviousSettlement),
    scrape_time  = now(tzone = "Australia/Melbourne"),
    cash_rate    = 100 - priceLastTrade
  ) %>%
  filter(pricePreviousSettlement != 0) %>%
  select(date, cash_rate, scrape_date, scrape_time)

print(new_data)

# after you have new_data

# 1. Grab the single scrape_time (they should all be the same)
scrape_dt <- unique(new_data$scrape_time)

# 2. If for some reason there's more than one, fall back to Sys.Date()-1
if (length(scrape_dt) != 1) {
  scrape_dt <- Sys.Date() - 1
}

# 3. Format as YYYY-MM-DD
scrape_str <- format(scrape_dt, "%Y-%m-%d_%H%M")

# 4. Build a single file name
out_file <- file.path(
  "daily_data",
  paste0("scraped_cash_rate_", scrape_str, ".csv")
)

# 5. Write the CSV once
write_csv(new_data, out_file)


library(purrr)
library(readr)
library(dplyr)
library(lubridate)

# 1. list your files
files <- list.files(
  path       = "daily_data",
  pattern    = "\\.csv$",
  full.names = TRUE
)

df_list <- files %>% map(function(f) {
  df <- read_csv(f, col_types = cols())

  if (!"scrape_time" %in% names(df)) {
    # create a POSIXct NA in Melbourne time
    df$scrape_time <- as.POSIXct(NA, tz = "Australia/Melbourne")
  } else {
    # parse any existing scrape_time strings into POSIXct in Melbourne time
    df <- df %>%
      mutate(scrape_time = ymd_hms(scrape_time, tz = "Australia/Melbourne"))
  }

  df
})


# 3. bind them into one data‑frame
all_data <- bind_rows(df_list) %>%
  mutate(
    date        = as.Date(date),
    cash_rate   = as.double(cash_rate),    # ← use as.double()
    scrape_date = as.Date(scrape_date),
    scrape_time = if_else(
      is.na(scrape_time),
      as.POSIXct(paste(scrape_date, "12:00:00"),
                 tz = "Australia/Melbourne"),
      scrape_time
    )
  ) %>%
  filter(
    !scrape_date %in% ymd(c(
      "2022-08-06","2022-08-07","2022-08-08",
      "2023-01-18","2023-01-24","2023-01-31",
      "2023-02-02","2022-12-30","2022-12-29"
    )),
    !is.na(date),
    !is.na(cash_rate)
  )

# 5. ensure the output folder exists
if (!dir.exists("combined_data")) {
  dir.create("combined_data", recursive = TRUE)
}

tail(all_data,20)

# 6. save
saveRDS(all_data, file = "combined_data/all_data.Rds")
write_csv(all_data,  file = "combined_data/cash_rate.csv")

