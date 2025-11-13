# plot_cash_rate_levels.R ----------------------------------------------------
#
# Build a chart that compares the raw implied cash rate path for the latest
# market scrape with snapshots from one day, one week, and one month earlier.
# The script expects to find the daily scrape data in either the
# `combined_data/all_data.Rds` file or in the `daily_data/` directory populated
# with the individual CSV extracts. The resulting chart is written to the
# `docs/` folder alongside the other published artefacts.

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(glue)
  library(here)
  library(lubridate)
  library(purrr)
  library(readr)
  library(tidyr)
})

# -----------------------------------------------------------------------------
# Helper functions ------------------------------------------------------------
# -----------------------------------------------------------------------------

# Attempt to load the combined data frame of all scrapes. Prefer the RDS
# because it avoids re-parsing every daily CSV, but gracefully fall back to the
# CSVs when necessary.
load_scrape_history <- function(root) {
  combined_path <- file.path(root, "combined_data", "all_data.Rds")
  csv_dir       <- file.path(root, "daily_data")

  if (file.exists(combined_path)) {
    message(glue("📄 Loading combined scrape history from {combined_path}"))
    data <- readRDS(combined_path)
  } else if (dir.exists(csv_dir)) {
    csv_files <- list.files(csv_dir, pattern = "\\\.csv$", full.names = TRUE)

    if (length(csv_files) == 0) {
      stop(glue("No CSV files were found in {csv_dir}. " ,
                "Cannot build the cash rate level chart."))
    }

    message(glue("📄 Loading {length(csv_files)} daily scrape files from {csv_dir}"))
    data <- map_dfr(csv_files, ~ read_csv(.x, show_col_types = FALSE))
  } else {
    stop(
      glue(
        "Unable to locate cash rate data. Expected either {combined_path} or the {csv_dir} directory."
      )
    )
  }

  data
}

# Given a target datetime, return the most recent scrape that occurred on or
# before that time. Returns NA when no such scrape exists.
select_scrape_at_or_before <- function(history, target_time) {
  candidates <- history %>%
    filter(!is.na(scrape_time), scrape_time <= target_time) %>%
    distinct(scrape_time) %>%
    arrange(desc(scrape_time)) %>%
    pull(scrape_time)

  if (length(candidates) == 0) {
    return(as.POSIXct(NA))
  }

  candidates[1]
}

# -----------------------------------------------------------------------------
# Main execution --------------------------------------------------------------
# -----------------------------------------------------------------------------

root_dir <- here::here()
out_dir  <- file.path(root_dir, "docs")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

scrape_history <- load_scrape_history(root_dir) %>%
  mutate(
    scrape_time  = lubridate::as_datetime(scrape_time, tz = "UTC"),
    meeting_date = lubridate::as_date(meeting_date)
  )

required_cols <- c("scrape_time", "meeting_date", "implied_mean")
missing_cols  <- setdiff(required_cols, names(scrape_history))

if (length(missing_cols) > 0) {
  stop(glue("Cash rate data is missing the following required columns: {glue_collapse(missing_cols, ', ')}"))
}

latest_scrape <- max(scrape_history$scrape_time, na.rm = TRUE)

reference_windows <- tibble::tibble(
  label        = c("Latest", "1 day ago", "1 week ago", "1 month ago"),
  offset_days  = c(0, 1, 7, 30)
) %>%
  mutate(
    target_time = latest_scrape - days(offset_days),
    scrape_time = map(target_time, ~ select_scrape_at_or_before(scrape_history, .x))
  ) %>%
  unnest(cols = scrape_time) %>%
  filter(!is.na(scrape_time)) %>%
  arrange(scrape_time)

if (nrow(reference_windows) == 0) {
  stop("Cash rate history does not contain any scrapes to plot.")
}

# Ensure we keep the label associated with the most recent offset when multiple
# labels resolve to the same scrape time (for example when the history is less
# than a week old). We retain the first occurrence by ordering the labels.
reference_windows <- reference_windows %>%
  mutate(label = factor(label, levels = c("Latest", "1 day ago", "1 week ago", "1 month ago"))) %>%
  arrange(label) %>%
  distinct(scrape_time, .keep_all = TRUE)

plot_data <- scrape_history %>%
  semi_join(reference_windows, by = "scrape_time") %>%
  left_join(reference_windows, by = "scrape_time") %>%
  mutate(
    label = forcats::fct_reorder(label, scrape_time, .desc = TRUE),
    meeting_date = as.Date(meeting_date)
  )

subtitle_text <- glue(
  "Latest scrape collected at {format(lubridate::with_tz(latest_scrape, 'Australia/Sydney'), '%Y-%m-%d %H:%M %Z')}"
)

cash_rate_plot <- ggplot(plot_data, aes(x = meeting_date, y = implied_mean, colour = label, group = label)) +
  geom_line(linewidth = 0.9, alpha = 0.9) +
  geom_point(size = 2.1, alpha = 0.9) +
  scale_colour_brewer(palette = "Dark2", direction = -1) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  scale_y_continuous(labels = scales::label_number(suffix = "%")) +
  labs(
    title    = "Implied cash rate path across recent scrapes",
    subtitle = subtitle_text,
    x        = "Meeting date",
    y        = "Implied cash rate",
    colour   = "Scrape"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

output_path <- file.path(out_dir, "cash_rate_levels.png")

ggsave(
  filename = output_path,
  plot     = cash_rate_plot,
  width    = 10,
  height   = 6,
  dpi      = 300
)

message(glue("💾 Saved cash rate levels chart to {output_path}"))
