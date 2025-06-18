#!/usr/bin/env Rscript
# ------------------------------------------------------------
# NAIRU Analysis and Interactive Visualization Script
# ------------------------------------------------------------
# Author: [Your Name]
# Date: [YYYY-MM-DD]

# ---- 1. Load required libraries ------------------------------------------
library(ggplot2)
library(readr)
library(dplyr)
library(zoo)
library(purrr)
library(tidyr)
library(here)
library(plotly)
library(htmlwidgets)
library(lubridate)  # for month() helper

# ---- 2. Set up file paths and root directory -----------------------------
root        <- Sys.getenv("GITHUB_WORKSPACE", unset = here::here())
csv_in      <- file.path(root, "output", "NAIRU_baseline.csv")
vintage_dir <- file.path(root, "output", "vintages")
output_dir  <- file.path(root, "output")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# ---- 3. ABS quarterly release timetable ---------------------------------
# CPI releases in Jan/Apr/Jul/Oct, NA in Mar/Jun/Sep/Dec (approximate dates)
cpi_dates <- as.Date(c(
  "2025-01-29","2025-04-30","2025-07-30","2025-10-29",
  "2026-01-28","2026-04-29","2026-07-29","2026-10-28",
  "2027-01-27","2027-04-28","2027-07-28","2027-10-27"
))
na_dates <- as.Date(c(
  "2025-03-05","2025-06-04","2025-09-03","2025-12-03",
  "2026-03-04","2026-06-03","2026-09-02","2026-12-02",
  "2027-03-03","2027-06-02","2027-09-01","2027-12-01"
))

# ---- 4. Helper functions ------------------------------------------------
ensure_dates <- function(df, start_qtr = "1997 Q3") {
  if (!"date" %in% tolower(names(df))) {
    start <- as.yearqtr(start_qtr)
    df$date <- start + (seq_len(nrow(df)) - 1) / 4
  }
  df %>% mutate(date = as.yearqtr(.data[["date"]]))
}
read_vintage <- function(path, label, start_qtr = "1997 Q3") {
  read_csv(path, show_col_types = FALSE) %>%
    ensure_dates(start_qtr) %>%
    mutate(vintage = label) %>%
    select(date, median, vintage)
}
read_vintage_safe <- function(path) {
  if (is.na(path) || !nzchar(path)) return(tibble())
  df <- suppressMessages(read_csv(path, comment = "#", show_col_types = FALSE))
  if (nrow(df) == 0 || !"median" %in% names(df)) return(tibble())
  df %>% ensure_dates() %>% arrange(date) %>% summarise(
    max_date     = max(date),
    nairu_latest = median[which.max(date)]
  )
}
fmt_yq <- function(yq) format(yq, "%Y Q%q")

# Custom theme for all plots -----------------------------------------------
my_theme <- theme_bw() +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y  = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.position.inside = c(1.02, 0.5)  # use inside for ggplot2 ≥3.5.0
  )

# ---- 5. Load baseline NAIRU data -----------------------------------------
nairu_df <- read_csv(csv_in, show_col_types = FALSE) %>%
  mutate(date = as.yearqtr(date)) %>%
  filter(date >= as.yearqtr("2010 Q1")) %>%
  arrange(date)

# ---- 6. Figure 1: NAIRU history with 90% credible interval ----------------
p1 <- ggplot(nairu_df, aes(x = date)) +
  geom_ribbon(aes(ymin = lowera, ymax = uppera), fill = "orange", alpha = 0.3) +
  geom_line(aes(y = median), colour = "red", linewidth = 1) +
  geom_line(aes(y = LUR), colour = "blue", linewidth = 0.8) +
  geom_point(data = slice_tail(nairu_df, n = 1), aes(y = median), colour = "black", size = 3) +
  scale_x_continuous(breaks = pretty(nairu_df$date, n = 10)) +
  labs(title = "NAIRU estimate with 90% credible interval", x = "Year", y = "Percent") +
  my_theme
ggsave(file.path(output_dir, "nairu_history.png"), p1, width = 8, height = 5, dpi = 300)
saveWidget(as_widget(ggplotly(p1)), file.path(output_dir, "nairu_history.html"))
message("Figure 1 saved: nairu_history.png and .html")

# ---- 7. Figure 2: Zoom-in (2010-present) ----------------------------------
p2 <- ggplot(nairu_df, aes(x = date)) +
  geom_ribbon(aes(ymin = lowera, ymax = uppera), fill = "orange", alpha = 0.3) +
  geom_line(aes(y = median), colour = "red", linewidth = 1) +
  geom_line(aes(y = LUR), colour = "blue", linewidth = 0.8) +
  labs(title = "NAIRU estimate (post-GFC)", subtitle = "90% credible interval", x = "Quarter", y = "Percent") +
  my_theme
ggsave(file.path(output_dir, "nairu_zoom_2010.png"), p2, width = 8, height = 5, dpi = 300)
saveWidget(as_widget(ggplotly(p2)), file.path(output_dir, "nairu_zoom_2010.html"))
message("Figure 2 saved: nairu_zoom_2010.png and .html")

# ---- 8. Figure 3: NAIRU vintages comparison -------------------------------
types <- list.files(vintage_dir, pattern = "\\.csv$", full.names = TRUE)
labels <- c("Latest", tools::file_path_sans_ext(basename(types)))
vintages_df <- map2_dfr(c(csv_in, types), labels, read_vintage)

p3 <- ggplot(vintages_df, aes(x = date, y = median, colour = vintage)) +
  geom_line(linewidth = 0.8) +
  labs(title = "NAIRU estimate – vintage comparison", x = "Year", y = "Percent", colour = "Vintage") +
  my_theme
ggsave(file.path(output_dir, "nairu_vintages.png"), p3, width = 8, height = 5, dpi = 300)
saveWidget(as_widget(ggplotly(p3)), file.path(output_dir, "nairu_vintages.html"))
message("Figure 3 saved: nairu_vintages.png and .html")

# ---- 9. Figure 4: Most-recent NAIRU estimates across vintages ------------
latest_vintages <- list.files(vintage_dir, pattern = "\\.csv$", full.names = TRUE)
last8 <- head(latest_vintages[order(file.info(latest_vintages)$mtime, decreasing = TRUE)], 8)

summary_df <- map_dfr(last8, read_vintage_safe) %>%
  arrange(max_date) %>%
  mutate(prev_max = lag(max_date)) %>%
  rowwise() %>%
  mutate(
    # classify by month to avoid reliance on exact dates
    release_type = case_when(
      month(max_date) %in% c(1,4,7,10)  ~ "CPI",
      month(max_date) %in% c(3,6,9,12)  ~ "NA",
      TRUE                               ~ "Other"
    ),
    new_qtrs = if (is.na(prev_max)) {
      "—"
    } else if (max_date <= prev_max) {
      ""
    } else {
      paste(seq(prev_max + 0.25, max_date, 0.25) %>% fmt_yq(), collapse = ", ")
    }
  ) %>%
  ungroup() %>%
  mutate(vintage_label = factor(release_type, levels = c("CPI","NA","Other")))

p4 <- ggplot(summary_df, aes(x = vintage_label, y = nairu_latest)) +
  geom_col(fill = "steelblue") +
  labs(title = "Most-recent NAIRU estimates by data release type", x = "Release Type", y = "NAIRU (%)") +
  my_theme
ggsave(file.path(output_dir, "nairu_last8_bar.png"), p4, width = 9, height = 5, dpi = 300)
saveWidget(as_widget(ggplotly(p4)), file.path(output_dir, "nairu_last8_bar.html"))
message("Figure 4 saved: nairu_last8_bar.png and .html")
