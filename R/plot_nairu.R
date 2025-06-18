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
library(plotly)
library(htmlwidgets)
library(lubridate)  # for month() helper

# ---- 2. Set up file paths without using 'here' ---------------------------
# Assume script is run from project root
target_dir <- getwd()
csv_in      <- file.path(target_dir, "output", "NAIRU_baseline.csv")
vintage_dir <- file.path(target_dir, "output", "vintages")
output_dir  <- file.path(target_dir, "output")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# ---- 3. ABS release timetable (approximate) -------------------------------
# CPI in Jan/Apr/Jul/Oct; National Accounts in Mar/Jun/Sep/Dec
table_month <- list(
  CPI      = c(1, 4, 7, 10),
  NA_month = c(3, 6, 9, 12)
)

# ---- 4. Helper functions ------------------------------------------------
ensure_dates <- function(df, start_qtr = "1997 Q3") {
  if (!"date" %in% tolower(names(df))) {
    start <- as.yearqtr(start_qtr)
    df$date <- start + (seq_len(nrow(df)) - 1) / 4
  }
  df %>% mutate(date = as.yearqtr(.data[["date"]]))
}

read_vintage_safe <- function(path) {
  if (is.na(path) || !nzchar(path)) return(tibble())
  df <- suppressMessages(read_csv(path, comment = "#", show_col_types = FALSE))
  if (nrow(df) == 0 || !"median" %in% names(df)) return(tibble())
  df %>%
    ensure_dates() %>%
    arrange(date) %>%
    summarise(
      max_date     = max(date),
      nairu_latest = median[which.max(date)]
    )
}

fmt_yq <- function(yq) format(yq, "%Y Q%q")

# Custom theme for all plots -----------------------------------------------
my_theme <- theme_bw() +
  theme(
    axis.text.x            = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y            = element_text(size = 12),
    axis.title.x           = element_text(size = 14),
    axis.title.y           = element_text(size = 14),
    legend.position.inside = c(1.02, 0.5)
  )

# ---- 5. Load baseline NAIRU data -----------------------------------------
nairu_df <- read_csv(csv_in, show_col_types = FALSE) %>%
  mutate(date = as.yearqtr(date)) %>%
  filter(date >= as.yearqtr("2010 Q1")) %>%
  arrange(date)

# ---- 6. Figure 1: NAIRU history ------------------------------------------
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
message("Figure 1 saved")

# ---- 7. Figure 2: Zoom-in (2010-present) ----------------------------------
p2 <- ggplot(nairu_df, aes(x = date)) +
  geom_ribbon(aes(ymin = lowera, ymax = uppera), fill = "orange", alpha = 0.3) +
  geom_line(aes(y = median), colour = "red", linewidth = 1) +
  geom_line(aes(y = LUR), colour = "blue", linewidth = 0.8) +
  labs(title = "NAIRU estimate (post-GFC)", subtitle = "90% credible interval", x = "Quarter", y = "Percent") +
  my_theme

ggsave(file.path(output_dir, "nairu_zoom_2010.png"), p2, width = 8, height = 5, dpi = 300)
saveWidget(as_widget(ggplotly(p2)), file.path(output_dir, "nairu_zoom_2010.html"))
message("Figure 2 saved")

# ---- 8. Figure 3: Vintage comparison ------------------------------------
types <- list.files(vintage_dir, pattern = "\\.csv$", full.names = TRUE)
last8 <- head(types[order(file.info(types)$mtime, decreasing = TRUE)], 8)

tmp_df <- map_dfr(last8, read_vintage_safe)
summary_df <- tmp_df %>%
  arrange(max_date) %>%
  mutate(prev_max = lag(max_date)) %>%
  rowwise() %>%
  mutate(
    new_qtrs = if (is.na(prev_max)) fmt_yq(max_date)
              else if (max_date <= prev_max) fmt_yq(max_date)
              else paste(seq(prev_max + 0.25, max_date, 0.25) %>% fmt_yq(), collapse = ", "),
    release_type = if (month(max_date) %in% table_month$CPI)       "CPI"
                   else if (month(max_date) %in% table_month$NA_month) "NA"
                   else "Other"
  ) %>%
  ungroup()

gg_x <- factor(summary_df$new_qtrs, levels = unique(summary_df$new_qtrs))  # avoid duplicated factor levels
p3 <- ggplot(summary_df, aes(x = gg_x, y = nairu_latest, fill = release_type)) +
  geom_col(width = 0.7) +
  scale_x_discrete(labels = summary_df$release_type) +
  labs(title = "Most-recent NAIRU estimates by data release type",
       x = "Release Type", y = "NAIRU (%)", fill = "Release") +
  my_theme

ggsave(file.path(output_dir, "nairu_last8_bar.png"), p3, width = 9, height = 5, dpi = 300)
saveWidget(as_widget(ggplotly(p3)), file.path(output_dir, "nairu_last8_bar.html"))
message("Figure 3 saved")
