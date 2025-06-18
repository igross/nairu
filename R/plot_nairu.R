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

# ---- 2. Set up file paths and root directory -----------------------------
root        <- Sys.getenv("GITHUB_WORKSPACE", unset = here::here())
csv_in      <- file.path(root, "output", "NAIRU_baseline.csv")
vintage_dir <- file.path(root, "output", "vintages")
output_dir  <- file.path(root, "output")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# ---- 3. Helper functions ------------------------------------------------
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
    vintage_file = basename(path),
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
    legend.position = c(1.02, 0.5)
  )

# ---- 4. Load baseline NAIRU data -----------------------------------------
nairu_df <- read_csv(csv_in, show_col_types = FALSE) %>%
  mutate(date = as.yearqtr(date)) %>%
  filter(date >= as.yearqtr("2010 Q1")) %>%
  arrange(date)

# ---- 5. Figure 1: NAIRU history with 90% credible interval ----------------
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

# ---- 6. Figure 2: Zoom-in (2010-present) ----------------------------------
p2 <- ggplot(nairu_df, aes(x = date)) +
  geom_ribbon(aes(ymin = lowera, ymax = uppera), fill = "orange", alpha = 0.3) +
  geom_line(aes(y = median), colour = "red", linewidth = 1) +
  geom_line(aes(y = LUR), colour = "blue", linewidth = 0.8) +
  labs(title = "NAIRU estimate (post-GFC)", subtitle = "90% credible interval", x = "Quarter", y = "Percent") +
  my_theme
ggsave(file.path(output_dir, "nairu_zoom_2010.png"), p2, width = 8, height = 5, dpi = 300)
saveWidget(as_widget(ggplotly(p2)), file.path(output_dir, "nairu_zoom_2010.html"))
message("Figure 2 saved: nairu_zoom_2010.png and .html")

# ---- 7. Figure 3: NAIRU vintages comparison -------------------------------
types <- list.files(vintage_dir, pattern = "\\.csv$", full.names = TRUE)
files <- c(csv_in, types)
labels <- c("Latest", tools::file_path_sans_ext(basename(types)))
vintages_df <- map2_dfr(files, labels, read_vintage)

p3 <- ggplot(vintages_df, aes(x = date, y = median, colour = vintage)) +
  geom_line(linewidth = 0.8) +
  labs(title = "NAIRU estimate – vintage comparison", x = "Year", y = "Percent", colour = "Vintage") +
  my_theme
ggsave(file.path(output_dir, "nairu_vintages.png"), p3, width = 8, height = 5, dpi = 300)
saveWidget(as_widget(ggplotly(p3)), file.path(output_dir, "nairu_vintages.html"))
message("Figure 3 saved: nairu_vintages.png and .html")

# ---- 8. Figure 4: Most-recent NAIRU estimates across vintages ------------
latest_vintages <- list.files(vintage_dir, pattern = "\\.csv$", full.names = TRUE)
ordered <- latest_vintages[order(file.info(latest_vintages)$mtime, decreasing = TRUE)]
last8 <- head(ordered, 8)
summary_df <- map_dfr(last8, read_vintage_safe) %>%
  arrange(max_date) %>%
  mutate(prev_max = lag(max_date)) %>%
  rowwise() %>%
  mutate(new_qtrs = if (is.na(prev_max)) "—" else if (max_date <= prev_max) "" else paste(seq(prev_max + 0.25, max_date, 0.25) %>% fmt_yq(), collapse = ", ")) %>%
  ungroup() %>%
  mutate(vintage_label = new_qtrs)

p4 <- ggplot(summary_df, aes(x = vintage_label, y = nairu_latest)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = vintage_label), vjust = -0.4, size = 3) +
  labs(title = "Most-recent NAIRU estimates by data added", x = "Quarters added", y = "NAIRU (%)") +
  my_theme
ggsave(file.path(output_dir, "nairu_last8_bar.png"), p4, width = 9, height = 5, dpi = 300)
saveWidget(as_widget(ggplotly(p4)), file.path(output_dir, "nairu_last8_bar.html"))
message("Figure 4 saved: nairu_last8_bar.png and .html")
