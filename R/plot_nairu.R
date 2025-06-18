#!/usr/bin/env Rscript
# ------------------------------------------------------------
# NAIRU Analysis and Visualization Script
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

# ---- 2. Set up file paths and root directory -----------------------------
root        <- Sys.getenv("GITHUB_WORKSPACE", unset = here::here())
csv_in      <- file.path(root, "output", "NAIRU_baseline.csv")
vintage_dir <- file.path(root, "output", "vintages")
output_dir  <- file.path(root, "output")

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
  df %>%
    ensure_dates() %>%
    arrange(date) %>%
    summarise(
      vintage_file = basename(path),
      max_date     = max(date),
      nairu_latest = median[which.max(date)]
    )
}

fmt_yq <- function(yq) format(yq, "%Y Q%q")

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
  labs(title = "NAIRU estimate with 90% credible interval",
       subtitle = "Red = median; orange band = 5–95%; blue = actual LUR",
       x = "Year", y = "Percent") +
  theme_minimal(base_size = 12)
ggsave(file.path(output_dir, "nairu_history.png"), p1, width = 8, height = 5, dpi = 300)
message("Figure 1 saved: nairu_history.png")

# ---- 6. Figure 2: Zoom-in (2010-present) ----------------------------------
p2 <- ggplot(nairu_df, aes(x = date)) +
  geom_ribbon(aes(ymin = lowera, ymax = uppera), fill = "orange", alpha = 0.3) +
  geom_line(aes(y = median), colour = "red", linewidth = 1) +
  geom_line(aes(y = LUR), colour = "blue", linewidth = 0.8) +
  labs(title = "NAIRU estimate (post-GFC) with 90% band",
       subtitle = "Red = median NAIRU; blue = LUR",
       x = NULL, y = "Percent") +
  theme_minimal(base_size = 12)
ggsave(file.path(output_dir, "nairu_zoom_2010.png"), p2, width = 8, height = 5, dpi = 300)
message("Figure 2 saved: nairu_zoom_2010.png")

# ---- 7. Figure 3: Unemployment-gap heat-map -------------------------------
gap_df <- nairu_df %>%
  mutate(
    year = factor(floor(as.numeric(date))),
    quar = factor(cycle(date), levels = 1:4, labels = c("Q1","Q2","Q3","Q4")),
    gap  = LUR - median
  )

p3 <- ggplot(gap_df, aes(x = year, y = quar, fill = gap)) +
  geom_tile() +
  scale_fill_gradient2(low = "steelblue", mid = "white", high = "firebrick", midpoint = 0, name = "LUR – NAIRU") +
  labs(title = "Unemployment-gap heat-map", x = "Year", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
ggsave(file.path(output_dir, "unemp_gap_heat.png"), p3, width = 9, height = 4.5, dpi = 300)
message("Figure 3 saved: unemp_gap_heat.png")

# ---- 8. Figure 4: NAIRU vintages comparison -------------------------------
types <- list.files(vintage_dir, pattern = "\\.csv$", full.names = TRUE)
files <- c(csv_in, types)
labels <- c("Latest", tools::file_path_sans_ext(basename(types)))
vintages_df <- map2_dfr(files, labels, read_vintage)

p4 <- ggplot(vintages_df, aes(x = date, y = median, colour = vintage)) +
  geom_line(linewidth = 0.8) +
  labs(title = "NAIRU estimate – vintage comparison",
       subtitle = "Revisions across estimation runs",
       x = "Year", y = "Percent", colour = "Vintage") +
  theme_minimal(base_size = 12)
ggsave(file.path(output_dir, "nairu_vintages.png"), p4, width = 8, height = 5, dpi = 300)
message("Figure 4 saved: nairu_vintages.png")

# ---- 9. Figure 5: Most-recent NAIRU estimates across vintages ------------
latest_vintages <- list.files(vintage_dir, pattern = "\\.csv$", full.names = TRUE)
ordered <- latest_vintages[order(file.info(latest_vintages)$mtime, decreasing = TRUE)]
last8 <- head(ordered, 8)

summary_df <- map_dfr(last8, read_vintage_safe) %>%
  arrange(max_date) %>%
  mutate(prev_max = lag(max_date)) %>%
  rowwise() %>%
  mutate(new_qtrs = if (is.na(prev_max)) {
                      "—"
                    } else if (max_date <= prev_max) {
                      ""
                    } else {
                      paste(
                        seq(prev_max + 0.25, max_date, 0.25) %>% fmt_yq(),
                        collapse = ", "
                      )
                    }) %>%
  ungroup() %>%
  mutate(vintage_lab = factor(vintage_file, levels = vintage_file))

p5 <- ggplot(summary_df, aes(x = vintage_lab, y = nairu_latest)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = new_qtrs), vjust = -0.4, size = 3) +
  labs(title = "Most-recent NAIRU estimate across successive vintages",
       subtitle = "Text labels show the quarter(s) newly incorporated since the prior vintage",
       x = "Vintage (ordered by data coverage)", y = "NAIRU (%)") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))
ggsave(file.path(output_dir, "nairu_last8_bar.png"), p5, width = 9, height = 5, dpi = 300)
message("Figure 5 saved: nairu_last8_bar.png")
