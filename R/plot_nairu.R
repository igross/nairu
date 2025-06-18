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
  # Extract file date from filename (expect YYYY-MM-DD.csv)
  fname      <- basename(path)
  date_str   <- tools::file_path_sans_ext(fname)
  file_date_d<- as.Date(date_str)
  # Convert to year-quarter
  file_date  <- as.yearqtr(file_date_d)

  # Read NAIRU data
  df <- suppressMessages(read_csv(path, comment = "#", show_col_types = FALSE))
  if (nrow(df) == 0 || !"median" %in% names(df)) return(tibble())
  df <- ensure_dates(df)

  # Find matching quarter in data
  idx <- which(df$date == file_date)
  if (length(idx) == 0) idx <- which.max(df$date)
  nairu_val <- df$median[idx]

  tibble(
    max_date     = file_date,
    nairu_latest = nairu_val
  )
}

fmt_yq <- function(yq) format(yq, "%Y Q%q")
 function(yq) format(yq, "%Y Q%q")

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
# Build summary_df robustly using pmap to avoid NA condition issues
summary_df <- tmp_df %>%
  arrange(max_date) %>%
  mutate(prev_max = lag(max_date)) %>%
  # Compute new_qtrs per row
  mutate(
    new_qtrs = purrr::pmap_chr(
      list(prev_max, max_date),
      function(prev, curr) {
        if (is.na(curr) || is.na(prev)) {
          fmt_yq(curr)
        } else if (curr <= prev) {
          fmt_yq(curr)
        } else {
          seq(prev + 0.25, curr, 0.25) %>%
            purrr::map_chr(fmt_yq) %>%
            paste(collapse = ", ")
        }
      }
    ),
    # Determine release type by quarter-month
    release_type = purrr::map_chr(max_date, function(d) {
      m <- lubridate::month(as.Date(d))
      if (m %in% table_month$CPI) {
        "CPI"
      } else {
        "GDP"
      }
    })
  ) %>%
  ungroup()

# ---- Debug: print summary_df to console ----
print(summary_df)

# Remove any duplicate quarters (e.g. two Q3 runs) to prevent stacked bars
summary_df <- summary_df %>% distinct(new_qtrs, .keep_all = TRUE)


print(summary_df)

# Relabel any "NA" release_type to "GDP" for chart consistency
summary_df <- summary_df %>%
  mutate(release_type = ifelse(release_type == "NA", "GDP", release_type))

# Add an explicit index for each vintage to ensure discrete bar positions
summary_df <- summary_df %>% mutate(idx = row_number())

# Plot using index for x-axis to separate duplicate quarters
p3 <- ggplot(summary_df, aes(x = factor(idx), y = nairu_latest, fill = release_type)) +
  geom_col(width = 0.7) +
  scale_x_discrete(labels = paste0(summary_df$release_type, "\n", summary_df$new_qtrs))  # show release type and quarter +  # show release type and quarter
  labs(
    title = "Most-recent NAIRU estimates by data release type",
    x     = "Release (type and quarter)",
    y     = "NAIRU (%)",
    fill  = "Release"
  ) +
  my_theme

ggsave(file.path(output_dir, "nairu_last8_bar.png"), p3, width = 9, height = 5, dpi = 300)
saveWidget(as_widget(ggplotly(p3)), file.path(output_dir, "nairu_last8_bar.html"))
message("Figure 3 saved")

# ---- 9. Figure 4: All vintages series colored by vintage ----
# Read all vintage files including baseline
all_files <- c(csv_in, list.files(vintage_dir, pattern = "\\.csv$", full.names = TRUE))  # properly escape backslash for regex
labels    <- c("Baseline", tools::file_path_sans_ext(basename(list.files(vintage_dir, pattern = "\\.csv$", full.names = FALSE))))  # escape backslash in CSV pattern
vintages_df <- map2_dfr(all_files, labels, function(path, label) {
  df <- suppressMessages(read_csv(path, show_col_types = FALSE)) %>% ensure_dates()
  df %>% mutate(vintage = label)
})

# Determine unique vintages and assign colors: rainbow for old, black for baseline
total <- length(unique(vintages_df$vintage))
palette <- rainbow(total - 1)
color_map <- setNames(c(palette, "black"), c(sort(unique(vintages_df$vintage))[unique(vintages_df$vintage) != "Baseline"], "Baseline"))

p4 <- ggplot(vintages_df, aes(x = date, y = median, color = vintage)) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = color_map) +
  labs(title = "NAIRU estimates across all vintages",
       x = "Year", y = "NAIRU (%)", color = "Vintage") +
  my_theme

ggsave(file.path(output_dir, "nairu_all_vintages.png"), p4, width = 8, height = 5, dpi = 300)
saveWidget(as_widget(ggplotly(p4)), file.path(output_dir, "nairu_all_vintages.html"))
message("Figure 4 saved: all vintages.png and .html")
