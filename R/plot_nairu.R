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
library(lubridate)

# ---- 2. Set up file paths -----------------------------------------------
target_dir  <- getwd()
csv_in      <- file.path(target_dir, "output", "NAIRU_baseline.csv")
vintage_dir <- file.path(target_dir, "output", "vintages")
output_dir  <- file.path(target_dir, "output")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# ---- 3. ABS release months (approximate) ---------------------------------
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
  # filename "YYYY-MM-DD.csv"
  fname     <- basename(path)
  date_str  <- tools::file_path_sans_ext(fname)
  pub_date  <- as.Date(date_str)
  file_date <- as.yearqtr(pub_date)

  df <- suppressMessages(read_csv(path, show_col_types = FALSE))
  if (nrow(df) == 0 || !"median" %in% names(df)) return(tibble())
  df <- ensure_dates(df)

  idx <- which(df$date == file_date)
  if (length(idx) == 0) idx <- which.max(df$date)
  nairu_val <- df$median[idx]

  tibble(
    pub_date     = pub_date,
    max_date     = file_date,
    nairu_latest = nairu_val
  )
}

fmt_yq <- function(yq) format(yq, "%Y Q%q")

# Custom theme ------------------------------------------------------------
my_theme <- theme_bw() +
  theme(
    axis.text.x            = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y            = element_text(size = 12),
    axis.title.x           = element_text(size = 14),
    axis.title.y           = element_text(size = 14),
    legend.position.inside = c(1.02, 0.5)
  )

library(janitor)

# ---- 5. Load baseline NAIRU data -----------------------------------------
nairu_df <- read_csv(csv_in, show_col_types = FALSE) %>%
  clean_names() %>%                       # variable, median, lowera, ...
  rename(
    lower = lowera,                      # use the 'a' interval
    upper = uppera
  ) %>%
  mutate(
    date_qtr = as.yearqtr(date),         # keep as yearqtr for pretty ticks
    date     = as.Date(date_qtr)         # Plotly likes Date objects
  ) %>%
  filter(date_qtr >= as.yearqtr("2010 Q1")) %>%   # post-GFC sample
  arrange(date_qtr)

nairu_df %>%
  summarise(
    n_rows   = n(),
    n_lower  = sum(!is.na(lower)),
    n_upper  = sum(!is.na(upper)),
    min_gap  = min(upper - lower, na.rm = TRUE),
    any_reversed = any(lower >= upper, na.rm = TRUE)
  )


# quick sanity check in the CI log
message("Loaded ", nrow(nairu_df), " rows – ",
        sum(!is.na(nairu_df$median)), " have a median value")

# ---- 6. Figure 1: full history ------------------------------------------
p1 <- ggplot(nairu_df, aes(x = date)) +
  geom_ribbon(
    data = nairu_df,
    aes(ymin = lower, ymax = upper),
    fill   = "orange",
    alpha  = 0.3,
    colour = NA          # border off; set to "black" if you want an outline
  ) +
  geom_line(aes(y = median, group = 1),
            colour = "red",  linewidth = 1) +
  geom_line(aes(y = lur,    group = 1),
            colour = "blue", linewidth = 0.8) +
  geom_point(data = slice_tail(nairu_df, n = 1),
             aes(y = median), colour = "black", size = 3) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(title = "NAIRU estimate with 90 % credible interval",
       x = "Year", y = "Percent") +
  my_theme

# ---- 7. Figure 2: zoom 2010-present -------------------------------------
# ---- Figure 2 : NAIRU zoom-in (2010 – present) ----------------------------
nairu_zoom <- nairu_df |>
  filter(date_qtr >= as.yearqtr("2010 Q1"))

p2 <- ggplot(nairu_zoom, aes(x = date)) +
  # 90 % credible-interval “fan”
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    fill  = "orange",
    alpha = 0.3
  ) +
  # NAIRU median and unemployment rate
  geom_line(aes(y = median, group = 1),
            colour = "red",  linewidth = 1) +
  geom_line(aes(y = lur,    group = 1),
            colour = "blue", linewidth = 0.8) +
  # Highlight the latest NAIRU point
  geom_point(data = slice_tail(nairu_zoom, n = 1),
             aes(y = median),
             colour = "black", size = 3) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title    = "NAIRU estimate (post-GFC)",
       subtitle = "90 % credible interval",
       x = "Quarter", y = "Percent") +
  my_theme

# save static & interactive versions
ggsave(file.path(output_dir, "nairu_zoom_2010.png"),
       p2, width = 8, height = 5, dpi = 300)

saveWidget(ggplotly(p2, tooltip = "text"),
           file.path(output_dir, "nairu_zoom_2010.html"))

message("Figure 2 saved")



# ---- 8. Figure 3: Most-recent by release type ---------------------------
types <- list.files(vintage_dir, pattern = "\\.csv$", full.names = TRUE)
last8 <- head(types[order(file.info(types)$mtime, decreasing = TRUE)], 8)
tmp_df <- map_dfr(last8, read_vintage_safe)

summary_df <- tmp_df %>%
  arrange(max_date) %>%
  mutate(prev_max = lag(max_date)) %>%
  mutate(
    new_qtrs = pmap_chr(list(prev_max, max_date), ~ {
      prev <- ..1; curr <- ..2
      if (is.na(curr) || is.na(prev)) fmt_yq(curr)
      else if (curr <= prev) fmt_yq(curr)
      else paste(seq(prev + 0.25, curr, 0.25) %>% map_chr(fmt_yq), collapse = ", ")
    }),
    release_type = map_chr(pub_date, ~ if (month(.x) %in% table_month$CPI) "CPI" else "GDP")
  ) %>%
  ungroup() %>%
  filter(!is.na(max_date)) %>%
  distinct(new_qtrs, .keep_all = TRUE) %>%
  mutate(idx = row_number())

# Debug: inspect summary_df
print(summary_df)

p3 <- ggplot(
  summary_df,
  aes(x = factor(idx), y = nairu_latest, fill = release_type,
      text = paste0("Release: ", new_qtrs, "<br>NAIRU: ", nairu_latest))
) +
  geom_col(width = 0.7) +
  scale_x_discrete(labels = paste0(summary_df$release_type, "\n", summary_df$new_qtrs)) +
  labs(title = "Most-recent NAIRU estimates by release type",
       x = "Release (type and quarter)", y = "NAIRU (%)",
       fill = "Release") +
  my_theme

ggsave(file.path(output_dir, "nairu_last8_bar.png"), p3, width = 9, height = 5, dpi = 300)
saveWidget(ggplotly(p3, tooltip = "text"), file.path(output_dir, "nairu_last8_bar.html"))
message("Figure 3 saved")

# ---- 9. Figure 4: All vintages series colored ---------------------------
# Use map2_dfr to attach vintage label clearly from filename
files      <- list.files(vintage_dir, pattern = "\\.csv$", full.names = TRUE)
labels     <- tools::file_path_sans_ext(basename(files))
vintages_df <- map2_dfr(files, labels, function(path, label) {
  df <- suppressMessages(read_csv(path, show_col_types = FALSE)) %>% ensure_dates()
  df %>% mutate(vintage = label)
})
# Debug: inspect vintages_df
print(str(vintages_df)); print(head(vintages_df))

# 1. Identify all vintages, and pick out "Baseline" if it's there:
all_vints  <- unique(vintages_df$vintage)
old_labels <- setdiff(all_vints, "Baseline")

vints <- unique(vintages_df$vintage)

if ("Baseline" %in% vints) {
  # one extra colour for Baseline
  palette   <- rainbow(length(vints) - 1)
  color_map <- setNames(
    c(palette, "black"),
    c(setdiff(vints, "Baseline"), "Baseline")
  )
} else {
  # no Baseline; one-for-one mapping
  palette   <- rainbow(length(vints))
  color_map <- setNames(palette, vints)
}


p4 <- ggplot(
  vintages_df,
  aes(x = date, y = median, color = vintage,
      text = paste0("Date: ", date, "<br>NAIRU: ", median))
) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = color_map) +
  labs(title = "NAIRU estimates across all vintages",
       x = "Year", y = "NAIRU (%)", color = "Vintage") +
  my_theme

ggsave(file.path(output_dir, "nairu_all_vintages.png"), p4, width = 8, height = 5, dpi = 300)
saveWidget(ggplotly(p4, tooltip = "text"), file.path(output_dir, "nairu_all_vintages.html"))
message("Figure 4 saved: all vintages")

                           
