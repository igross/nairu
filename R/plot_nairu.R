#!/usr/bin/env Rscript
# ------------------------------------------------------------
#  Plot NAIRU history with 90 % bands + latest observation
# ------------------------------------------------------------
library(ggplot2)
library(readr)
library(zoo)     # for as.yearqtr
library(dplyr)
library(here)

# ---- paths --------------------------------------------------
root      <- Sys.getenv("GITHUB_WORKSPACE", unset = here::here())
csv_in    <- file.path(root, "output", "NAIRU_baseline.csv")
png_out   <- file.path(root, "output", "nairu_history.png")


nairu_df <- read_csv(csv_in, show_col_types = FALSE) |>
  mutate(date = as.yearqtr(date)) |>
  filter(date >= as.yearqtr("2010 Q1")) |>
  arrange(date)

# ---- plot ---------------------------------------------------
p <- ggplot(nairu_df, aes(x = date)) +
  geom_ribbon(aes(ymin = lowera, ymax = uppera),
              fill = "orange", alpha = 0.30) +
  geom_line(aes(y = median), colour = "red",  linewidth = 1) +
  geom_line(aes(y = LUR),    colour = "blue", linewidth = 0.8) +
  geom_point(data = slice_tail(nairu_df, n = 1),
             aes(y = median), colour = "black", size = 3) +
  scale_x_continuous(breaks = pretty(nairu_df$date, n = 10)) +
  labs(title    = "NAIRU estimate with 90 % credible interval",
       subtitle = "Red line = median, orange band = 5–95 %; blue = actual LUR",
       x = "Year", y = "Percent") +
  theme_minimal(base_size = 12)

ggsave(png_out, p, width = 8, height = 5, dpi = 300, bg = "white")
message("Plot written to: ", png_out)


#!/usr/bin/env Rscript
# ------------------------------------------------------------
#  Zoom-in plot: NAIRU vs LUR, 2010-present
# ------------------------------------------------------------
library(ggplot2); library(readr); library(dplyr); library(zoo); library(here)

root      <- Sys.getenv("GITHUB_WORKSPACE", unset = here::here())
csv_in    <- file.path(root, "output", "NAIRU_baseline.csv")
png_out   <- file.path(root, "output", "nairu_zoom_2010.png")

nairu_df <- read_csv(csv_in, show_col_types = FALSE) |>
  mutate(date = as.yearqtr(date)) |>
  filter(date >= as.yearqtr("2010 Q1")) |>
  arrange(date)

p <- ggplot(nairu_df, aes(x = date)) +
  geom_ribbon(aes(ymin = lowera, ymax = uppera),
              fill = "orange", alpha = 0.3) +
  geom_line(aes(y = median), colour = "red", size = 1) +
  geom_line(aes(y = LUR),    colour = "blue", size = 0.8) +
  labs(title    = "NAIRU estimate (post-GFC) with 90 % band",
       subtitle = "Red = median NAIRU, blue = LUR",
       x = "", y = "Percent") +
  theme_minimal(base_size = 12)

ggsave(png_out, p, width = 8, height = 5, dpi = 300, bg = "white")
message("Plot written to: ", png_out)


#!/usr/bin/env Rscript
# ------------------------------------------------------------
#  Heat-map of unemployment gap
# ------------------------------------------------------------
library(ggplot2); library(readr); library(dplyr); library(zoo); library(here)

root   <- Sys.getenv("GITHUB_WORKSPACE", unset = here::here())
csv_in <- file.path(root, "output", "NAIRU_baseline.csv")
png_out<- file.path(root, "output", "unemp_gap_heat.png")

gap_df <- read_csv(csv_in, show_col_types = FALSE) |>
  mutate(date  = as.yearqtr(date),
         year  = factor(floor(as.numeric(date))),
         quar  = factor(cycle(date), levels = 1:4,
                        labels = c("Q1","Q2","Q3","Q4")),
         gap   = LUR - median)

p <- ggplot(gap_df, aes(x = year, y = quar, fill = gap)) +
  geom_tile() +
  scale_fill_gradient2(low = "steelblue", mid = "white", high = "firebrick",
                       midpoint = 0, name = "LUR – NAIRU") +
  labs(title = "Unemployment-gap heat-map",
       x = "Year", y = "") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

ggsave(png_out, p, width = 9, height = 4.5, dpi = 300, bg = "white")
message("Plot written to: ", png_out)


#!/usr/bin/env Rscript
# ------------------------------------------------------------
#  NAIRU vintages comparison
# ------------------------------------------------------------
library(ggplot2); library(readr); library(dplyr); library(tidyr); library(purrr); library(zoo); library(here)

root        <- Sys.getenv("GITHUB_WORKSPACE", unset = here::here())
latest_csv  <- file.path(root, "output", "NAIRU_baseline.csv")
vintage_dir <- file.path(root, "output", "vintages")
png_out     <- file.path(root, "output", "nairu_vintages.png")

# helper to read a csv and tag its vintage
# ---- data ---------------------------------------------------
ensure_dates <- function(df, start_qtr = "1997 Q3") {
  # Add dates if the file doesn’t have any (case-insensitive test)
  if (!any(tolower(names(df)) == "date")) {
    start <- as.yearqtr(start_qtr)              # e.g. 1997 Q3
    df$date <- start + (seq_len(nrow(df)) - 1) / 4
  }
  # Make sure it’s a yearqtr object for ggplot & dplyr to play nicely
  df %>% mutate(date = as.yearqtr(.data[["date"]]))
}

# ── 2. Handy reader for vintage files (uses ensure_dates) ──
read_vintage <- function(path, label, start_qtr = "1997 Q3") {
  read_csv(path, show_col_types = FALSE) %>%
    ensure_dates(start_qtr) %>%                 # add/standardise dates
    mutate(vintage = label) %>%                 # tag the vintage
    select(date, median, vintage)               # keep what you need
}


# collect all csvs (latest + vintages)
files <- c(latest_csv, list.files(vintage_dir, pattern = "\\.csv$", full.names = TRUE))
labels<- c("Latest", tools::file_path_sans_ext(basename(list.files(vintage_dir, "\\.csv$"))))

vintages_df <- map2_dfr(files, labels, read_vintage)

p <- ggplot(vintages_df, aes(x = date, y = median, colour = vintage)) +
  geom_line(size = 0.8) +
  labs(title    = "NAIRU estimate – vintage comparison",
       subtitle = "Revisions across estimation runs",
       x = "Year", y = "Percent", colour = "Vintage") +
  theme_minimal(base_size = 12)

ggsave(png_out, p, width = 8, height = 5, dpi = 300, bg = "white")
message("Plot written to: ", png_out)

# ---- helper -------------------------------------------------
ensure_dates <- function(df, start_qtr = "1997 Q3") {
  if (!any(tolower(names(df)) == "date")) {
    start <- as.yearqtr(start_qtr)
    df$date <- start + (seq_len(nrow(df)) - 1) / 4
  }
  mutate(df, date = as.yearqtr(.data[["date"]]))
}

read_vintage_safe <- function(path) {
  df <- suppressMessages(
          read_csv(path, comment = "#", show_col_types = FALSE)
        )

  # skip Stan diagnostic files (no 'median' column or zero rows)
  if (nrow(df) == 0 || !"median" %in% names(df)) return(NULL)

  df %>% 
    ensure_dates() %>% 
    slice_tail(n = 1) %>%          # keep most-recent obs only
    transmute(vintage = basename(path),
              nairu   = median)
}

# ---- paths --------------------------------------------------
png_out     <- file.path(root, "output", "nairu_last_8_vintages.png")

# ---- collect last-8 vintages -------------------------------
all_files <- list.files(vintage_dir, pattern = "\\.csv$", full.names = TRUE)

# order newest → oldest, drop NAs, then take at most eight
ordered  <- all_files[order(file.info(all_files)$mtime, decreasing = TRUE)]
ordered  <- ordered[!is.na(ordered)]             # just in case
last8    <- head(ordered, 8)                     # <=8 files, never NA

summary_df <- purrr::map_dfr(last8, read_vintage_safe)

# bail if we ended up with fewer than 2 usable vintages
stopifnot(nrow(summary_df) >= 2)

# ---- bar chart ---------------------------------------------
p <- ggplot(summary_df,
            aes(x = reorder(vintage, nairu), y = nairu)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Most-recent NAIRU estimate in each of the last 8 vintages",
       x = "Vintage file", y = "NAIRU") +
  theme_minimal(base_size = 12)

ggsave(png_out, p, width = 8, height = 4.5, dpi = 300, bg = "white")
message("Bar chart written to: ", png_out)
