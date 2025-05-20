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

# ---- data ---------------------------------------------------
nairu_df <- read_csv(csv_in, show_col_types = FALSE) %>%
  mutate(date = as.yearqtr(date)) %>%
  arrange(date)

# ---- plot ---------------------------------------------------
p <- ggplot(nairu_df, aes(x = date)) +
  geom_ribbon(aes(ymin = lowera, ymax = uppera),
              fill = "orange", alpha = 0.30) +
  geom_line(aes(y = median), colour = "red",  size = 1) +
  geom_line(aes(y = LUR),    colour = "blue", size = 0.8) +
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
read_vintage <- function(path, label) {
  read_csv(path, show_col_types = FALSE) |>
    mutate(date = as.yearqtr(date),
           vintage = label) |>
    select(date, median, vintage)
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


