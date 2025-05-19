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


