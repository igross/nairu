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
library(scales)      # added for date_breaks & number_format
library(janitor)
library(ggforce)
library(viridisLite)

# ---- 2. Set up file paths -----------------------------------------------
target_dir  <- getwd()
csv_in      <- file.path(target_dir, "docs", "NAIRU_baseline.csv")
vintage_dir <- file.path(target_dir, "docs", "vintages")
output_dir  <- file.path(target_dir, "docs")
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
  fname     <- basename(path)
  date_str  <- tools::file_path_sans_ext(fname)
  pub_date  <- as.Date(date_str)
  file_date <- as.yearqtr(pub_date)

  df <- suppressMessages(read_csv(path, show_col_types = FALSE))
  if (nrow(df) == 0 || !"median" %in% names(df)) {
    return(tibble::tibble(
      pub_date     = as.Date(character()),
      max_date     = zoo::as.yearqtr(character()),
      nairu_latest = numeric()
    ))
  }
  df <- ensure_dates(df) %>%
      mutate(date = as.Date(date, frac = 0.5))   # ← mid-quarter

  idx <- which(df$date == file_date)
  if (length(idx) == 0) idx <- which.max(df$date)
  nairu_val <- df$median[idx]

  tibble::tibble(
    pub_date     = pub_date,
    max_date     = file_date,
    nairu_latest = nairu_val
  )
}

fmt_yq <- function(yq) format(yq, "%Y Q%q")

# Load and tidy NAIRU estimates for a given model --------------------------
read_nairu_model <- function(path, model_label) {
  suppressMessages(read_csv(path, show_col_types = FALSE)) %>%
    clean_names() %>%
    rename(
      lower = lowera,
      upper = uppera
    ) %>%
    mutate(
      date_qtr = as.yearqtr(date, "%Y Q%q"),
      date     = as.Date(date_qtr, frac = 0.5),
      model    = model_label,
      qtr_lbl  = format(date_qtr, "%Y-Q%q")
    ) %>%
    arrange(date_qtr) %>%
    filter(!is.na(median))
}

# Convert a decomposition CSV into a tidy, long format --------------------
tidy_decomposition <- function(path, series_label, component_labels) {
  if (!file.exists(path)) return(tibble::tibble())

  df_long <- suppressMessages(read_csv(path, show_col_types = FALSE)) %>%
    pivot_longer(-date_qtr, names_to = "component", values_to = "value") %>%
    mutate(
      component = recode(
        component,
        !!!component_labels,
        .default = tools::toTitleCase(gsub("_", " ", component))
      ),
      date_qtr = as.yearqtr(date_qtr, "%Y Q%q"),
      date     = as.Date(date_qtr, frac = 0.5),
      series   = series_label
    ) %>%
    filter(!is.na(value))

  if (nrow(df_long) == 0) return(df_long)

  preferred_levels <- unique(unname(component_labels))
  extra_levels     <- setdiff(unique(df_long$component), preferred_levels)

  df_long %>%
    mutate(
      component = factor(component, levels = c(preferred_levels, extra_levels)),
      tooltip   = sprintf(
        "%s<br>%s<br>%s: %.2f pp",
        format(date_qtr, "%Y-Q%q"),
        series,
        component,
        value
      )
    )
}

# Custom theme ------------------------------------------------------------
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
  clean_names() %>%
  rename(
    lower = lowera,
    upper = uppera
  ) %>%
mutate(
  date_qtr = as.yearqtr(date),
  date     = as.Date(date_qtr, frac = 0.5)   # ← mid-quarter
) %>%
  filter(date_qtr >= as.yearqtr("1999 Q1")) %>%
  arrange(date_qtr)

# immediately after you construct nairu_df ─────────────────────────────
nairu_df  <- nairu_df  %>% mutate(qtr_lbl = format(date_qtr, "%Y-Q%q"))
nairu_zoom <- nairu_df %>% filter(date_qtr >= as.yearqtr("2010 Q1"))  # reuse with qtr_lbl

tail(nairu_df)

message("Loaded ", nrow(nairu_df), " rows – ",
        sum(!is.na(nairu_df$median)), " have a median value")

# ---- 6. Figure 1: full history ------------------------------------------

# ── Figure 1 ──────────────────────────────────────────────────────────────
p1 <- ggplot(nairu_df, aes(x = date, group = 1)) +     # << add group = 1
  geom_ribbon(
    aes(ymin = lower, ymax = upper,
        text = sprintf("%s<br>Credible band: %.2f – %.2f",
                        qtr_lbl, lower, upper)),
    fill = "orange", alpha = .30, colour = NA
  ) +
  geom_line(
    aes(y = median,
        text = sprintf("%s<br>Median NAIRU: %.2f", qtr_lbl, median)),
    colour = "red", linewidth = 1
  ) +
  geom_line(
    aes(y = lur,
        text = sprintf("%s<br>Unemp. rate: %.2f", qtr_lbl, lur)),
    colour = "blue", linewidth = .8
  ) +
  geom_point(
    data = slice_tail(nairu_df, n = 1),
    aes(y = median,
        text = sprintf("Latest (%s)<br>Median: %.2f", qtr_lbl, median)),
    colour = "black", size = 3
  ) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(title = "NAIRU estimate with 90% uncertainty bands",
       x = "Year", y = "Percent") +
  my_theme


ggsave(file.path(output_dir, "nairu_history.png"),
       p1, width = 8, height = 5, dpi = 300)
saveWidget(ggplotly(p1, tooltip = "text"),
           file.path(output_dir, "nairu_history.html"))

# ---- 7. Figure 2: zoom 2010-present -------------------------------------
# ---- Figure 2: NAIRU estimate (2010-present) -----------------------------

# ensure we have the zoomed subset (with qtr_lbl already in nairu_df)
nairu_zoom <- nairu_df %>%
  filter(date_qtr >= as.yearqtr("2010 Q1"))

p2 <- ggplot(nairu_zoom, aes(x = date, group = 1)) +   # <- single group
  geom_ribbon(
    aes(ymin = lower, ymax = upper,
        text = sprintf("%s<br>Credible band: %.2f – %.2f",
                       qtr_lbl, lower, upper)),
    fill = "orange", alpha = 0.3, colour = NA, na.rm = TRUE
  ) +
  geom_line(
    aes(y = median,
        text = sprintf("%s<br>Median NAIRU: %.2f",
                       qtr_lbl, median)),
    colour = "red", linewidth = 1, na.rm = TRUE
  ) +
  geom_line(
    aes(y = lur,
        text = sprintf("%s<br>Unemp. rate: %.2f",
                       qtr_lbl, lur)),
    colour = "blue", linewidth = 0.8, na.rm = TRUE
  ) +
  geom_point(                                          # highlight latest value
    data = slice_tail(nairu_zoom, n = 1),
    aes(y = median,
        text = sprintf("Latest (%s)<br>Median: %.2f",
                       qtr_lbl, median)),
    colour = "black", size = 3
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title    = "NAIRU estimate (post-GFC)",
    subtitle = "90 % credible interval",
    x        = "Quarter",
    y        = "Percent"
  ) +
  my_theme

# save outputs -------------------------------------------------------------
ggsave(file.path(output_dir, "nairu_zoom_2010.png"),
       p2, width = 8, height = 5, dpi = 300)

saveWidget(
  plotly::ggplotly(p2, tooltip = "text"),
  file.path(output_dir, "nairu_zoom_2010.html")
)

message("Figure 2 saved")


# ---- 8. Figure 3: Most-recent by release type ---------------------------
types <- list.files(vintage_dir, pattern = "\\.csv$", full.names = TRUE)
last8 <- head(types[order(file.info(types)$mtime, decreasing = TRUE)], 8)
tmp_df <- map_dfr(last8, read_vintage_safe)

if (nrow(tmp_df) > 0 && "max_date" %in% names(tmp_df)) {
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

  p3 <- ggplot(
    summary_df,
    aes(x = factor(idx), y = nairu_latest, fill = release_type,
        text = paste0("Release: ", new_qtrs, "<br>NAIRU: ", nairu_latest))
  ) +
    geom_col(width = 0.7) +
    scale_y_continuous(limits = c(4, 5)) +
    scale_x_discrete(labels = paste0(summary_df$release_type, "\n", summary_df$new_qtrs)) +
    labs(title = "Most-recent NAIRU estimates by release type",
         x = "Release (type and quarter)", y = "NAIRU (%)",
         fill = "Release") +
    my_theme

  ggsave(file.path(output_dir, "nairu_last8_bar.png"), p3, width = 9, height = 5, dpi = 300)
  saveWidget(ggplotly(p3, tooltip = "text"),
             file.path(output_dir, "nairu_last8_bar.html"))
  message("Figure 3 saved")
} else {
  message("Skipping Figure 3 – no recent vintage files were found")
}

# ---- 9. Figure 4: All vintages series colored ---------------------------
files  <- list.files(vintage_dir, pattern = "\\.csv$", full.names = TRUE)
labels <- tools::file_path_sans_ext(basename(files))

vintages_df <- map2_dfr(files, labels, function(path, label) {
  df <- suppressMessages(read_csv(path, show_col_types = FALSE))
  if (nrow(df) == 0) {
    return(tibble::tibble())
  }

  ensure_dates(df) %>% mutate(vintage = label)
})

if (nrow(vintages_df) > 0 && "vintage" %in% names(vintages_df)) {
  all_vints <- unique(vintages_df$vintage)

  if ("Baseline" %in% all_vints) {
    palette   <- rainbow(length(all_vints) - 1)
    color_map <- setNames(c(palette, "black"), c(setdiff(all_vints, "Baseline"), "Baseline"))
  } else {
    palette   <- rainbow(length(all_vints))
    color_map <- setNames(palette, all_vints)
  }

  p4 <- ggplot(
    vintages_df,
    aes(
      x     = date,
      y     = median,
      color = vintage,
      text  = paste0("Date: ", date, "<br>NAIRU: ", median)
    )
  ) +
    geom_line(linewidth = 0.8) +
    scale_color_manual(values = color_map) +
    labs(
      title = "NAIRU estimates across all vintages",
      x     = "Year",
      y     = "NAIRU (%)",
      color = "Vintage"
    ) +
    my_theme

  ggsave(file.path(output_dir, "nairu_all_vintages.png"), p4, width = 8, height = 5, dpi = 300)
  saveWidget(ggplotly(p4, tooltip = "text"), file.path(output_dir, "nairu_all_vintages.html"))
  message("Figure 4 saved: all vintages")
} else {
  message("Skipping Figure 4 – no valid vintage files were found")
}

# ---- 10. Figure 5: NAIRU across all regions ------------------------------

regions_file <- file.path(output_dir, "NAIRU_all_regions.csv")

# 10.1 Read raw CSV
nairu_regions_raw <- read_csv(regions_file, show_col_types = FALSE)

# 10.2 Parse mixed‐format dates
#    - “YYYY Qq” → as.yearqtr() → as.Date()
#    - “YYYY-MM-DD” → as.Date()
is_q      <- grepl("Q", nairu_regions_raw$date)
parsed_dt <- as.Date(rep(NA, nrow(nairu_regions_raw)))

parsed_dt[is_q]      <- as.Date(
  as.yearqtr(nairu_regions_raw$date[is_q], format = "%Y Q%q")
)
parsed_dt[!is_q]     <- as.Date(nairu_regions_raw$date[!is_q])

# 10.3 Build final data frame
nairu_regions <- nairu_regions_raw %>%
  mutate(
    date   = as.Date(parsed_dt, frac = 0.5),  # ← mid-quarter
    region = factor(region)
  )

# 10.4 Plot
p5 <- ggplot(nairu_regions, aes(x = date, y = median, group = region)) +
  geom_ribbon(aes(ymin = lower90, ymax = upper90, fill = region),
              alpha = 0.25, colour = NA) +
  geom_line(aes(colour = region), linewidth = 0.8) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  labs(
    title    = "Estimated NAIRU by Region",
    subtitle = "Median (solid lines) and 90% credible intervals",
    x        = NULL,
    y        = "Percent",
    colour   = "Region",
    fill     = "Region"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

# 10.5 Save
ggsave(file.path(output_dir, "nairu_regions.png"),
       p5, width = 8, height = 5, dpi = 300)
htmlwidgets::saveWidget(
  plotly::ggplotly(p5, tooltip = "text"),
  file.path(output_dir, "nairu_regions.html")
)
message("✔  Figure 5 saved: regions")



# ---- 11. NAIRU estimates by model ---------------------------------------

model_specs <- tibble::tribble(
  ~path,                                      ~label,
  file.path(output_dir, "NAIRU_baseline.csv"),          "CPI & ULC model",
  file.path(output_dir, "NAIRU_aena.csv"),              "CPI & AENA model",
  file.path(output_dir, "NAIRU_aena_wpi.csv"),          "CPI with AENA & WPI model",
  file.path(output_dir, "NAIRU_wpi.csv"),               "CPI & WPI model",
  file.path(output_dir, "NAIRU_wpi_no_inflation.csv"),  "WPI-only model"
) %>%
  filter(file.exists(path))

nairu_models_df <- purrr::map2_dfr(
  model_specs$path,
  model_specs$label,
  read_nairu_model
)

if (nrow(nairu_models_df) > 0) {
  latest_points <- nairu_models_df %>%
    group_by(model) %>%
    filter(date == max(date)) %>%
    ungroup()

  model_levels  <- unique(nairu_models_df$model)
  model_palette <- viridisLite::viridis(length(model_levels), end = 0.85)
  lur_df <- nairu_models_df %>% distinct(date, lur, qtr_lbl) %>% arrange(date)

  readr::write_csv(
    nairu_models_df %>%
      arrange(date, model) %>%
      transmute(
        date    = format(date, "%Y-%m-%d"),
        quarter = qtr_lbl,
        model,
        median
      ),
    file.path(output_dir, "nairu_model_medians.csv")
  )
  message("✔  Saved NAIRU model medians CSV")

  p_models <- ggplot(nairu_models_df, aes(x = date)) +
    geom_ribbon(
      aes(
        ymin = lower,
        ymax = upper,
        fill = model,
        group = model,
        text = sprintf(
          "%s<br>%s credible interval: %.2f – %.2f",
          qtr_lbl, model, lower, upper
        )
      ),
      alpha = 0.25, colour = NA
    ) +
    geom_line(
      aes(
        y = median,
        colour = model,
        group = model,
        text = sprintf("%s<br>%s median NAIRU: %.2f", qtr_lbl, model, median)
      ),
      linewidth = 1, na.rm = TRUE
    ) +
    geom_point(
      data = latest_points,
      aes(
        y = median,
        colour = model,
        text = sprintf("Latest (%s)<br>%s median: %.2f", qtr_lbl, model, median)
      ),
      size = 2.5
    ) +
    geom_line(
      data = lur_df,
      aes(
        x = date,
        y = lur,
        text = sprintf("%s<br>Unemp. rate: %.2f", qtr_lbl, lur)
      ),
      inherit.aes = FALSE,
      colour = "#2c3e50",
      linewidth = 0.8,
      linetype = "dashed"
    ) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    scale_colour_manual(values = setNames(model_palette, model_levels)) +
    scale_fill_manual(values = setNames(model_palette, model_levels)) +
    labs(
      title    = "NAIRU estimates by model",
      subtitle = "Median estimates with 90% credible intervals",
      x        = "Year",
      y        = "Percent",
      colour   = "Model",
      fill     = "Model"
    ) +
    my_theme +
    theme(legend.position = "bottom")

  ggsave(
    file.path(output_dir, "nairu_models.png"),
    p_models,
    width = 9, height = 8, dpi = 300
  )

  htmlwidgets::saveWidget(
    plotly::ggplotly(p_models, tooltip = "text"),
    file.path(output_dir, "nairu_models.html")
  )

  message("✔  Saved NAIRU model comparison plot")

  # ---- 12. Change in NAIRU estimates by model --------------------------

  model_levels  <- unique(nairu_models_df$model)
  model_palette <- setNames(viridisLite::viridis(length(model_levels)), model_levels)

  nairu_changes <- nairu_models_df %>%
    arrange(model, date) %>%
    group_by(model) %>%
    mutate(
      median_change = median - dplyr::lag(median),
      tooltip       = sprintf(
        "%s<br>%s change in median NAIRU: %+0.2f",
        qtr_lbl, model, median_change
      )
    ) %>%
    ungroup() %>%
    filter(!is.na(median_change))

  if (nrow(nairu_changes) > 0) {
    p_change <- ggplot(
      nairu_changes,
      aes(
        x     = date,
        y     = median_change,
        colour = model,
        text  = tooltip
      )
    ) +
      geom_hline(yintercept = 0, colour = "grey50", linetype = "dashed") +
      geom_line(linewidth = 0.9) +
      scale_colour_manual(values = model_palette) +
      scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
      scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
      labs(
        title    = "Quarterly change in NAIRU estimates by model",
        subtitle = "Difference in median NAIRU relative to the previous quarter",
        x        = "Year",
        y        = "Percentage points",
        colour   = "Model"
      ) +
      my_theme +
      theme(legend.position = "bottom")

    ggsave(
      file.path(output_dir, "nairu_model_change.png"),
      p_change,
      width = 8, height = 5, dpi = 300
    )

    htmlwidgets::saveWidget(
      plotly::ggplotly(p_change, tooltip = "text"),
      file.path(output_dir, "nairu_model_change.html")
    )

    message("✔  Saved NAIRU model change plot")
  } else {
    message("⚠  Not enough observations to compute NAIRU changes")
  }

  # ---- 13. Average NAIRU across models ----------------------------------

  models_for_average <- nairu_models_df %>%
    filter(model != "WPI-only model")

  if (nrow(models_for_average) == 0) {
    message("⚠  Not enough models (after excluding WPI-only) to compute NAIRU average")
  } else {
    model_summary <- models_for_average %>%
      group_by(date, date_qtr) %>%
      summarise(
        avg_median = if (all(is.na(median))) NA_real_ else mean(median, na.rm = TRUE),
        min_median = if (all(is.na(median))) NA_real_ else min(median, na.rm = TRUE),
        max_median = if (all(is.na(median))) NA_real_ else max(median, na.rm = TRUE),
        min_lower  = if (all(is.na(lower)))  NA_real_ else min(lower, na.rm = TRUE),
        max_upper  = if (all(is.na(upper)))  NA_real_ else max(upper, na.rm = TRUE),
        qtr_lbl    = first(qtr_lbl),
        n_models   = sum(!is.na(median)),
        .groups    = "drop"
      )

    latest_avg <- slice_tail(model_summary, n = 1)

    line_df <- model_summary %>%
      select(date, qtr_lbl, n_models, avg_median, min_median, max_median) %>%
      pivot_longer(
        cols      = c(avg_median, min_median, max_median),
        names_to  = "series",
        values_to = "value"
      ) %>%
      mutate(
        series = recode(
          series,
          avg_median = "Average median",
          min_median = "Minimum median",
          max_median = "Maximum median"
        ),
        tooltip = sprintf(
          "%s<br>%s: %.2f<br>Models: %d",
          qtr_lbl, series, value, n_models
        )
      )

    p_avg <- ggplot(model_summary, aes(x = date, group = 1)) +
      geom_ribbon(
        aes(
          ymin = min_lower,
          ymax = max_upper,
          text = sprintf(
            "%s<br>Credible band union: %.2f – %.2f<br>Models: %d",
            qtr_lbl, min_lower, max_upper, n_models
          )
        ),
        fill = "#cce5ff", alpha = 0.4, colour = NA
      ) +
      geom_ribbon(
        aes(
          ymin = min_median,
          ymax = max_median,
          text = sprintf(
            "%s<br>Median range: %.2f – %.2f<br>Models: %d",
            qtr_lbl, min_median, max_median, n_models
          )
        ),
        fill = "#99c2ff", alpha = 0.6, colour = NA
      ) +
      geom_line(
        data = line_df,
        aes(
          y = value,
          group = series,
          colour = series,
          linetype = series,
          text = tooltip
        ),
        linewidth = 0.9
      ) +
      geom_point(
        data = latest_avg,
        aes(
          y = avg_median,
          text = sprintf(
            "Latest (%s)<br>Average median: %.2f<br>Models: %d",
            qtr_lbl, avg_median, n_models
          )
        ),
        colour = "black", size = 3
      ) +
      scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
      labs(
        title    = "Average NAIRU estimate across models",
        subtitle = "Shaded area shows the range of model medians and credible intervals",
        x        = "Year",
        y        = "Percent"
      ) +
      my_theme +
      scale_colour_manual(
        values = c(
          "Average median" = "#d62728",
          "Minimum median" = "#1f78b4",
          "Maximum median" = "#1f78b4"
        )
      ) +
      scale_linetype_manual(
        values = c(
          "Average median" = "solid",
          "Minimum median" = "dashed",
          "Maximum median" = "dashed"
        )
      ) +
      guides(colour = guide_legend(title = "Series"),
             linetype = guide_legend(title = "Series")) +
      theme(legend.position = "bottom")

    ggsave(
      file.path(output_dir, "nairu_model_average.png"),
      p_avg,
      width = 8, height = 5, dpi = 300
    )

    htmlwidgets::saveWidget(
      plotly::ggplotly(p_avg, tooltip = "text"),
      file.path(output_dir, "nairu_model_average.html")
    )

    message("✔  Saved NAIRU model average plot")
  }
}


#  FULL decomposition bar-charts by model
# ─────────────────────────────────────────────────────────────────────────────

base_component_labels <- c(
  expectations = "Inflation expectations",
  dummies      = "Dummy variables",
  import_price = "Import-price shocks",
  momentum     = "Momentum",
  unemp_gap    = "Unemployment gap",
  residuals    = "Residual"
)

decomposition_specs <- list(
  list(
    label             = "CPI & ULC model",
    output_stub       = "infl_ulc_decomp",
    component_labels  = c(base_component_labels, ulc_demeaned = "ΔULC (demeaned)"),
    series_files      = list(
      list(path = file.path(output_dir, "infl_pi_decomp.csv"), label = "Inflation"),
      list(path = file.path(output_dir, "ulc_decomp.csv"),       label = "Unit labour costs")
    )
  ),
  list(
    label             = "CPI & WPI model",
    output_stub       = "infl_wage_decomp_wpi",
    component_labels  = c(base_component_labels, wpi_demeaned = "ΔWPI (demeaned)"),
    series_files      = list(
      list(path = file.path(output_dir, "infl_pi_decomp_wpi.csv"), label = "Inflation"),
      list(path = file.path(output_dir, "wage_decomp_wpi.csv"),    label = "Wage growth")
    )
  )
)

biennial_decomp_scale <- scale_x_date(
  date_breaks = "2 years",
  date_labels = "%Y"
)

plotly_biennial_axis <- list(
  dtick     = "M24",
  tickformat = "%Y"
)

for (spec in decomposition_specs) {
  series_dfs <- purrr::map(
    spec$series_files,
    ~ tidy_decomposition(.x$path, .x$label, spec$component_labels)
  )
  decomp_df  <- purrr::list_rbind(series_dfs)

  if (nrow(decomp_df) == 0) {
    message("⚠  No decomposition data available for ", spec$label)
    next
  }

  comp_levels <- levels(decomp_df$component)
  palette_cols <- setNames(
    viridisLite::turbo(length(comp_levels)),
    comp_levels
  )

  p_decomp <- ggplot(
    decomp_df,
    aes(
      x    = date,
      y    = value,
      fill = component,
      text = tooltip
    )
  ) +
    geom_col(width = 90, position = position_stack(reverse = TRUE)) +
    facet_wrap(~ series, ncol = 1, scales = "free_y") +
    labs(
      title = paste0(spec$label, " – decomposition"),
      x     = "Year",
      y     = "Percentage-point contribution (q/q)",
      fill  = "Component"
    ) +
    biennial_decomp_scale +
    scale_fill_manual(values = palette_cols) +
    my_theme +
    theme(legend.position = "bottom")

  png_path  <- file.path(output_dir, paste0(spec$output_stub, ".png"))
  html_path <- file.path(output_dir, paste0(spec$output_stub, ".html"))

  ggsave(png_path, p_decomp, width = 9, height = 6, dpi = 300)
  p_decomp_interactive <- plotly::ggplotly(p_decomp, tooltip = "text") %>%
    plotly::layout(xaxis = plotly_biennial_axis)
  htmlwidgets::saveWidget(p_decomp_interactive, html_path)

  message("✔  Saved decomposition plot for ", spec$label)
}


# ---- 11. Phillips curve style scatter: inflation vs unemployment gap -----

library(readrba)

# 11.1 Download year-ended trimmed-mean CPI inflation from RBA
trim_infl <- read_rba(series_id = "GCPIOCPMTMYP") %>%
  mutate(date_qtr = as.yearqtr(date, "%Y-%m-%d"),
         date     = as.Date(date_qtr, frac = 0.5)) %>%
  select(date, trimmed_mean = value)

# 11.2 Merge into NAIRU dataset
# ---- 11. Inflation vs Unemployment gap (with central axes) ----

# Ensure unemployment gap is available
nairu_df <- nairu_df %>%
  left_join(trim_infl, by = "date") %>%
  mutate(unemp_gap = lur - median,
         age = as.numeric(date - min(date)),          # age in days
         alpha_val = scales::rescale(date, to = c(0.1, 1)))  # fade old → new


                           nairu_df <- nairu_df %>% 
  mutate(alpha_val = scales::rescale(date, to = c(0.1, 1)))

# Set limits symmetric around the central cross (0 for x, 2.5 for y)
x_max <- max(abs(range(nairu_df$unemp_gap, na.rm = TRUE)))
y_max <- max(abs(range(nairu_df$trimmed_mean - 2.5, na.rm = TRUE)))

x_lims <- c(-x_max, x_max)
y_lims <- 2.5 + c(-y_max, y_max)

# Define fractions of axis ranges
x_frac <- 0.2  # e.g., 20% of x-axis range
y_frac <- 0.2  # e.g., 20% of y-axis range

x_range <- diff(x_lims)
y_range <- diff(y_lims)

circles <- data.frame(
  x0 = 0,
  y0 = 2.5,
  rx = c(0.5 * x_max, 0.25 * x_max),  # horizontal radii
  ry = c(0.5 * y_max *1.3, 0.25 * y_max*1.3)   # vertical radii
)

print(nairu_df$alpha_val)
                           
cutoff_date <- max(nairu_df$date) - lubridate::years(2)

p_pc <- ggplot(nairu_df, aes(x = unemp_gap, y = trimmed_mean)) +
  # target circles
geom_ellipse(
  data = circles,
  aes(x0 = x0, y0 = y0, a = rx, b = ry, angle = 0),  # add angle = 0
  inherit.aes = FALSE,
  colour = "grey40", linetype = "dashed",
  linewidth = 0.4, alpha = 0.5
)+
  # line for last 2 years only
  geom_path(
    data = nairu_df %>% filter(date >= cutoff_date),
    aes(x = unemp_gap, y = trimmed_mean),
    colour = "steelblue", linewidth = 0.6
  ) +
  # fading points (all)
  geom_point(aes(alpha = alpha_val), size = 1, colour = "steelblue") +
  # most recent point highlighted
  geom_point(
    data = slice_tail(nairu_df, n = 1),
    aes(x = unemp_gap, y = trimmed_mean),
    colour = "black", fill = "red", shape = 21, size = 1, stroke = 1.2,
    inherit.aes = FALSE
  ) +
  # axes cross
  geom_hline(yintercept = 2.5, colour = "black") +
  geom_vline(xintercept = 0,   colour = "black") +
  scale_x_continuous(limits = x_lims) +
  scale_y_continuous(limits = y_lims) +
  scale_alpha_continuous(range = c(0.1, 1)) +
  labs(
    title    = "Inflation vs Unemployment Gap",
    x        = "Unemployment gap (UR – NAIRU, % points)",
    y        = "Trimmed-mean inflation (%, y/y)"
  ) +
  theme_minimal(base_size = 13) +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),legend.position = "none")  # drop legend

# Save
ggsave(file.path(output_dir, "phillips_gap.png"),
       p_pc, width = 7, height = 5, dpi = 300)
saveWidget(plotly::ggplotly(p_pc, tooltip = c("x", "y")),
           file.path(output_dir, "phillips_gap_target.html"))


                           library(readrba)

# Import unemployment rate (monthly, seasonally adjusted)
unemployment_forecasts <- read_rba(series_id = "GLFSURSA")

# Import trimmed mean inflation (year-ended, quarterly)
trimmed_mean_forecast<- read_rba(series_id = "GCPIOCPMTMYP")

# Get the most recent NAIRU median
latest_median <- tail(nairu_df$median, 1)

# Merge forecasts using latest median for gap
# Prepare forecast dataframe
forecasts_df <- trimmed_mean_forecast %>%
  # align dates to quarters
  mutate(date = as.Date(as.yearqtr(date, format = "%Y-%m-%d"), frac = 0.5)) %>%
       mutate(
    trimmed_mean = value
  ) %>%
  left_join(
    unemployment_forecasts %>%
      mutate(date = as.Date(as.yearqtr(date, format = "%Y-%m-%d"), frac = 0.5)) %>%
      select(date, lur = value),
    by = "date"
  ) %>%
  mutate(
    unemp_gap = lur - latest_median,  # use latest NAIRU
    type = "forecast"
  )


# --- Prepare original NAIRU data ---
nairu_df <- nairu_df %>%
  mutate(alpha_val = scales::rescale(date, to = c(0.1, 1)))

x_max <- max(abs(range(nairu_df$unemp_gap, na.rm = TRUE)))
y_max <- max(abs(range(nairu_df$trimmed_mean - 2.5, na.rm = TRUE)))

x_lims <- c(-x_max, x_max)
y_lims <- 2.5 + c(-y_max, y_max)

# Target ovals
circles <- data.frame(
  x0 = 0,
  y0 = 2.5,
  rx = c(0.5 * x_max, 0.25 * x_max),
  ry = c(0.5 * y_max * 1.3, 0.25 * y_max * 1.3)
)

cutoff_date <- max(nairu_df$date) - years(2)

# --- Plot ---
p_pc <- ggplot() +
  # target ovals
  geom_ellipse(
    data = circles,
    aes(x0 = x0, y0 = y0, a = rx, b = ry, angle = 0),
    inherit.aes = FALSE,
    colour = "grey40", linetype = "dashed",
    linewidth = 0.4, alpha = 0.5
  ) +
  # last 2 years of actual series
  geom_path(
    data = nairu_df %>% filter(date >= cutoff_date),
    aes(x = unemp_gap, y = trimmed_mean),
    colour = "steelblue", linewidth = 0.6
  ) +
  # fading points (full history)
  geom_point(
    data = nairu_df,
    aes(x = unemp_gap, y = trimmed_mean, alpha = alpha_val),
    size = 1, colour = "steelblue"
  ) +
  # RBA forecasts
  geom_path(
    data = forecasts_df,
    aes(x = unemp_gap, y = trimmed_mean),
    colour = "red", linewidth = 0.6
  ) +
  geom_point(
    data = forecasts_df,
    aes(x = unemp_gap, y = trimmed_mean),
    colour = "red", size = 1.5
  ) +
  # most recent actual point highlighted
  geom_point(
    data = slice_tail(nairu_df, n = 1),
    aes(x = unemp_gap, y = trimmed_mean),
    colour = "black", fill = "red", shape = 21, size = 1.5, stroke = 1.2,
    inherit.aes = FALSE
  ) +
  # axes cross
  geom_hline(yintercept = 2.5, colour = "black") +
  geom_vline(xintercept = 0, colour = "black") +
  scale_x_continuous(limits = x_lims) +
  scale_y_continuous(limits = y_lims) +
  scale_alpha_continuous(range = c(0.1, 1)) +
  labs(
    title = "Inflation vs Unemployment Gap",
    x = "Unemployment gap (UR – NAIRU, % points)",
    y = "Trimmed-mean inflation (%, y/y)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    legend.position = "none",
    panel.grid = element_blank()
  )

# --- Save ---
ggsave(file.path(output_dir, "phillips_gap_forecasts.png"), p_pc, width = 7, height = 5, dpi = 300)
