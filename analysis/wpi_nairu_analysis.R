# Analyze WPI-inclusive NAIRU model runs in R
#
# The script mirrors the earlier Python utility but uses only base R
# dependencies. It finds any NAIRU credible interval lower bounds below a
# configurable threshold, writes those observations to CSV, and produces
# SVG scatter plots comparing parameter medians for all WPI models and the
# subset whose NAIRU dips below the threshold.

threshold <- 3.0

model_info <- list(
  wpi = list(
    name = "CPI & WPI model",
    nairu_file = "docs/data/NAIRU_wpi.csv",
    param_file = "docs/data/posterior_summary_params_wpi.csv",
    color = "#1f77b4"
  ),
  aena_wpi = list(
    name = "CPI with AENA & WPI model",
    nairu_file = "docs/data/NAIRU_aena_wpi.csv",
    param_file = "docs/data/posterior_summary_params_aena_wpi.csv",
    color = "#d62728"
  ),
  ulc_wpi = list(
    name = "CPI with ULC & WPI model",
    nairu_file = "docs/data/NAIRU_ulc_wpi.csv",
    param_file = "docs/data/posterior_summary_params_ulc_wpi.csv",
    color = "#2ca02c"
  ),
  ulc_aena_wpi = list(
    name = "CPI with ULC, AENA & WPI model",
    nairu_file = "docs/data/NAIRU_ulc_aena_wpi.csv",
    param_file = "docs/data/posterior_summary_params_ulc_aena_wpi.csv",
    color = "#9467bd"
  )
)

read_csv <- function(path) {
  read.csv(path, stringsAsFactors = FALSE)
}

to_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}

find_below_threshold <- function(info) {
  nairu <- read_csv(info$nairu_file)
  lowera <- to_numeric(nairu$lowera)
  nairu[!is.na(lowera) & lowera < threshold, c("date", "variable", "median", "lowera", "lowerb", "uppera", "upperb")]
}

collect_params <- function(info) {
  params <- read_csv(info$param_file)
  data.frame(
    model = info$name,
    parameter = params$parameter,
    median = to_numeric(params$median),
    mean = to_numeric(params$mean),
    sd = to_numeric(params$sd),
    stringsAsFactors = FALSE
  )
}

write_below_threshold_summary <- function(below_runs, output_path) {
  rows <- do.call(rbind, Map(function(key, df) {
    if (nrow(df) == 0) return(NULL)
    data.frame(
      model = model_info[[key]]$name,
      df,
      stringsAsFactors = FALSE
    )
  }, names(below_runs), below_runs))

  if (is.null(rows)) {
    rows <- data.frame(
      model = character(), date = character(), variable = character(),
      median = numeric(), lowera = numeric(), lowerb = numeric(),
      uppera = numeric(), upperb = numeric(), stringsAsFactors = FALSE
    )
  }

  dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
  write.csv(rows, output_path, row.names = FALSE)
}

compute_param_spread <- function(params) {
  medians <- params$median
  medians <- medians[!is.na(medians)]
  if (length(medians) == 0) {
    c(0, 1)
  } else {
    range(medians)
  }
}

plot_params <- function(params, model_colors, filename, title_text) {
  if (nrow(params) == 0) {
    warning(sprintf("No parameters to plot for %s", filename))
    return()
  }

  parameters <- sort(unique(params$parameter))
  param_index <- setNames(seq_along(parameters), parameters)

  spread <- compute_param_spread(params)
  padding <- if (diff(spread) > 0) diff(spread) * 0.05 else 0.5
  x_min <- spread[1] - padding
  x_max <- spread[2] + padding

  # SVG output sized to match earlier artifacts (approx 1100x* pixels)
  svg(filename, width = 11, height = 8, pointsize = 10)
  on.exit(dev.off(), add = TRUE)

  par(mar = c(5, 12, 4, 2))
  plot(
    NA, xlim = c(x_min, x_max), ylim = c(0.5, length(parameters) + 0.5),
    yaxt = "n", xlab = "Parameter median", ylab = "", main = title_text
  )
  axis(2, at = seq_along(parameters), labels = parameters, las = 2, cex.axis = 0.7)
  abline(h = seq_along(parameters), col = "#EEEEEE", lwd = 0.8)

  for (i in seq_len(nrow(params))) {
    param <- params$parameter[i]
    median_val <- params$median[i]
    if (is.na(median_val)) next
    model_name <- params$model[i]
    points(
      x = median_val,
      y = param_index[[param]],
      pch = 21,
      bg = model_colors[[model_name]],
      col = "#333333",
      cex = 1.1
    )
  }

  legend(
    "bottom", inset = -0.2, xpd = TRUE, horiz = TRUE,
    legend = names(model_colors),
    pt.bg = unlist(model_colors), pch = 21, pt.cex = 1.2,
    col = "#333333", bty = "n", cex = 0.8
  )
}

main <- function() {
  below_runs <- list()
  params_all <- data.frame()

  for (key in names(model_info)) {
    info <- model_info[[key]]
    below <- find_below_threshold(info)
    if (nrow(below) > 0) {
      below_runs[[key]] <- below
    } else {
      below_runs[[key]] <- below[0, ]
    }

    params <- collect_params(info)
    params$below_threshold <- nrow(below) > 0
    params_all <- rbind(params_all, params)
  }

  write_below_threshold_summary(below_runs, "docs/data/wpi_nairu_below3.csv")

  model_colors <- setNames(vapply(model_info, function(x) x$color, character(1)),
                           vapply(model_info, function(x) x$name, character(1)))

  plot_params(
    params_all,
    model_colors,
    filename = "docs/wpi_nairu_params_all.svg",
    title_text = sprintf("WPI-inclusive NAIRU model parameter medians (threshold %.0f%%)", threshold)
  )

  params_subset <- params_all[params_all$below_threshold, ]
  subset_colors <- model_colors[unique(params_subset$model)]
  plot_params(
    params_subset,
    subset_colors,
    filename = "docs/wpi_nairu_params_below3.svg",
    title_text = "Parameter medians for models with NAIRU below 3%"
  )
}

if (identical(environment(), globalenv())) {
  main()
}
