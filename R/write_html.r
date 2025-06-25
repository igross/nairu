# write_index_html.R  — NAIRU dashboard (auto-include all plots)
# ----------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(readr);  library(dplyr);  library(zoo)
  library(ggplot2); library(htmlwidgets);  library(stringr)
})

output_dir <- "output"                # adjust if your pipeline uses a different folder
docs_dir   <- "docs"
dir.create(docs_dir, showWarnings = FALSE, recursive = TRUE)

# ─────────────────────────────────────────────────────────────────────────────
# 1.  NAIRU sparkline  --------------------------------------------------------
spark_png  <- file.path(docs_dir, "nairu_spark.png")
nairu_csv  <- file.path(output_dir, "NAIRU_baseline.csv")
spark_html <- ""

if (file.exists(nairu_csv)) {
  nairu_df <- read_csv(nairu_csv, show_col_types = FALSE) %>%
              mutate(date = as.yearqtr(date))
  ggplot(nairu_df, aes(date, median)) +
    geom_line(size = 1, colour = "#2c3e50") +
    theme_void() +
    ggsave(spark_png, width = 6, height = 1.2, dpi = 120)
  spark_html <- sprintf('
  <div style="margin:40px auto;max-width:600px;">
    <h2 style="text-align:center;">Estimated NAIRU (median)</h2>
    <img src="%s" style="width:100%%;">
  </div>', basename(spark_png))
}

# ─────────────────────────────────────────────────────────────────────────────
# 2. priority decomposition charts -------------------------------------------
static_png <- file.path(output_dir, "infl_ulc_decomp.png")
interactive_html <- file.path(output_dir, "infl_ulc_decomp.html")

decomp_html <- ""
if (file.exists(interactive_html)) {
  decomp_html <- sprintf('
    <h2 style="text-align:center;">Inflation &amp; ULC Decomposition</h2>
    <div style="display:flex;justify-content:center;margin:40px 0;">
      <iframe src="../%s"
        style="width:95%%;height:800px;border:none;border-radius:15px;
               box-shadow:0 4px 20px rgba(0,0,0,0.1);"></iframe>
    </div>', file.path(output_dir, basename(interactive_html)))
} else if (file.exists(static_png)) {
  decomp_html <- sprintf('
    <h2 style="text-align:center;">Inflation &amp; ULC Decomposition</h2>
    <div class="chart-card" style="max-width:1000px;margin:0 auto;">
      <img src="%s" alt="Decomposition stacked bar">
    </div>', file.path(output_dir, basename(static_png)))
}

# ─────────────────────────────────────────────────────────────────────────────
# 3. auto-collect any *other* PNG/HTML charts --------------------------------
#    (excludes the ones we already handled)
priority_files <- c(basename(spark_png), basename(static_png),
                    basename(interactive_html))

png_files <- list.files(output_dir, pattern = "\\.png$", full.names = FALSE)
html_files <- list.files(output_dir, pattern = "\\.html$", full.names = FALSE)

# match PNGs to an HTML with the same stem
chart_cards <- character(0)

for (png in png_files) {
  if (png %in% priority_files) next
  stem <- str_remove(png, "\\.png$")
  html_match <- sprintf("%s.html", stem)
  if (html_match %in% html_files) {
    # interactive pair exists
    chart_cards <- c(chart_cards, sprintf('
      <div class="chart-card">
        <iframe src="../%s"
          style="width:100%%;height:600px;border:none;border-radius:10px;"></iframe>
      </div>', file.path(output_dir, html_match)))
  } else {
    chart_cards <- c(chart_cards, sprintf('
      <div class="chart-card">
        <img src="%s" alt="%s">
      </div>', file.path(output_dir, png), stem))
  }
}

other_charts_html <- if (length(chart_cards)) {
  paste('<h2 style="text-align:center;margin-top:60px;">Additional Charts</h2>',
        '<div class="grid">', paste(chart_cards, collapse = "\n"), '</div>')
} else ""

# ─────────────────────────────────────────────────────────────────────────────
# 4. HTML scaffolding ---------------------------------------------------------
intro_paragraph <- '
  <p style="max-width:800px;margin:0 auto 30px auto;text-align:center;
            font-size:1.1rem;color:#444;">
    This dashboard shows the latest <strong>state-space NAIRU model</strong>
    results: the median NAIRU path and a full decomposition of quarterly
    inflation and unit-labour-cost growth.  Charts refresh automatically from
    the GitHub Actions pipeline.
  </p>'

html <- sprintf('
<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<title>NAIRU model dashboard</title>
<style>
  body{font-family:"Segoe UI",Roboto,sans-serif;background:#f5f7fa;color:#333;
       margin:0;padding:40px 20px;}
  h1{font-size:2rem;color:#2c3e50;text-align:center;margin-bottom:20px;}
  h2{font-size:1.4rem;color:#2c3e50;margin:40px 0 15px 0;text-align:center;}
  .grid{display:grid;grid-template-columns:repeat(auto-fill,minmax(320px,1fr));
        gap:30px;padding:10px;max-width:1200px;margin:0 auto;}
  .chart-card{background:#fff;border-radius:10px;box-shadow:0 4px 10px
              rgba(0,0,0,0.05);padding:15px;text-align:center;}
  .chart-card img{width:100%%;border-radius:6px;}
</style>
</head>
<body>

<h1>NAIRU Model Results — %s</h1>
%s  <!-- intro paragraph -->
%s  <!-- sparkline -->
%s  <!-- decomposition chart(s) -->
%s  <!-- any extra charts -->

</body>
</html>', format(Sys.Date(), "%d %b %Y"),
   intro_paragraph, spark_html, decomp_html, other_charts_html)

# ─────────────────────────────────────────────────────────────────────────────
# 5. write file --------------------------------------------------------------
writeLines(html, file.path(docs_dir, "index.html"))
message("✅ docs/index.html written, with ",
        length(chart_cards) + (spark_html != "") + (decomp_html != ""),
        " chart section(s).")
