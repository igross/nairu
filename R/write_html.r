# write_index_html.R  — NAIRU dashboard (PNG-only version, fixed paths)
# ----------------------------------------------------------------------------
# Builds docs/index.html **without reading any CSVs**.
# Assumes all charts (static PNGs or interactive HTML) are already saved
# in `output_dir`; we just embed them.
# ----------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(stringr)
})

output_dir <- "docs"            # folder that holds all PNG/HTML plots
docs_dir   <- "docs"
dir.create(docs_dir, showWarnings = FALSE, recursive = TRUE)

# ─────────────────────────────────────────────────────────────────────────────
# 1.  NAIRU sparkline ----------------------------------------------------------
spark_png  <- file.path(output_dir, "nairu_zoom_2010.png")
spark_html <- if (file.exists(spark_png)) {
  sprintf('
  <div style="margin:40px auto;max-width:600px;">
    <h2 style="text-align:center;">Estimated NAIRU (median)</h2>
    <img src="%s" style="width:100%%;">
  </div>', basename(spark_png))           # ← drop leading "docs/"
} else ""

# ─────────────────────────────────────────────────────────────────────────────
# 2.  Inflation & ULC decomposition -------------------------------------------
static_png       <- file.path(output_dir, "infl_ulc_decomp.png")
interactive_html <- file.path(output_dir, "infl_ulc_decomp.html")

decomp_html <- if (file.exists(interactive_html)) {
  sprintf('
    <h2 style="text-align:center;">Inflation &amp; ULC Decomposition</h2>
    <div style="display:flex;justify-content:center;margin:40px 0;">
      <iframe src="%s"
              style="width:95%%;height:800px;border:none;border-radius:15px;
                     box-shadow:0 4px 20px rgba(0,0,0,0.1);"></iframe>
    </div>', basename(interactive_html))   # ← no "../", no "docs/"
} else if (file.exists(static_png)) {
  sprintf('
    <h2 style="text-align:center;">Inflation &amp; ULC Decomposition</h2>
    <div class="chart-card" style="max-width:1000px;margin:0 auto;">
      <img src="%s" alt="Decomposition stacked bar">
    </div>', basename(static_png))         # ← drop leading "docs/"
} else ""

# ─────────────────────────────────────────────────────────────────────────────
# 3.  Auto-include any other plots in output_dir ------------------------------
priority_files <- basename(c(spark_png, static_png, interactive_html))

png_files  <- list.files(output_dir, "\\.png$",  full.names = FALSE)
html_files <- list.files(output_dir, "\\.html$", full.names = FALSE)

chart_cards <- character(0)
for (png in png_files) {
  if (png %in% priority_files) next
  stem <- str_remove(png, "\\.png$")
  html_match <- paste0(stem, ".html")
  if (html_match %in% html_files) {
    chart_cards <- c(chart_cards, sprintf(
      '<div class="chart-card">
         <iframe src="%s"
                 style="width:100%%;height:600px;border:none;border-radius:10px;"></iframe>
       </div>', html_match))               # ← plain filename
  } else {
    chart_cards <- c(chart_cards, sprintf(
      '<div class="chart-card">
         <img src="%s" alt="%s">
       </div>', png, stem))                # ← plain filename
  }
}

other_charts_html <- if (length(chart_cards)) {
  paste('<h2 style="text-align:center;margin-top:60px;">Additional Charts</h2>',
        '<div class="grid">', paste(chart_cards, collapse = "\n"), '</div>')
} else ""

# ─────────────────────────────────────────────────────────────────────────────
# 4.  HTML scaffold -----------------------------------------------------------
intro_paragraph <- '
  <p style="max-width:800px;margin:0 auto 30px auto;text-align:center;
            font-size:1.1rem;color:#444;">
    This dashboard shows the latest <strong>state-space NAIRU model</strong>
    outputs (all charts are refreshed automatically in the GitHub Actions
    workflow).
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
  .chart-card{background:#fff;border-radius:10px;box-shadow:0 4px 10px rgba(0,0,0,0.05);
              padding:15px;text-align:center;}
  .chart-card img{width:100%%;border-radius:6px;}
</style>
</head>
<body>

<h1>NAIRU Model Results — %s</h1>
%s  <!-- intro -->
%s  <!-- NAIRU sparkline -->
%s  <!-- decomposition -->
%s  <!-- other charts -->

</body>
</html>', format(Sys.Date(), "%d %b %Y"),
   intro_paragraph, spark_html, decomp_html, other_charts_html)

writeLines(html, file.path(docs_dir, "index.html"))
message("✅ docs/index.html written (PNG-only mode).")
