# write_index_html.R  — NAIRU dashboard (interactive NAIRU only)
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
# 1.  Interactive NAIRU chart --------------------------------------------------
nairu_html <- file.path(output_dir, "nairu_zoom_2010.html")

spark_html <- if (file.exists(nairu_html)) {
  sprintf('
    <h2 style="text-align:center;">Non-Accelerating Inflation Rate of Unemployment</h2>
    <div style="display:flex;justify-content:center;margin:40px 0;">
      <iframe src="%s"
              style="width:95%%;height:800px;border:none;border-radius:15px;
                     box-shadow:0 4px 20px rgba(0,0,0,0.1);"></iframe>
    </div>', basename(nairu_html))         # relative to docs/index.html
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
    </div>', basename(interactive_html))
} else if (file.exists(static_png)) {
  sprintf('
    <h2 style="text-align:center;">Inflation &amp; ULC Decomposition</h2>
    <div class="chart-card" style="max-width:1000px;margin:0 auto;">
      <img src="%s" alt="Decomposition stacked bar">
    </div>', basename(static_png))
} else ""

# ─────────────────────────────────────────────────────────────────────────────
# 3.  HTML scaffold -----------------------------------------------------------
intro_paragraph <- '
  <p style="max-width:800px;margin:0 auto 30px auto;text-align:center;
            font-size:1.1rem;color:#444;">
    Below is the latest results from a state-space NAIRU model.
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
  .chart-card{background:#fff;border-radius:10px;box-shadow:0 4px 10px rgba(0,0,0,0.05);
              padding:15px;text-align:center;}
  .chart-card img{width:100%%;border-radius:6px;}
</style>
</head>
<body>

<h1>NAIRU Model Results — %s</h1>
%s  <!-- intro -->
%s  <!-- NAIRU -->
%s  <!-- decomposition -->

</body>
</html>', format(Sys.Date(), "%d %b %Y"),
   intro_paragraph, spark_html, decomp_html)

writeLines(html, file.path(docs_dir, "index.html"))
message("✅ docs/index.html written (interactive NAIRU, no other charts).")
