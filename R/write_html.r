# write_index_html.R  — NAIRU dashboard
# ---------------------------------------------------------------------------
# Generates docs/index.html that summarises your latest NAIRU‐model outputs.
# Expects these artefacts in `output_dir` (created by earlier steps):
#   • infl_ulc_decomp.png           – static stacked bar-chart
#   • infl_ulc_decomp.html          – interactive plotly version
#   • NAIRU_baseline.csv            – state medians & bands
# ---------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(readr);  library(dplyr);   library(zoo)
  library(ggplot2); library(htmlwidgets);  # for the NAIRU sparkline
})

output_dir <- "output"           # adjust if different
docs_dir   <- "docs"
dir.create(docs_dir, showWarnings = FALSE, recursive = TRUE)

# ── 1. small NAIRU sparkline -------------------------------------------------
spark_png <- file.path(docs_dir, "nairu_spark.png")
nairu_csv <- file.path(output_dir, "NAIRU_baseline.csv")

if (file.exists(nairu_csv)) {
  nairu_df <- read_csv(nairu_csv, show_col_types = FALSE) %>%
    mutate(date = as.yearqtr(date))
  ggplot(nairu_df, aes(x = date, y = median)) +
    geom_line(size = 1, colour = "#2c3e50") +
    theme_void() +
    ggsave(spark_png, width = 6, height = 1.2, dpi = 120)
  spark_html <- sprintf(
    '<div style="margin:40px auto; max-width:600px;">
       <h2 style="text-align:center;">Estimated NAIRU (median)</h2>
       <img src="nairu_spark.png" style="width:100%%;">
     </div>'
  )
} else {
  spark_html <- ""
}

# ── 2. static & interactive decomposition charts ----------------------------
static_png <- file.path(output_dir, "infl_ulc_decomp.png")
interactive_html <- file.path(output_dir, "infl_ulc_decomp.html")

decomp_static <- if (file.exists(static_png)) {
  sprintf(
    '<h2 style="text-align:center;">Inflation &amp; ULC Decomposition</h2>
     <div class="chart-card" style="max-width:1000px;margin:0 auto;">
       <img src="%s" alt="Decomposition stacked bar">
     </div>', basename(static_png))
} else ""

decomp_iframe <- if (file.exists(interactive_html)) {
  sprintf(
    '<div style="display:flex;justify-content:center;margin:60px 0;">
       <iframe src="../%s"
               style="width:95%%;height:800px;border:none;border-radius:15px;
                      box-shadow:0 4px 20px rgba(0,0,0,0.1);">
       </iframe>
     </div>', interactive_html)
} else ""

# ── 3. HTML scaffold ---------------------------------------------------------
intro_paragraph <- '
  <p style="max-width:800px;margin:0 auto 30px auto;text-align:center;
            font-size:1.1rem;color:#444;">
    This dashboard shows the latest <strong>state-space NAIRU model</strong> results:
    the median NAIRU path and a full decomposition of quarterly inflation and
    unit-labour-cost growth.  All charts refresh automatically from the GitHub
    Actions pipeline.
  </p>'

html <- sprintf('
<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8" />
<title>NAIRU model dashboard</title>
<style>
  body{font-family:"Segoe UI",Roboto,sans-serif;background:#f5f7fa;
       color:#333;margin:0;padding:40px 20px;}
  h1{text-align:center;margin-bottom:20px;font-size:2rem;color:#2c3e50;}
  h2{font-size:1.4rem;color:#2c3e50;margin:40px 0 15px 0;text-align:center;}
  .chart-card{background:#fff;border-radius:10px;box-shadow:0 4px 10px rgba(0,0,0,0.05);
              padding:15px;text-align:center;}
  .chart-card img{width:100%%;border-radius:6px;}
</style>
</head>
<body>

<h1>NAIRU Model Results — %s</h1>
%s
%s
%s
%s

</body>
</html>', format(Sys.Date(), "%d %b %Y"),
   intro_paragraph, spark_html, decomp_static, decomp_iframe)

# ── 4. write file -----------------------------------------------------------
writeLines(html, file.path(docs_dir, "index.html"))
message("✅ docs/index.html written.")
