# Add the shared site shell after charts are assembled; data pipelines are unchanged.
apply_site_theme <- function(html, site) {
  read_asset <- function(name) paste(readLines(file.path("docs", "assets", name), warn = FALSE), collapse = "\n")
  html <- sub('<html lang="en">', '<html lang="en-AU">', html, fixed = TRUE)
  html <- sub('</head>', paste0('<meta name="viewport" content="width=device-width, initial-scale=1">\n<link rel="icon" href="https://isaacgross.net/assets/favicon.svg" type="image/svg+xml">\n<link rel="stylesheet" href="assets/site.css?v=20260927">\n<link rel="stylesheet" href="assets/dashboard.css?v=20260927">\n<script src="assets/dashboard.js?v=20260927d" defer></script>\n</head>'), html, fixed = TRUE)
  date <- ""
  if (site == "nairu") {
    match <- regmatches(html, regexec('NAIRU Model Results — ([^<]+)', html))[[1]]
    if (length(match) > 1) date <- paste0('<p class="refresh-date">Model updated ', match[2], '</p>')
    html <- sub('<h1>NAIRU Model Results[^<]+</h1>', '', html)
    title <- 'NAIRU estimates'
  } else title <- 'Cash-rate expectations'
  html <- sub("<title>[^<]*</title>", paste0("<title>", title, " — Isaac Gross</title>"), html)
  # Existing page section headings become h2 below the single page title.
  html <- gsub('<h1', '<h2', html, fixed = TRUE)
  html <- gsub('</h1>', '</h2>', html, fixed = TRUE)
  tools <- paste0('<nav class="tool-nav" aria-label="Economic analysis tools">',
    '<a href="https://rba.isaacgross.net/"', if(site == "cash") ' aria-current="page"' else '', '>Cash-rate expectations</a>',
    '<a href="https://nairu.isaacgross.net/"', if(site == "nairu") ' aria-current="page"' else '', '>NAIRU estimates</a>',
    '<a href="https://isaacgross.net/scenario-analysis/">Scenario Analysis</a>',
    '<a href="https://isaacgross.net/optimal-policy/">Optimal Policy</a></nav>')
  hero <- paste0('<header class="page-heading"><p class="eyebrow">Australian Economic Analysis</p><h1>',title,'<span class="period">.</span></h1>',date,'</header>',tools)
  html <- sub('<body>', paste0('<body class="dashboard">',read_asset('header.html'),'<main id="main" class="wrap">',hero), html, fixed = TRUE)
  html <- sub('</body>',paste0('</main>',read_asset('footer.html'),'</body>'),html,fixed=TRUE)
  html <- gsub('<iframe src="([^"]+)"', '<iframe loading="lazy" title="Interactive economic chart: \\1" src="\\1"', html)
  html
}
