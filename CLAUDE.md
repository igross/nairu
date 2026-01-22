# CLAUDE.md - AI Assistant Guide for Australian NAIRU Dashboard

## Project Overview

This repository powers the **Australian NAIRU Dashboard** at https://nairu.angusgrossart.com. It estimates the Non-Accelerating Inflation Rate of Unemployment (NAIRU) for Australia using Bayesian state-space models.

**Key functionality:**
- Downloads quarterly economic data from ABS (Australian Bureau of Statistics) and RBA (Reserve Bank of Australia) APIs
- Runs Bayesian MCMC estimation using Stan probabilistic programming
- Generates interactive Plotly visualizations and CSV exports
- Publishes results to GitHub Pages via the `docs/` folder
- Automated refresh tied to ABS quarterly release calendar

## Directory Structure

```
nairu/
├── R/                    # Core R analysis scripts (main codebase)
├── stan/                 # Bayesian state-space models (Stan language)
├── docs/                 # Generated outputs for GitHub Pages
│   ├── data/            # CSV exports of model results
│   ├── vintages/        # Historical quarterly snapshots
│   └── *.html, *.png    # Interactive dashboards and charts
├── inputs/               # Static input data files
├── output/vintages/      # Staged vintage data before publishing
├── .github/workflows/    # GitHub Actions CI/CD automation
├── README.md             # Project documentation
├── renv.lock             # R package dependency management
└── cash-rate-scraper.Rproj  # RStudio project file
```

## Key Scripts

### R Scripts (`R/`)

| Script | Purpose |
|--------|---------|
| `run_nairu.R` | **Main entry point** - orchestrates data download, model fitting, and output generation. Checks release calendar and respects `FORCE_REFRESH` env var. |
| `state_nairu.r` | Helper functions for fitting state-space models and post-processing MCMC draws from Stan |
| `plot_nairu.R` | Builds ggplot2 and Plotly visualizations for NAIRU estimates and model comparisons |
| `make_vintages.R` | Archives quarterly snapshots to `docs/vintages/` for tracking revisions over time |
| `write_html.r` | Assembles static HTML pages embedding charts and tables |
| `nairu_realtime.R` | State-level NAIRU estimation for Australian states |
| `custom_headline_inflation.R` | Constructs alternative inflation measures from ABS CPI data |
| `plot_cash_rate_levels.R` | Generates cash rate visualization charts |

### Stan Models (`stan/`)

Nine Bayesian state-space model variants exploring different inflation/wage measure combinations:

- `NAIRU_cpi_ulc.stan` - CPI with Unit Labor Cost (primary model)
- `NAIRU_cpi_wpi.stan` - CPI with Wage Price Index
- `NAIRU_cpi_aena.stan` - CPI with Average Earnings
- `NAIRU_cpi_aena_wpi.stan` - Multi-component variant
- `NAIRU_cpi_ulc_aena.stan`, `NAIRU_cpi_ulc_aena_wpi.stan` - Extended variants
- `NAIRU_cpi_ulc_counterfactual.stan` - Policy counterfactual analysis
- `NAIRU_wpi_only.stan` - Simpler WPI-only model

**Model architecture features:**
- Latent NAIRU state variable (time-varying unemployment equilibrium)
- Phillips curve inflation dynamics
- Multiple lag structures (up to 4 lags on inflation, 2 on wages)
- Missing data handling for ULC/WPI observations
- Constrained parameter bounds

## Development Workflow

### Running Locally

```r
# Force a refresh on non-release days
Sys.setenv(FORCE_REFRESH = "true")
source("R/run_nairu.R")
```

### Required R Packages

Key dependencies (installed via GitHub Actions or locally):
- `tidyverse`, `dplyr`, `ggplot2`, `tidyr` - Data manipulation
- `readabs`, `readrba` - ABS/RBA data APIs
- `rstan` - Bayesian inference
- `plotly` - Interactive visualizations
- `zoo`, `lubridate` - Time series handling
- `here` - Project path management
- `ggthemes`, `ggrepel`, `viridisLite` - Visualization styling

### Environment Variables

| Variable | Purpose | Default |
|----------|---------|---------|
| `FORCE_REFRESH` | Override release calendar check | `false` |
| `AENA_SERIES_ID` | ABS series ID for average earnings | `A129552326F` |
| `GITHUB_WORKSPACE` | Root directory (set by GitHub Actions) | `here::here()` |

### Output Locations

All generated artifacts go to `docs/`:
- `docs/index.html` - Main NAIRU dashboard
- `docs/*.html` - Individual chart pages
- `docs/*.png` - Static chart images
- `docs/data/` - CSV exports (model estimates, parameters)
- `docs/vintages/` - Historical quarterly releases

## CI/CD Pipeline

The `.github/workflows/refresh-data.yaml` workflow runs on:
- **Scheduled triggers** matching ABS release calendar (CPI, National Accounts, WPI)
- **Pull requests** to main/master
- **Manual dispatch** via GitHub Actions UI
- **Monthly vintage archival** (1st of each month)

**Workflow steps:**
1. Setup R 4.4.2 and Pandoc on macOS-latest
2. Install XQuartz (required for plotting backends)
3. Install R package dependencies
4. Run `make_vintages.R` (conditional on trigger type)
5. Run `plot_nairu.R` to regenerate visualizations
6. Commit and push generated files

## Coding Conventions

### R Code Style

- Libraries loaded at top of script with semicolons for compactness
- Uses tidyverse/dplyr piping (`%>%`)
- `zoo::as.yearqtr()` for quarterly date handling
- Section headers with comment blocks: `# ---- section name ----`
- Decorative separators: `#----...----` or `# ────...────`
- Error handling with `tryCatch()` for API calls

### Stan Model Style

- Detailed header comments explaining model variant
- Organized blocks: data, transformed data, parameters, model, generated quantities
- Constrained parameter declarations with bounds
- Vectorized operations where possible
- Missing data indicators as integer arrays

### File Naming

- R scripts: `snake_case.R` or `snake_case.r` (inconsistent casing)
- Stan models: `NAIRU_descriptor.stan` with SCREAMING_SNAKE for NAIRU prefix
- Output CSVs: descriptive names like `NAIRU_baseline.csv`, `posterior_summary_params_*.csv`

## Data Sources

### Australian Bureau of Statistics (ABS)

| Catalogue | Content | Key Series |
|-----------|---------|------------|
| 5206.0 | National Accounts | ULC (A2304402X), GDP (A2302915V), AENA |
| 6202.0 | Labour Force | Unemployment rate (A84423050A) |
| 6345.0 | Wage Price Index | WPI (A2713849C) |
| 6457.0 | Trade Price | Import prices (A2298279F) |

### Reserve Bank of Australia (RBA)

- `GBONYLD` - 10-year government bond yield
- `GCPIOCPMTMQP` - Trimmed mean CPI (fallback)

### Static Inputs

- `inputs/PIE_RBAQ.CSV` - RBA inflation expectations (quarterly, 1959-present)

## Important Notes for AI Assistants

### When Modifying Code

1. **Test with FORCE_REFRESH=true** - The main script short-circuits on non-release days
2. **Respect data dependencies** - Scripts assume specific ABS/RBA series IDs
3. **Check Stan model compilation** - Changes to .stan files require recompilation
4. **Preserve output structure** - GitHub Pages expects specific file paths in `docs/`

### Common Tasks

**Add a new model variant:**
1. Create new `.stan` file in `stan/` following existing patterns
2. Update `run_nairu.R` to compile and fit the new model
3. Update `plot_nairu.R` to include in visualizations

**Update data sources:**
1. Modify series IDs in `run_nairu.R` or use environment variables
2. Ensure downstream transformations handle any format changes

**Modify CI schedule:**
1. Edit `.github/workflows/refresh-data.yaml`
2. Cron times are UTC - comments show Australian local times (AEDT/AEST)

### Files to Avoid Modifying

- `docs/` - Generated outputs (will be overwritten)
- `output/vintages/` - Archived data (append-only)
- `renv.lock` - Currently minimal; dependencies installed directly in CI

### Git Conventions

- Automated commits use message: "Refreshing data YYYY-MM-DD"
- Main branch: `main` or `master`
- User configured as `igross` in CI

## Model Economics Background

The NAIRU (Non-Accelerating Inflation Rate of Unemployment) represents the unemployment rate consistent with stable inflation. The models implement:

- **Phillips Curve**: Relationship between unemployment gap (actual - NAIRU) and inflation
- **State-Space Framework**: NAIRU treated as latent time-varying state
- **Multiple Measures**: Different wage/price indicators (ULC, WPI, CPI trimmed mean)
- **Expectations**: RBA and bond-market inflation expectations as anchors
- **Import Prices**: External price shocks from trade data
