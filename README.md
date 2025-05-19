
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cash-rate-scraper

The key script in this repo is `R/scrape_cash_rate.R`. This file parses
market expectations for the cash rate based on the [latest ASX cash rate
implied yield
curve](https://www.asx.com.au/markets/trade-our-derivatives-market/futures-market/rba-rate-tracker).

The data is saved as a CSV in `daily_data`. The file
`combined_data/all_data.Rds` contains a dataframe that is the
combination of all the daily data CSVs.

Note that there was a gap in the data collection between 1 July and 20
July, as the ASX changed its website.

I offer no assurance that this will continue to work, or that the data
extracted using this script will be free of errors.

The `.github/workflows/refresh_data.yaml` file contains the instructions
to GitHub Actions to tell it to run `scrape_cash_rate.R` each day and
commit the results in this repo.

Please fork/copy/modify as you see fit.

# Graphs!

The file `R/viz_cash_rate.R` produces visualisations of this data, which
are shown below:

    #> # A tibble: 34,062 × 11
    #>    scrape_time         meeting_date implied_mean stdev days_to_meeting bucket
    #>    <dttm>              <date>              <dbl> <dbl>           <int>  <dbl>
    #>  1 2025-04-01 12:00:00 2025-04-01           4.08    NA               0   0.1 
    #>  2 2025-04-01 12:00:00 2025-04-01           4.08    NA               0   0.35
    #>  3 2025-04-01 12:00:00 2025-04-01           4.08    NA               0   0.6 
    #>  4 2025-04-01 12:00:00 2025-04-01           4.08    NA               0   0.85
    #>  5 2025-04-01 12:00:00 2025-04-01           4.08    NA               0   1.1 
    #>  6 2025-04-01 12:00:00 2025-04-01           4.08    NA               0   1.35
    #>  7 2025-04-01 12:00:00 2025-04-01           4.08    NA               0   1.6 
    #>  8 2025-04-01 12:00:00 2025-04-01           4.08    NA               0   1.85
    #>  9 2025-04-01 12:00:00 2025-04-01           4.08    NA               0   2.1 
    #> 10 2025-04-01 12:00:00 2025-04-01           4.08    NA               0   2.35
    #> 11 2025-04-01 12:00:00 2025-04-01           4.08    NA               0   2.6 
    #> 12 2025-04-01 12:00:00 2025-04-01           4.08    NA               0   2.85
    #> 13 2025-04-01 12:00:00 2025-04-01           4.08    NA               0   3.1 
    #> 14 2025-04-01 12:00:00 2025-04-01           4.08    NA               0   3.35
    #> 15 2025-04-01 12:00:00 2025-04-01           4.08    NA               0   3.6 
    #> 16 2025-04-01 12:00:00 2025-04-01           4.08    NA               0   3.85
    #> 17 2025-04-01 12:00:00 2025-04-01           4.08    NA               0   4.1 
    #> 18 2025-04-01 12:00:00 2025-04-01           4.08    NA               0   4.35
    #> 19 2025-04-01 12:00:00 2025-04-01           4.08    NA               0   4.6 
    #> 20 2025-04-01 12:00:00 2025-04-01           4.08    NA               0   4.85
    #>    probability_linear probability_prob probability   diff diff_s
    #>                 <dbl>            <dbl>       <dbl>  <dbl>  <dbl>
    #>  1             0                    NA          NA -4     -1.41 
    #>  2             0                    NA          NA -3.75  -1.39 
    #>  3             0                    NA          NA -3.5   -1.37 
    #>  4             0                    NA          NA -3.25  -1.34 
    #>  5             0                    NA          NA -3     -1.32 
    #>  6             0                    NA          NA -2.75  -1.29 
    #>  7             0                    NA          NA -2.5   -1.26 
    #>  8             0                    NA          NA -2.25  -1.22 
    #>  9             0                    NA          NA -2     -1.19 
    #> 10             0                    NA          NA -1.75  -1.15 
    #> 11             0                    NA          NA -1.5   -1.11 
    #> 12             0                    NA          NA -1.25  -1.06 
    #> 13             0                    NA          NA -1     -1    
    #> 14             0                    NA          NA -0.750 -0.931
    #> 15             0                    NA          NA -0.500 -0.841
    #> 16             0.0600               NA          NA -0.250 -0.707
    #> 17             0.940                NA          NA  0      0    
    #> 18             0                    NA          NA  0.25   0.707
    #> 19             0                    NA          NA  0.5    0.841
    #> 20             0                    NA          NA  0.75   0.931
    #> # ℹ 34,042 more rows
    #> Warning: A numeric `legend.position` argument in `theme()` was deprecated in ggplot2
    #> 3.5.0.
    #> ℹ Please use the `legend.position.inside` argument of `theme()` instead.
    #> This warning is displayed once every 8 hours.
    #> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    #> generated.
