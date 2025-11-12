#' Download CPI data and compute a custom headline inflation series
#'
#' This module uses the `readabs` package to download CPI index numbers and
#' relative importance weights from the ABS catalogue 6401.0 (Consumer Price
#' Index, Australia).  A helper function constructs an alternative headline
#' inflation series by applying user supplied preferences over cities,
#' consumption bundles and weights.
#'
#' The public ABS data cubes occasionally change layout or naming conventions.
#' The helpers below rely on the long form output returned by `readabs::read_abs`
#' and therefore only require the expected metadata columns (`series`, `date`,
#' `value`, `table_no`, `unit`).  If ABS update their naming the functions will
#' still work so long as the `series` column continues to contain the city and
#' expenditure class labels separated by semicolons.
#'
#' @examples
#' \dontrun{
#' custom_cpi <- download_custom_cpi(
#'   city = "Sydney",
#'   include_items = c("Food", "Housing", "Transport"),
#'   exclude_items = "Tobacco",
#'   weight_overrides = c("Child care" = 0, "Beef and veal" = 0)
#' )
#'
#' head(custom_cpi$headline)
#' }
#'
#' @seealso readabs::read_abs

library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(readabs)

#' Download CPI data for a given city
#'
#' @param city Capital city name to keep.  Must match the labels used by ABS
#'   (e.g. "Sydney", "Melbourne", "Brisbane", etc.).
#' @param tables Character vector of table numbers to download.  By default the
#'   function requests table 13 (index numbers) and table 11 (relative
#'   importance weights).
#' @param refresh Passed to [readabs::read_abs()] to force an update of the
#'   cached files.
#' @return A list with two tibbles: `indexes` (CPI index numbers) and `weights`
#'   (relative importance weights).
#' @export

download_cpi_components <- function(
    city = "Sydney",
    tables = c("11", "13"),
    refresh = FALSE
) {
  raw <- readabs::read_abs(
    catalogue_number = "6401.0",
    tables = tables,
    update = refresh
  )

  cleaned <- raw %>%
    mutate(
      city_label = stringr::str_trim(purrr::map_chr(
        stringr::str_split(series, ";"),
        ~ if (length(.x) > 0) .x[[1]] else ""
      )),
      component = purrr::map_chr(
        stringr::str_split(series, ";"),
        ~ if (length(.x) > 1) stringr::str_trim(.x[[length(.x) - 1]]) else series
      )
    )

  available_cities <- cleaned %>%
    distinct(city_label) %>%
    pull()

  if (!city %in% available_cities) {
    stop(
      "City '", city, "' not found in ABS CPI tables. Available options: ",
      paste(sort(available_cities), collapse = ", ")
    )
  }

  indexes <- cleaned %>%
    filter(
      city_label == city,
      table_no %in% tables,
      stringr::str_detect(stringr::str_to_lower(unit), "index")
    ) %>%
    select(table_no, component, date, value) %>%
    arrange(component, date)

  weights <- cleaned %>%
    filter(
      city_label == city,
      table_no %in% tables,
      stringr::str_detect(stringr::str_to_lower(unit), "weight")
    ) %>%
    select(table_no, component, date, value) %>%
    arrange(component, date)

  list(indexes = indexes, weights = weights)
}

#' Prepare weights for a custom consumption bundle
#'
#' @param weight_tbl Tibble with columns `component`, `date`, and `value` where
#'   `value` is the relative importance weight published by ABS.
#' @param include_items Character vector of regular expressions.  Only
#'   components matching at least one expression are retained.  Use `NULL` to
#'   keep all components.
#' @param exclude_items Character vector of regular expressions specifying
#'   components to drop.  Exclusions are applied after inclusions.
#' @param weight_overrides Named numeric vector giving custom weights for
#'   specific components.  Names are matched exactly against the `component`
#'   column.  Weights are re-normalised after the overrides are applied.
#' @param latest_only Logical.  If `TRUE` (default) the most recent set of ABS
#'   weights (for example the latest COICOP weighting pattern release) is used.
#'   Otherwise the function returns time-varying weights.
#' @return Tibble with columns `component` and `weight`.
#' @keywords internal

prepare_custom_weights <- function(
    weight_tbl,
    include_items = NULL,
    exclude_items = NULL,
    weight_overrides = NULL,
    latest_only = TRUE
) {
  stopifnot("component" %in% names(weight_tbl))
  stopifnot("value" %in% names(weight_tbl))

  tbl <- weight_tbl %>%
    select(component, date, weight = value)

  if (latest_only) {
    latest_weight_date <- tbl %>%
      summarise(latest = max(date, na.rm = TRUE)) %>%
      pull(latest)

    if (is.na(latest_weight_date)) {
      stop("No valid release date found for weight data.")
    }

    tbl <- tbl %>%
      filter(date == latest_weight_date) %>%
      select(-date)
  }

  tbl <- tbl %>% distinct()

  if (!is.null(include_items)) {
    keep_idx <- purrr::map_lgl(tbl$component, function(x) {
      any(stringr::str_detect(x, include_items))
    })
    tbl <- tbl[keep_idx, , drop = FALSE]
  }

  if (!is.null(exclude_items)) {
    drop_idx <- purrr::map_lgl(tbl$component, function(x) {
      any(stringr::str_detect(x, exclude_items))
    })
    tbl <- tbl[!drop_idx, , drop = FALSE]
  }

  if (!is.null(weight_overrides)) {
    matches <- match(names(weight_overrides), tbl$component)
    if (anyNA(matches)) {
      missing_components <- names(weight_overrides)[is.na(matches)]
      stop(
        "Weight overrides provided for components not present in the bundle: ",
        paste(missing_components, collapse = ", ")
      )
    }
    tbl$weight[matches] <- weight_overrides
  }

  tbl <- tbl %>%
    mutate(weight = weight / sum(weight))

  tbl
}

#' Construct a custom headline inflation series
#'
#' @param indexes Tibble containing CPI index numbers (as returned by
#'   [download_cpi_components()]).
#' @param weights Tibble containing weights prepared by
#'   [prepare_custom_weights()].
#' @param annualise Logical; if `TRUE` the quarter-on-quarter rate is converted
#'   to an annualised rate (multiplied by four via compound interest).
#' @return A tibble with columns `date`, `headline` (quarterly inflation) and
#'   `headline_annualised` if `annualise = TRUE`.
#' @export

construct_custom_headline <- function(indexes, weights, annualise = TRUE) {
  stopifnot("component" %in% names(indexes))
  stopifnot("value" %in% names(indexes))
  stopifnot("component" %in% names(weights))
  stopifnot("weight" %in% names(weights))

  component_inflation <- indexes %>%
    semi_join(weights, by = "component") %>%
    group_by(component) %>%
    arrange(date, .by_group = TRUE) %>%
    mutate(qoq = value / dplyr::lag(value) - 1) %>%
    ungroup()

  headline <- component_inflation %>%
    filter(!is.na(qoq)) %>%
    left_join(weights, by = "component") %>%
    group_by(date) %>%
    summarise(headline = sum(qoq * weight), .groups = "drop")

  if (annualise) {
    headline <- headline %>%
      mutate(headline_annualised = (1 + headline)^4 - 1)
  }

  list(
    headline = headline,
    component_contributions = component_inflation %>%
      filter(!is.na(qoq)) %>%
      left_join(weights, by = "component") %>%
      mutate(contribution = qoq * weight)
  )
}

#' Convenience wrapper to download CPI data and compute a custom headline series
#'
#' @inheritParams download_cpi_components
#' @inheritParams prepare_custom_weights
#' @param annualise Logical; pass-through to [construct_custom_headline()].
#' @return A list containing the downloaded data, weights and the headline
#'   inflation series.
#' @export

download_custom_cpi <- function(
    city = "Sydney",
    include_items = NULL,
    exclude_items = NULL,
    weight_overrides = NULL,
    refresh = FALSE,
    annualise = TRUE
) {
  data <- download_cpi_components(city = city, refresh = refresh)

  weights <- prepare_custom_weights(
    weight_tbl = data$weights,
    include_items = include_items,
    exclude_items = exclude_items,
    weight_overrides = weight_overrides
  )

  headline <- construct_custom_headline(
    indexes = data$indexes,
    weights = weights,
    annualise = annualise
  )

  list(
    city = city,
    indexes = data$indexes,
    weights = weights,
    headline = headline$headline,
    contributions = headline$component_contributions
  )
}
