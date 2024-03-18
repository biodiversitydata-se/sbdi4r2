#' Narrow a query by specifying filters
#'
#' "Filters" are arguments of the form `field` `logical` `value` that are used
#' to narrow down the number of records returned by a specific query.
#' For example, it is common for users to request records from a particular year
#' (`year == 2020`), or to return all records except for fossils
#'  (`basisOfRecord != "FossilSpecimen"`).
#'
#' The result of `sbdi_filter()` can be passed to the `filter`
#' argument in [atlas_occurrences()], [atlas_species()],
#' [atlas_counts()] or [atlas_media()].
#'
#' `sbdi_filter` uses non-standard evaluation (NSE),
#' and is designed to be as compatible as possible with `dplyr::filter()`
#' syntax.
#'
#' @param ... filters, in the form `field logical value`
#' @return A tibble containing filter values.
#' @seealso [search_taxa()] and [sbdi_geolocate()] for other ways to restrict
#' the information returned by [atlas_occurrences()] and related functions. Use
#' `search_all(fields)` to find fields that
#' you can filter by, and [show_values()] to find what values
#' of those filters are available.
#' @details
#' All statements passed to `sbdi_filter()` take the form of field - logical - value. Permissible examples include:
#'   * `=` or `==` (e.g. `year = 2020`)
#'   * `!=`, e.g. `year != 2020`)
#'   * `>` or `>=` (e.g. `year >= 2020`)
#'   * `<` or `<=` (e.g. `year <= 2020`)
#'   * `OR` statements (e.g. `year == 2018 | year == 2020`)
#'   * `AND` statements (e.g. `year >= 2000 & year <= 2020`)
#'
#' In some cases `R` will fail to parse inputs with a single equals sign
#' (`=`), particularly where statements are separated by `&` or
#' `|`. This problem can be avoided by using a double-equals (`==`) instead.
#'
#' *Notes on behaviour*
#'
#' Separating statements with a comma is equivalent to an `AND` statement;
#' Ergo `sbdi_filter(year >= 2010 & year < 2020)` is the same as
#' `sbdi_filter(year >= 2010, year < 2020)`.
#'
#' All statements must include the field name; so
#' `sbdi_filter(year == 2010 | year == 2021)` works, as does
#' `sbdi_filter(year == c(2010, 2021))`, but `sbdi_filter(year == 2010 | 2021)`
#' fails.
#'
#' It is possible to use an object to specify required values, e.g.
#' `year_value <- 2010; sbdi_filter(year > year_value)`
#'
#' `solr` supports range queries on text as well as numbers; so this is valid:
#' `sbdi_filter(cl22 >= "Tasmania")`
#'
#' @examples \dontrun{
#' # Filter query results to return records of interest
#' sbdi_call() |>
#'   sbdi_filter(year >= 2019,
#'                basisOfRecord == "HumanObservation") |>
#'   atlas_counts()
#'
#' # Alternatively, the same call using `dplyr` functions:
#' request_data() |>
#'   filter(year >= 2019,
#'                basisOfRecord == "HumanObservation") |>
#'   count() |>
#'   collect()
#' }
#' @importFrom galah galah_filter
#' @importFrom rlang enquos
#' @export
sbdi_filter <- function(...){
  dots <- enquos(..., .ignore_empty = "all") |>
    galah:::detect_request_object()
  resp <- galah_filter(dots)
  return(resp)
}
