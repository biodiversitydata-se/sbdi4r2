#' Apply a data quality profile
#'
#' A 'profile' is a group of filters that are pre-applied by the ALA. Using a
#' data profile allows a query to be filtered quickly to the most relevant or
#' quality-assured data that is fit-for-purpose. For example, the "ALA" profile
#' is designed to exclude lower quality records, whereas other profiles apply
#' filters specific to species distribution modelling (e.g. CDSM).
#'
#' Note that only one profile can be loaded at a time; if multiple profiles are
#' given, the first valid profile is used.
#'
#' For more bespoke editing of filters within a profile, use [sbdi_filter()]
#'
#' @param ... a profile name. Should be a `string` - the name or abbreviation
#'    of a data quality profile to apply to the query. Valid values can be seen
#'    using `show_all(profiles)`
#' @return A tibble containing a valid data profile value.
#' @seealso [show_all()] and [search_all()] to look up available data profiles.
#' [sbdi_filter()] can be used for more bespoke editing of individual data
#' profile filters.
#' @name sbdi_apply_profile
#' @examples \dontrun{
#' # Apply a data quality profile to a query
#' sbdi_call() |>
#'   sbdi_identify("reptilia") |>
#'     filter(year == 2021) |>
#'       sbdi_apply_profile(ALA) |>
#'         atlas_counts()
#'  }
#' @importFrom galah galah_apply_profile
#' @export
sbdi_apply_profile <- function(...){
  galah_apply_profile(...)
}
