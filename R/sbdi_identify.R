#' Narrow a query by passing taxonomic identifiers
#'
#' When conducting a search or creating a data query, it is common to identify
#' a known taxon or group of taxa to narrow down the records or results returned.
#'
#' `sbdi_identify()` is used to identify taxa you want returned in a search or
#' a data query. Users to pass scientific names or taxonomic identifiers
#' with pipes to provide data only for the biological group of interest.
#'
#' It is good to use [search_taxa()] and [search_identifiers()]
#' first to check that the taxa you provide to `sbdi_identify()` return the
#' correct results.
#'
#' @param ... One or more scientific names.
#' @return A tibble containing identified taxa.
#' @seealso [search_taxa()] to find identifiers from scientific names;
#' [search_identifiers()] for how to get names if taxonomic identifiers
#' are already known.
#' @examples \dontrun{
#' # Specify a taxon. A valid taxon will return an identifier.
#' sbdi_identify("reptilia")
#'
#' # Specify more than one taxon at a time.
#' sbdi_identify("reptilia", "mammalia", "aves", "pisces")
#'
#' # Use `sbdi_identify()` to narrow your queries
#' sbdi_call() |>
#'   sbdi_identify("Eolophus") |>
#'   atlas_counts()
#'
#' # Within a pipe, `identify()` and `sbdi_identify()` are synonymous.
#' # hence the following is identical to the previous example:
#' request_data() |>
#'   identify("Eolophus") |>
#'   count() |>
#'   collect()
#'
#' # If you know a valid taxon identifier, use `sbdi_filter()` instead.
#' id <- "https://biodiversity.org.au/afd/taxa/009169a9-a916-40ee-866c-669ae0a21c5c"
#' sbdi_call() |>
#'   sbdi_filter(lsid == id) |>
#'   atlas_counts()
#' }
#' @importFrom galah galah_identify
#' @export
sbdi_identify <- function(...) {
  resp <- galah_identify(... = ...)
  return(resp)
}
