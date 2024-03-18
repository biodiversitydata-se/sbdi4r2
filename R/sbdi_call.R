#' Start building a query
#'
#' @description
#' To download data from the selected atlas, one must construct a query. This
#' query tells the atlas API what data to download and return, as well as how it
#' should be filtered. Using `sbdi_call()` allows you to build a piped query to
#' download data, in the same way that you would wrangle data with `dplyr` and
#' the `tidyverse`.
#'
#' `sbdi_call()` is a wrapper to a group of underlying
#' `request_` functions in the `galah` package. Each of these functions can begin a piped query and end
#' with `collapse()`, `compute()` or `collect()`.
#'
#' The underlying `request_` #' functions are useful because they allow `sbdi4r2` and `galah`
#' to separate different types of requests to perform better. For example,
#' `filter.data_request` translates filters in R to `solr`, whereas
#' `filter.metadata_request` searches using a search term.
#'
#' For more details see the object-oriented programming vignette:
#' \code{vignette("object_oriented_programming", package = "galah")}
#'
#' @param method string: what `request` function should be called. Should be one
#' of `"data"` (default), `"metadata"` or `"files"`
#' @param type string: what form of data should be returned? Acceptable values
#' are specified by the corresponding `request` function
#' @param ... Zero or more arguments to alter a query. See 'details'.
#'
#' @details
#' Each atlas has several types of data that can be chosen. Currently supported
#' are `"occurrences"` (the default), and `"species"` (and `"media"` only for ALA).
#' It is also possible to use  `type = "occurrences-count"` and `type = "species-count"`;
#' but in practice this is synonymous with `sbdi_call() |> count()`, and is
#' therefore only practically useful for debugging (via `collapse()` and
#' `compute()`).
#'
#' Other named arguments are supported via `...`. In practice, functions
#' with a `sbdi_` prefix (and `galah_` for that matter) and S3 methods ported
#' from `dplyr` assign information to the correct slots internally. Overwriting these with
#' user-defined alternatives is possible, but not advised. Accepted
#' arguments are:
#'
#'  - `filter` (accepts `sbdi_filter()` or \code{\link[=filter.data_request]{filter()}})
#'  - `select` (accepts `sbdi_select()` or \code{\link[=filter.data_request]{select}})
#'  - `group_by` (accepts `sbdi_group_by()` or \code{\link[=group_by.data_request]{group_by()}})
#'  - `identify` (accepts `sbdi_identify()` or \code{\link[=identify.data_request]{identify()}})
#'  - `geolocate` (accepts `sbdi_geolocate()`, `sbdi_polygon()` `sbdi_bbox()` or
#'    \code{\link[=st_crop.data_request]{st_crop()}})
#'  - `limit` (accepts \code{\link[=slice_head.data_request]{slice_head()}})
#'  - `doi` (accepts a sting listing a valid DOI, specific to `collect()` when `type = "doi"`)
#'
#' Unrecognised names are ignored by `collect()` and related functions.
#'
#' @return Each sub-function returns a different object class: `request_data()`
#' returns `data_request`. `request_metadata` returns `metadata_request`,
#' `request_files()` returns `files_request`.
#' @seealso [collapse.data_request()], [compute.data_request()], [collect.data_request()]
#' @rdname sbdi_call
#' @examples \dontrun{
#' # Begin your query with `sbdi_call()`, then pipe using `%>%` or `|>`
#'
#' # Get number of records of *Aves* from 2001 to 2004 by year
#' sbdi_call() |>
#'   sbdi_identify("Aves") |>
#'   sbdi_filter(year > 2000 & year < 2005) |>
#'   sbdi_group_by(year) |>
#'   atlas_counts()
#'
#' # Get information for all species in *Cacatuidae* family
#' sbdi_call() |>
#'   sbdi_identify("Cacatuidae") |>
#'   atlas_species()
#'
#' # Download records of genus *Eolophus* from 2001 to 2004
#' sbdi_config(email = "your-email@email.com")
#'
#' sbdi_call() |>
#'   sbdi_identify("Eolophus") |>
#'   sbdi_filter(year > 2000 & year < 2005) |>
#'   atlas_occurrences()
#'
#' }
#' @importFrom galah galah_call
#' @export
sbdi_call <- function(method = c("data", "metadata", "files"),
                       type,
                       ...){
  resp <- galah_call(method = method, type, ...)
  return(resp)
}
