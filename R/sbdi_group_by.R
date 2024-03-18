#' Specify fields to group when downloading record counts
#'
#' `count.data_request()` and `atlas_counts()` support server-side grouping of
#' data. Grouping can be used to return record counts grouped by multiple, valid
#' fields (found by `search_all(fields)`).
#' @param ... zero or more individual column names to include
#' @return If any arguments are provided, returns a `data.frame` with
#' columns `name` and `type`, as per [select.data_request()].
#' @examples \dontrun{
#' sbdi_call() |>
#'   sbdi_group_by(basisOfRecord) |>
#'   atlas_counts()
#' }
#' @importFrom galah galah_group_by
#' @export
sbdi_group_by <- function(...){
  resp <- galah_group_by(...)
  return(resp)
}
