#' Narrow a query to within a specified area
#'
#' Restrict results to those from a specified area using `sbdi_geolocate()`.
#' Areas can be specified as either polygons or bounding boxes, depending on
#' `type`. Alternatively, users can call the underlying functions directly via
#' `sbdi_bbox()` or `sbdi_polygon()`. Finally, it is possible to use `sf`
#' syntax by calling `st_crop()`, which is synonymous with `sbdi_polygon()`.
#'
#' If calling `sbdi_geolocate()`, the default `type` is `"polygon"`, which
#' narrows queries to within an area supplied as a `POLYGON`. Polygons must be
#' specified as either an `sf` object, a 'well-known text' (WKT) string, or a
#' shapefile. Shapefiles must be simple to be accepted by the ALA.
#' Alternatively, set `type = "bbox"` to narrow queries to within a bounding
#' box. Bounding boxes can be extracted from a supplied `sf` object or
#' a shapefile. A bounding box can also be supplied as a `bbox` object
#' (via `sf::st_bbox()`) or a `tibble`/`data.frame`.
#'
#' @param ... a single `sf` object, WKT string or shapefile. Bounding boxes can
#' be supplied as a `tibble`/`data.frame` or a `bbox`
#' @param type `string`: one of `c("polygon", "bbox")`. Defaults to
#' `"polygon"`. If `type = "polygon"`, a multipolygon will be built via
#' [sbdi_polygon()]. If `type = "bbox"`, a multipolygon will be built via
#' [sbdi_bbox()]. The multipolygon is used to narrow a query to the ALA.
#' @details If `type = "polygon"`, WKT strings longer than 10000 characters and
#' `sf` objects with more than 500 vertices will not be
#' accepted by the ALA. Some polygons  may need to be simplified. If
#' `type = "bbox"`, sf objects and shapefiles will be converted to a bounding
#' box to query the ALA.
#' @return length-1 string (class `character`) containing a multipolygon WKT
#' string representing the area provided.
#' @name sbdi_geolocate
#' @importFrom galah galah_geolocate
#' @examples \dontrun{
#' # Search for records within a polygon using a shapefile
#' location <- sf::st_read("path/to/shapefile.shp")
#' sbdi_call() |>
#'   sbdi_identify("vulpes") |>
#'   sbdi_geolocate(location) |>
#'   atlas_counts()
#'
#' # Search for records within the bounding box of a shapefile
#' location <- sf::st_read("path/to/shapefile.shp")
#' sbdi_call() |>
#'   sbdi_identify("vulpes") |>
#'   sbdi_geolocate(location, type = "bbox") |>
#'   atlas_counts()
#'
#' # Search for records within a polygon using an `sf` object
#' location <- "POLYGON((142.3 -29.0,142.7 -29.1,142.7 -29.4,142.3 -29.0))" |>
#'  sf::st_as_sfc()
#' sbdi_call() |>
#'   sbdi_identify("reptilia") |>
#'   sbdi_polygon(location) |>
#'   atlas_counts()
#'
#' # Alternatively, we can use `st_crop()` as a synonym for `sbdi_polygon()`.
#' # Hence the above example can be rewritten as:
#' request_data() |>
#'   identify("reptilia") |>
#'   st_crop(location) |>
#'   count() |>
#'   collect()
#'
#' # Search for records using a Well-known Text string (WKT)
#' wkt <- "POLYGON((142.3 -29.0,142.7 -29.1,142.7 -29.4,142.3 -29.0))"
#' sbdi_call() |>
#'   sbdi_identify("vulpes") |>
#'   sbdi_geolocate(wkt) |>
#'   atlas_counts()
#'
#' # Search for records within the bounding box extracted from an `sf` object
#' location <- "POLYGON((142.3 -29.0,142.7 -29.1,142.7 -29.4,142.3 -29.0))" |>
#'   sf::st_as_sfc()
#' sbdi_call() |>
#'   sbdi_identify("vulpes") |>
#'   sbdi_geolocate(location, type = "bbox") |>
#'   atlas_counts()
#'
#' # Search for records using a bounding box of coordinates
#' b_box <- sf::st_bbox(c(xmin = 143, xmax = 148, ymin = -29, ymax = -28),
#'                      crs = sf::st_crs("WGS84"))
#' sbdi_call() |>
#'   sbdi_identify("reptilia") |>
#'   sbdi_geolocate(b_box, type = "bbox") |>
#'   atlas_counts()
#'
#' # Search for records using a bounding box in a `tibble` or `data.frame`
#' b_box <- tibble::tibble(xmin = 148, ymin = -29, xmax = 143, ymax = -21)
#' sbdi_call() |>
#'   sbdi_identify("vulpes") |>
#'   sbdi_geolocate(b_box, type = "bbox") |>
#'   atlas_counts()
#' }
#' @importFrom galah galah_geolocate
#' @export
sbdi_geolocate <- function(..., type = c("polygon", "bbox")) {
  galah_geolocate(... = ..., type = type)
}

#' @rdname sbdi_geolocate
#' @importFrom galah galah_polygon
#' @export
sbdi_polygon <- function(...){
  galah_polygon(... = ...)
}

#' @rdname sbdi_geolocate
#' @importFrom galah galah_bbox
#' @export
sbdi_bbox <- function(...) {
  galah_bbox(... = ...)
}
