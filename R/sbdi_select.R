#' @title Specify fields for occurrence download
#'
#' @description GBIF and it's partner nodes store content in hundreds of
#' different fields, and users often require thousands or millions of records at
#' a time. To reduce time taken to download data, and limit complexity of the
#' resulting `tibble`, it is sensible to restrict the fields returned by
#' [atlas_occurrences()]. This function allows easy selection of fields, or
#' commonly-requested groups of columns, following syntax shared with
#' `dplyr::select()`.
#'
#' The full list of available fields can be viewed with `show_all(fields)`. Note
#' that `select()` and `sbdi_select()` are supported for all atlases that allow
#' downloads, with the exception of GBIF, for which all columns are returned.
#'
#' @param ... zero or more individual column names to include
#' @param group `string`: (optional) name of one or more column groups to
#' include. Valid options are `"basic"`, `"event"` and
#' `"assertions"`
#' @return A tibble
#' specifying the name and type of each column to include in the
#' call to `atlas_counts()` or `atlas_occurrences()`.
#' @details
#' Calling the argument `group = "basic"` returns the following columns:
#'
#'   * `decimalLatitude`
#'   * `decimalLongitude`
#'   * `eventDate`
#'   * `scientificName`
#'   * `taxonConceptID`
#'   * `recordID`
#'   * `dataResourceName`
#'   * `occurrenceStatus`
#'
#' Using `group = "event"` returns the following columns:
#'
#'   * `eventRemarks`
#'   * `eventTime`
#'   * `eventID`
#'   * `eventDate`
#'   * `samplingEffort`
#'   * `samplingProtocol`
#'
#' Using `group = "media"` returns the following columns:
#'
#'   * `multimedia`
#'   * `multimediaLicence`
#'   * `images`
#'   * `videos`
#'   * `sounds`
#'
#' Using `group = "assertions"` returns all quality assertion-related
#' columns. The list of assertions is shown by `show_all_assertions()`.
#'
#' @seealso [search_taxa()], [sbdi_filter()] and
#' [sbdi_geolocate()] for other ways to restrict the information returned
#' by [atlas_occurrences()] and related functions; [atlas_counts()]
#' for how to get counts by levels of variables returned by `sbdi_select`;
#' `show_all(fields)` to list available fields.
#'
#' @examples \dontrun{
#' # Download occurrence records of *Perameles*,
#' # Only return scientificName and eventDate columns
#' sbdi_config(email = "your-email@email.com")
#' sbdi_call() |>
#'   sbdi_identify("perameles")|>
#'   sbdi_select(scientificName, eventDate) |>
#'   atlas_occurrences()
#'
#' # Only return the "basic" group of columns and the basisOfRecord column
#' sbdi_call() |>
#'   sbdi_identify("perameles") |>
#'   sbdi_select(basisOfRecord, group = "basic") |>
#'   atlas_occurrences()
#'
#' # When used in a pipe, `sbdi_select()` and `select()` are synonymous.
#' # Hence the previous example can be rewritten as:
#' request_data() |>
#'   identify("perameles") |>
#'   select(basisOfRecord, group = "basic") |>
#'   collect()
#' }
#' @importFrom galah galah_select
#' @export
sbdi_select <- function(..., group){
  galah_select(..., group)
}

#' Internal function to specify 'basic' columns in `select()`
#' @noRd
#' @keywords Internal
default_columns <- function() {
  atlas <- pour("atlas", "region")
  switch (atlas,
          "Austria" = c("id",
                        "taxon_name",
                        "taxon_concept_lsid",
                        "latitude",
                        "longitude",
                        "occurrence_date",
                        "occurrence_status",
                        "data_resource_uid"),
          "Guatemala" = c("id",
                          "taxon_name",
                          "taxon_concept_lsid",
                          "latitude",
                          "longitude",
                          "occurrence_date",
                          "occurrence_status",
                          "data_resource_uid"),
          "Spain" = c("id",
                      "scientificName",
                      "taxonConceptID",
                      "decimalLatitude",
                      "decimalLongitude",
                      "eventDate",
                      "occurrenceStatus",
                      "dataResourceUid"),
          # "Sweden" = c("id",
          #              "taxon_name",
          #               "taxon_concept_lsid",
          #               "latitude",
          #               "longitude",
          #               "occurrence_date",
          #               "occurrence_status",
          #               "data_resource_uid"),
          "United Kingdom" = c("id",
                               "taxon_name",
                               "taxon_concept_lsid",
                               "latitude",
                               "longitude",
                               "occurrence_date",
                               "occurrence_status",
                               "data_resource_uid"),
          c("recordID", # note this requires that the ALA name (`id`) be corrected
            "scientificName",
            "taxonConceptID",
            "decimalLatitude",
            "decimalLongitude",
            "eventDate",
            "occurrenceStatus",
            "dataResourceName")
  )
}
