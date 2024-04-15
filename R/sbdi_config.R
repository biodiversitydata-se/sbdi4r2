#' Get or set configuration options that control sbdi4r2 behavior
#'
#' @references \url{https://api.biodiversitydata.se/}
#' @references \url{https://spatial.biodiversitydata.se/layers/} this will eventually move to the api link
#'
#' #' The `sbdi4r2` package (via the `galah` package) supports large data downloads, and also
#' interfaces with the SBDI which requires that users of some services
#' provide a registered email address and reason for downloading data. The
#' `sbdi_config` function provides a way to manage these issues as simply
#' as possible.
#'
#' Invoking \code{sbdi_config()} with no arguments returns a list with the current values of the options.
#'
#' \code{sbdi_reasons()} returns a data frame with information describing the valid options for \code{download_reason_id}
#'
#' @param atlas string: Living Atlas to point to, SBDI by default. Can be an organisation name, 
#' acronym, or region (see [show_all_atlases()] for admissible values).
#' @param \dots Options can be defined using the form `name = "value"`.
#' Valid arguments are:
#'
#'  *  `api-key` string: A registered API key (currently unused).
#'
#'  *  `directory` string: the directory to use for the cache.
#'  By default this is a temporary directory, which means that results will
#'  only be cached within an R session and cleared automatically when the user exits R.
#'  The user may wish to set this to a non-temporary directory for
#'  caching across sessions. The directory must exist on the file system.
#'
#'  *  `download_reason_id` numeric or string: the "download reason" required
#'  by some ALA services, either as a numeric ID (currently 0--13)
#'  or a string (see `show_all(reasons)` for a list of valid ID codes and
#'  names). By default this is NA. Some ALA services require a valid
#'  download_reason_id code, either specified here or directly to the
#'  associated R function.
#'
#'  *  `email` string: An email address that has been registered with the chosen
#'  atlas. For the SBDI, you can register at
#'  [this address](https://auth.biodiversitydata.se/userdetails/registration/createAccount).
#'
#'  *  `password` string: A registered password (GBIF only)
#'
#'  *  `run_checks` logical: should `sbdi4r2` run checks for filters
#'  and columns. If making lots of requests sequentially, checks can slow down
#'  the process and lead to HTTP 500 errors, so should be turned off. Defaults
#'  to TRUE.
#'
#'  *  `send_email` logical: should you receive an email for each query to
#'  [atlas_occurrences()]? Defaults to `FALSE`; but can be
#'  useful in some instances, for example for tracking DOIs assigned to
#'  specific downloads for later citation.
#'
#'  *  `username` string: A registered username (GBIF only)
#'
#'  *  `verbose` logical: should `sbdi4r2` give verbose such as progress bars? Defaults to FALSE.
#'
#' @return For `sbdi_config()`, a `list` of all options.
#' When `sbdi_config(...)` is called with arguments, nothing is returned but the configuration is set.
#'
#'
#' @examples \dontrun{
#' # To download occurrence records, enter your email in `sbdi_config()`.
#' # This email should be registered with the atlas in question.
#' sbdi_config(email = "your-email@email.com")
#'
#' # Some ALA services require that you add a reason for downloading data.
#' # Add your selected reason using the option `download_reason_id`
#' sbdi_config(download_reason_id = 0)
#'
#' # To look up all valid reasons to enter, use `show_all(reasons)`
#' # show_all(reasons) or simply `show_reasons()`
#'
#' # Make debugging in your session easier by setting `verbose = TRUE`
#' sbdi_config(verbose = TRUE)
#' }
#' @importFrom galah galah_config
#' @export sbdi_config
sbdi_config <- function(atlas = "SBDI",...) {
  galah_config(atlas = atlas, ... = ...)
  galah_config()
}

#' List valid download reasons
#' @rdname sbdi_config
#' @importFrom galah show_all
#' @export
sbdi_reasons <- function() {
  show_all("reasons")
}
