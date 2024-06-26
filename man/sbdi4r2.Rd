% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sbdi4r2-package.R
\docType{package}
\name{sbdi4r2}
\alias{sbdi4r2-package}
\alias{sbdi4r2}
\title{Biodiversity Data from the GBIF Node Network
\pkg{sbdi4r2}}
\description{
This project enables the R community to access data and tools hosted by the
Swedish Biodiversity Data Infrastructure (SBDI). The goal of the project is
to enable basic species and related information to be queried and output in
standard formats for R.
`sbdi4r2` is based around the extensive web services provided
by the Swedish Biodiversity Data Infrastructure; see the API link below.
The Global Biodiversity Information Facility (GBIF; <https://www.gbif.org>)
provides tools to enable users to find, access, combine and visualise
biodiversity data. `sbdi4r2` (via `galah`) enables the R community to directly access data and
resources hosted by GBIF and several of it's subsidiary organisations, known
as 'nodes'. The basic unit of observation stored by these infrastructures is
an **occurrence** record, based on the Darwin Core' data standard
(<https://dwc.tdwg.org>); however `galah` also enables users to locate and
download taxonomic information, or associated media such images or sounds,
all while restricting their queries to particular taxa or locations. Users
can specify which columns are returned by a query, or restrict their results
to observations that meet particular quality-control criteria.
}
\section{Functions}{

**Piping functions**

  * [sbdi_call()] or \code{\link[=request_data]{request_()}} et al.  Start to build a data query
  * \code{\link[=collapse_galah]{collapse()}} Generate a query
  * \code{\link[=compute_galah]{compute()}} Compute a query
  * \code{\link[=collect_galah]{collect()}} Retrieve a database query

**Look up information**

  * [search_taxa()] Search for taxa using a text-search
  * [search_identifiers()] Search for taxa using taxonomic identifiers
  * [show_all()] & [search_all()] Data for generating filter queries
  * [show_values()] & [search_values()] Show or search for values _within_
  `fields`, `profiles`, `lists`, `collections`, `datasets` or `providers`

**Lazy data manipulation**

  * \code{\link[=identify.data_request]{identify()}} or [sbdi_identify()] Search for taxonomic identifiers
  * \code{\link[=filter.data_request]{filter()}} or [sbdi_filter()]Filter records
  * \code{\link[=select.data_request]{select()}} or [sbdi_select()]Fields to report information for
  * \code{\link[=group_by.data_request]{group_by()}} or [sbdi_group_by()] Fields to group counts by
  * \code{\link[=st_crop.data_request]{st_crop()}} or [sbdi_geolocate()] Specify a location
  * [apply_profile()] or [sbdi_apply_profile()] Restrict to data that pass predefined checks (ALA only)
  * \code{\link[=slice_head.data_request]{slice_head()}} Choose the first n rows of a download
  * \code{\link[=arrange.data_request]{arrange()}} Arrange rows of a query on the server side

**Download data**

  * [atlas_occurrences()] Download occurrence records
  * [atlas_counts()] or \code{\link[=count.data_request]{count()}} Count the number of records or species returned by a query
  * [atlas_species()] Download species lists
  * [atlas_taxonomy()] Return a section of the ALA taxonomic tree
  * [atlas_media()] View images and sounds available to download
  * [collect_media()] Download images and sounds

**Configure session**

  * [sbdi_config()] Package configuration options

**Cite**

  * [atlas_citation()] Citation for a dataset
}

\references{
\url{https://api.biodiversitydata.se/}
}
\seealso{
Useful links:
\itemize{
  \item \url{https://biodiversitydata-se.github.io/sbdi4r2/}
  \item Report bugs at \url{https://github.com/biodiversitydata-se/sbdi4r2/issues}
}

}
\author{
\strong{Maintainer}: Manash Shah \email{support@biodiversitydata.se} [contributor]

Authors:
\itemize{
  \item Debora Arlt
  \item Alejandro Ruete
}

}
\keyword{internal}
