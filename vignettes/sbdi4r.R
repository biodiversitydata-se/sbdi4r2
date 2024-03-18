## ----setup, include = FALSE---------------------------------------------------
library(knitr)
opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message=FALSE------------------------------------------------------------
library(sbdi4r2)
library(galah)
sbdi_config(email = "aleruete@gmail.com")

## ----message=FALSE------------------------------------------------------------
to_install <- c( "dplyr", "ggplot2",  "lubridate", "leaflet","maps", "mapdata",
                 "sf",  "tidyr", "vegan") 
to_install <- to_install[!sapply(to_install, requireNamespace, quietly = TRUE)]
if (length(to_install) > 0)
    install.packages(to_install, repos = "http://cran.us.r-project.org")

## ----warning=FALSE, message=FALSE, eval=FALSE---------------------------------
#  sx <- sbdi_call() |>
#    sbdi_identify("parus") |>
#    atlas_species()
#  sx

## ----message=FALSE, eval=FALSE------------------------------------------------
#  sx <- sbdi_call() |>
#      sbdi_identify("paridae") |>
#      atlas_species()

## ----message=FALSE, eval=FALSE------------------------------------------------
#  sx <- sx |>
#    filter(Genus != "") ## restrict to species and not hybrids

## -----------------------------------------------------------------------------
focal_spp <- search_taxa("Callitriche cophocarpa")
## or equally valid
focal_spp <- search_taxa("sommarlånke")
focal_spp$taxon_concept_id

x <- sbdi_call() |> 
  sbdi_identify("Callitriche cophocarpa") |>
  atlas_occurrences()

table(x$dataResourceName)

## ----message=FALSE------------------------------------------------------------
taxa <- c("Callitriche", "Anarrhinum")
x <- sbdi_call() |> 
  sbdi_identify(taxa) |>
  atlas_occurrences()

table(x$dataResourceName)

## ----message=FALSE------------------------------------------------------------
taxa <- "Callitriche cophocarpa"
xf <- sbdi_call() |> 
  sbdi_identify(taxa) |>
  filter(dataResourceName == "Lund University Biological Museum - Botanical collection (LD)") |>
  atlas_occurrences()
table(xf$dataResourceName)

## ----eval= FALSE--------------------------------------------------------------
#  show_all(fields)
#  
#  show_all(fields) |>
#    filter(grepl("cl", id)) |>
#    as.data.frame()

## ----eval= FALSE--------------------------------------------------------------
#  show_all(fields) |>
#    filter(description == "LA-regioner") |>
#    as.data.frame()
#  
#  show_all(fields) |>
#    filter(description == "Län") |>
#    as.data.frame()
#  
#  xf <- sbdi_call() |>
#    sbdi_identify(taxa) |>
#    filter("cl10097" == "Uppsala") |>
#    atlas_occurrences()
#  
#  

## ----message=FALSE------------------------------------------------------------
xf <- sbdi_call() |> 
  sbdi_identify(taxa) |>
  filter(coordinateUncertaintyInMeters <= 100) |>
  atlas_occurrences()

## ----message=FALSE------------------------------------------------------------
x2yr <- sbdi_call() |> 
  sbdi_identify(taxa) |>
  filter(year == 2010 | year == 2020) |>
  atlas_occurrences()
nrow(x2yr)

## ----message=FALSE, fig.width=8, fig.height=6---------------------------------
xf <- sbdi_call() |> 
  sbdi_identify(taxa) |>
  filter(year >= 2010, year <= 2020) |>
  atlas_occurrences()

library(lubridate)
hist(year(xf$eventDate), xlab = "Year", main = "")

## ----message=FALSE, fig.width=8, fig.height=6---------------------------------
xf <- sbdi_call() |> 
  sbdi_identify(taxa) |>
  filter(year >= 2010, year <= 2020,
         month >= 6, month <= 8) |>
  atlas_occurrences()

hist(month(xf$eventDate), xlab = "Month", main = "")

## ----message=FALSE------------------------------------------------------------
xbor <- sbdi_call() |> 
  sbdi_identify(taxa) |>
  filter(basisOfRecord == "PreservedSpecimen") |>
  select(basisOfRecord) |> 
  atlas_occurrences()
                  

unique(xbor$basisOfRecord)

## ----assertions---------------------------------------------------------------
show_all(assertions)
assertFatal <- show_all(assertions) |> 
  filter(category == "Error")
search_all(assertions, "longitude")

xassert <- sbdi_call() |> 
  sbdi_identify(taxa) |>
  select(assertFatal$id) |> 
  atlas_occurrences()

colSums(xassert[,assertFatal$id])

assertWarning <- show_all(assertions) |> 
  filter(category == "Warning")

xassert <- sbdi_call() |> 
  sbdi_identify(taxa) |>
  select(assertWarning$id) |> 
  atlas_occurrences()

colSums(xassert[,assertWarning$id])

## ----message=FALSE, fig.width=9, fig.height=9---------------------------------
xf <- sbdi_call() |> 
  sbdi_identify(taxa) |>
  atlas_occurrences()

 ## note this should ideally be states        
  data("swe_wgs84", package = "sbdi4r2", envir = environment())
  
  plot(swe_wgs84[["Border"]]$geometry, col = "grey", border = NA) 
  points(xf$decimalLongitude, xf$decimalLatitude, pch = 19, col = "black")

## -----------------------------------------------------------------------------
library(sf)

xf_sf <- xf |> 
  filter(!is.na(decimalLatitude),
         !is.na(decimalLongitude)) |> 
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

plot(swe_wgs84[["Border"]]$geometry, col = "grey", border = NA) 
plot(xf_sf$geometry, pch = 19, add = TRUE)

## ----message=FALSE, fig.width=9, fig.height=9---------------------------------
library(leaflet)
## make a link to the web page for each occurrence
popup_link <- paste0("<a href=\"https://records.biodiversitydata.se/occurrences/",
                      xf_sf$recordID,"\">Link to occurrence record</a>")

## blank map, with imagery background
m <- leaflet() |>  
  addProviderTiles("Esri.WorldImagery") |>
  addCircleMarkers(data = xf_sf , 
                   radius = 2, fillOpacity = .5, opacity = 1,
                   popup = popup_link)
m

## ----eval=FALSE---------------------------------------------------------------
#  # save as data.frame
#  Callitriche <- as.data.frame(xf)
#  
#  # simplyfy data frame
#  calli <- data.frame(Callitriche$scientificName,
#                      Callitriche$decimalLatitude,
#                      Callitriche$decimalLongitude)
#  # simplify column names
#  colnames(calli) <- c("species","latitude","longitude")
#  # remove rows with missing values (NAs)
#  calli <- na.omit(calli)
#  
#  # save new dataframe
#  write.csv(calli, "Callitriche.csv")

## ----message=FALSE, warning=FALSE---------------------------------------------
library(sf)
# load some shapes over Sweden
# Political borders
data("swe_wgs84", package = "sbdi4r2", envir = environment()) 
# A standard 50km grid
data("Sweden_Grid_50km_Wgs84", package = "sbdi4r2", envir = environment()) 

grid <- Sweden_Grid_50km_Wgs84
# grid <- st_transform(grid, crs = st_crs(3006))

nObs <- nrow(xf_sf)

## overlay the data with the grid
listGrid <- st_intersects(grid, xf_sf)

ObsInGridList <- list()
for (i in seq(length(listGrid))) {
  if (length(listGrid[[i]]) == 0) {
    ObsInGridList[[i]] <- NA
  } else {
    ObsInGridList[[i]] <- st_drop_geometry(xf_sf[listGrid[[i]],])
  }
}
wNonEmpty <- which( unlist(lapply(ObsInGridList, function(x) !all(is.na(x)))) )
if (length(wNonEmpty) == 0) message("Observations don't overlap any grid cell.")

## check nObs
sum(unlist(lapply(ObsInGridList, nrow))) == nObs
length(ObsInGridList) == nrow(grid)


## -----------------------------------------------------------------------------
## apply a summary over the grid
nCells <- length(ObsInGridList)

res <- data.frame("nObs" = as.numeric(rep(NA, nCells)),
                  "nYears" = as.numeric(rep(NA, nCells)),
                  row.names = row.names(grid),
                  stringsAsFactors = FALSE)

cols2use <- c("scientificName", "eventDate")

dataRes <- lapply(ObsInGridList[wNonEmpty], function(x){
  x <- x[,cols2use]
  x$year <- year(x$eventDate)
  colnames(x) <- c("scientificName", "year")
  
  return(c("nObs" = nrow(x),
           "nYears" = length(unique(x[,"year"]))
  ))
})

dataRes <- as.data.frame(dplyr::bind_rows(dataRes, .id = "id"))

res[wNonEmpty,] <- dataRes[,-1]
res$nObs <- as.numeric(res$nObs)
resSf <- st_as_sf(cbind(res, st_geometry(grid)) )
rownames(resSf) <- grid$id

## ----grid, message=FALSE, warning=FALSE, fig.width=6, fig.height=6------------
palBW <- leaflet::colorNumeric(palette = c("white", "navyblue"),
                               domain = c(0, max(resSf$nObs, na.rm = TRUE)), 
                               na.color = "transparent")
oldpar <- par()
par(mar = c(1,1,0,0))
plot(resSf$geometry, col = palBW(resSf$nObs), border = NA)
plot(swe_wgs84$Border, border = 1, lwd = 1, add = T)
legend("bottomleft", 
       legend = round(seq(0, max(resSf$nObs, na.rm = TRUE), length.out = 5)),
       col = palBW(seq(0, max(resSf$nObs, na.rm = TRUE), length.out = 5)),
       title = "Number of \nobservations", pch = 15, bty = "n")
suppressWarnings(par(oldpar))

## ----message=FALSE, warning=FALSE---------------------------------------------
counties <- swe_wgs84$Counties
obs <- st_transform(xf_sf, crs = st_crs(counties))

## overlay the data with the counties
listGrid <- st_intersects(counties, obs)

ObsInCountyList <- list()
for (i in seq(length(listGrid))) {
  if (length(listGrid[[i]]) == 0) {
    ObsInCountyList[[i]] <- NA
  } else {
    ObsInCountyList[[i]] <- st_drop_geometry(xf_sf[listGrid[[i]],])
  }
}
wNonEmpty <- which( unlist(lapply(ObsInCountyList, function(x) !all(is.na(x)))) )
if (length(wNonEmpty) == 0) message("Observations don't overlap any grid cell.")

## check nObs
sum(unlist(lapply(ObsInCountyList, nrow))) == nObs # some observations are not in the counties territory
length(ObsInCountyList) == nrow(counties)

## apply a summary over the grid
nCells <- length(ObsInCountyList)

res <- data.frame("nObs" = as.numeric(rep(NA, nCells)),
                  "nYears" = as.numeric(rep(NA, nCells)),
                  stringsAsFactors = FALSE)

cols2use <- c("scientificName", "eventDate")

dataRes <- lapply(ObsInCountyList[wNonEmpty], function(x){
  x <- x[,cols2use]
  x$year <- year(x$eventDate)
  colnames(x) <- c("scientificName", "year")
  
  return(c("nObs" = nrow(x),
           "nYears" = length(unique(x[,"year"]))
  ))
})


dataRes <- as.data.frame(dplyr::bind_rows(dataRes, .id = "id"))
res[wNonEmpty,] <- dataRes[,-1]
res$nObs <- as.numeric(res$nObs)

resSf <- st_as_sf(cbind(res, st_geometry(counties)))
rownames(resSf) <- counties$LnNamn

## ----counties, warning=FALSE, fig.width=6, fig.height=6-----------------------
palBW <- leaflet::colorNumeric(c("white", "navyblue"), 
                               c(0, max(resSf$nObs, na.rm = TRUE)), 
                               na.color = "transparent")
oldpar <- par()
par(mar = c(1,1,0,0))
plot(resSf$geometry, col = palBW(resSf$nObs), border = NA)
plot(swe_wgs84$Border, border = 1, lwd = 1, add = T)
text(st_coordinates(st_centroid(counties)), 
    labels = as.character(counties$LnNamn), font = 2, cex = .5 )
legend("bottomleft", 
       legend = round(seq(0, max(resSf$nObs, na.rm = TRUE), length.out = 5)),
       col = palBW(seq(0, max(resSf$nObs, na.rm = TRUE), length.out = 5)),
       title = "Number of \nobservations", pch = 15, bty = "n")
suppressWarnings(par(oldpar))

## ----warning=FALSE, fig.width=8, fig.height=6---------------------------------
countiesLab <- as.character(counties$LnNamn)
## Add a column to the obs data.frame to hold the id of the overlapped polygon, 
## in this case, Län (county) and plot which observation didn't fall with any 
## territory.
obs$county <- countiesLab[as.integer(st_intersects(obs, counties))]

oldpar <- par()
par(mar = c(1,1,0,0))
plot(counties$geometry, border = 1, lwd = 1)
plot(obs$geometry[which(is.na(obs$county))], 
     pch = 19, cex = .5, col = "red", add = T)
suppressWarnings(par(oldpar))

## ----eval=FALSE---------------------------------------------------------------
#  shape <- st_read(dsn = file.path("your/path/to/file", "Kommun_Sweref99TM_region.shp"))

## -----------------------------------------------------------------------------
shape <- swe$Municipalities
## extract just the Municipality of Örebro
shape <- shape[shape$KnNamn == "Örebro", ]

## ----eval=FALSE---------------------------------------------------------------
#  wkt <- shape |>
#    st_transform(crs = st_crs(4326)) |>
#    st_geometry() |>
#    st_as_text()

## ----warning=FALSE------------------------------------------------------------
shape <- st_transform(shape,
                      crs = st_crs(4326))
shape <- st_union(shape)

## extract the convex hull of the polygon to reduce the length of the WKT string
shape_ch <- st_convex_hull(shape)

# cast it as MULTIPOLYGON as this is what SBDIs API need
# NOTE: as of today, the SBDI APIs will only work properly if the polygon is 
# submitted as a MULTIPOLYGON
shape_ch <- st_cast(shape_ch, to = "POLYGON")

# create WKT string
wkt <- st_as_text(shape_ch)

## ----eval=FALSE---------------------------------------------------------------
#  sbdi_call() |>
#    sbdi_identify("amphibia") |>
#    sbdi_geolocate(wkt) |>
#    filter(taxonRank == "species") |>
#    atlas_occurrences() |>
#    group_by(taxonConceptID, scientificName) |>
#    reframe(count = n())

## ----message=FALSE, echo=FALSE------------------------------------------------
tryCatch({
  sbdi_call() |>
  sbdi_identify("amphibia") |>
  sbdi_geolocate(wkt) |>
  filter(taxonRank == "species") |> 
  atlas_occurrences() |> 
  group_by(taxonConceptID, scientificName) |> 
  reframe(count = n())
}, error = function(e) { print(e$message)})

## ----eval=FALSE---------------------------------------------------------------
#  ## A rough polygon around the Mällardalen
#  wkt <- "POLYGON((14.94 58.88, 14.94 59.69, 18.92 59.69, 18.92 58.88, 14.94 58.88))"
#  
#  ## define some environmental layers of interest [see sbdi_fields(fields_type = "occurrence")]
#  # el10009 WorldClim Mean Temperature of Warmest Quarter https://spatial.biodiversitydata.se/ws/layers/view/more/worldclim_bio_10
#  # el10011 WorldClim Annual Precipitation https://spatial.biodiversitydata.se/ws/layers/view/more/worldclim_bio_12
#  env_layers <- c("el10009","el10011")
#  
#  ## Download the data.  We use the `occurrences()` function, adding environmental
#  ## data via the 'extra' parameter.
#  x <- sbdi_call() |>
#    sbdi_identify("Fabaceae") |>
#    sbdi_geolocate(wkt) |>
#    filter(taxonRank == "species") |>
#    select(recordID, scientificName, taxonConceptID, taxonRank,
#           decimalLatitude, decimalLongitude,
#           eventDate, occurrenceStatus, all_of(env_layers)) |>
#    atlas_occurrences()

## ----eval=FALSE---------------------------------------------------------------
#  library(tidyverse)
#  xgridded <- x |>
#      ## discard genus- and higher-level records
#      filter(taxonRank %in%
#               c("species", "subspecies", "variety", "form", "cultivar")) |>
#      mutate(longitude = round(decimalLongitude * 5)/5,
#             latitude = round(decimalLatitude * 5)/5,
#             el10009 = el10009 /10) %>%
#      ## average environmental vars within each bin
#      group_by(longitude,latitude) %>%
#      mutate(annPrec = mean(el10011, na.rm=TRUE),
#             meanTempWarmQuart = mean(el10009, na.rm=TRUE)) |>
#      ## subset to vars of interest
#      select(longitude, latitude, scientificName, annPrec, meanTempWarmQuart) |>
#      ## take one row per cell per species (presence)
#      distinct() |>
#      ## calculate species richness
#      mutate(richness = n()) |>
#      ## convert to wide format (sites by species)
#      mutate(present = 1) |>
#      do(spread(data =., key = scientificName, value = present, fill = 0)) |>
#      ungroup()
#  
#  ## where a species was not present, it will have NA: convert these to 0
#  sppcols <- setdiff(names(xgridded),
#                     c("longitude", "latitude",
#                       "annPrec",
#                       "meanTempWarmQuart",
#                       "richness"))
#  xgridded <- xgridded |>
#    mutate_at(sppcols, function(z) ifelse(is.na(z), 0, z))
#  saveRDS(xgridded, file = "vignette_fabaceae.rds")

## ----include=FALSE------------------------------------------------------------
## load data from a local copy so that vignette building doesn't require downloading a big chunk of data and slow sites-by-species processing
## this file generated by running the above unevaluated code blocks, then
## saveRDS(xgridded, file="vignette_fabaceae.rds")
xgridded <- readRDS("vignette_fabaceae.rds")
sppcols <- setdiff(names(xgridded), c("longitude", "latitude", 
                                      "worldClimAnnualPrecipitation", 
                                      "worldClimMeanTemperatureOfWarmestQuarter", 
                                      "richness"))

## ----message=FALSE, warning=FALSE---------------------------------------------
xgridded[, 1:10]

## ----warning=FALSE, fig.width=8, fig.height=6---------------------------------
library(ggplot2)
ggplot(xgridded, aes(longitude, richness)) + 
  labs(x = "Longitud (º)", 
       y = "Species richness") +
  lims(y = c(0,100)) +
  geom_point() + 
  theme_bw()

## ----warning=FALSE, fig.width=8, fig.height=6---------------------------------
ggplot(xgridded, aes(meanTempWarmQuart, annPrec, 
                     colour = richness)) +
  labs(x = "Mean temperature of warmest quarter (ºC)" , 
       y = "Annual precipitation (mm)",
       colour = "Species \nrichness") + 
  scale_colour_distiller(palette = "Spectral") +
  geom_point(size=3) + 
  theme_bw()

## ----fig.width=6, fig.height=6, message=FALSE, warning=FALSE------------------
library(vegan)
## Bray-Curtis dissimilarity
D <- vegdist(xgridded[, sppcols], "bray")
## UPGMA clustering
cl <- hclust(D, method = "ave")
## plot the dendrogram
plot(cl)
## extract group labels at the 5-group level
grp <- cutree(cl, 5)
## coalesce small (outlier) groups into a single catch-all group
sing <- which(table(grp) < 5)
# grp[grp %in% sing] <- 6 ## put these in a new combined group
grp <- sapply(grp, function(z)which(unique(grp) == z)) ## renumber groups
xgridded$grp <- as.factor(grp)
## plot
## colours for clusters
thiscol <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", 
             "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")
ggplot(xgridded, aes(longitude, latitude, colour = grp)) + 
  labs(x = "Longitude", y = "Latitude", colour = "Group") + 
  geom_point(size = 3) +
  scale_colour_manual(values = thiscol) + 
  theme_bw()

## or a slightly nicer map plot
library(maps)
library(mapdata)
oldpar <- par()
par(mar = c(1,1,0,0))
map("worldHires", "Sweden", 
    xlim = c(14.5, 20), ylim = c(58.8, 59.95), 
    col = "gray90", fill = TRUE)
with(xgridded, points(longitude, latitude, 
                      pch = 21, col = thiscol[grp], 
                      bg = thiscol[grp], cex = 0.75))
suppressWarnings(par(oldpar))

