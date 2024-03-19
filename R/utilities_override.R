##---------------------------------------------------------------
##                Data request helper functions                --
##---------------------------------------------------------------
# Override galah until fixed centrally

# image_fields <- function() {
#   atlas <- pour("atlas", "region")
#   switch (atlas,
#           "Austria" = "all_image_url",
#           "Guatemala" = "all_image_url",
#           "Spain" = "all_image_url",
#           c("images", "videos", "sounds")
#   )
# }

species_facets <- function(){
  atlas <- pour("atlas", "region")
  switch(atlas,
         "Australia" = "speciesID",
         # "Austria" = "species_guid",
         # "Brazil" = "species_guid",
         # "Canada" = "species_guid",
         "Sweden" = "speciesID",
         "France" = "speciesID",
         "Spain" = "speciesID",
         "species_guid"
  )
}