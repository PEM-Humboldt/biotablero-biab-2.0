

# Install required packages

packages <- c("rstac", "tibble", "sp", "sf", "rgdal",  "lubridate", "dplyr",
              "rgbif", "tidyr", "stars", "raster", "terra", "rjson", "RCurl")
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


library("devtools")
if (!"stacatalogue" %in% installed.packages()[,"Package"]) devtools::install_github("ReseauBiodiversiteQuebec/stac-catalogue")
if (!"gdalcubes" %in% installed.packages()[,"Package"]) devtools::install_github("appelmar/gdalcubes_R")

## Load required packages
library("rjson")
library("gdalcubes")
library("stacatalogue")
library("rstac")
library("tibble")
library("sp")
library("rgdal")
library("lubridate")
library("RCurl")
library("sf")
library("dplyr")
library("rgbif")
library("tidyr")
library("stars")
library("ggplot2")
library("raster")
library("terra")
options(timeout = max(60000000, getOption("timeout")))


## Receiving args
args <- commandArgs(trailingOnly=TRUE)
outputFolder <- args[1] # Arg 1 is always the output folder
cat(args, sep = "\n")

setwd(outputFolder)
print("output:")
print(outputFolder)
input <- fromJSON(file=file.path(outputFolder, "input.json"))
print("Inputs: ")
print(input)


# Load functions

source("/scripts/climateMetrics/climateMetricsFunc.R")
source("/scripts/data/loadObservationsFunc.R")


# Pb if buffer set to 0, transformed into empty string. Warn JM about this
if (input$buffer_box == "...") buffer_box <- 0

# Case 1: we create an extent from a set of observations
if (input$use_obs) {
    obs <- load_observations(species = input$species,
        limit = input$species_limit,
        database = "gbif",
        year_start = input$species_year_start,
        year_end = input$species_year_end) 

# Reproject the obs to the data cube projection
obs_pts <-
          stacatalogue::project_coords(obs,
                         lon = "decimal_longitude",
                         lat = "decimal_latitude",
                         proj_from = "+proj=longlat +datum=WGS84",
                         proj_to = input$srs_cube)

# Create the extent (data cube projection)
bbox <- stacatalogue::points_to_bbox(obs_pts, buffer = buffer_box)


# Case 2: we use a shapefile
} else if (!is.null(input$shapefile_path)) {
    obs <- NULL
    shp <- sf::st_read(input$shapefile_path)
    bbox <- stacatalogue::shp_to_bbox(shp,
        proj_to = input$srs_cube)

# Case 3: we use a vector
} else if (!is.null(input$bbox)) {

obs <- NULL
bbox <- st_bbox(c(xmin = bbox[1], xmax = bbox[2], 
            ymax = bbox[3], ymin = bbox[4]), crs = st_crs(input$srs_cube))
    } 

n_year <- as.integer(substr(input$t1, 1, 4)) - as.integer(substr(input$t0, 1, 4)) + 1 
temporal_res <- paste0("P", n_year, "Y")

print("Loading current climate...")
cube_current <- stacatalogue::load_cube(collections = 'chelsa-monthly', 
                          bbox = bbox,
                          t0 = input$t0,
                          t1 = input$t1,
                          limit = 5000,
                          variable = "tas",
                          srs.cube = input$srs_cube,
                          spatial.res = input$spatial_res, # in meters
                          temporal.res = temporal_res, # see number of years t0 to t1
                          aggregation = input$aggregation,
                          resampling = "bilinear"
                         )
print("Loading current climate loaded.")

print("Loading future climate...")
cube_future <- stacatalogue::load_cube_projection(collections = 'chelsa-clim-proj',            
                          bbox = bbox,
                          limit = 5000,
                          srs.cube = input$srs_cube,
                         rcp = input$rcp, #ssp126, ssp370, ssp585
                          time.span =input$time_span, #"2011-2040", 2041-2070 or 2071-2100
                          variable = "bio1",
                        spatial.res = input$spatial_res,# in meters
                           temporal.res = temporal_res, 
                           aggregation = input$aggregation,
                           resampling = "bilinear"
  
)

print("Future climate loaded.")


print("Calculating metrics...")
tif <- climate_metrics(cube_current,
                          cube_future,
                          metric = "local",
                           t_match = input$t_match
                          )

output_tif <- file.path(outputFolder, "local_velocity.tif")
raster::writeRaster(x = tif,
                          output_tif,
                          overwrite = TRUE)

print("Metrics saved.")

# Outputing result to JSON
output <- list("output_tif" = output_tif,
               "metric" = "local_velocity")

jsonData <- toJSON(output, indent=2)
write(jsonData, file.path(outputFolder,"output.json"))




