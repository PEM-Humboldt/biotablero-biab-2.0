# Count runtime
ptm = proc.time()

# Set working directory
setwd("/scripts/LossAndPersistence")

# Set output folder
args <- commandArgs(trailingOnly=TRUE)
outputFolder <- args[1] # Arg 1 is always the output folder

# Load previous functions
source("./functions/load_libraries_function.R")
source("./functions/raster_category_area_by_geofence_function.R")

# Load/install libraries
load_libraries(c("doParallel","sf","dplyr","rjson"))

#### DEFINE INPUT PARAMETERS ####

# Set temporal path
temporal_path = file.path(outputFolder, "borrar")

# Set land cover raster resolution in meters
raster_res = 27.7

# Set persistence_loss rasters files for each period
raster_file = c("./info_intermedia/perdida_persistencia_2000_2005.tif",
                "./info_intermedia/perdida_persistencia_2006_2010.tif",
                "./info_intermedia/perdida_persistencia_2011_2015.tif",
                "./info_intermedia/perdida_persistencia_2016_2021.tif")

# Read persistence_loss classes data
# data.frame with two columns: first column with persistence_loss raster values and second column with name of classes
categories = read.csv("./info_intermedia/pedida_persistencia_categories.csv", stringsAsFactors = F)

# Set persistence_loss periods of years
raster_year = c("2000-2005", "2006-2010", "2011-2015", "2016-2021")

# Set geofences file
geo_file = "./info_intermedia/geofences.gpkg"

# Set geofences' layer name
geo_layer = "geofences"

# Read geofences file
geo_data = read_sf(geo_file)

# Set defined geofences for running statistics
geo_defined = "environmental_authorities"

# Set number of cores for parallelizing
n_cores = detectCores()

#list output files
result_files <- list()

#### CALCULATE AREA FOR EACH PERSISTENCE_LOSS CLASS, GEOFENCE AND PERIOD ####

forest_results = foreach(i = 1:length(raster_file), .combine = rbind.data.frame, .verbose = TRUE) %do% {
  
  results = raster_category_area_by_geofence(temporal_path = temporal_path,
                                             raster_file = raster_file[i],
                                             raster_res = raster_res,
                                             raster_year = raster_year[i],
                                             categories = categories,
                                             geo_file = geo_file,
                                             geo_layer = geo_layer,
                                             geo_defined = geo_defined,
                                             n_cores = n_cores)
  
  # Clean memory
  gc()
  
  # Write results table
  #write.csv(results,
  #           paste0("/output/LossAndPersistence/forest_persistence_loss_", raster_year[i], ".csv"),
  #          row.names = FALSE)

  # Return results
  return(results)
  
  # Clean memory
  gc()
}

# Clean memory
gc()

#Get runtime
total_time = proc.time() - ptm #runtime: 2961.31 seconds

data_frame <- as.data.frame(do.call(rbind, forest_results))# Convert list to data frame rows

output <- list("output_json" =  toJSON(data_frame)) 
jsonData <- toJSON(output, indent=2)
write(jsonData, file.path(outputFolder, "output.json"))
