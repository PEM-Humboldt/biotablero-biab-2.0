# Count runtime
ptm = proc.time()

# Set working directory
setwd("/scripts/LossAndPersistence")

# Load previous functions
source("./functions/load_libraries_function.R", chdir=T)
source("./functions/raster_category_area_by_geofence_function.R", chdir=T)

# Load/install libraries
load_libraries(c("doParallel","rlang","stats","sf","dplyr"))

#### DEFINE INPUT PARAMETERS ####

# Set temporal path
temporal_path = "./borrar"

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
geo_defined = "basin_areas"

# Set number of cores for parallelizing
n_cores = detectCores()

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
  
  # Identify for which geofences no data were obtained.
  geo_ids = paste0(results$geofence_type[is.na(results$area_ha)], "_", results$geofence_id[is.na(results$area_ha)])
  
  # Delimit results to geofences with data in 
  if (geo_ids != "_"){
    results = results[-which(is.na(results$area_ha)),]
  }
  
# Delimit results to geofences with data
results = results[-which(is.na(results$area_ha)),]

 #### ESTIMATE NO_FOREST AREA BY SUBTRACTING THE FOREST PERSISTENCE AND LOSS AREA FROM THE GEOFENCE AREA ####
  
  # Sum forest persistence and loss area by geofence
  forest_area = as.data.frame(results %>%
                                group_by(geofence_id, geofence_type) %>%
                                summarise(forest_area = sum(area_ha)))
  
  # Merge geofence area to forest area table
  forest_area$geofence_area = geo_data$area_ha[match(paste0(forest_area$geofence_id, "_", forest_area$geofence_type), paste0(geo_data$geofence_id, "_", geo_data$geofence_type))]
  
  # Calculate no_forest area
  forest_area$area_ha = forest_area$geofence_area - forest_area$forest_area
  
  # Add year and category fields to forest area table
  forest_area$year = raster_year[i]
  forest_area$category = "no_bosque"
  
  # Delete forest_area and geofence_area fields from forest area table
  forest_area["forest_area"] = NULL
  forest_area["geofence_area"] = NULL
  
  # Merge no_forest area to results table
  results = bind_rows(results, forest_area)
  
  # Write results table
  write.csv(results,
            paste0("./info_final/forest_persistence_loss_", raster_year[i], ".csv"),
            row.names = FALSE)

 #### VERIFY GEOFENCES FOR WHICH NO DATA WERE OBTAINED ####
  
  # Delimit geofences to those for which no data were obtained
  geo_data_nas = geo_data[paste0(geo_data$geofence_type, "_", geo_data$geofence_id) %in% geo_ids,]
  
  # Read raster data
  raster_data = raster(raster_file[i])
  
  # Set the extent of the plot
  xmin = min(st_bbox(geo_data_nas)$xmin, xmin(raster_data))
  xmax = max(st_bbox(geo_data_nas)$xmax, xmax(raster_data))
  ymin = min(st_bbox(geo_data_nas)$ymin, ymin(raster_data))
  ymax = max(st_bbox(geo_data_nas)$ymax, ymax(raster_data))
  
  # Save plot of geofences and persistence_loss for which no data were obtained for verification
  png(paste0("./info_final/forest_persistence_loss_verification_", raster_year[i], ".png"),
      height = 3000, width = 3000, res = 300)
  
  # Plot geofences and persistence_loss for which no data were obtained for verification
  plot(st_geometry(geo_data_nas), xlim=c(xmin,xmax), ylim=c(ymin,ymax))
  box()
  plot(raster_data, add = TRUE, legend = FALSE)
  dev.off()
  
  # Write geofencesfor which no data were obtained for verification
  write_sf(geo_data_nas,
           paste0("./info_final/forest_persistence_loss_verification_", raster_year[i], ".gpkg"))
  
  # Return results
  return(results)
  
  # Clean memory
  gc()
}

# Clean memory
gc()

# Write final results table
write.csv(forest_results,
          "./info_final/forest_persistence_loss.csv",
          row.names = FALSE)

#Get runtime
proc.time() - ptm #runtime: 2961.31 seconds