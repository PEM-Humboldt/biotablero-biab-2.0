#### DESCRIPTION ####
# This function returns a data.frame with the area in hectares for each of the categories of a
# raster that is intersected by a set of geofences. It loops masking the raster over the set of
# geofences and then calculating the area (ha) of each of the raster categories. The function is
# designed for running in parallel by setting more than one core in the "n_cores" argument.
# The data.frame returned, and therefore, the arguments of this function respond to the data model
# defined for BioTablero.

#### ARGUMENTS ####
# temporal_path: folder path for writing the masked rasters by each geofence - character
# raster_file:   file path for the raster - character
# raster_res:    raster resolution in meters - numeric
# raster_year:   year of the raster information - integer
# categories:    data.frame/matrix matching raster values (1st column) with categories (2nd column)
# geo_file:      file path for the geofences file - character
# geo_layer:     geofences' layer name - character. It corresponds to how the layer is named when is 
#                added to a GIS software. Regularly, it corresponds to the same name of the file,
#                but it could change in geopackage files.
# geo_defined:   geofences definded for running statistics - character. If value is different to "all"
#                (the default), names must match those in the "geofence_type" field of the geofence
#                file (geo_file).
# n_cores:       number of cores for running in parallel - integer

#### FUNCTION ####
raster_category_area_by_geofence = function(temporal_path,
                                            raster_file,
                                            raster_res,
                                            raster_year,
                                            categories,
                                            geo_file,
                                            geo_layer,
                                            geo_defined = "all",
                                            n_cores){
  
  #### LOAD/INSTALL FUNCTIONS/LIBRARIES ####
  
  #Load previous functions
  source("./functions/load_libraries_function.R")
  
  #Load/install libraries
  load_libraries(c("gdalUtilities", "doParallel", "sf", "rasterDT","raster","rgdal"))
  
  #### MASK RASTER Y CALCULATE AREA ####
  
  #Read geofences file
  geo_data = read_sf(geo_file)
  
  #Convert sf object to data.frame
  geo_data = as.data.frame(geo_data %>% st_set_geometry(NULL))
  
  #Delimit data to geofences defined only if "geo_defined" argument is different to "all"
  if (geo_defined != "all"){
    geo_data = geo_data[geo_data$geofence_type %in% geo_defined,]
  }
  
  #Clean memory
  gc()
  
  #Define cluster object for parallelizing
  cl = makeCluster(n_cores,type = "PSOCK")
  
  #Initialize parallel cluster
  registerDoParallel(cl,cores=n_cores)
  
  #Mask raster iterating (in parallel) over the geofences' polygons
  geo_results = foreach(i = 1:nrow(geo_data), .combine = rbind.data.frame, .packages = c("raster","rasterDT","gdalUtilities"), .inorder = FALSE) %dopar% {
    
    #Set output masked raster name
    geo_name = paste0(geo_data$geofence_type[i], "_",geo_data$geofence_id[i])
    
    #Set SQL_query
    sql_query = paste0("select * from geofences where geofence_type='", geo_data$geofence_type[i], 
                       "' AND geofence_id='",geo_data$geofence_id[i], "'")
    
    #Mask raster
    r_masked = try(gdalUtilities::gdalwarp(srcfile = raster_file,
                                           dstfile = paste0(temporal_path, "/", geo_name, ".tif"),
                                           cutline = geo_file,
                                           cl = geo_layer,
                                           csql = sql_query,
                                           crop_to_cutline = TRUE,
                                           multi = TRUE,
                                           co = list("COMPRESS=DEFLATE", "ZLEVEL=1"),
                                           overwrite = TRUE), silent = TRUE)
    
    #Read the masked raster
    r_masked = try(raster(r_masked), silent = TRUE)
    
    #Calculate the raster values frequencies
    frequencies = try(freqDT(r_masked, useNA="no", merge=TRUE), silent = TRUE)
    
    #Test if the raster is empty or if it was not created
    if (nrow(frequencies) == 0 || class(r_masked) == "try-error"){
      
      #If true there is no intersection between the geofence and the raster, or the raster was not created because the geofence extent
      #does not intersect at least one full raster pixel. In both cases it returns NA in "category" and "area_ha" fields
      frequencies = data.frame(geofence_id = geo_data$geofence_id[i],
                               geofence_type = geo_data$geofence_type[i],
                               year = raster_year,
                               category = NA,
                               area_ha = NA,
                               stringsAsFactors = FALSE)
      
      #Return final table to the loop
      return(frequencies)
      
      #Clean memory
      gc()
    }
    
    #If false there it is intersection between the geofence and the raster and it was created, then the area is calculated
    else {
      
      #Append raster categories to frequencies table
      frequencies = base::merge(frequencies, categories, by.x="ID", by.y=colnames(categories)[1])
      
      #Calculate raster values area in ha
      frequencies$freq = frequencies$freq * (raster_res^2/10000)
      
      #Set final table
      frequencies = data.frame(geofence_id = geo_data$geofence_id[i],
                               geofence_type = geo_data$geofence_type[i],
                               year = raster_year,
                               category = as.vector(t(frequencies[,3])),
                               area_ha = frequencies$freq,
                               stringsAsFactors = FALSE)
      
      #Return final table to the loop
      return(frequencies)
      
      #Clean memory
      gc()
    }
  }
  
  #Stop parallel cluster
  stopCluster(cl)
  
  #Clean memory
  gc()
  
  #Return final table with area data by category and geofence
  return(geo_results)
}