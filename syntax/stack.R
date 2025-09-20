processed_layer_path <- "/home/energysiting/data/processed_data/variables"


## solar
cov.tech <- c(file.path(processed_layer_path, "env_impactScore_solar.tif"),
                   file.path(processed_layer_path, "solar_capacity.tif"),
                   file.path(processed_layer_path, "solar_lag.tif"))

cov.names <- c("Environmental score",
               "Capacity factor",
               "Spatial lag")

raster_list <- list()
# Loop through each coefficient and convert to raster
for (i in 1:length(cov.names)) {
  # Add raster to the list
  raster_list[[cov.names[[i]]]] <- raster(cov.tech[[i]]) %>% 
    raster::scale()
}

# Create a raster stack from the list of coefficient rasters
solar_stack <- stack(raster_list)
raster::writeRaster(solar_stack, "./data/solar_covStack.tif", 
                    overwrite=TRUE)


### predictors old
cov.filePaths <- c(file.path(processed_layer_path, "transmission_110_qgis.tif"),
                   file.path(processed_layer_path, "land_acquisition.tif"),
                   file.path(processed_layer_path, "roads_qgis.tif"), 
                   file.path(processed_layer_path, "slope.tif"),
                   file.path(processed_layer_path, "pop_density.tif"),
                   
                   file.path(processed_layer_path, "hail_lag.tif"),
                   file.path(processed_layer_path, "fire.tif"),
                   file.path(processed_layer_path, "community.tif"),
                   
                   file.path(processed_layer_path, "lowincome.tif"),
                   file.path(processed_layer_path, "minor.tif"),
                   file.path(processed_layer_path, "employ.tif"),

                   file.path(processed_layer_path, "lulc_forest.tif"),
                   file.path(processed_layer_path, "lulc_grassland.tif"),
                   file.path(processed_layer_path, "lulc_shrubland.tif"),
                   file.path(processed_layer_path, "lulc_riparian.tif"),
                   file.path(processed_layer_path, "lulc_sparselyvegetated.tif"),
                   file.path(processed_layer_path, "lulc_agricultural.tif"),
                   file.path(processed_layer_path, "lulc_developed.tif"),
                   file.path(processed_layer_path, "lulc_other.tif"),
                   
                   file.path(processed_layer_path, "region_ne.tif"),
                   file.path(processed_layer_path, "region_mw.tif"),
                   file.path(processed_layer_path, "region_w.tif"),
                   file.path(processed_layer_path, "region_s.tif"),
                   file.path(processed_layer_path, "region_tex.tif"),
                   file.path(processed_layer_path, "region_mtw.tif")
)


cov.names <- c("Transmission dist",
               "Land acquisition",
               "Road dist", 
               "Slope",
               "Population density", 
               
               "Hail risk",
               "Fire risk",
               "Energy community",
               
               "Lowincome", 
               "Minority", 
               "Unemployment",
               
               "Forest", 
               "Grassland", 
               "Shrubland", 
               "Riparian", 
               "Vegetated", 
               "Agriculture", 
               "Developed", 
               "Ohter lands",
               
               "Northeast",
               "Midwest",
               "West",
               "South",
               "Texas",
               "Mtwest")

raster_list <- list()
for (i in 1:length(cov.names)) {
  if(i < 12){
    raster_list[[cov.names[[i]]]] <- raster(cov.filePaths[[i]]) %>% 
      raster::scale()
  }else{
    raster_list[[cov.names[[i]]]] <- raster(cov.filePaths[[i]]) 
  }
}

# Create a raster stack from the list of coefficient rasters
coeff_stack <- stack(raster_list)
raster::writeRaster(coeff_stack, "./data/covStack_old.tif", 
                    overwrite=TRUE)

### predictors new
cov.filePaths_new <- c(file.path(processed_layer_path, "transmission_new.tif"),
                       file.path(processed_layer_path, "land_acquisition.tif"),
                       file.path(processed_layer_path, "roads_qgis.tif"), 

                       file.path(processed_layer_path, "slope.tif"),
                       file.path(processed_layer_path, "pop_density.tif"),
                       
                       file.path(processed_layer_path, "hail_lag.tif"),
                       file.path(processed_layer_path, "fire.tif"),
                       file.path(processed_layer_path, "community.tif"),

                       file.path(processed_layer_path, "lowincome.tif"),
                       file.path(processed_layer_path, "minor.tif"),
                       file.path(processed_layer_path, "employ.tif"),
                       
                       file.path(processed_layer_path, "lulc_forest_new.tif"),
                       file.path(processed_layer_path, "lulc_grassland_new.tif"),
                       file.path(processed_layer_path, "lulc_shrubland_new.tif"),
                       file.path(processed_layer_path, "lulc_riparian_new.tif"),
                       file.path(processed_layer_path, "lulc_sparselyvegetated_new.tif"),
                       file.path(processed_layer_path, "lulc_agricultural_new.tif"),
                       file.path(processed_layer_path, "lulc_developed_new.tif"),
                       file.path(processed_layer_path, "lulc_other_new.tif"),
                       
                       file.path(processed_layer_path, "region_ne.tif"),
                       file.path(processed_layer_path, "region_mw.tif"),
                       file.path(processed_layer_path, "region_w.tif"),
                       file.path(processed_layer_path, "region_s.tif"),
                       file.path(processed_layer_path, "region_tex.tif"),
                       file.path(processed_layer_path, "region_mtw.tif")
)


raster_list <- list()
for (i in 1:length(cov.names)) {
  if(i < 12){
    raster_list[[cov.names[[i]]]] <- raster(cov.filePaths_new[[i]]) %>% 
      raster::scale()
  }else{ 
    raster_list[[cov.names[[i]]]] <- raster(cov.filePaths_new[[i]]) 
  }
}


coeff_stack_new <- stack(raster_list)
raster::writeRaster(coeff_stack_new, "./data/covStack_new.tif", 
                    overwrite=TRUE)
