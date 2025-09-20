base_ras <- terra::rast("/home/energysiting/data/processed_data/masks/US_base_raster.tif")

# read in new data from the USGS database
solar_location_path <- './siting/uspvdb_v2_0_20240801.geojson'
sub_location_path <- "./trend/data/solar_sub.csv" # substations nearest solar project


solar_mask <- st_read(solar_location_path) %>% # read in the file
  filter(!p_state %in% c("HI","AK")) %>% 
  mutate(unique_id = row_number()) %>% 
  st_transform(crs = 5070) %>% # transform crs to ours
  st_buffer(dist = 500) # apply buffer calculated above
# plot(solar_mask$geometry)

# solar
solar_locations <- st_read(solar_location_path) %>% # read in the file
  st_transform(crs = 5070) %>% # reproject crs into projection of base raster
  filter(p_year >= 2017) %>% #projects after 2017
  filter(p_cap_ac >= 1) %>% 
  filter(!p_state %in% c("HI","AK")) %>% 
  mutate(unique_id = row_number()) %>% 
  rename(state_code = p_state) %>% 
  
  mutate(area_m2 = st_area(geometry),
         radius_m = sqrt(area_m2/pi) %>% 
           as.numeric()) 

# queue substations 
solar_inter <- read_csv("./trend/data/solar_inter.csv") %>% # read in the file
  mutate(unique_id = row_number()) %>%
  st_as_sf(coords = c("X", "Y"), crs = 4269) %>%
  st_transform(crs = 5070) %>% # transform crs to ours
  st_buffer(dist = 1600, # in km
            endCapStyle = 'ROUND') %>%
  mutate(area_m2 = st_area(.),
         radius_m = sqrt(area_m2/pi) %>% as.numeric()) # make radius column from area

# neareast substation for solar plant
solar_sub <- read_csv(sub_location_path) %>%  # read in the file
  mutate(unique_id = row_number()) %>% 
  st_as_sf(coords = c("X", "Y"), crs = 4269) %>% 
  st_transform(crs = 5070) %>% # transform crs to ours
  st_buffer(dist = 1600, 
            endCapStyle = 'ROUND') %>% 
  mutate(area_m2 = st_area(.),
         radius_m = sqrt(area_m2/pi) %>% as.numeric()) # make radius column from area


# rasterize mask
solar_mask_rast <- solar_mask %>% # vectorize to rasterize
  terra::vect() %>%
  terra::rasterize(y = base_ras,
                   field = "unique_id",
                   touches = TRUE) %>% 
  terra::mask(base_ras)
# plot(solar_mask_rast)

# rasterize locations
solar_location_rast <- solar_locations %>% 
  terra::vect() %>% # vectorize to rasterize
  terra::rasterize(y = base_ras,
                   field = "unique_id",
                   touches = TRUE) %>% # any polygon that touches will be converted
  terra::mask(base_ras)


# rasterize locations based on queue substation locations 
solar_inter_rast <- solar_inter %>% 
  terra::vect() %>% # vectorize to rasterize
  terra::rasterize(y = base_ras,
                   field = "unique_id",
                   touches = TRUE) %>% # any polygon that touches will be converted
  terra::mask(base_ras)


solar_sub_rast <- solar_sub %>% 
  terra::vect() %>% # vectorize to rasterize
  terra::rasterize(y = base_ras,
                   field = "unique_id",
                   touches = TRUE) %>% # any polygon that touches will be converted
  terra::mask(base_ras)


# save
writeRaster(x = solar_mask_rast,
            filename = "./trend/data/solar_hull_existing_mask.tif",
            overwrite = TRUE)

writeRaster(x = solar_location_rast,
            filename = "./trend/data/solar_hull_existing.tif",
            overwrite = TRUE)


write_csv(solar_locations, path = "./trend/data/solar_hull_existing.csv",
          append = FALSE)

# # append = FALSE allows this command to overwrite previous results
# st_write(solar_locations %>% 
#            mutate(point = st_centroid(geometry)),
#          "./trend/data/solar_hull_existing_pts.shp",
#          append = FALSE)

### inter
writeRaster(x = solar_inter_rast,
            filename = "./trend/data/solar_inter_existing.tif",
            overwrite = TRUE)

# plot(rast("./trend/data/solar_inter_existing.tif"))

write_csv(solar_inter, path = "./trend/data/solar_inter_existing.csv",
          append = FALSE)
# 
# # append = FALSE allows this command to overwrite previous results
# st_write(solar_inter %>% 
#            mutate(point = st_centroid(geometry)),
#          "./trend/data/solar_inter_existing_pts.shp",
#          append = FALSE)


### sub
writeRaster(x = solar_sub_rast,
            filename = "./trend/data/solar_sub_existing.tif",
            overwrite = TRUE)

write_csv(solar_sub, path = "./trend/data/solar_sub_existing.csv",
          append = FALSE)
# 
# # append = FALSE allows this command to overwrite previous results
# st_write(solar_sub %>% 
#            mutate(point = st_centroid(geometry)),
#          "./trend/data/solar_sub_existing_pts.shp",
#          append = FALSE)


save(solar_locations,solar_inter,solar_sub,
     file = "./trend/data/location_solar.RData")


