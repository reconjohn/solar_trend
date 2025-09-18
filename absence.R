state_boundaries <- st_read(file.path(raw_data_path, 'us_state_outlines/tl_2018_us_state.shp')) %>% #read state boundaries
  st_transform(crs = 5070)
projection <- (raster::crs("+init=epsg:5070"))
# plot(state_boundaries$geometry)

# read in the data
## solar
### suitable areas
solar_mask_rast <- terra::rast("./trend/data/solar_hull_existing_mask.tif")
# plot(solar_mask_rast)
solar_suit <- terra::rast("/home/energysiting/data/processed_data/masks/solar_suitability_SL1.tif")
# plot(solar_suit)

solar_location_csv <- read_csv(file = "./trend/data/solar_hull_existing.csv")

# masking
solar_absence_roi <- solar_suit %>% 
  terra::mask(mask = solar_mask_rast, inverse = TRUE) %>% # we use this to remove the current solar locations from possible absence point locations
  terra::project(projection) %>% 
  raster()
# plot(solar_absence_roi)

# save, suitable area after removing suitability criteria and existing solar project buffer areas
writeRaster(x = solar_absence_roi,
            filename = "./trend/data/solar_absence_roi.tif",
            overwrite = TRUE)

solar_absence_roi <- raster("./trend/data/solar_absence_roi.tif")

# Define the function
randomPts <- function(raster_mask, folder, seeds, tech, location_csv){
  
  ### absent points
  Pts <- dim(location_csv)[1] # bgPt.multiplier
  
  state_proportions <- location_csv %>% # Record proportions of total plants for each state
    filter(!state_code == 'HI') %>% 
    group_by(state_code) %>% 
    summarise(n = n()) %>% 
    mutate(freq = n / sum(n)) %>%
    select(-n) 
  
  state_proportions$count <- round(state_proportions$freq*Pts,0) # Calculate counts per state based on total points needed
  
  state_codes <- c('AL', 'AZ', 'AR', 'CA', 'CO', 'CT', 'DE', 'FL', 'GA', 'ID', 'IL', 'IN',
                   'IA', 'KS', 'KY', 'LA', 'ME', 'MD', 'MA', 'MI', 'MN', 'MS', 'MO', 'MT',
                   'NE', 'NV', 'NH', 'NJ', 'NM', 'NY', 'NC', 'ND', 'OH', 'OK', 'OR', 'PA',
                   'RI', 'SC', 'SD', 'TN', 'TX', 'UT', 'VT', 'VA', 'WA', 'WV', 'WI', 'WY')
  
  # Add two points for each state that wasn't accounted for initially
  for (state in setdiff(state_codes, state_proportions$state_code)){
    state_proportions <- state_proportions %>% add_row(state_code = state, freq = 0, count = 10)
  }
  
  # Create radii for each point, and set standard radius for states that previously had no projects
  radii <- location_csv$radius_m
  # mean(radii)

  radii <- append(radii, rep(mean(radii), (sum(state_proportions$count) - length(radii)))) 

  absence_locations <- data.frame() # prepare a dataframe for sampled points
  for (seed in 1:length(seeds)){ # Iterate through each seed
    
    set.seed(seeds[seed]) # Set seed for current iteration
    for (state in state_proportions$state_code){ # Iterate through each state we want to sample from
      current_boundary <- state_boundaries[state_boundaries$STUSPS == state, ] # Set current state boundary
      raster_data <- terra::crop(raster_mask, current_boundary, mask= T) # clip the current_boundary from raster_master
      raster_data <- mask(raster_data, current_boundary) # remove outsiders from current_boundary
      cell_indices <- which(raster_data[] == 1)
      coordinates <- raster::xyFromCell(raster_data, 
                                        sample(cell_indices, 
                                               state_proportions[state_proportions$state_code == state, ]$count)) %>% 
        as.data.frame()
      
      absence_locations <- coordinates %>% 
        mutate(iteration = seed) %>% 
        rbind(absence_locations)
    }
  }
  
  absence_buff <- st_as_sf(absence_locations, coords = c('x', 'y'), crs = projection) %>%  # Create geometry for sampled lat/longs
    mutate(id = seq(1,nrow(absence_locations),1)) %>%
    st_buffer(sample(radii, nrow(absence_locations), replace = T)) # Buffer points
  
  return(absence_buff)
}

s_ab_sf <- randomPts(raster_mask = solar_absence_roi,
                   seeds = c(1:10), tech = "solar", location_csv = solar_location_csv) # Can add more seeds to list to get more sampling iterations

### solar interconnection analysis
solar_inter_csv <- read_csv(file = "./trend/data/solar_inter_existing.csv") %>% 
  dplyr::rename(state_code = STATE)

s_inter_ab_sf <- randomPts(raster_mask = solar_absence_roi,
                         seeds = c(1:10), tech = "solar", location_csv = solar_inter_csv)


solar_sub_csv <- read_csv(file = "./trend/data/solar_sub_existing.csv") %>% 
  dplyr::rename(state_code = STATE)

s_sub_ab_sf <- randomPts(raster_mask = solar_absence_roi,
                           seeds = c(1:10), tech = "solar", location_csv = solar_sub_csv)


save(s_inter_ab_sf,s_sub_ab_sf,s_ab_sf, solar_absence_roi,
     file = "./trend/data/absence_trend.RData")
load("./trend/data/absence_trend.RData")
