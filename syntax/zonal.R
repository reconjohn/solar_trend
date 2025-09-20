base_ras <- terra::rast("/home/energysiting/data/processed_data/masks/US_base_raster.tif")
load("./data/absence_trend.RData") # w_inter_ab_sf,w_sub_ab_sf,w_ab_sf, solar_absence_roi,

solar_stack <- stack("./data/solar_covStack.tif")
coeff_stack <- stack("./data/covStack_old.tif")
coeff_stack_new <- stack("./data/covStack_new.tif")

solar_location_rast <- rast("./data/solar_hull_existing.tif")
solar_inter_rast <- rast("./data/solar_inter_existing.tif")
solar_sub_rast <- rast("./data/solar_sub_existing.tif")

## stack
names(coeff_stack) <- c("tx", "landAcq", "roads", "slope", "pop",
                        "hail", "fire", "community", "lowincome", "minority", "unemploy", "lulc_forest", "lulc_grassland", 
                        "lulc_shrubland", "lulc_riparian", "lulc_sparse", 
                        "lulc_agriculture", "lulc_developed", "lulc_other",
                        "region_ne", "region_mw", "region_w", "region_s", "region_tex", 
                        "region_mtw")
names(coeff_stack_new) <- c("tx", "landAcq", "roads", "slope", "pop",
                        "hail", "fire", "community", "lowincome", "minority", "unemploy", "lulc_forest", "lulc_grassland", 
                        "lulc_shrubland", "lulc_riparian", "lulc_sparse", 
                        "lulc_agriculture", "lulc_developed", "lulc_other",
                        "region_ne", "region_mw", "region_w", "region_s", "region_tex", 
                        "region_mtw")

names(solar_stack) <- c("env", "cf", "lag")

solar_IV <- stack(list(coeff_stack,solar_stack))
solar_IV_new <- stack(list(coeff_stack_new,solar_stack))
# plot(solar_IV)


## DV
s_ex <- raster(solar_location_rast)
s_ab <- fasterize::fasterize(sf = s_ab_sf, 
                             raster = raster(base_ras), 
                             field = "id") # Rasterize points

### solar interconnection
s_ex_inter <- raster(solar_inter_rast)
s_ab_inter <- fasterize::fasterize(sf = s_inter_ab_sf, 
                                 raster = raster(base_ras), 
                                 field = "id") # Rasterize points


s_ex_sub <- raster(solar_sub_rast)
s_ab_sub <- fasterize::fasterize(sf = s_sub_ab_sf, 
                                   raster = raster(base_ras), 
                                   field = "id") # Rasterize points

## zonal statistics 
### existing projects 
solar.cov.bg <- as.data.frame(zonal(x = solar_IV, z = s_ab, fun ='mean', na.rm = TRUE)) %>%
  mutate(treat = 0)
solar.cov.existing <- as.data.frame(zonal(x = solar_IV, z = s_ex, fun ='mean', na.rm = TRUE)) %>%
  mutate(treat = 1)
write.csv(solar.cov.bg, "./data/bg_cov_solar.csv", row.names = FALSE)
write.csv(solar.cov.existing, "./data/existing_cov_solar.csv", row.names = FALSE)


### solar existing substation
solar.cov.bg.sub <- as.data.frame(zonal(x = solar_IV, z = s_ab_sub, fun ='mean', na.rm = TRUE)) %>%
  mutate(treat = 0)
solar.cov.existing.sub <- as.data.frame(zonal(x = solar_IV, z = s_ex_sub, fun ='mean', na.rm = TRUE)) %>%
  mutate(treat = 1)
write.csv(solar.cov.bg.sub, "./data/bg_cov_solar_sub.csv", row.names = FALSE)
write.csv(solar.cov.existing.sub, "./data/existing_cov_solar_sub.csv", row.names = FALSE)


### solar interconnect
solar.cov.bg.inter <- as.data.frame(zonal(x = solar_IV_new, z = s_ab_inter, fun ='mean', na.rm = TRUE)) %>%
  mutate(treat = 0)
solar.cov.existing.inter <- as.data.frame(zonal(x = solar_IV_new, z = s_ex_inter, fun ='mean', na.rm = TRUE)) %>%
  mutate(treat = 1)
write.csv(solar.cov.bg.inter, "./data/bg_cov_solar_inter.csv", row.names = FALSE)
write.csv(solar.cov.existing.inter, "./data/existing_cov_solar_inter.csv", row.names = FALSE)

