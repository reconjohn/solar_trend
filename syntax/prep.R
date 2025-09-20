load("../data/derived/region.RData") # rgn 
load("./data/location_solar.RData") # solar_locations,solar_inter,solar_sub,
load("./data/absence_trend.RData")
load("./data/model_data.RData")
# solar.cov.bg.inter, solar.cov.existing.inter, 
# solar.cov.bg.sub, solar.cov.existing.sub, 
# solar.cov.bg, solar.cov.existing, 
# rst_sub, rst, rst_inter,

rgn <- rgn %>% 
  mutate(region = recode(region, 
                         "mtwest" = "Mtwest",
                         "midwest" = "Midwest",
                         "northeast" = "Northeast",
                         "south" = "South",
                         "texas" = "Texas",
                         "west" = "West")) %>% 
  mutate(region = factor(region, levels = c("West","Mtwest","Midwest","Texas","South","Northeast")))

rgn <- rgn[c(6,2,1,5,4,3),]


# queue projects
solar_que <- read_csv("../trend1/data/final_output_word_match.csv") %>% # read in the file
  filter(str_detect(lbnl_type,"Solar")&
           status == "active"&
           capacity_mw > 1)

solar_queue <- solar_que %>% # filter for utility scale (>1MW)
  filter(match_confidence > 40) %>%
  st_as_sf(coords = c("matched_sub_lon", "matched_sub_lat"), crs = 4269) %>%  # project
  
  mutate(unique_id = row_number()) %>%
  mutate(status = ifelse(str_detect(ia_status_clean,"IA|Construction"), "Late","Early"),
         status = ifelse(is.na(status), "Early", status)) %>% 
  
  st_transform(crs = 5070) %>% # transform crs to ours
  dplyr::select(unique_id,state,capacity_mw, status) 

# queue locations
s <- solar_locations %>% 
  st_buffer(7000) %>% # for visualization in figures
  dplyr::rename(state = state_code,
                capacity = p_cap_ac) %>% 
  dplyr::select(state, capacity) %>% 
  mutate(class = "Operational")

s_sub <- solar_sub %>% 
  st_buffer(7000) %>% 
  mutate(capacity = NA) %>% 
  rename(state = STATE) %>% 
  dplyr::select(state, capacity) %>% 
  mutate(class = "Substation")


s_dat_compare <- solar.cov.bg.inter %>% 
  dplyr::select(-group) %>% 
  rbind(solar.cov.existing.inter) %>% 
  mutate(class = "Queue") %>% 
  
  rbind(
    solar.cov.bg.sub %>% 
      dplyr::select(-group) %>% 
      rbind(solar.cov.existing.sub) %>% 
      mutate(class = "Substation"),
    solar.cov.bg %>% 
      dplyr::select(-group) %>% 
      rbind(solar.cov.existing) %>% 
      mutate(class = "Operational")
  ) 

s_dat_compare <- s_dat_compare %>% 
  mutate(region = names(s_dat_compare[21:26])[max.col(s_dat_compare[21:26])]) %>% 
  dplyr::select(-region_ne:-region_mtw) %>% 

  # scale
  mutate(across(
    .cols = setdiff(names(.), c("zone", "treat", "class","region",
                                names(.)[str_detect(names(.), "lulc")])),
    .fns = ~ scale(.) %>% as.numeric()
  )) %>%
  
  mutate(treat = factor(treat)) %>% 
  na.omit() 


# f2
# random slop model
s_dat_comp <- s_dat_compare %>%
  # mutate(region = names(w_dat_compare[21:26])[max.col(w_dat_compare[21:26])],
  #        land = names(w_dat_compare[13:20])[max.col(w_dat_compare[13:20])]) %>% # revert one-hot incode
  dplyr::select(-zone)

RE <- function(tech){

  da <- s_dat_comp %>%
    filter(class == tech)
  # %>%
  #   mutate(treat = as.numeric(treat))
  model1vars <- names(da)[c(1:22,25)] %>%
    setdiff(c("cf","hail","fire","community"))
  fvar <- as.formula(paste("treat ~", paste(model1vars, collapse = " + "), "+ (cf+hail+fire+community|region)"))

  fit <- glmer(fvar, family = binomial(link="logit"), data = da)


  # summary(fit)

  tp <- sqrt(attr(ranef(fit, condVar=T)[[1]], "postVar"))*1.96

  d <- as.data.frame(ranef(fit)$region) %>%
    tibble::rownames_to_column("region") %>%
    mutate(region = recode(region,
                           "region_mtw" = "Mtwest",
                           "region_mw" = "Midwest",
                           "region_ne" = "Northeast",
                           "region_s" = "South",
                           "region_tex" = "Texas",
                           "region_w" = "West")) %>%
    dplyr::select(-`(Intercept)`) %>%
    gather(variable, R_effect, -region) %>%
    mutate(SE = c(tp[2,2,],tp[3,3,],tp[4,4,],tp[5,5,]))

  return(d)
}


s_d <- RE("Operational") %>%
  mutate(class = "Operational") %>%
  rbind(
    RE("Queue") %>%
      mutate(class = "Queue")
  ) %>%
  mutate(variable = factor(variable, levels = c("cf", "hail", "fire", "community"))) %>%
  mutate(variable = recode(variable, "cf" = "Capacity factor",
                           "hail" = "Hail",
                           "fire" = "Wildfire",
                           "community" = "Energy community"),
         variable = factor(variable, levels = c("Capacity factor","Hail","Wildfire","Energy community"))) %>%
  mutate(class = factor(class, levels = c("Operational","Queue")),
         region = factor(region, levels = rev(c("West","Mtwest","Midwest","Texas","South","Northeast"))))
write.csv(s_d, "./data_compare.csv", row.names = FALSE)
# s_d <- read_csv("./data_compare.csv")

# f3
# ## prediction
solar_absence_roi <- raster("./data/solar_absence_roi.tif")
mask_solar <- function(rast_file, solar_absence_roi){
  rast_file %>%
    terra::mask(mask = solar_absence_roi)
}

# ### solar prediction
# s_dat_t <- solar.cov.bg %>%
#   filter(group == 5) %>%
#   dplyr::select(-group) %>%
#   rbind(solar.cov.existing) %>%
#   mutate(treat = factor(treat)) %>%
#   na.omit()
# 
# glm1_solar <- glm(treat ~ tx + landAcq + roads + slope + pop + hail + fire
#                   + community + lowincome + minority + unemploy +
#                     lulc_forest + lulc_grassland + lulc_shrubland + lulc_riparian + lulc_sparse + lulc_agriculture + lulc_developed + lulc_other +
#                     region_mw + region_ne +  region_s + region_tex + region_w + region_mtw +
#                     env + cf + lag,
#                   data=s_dat_t, family = binomial(link="logit"))
# 
# 
# solar.pg.project <- predict(object = solar_IV, model = glm1_solar, type = "response", ext=extent(solar_IV))
# # plot(solar.pg.project)
# # plot(aoi_vect, add=TRUE)
# writeRaster(solar.pg.project, "./data/pred_logReg_s_project.tif", overwrite=TRUE)
# plot(rast("./data/pred_logReg_s_project.tif"))
# 
# ### queue prediction
# s_dat_inter <- solar.cov.bg.inter %>%
#   filter(group == 5) %>%
#   dplyr::select(-group) %>%
#   rbind(solar.cov.existing.inter) %>%
#   mutate(treat = factor(treat)) %>%
#   na.omit()
# 
# 
# glm1_solar <- glm(treat ~ tx + landAcq + roads + slope + pop + hail + fire + community + lowincome + minority + unemploy +
#                     lulc_forest + lulc_grassland + lulc_shrubland + lulc_riparian + lulc_sparse + lulc_agriculture + lulc_developed + lulc_other +
#                     region_mw + region_ne +  region_s + region_tex + region_w + region_mtw +
#                     env + cf + lag,
#                   data=s_dat_inter, family = binomial(link="logit"))
# # summary(glm1_solar)
# 
# solar.pg.inter <- predict(object = solar_IV_new, model = glm1_solar, type = "response", ext=extent(solar_IV_new))
# # plot(solar.pg.inter)
# # plot(aoi_vect, add=TRUE)
# writeRaster(solar.pg.inter, "./data/pred_logReg_s_queue.tif", overwrite=TRUE)
# plot(rast("./data/pred_logReg_s_queue.tif"))
# 
# ### project substation prediction
# s_dat_sub <- solar.cov.bg.sub %>%
#   filter(group == 5) %>%
#   dplyr::select(-group) %>%
#   rbind(solar.cov.existing.sub) %>%
#   mutate(treat = factor(treat)) %>%
#   na.omit()
# 
# 
# glm1_solar <- glm(treat ~ tx + landAcq + roads + slope + pop + hail + fire + community + lowincome + minority + unemploy +
#                     lulc_forest + lulc_grassland + lulc_shrubland + lulc_riparian + lulc_sparse + lulc_agriculture + lulc_developed + lulc_other +
#                     region_mw + region_ne +  region_s + region_tex + region_w + region_mtw +
#                     env + cf + lag,
#                   data=s_dat_sub, family = binomial(link="logit"))
# # summary(glm1_solar)
# 
# solar.pg.sub <- predict(object = solar_IV, model = glm1_solar, type = "response", ext=extent(solar_IV))
# writeRaster(solar.pg.sub, "./data/pred_logReg_s_sub.tif", overwrite=TRUE)
# plot(rast("./data/pred_logReg_s_sub.tif"))
# 
# pred_logReg_s <- rast("./data/pred_logReg_s_project.tif")
# pred_logReg_s_inter <- rast("./data/pred_logReg_s_queue.tif")
# pred_logReg_s_sub <- rast("./data/pred_logReg_s_sub.tif")
# 
# results <- list(pred_logReg_s, pred_logReg_s_sub, pred_logReg_s_inter)
# 
# new_results <- list()
# for(i in 1:3){
#   new_results[i] <- mask_solar(results[[i]], rast(solar_absence_roi))
# }
# 
# writeRaster(rast(new_results), "./data/trend_surface.tif", overwrite=TRUE)
new_results <- rast("./data/trend_surface.tif")



# f4
# df_results <- new_results[[3]] - new_results[[1]] # queue - project
# # names(model_data)
# model_data <- rast(list(df_results, rast(solar_IV_new)))
# # model_data <- stack(list(raster(df_results), solar_IV))
# 
# sample_size <- 100000
# valid_cells_mask <- rast(solar_absence_roi) == 1
# valid_cell_numbers <- cells(valid_cells_mask) # get index for true cells
# sampled_cell_numbers <- sample(valid_cell_numbers, size = sample_size) # sample the cells
# sample_coords <- xyFromCell(model_data, sampled_cell_numbers) # get the xy coordinates based on the cell IDs
# sampled_data <- extract(model_data, sample_coords) # extract values on the cell IDs
# # sampled_data <- spatSample(model_data, size = sample_size, method = "random",
# #                            na.rm = TRUE, as.df = TRUE)
# write.csv(sampled_data, "./data/f4_project_sample.csv", row.names = FALSE)
sampled_data <- read_csv("./data/f4_project_sample.csv")


df_model <- sampled_data %>% 
  mutate(across(
    .cols = setdiff(names(.), c("pred_logReg_s_queue", names(.)[str_detect(names(.), "lulc|region")])),
    .fns = ~ scale(.) %>% as.numeric() # scale predictors before regression 
  ))

model_formula  <- as.formula(pred_logReg_s_queue ~ tx + landAcq + roads + slope + pop + hail + fire + community + lowincome + minority + unemploy + 
                               lulc_forest + lulc_grassland + lulc_shrubland + lulc_riparian + lulc_sparse + lulc_agriculture + lulc_developed + lulc_other +
                               region_mw + region_ne +  region_s + region_tex + region_w + region_mtw +
                               env + cf + lag)

f3_glm <- lm(model_formula, data = df_model)


# df_results_sub <- new_results[[3]] - new_results[[2]] # queue - sub
# # names(model_data)
# model_data_sub <- rast(list(df_results_sub, rast(solar_IV_new)))
# # model_data <- stack(list(raster(df_results), solar_IV))
# 
# sample_size <- 100000
# valid_cells_mask <- rast(solar_absence_roi) == 1
# valid_cell_numbers_sub <- cells(valid_cells_mask) # get index for true cells
# valid_cell_numbers_sub <- sample(valid_cell_numbers_sub, size = sample_size) # sample the cells
# sample_coords_sub <- xyFromCell(model_data_sub, valid_cell_numbers_sub) # get the xy coordinates based on the cell IDs
# sampled_data_sub <- extract(model_data_sub, sample_coords_sub) # extract values on the cell IDs
# write.csv(sampled_data_sub, "./data/f4_sub_sample.csv", row.names = FALSE)
sampled_data_sub <- read_csv("./data/f4_sub_sample.csv")

df_model <- sampled_data_sub %>% 
  mutate(across(
    .cols = setdiff(names(.), c("pred_logReg_s_queue", names(.)[str_detect(names(.), "lulc|region")])),
    .fns = ~ scale(.) %>% as.numeric() # scale predictors before regression 
  ))


model_formula  <- as.formula(pred_logReg_s_queue ~ tx + landAcq + roads + slope + pop + hail + fire + community + lowincome + minority + unemploy +  
                               lulc_forest + lulc_grassland + lulc_shrubland + lulc_riparian + lulc_sparse + lulc_agriculture + lulc_developed + lulc_other +
                               region_mw + region_ne +  region_s + region_tex + region_w + region_mtw +
                               env + cf + lag)

f3_glm_sub <- lm(model_formula, data = df_model)


# f5
### data cleaning
# queue_rast <- solar_queue %>% # queue project locations 
#   st_transform(crs = 5070) %>% # transform crs to ours
#   st_buffer(dist = 1600, # in km
#             endCapStyle = 'ROUND') %>% 
# 
#   terra::vect() %>% # vectorize to rasterize
#   terra::rasterize(y = base_ras,
#                    field = "unique_id",
#                    touches = TRUE) %>% # any polygon that touches will be converted
#   terra::mask(base_ras)
# s_q_rast <- raster(queue_rast)
# s_q_zonal <- as.data.frame(zonal(x = solar_IV_new, z = s_q_rast, fun ='mean', na.rm = TRUE)) 
# write.csv(s_q_zonal, "./data/s_q_zonal.csv", row.names = FALSE)
s_q_zonal <- read_csv("./data/s_q_zonal.csv")


df <- solar_queue %>% 
  left_join(s_q_zonal, by = c("unique_id" = "zone"))

complete_rows <- df %>%
  filter(!is.na(tx))  # or use a more comprehensive NA check if needed

df_filled <- st_join(df, complete_rows, join = st_equals, suffix = c("", "_filled"))

df_filled <- df_filled %>%
  mutate(across(
    tx:lag,  # replace with actual columns needing fill
    ~coalesce(.x, get(paste0(cur_column(), "_filled")))
  )) %>%
  select(-ends_with("_filled")) %>% 
  st_drop_geometry()


### phase specific data scaled based on 5km buffer and capacities 
cap_dat <- df_filled %>% 
  dplyr::select(-unique_id,-state) %>% 
  mutate(across(
    .cols = setdiff(names(.), c("capacity_mw", "status", names(.)[str_detect(names(.), "lulc|region")])),
    .fns = ~ scale(.) %>% as.numeric() # scale predictors before regression 
  ))


cap_dat_r <- cap_dat %>%
  mutate(region = names(cap_dat[22:27])[max.col(cap_dat[22:27])]) %>% # revert one-hot incode
  dplyr::select(-region_ne:-region_mtw)


### capacity modeling 
model_formula  <- as.formula(capacity_mw ~ tx + landAcq + roads + slope + pop + hail + fire + community + lowincome + minority + unemploy + 
                               lulc_forest + lulc_grassland + lulc_shrubland + lulc_riparian + lulc_sparse + lulc_agriculture + lulc_developed + lulc_other +
                               region +
                               env + cf + lag)

f4_glm3 <- lm(model_formula, data = cap_dat_r)


### capacity modeling by phase
model_formula  <- as.formula(capacity_mw ~  pop + lowincome + minority + unemploy +  
                               lulc_forest + lulc_grassland + lulc_shrubland + lulc_riparian + lulc_sparse + lulc_agriculture + lulc_developed + lulc_other +
                               region +
                               (tx + landAcq + roads + slope + hail + fire + community + 
                                  + env + cf + lag|status))

f4_glm4 <- lmer(model_formula, data = cap_dat_r)


### SI
# predictor_data <- rast(solar_IV_new)
# sample_coords <- solar_queue %>%
#   st_coordinates()
# sampled_cap <- extract(predictor_data, sample_coords)
# write.csv(sampled_cap, "./data/f5_capa_queue.csv", row.names = FALSE)
sampled_cap <- read_csv("./data/f5_capa_queue.csv")

df_cap <- sampled_cap %>% 
  mutate(across(
    .cols = setdiff(names(.), c(names(.)[str_detect(names(.), "lulc|region")])),
    .fns = ~ scale(.) %>% as.numeric() # scale predictors before regression 
  ))


df_cap$capacity <- solar_queue$capacity_mw
df_cap$status <- solar_queue$status

model_formula  <- as.formula(capacity ~ tx + landAcq + roads + slope + pop + hail + fire + community + lowincome + minority + unemploy + 
                               lulc_forest + lulc_grassland + lulc_shrubland + lulc_riparian + lulc_sparse + lulc_agriculture + lulc_developed + lulc_other +
                               region_mw + region_ne +  region_s + region_tex + region_w + region_mtw +
                               env + cf + lag)

f4_glm1 <- lm(model_formula, data = df_cap)

model_formula  <- as.formula(capacity ~  pop + lowincome + minority + unemploy +  
                               lulc_forest + lulc_grassland + lulc_shrubland + lulc_riparian + lulc_sparse + lulc_agriculture + lulc_developed + lulc_other +
                               region_mw + region_ne +  region_s + region_tex + region_w + region_mtw +
                               (tx + landAcq + roads + slope + hail + fire + community + 
                                  + env + cf + lag|status))

f4_glm2 <- lmer(model_formula, data = df_cap)


save(rgn, solar_que, solar_queue, s, s_sub, solar_inter, s_dat_compare, s_d,
     sampled_data, sampled_data_sub,sampled_cap,
     f3_glm,f3_glm_sub,f4_glm1,f4_glm2,f4_glm3,f4_glm4,
     file = "./data/trend_data.RData")
