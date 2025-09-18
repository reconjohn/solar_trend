base_ras <- terra::rast("/home/energysiting/data/processed_data/masks/US_base_raster.tif")
# existing vs. queue
tp <- s_dat_compare %>% 
  filter(class != "Substation") %>% 
  dplyr::select(-zone)


glm1 <- glm(treat ~ tx + landAcq + roads + slope + pop + hail + fire + community + lowincome + minority + unemploy +
                    lulc_forest + lulc_grassland + lulc_shrubland + lulc_riparian + lulc_sparse + lulc_agriculture + lulc_developed + lulc_other +
                    region +
                    env + cf + lag + 
              class*(tx + landAcq + roads + slope + pop + hail + fire + community + lowincome + minority + unemploy +
              lulc_forest + lulc_grassland + lulc_shrubland + lulc_riparian + lulc_sparse + lulc_agriculture + lulc_developed + lulc_other +
              region +
              env + cf + lag), 
                  data=tp, family = binomial(link="logit"))

glm1 %>% summary()
# closer TX, steeper slope, more people, avoiding hail, closer energy community,
# strict to env, more cf, cluster
# queue: closer TX, less people, high hail,
# less NE, West, not strict to env, less cf, less cluster


################################################################################
# late vs. early 
### data cleaning
queue_rast <- solar_queue %>% # queue project locations 
  st_transform(crs = 5070) %>% # transform crs to ours
  st_buffer(dist = 1600, # in km
            endCapStyle = 'ROUND') %>% 

  terra::vect() %>% # vectorize to rasterize
  terra::rasterize(y = base_ras,
                   field = "unique_id",
                   touches = TRUE) %>% # any polygon that touches will be converted
  terra::mask(base_ras)
s_q_rast <- raster(queue_rast)
s_q_zonal <- as.data.frame(zonal(x = solar_IV_new, z = s_q_rast, fun ='mean', na.rm = TRUE)) 
write.csv(s_q_zonal, "./trend/data/s_q_zonal.csv", row.names = FALSE)
s_q_zonal <- read_csv("./trend/data/s_q_zonal.csv")


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
    .cols = setdiff(names(.), c("capacity_mw", "status")),
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

f5b <- rst_plot(f4_glm3) +
  scale_y_continuous(
    labels = scales::label_number(accuracy = 1)             
  ) 


### capacity modeling by phase
model_formula  <- as.formula(capacity_mw ~  pop + lowincome + minority + unemploy +  
                               lulc_forest + lulc_grassland + lulc_shrubland + lulc_riparian + lulc_sparse + lulc_agriculture + lulc_developed + lulc_other +
                               region +
                               (tx + landAcq + roads + slope + hail + fire + community + 
                                  + env + cf + lag|status))

f4_glm4 <- lmer(model_formula, data = cap_dat_r)

tp <- sqrt(attr(ranef(f4_glm4, condVar=T)[[1]], "postVar"))*1.96
f5c <- as.data.frame(ranef(f4_glm4)$status) %>% 
  tibble::rownames_to_column("status") %>% 
  dplyr::select(-`(Intercept)`) %>% 
  
  gather(variable, R_effect, -status) %>% 
  
  mutate(vari = rep(v_name[c(-1,-6,-10:-24)],each = 2),
         vari = fct_reorder(vari, R_effect)) %>% 
  
  mutate(SE = c(tp[2,2,],tp[3,3,],tp[4,4,],tp[5,5,],tp[6,6,],
                tp[7,7,],tp[8,8,],tp[9,9,],tp[10,10,],
                tp[11,11,])) %>% 
  
  # mutate(variable = recode(variable, "cf" = "Capacity factor",
  #                          "hail" = "Hail",
  #                          "fire" = "Fire",
  #                          "community" = "Energy community",
  #                          "lag" = "Spatial effects")) %>% 
  # mutate(variable = factor(variable, levels = c("Capacity factor", "Hail", "Fire", "Energy community", "Spatial effects"))) %>% 
  mutate(status = factor(status, levels = c("Late","Early"))) %>% 
  mutate(upper = R_effect+SE,
         lower = R_effect-SE) %>% 
  
  mutate(domain = case_when(str_detect(variable, "tx|roads|landAcq|cf|slope") ~ "Technical",
                            str_detect(variable, "env|hail|fire") ~ "Environmental risk",
                            str_detect(variable, "rps|lag|community") ~ "Spatial/policy"),
         domain = factor(domain, levels = c("Technical","Environmental risk","Spatial/policy"))) %>% 
  
  
  ggplot(aes(y = R_effect, x = vari, 
             ymin=lower, ymax=upper, color = status)) +
  geom_hline(yintercept = 0,linetype = "dashed", size = 0.5, color = "gray30") +
  
  geom_errorbar(width = 0.3, size = 0.8,
                position = position_dodge(width = 0.5)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  
  # geom_pointrangeh(position = position_dodge2v(height = 0.4), fatten = 2, size = 0.7) +
  
  facet_wrap(~domain, scales = "free") +
  theme_bw() +
  
  
  labs(y = "Capacity (MW)", x ="", color = "Phase",
       title = "") +
  
  scale_color_manual(values=c("brown", "darkblue")) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background =element_rect(fill="gray22",color="gray22"),
        strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=12, face = "bold"),
        legend.position = "bottom",
        legend.text = element_text(size=12),
        axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=12,
                                   angle = 45, hjust = 1),
        axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=12),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=12),
        plot.title=element_text(family="Franklin Gothic Demi", size=20)) 


f5 <- ggarrange(
  ggarrange(f4a, f5b, widths = c(1,1.2), nrow = 1),
  f5c,
  nrow = 2,
  heights = c(1,0.9),
  labels = c("A", "B"),  # Adds labels to plots
  label.x = 0,        # Adjust horizontal position of labels
  label.y = 1,        # Adjust vertical position of labels
  # vjust = 1,
  # hjust = -1,
  font.label = list(size = 14, face = "bold")
  
)

ggsave("./trend/fig/f5.png", f5, width = 12, height = 12)


# ### random slope 
# RE <- function(){
#   
#   da <- cap_dat_r 
#   # %>%
#   #   mutate(treat = as.numeric(treat))
#   model1vars <- names(da)[c(3:24)] %>%
#     setdiff(c("cf","hail","fire","community"))
#   fvar <- as.formula(paste("capacity_mw ~", paste(model1vars, collapse = " + "), "+ (cf+hail+fire+community|region) + 
#                            (cf+hail+fire+community|status)"))
#   
#   fit <- lmer(fvar, data = da)
#   
#   
#   # summary(fit)
#   
#   tp <- sqrt(attr(ranef(fit, condVar=T)[[1]], "postVar"))*1.96
#   
#   d <- as.data.frame(ranef(fit)$region) %>%
#     tibble::rownames_to_column("region") %>%
#     mutate(region = recode(region,
#                            "region_mtw" = "Mtwest",
#                            "region_mw" = "Midwest",
#                            "region_ne" = "Northeast",
#                            "region_s" = "South",
#                            "region_tex" = "Texas",
#                            "region_w" = "West")) %>%
#     dplyr::select(-`(Intercept)`) %>%
#     gather(variable, R_effect, -region) %>%
#     mutate(SE = c(tp[2,2,],tp[3,3,],tp[4,4,],tp[5,5,]))
#   
#   return(d)
# }
# 
# 
# s_cap <- RE("Late") %>%
#   mutate(class = "Late") %>%
#   rbind(
#     RE("Early") %>%
#       mutate(class = "Early")
#   ) %>%
#   mutate(variable = factor(variable, levels = c("cf", "hail", "fire", "community"))) %>%
#   mutate(variable = recode(variable, "cf" = "Capacity factor",
#                            "hail" = "Hail",
#                            "fire" = "Wildfire",
#                            "community" = "Energy community"),
#          variable = factor(variable, levels = c("Capacity factor","Hail","Wildfire","Energy community"))) %>%
#   mutate(class = factor(class, levels = c("Late","Early")),
#          region = factor(region, levels = rev(c("West","Mtwest","Midwest","Texas","South","Northeast"))))
# 
# 
# s_cap %>% 
#   ggplot(aes(x = R_effect, y = region, color = class, 
#              xmin=R_effect-SE, xmax=R_effect+SE)) +
#   geom_vline(xintercept = 0,linetype = "dashed", size = 0.5, color = "gray30") +
#   
#   geom_pointrangeh(position = position_dodge2v(height = 0.4), fatten = 2, size = 0.7) +
#   
#   facet_wrap(~variable, scales = "free_x", nrow = 4) +
#   theme_bw() +
#   # 
#   # scale_x_continuous(
#   #   trans = scales::pseudo_log_trans(base = 10, sigma = 0.2)  # Pseudo-log transformation
#   #   
#   # )  +
#   
#   labs(x = "", y ="", color = "",
#        title = "") +
#   
#   scale_fill_manual(values=c("red", "gray")) +
#   theme(panel.grid.minor = element_blank(),
#         panel.grid.major.x = element_blank(),
#         strip.background =element_rect(fill="gray22",color="gray22"),
#         strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=12, face = "bold"),
#         legend.position = "right",
#         axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=10),
#         axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=12),
#         axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=12),
#         plot.title=element_text(family="Franklin Gothic Demi", size=20)) 
