# base_ras <- terra::rast("/home/energysiting/data/processed_data/masks/US_base_raster.tif")
# # existing vs. queue
# tp <- s_dat_compare %>% 
#   filter(class != "Substation") %>% 
#   dplyr::select(-zone)
# 
# 
# glm1 <- glm(treat ~ tx + landAcq + roads + slope + pop + hail + fire + community + lowincome + minority + unemploy +
#                     lulc_forest + lulc_grassland + lulc_shrubland + lulc_riparian + lulc_sparse + lulc_agriculture + lulc_developed + lulc_other +
#                     region +
#                     env + cf + lag + 
#               class*(tx + landAcq + roads + slope + pop + hail + fire + community + lowincome + minority + unemploy +
#               lulc_forest + lulc_grassland + lulc_shrubland + lulc_riparian + lulc_sparse + lulc_agriculture + lulc_developed + lulc_other +
#               region +
#               env + cf + lag), 
#                   data=tp, family = binomial(link="logit"))
# 
# glm1 %>% summary()
# # closer TX, steeper slope, more people, avoiding hail, closer energy community,
# # strict to env, more cf, cluster
# # queue: closer TX, less people, high hail,
# # less NE, West, not strict to env, less cf, less cluster



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
