source("./syntax/function.R")
processed_layer_path <- "/home/energysiting/data/processed_data/variables"



s1a <- solar_que %>% # filter for utility scale (>1MW)
  mutate(region = case_when(state %in% northeast ~ "Northeast",
                            state %in% midwest ~ "Midwest",
                            state %in% west ~ "West",
                            state %in% south ~ "South",
                            state %in% mtwest ~ "Mtwest",
                            T ~ "Texas")) %>% 
  mutate(type = ifelse(lbnl_type == "Solar+Battery", "Solar+Battery",
                       ifelse(lbnl_type == "Solar", "Solar", "Others")),
         type = factor(type, levels = c("Solar","Solar+Battery","Others")),
         region = factor(region, levels = c("West","Mtwest","Midwest","Texas","South","Northeast"))) %>% 
  mutate(Match = ifelse(match_confidence > 40, "O", "X")) %>% 
  group_by(type, region) %>% 
  summarise(Count = n()) %>% 
  
  ggplot() +
  geom_col(aes(x = type, y = Count, fill = type), position = "dodge") +
  facet_wrap(~region, nrow = 1) +
  labs(x = "", y = "Count", fill = "") +
  theme_classic() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background =element_rect(fill="gray22",color="gray22"),
        strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=12, face = "bold"),
        legend.position = "bottom",
        legend.key.spacing.x = unit(1, "lines"),
        # axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text = element_text(color = "black",family="Franklin Gothic Book",size=12),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=12),
        plot.title=element_text(family="Franklin Gothic Demi", size=20))


s1b <- solar_que %>% # filter for utility scale (>1MW)
  mutate(region = case_when(state %in% northeast ~ "Northeast",
                            state %in% midwest ~ "Midwest",
                            state %in% west ~ "West",
                            state %in% south ~ "South",
                            state %in% mtwest ~ "Mtwest",
                            T ~ "Texas")) %>% 
  mutate(type = ifelse(lbnl_type == "Solar+Battery", "Solar+Battery",
                       ifelse(lbnl_type == "Solar", "Solar", "Others")),
         type = factor(type, levels = c("Solar","Solar+Battery","Others")),
         region = factor(region, levels = c("West","Mtwest","Midwest","Texas","South","Northeast"))) %>% 
  mutate(Match = ifelse(match_confidence > 40, "O", "X")) %>% 
  group_by(type, region) %>% 
  summarise(Capacity = mean(capacity_mw, na.rm = T)) %>% 
  ggplot() +
  geom_col(aes(x = type, y = Capacity, fill = type), position = "dodge") +
  facet_wrap(~region, nrow = 1) +
  labs(x = "", y = "Capacity (MW/project)", fill = "") +
  theme_classic() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background =element_rect(fill="gray22",color="gray22"),
        strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=12, face = "bold"),
        legend.position = "bottom",
        # axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text = element_text(color = "black",family="Franklin Gothic Book",size=12),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=12),
        plot.title=element_text(family="Franklin Gothic Demi", size=20)) +
  guides(fill = guide_legend(byrow = T))


ggsave("./fig/s1.png", 
       ggarrange(s1a, s1b, nrow = 2,
                 common.legend = T, legend = "bottom"), width = 12, height = 8)


tp <- solar_que %>% # filter for utility scale (>1MW)
  mutate(region = case_when(state %in% northeast ~ "Northeast",
                            state %in% midwest ~ "Midwest",
                            state %in% west ~ "West",
                            state %in% south ~ "South",
                            state %in% mtwest ~ "Mtwest",
                            T ~ "Texas")) %>% 
  mutate(type = ifelse(lbnl_type == "Solar+Battery", "Solar+Battery",
                       ifelse(lbnl_type == "Solar", "Solar", "Others")),
         type = factor(type, levels = c("Solar","Solar+Battery","Others")),
         region = factor(region, levels = c("West","Mtwest","Midwest","Texas","South","Northeast"))) %>% 
  mutate(Match = ifelse(match_confidence > 40, "O", "X")) %>% 
  group_by(region, Match) %>% 
  summarise(Count = n(),
            `Capacity (MW)` = sum(capacity_mw)) %>% 
  ungroup

s2 <- rbind(
  tp %>% 
    dplyr::select(-`Capacity (MW)`) %>% 
    pivot_wider(names_from = Match, values_from = Count) %>% 
    mutate(X = coalesce(X, 0)) %>% 
    mutate(class = "Count") %>% 
    mutate(Matched = O/sum(O),
           Unmatched = X/sum(X),
           Difference = Matched - Unmatched),
  tp %>% 
    dplyr::select(-Count) %>% 
    pivot_wider(names_from = Match, values_from = `Capacity (MW)`) %>% 
    mutate(X = coalesce(X, 0)) %>% 
    mutate(class = "Capacity (MW)") %>% 
    mutate(Matched = O/sum(O),
           Unmatched = X/sum(X),
           Difference = Matched - Unmatched)
) %>% 
  gather(key, value, Matched:Difference) %>% 
  mutate(class = factor(class, levels = c("Count","Capacity (MW)")),
         key = factor(key, levels = c("Matched","Unmatched","Difference"))) %>% 
  
  mutate(region = factor(region, levels = c("West","Mtwest","Midwest","Texas","South","Northeast")),
         region_fill = factor(region, levels = rev(c("West","Mtwest","Midwest","Texas","South","Northeast")))) %>% 
  
  ggplot() +
  geom_col(aes(x = value, y = region_fill, fill = region), position = "dodge") +
  facet_grid(key~class) +
  labs(x = "Proportion", y = "", fill = "") +
  theme_classic() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background =element_rect(fill="gray22",color="gray22"),
        strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=12, face = "bold"),
        legend.position = "bottom",
        axis.text.x = element_text(),
        # axis.ticks.x = element_blank(),
        axis.text = element_text(color = "black",family="Franklin Gothic Book",size=12),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=12),
        plot.title=element_text(family="Franklin Gothic Demi", size=20)) +
  guides(fill = guide_legend(byrow=T, nrow = 1))


ggsave("./fig/s2.png", s2, width = 12, height = 6)


s3 <- solar_que %>% # filter for utility scale (>1MW)
  mutate(region = case_when(state %in% northeast ~ "Northeast",
                            state %in% midwest ~ "Midwest",
                            state %in% west ~ "West",
                            state %in% south ~ "South",
                            state %in% mtwest ~ "Mtwest",
                            T ~ "Texas")) %>% 
  mutate(type = ifelse(lbnl_type == "Solar+Battery", "Solar+Battery",
                       ifelse(lbnl_type == "Solar", "Solar", "Others")),
         type = factor(type, levels = c("Solar","Solar+Battery","Others")),
         region = factor(region, levels = c("West","Mtwest","Midwest","Texas","South","Northeast"))) %>% 
  mutate(Match = ifelse(match_confidence > 40, "O", "X")) %>% 
  group_by(type, region, Match) %>% 
  summarise(Capacity = mean(capacity_mw, na.rm = T)) %>% 
  
  ggplot() +
  geom_col(aes(x = type, y = Capacity, fill = Match), position = "dodge") +
  facet_wrap(~region, nrow = 1) +
  labs(x = "Type", y = "Average capacity (MW/project)", fill = "Matched") +
  theme_classic() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background =element_rect(fill="gray22",color="gray22"),
        strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=12, face = "bold"),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1),
        # axis.ticks.x = element_blank(),
        axis.text = element_text(color = "black",family="Franklin Gothic Book",size=12),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=12),
        plot.title=element_text(family="Franklin Gothic Demi", size=20))

ggsave("./fig/s3.png", s3, width = 12, height = 6)


### table
solar_que$lbnl_type %>% table()
solar_que %>% # filter for utility scale (>1MW)
  mutate(region = case_when(state %in% northeast ~ "Northeast",
                            state %in% midwest ~ "Midwest",
                            state %in% west ~ "West",
                            state %in% south ~ "South",
                            state %in% mtwest ~ "Mtwest",
                            T ~ "Texas")) %>% 
  mutate(type = ifelse(lbnl_type == "Solar+Battery", "Solar+Battery",
                       ifelse(lbnl_type == "Solar", "Solar", "Others")),
         type = factor(type, levels = c("Solar","Solar+Battery","Others")),
         region = factor(region, levels = c("West","Mtwest","Midwest","Texas","South","Northeast"))) %>% 
  mutate(Match = ifelse(match_confidence > 40, "O", "X")) %>% 
  group_by(type, Match) %>% 
  summarise(Count = n(),
            Capacity = mean(capacity_mw, na.rm = T),
            sd = sd(capacity_mw, na.rm = T)) 


png(filename = "./fig/lulc.png", width = 12, height = 12, units = "in", res = 300)

par(mfrow = c(2,1))

plot(rast(file.path(processed_layer_path, "lulc.tif")), main = "2016 Land Cover",
     axes = FALSE,           # Remove axes
     box = FALSE)
plot(rast(file.path(processed_layer_path, "lulc_new.tif")), main = "2023 Land Cover",
     axes = FALSE,           # Remove axes
     box = FALSE)

dev.off()


solar_stack <- stack("./data/solar_covStack.tif")
coeff_stack <- stack("./data/covStack_old.tif")
coeff_stack_new <- stack("./data/covStack_new.tif")


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


png(filename = "./fig/var_cov.png", width = 14, height = 8, units = "in", res = 300)

plot(stack(coeff_stack_new[[2:11]]), 
     axes = FALSE,           # Remove axes
     box = FALSE,            # Remove the outline borders
     main = cov.names[c(2:11)],  # Optional: Use layer names as titles
     cex.main = 1.5, 
     nc = 4                  # Adjust the number of columns in the plot layout
)
dev.off()


png(filename = "./fig/tx.png", width = 14, height = 6, units = "in", res = 300)

plot(stack(list(coeff_stack[[1]],coeff_stack_new[[1]])), 
     axes = FALSE,           # Remove axes
     box = FALSE,            # Remove the outline borders
     main = c("Transmission dist 2017","Transmission dist 2024"),  # Optional: Use layer names as titles
     cex.main = 1.5,
     nc = 4                  # Adjust the number of columns in the plot layout
)
dev.off()


rgn_plot <- rgn %>% 
  mutate(region = factor(region, levels = c("West","Mtwest","Midwest","Texas","South","Northeast"))) %>% 
  ggplot() +
  geom_sf(aes(fill = region)) +
  labs(fill = "") +
  coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), 
           ylim = c(-2300000,730000), expand = FALSE, datum = NA) +
  theme(legend.position = "right",
        panel.background = element_rect(fill = "white", color = NA),  # removes gray outside map
        plot.background = element_rect(fill = "white", color = NA),   # removes gray around entire plot
        panel.grid = element_blank())   


png(filename = "./fig/change_old.png", width = 14, height = 10, units = "in", res = 300)
plot(coeff_stack[[12:19]],
     axes = FALSE,           # Remove axes
     box = FALSE,            # Remove the outline borders
     legend = FALSE, 
     main = cov.names[c(12:19)],  # Optional: Use layer names as titles
     cex.main = 1.5,
     nc = 3                  # Adjust the number of columns in the plot layout
)
dev.off()


png(filename = "./fig/change_new.png", width = 14, height = 10, units = "in", res = 300)
plot(coeff_stack_new[[12:19]],
     axes = FALSE,           # Remove axes
     box = FALSE,            # Remove the outline borders
     legend = FALSE, 
     main = cov.names[c(12:19)],  # Optional: Use layer names as titles
     cex.main = 1.5,
     nc = 3                  # Adjust the number of columns in the plot layout
)
dev.off()

png(filename = "./fig/solar.png", width = 14, height = 4, units = "in", res = 300)
plot(stack(list(solar_stack)), 
     axes = FALSE,           # Remove axes
     box = FALSE,            # Remove the outline borders
     main = c("Solar environmental score", "Solar CF", "Solar lag"),  # Optional: Use layer names as titles
     cex.main = 1.5,
     nc = 3                  # Adjust the number of columns in the plot layout
)
dev.off()


## table 1
modela <- list(s_dat_compare %>% 
                 rst("Operational") %>% 
                 pluck(2), 
               s_dat_compare %>% 
                 rst("Substation") %>% 
                 pluck(2),
               s_dat_compare %>% 
                 rst("Queue") %>% 
                 pluck(2))

stargazer(modela,
          type = "text",
          title = "Regression Results",
          digits = 3,
          model.names = F,
          column.labels = c("Operational","Substation","Queue"),
          out = "regression_results.txt")


# 
# # prediction
# class <- c("Project","Substation","Queue")
# 
# 
# mean_results <- data.frame()
# for(j in 1:3){
#   
#   rda <- new_results[[j]]
#   
#   # Initialize a data frame to store the results
#   res_m <- numeric()
#   # res_s <- numeric()
#   
#   # Loop through each polygon and calculate mean raster value
#   for (i in 1:length(polygons_vect)) {
#     # Crop raster to the current polygon
#     cropped_raster <- terra::crop(rda, polygons_vect[i])
#     
#     # Mask raster to ensure only the polygon area is considered
#     masked_raster <- mask(cropped_raster, polygons_vect[i])
#     
#     # plot(masked_raster)
#     # Store the raster values in the list, naming it with the index
#     res_m[i] <-  mean(values(masked_raster), na.rm = T)
#     # res_s[i] <-  sd(values(masked_raster), na.rm = T)
#     
#   }
#   
#   tp <- region %>% 
#     st_drop_geometry() %>% 
#     mutate(mean = res_m,
#            type = class[j])
#   
#   mean_results <- tp %>% 
#     rbind(mean_results)
# }
# 
# 
# s1a <- mean_results %>% 
#   pivot_wider(names_from = type, values_from = mean) %>% 
#   dplyr::select(-Substation) %>% 
#   mutate(Difference = Queue - Project) %>% 
#   gather(type, mean, Queue:Difference) %>% 
#   mutate(type = factor(type, levels = c("Project","Queue","Difference"))) %>% 
#   mutate(region = factor(region, levels = rev(c("West","Mtwest","Midwest","Texas","South","Northeast")))) %>% 
#   
#   ggplot() +
#   geom_col(aes(x = mean, y = region), position = position_dodge(width = 0.9), 
#            width = 0.8, fill = "gray70") +
#   geom_vline(xintercept = 0,linetype = "dashed", size = 0.5, color = "gray30") +
#   # geom_errorbar(aes(xmin = mean - 1.96*sd, xmax = mean + 1.96*sd, y = region), position = position_dodge(width = 0.9), width = 0.8) +
#   facet_wrap(~type, scales = "free", nrow = 1)+
#   theme_bw() +
#   
#   labs(x = "Probability", y ="", color = "",
#        title = "") +
#   
#   theme(panel.grid.minor = element_blank(),
#         panel.grid.major.x = element_blank(),
#         strip.background =element_rect(fill="gray22",color="gray22"),
#         strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=12),
#         legend.position = "none",
#         axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=12),
#         axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=12),
#         axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=12),
#         plot.title=element_text(family="Franklin Gothic Demi", size=16, hjust = -0.14)) 
# 
# 
# s1b <- mean_results %>% 
#   pivot_wider(names_from = type, values_from = mean) %>% 
#   dplyr::select(-Project) %>% 
#   mutate(Difference = Queue - Substation) %>% 
#   gather(type, mean, Queue:Difference) %>% 
#   mutate(type = factor(type, levels = c("Substation","Queue","Difference"))) %>% 
#   mutate(region = factor(region, levels = rev(c("West","Mtwest","Midwest","Texas","South","Northeast")))) %>% 
#   
#   ggplot() +
#   geom_col(aes(x = mean, y = region), position = position_dodge(width = 0.9), 
#            width = 0.8, fill = "gray70") +
#   geom_vline(xintercept = 0,linetype = "dashed", size = 0.5, color = "gray30") +
#   # geom_errorbar(aes(xmin = mean - 1.96*sd, xmax = mean + 1.96*sd, y = region), position = position_dodge(width = 0.9), width = 0.8) +
#   facet_wrap(~type, scales = "free", nrow = 1)+
#   theme_bw() +
#   
#   labs(x = "Probability", y ="", color = "",
#        title = "") +
#   
#   theme(panel.grid.minor = element_blank(),
#         panel.grid.major.x = element_blank(),
#         strip.background =element_rect(fill="gray22",color="gray22"),
#         strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=12),
#         legend.position = "none",
#         axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=12),
#         axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=12),
#         axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=12),
#         plot.title=element_text(family="Franklin Gothic Demi", size=16, hjust = -0.14)) 
# 
# ggarrange(s1a, s1b, nrow = 2)


### probability change trend
s8a <- prediction_plot(new_results[[1]], s_pts, "Operational") +
  theme_publication() +
  theme(legend.position = "bottom")

s8b <- prediction_plot(new_results[[2]], s_pts, "Substation") +
  theme_publication() +
  theme(legend.position = "bottom")

s8c <- prediction_plot(new_results[[3]], inter_pts, "Queue") +
  theme_publication() +
  theme(legend.position = "bottom")

# s8b <- ggplot() +
#   geom_sf(data = polygons_vect, fill = "gray50", color = NA, size = 1.5) +
#   geom_spatraster(data = df_results_sub, aes(fill = pred_logReg_s_queue)) +
#   geom_sf(data = polygons_vect, fill = NA, color = "black", size = 1.5) +
#   # Use the same scales for consistency, but legend is hidden here.
#   scale_fill_distiller(palette = "RdBu", name = "Probability \ndifference ",
#                        na.value = NA,
#                        limits = c(-0.7, 0.7)) +   
#   
#   # scale_fill_distiller(palette = "Spectral", name = "Probability \ndifference ", na.value = NA) + 
#   # scale_color_manual(
#   #   name = "",
#   #   values = c("Sites" = "red")
#   # ) +
#   labs(title = "Probability change") +
#   theme_minimal() +
#   theme_publication() +
#   theme(legend.position = "bottom")


# s8c <- rst_plot(f3_glm_sub) +
#   labs(y = "Probability difference") +
#   theme(legend.position = "right")


## table 2
modelb <- list(f3_glm, f3_glm_sub)

stargazer(modelb,
          type = "text",
          title = "Regression Results",
          digits = 3,
          model.names = F,
          column.labels = c("Operational","Substation"),
          out = "regression_results.txt")


# s8 <- ggarrange(
#   ggarrange(s8a, s8b, nrow = 1),
#   s8c, nrow = 2, 
#   labels = c("A", "B"),  # Adds labels to plots
#   label.x = 0,        # Adjust horizontal position of labels
#   label.y = 1,        # Adjust vertical position of labels
#   # vjust = 1,
#   # hjust = -1,
#   font.label = list(size = 14, face = "bold")
# )

ggsave("./fig/s8.png", ggarrange(s8a,s8b,s8c, nrow = 1), width = 12, height = 4)



### additional for sensistivity analysis requested by reviewers 

data <- s_dat_compare %>% 
  filter(class == "Operational")
glm_o <- glm(treat ~ tx + landAcq + roads + slope + pop + hail + fire + community + lowincome + minority + unemploy +
               lulc_forest + lulc_grassland + lulc_shrubland + lulc_riparian + lulc_sparse + lulc_agriculture + lulc_developed  +
               env + cf + lag, 
             data=data, family = binomial(link="logit"))


data <- s_dat_compare %>% 
  filter(class == "Queue")
glm_q <- glm(treat ~ tx + landAcq + roads + slope + pop + hail + fire + community + lowincome + minority + unemploy +
               lulc_forest + lulc_grassland + lulc_shrubland + lulc_riparian + lulc_sparse + lulc_agriculture + lulc_developed +
               env + cf + lag, 
             data=data, family = binomial(link="logit"))




# VIF
library(car)
vif_o   <- vif(glm_o) 
vif_q   <- vif(glm_q) 


s8a <- data.frame(
  Predictor = names(vif_o),
  Operational = round(vif_o, 2),
  Queue = round(vif_q, 2)
) %>% 
  mutate(Predictor = dplyr::recode(Predictor, !!!name_lookup)) %>% 
  
  pivot_longer(cols = c(Operational, Queue),
               names_to = "Model",
               values_to = "VIF") %>% 
  mutate(Model = str_wrap(Model, width = 10)) %>% 
  
  ggplot(aes(x = reorder(Predictor, VIF), y = VIF, fill = Model)) +
  geom_col(position = position_dodge(width = 0.7)) +
  
  scale_fill_manual(values = c("#1f78b4", "#33a02c"),
                    labels = c("Operational", "Queue")) +
  labs(x = "Predictor",
       y = "Variance Inflation Factor (VIF)",
       fill = "Model",
       title = "Comparison of VIFs across Operational and Queue models") +
  
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom",
    axis.text.x = element_text(color = "black", size = 10, angle = 40, hjust = 1),
    legend.box = "horizontal",
    legend.box.margin = margin(r = 20)     # add right-side margin
    
  )



data <- s_dat_compare %>% 
  filter(class == "Operational")
glm_os <- glm(treat ~ pop + hail, 
              data=data, family = binomial(link="logit"))


data <- s_dat_compare %>% 
  filter(class == "Queue")
glm_qs <- glm(treat ~ pop + hail, 
              data=data, family = binomial(link="logit"))


vif_o   <- vif(glm_os) 
vif_q   <- vif(glm_qs) 

s8b <- data.frame(
  Predictor = names(vif_o),
  Operational = round(vif_o, 2),
  Queue = round(vif_q, 2)
) %>% 
  mutate(Predictor = dplyr::recode(Predictor, !!!name_lookup)) %>% 
  
  pivot_longer(cols = c(Operational, Queue),
               names_to = "Model",
               values_to = "VIF") %>% 
  mutate(Model = str_wrap(Model, width = 10)) %>% 
  
  ggplot(aes(x = reorder(Predictor, VIF), y = VIF, fill = Model)) +
  geom_col(position = position_dodge(width = 0.7)) +
  
  scale_fill_manual(values = c("#1f78b4", "#33a02c"),
                    labels = c("Operational", "Queue")) +
  labs(x = "Predictor",
       y = "Variance Inflation Factor (VIF)",
       fill = "Model",
       title = "Comparison of VIFs across Operational and Queue models") +
  
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom",
    axis.text.x = element_text(color = "black", size = 10, hjust = 1),
    legend.box = "horizontal",
    legend.box.margin = margin(r = 20)     # add right-side margin
    
  )

data <- s_dat_compare %>% 
  filter(class == "Operational")

s8c <- ggplot(data, aes(x = pop, y = hail)) +
  geom_point(color = "steelblue", alpha = 0.7) +
  labs(x = "Population Density",
       y = "Hail") +
  annotate("text",
           x = max(data$pop, na.rm = TRUE),
           y = max(data$hail, na.rm = TRUE),
           label = paste0("r = ",
                          round(cor(data$hail, data$pop,
                                    use = "complete.obs"), 3)),
           hjust = 1, vjust = 1,
           size = 5)


ggsave("./fig/s8a.png", 
       ggarrange(s8a, 
                 ggarrange(s8b,s8c, nrow = 1),
                 nrow = 2,
                 heights = c(1.5,1)), width = 12, height = 10)



### model without pop 
data <- s_dat_compare %>% 
  filter(class == "Operational")
glm_op <- glm(treat ~ tx + landAcq + roads + slope + hail + fire + community + lowincome + minority + unemploy +
                lulc_forest + lulc_grassland + lulc_shrubland + lulc_riparian + lulc_sparse + lulc_agriculture + lulc_developed  +
                env + cf + lag, 
              data=data, family = binomial(link="logit"))

m1 <- tidy(glm_o) %>% mutate(model = "Population density included")
m2 <- tidy(glm_op) %>% mutate(model = "Population density excluded")


# Combine
coef_df <- bind_rows(m1, m2)

coef_df <- coef_df %>%
  filter(term != "(Intercept)") %>%
  mutate(
    lower = estimate - 1.96*std.error,
    upper = estimate + 1.96*std.error
  ) %>% 
  mutate(term = dplyr::recode(term, !!!name_lookup)) 


s8bb <- ggplot(coef_df, aes(x = reorder(term, estimate), y = estimate, color = model)) +
  geom_point(position = position_dodge(width = 0.6), size = 2) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.2,
                position = position_dodge(width = 0.6)) +
  labs(
    x = "Coefficient",
    y = "Estimate (±1.96*SE)",
    color = "Model",
    title = "Operational project model comparison"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(color = "black", size = 10, angle = 40, hjust = 1),
  )


ggsave("./fig/s8b.png", 
       s8bb, width = 12, height = 6)


### old/new cov sensitivity 
s_dat_inter <- solar.cov.bg.inter %>%
  filter(group == 5) %>%
  dplyr::select(-group) %>%
  rbind(solar.cov.existing.inter) %>%
  mutate(treat = factor(treat)) %>%
  na.omit()


glm1_solar <- glm(treat ~ tx + landAcq + roads + slope + pop + hail + fire + community + lowincome + minority + unemploy +
                    lulc_forest + lulc_grassland + lulc_shrubland + lulc_riparian + lulc_sparse + lulc_agriculture + lulc_developed +
                    env + cf + lag,
                  data=s_dat_inter, family = binomial(link="logit"))
# summary(glm1_solar)


s.bg.que.old <- read_csv("./data/bg_cov_solar_inter_old.csv")
s.ex.que.old <- read_csv("./data/existing_cov_solar_inter_old.csv")

s.bg.que.old <- s.bg.que.old %>% 
  mutate(group = c(rep(seq(1,10,1), each = floor(nrow(s.bg.que.old)/10)),rep(10,1))) # modify the last rep(10,x)

s_dat_que <- s.bg.que.old %>%
  filter(group == 5) %>%
  dplyr::select(-group) %>%
  rbind(s.ex.que.old) %>%
  mutate(treat = factor(treat)) %>%
  na.omit()


glm1_solar1 <- glm(treat ~ tx + landAcq + roads + slope + pop + hail + fire + community + lowincome + minority + unemploy +
                    lulc_forest + lulc_grassland + lulc_shrubland + lulc_riparian + lulc_sparse + lulc_agriculture + lulc_developed +
                    env + cf + lag,
                  data=s_dat_que, family = binomial(link="logit"))
# summary(glm1_solar)


m1 <- tidy(glm1_solar) %>% mutate(model = "Updated TX & LULC")
m2 <- tidy(glm1_solar1) %>% mutate(model = "2017 TX & 2016 LULC")


# Combine
coef_df <- bind_rows(m1, m2)

coef_df <- coef_df %>%
  filter(term != "(Intercept)") %>%
  mutate(
    lower = estimate - 1.96*std.error,
    upper = estimate + 1.96*std.error
  ) %>% 
  mutate(term = dplyr::recode(term, !!!name_lookup)) 


s8cc <- ggplot(coef_df, aes(x = reorder(term, estimate), y = estimate, color = model)) +
  geom_point(position = position_dodge(width = 0.6), size = 2) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.2,
                position = position_dodge(width = 0.6)) +
  labs(
    x = "Coefficient",
    y = "Estimate (±1.96*SE)",
    color = "Model",
    title = "Queue project model comparison"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(color = "black", size = 10, angle = 45, hjust = 1),
  )

ggsave("./fig/s8c.png", 
       s8cc, width = 12, height = 6)


### spatial autocorrelation
set.seed(42)
n_total_pixels <- 500000  # Simulating a large raster dataset
# df <- data.frame(
#   x = runif(n_total_pixels, min = -125, max = -67), # US Longitude range
#   y = runif(n_total_pixels, min = 25, max = 49),   # US Latitude range
#   residuals = rnorm(n_total_pixels, mean = 0, sd = 1)
# )





sample_size <- 10000

if (nrow(df) > sample_size) {
  df_sample <- df[sample(1:nrow(df), sample_size, replace = FALSE), ]
  message(paste("Subsampled", sample_size, "pixels out of", nrow(df), "total pixels."))
} else {
  df_sample <- df
}


coords <- cbind(df_sample$x, df_sample$y)

# Find the 4 nearest neighbors for each cell centroid
# k = 4 is standard for grid structures (equivalent to Rook adjacency)
knn_neighbors <- knearneigh(coords, k = 4)

# Convert to an nb neighbor object
nb_list <- knn2nb(knn_neighbors)

# ------------------------------------------------------------------------------
# 4. CREATE THE ROW-STANDARDIZED SPATIAL WEIGHTS MATRIX (W)
# ------------------------------------------------------------------------------
# Style "W" row-standardizes the weights so they sum to 1 for each observation.
spatial_weights <- nb2listw(nb_list, style = "W")

# ------------------------------------------------------------------------------
# 5. RUN THE SPATIAL AUTOCORRELATION TESTS
# ------------------------------------------------------------------------------
message("\n--- Running Analytical Moran's I Test ---")
# Standard analytical test (assumes asymptotic normality)
moran_analytical <- moran.test(df_sample$residuals, spatial_weights)
print(moran_analytical)


message("\n--- Running Monte Carlo Permutation Test (999 simulations) ---")
# Robust permutation test that does not rely on normality assumptions
moran_mc <- moran.mc(df_sample$residuals, spatial_weights, nsim = 999)
print(moran_mc)

# ------------------------------------------------------------------------------
# 6. EXTRACT RESULTS FOR YOUR REVIEWER RESPONSE
# ------------------------------------------------------------------------------
obs_I <- moran_mc$statistic
p_val <- moran_mc$p.value

cat("\n======================================================================\n")
cat(sprintf("Moran's I Statistic: %0.4f\n", obs_I))
cat(sprintf("Permutation p-value: %0.4f\n", p_val))
cat("======================================================================\n")






## table 3
modelc <- list(f4_glm3, f4_glm1)
stargazer(modelc,
          type = "text",
          title = "Regression Results",
          digits = 3,
          model.names = F,
          column.labels = c("1-mile","250-meter"),
          out = "regression_results.txt")


s9 <- rst_plot(f4_glm3, f4_glm1) +
  scale_y_continuous(
    labels = scales::label_number(accuracy = 1)             
  ) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(), # Remove horizontal grid lines for clarity.
    panel.spacing = unit(1.5, "lines"),
    
    # Style facet labels.
    strip.background = element_rect(fill = "gray80", color = NA),
    strip.text = element_text(color = 'black', face = "bold", size = 12),
    
    # Legend position and styling.
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    
    # Axis styling.
    axis.text.x = element_text(color = "black", size = 12, angle = 45, hjust = 1),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    
    # Title and subtitle styling.
    plot.title = element_text(face = "bold", size = 18, margin = margin(b = 5), hjust = 0.5),
    plot.subtitle = element_text(size = 14, color = "gray30", margin = margin(b = 15))
    
  )

ggsave("./fig/s9.png", s9, width = 12, height = 10)




tp <- sqrt(attr(ranef(f4_glm2, condVar=T)[[1]], "postVar"))*1.96
s10b <- as.data.frame(ranef(f4_glm2)$status) %>% 
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



s10 <- ggarrange(s10a, s10b,
                 heights = c(2,1.2),
  nrow = 2,
  labels = c("A", "B"),  # Adds labels to plots
  label.x = 0,        # Adjust horizontal position of labels
  label.y = 1,        # Adjust vertical position of labels
  # vjust = 1,
  # hjust = -1,
  font.label = list(size = 14, face = "bold"))

ggsave("./fig/s10.png", s10, width = 12, height = 12)



### regression results
# Extract tidy fixed effects summary
fixed_df <- tidy(fit, effects = "fixed", conf.int = TRUE)

# Define name mapping
name_map <- c(
  "TYPEHUQ" = "Multifamily",
  "SQFTEST" = "Larger home",
  "YEARMADERANGE" = "Newer home",
  "TOTROOMS" = "More rooms",
  "NHSLDMEM" = "More people",
  "FUELH2O1" = "Electricity (Water Heating)",
  "FUELH2O5" = "Fuel (Water Heating)",
  "ACEQUIPM1" = "Central AC",
  "ACEQUIPM3" = "Room AC",
  "ACEQUIPM4" = "Portable AC",
  "ACEQUIPM6" = "Other AC",
  "MONEYPY" = "Income",
  "EDUCATION" = "Education",
  "HOUSEHOLDER_RACE2" = "Black",
  "HOUSEHOLDER_RACE3" = "Asian",
  "HOUSEHOLDER_RACE4" = "Other Race"
)

# Apply the name mapping to the tidy dataframe
fixed_df <- fixed_df %>%
  mutate(term = recode(term, !!!name_map))

# Display as formatted table
kable(fixed_df, digits = 3, caption = "Fixed Effects Summary from Mixed Effect Model") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE)
# tidy(fit, effects = "ran_pars")
