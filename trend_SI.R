source("./trend/trend_function.R")
processed_layer_path <- "/home/energysiting/data/processed_data/variables"


png(filename = "./trend/fig/lulc.png", width = 12, height = 12, units = "in", res = 300)

par(mfrow = c(2,1))

plot(rast(file.path(processed_layer_path, "lulc.tif")), main = "2016 Land Cover",
     axes = FALSE,           # Remove axes
     box = FALSE)
plot(rast(file.path(processed_layer_path, "lulc_new.tif")), main = "2023 Land Cover",
     axes = FALSE,           # Remove axes
     box = FALSE)

dev.off()


solar_stack <- stack("./trend/data/solar_covStack.tif")
coeff_stack <- stack("./trend/data/covStack_old.tif")
coeff_stack_new <- stack("./trend/data/covStack_new.tif")


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


png(filename = "./trend/fig/var_cov.png", width = 14, height = 8, units = "in", res = 300)

plot(stack(list(coeff_stack[[1]],coeff_stack_new[[1:11]])), 
     axes = FALSE,           # Remove axes
     box = FALSE,            # Remove the outline borders
     main = c("Transmission dist 2017","Transmission dist 2024",cov.names[c(2:11)]),  # Optional: Use layer names as titles
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


png(filename = "./trend/fig/change_old.png", width = 14, height = 10, units = "in", res = 300)
plot(coeff_stack[[12:19]],
     axes = FALSE,           # Remove axes
     box = FALSE,            # Remove the outline borders
     legend = FALSE, 
     main = cov.names[c(12:19)],  # Optional: Use layer names as titles
     nc = 3                  # Adjust the number of columns in the plot layout
)
dev.off()


png(filename = "./trend/fig/change_new.png", width = 14, height = 10, units = "in", res = 300)
plot(coeff_stack_new[[12:19]],
     axes = FALSE,           # Remove axes
     box = FALSE,            # Remove the outline borders
     legend = FALSE, 
     main = cov.names[c(12:19)],  # Optional: Use layer names as titles
     nc = 3                  # Adjust the number of columns in the plot layout
)
dev.off()

png(filename = "./trend/fig/solar.png", width = 14, height = 4, units = "in", res = 300)
plot(stack(list(solar_stack)), 
     axes = FALSE,           # Remove axes
     box = FALSE,            # Remove the outline borders
     main = c("Solar environmental score", "Solar CF", "Solar lag"),  # Optional: Use layer names as titles
     nc = 3                  # Adjust the number of columns in the plot layout
)
dev.off()


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
f3a_s <- prediction_plot(new_results[[2]], s_pts, "Substation")
f3b_s <- ggplot() +
  geom_sf(data = polygons_vect, fill = "gray50", color = NA, size = 1.5) +
  geom_spatraster(data = df_results_sub, aes(fill = pred_logReg_s_queue)) +
  geom_sf(data = polygons_vect, fill = NA, color = "black", size = 1.5) +
  # Use the same scales for consistency, but legend is hidden here.
  scale_fill_distiller(palette = "RdBu", name = "Probability \ndifference ",
                       na.value = NA,
                       limits = c(-0.7, 0.7)) +   
  
  # scale_fill_distiller(palette = "Spectral", name = "Probability \ndifference ", na.value = NA) + 
  # scale_color_manual(
  #   name = "",
  #   values = c("Sites" = "red")
  # ) +
  labs(title = "Probability change") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom" # Hide the legend on individual plots
  )



f3c_s <- rst_plot(f3_glm_sub) +
  labs(y = "Probability difference") +
  theme(legend.position = "right")


f3_s <- ggarrange(
  ggarrange(f3a_s, f3b_s, nrow = 1),
  f3c_s, nrow = 2, 
  labels = c("A", "B"),  # Adds labels to plots
  label.x = 0,        # Adjust horizontal position of labels
  label.y = 1,        # Adjust vertical position of labels
  # vjust = 1,
  # hjust = -1,
  font.label = list(size = 14, face = "bold")
)

ggsave("./trend/fig/f3_s.png", f3_s, width = 12, height = 12)



s10 <- solar_que %>% # filter for utility scale (>1MW)
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
  summarise(Count = n()) %>% 
  
  ggplot() +
  geom_col(aes(x = type, y = Count, fill = Match), position = "dodge") +
  facet_wrap(~region, nrow = 1) +
  labs(x = "Type", y = "Count", fill = "Matched") +
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
  
ggsave("./trend/fig/s10.png", s10, width = 12, height = 6)

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

s11 <- rbind(
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


ggsave("./trend/fig/s11.png", s11, width = 12, height = 6)

s12 <- solar_que %>% # filter for utility scale (>1MW)
  mutate(region = case_when(state %in% northeast ~ "Northeast",
                            state %in% midwest ~ "Midwest",
                            state %in% west ~ "West",
                            state %in% south ~ "South",
                            state %in% mtwest ~ "Mtwest",
                            T ~ "Texas")) %>% 
  mutate(type = ifelse(lbnl_type == "Solar+Battery", "Solar+Battery",
                       ifelse(lbnl_type == "Solar", "Solar", "Others")),
         type = factor(type, levels = c("Solar","Others","Solar+Battery")),
         region = factor(region, levels = c("West","Mtwest","Midwest","Texas","South","Northeast"))) %>% 
  
  filter(match_confidence > 40) %>%
  st_as_sf(coords = c("matched_sub_lon", "matched_sub_lat"), crs = 4269) %>%  
  
  mutate(capacity_bin = cut(capacity_mw,
                            breaks = c(1, 50, 200, 500, 1000, Inf),
                            labels = c("1–50", "51–200", "201–500", "501–1000", "1000+"),
                            right = FALSE)) %>%
  
  ggplot() +

  geom_sf(data = rgn, color = NA, fill = "gray50") + # US border
  geom_sf(aes(color = type, size = capacity_bin), alpha = 0.7) +
  geom_sf(data = rgn, color = "gray0", fill = NA) + # US border
  # Use the same scales for consistency, but legend is hidden here.
  scale_color_brewer(palette = "RdYlBu", direction = -1, name = "") +
  scale_size_manual(
    values = c(0.2, 1, 3, 4, 5)
  ) +
  theme_minimal() +
  
  labs(title = "", fill = "", size = "Capacity (MW)") +
  coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), 
           ylim = c(-2300000,730000), expand = FALSE, datum = NA) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.text = element_text(color = 'black', size=12),
        strip.text = element_text(color = 'black',family="Franklin Gothic Book",size=12, face = "bold"),
        legend.position = "right",
        legend.key.size = unit(1, 'cm'), 
        axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=12),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=12),
        plot.title=element_text(family="Franklin Gothic Demi", size=20, hjust = 0.5))

ggsave("./trend/fig/s12.png", s12, width = 12, height = 10)
