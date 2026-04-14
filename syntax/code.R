source("./syntax/function.R")

### figures
s_com$highlight <- "Sites"
f0a <- s_com %>% 
  ggplot() +
  geom_sf(data = rgn, aes(fill = region), color = "gray0") + # US border
  
  # First fill scale for region
  scale_fill_manual(
    values = setNames(scales::hue_pal(l = 70, c = 40)(6), 
                      c("West","Mtwest","Midwest","Texas","South","Northeast")),
    name = ""
  ) +
  
  # Introduce a new fill scale
  ggnewscale::new_scale_fill() +
  
  # Overlay s_com with highlight fill
  geom_sf(data = s_com, aes(fill = highlight), color = "gray10", size = 0.7, stroke = 0.2) +
  
  # Second fill scale for highlight
  scale_fill_manual(
    values = c("Sites" = "gray10"),
    name = ""
  ) +
  
  
  # geom_sf(aes(fill = highlight), color = NA, size = 0.3) +
  
  facet_wrap(~ class, nrow = 3) +
  
  theme_minimal() +
  
  labs(title = "", fill = "") +
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
        plot.title=element_text(family="Franklin Gothic Demi", size=20, hjust = 0.5)) +
  guides(fill = guide_legend(reverse = T))


# count
f0b1 <- solar_que %>% 
  mutate(region = case_when(state %in% northeast ~ "Northeast",
                            state %in% midwest ~ "Midwest",
                            state %in% west ~ "West",
                            state %in% south ~ "South",
                            state %in% mtwest ~ "Mtwest",
                            T ~ "Texas")) %>% 
  group_by(region) %>% 
  summarise(Count = n()) %>% 
  ungroup() %>% 
  mutate(class = "Queue") %>% 
  rbind(
    s %>% 
      st_drop_geometry() %>% 
      mutate(region = case_when(state %in% northeast ~ "Northeast",
                                state %in% midwest ~ "Midwest",
                                state %in% west ~ "West",
                                state %in% south ~ "South",
                                state %in% mtwest ~ "Mtwest",
                                T ~ "Texas")) %>% 
      
      group_by(region) %>% 
      summarise(Count = n()) %>% 
      ungroup() %>% 
      mutate(class = "Operational")
  ) %>% 
  pivot_wider(names_from = class, values_from = Count) %>% 
  mutate(Difference = Queue - Operational) %>% 
  gather(class, Count, Queue:Difference) %>% 
  
  mutate(region = factor(region, levels = c("West","Mtwest","Midwest","Texas","South","Northeast")),
         region_fill = factor(region, levels = rev(c("West","Mtwest","Midwest","Texas","South","Northeast")))) %>% 
  mutate(class = factor(class, levels = c("Operational","Queue","Difference"))) %>% 
  
  ggplot() +
  geom_col(aes(x = Count, y = region_fill, fill = region)) +
  geom_vline(xintercept = 0,linetype = "dashed", size = 0.5, color = "gray30") +
  
  scale_fill_manual(
    values = setNames(scales::hue_pal(l = 70, c = 40)(6), 
                      c("West","Mtwest","Midwest","Texas","South","Northeast")),
    name = ""
  ) +
  
  labs(x = "Count", y = "",fill = "") +
  facet_wrap(~ class, nrow = 3) +
  theme_classic() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background =element_rect(fill="gray22",color="gray22"),
        strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=12, face = "bold"),
        legend.position = "none",
        # axis.text.x = element_blank(),
        # axis.ticks.x = element_blank(),
        axis.text = element_text(color = "black",family="Franklin Gothic Book",size=12),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=12),
        plot.title=element_text(family="Franklin Gothic Demi", size=20)) +
  guides(fill = guide_legend(byrow=T, nrow = 1))

# f0b <- solar_inter %>% 
#   dplyr::rename(state = STATE) %>% 
#   st_drop_geometry() %>% 
#   mutate(region = case_when(state %in% northeast ~ "Northeast",
#                             state %in% midwest ~ "Midwest",
#                             state %in% west ~ "West",
#                             state %in% south ~ "South",
#                             state %in% mtwest ~ "Mtwest",
#                             T ~ "Texas")) %>% 
#   group_by(region) %>% 
#   summarise(Count = sum(count, na.rm = T)) %>% 
#   ungroup() %>% 
#   mutate(class = "Queue") %>% 
#   rbind(
#     w %>% 
#       st_drop_geometry() %>% 
#       mutate(region = case_when(state %in% northeast ~ "Northeast",
#                                 state %in% midwest ~ "Midwest",
#                                 state %in% west ~ "West",
#                                 state %in% south ~ "South",
#                                 state %in% mtwest ~ "Mtwest",
#                                 T ~ "Texas")) %>% 
#       
#       group_by(region) %>% 
#       summarise(Count = n()) %>% 
#       ungroup() %>% 
#       mutate(class = "Project")
#   ) %>% 
#   pivot_wider(names_from = class, values_from = Count) %>% 
#   mutate(Difference = Queue - Project) %>% 
#   gather(class, Count, Queue:Difference) %>% 
#   
#   mutate(region = factor(region, levels = c("West","Mtwest","Midwest","Texas","South","Northeast")),
#          region_fill = factor(region, levels = rev(c("West","Mtwest","Midwest","Texas","South","Northeast")))) %>% 
#   mutate(class = factor(class, levels = c("Project","Queue","Difference"))) %>% 
#   
#   ggplot() +
#   geom_col(aes(x = Count, y = region_fill, fill = region)) +
#   geom_vline(xintercept = 0,linetype = "dashed", size = 0.5, color = "gray30") +
#   
#   scale_fill_manual(
#     values = setNames(scales::hue_pal(l = 70, c = 40)(6), 
#                       c("West","Mtwest","Midwest","Texas","South","Northeast")),
#     name = ""
#   ) +
#   
#   labs(x = "Count", y = "",fill = "") +
#   facet_wrap(~ class, nrow = 3) +
#   theme_classic() +
#   theme(panel.grid.minor = element_blank(),
#         panel.grid.major.x = element_blank(),
#         strip.background =element_rect(fill="gray22",color="gray22"),
#         strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=12, face = "bold"),
#         legend.position = "none",
#         # axis.text.x = element_blank(),
#         # axis.ticks.x = element_blank(),
#         axis.text = element_text(color = "black",family="Franklin Gothic Book",size=12),
#         axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=12),
#         plot.title=element_text(family="Franklin Gothic Demi", size=20)) +
#   guides(fill = guide_legend(byrow=T, nrow = 1))

# f0b <- s_com %>% 
#   filter(class != "Substation") %>% 
#   st_drop_geometry() %>% 
#   mutate(region = case_when(state %in% northeast ~ "Northeast",
#                             state %in% midwest ~ "Midwest",
#                             state %in% west ~ "West",
#                             state %in% south ~ "South",
#                             state %in% mtwest ~ "Mtwest",
#                             T ~ "Texas")) %>% 
#   
#   group_by(region, class) %>% 
#   summarise(Count = n()) %>% 
#   ungroup() %>% 
#   pivot_wider(names_from = class, values_from = Count) %>% 
#   mutate(Difference = Queue - Project) %>% 
#   gather(class, Count, Queue:Difference) %>% 
# 
#   mutate(region = factor(region, levels = c("West","Mtwest","Texas","Midwest","South","Northeast")),
#          region_fill = factor(region, levels = rev(c("West","Mtwest","Texas","Midwest","South","Northeast")))) %>% 
#   mutate(class = factor(class, levels = c("Queue","Project","Difference"))) %>% 
# 
#   ggplot() +
#   geom_col(aes(x = region, y = Count, fill = region)) +
#   labs(x = "Count", y = "",fill = "") +
#   facet_wrap(~ class, nrow = 1) +
#   theme_classic() +
#   theme(panel.grid.minor = element_blank(),
#         panel.grid.major.x = element_blank(),
#         strip.background =element_rect(fill="gray22",color="gray22"),
#         strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=12, face = "bold"),
#         legend.position = "bottom",
#         axis.text.x = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=12),
#         axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=12),
#         plot.title=element_text(family="Franklin Gothic Demi", size=20)) +
#   guides(fill = guide_legend(byrow=T, nrow = 1))


# capacity/ count
f0c1 <- solar_que %>% 
  mutate(region = case_when(state %in% northeast ~ "Northeast",
                            state %in% midwest ~ "Midwest",
                            state %in% west ~ "West",
                            state %in% south ~ "South",
                            state %in% mtwest ~ "Mtwest",
                            T ~ "Texas")) %>% 
  group_by(region) %>% 
  summarise(Capacity = sum(capacity_mw, na.rm = T)/n()) %>% 
  ungroup() %>% 
  mutate(class = "Queue") %>% 
  rbind(
    s %>% 
      st_drop_geometry() %>% 
      mutate(region = case_when(state %in% northeast ~ "Northeast",
                                state %in% midwest ~ "Midwest",
                                state %in% west ~ "West",
                                state %in% south ~ "South",
                                state %in% mtwest ~ "Mtwest",
                                T ~ "Texas")) %>% 
      
      group_by(region) %>% 
      summarise(Capacity = sum(capacity, na.rm = T)/n()) %>% 
      ungroup() %>% 
      mutate(class = "Operational")
  ) %>% 
  pivot_wider(names_from = class, values_from = Capacity) %>% 
  mutate(Difference = Queue - Operational) %>% 
  gather(class, Capacity, Queue:Difference) %>% 
  
  mutate(region = factor(region, levels = c("West","Mtwest","Midwest","Texas","South","Northeast")),
         region_fill = factor(region, levels = rev(c("West","Mtwest","Midwest","Texas","South","Northeast")))) %>% 
  mutate(class = factor(class, levels = c("Operational","Queue","Difference"))) %>% 
  
  ggplot() +
  geom_col(aes(x = Capacity, y = region_fill, fill = region)) +
  geom_vline(xintercept = 0,linetype = "dashed", size = 0.5, color = "gray30") +
  
  scale_fill_manual(
    values = setNames(scales::hue_pal(l = 70, c = 40)(6), 
                      c("West","Mtwest","Midwest","Texas","South","Northeast")),
    name = ""
  ) +
  
  labs(x = "Capacity (MW/project)", y = "",fill = "") +
  facet_wrap(~ class, nrow = 3) +
  theme_classic() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background =element_rect(fill="gray22",color="gray22"),
        strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=12, face = "bold"),
        legend.position = "none",
        # axis.text.x = element_blank(),
        # axis.ticks.x = element_blank(),
        axis.text = element_text(color = "black",family="Franklin Gothic Book",size=12),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=12),
        plot.title=element_text(family="Franklin Gothic Demi", size=20)) +
  guides(fill = guide_legend(byrow=T, nrow = 1))


# # capacity
# f0c2 <- solar_que %>% 
#   mutate(region = case_when(state %in% northeast ~ "Northeast",
#                             state %in% midwest ~ "Midwest",
#                             state %in% west ~ "West",
#                             state %in% south ~ "South",
#                             state %in% mtwest ~ "Mtwest",
#                             T ~ "Texas")) %>% 
#   group_by(region) %>% 
#   summarise(Capacity = sum(capacity_mw, na.rm = T)) %>% 
#   ungroup() %>% 
#   mutate(class = "Queue") %>% 
#   rbind(
#     w %>% 
#       st_drop_geometry() %>% 
#       mutate(region = case_when(state %in% northeast ~ "Northeast",
#                                 state %in% midwest ~ "Midwest",
#                                 state %in% west ~ "West",
#                                 state %in% south ~ "South",
#                                 state %in% mtwest ~ "Mtwest",
#                                 T ~ "Texas")) %>% 
#       
#       group_by(region) %>% 
#       summarise(Capacity = sum(capacity/1000, na.rm = T)) %>% 
#       ungroup() %>% 
#       mutate(class = "Project")
#   ) %>% 
#   pivot_wider(names_from = class, values_from = Capacity) %>% 
#   mutate(Difference = Queue - Project) %>% 
#   gather(class, Capacity, Queue:Difference) %>% 
#   
#   mutate(region = factor(region, levels = c("West","Mtwest","Midwest","Texas","South","Northeast")),
#          region_fill = factor(region, levels = rev(c("West","Mtwest","Midwest","Texas","South","Northeast")))) %>% 
#   mutate(class = factor(class, levels = c("Project","Queue","Difference"))) %>% 
#   
#   ggplot() +
#   geom_col(aes(x = Capacity, y = region_fill, fill = region)) +
#   geom_vline(xintercept = 0,linetype = "dashed", size = 0.5, color = "gray30") +
#   
#   scale_fill_manual(
#     values = setNames(scales::hue_pal(l = 70, c = 40)(6), 
#                       c("West","Mtwest","Midwest","Texas","South","Northeast")),
#     name = ""
#   ) +
#   
#   labs(x = "Capacity (MW)", y = "",fill = "") +
#   facet_wrap(~ class, nrow = 3) +
#   theme_classic() +
#   theme(panel.grid.minor = element_blank(),
#         panel.grid.major.x = element_blank(),
#         strip.background =element_rect(fill="gray22",color="gray22"),
#         strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=12, face = "bold"),
#         legend.position = "none",
#         # axis.text.x = element_blank(),
#         # axis.ticks.x = element_blank(),
#         axis.text = element_text(color = "black",family="Franklin Gothic Book",size=12),
#         axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=12),
#         plot.title=element_text(family="Franklin Gothic Demi", size=20)) +
#   guides(fill = guide_legend(byrow=T, nrow = 1))
# 
# 
# f0c <- solar_inter %>% 
#   dplyr::rename(state = STATE) %>% 
#   st_drop_geometry() %>% 
#   mutate(region = case_when(state %in% northeast ~ "Northeast",
#                             state %in% midwest ~ "Midwest",
#                             state %in% west ~ "West",
#                             state %in% south ~ "South",
#                             state %in% mtwest ~ "Mtwest",
#                             T ~ "Texas")) %>% 
#   group_by(region) %>% 
#   summarise(Capacity = sum(capacity_mw, na.rm = T)/sum(count, na.rm = T)) %>% 
#   ungroup() %>% 
#   mutate(class = "Queue") %>% 
#   rbind(
#     w %>% 
#       st_drop_geometry() %>% 
#       mutate(region = case_when(state %in% northeast ~ "Northeast",
#                                 state %in% midwest ~ "Midwest",
#                                 state %in% west ~ "West",
#                                 state %in% south ~ "South",
#                                 state %in% mtwest ~ "Mtwest",
#                                 T ~ "Texas")) %>% 
#       
#       group_by(region) %>% 
#       summarise(Capacity = sum(capacity, na.rm = T)/n()) %>% 
#       ungroup() %>% 
#       mutate(class = "Project")
#   ) %>% 
#   pivot_wider(names_from = class, values_from = Capacity) %>% 
#   mutate(Difference = Queue - Project) %>% 
#   gather(class, Capacity, Queue:Difference) %>% 
#   
#   mutate(region = factor(region, levels = c("West","Mtwest","Midwest","Texas","South","Northeast")),
#          region_fill = factor(region, levels = rev(c("West","Mtwest","Midwest","Texas","South","Northeast")))) %>% 
#   mutate(class = factor(class, levels = c("Project","Queue","Difference"))) %>% 
#   
#   ggplot() +
#   geom_col(aes(x = Capacity, y = region_fill, fill = region)) +
#   geom_vline(xintercept = 0,linetype = "dashed", size = 0.5, color = "gray30") +
#   
#   scale_fill_manual(
#     values = setNames(scales::hue_pal(l = 70, c = 40)(6), 
#                       c("West","Mtwest","Midwest","Texas","South","Northeast")),
#     name = ""
#   ) +
#   
#   labs(x = "Capacity (MW/count)", y = "",fill = "") +
#   facet_wrap(~ class, nrow = 3) +
#   theme_classic() +
#   theme(panel.grid.minor = element_blank(),
#         panel.grid.major.x = element_blank(),
#         strip.background =element_rect(fill="gray22",color="gray22"),
#         strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=12, face = "bold"),
#         legend.position = "none",
#         # axis.text.x = element_blank(),
#         # axis.ticks.x = element_blank(),
#         axis.text = element_text(color = "black",family="Franklin Gothic Book",size=12),
#         axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=12),
#         plot.title=element_text(family="Franklin Gothic Demi", size=20)) +
#   guides(fill = guide_legend(byrow=T, nrow = 1))
    

f0 <- ggarrange(
  f0a,
  ggarrange(f0b1,f0c1, nrow = 1),
  nrow = 1, widths = c(2,1.8),
  labels = c("A", "B"),  # Adds labels to plots
  label.x = 0,        # Adjust horizontal position of labels
  label.y = 1,        # Adjust vertical position of labels
  # vjust = 1,
  # hjust = -1,
  font.label = list(size = 14, face = "bold")
)

ggsave("./fig/f0.png", f0, width = 12, height = 8)



f1 <- s_dat_compare %>% 
  rst("Operational") %>% 
  rbind(
    s_dat_compare %>% 
      rst("Substation"),
    s_dat_compare %>% 
      rst("Queue")
  ) %>% 
  mutate(class = factor(class, levels = c("Operational","Substation","Queue"))) %>% 
  mutate(vari = rep(v_name, 3),
         vari = fct_reorder(vari, pe)) %>% 
  filter(!var == "(Intercept)") %>%
  filter(!var %in% c("slope","env","landAcq","roads")) %>% 
  mutate(domain = case_when(str_detect(var, "tx|roads|landAcq|cf|slope") ~ "Technical",
                            str_detect(var, "env|hail|fire") ~ "Environmental risk",
                            str_detect(var, "lowincome|minority|unemploy|pop") ~ "Social",
                            str_detect(var, "lag|community") ~ "Spatial/policy",
                            str_detect(var, "lulc") ~ "Land use",
                            str_detect(var, "region") ~ "Regional"),
         domain = factor(domain, levels = c("Technical","Environmental risk","Spatial/policy",
                                            "Social","Land use", "Regional"))) %>% 
  filter(domain %in% c("Technical","Environmental risk","Spatial/policy")) %>% 
  
  ggplot(aes(x = pe, y = vari, color = class)) +
  
  geom_vline(xintercept = 0, linetype = "dashed", size = 0.5, color = "gray50") +
  
  geom_errorbar(
    aes(xmin = pe - 1.96 * se, xmax = pe + 1.96 * se), 
    width = 0.5, size = 0.8,
    position = position_dodge(width = 0.6)
  ) +
  
  geom_point(
    aes(fill = class), 
    size = 2.5, pch = 21, color = "white",
    position = position_dodge(width = 0.6)
  ) +
  
  facet_wrap(~ domain, scales = "free") +
  
  scale_color_manual(
    name = "Project Status:",
    values = c("Operational" = "#003f5c",  # Dark Blue
               "Substation"  = "#4f7aa6",  # Medium Blue
               "Queue"       = "#ffa600")   # Contrasting Orange
  ) +
  scale_fill_manual(
    name = "Project Status:",
    values = c("Operational" = "#003f5c",  # Dark Blue
               "Substation"  = "#4f7aa6",  # Medium Blue
               "Queue"       = "#ffa600")   # Contrasting Orange
  ) +
  
  # scale_x_continuous(
  #   trans = pseudo_log_trans(base = 10, sigma = 0.1),
  #   # Define clear, interpretable breaks for the odds ratio.
  #   # breaks = c(0.2, 0.5, 1, 2, 5),
  #   labels = label_number(accuracy = 0.1)
  # ) +
  
  labs(
    x = "Odds ratio (log scale)",
    y = "", # Y-axis title is removed as variable names are self-explanatory.
    title = ""
  ) +
  
  theme_minimal(base_size = 14, base_family = "Franklin Gothic Book") +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(), # Remove horizontal grid lines for a cleaner look.
    panel.spacing = unit(1.5, "lines"), # Add space between facet panels.
    
    strip.background = element_rect(fill = "gray90", color = NA),
    strip.text = element_text(color = 'black', face = "bold", size = 12),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 12),
    
    axis.text = element_text(color = "black", size = 12),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    
    plot.title = element_text(face = "bold", size = 18, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 14, color = "gray30", margin = margin(b = 15))
  )


ggsave("./fig/f1.png", f1, width = 12, height = 5)



# s_plot <- r_plot(s_d) +
#   theme(legend.position = "bottom")


f2a <- prediction_plot(new_results[[1]], s_pts, "Operational")
f2b <- prediction_plot(new_results[[3]], inter_pts, "Queue")


### probability change trend
f2c <- ggplot() +
  geom_sf(data = polygons_vect, fill = "gray50", color = NA) +
  geom_spatraster(data = df_results, aes(fill = pred_logReg_s_queue)) +
  geom_sf(data = polygons_vect, fill = NA, color = "black", linewidth = 0.5) +
  # Use the same scales for consistency, but legend is hidden here.
  scale_fill_distiller(palette = "RdBu", name = "Probability   \ndifference ",
                       na.value = NA,
                       limits = c(-0.7, 0.7)) +      # Emphasizes extremes, compresses center
  
  # scale_fill_gradient2(
  #   low = "blue",
  #   mid = "yellow",
  #   high = "red",
  #   midpoint = 0,
  #   name = "Probability \ndifference",
  #   na.value = NA,
  #   limits = c(-0.5, 0.5)  # Adjust based on your data range
  # ) +

  # scale_color_manual(
  #   name = "",
  #   values = c("Sites" = "red")
  # ) +
  labs(title = "Difference (Queue - Operational)") +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(), # Remove horizontal grid lines for clarity.
    panel.grid = element_blank(),
    panel.spacing = unit(1.5, "lines"),
    
    # Style facet labels.
    strip.background = element_rect(fill = "white", color = NA),
    strip.text = element_text(color = 'black', face = "bold", size = 10),
    
    # Legend position and styling.
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    
    # Axis styling.
    # axis.text = element_text(color = "black", size = 10),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    
    # Title and subtitle styling.
    plot.title = element_text(face = "bold", size = 18, margin = margin(b = 5), hjust = 0.5),
    plot.subtitle = element_text(size = 14, color = "gray30", margin = margin(b = 15)),
  )



f2d <- rst_plot1(f3_glm, f3_glm_sub) +
  labs(x = "Probability difference (%)")



f2 <- ggarrange(
  ggarrange(  ggarrange(f2a,f2b, nrow = 2),
              f2c, nrow = 2, heights = c(2,1)),
 
  f2d,
  widths = c(1,0.8),
  nrow = 1,
  labels = c("A", "B"),  # Adds labels to plots
  label.x = 0,        # Adjust horizontal position of labels
  label.y = 1,        # Adjust vertical position of labels
  # vjust = 1,
  # hjust = -1,
  font.label = list(size = 14, face = "bold"))

ggsave("./fig/f2.png", f2, width = 12, height = 10)



s_plot <- s_d %>% 
  ggplot(
    aes(x = R_effect*100, y = region)
  ) +
  
  # A vertical line at x=1 (Odds Ratio of 1) indicates no effect.
  geom_vline(xintercept = 0, linetype = "dashed", size = 0.5, color = "gray50") +
  
  geom_pointrangeh(
    aes(xmin = (R_effect - SE)*100, xmax = (R_effect + SE)*100),
    position = position_dodge2v(height = 0.6, reverse = TRUE), 
    fatten = 2, 
    size = 0.7
  ) +
  
  geom_point(
    size = 2.5, pch = 21, color = "white",
    position = position_dodge2v(height = 0.6, reverse = TRUE)
  ) +
  
  # Facet by the main variable, allowing x-axes to vary if needed.
  facet_wrap(~variable, scales = "free_x", nrow = 4) +
  
  # # Use a pseudo-log scale to handle values on both sides of 1 gracefully.
  # scale_x_continuous(
  #   trans = pseudo_log_trans(base = 10, sigma = 0.1),
  #   # breaks = c(0.2, 0.5, 1, 2, 5) # Set clear, interpretable breaks
  # ) +
  
  labs(
    x = "Probability difference (%)",
    y = "", # Y-axis title is removed as region names are self-explanatory.
    title = ""
  ) +
  
  # --- Theming for a Clean, Professional Look ---
  theme_minimal(base_size = 14, base_family = "Franklin Gothic Book") +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(), # Remove horizontal grid lines for clarity.
    panel.spacing = unit(1.5, "lines"),
    
    # Style facet labels.
    strip.background = element_rect(fill = "gray90", color = NA),
    strip.text = element_text(color = 'black', face = "bold", size = 12),
    
    # Legend position and styling.
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    
    # Axis styling.
    axis.text = element_text(color = "black", size = 12),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    
    # Title and subtitle styling.
    plot.title = element_text(face = "bold", size = 18, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 14, color = "gray30", margin = margin(b = 15))
  )

s_map <- rgn %>% 
  mutate(region = recode(region, 
                         "mtwest" = "Mtwest",
                         "midwest" = "Midwest",
                         "northeast" = "Northeast",
                         "south" = "South",
                         "texas" = "Texas",
                         "west" = "West")) %>% 
  left_join(s_d, by = "region")  
var <- c("Capacity factor", "Hail", "Wildfire", "Energy community")

s_m <- mping(s_map %>% 
               mutate(R_effect = R_effect*100), var, "")


f3 <- ggarrange(s_m, s_plot, widths = c(3,1.7), nrow = 1,
                labels = c("A", "B"),  # Adds labels to plots
                label.x = 0,        # Adjust horizontal position of labels
                label.y = 1,        # Adjust vertical position of labels
                # vjust = 1,
                # hjust = -1,
                font.label = list(size = 14, face = "bold"))

ggsave("./fig/f3.png", f3, width = 12, height = 10)



### capacity 
f4a <- solar_queue %>%
  na.omit() %>% 
  mutate(capacity_bin = cut(capacity_mw,
                            breaks = c(1, 50, 200, 500, 1000, Inf),
                            labels = c("1–50", "51–200", "201–500", "501–1000", "1000+"),
                            right = FALSE)) %>%
  ggplot() +
  geom_sf(data = rgn, color = NA, fill = "gray85") + # US border
  geom_sf(aes(color = capacity_bin, size = capacity_bin), alpha = 0.7) +
  geom_sf(data = rgn, color = "black", fill = NA, linewidth = 0.7) + # US border
  # scale_color_brewer(palette = "RdYlBu", direction = -1, name = "Capacity (MW)") +
  scale_color_viridis_d(option = "plasma") +
  scale_size_manual(
    values = c(0.2, 1, 3, 4, 5)
  ) +

  theme_minimal() +
  labs(title = "Capacity (MW)", fill = "", size = "", color = "") +
  coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), 
           ylim = c(-2300000,730000), expand = FALSE, datum = NA) +
  theme_publication()



f4b <- solar_queue %>%
  na.omit() %>% 
  mutate(status = ifelse(status == "Late", "Late phase", "Early phase")) %>% 
  mutate(status = factor(status, levels = c("Late phase","Early phase"))) %>% 
  ggplot() +
  geom_sf(data = rgn, color = NA, fill = "white") + # US border
  geom_sf(aes(color = status), alpha = 0.8, size = 1.5) +
  geom_sf(data = rgn, color = "black", fill = NA, linewidth = 0.7) + # US border
  # scale_color_brewer(palette = "RdYlBu", direction = -1, name = "Capacity (MW)") +
  
  scale_color_manual(
    name = "",
    values = c("Early phase" = "#5D6D7E", "Late phase" = "red")
  ) +
  
  
  theme_minimal() +
  labs(title = "Project phase", fill = "", size = "", color = "") +
  coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), 
           ylim = c(-2300000,730000), expand = FALSE, datum = NA) +
  theme_publication()


f4c <- solar_queue %>% # filter for utility scale (>1MW)
   mutate(capacity_bin = cut(capacity_mw,
                            breaks = c(1, 50, 200, 500, 1000, Inf),
                            labels = c("1–50", "51–200", "201–500", "501–1000", "1000+"),
                            right = FALSE)) %>%
  
  ggplot() +
  
  geom_sf(data = rgn, color = NA, fill = "white") + # US border
  geom_sf(aes(color = type, size = capacity_bin), alpha = 0.7) +
  geom_sf(data = rgn, color = "gray0", fill = NA, linewidth = 0.7) + # US border
  # Use the same scales for consistency, but legend is hidden here.
  scale_color_manual(values = c("Solar" = "#0077b6", "Solar+Battery" = "#ee9b00", 
                                "Other hybrid" = "brown")) +
  
  
  scale_size_manual(values = c(0.5, 1.5, 3, 4.5, 6)) +
  theme_minimal() +
  
  labs(title = "C                         Project type", fill = "", color = "", size = "") +
  coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), 
           ylim = c(-2300000,730000), expand = FALSE, datum = NA) +
  theme_publication() +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = -0.01, vjust = 5))+
  guides(size = "none") 


  
# tp <- sqrt(attr(ranef(f4_glm4, condVar=T)[[2]], "postVar"))*1.96
# f4d <- as.data.frame(ranef(f4_glm4)$status) %>% 
#   tibble::rownames_to_column("status") %>% 
#   dplyr::select(-`(Intercept)`) %>% 
#   
#   gather(variable, R_effect, -status) %>% 
#   
#   mutate(vari = rep(v_name[c(-1,-4,-6,-10:-24)],each = 2),
#          vari = fct_reorder(vari, R_effect)) %>% 
#   
#   mutate(SE = c(tp[2,2,],tp[3,3,],tp[4,4,],tp[5,5,],tp[6,6,],
#                 tp[7,7,],tp[8,8,],tp[9,9,],tp[10,10,])) %>% 
#   
#   mutate(status = factor(status, levels = c("Late","Early"))) %>% 
#   mutate(upper = R_effect+SE,
#          lower = R_effect-SE) %>% 
#   
#   mutate(domain = case_when(str_detect(variable, "tx|roads|landAcq|cf|slope") ~ "Technical",
#                             str_detect(variable, "env|hail|fire") ~ "Environmental risk",
#                             str_detect(variable, "rps|lag|community") ~ "Spatial/policy"),
#          domain = factor(domain, levels = c("Technical","Environmental risk","Spatial/policy"))) %>% 
#   ggplot(
#     aes(x = R_effect, y = vari, color = status)
#   ) +
#   
#   geom_vline(xintercept = 0, linetype = "dashed", size = 0.5, color = "gray50") +
#   
#   geom_errorbarh(
#     aes(xmin = lower, xmax = upper),
#     height = 0.4, size = 0.8,
#     position = position_dodge(width = 0.5)
#   ) +
#   geom_point(
#     size = 3,
#     position = position_dodge(width = 0.5)
#   ) +
# 
#   facet_wrap(~domain, scales = "free_y", ncol = 1) +
#   
#   scale_color_manual(
#     name = "Phase  ",
#     values = c("Early" = "#5D6D7E", "Late" = "red")
#   ) +
#   
#   labs(
#     x = "Effect on project capacity (MW)",
#     y = ""
#   ) +
#   
#   theme_minimal(base_size = 14, base_family = "Franklin Gothic Book") +
#   theme(
#     panel.grid.minor = element_blank(),
#     panel.grid.major.y = element_blank(), # Remove horizontal grid lines for a dot plot.
#     panel.spacing = unit(2, "lines"),
#     
#     # Style facet labels.
#     strip.background = element_rect(fill = "gray90", color = NA),
#     strip.text = element_text(color = 'black', face = "bold", size = 12),
#     
#     # Legend position and styling.
#     legend.position = "bottom",
#     legend.title = element_text(face = "bold"),
#     
#     # Axis styling - no more rotated text!
#     axis.text = element_text(color = "black", size = 12),
#     axis.title.x = element_text(size = 12, margin = margin(t = 10)),
#     
#     # Title and subtitle styling.
#     plot.title = element_text(face = "bold", size = 18, margin = margin(b = 5)),
#     plot.subtitle = element_text(size = 14, color = "gray30", margin = margin(b = 15))
#   )

name_lookup <- c(
  "roads" = "Road\ndistance",
  "landAcq" = "Land\nacquisition cost",
  "slope" = "Slope",
  "pop" = "Population\ndensity",
  "hail" = "Hail",
  "fire" = "Wildfire",
  "community" = "Energy\nCommunity",
  "lowincome" = "Percent\nlow-income",
  "minority" = "Percent\nminority",
  "unemploy" = "Percent\nunemployed",
  "lulc_forest" = "Forest",
  "lulc_grassland" = "Grassland",
  "lulc_shrubland" = "Shrubland",
  "lulc_riparian" = "Riparian",
  "lulc_sparse" = "Vegetated",
  "lulc_agriculture" = "Agriculture",
  "lulc_developed" = "Developed",
  "env" = "Environment\nscore",
  "cf" = "Capacity\nfactor",
  "lag" = "Spatial lag",
  "tx" = "Transmission\ndistance" # Mapping tx to Transmission per your list
)


tidy_results <- tidy(f4_glm4, conf.int = TRUE) %>%
  filter(effect == "fixed", term != "(Intercept)") %>%
  mutate(
    # Identify Phase
    Phase = if_else(str_detect(term, "Late"), "Late", "Early"),
    # Clean the raw term to get the base variable name
    raw_var = str_remove_all(term, ":statusLate|:statusEarly|statusLate:|statusEarly:"),
    # Map the raw name to your formatted v_name
    Variable = recode(raw_var, !!!name_lookup),
    # Assign Categories for faceting
    Category = case_when(
      raw_var %in% c("cf", "tx") ~ "Technical",
      raw_var %in% c("hail", "fire") ~ "Environmental risk",
      raw_var %in% c("community", "lag") ~ "Spatial/policy",
      TRUE ~ "Social/LULC"
    ),
    Category = factor(Category, levels = c("Technical","Environmental risk","Spatial/policy"))
  ) %>%
  filter(Category != "Social/LULC")


f4d <- ggplot(tidy_results, aes(x = estimate, y = reorder(Variable, estimate), color = Phase)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  # Use position_dodge to prevent the dots from overlapping
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), 
                 height = 0.2, position = position_dodge(width = 0.5)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  # Create the separate panels (Technical, Environmental, etc.)
  facet_wrap(~ Category, scales = "free_y",  nrow = 3) +
  # Formatting to match the image style
  scale_color_manual(values = c("Early" = "#5D6D7E", "Late" = "#FF0000")) +
  labs(
    x = "Effect on project capacity (MW)",
    y = NULL,
    color = "Phase    "
  ) +
    theme_minimal(base_size = 14, base_family = "Franklin Gothic Book") +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(), # Remove horizontal grid lines for a dot plot.
      panel.spacing = unit(2, "lines"),

      # Style facet labels.
      strip.background = element_rect(fill = "gray90", color = NA),
      strip.text = element_text(color = 'black', face = "bold", size = 12),

      # Legend position and styling.
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),

      # Axis styling - no more rotated text!
      axis.text = element_text(color = "black", size = 12),
      axis.title.x = element_text(size = 12, margin = margin(t = 10)),

      # Title and subtitle styling.
      plot.title = element_text(face = "bold", size = 18, margin = margin(b = 5)),
      plot.subtitle = element_text(size = 14, color = "gray30", margin = margin(b = 15))
    )


tp <- sqrt(attr(ranef(f4_glm4, condVar=T)[[1]], "postVar"))*1.96
f4e <- as.data.frame(ranef(f4_glm4)$region) %>% 
  tibble::rownames_to_column("region") %>% 
  dplyr::select(-`(Intercept)`) %>% 
  
  gather(variable, R_effect, -region) %>% 
  
  # mutate(vari = rep(v_name[c(-1,-6,-10:-24)],each = 2),
  #        vari = fct_reorder(vari, R_effect)) %>% 
  
  mutate(SE = c(tp[2,2,],tp[3,3,])) %>% 
  
  # mutate(status = factor(status, levels = c("Late","Early"))) %>% 
  mutate(upper = R_effect+SE,
         lower = R_effect-SE) %>% 
  mutate(region = factor(region, levels = rev(c("West","Mtwest","Midwest","Texas","South","Northeast")))) %>%
  mutate(variable = ifelse(variable == "typeOther hybrid", "Other hybrid", "Solar+Battery"),
         variable = factor(variable, levels = c("Solar+Battery","Other hybrid"))) %>% 
  mutate(subtitle = "Project type") %>% 
  
  
  ggplot(
    aes(x = R_effect, y = region, color = variable)
  ) +
  
  geom_vline(xintercept = 0, linetype = "dashed", size = 0.5, color = "gray50") +
  
  geom_errorbarh(
    aes(xmin = lower, xmax = upper),
    height = 0.4, size = 0.8,
    position = position_dodge(width = 0.5)
  ) +
  geom_point(
    size = 3,
    position = position_dodge(width = 0.5)
  ) +
  
  facet_wrap(~subtitle, scales = "free_y", ncol = 1) +
  
  scale_color_manual(values = c("Solar+Battery" = "#ee9b00", 
                                "Other hybrid" = "brown")) +
  labs(
    x = "Effect on project capacity (MW)",
    y = "",
    color = "",
    title = "D"
  ) +
  
  theme_minimal(base_size = 14, base_family = "Franklin Gothic Book") +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(), # Remove horizontal grid lines for a dot plot.
    panel.spacing = unit(2, "lines"),
    
    # Style facet labels.
    strip.background = element_rect(fill = "gray90", color = NA),
    strip.text = element_text(color = 'black', face = "bold", size = 12),
    
    # Legend position and styling.
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    
    # Axis styling - no more rotated text!
    axis.text = element_text(color = "black", size = 12),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    
    # Title and subtitle styling.
    plot.title = element_text(face = "bold", size = 14, margin = margin(b = 5), hjust = -0.26),
    plot.subtitle = element_text(size = 14, color = "gray30", margin = margin(b = 15))
  )


f4 <- ggarrange(
  ggarrange(f4a, f4b, f4c, heights = c(0.9,1,1.2), nrow = 3),
  ggarrange(f4d, f4e, nrow = 2, heights = c(3,1.5)),
  nrow = 1,
  widths = c(1,0.7),
  labels = c("A", "B"),  # Adds labels to plots
  label.x = 0,        # Adjust horizontal position of labels
  label.y = 1,        # Adjust vertical position of labels
  # vjust = 1,
  # hjust = -1,
  font.label = list(size = 14, face = "bold")
  
)

ggsave("./fig/f4.png", f4, width = 12, height = 12)


