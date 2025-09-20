if (!require(librarian)){
  install.packages("librarian")
  library(librarian)
}

librarian::shelf("raster", "sf", "tidyverse", "terra", "here", "tictoc", "foreach", "doParallel", "foreign", "dplyr","tigris",
                 "stargazer", "caret", "tidycensus", "ggpubr", "tidyterra", "gridExtra", "rasterVis", "RColorBrewer", "grid",
                 "lme4", "lmerTest", "ggstance", "cowplot", "mapview")
sf_use_s2(FALSE)
library(ggnewscale)
ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)

# data
new_results <- rast("./data/trend_surface.tif") # prob surfaces
load("./data/trend_data.RData") 
# rgn, solar_que, solar_queue, s, s_sub, solar_inter, s_dat_compare, s_d,
# sampled_data, sampled_data_sub,sampled_cap,
# f3_glm,f3_glm_sub,f4_glm1,f4_glm2,




### f0 mapping
s_inter <- solar_inter %>% 
  st_buffer(7000) %>% 
  dplyr::rename(state = STATE,
                capacity = capacity_mw) %>% 
  dplyr::select(state, capacity) %>% 
  mutate(class = "Queue")


s_com <- rbind(s, s_sub, s_inter) %>% 
  mutate(class = factor(class, levels = c("Operational","Substation","Queue")))

northeast <- c("CT","ME", "NH", "RI", "VT", "MA", "NY", "PA", "DE", "NJ", "MD")
midwest <- c("ND", "SD", "NE", "KS", "MN", "IA", "MO", "WI", "IL", "MI", "IN", "OH", "OK") #moved OK to midwest
west <- c("CA", "OR", "WA", "ID", "UT", "AZ", "NM", "NV")
south <- c("AK", "LA", "AL", "GA", "FL", "SC", "NC", "VA", "WV", "KY", "TN", "MS", "AR")
texas <- c("TX")
mtwest <- c("CO", "WY", "MT")


# f1
rst <- function(data, scene){
  
  glm1_solar <- glm(treat ~ tx + landAcq + roads + slope + pop + hail + fire + community + lowincome + minority + unemploy +
                      lulc_forest + lulc_grassland + lulc_shrubland + lulc_riparian + lulc_sparse + lulc_agriculture + lulc_developed + lulc_other +
                      region +
                      env + cf + lag, 
                    data=data %>% 
                      filter(class == scene), family = binomial(link="logit"))
  
  rst <- summary(glm1_solar)$coefficients %>% 
    as.data.frame() %>% 
    # mutate(var = v_name) %>% 
    mutate(color = ifelse(`Pr(>|z|)` < 0.05, "Y", "N")) %>%
    dplyr::select(-"z value",-"Pr(>|z|)") %>% 
    tibble::rownames_to_column("var") %>%
    # filter(!var == "(Intercept)") %>% 
    rename(se = "Std. Error",
           pe = Estimate) %>%
    mutate(color = factor(color, levels = c("Y", "N"))) %>%  # Order 'var' based on 'exp(pe)'
    mutate(class = scene)
  
  return(rst)
}


v_name <- c("(Intercept)",
            "Transmission\ndistance",
            "Land\nacquisition cost",
            "Road\ndistance", 
            # "Substation dist",
            "Slope",
            "Population\ndensity", 
            
            "Hail",
            "Wildfire",
            "Energy\nCommunity",
            
            "Percent\nlow-income",
            "Percent\nminority", 
            "Percent\nunemployed",

            "Forest", 
            "Grassland", 
            "Shrubland", 
            "Riparian", 
            "Vegetated", 
            "Agriculture", 
            "Developed", 

            "Midwest",
            "Northeast",
            "South",
            "Texas",
            "West",

            "Environment\nscore",
            "Capacity\nfactor",
            "Spatial lag")



r_plot <- function(data){
  # if(tech == "solar"){
  #   name <- "Utility-scale solar"
  # }else{
  #   name <- "Land-based solar"
  # }
  
  plot <- data %>% 
    ggplot(aes(x = exp(R_effect), y = region, color = class, 
               xmin=exp(R_effect-SE), xmax=exp(R_effect+SE))) +
    geom_vline(xintercept = 1,linetype = "dashed", size = 0.5, color = "gray30") +
    
    geom_pointrangeh(position = position_dodge2v(height = 0.4), fatten = 2, size = 0.7) +
    
    facet_wrap(~variable, scales = "free_x", nrow = 4) +
    theme_bw() +
    
    scale_x_continuous(
      trans = scales::pseudo_log_trans(base = 10, sigma = 0.2)  # Pseudo-log transformation

    )  +
    
    labs(x = "Odds ratio", y ="", color = "",
         title = "") +
    
    scale_fill_manual(values=c("red", "gray")) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          strip.background =element_rect(fill="gray22",color="gray22"),
          strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=12, face = "bold"),
          legend.position = "right",
          axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=10),
          axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=12),
          axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=12),
          plot.title=element_text(family="Franklin Gothic Demi", size=20)) 
  
  return(plot)
  
}


mping <- function(data, var, tech){
  maps <- map(.x = var,
              .f = function(x) data %>%
                filter(variable == x) %>%
                
                ggplot() +
                geom_sf(fill = "white", color = "gray0") + # US border
                geom_sf(aes(fill = exp(R_effect)), size = 0.3) +
                
                theme_minimal() +
                scale_fill_gradient2(low = if (x %in% c("Hail", "Wildfire")) "brown" else "cornflowerblue", 
                                     mid = "white", 
                                     high = if (x %in% c("Hail", "Wildfire")) "cornflowerblue" else "brown",
                                     midpoint = 1,
                                     labels = scales::label_number(accuracy = 0.1)) +
                facet_wrap(~class, nrow = 1) +
                
                labs(title = x, fill = "") +
                coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), 
                         ylim = c(-2300000,730000), expand = FALSE, datum = NA) +
                theme(legend.position = "right",
                      plot.title = element_text(hjust = 0.1))
  )
  
  tp <- plot_grid(plotlist = maps, labels = tech, label_size = 14, label_fontface = "plain", nrow = 4, hjust = -0.2)
  
  return(tp)
}

# f3

s_pts <- s %>% 
  vect()
inter_pts <- s_inter %>% 
  vect()

polygons_vect <- rgn %>% 
  mutate(id = 1:6) %>% 
  st_transform(crs = st_crs(s)) %>% 
  terra::vect()

### function
prediction_plot <- function(data1, data2, class){
  
  ggplot() +
    # Use geom_spatraster to plot the SpatRaster object.
    # The `fill` aesthetic maps the raster values to a color scale.
    geom_sf(data = polygons_vect, fill = "gray50", color = NA) +
    geom_spatraster(data = data1, aes(fill = !!sym(names(data1)))) +
    # The points from the first red source
    # geom_sf(data = data2, 
    #         aes(color = "Sites"), fill = "red") +
    # The yellow polygon border
    geom_sf(data = polygons_vect, fill = NA, color = "white", linewidth = 0.5) +
    # The `fill` scale is for the raster, the `color` scale is for the points.
    scale_fill_viridis_c(labels = scales::label_number(accuracy = 0.1),
                         na.value = NA) + # Customize this as needed
    # scale_color_manual(
    #   name = "",
    #   values = c("Sites" = "red")
    # ) +
    labs(title = class, fill = "Probability  ") +
    theme_minimal() +
    
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(hjust = 0.5),
      legend.position = "bottom" # Hide the legend on individual plots
    ) 
  
}


rst_plot <- function(result){
  rst_cap <- summary(result)$coefficients %>% 
    as.data.frame() %>% 
    # mutate(var = v_name) %>% 
    mutate(color = ifelse(`Pr(>|t|)` < 0.05, "Significant", "Non-significant")) %>%
    dplyr::select(-"t value",-"Pr(>|t|)") %>% 
    tibble::rownames_to_column("var") %>%
    # filter(!var == "(Intercept)") %>% 
    rename(se = "Std. Error",
           pe = Estimate) %>%
    mutate(color = factor(color, levels = c("Significant", "Non-significant")))  # Order 'var' based on 'exp(pe)'
  
  p <- rst_cap %>% 
    mutate(vari = v_name,
           vari = fct_reorder(vari, pe)) %>% 
    filter(!var == "(Intercept)") %>%
    mutate(domain = case_when(str_detect(var, "tx|roads|landAcq|cf|slope") ~ "Technical",
                              str_detect(var, "env|hail|fire") ~ "Environmental risk",
                              str_detect(var, "lowincome|minority|unemploy|pop") ~ "Social",
                              str_detect(var, "lag|community") ~ "Spatial/policy",
                              str_detect(var, "lulc") ~ "Land use",
                              str_detect(var, "region") ~ "Regional"),
           domain = factor(domain, levels = c("Technical","Environmental risk","Spatial/policy",
                                              "Social","Land use", "Regional"))) %>% 
    
    ggplot(aes(x = vari, y = pe, color = color)) +
    geom_hline(yintercept = 0,linetype = "dashed", size = 0.5, color = "gray30") +
    geom_errorbar(aes(ymin=pe-1.96*se, ymax=pe+1.96*se), width = 0.3, size = 0.7,
                  position = position_dodge(width = 0.9)) +
    geom_point(aes(fill = color),size = 2,pch=21,
               position = position_dodge(width = 0.9)) +
    facet_wrap(~domain, scales = "free") +
    theme_bw() +
    scale_y_continuous(
      # trans = scales::pseudo_log_trans(base = 10, sigma = 0.2),  # Pseudo-log transformation
      # breaks = c(0, 1, 2, 4),                     # Custom breaks
      labels = scales::label_number(accuracy = 0.01)              # Format labels
    )  +
    
    # scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
    #               labels = scales::trans_format("log10", scales::math_format(10^.x))) +
    
    labs(fill = "", x = "", y = "Capacity (MW)", title = "", color = "") +
    # scale_fill_manual(values=c("red", "gray")) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          strip.background =element_rect(fill="gray22",color="gray22"),
          strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=10, face = "bold"),
          legend.position = "bottom",
          axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=8,
                                     angle = 45, hjust = 1),
          axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=12),
          axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=12),
          plot.title=element_text(family="Franklin Gothic Demi", size=16, hjust = -0.09, face = "bold"))
  
  return(p)
  
}



# f4
df_results <- new_results[[3]] - new_results[[1]]
df_results_sub <- new_results[[3]] - new_results[[2]] # queue - sub




