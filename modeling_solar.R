
#### solar
solar.cov.existing <- read_csv("./trend/data/existing_cov_solar.csv") 
# solar.cov.existing[, 13:20] <- t(apply(solar.cov.existing[, 13:20], 1, function(row) {
#   max_idx <- which(row == max(row))        # Find indices of maximum values
#   if(length(max_idx) > 1){
#     selected_idx <- sample(max_idx, 1)
#   }else{
#     selected_idx <- max_idx
#   }    # Randomly select one index if there are ties
#   as.integer(seq_along(row) == selected_idx) # Set 1 for the selected index, 0 otherwise
# }))
# solar.cov.existing[, 14:21] %>% rowSums() %>% sum()
# solar.cov.existing[, 22:27] %>% rowSums() %>% sum(., na.rm = T)

s.bg <- read_csv("./trend/data/bg_cov_solar.csv") 
# w.bg[, 13:20] <- t(apply(w.bg[, 13:20], 1, function(row) {
#   max_idx <- which(row == max(row))        # Find indices of maximum values
#   if(length(max_idx) > 1){
#     selected_idx <- sample(max_idx, 1)
#   }else{
#     selected_idx <- max_idx
#   }    # Randomly select one index if there are ties
#   as.integer(seq_along(row) == selected_idx) # Set 1 for the selected index, 0 otherwise
# }))
# w.bg[, 14:21] <- t(apply(w.bg[, 14:21], 1, function(row) as.integer(row == max(row))))
# w.bg[, 14:21] %>% rowSums() %>% sum()
# w.bg[, 22:27] %>% rowSums() %>% sum(., na.rm = T)

solar.cov.bg <- s.bg %>% 
  mutate(group = c(rep(seq(1,10,1), each = floor(nrow(s.bg)/10)),rep(10,8)))


# solar.cov.bg %>% 
#   group_by(group) %>% 
#   summarise_all(~ mean(., na.rm = TRUE)) %>% View


v_name <- c("(Intercept)",
            "Transmission dist",
            "Land acquisition",
            "Road dist", 
            "Slope",
            "Population density", 

            "Hail",
            "Fire",
            "Community",
            
            "Lowincome",
            "Minority",
            "Unemploy",
            
            "Forest", 
            "Grassland", 
            "Shrubland", 
            "Riparian", 
            "Sparsely vegetated", 
            "Agriculture", 
            "Developed", 
            
            "Northeast",
            "Midwest",
            "West",
            "South",
            "Texas", 
            
            "Environmental score",
            "Capacity factor",
            "Spatial lag")


### combined
s_dat <- solar.cov.bg %>% 
  dplyr::select(-group) %>% 
  rbind(solar.cov.existing) %>% 
  # scale
  mutate(across(
    .cols = setdiff(names(.), c("zone", "treat", names(.)[str_detect(names(.), "lulc|region")])),
    .fns = ~ scale(.) %>% as.numeric()
  )) %>%
  
  mutate(treat = factor(treat)) %>% 
  na.omit()


glm1_solar <- glm(treat ~ tx + landAcq + roads + slope + pop + hail + fire + community + lowincome + minority + unemploy +  
                    lulc_forest + lulc_grassland + lulc_shrubland + lulc_riparian + lulc_sparse + lulc_agriculture + lulc_developed + lulc_other +
                    region_ne + region_mw + region_w + region_s + region_tex + region_mtw +
                    env + cf + lag, 
                  data=s_dat, family = binomial(link="logit"))

rst <- summary(glm1_solar)$coefficients %>% 
  as.data.frame() %>% 
  mutate(var = v_name) %>% 
  mutate(color = ifelse(`Pr(>|z|)` < 0.05, "Y", "N")) %>%
  dplyr::select(-"z value",-"Pr(>|z|)") %>% 
  # tibble::rownames_to_column("var") %>% 
  filter(!var == "(Intercept)") %>% 
  rename(se = "Std. Error",
         pe = Estimate)

rst_ordered <- rst %>%
  mutate(color = factor(color, levels = c("Y", "N")),
         var = fct_reorder(var, exp(pe)))  # Order 'var' based on 'exp(pe)'

# Check the order of the variable
ordered_levels <- levels(rst_ordered$var)

s_reg <- rst_ordered %>% 
  ggplot(aes(x = exp(pe), y = var)) +
  geom_vline(xintercept = 1,linetype = "dashed", size = 0.5, color = "gray30") +
  geom_errorbar(aes(xmin=exp(pe-1.96*se), xmax=exp(pe+1.96*se)),color="gray50", width = 0.5) +
  geom_point(aes(fill = color),size = 2,pch=21) +
  theme_bw() +
  scale_x_continuous(
    trans = scales::pseudo_log_trans(base = 10, sigma = 0.2),  # Pseudo-log transformation
    breaks = c(0, 1, 4, 13),                     # Custom breaks
    labels = scales::label_number(accuracy = 0.1)              # Format labels
  )  +
  
  # scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
  #               labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  
  labs(fill = "Significant", y = "", x = "Odds ratio", title = "") +
  scale_fill_manual(values=c("red", "gray")) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background =element_rect(fill="gray22",color="gray22"),
        strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=12),
        legend.position = "none",
        axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=6),
        axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=9),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=11),
        plot.title=element_text(family="Franklin Gothic Demi", size=16, hjust = -0.09)) 


#### solar interconnection
solar.cov.existing.inter <- read_csv("./trend/data/existing_cov_solar_inter.csv")
#   mutate(lulc_sparse = ifelse(lulc_sparse > 0.4, 1, 0))
# 
# solar.cov.existing.inter[, 13:20] <- t(apply(solar.cov.existing.inter[, 13:20], 1, function(row) {
#   max_idx <- which(row == max(row))        # Find indices of maximum values
#   if(length(max_idx) > 1){
#     selected_idx <- sample(max_idx, 1)
#   }else{
#     selected_idx <- max_idx
#   }    # Randomly select one index if there are ties
#   as.integer(seq_along(row) == selected_idx) # Set 1 for the selected index, 0 otherwise
# }))
# solar.cov.existing.inter[, 15:22] %>% rowSums() %>% sum()
# solar.cov.existing.inter[, 23:28] %>% rowSums() %>% sum(., na.rm = T)

s.bg.inter <- read_csv("./trend/data/bg_cov_solar_inter.csv")
#   filter(!is.na(fire))
# w.bg.inter[, 13:20] <- t(apply(w.bg.inter[, 13:20], 1, function(row) {
#   max_idx <- which(row == max(row))        # Find indices of maximum values
#   if(length(max_idx) > 1){
#     selected_idx <- sample(max_idx, 1)
#   }else{
#     selected_idx <- max_idx
#   }    # Randomly select one index if there are ties
#   as.integer(seq_along(row) == selected_idx) # Set 1 for the selected index, 0 otherwise
# }))

# w.bg.inter[, 15:22] <- t(apply(w.bg.inter[, 15:22], 1, function(row) as.integer(row == max(row))))
# w.bg.inter[, 15:22] %>% rowSums() %>% sum()
# w.bg.inter[, 23:28] %>% rowSums() %>% sum(., na.rm = T)

solar.cov.bg.inter <- s.bg.inter %>% 
  mutate(group = c(rep(seq(1,10,1), each = floor(nrow(s.bg.inter)/10)),rep(10,1))) # modify the last rep(10,x)


# solar.cov.bg.inter %>% 
#   group_by(group) %>% 
#   summarise_all(~ mean(., na.rm = TRUE)) %>% View


  s_dat_inter <- solar.cov.bg.inter %>% 
    dplyr::select(-group) %>% 
    rbind(solar.cov.existing.inter) %>% 
    
    # scale
    mutate(across(
      .cols = setdiff(names(.), c("zone", "treat", names(.)[str_detect(names(.), "lulc|region")])),
      .fns = ~ scale(.) %>% as.numeric()
    )) %>%
    
    mutate(treat = factor(treat)) %>% 
    na.omit()
  
  glm1_solar <- glm(treat ~ tx + landAcq + roads + slope + pop + hail + fire + community + lowincome + minority + unemploy + 
                      lulc_forest + lulc_grassland + lulc_shrubland + lulc_riparian + lulc_sparse + lulc_agriculture + lulc_developed + lulc_other +
                      region_ne + region_mw + region_w + region_s + region_tex + region_mtw +
                      env + cf + lag, 
                    data=s_dat_inter, family = binomial(link="logit"))
  
  rst_inter <- summary(glm1_solar)$coefficients %>% 
    as.data.frame() %>% 
    mutate(var = v_name) %>% 
    mutate(color = ifelse(`Pr(>|z|)` < 0.05, "Y", "N")) %>%
    dplyr::select(-"z value",-"Pr(>|z|)") %>% 
    # tibble::rownames_to_column("var") %>% 
    filter(!var == "(Intercept)") %>% 
    rename(se = "Std. Error",
           pe = Estimate)

rst_ordered_inter <- rst_inter %>%
  mutate(color = factor(color, levels = c("Y", "N")),
         var = fct_reorder(var, exp(pe)))  # Order 'var' based on 'exp(pe)'

# Check the order of the variable
ordered_levels <- levels(rst_ordered_inter$var)

s_reg_inter <- rst_ordered_inter %>% 
  ggplot(aes(x = exp(pe), y = var)) +
  geom_vline(xintercept = 1,linetype = "dashed", size = 0.5, color = "gray30") +
  geom_errorbar(aes(xmin=exp(pe-1.96*se), xmax=exp(pe+1.96*se)),color="gray50", width = 0.5) +
  geom_point(aes(fill = color),size = 2,pch=21) +
  theme_bw() +
  scale_x_continuous(
    trans = scales::pseudo_log_trans(base = 10, sigma = 0.2),  # Pseudo-log transformation
    breaks = c(0, 1, 4, 13),                     # Custom breaks
    labels = scales::label_number(accuracy = 0.1)              # Format labels
  )  +
  
  # scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
  #               labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  
  labs(fill = "Significant", y = "", x = "Odds ratio", title = "") +
  scale_fill_manual(values=c("red", "gray")) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background =element_rect(fill="gray22",color="gray22"),
        strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=12),
        legend.position = "none",
        axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=6),
        axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=9),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=11),
        plot.title=element_text(family="Franklin Gothic Demi", size=16, hjust = -0.09))


### solar sub
solar.cov.existing.sub <- read_csv("./trend/data/existing_cov_solar_sub.csv")
# solar.cov.existing.sub[, 13:20] <- t(apply(solar.cov.existing.sub[, 13:20], 1, function(row) {
#   max_idx <- which(row == max(row))        # Find indices of maximum values
#   if(length(max_idx) > 1){
#     selected_idx <- sample(max_idx, 1)
#   }else{
#     selected_idx <- max_idx
#   }    # Randomly select one index if there are ties
#   as.integer(seq_along(row) == selected_idx) # Set 1 for the selected index, 0 otherwise
# }))
# solar.cov.existing.sub[, 15:22] %>% rowSums() %>% sum()
# solar.cov.existing.sub[, 23:28] %>% rowSums() %>% sum(., na.rm = T)

s.bg.sub <- read_csv("./trend/data/bg_cov_solar_sub.csv")
# w.bg.sub[, 13:20] <- t(apply(w.bg.sub[, 13:20], 1, function(row) {
#   max_idx <- which(row == max(row))        # Find indices of maximum values
#   if(length(max_idx) > 1){
#     selected_idx <- sample(max_idx, 1)
#   }else{
#     selected_idx <- max_idx
#   }    # Randomly select one index if there are ties
#   as.integer(seq_along(row) == selected_idx) # Set 1 for the selected index, 0 otherwise
# }))
# w.bg.sub[, 15:22] <- t(apply(w.bg.sub[, 15:22], 1, function(row) as.integer(row == max(row))))
# w.bg.sub[, 15:22] %>% rowSums() %>% sum()
# w.bg.sub[, 23:28] %>% rowSums() %>% sum(., na.rm = T)

solar.cov.bg.sub <- s.bg.sub %>% 
  mutate(group = c(rep(seq(1,10,1), each = floor(nrow(s.bg.sub)/10)),rep(10,0))) # modify the last rep(10,x)


# solar.cov.bg.sub %>% 
#   group_by(group) %>% 
#   summarise_all(~ mean(., na.rm = TRUE)) %>% View


  s_dat_sub <- solar.cov.bg.sub %>% 
    dplyr::select(-group) %>% 
    rbind(solar.cov.existing.sub) %>% 
    
    mutate(across(
      .cols = setdiff(names(.), c("zone", "treat", names(.)[str_detect(names(.), "lulc|region")])),
      .fns = ~ scale(.) %>% as.numeric()
    )) %>%
    
    mutate(treat = factor(treat)) %>% 
    na.omit()
  
  glm1_solar <- glm(treat ~ tx + landAcq + roads + slope + pop + hail + fire + community + lowincome + minority + unemploy +
                      lulc_forest + lulc_grassland + lulc_shrubland + lulc_riparian + lulc_sparse + lulc_agriculture + lulc_developed + lulc_other +
                      region_ne + region_mw + region_w + region_s + region_tex + region_mtw +
                      env + cf + lag, 
                    data=s_dat_sub, family = binomial(link="logit"))
  
  rst_sub <- summary(glm1_solar)$coefficients %>% 
    as.data.frame() %>% 
    mutate(var = v_name) %>% 
    mutate(color = ifelse(`Pr(>|z|)` < 0.05, "Y", "N")) %>%
    dplyr::select(-"z value",-"Pr(>|z|)") %>% 
    # tibble::rownames_to_column("var") %>% 
    filter(!var == "(Intercept)") %>% 
    rename(se = "Std. Error",
           pe = Estimate)

rst_ordered_sub <- rst_sub %>%
  mutate(color = factor(color, levels = c("Y", "N")),
         var = fct_reorder(var, exp(pe)))  # Order 'var' based on 'exp(pe)'

# Check the order of the variable
ordered_levels <- levels(rst_ordered_sub$var)

s_reg_sub <- rst_ordered_sub %>% 
  ggplot(aes(x = exp(pe), y = var)) +
  geom_vline(xintercept = 1,linetype = "dashed", size = 0.5, color = "gray30") +
  geom_errorbar(aes(xmin=exp(pe-1.96*se), xmax=exp(pe+1.96*se)),color="gray50", width = 0.5) +
  geom_point(aes(fill = color),size = 2,pch=21) +
  theme_bw() +
  scale_x_continuous(
    trans = scales::pseudo_log_trans(base = 10, sigma = 0.2),  # Pseudo-log transformation
    breaks = c(0, 1, 4, 13),                     # Custom breaks
    labels = scales::label_number(accuracy = 0.1)              # Format labels
  )  +
  
  # scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
  #               labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  
  labs(fill = "Significant", y = "", x = "Odds ratio", title = "") +
  scale_fill_manual(values=c("red", "gray")) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background =element_rect(fill="gray22",color="gray22"),
        strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=12),
        legend.position = "none",
        axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=6),
        axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=9),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=11),
        plot.title=element_text(family="Franklin Gothic Demi", size=16, hjust = -0.09)) 


save(solar.cov.bg.inter, solar.cov.existing.inter, 
     solar.cov.bg.sub, solar.cov.existing.sub, 
     solar.cov.bg, solar.cov.existing,
     file = "./trend/data/model_data.RData")
load("./trend/data/model_data.RData")
