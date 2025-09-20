all_matches <- read_csv("../trend1/data/final_output_word_match.csv")

solar <- all_matches %>% 
  filter(str_detect(lbnl_type,"Solar")&
           status == "active"&
           capacity_mw > 1 &
           match_confidence > 40) %>% 
  st_as_sf(coords = c("matched_sub_lon", "matched_sub_lat"), crs = 4269)

# mapview(solar)

sub_data <- read_csv("../data/raw_data/substation_tract_0.csv", show_col_types = FALSE) %>% 
  mutate(X = LONGITUDE,
         Y = LATITUDE) %>% 
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4269) %>% 
  filter(TYPE == "SUBSTATION") %>% 
  dplyr::select(STATE, X,Y,MAX_VOLT)

tt <- st_nearest_feature(solar, sub_data) # find the substation index nearest for each project
queue_sub <- sub_data[tt,]
queue_sub$capacity_mw <- solar$capacity_mw

queue_sub <- queue_sub %>% 
  group_by(STATE, X, Y, MAX_VOLT) %>% 
  summarise(count = n(),
            capacity_mw = sum(capacity_mw))

# mapview(queue_sub)

write_csv(queue_sub %>% 
            st_drop_geometry(), path = "./data/solar_inter.csv",
          append = FALSE)


vt <- queue_sub %>% 
  filter(MAX_VOLT > 0) 

vt$MAX_VOLT %>% mean()
