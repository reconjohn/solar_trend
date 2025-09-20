# current PV
solar <- st_read("../data/raw_data/uspvdb_v2_0_20240801.geojson") %>% 
  dplyr::select(unique_id = FID,  # select id column
                operating_year = p_year, # select install date
                state_code = p_state, # select US-state
                nameplate_capacity = p_cap_ac, # select MW column
                geometry) %>%
  filter(nameplate_capacity >= 1) %>% # filter for utility scale (>1MW)
  filter(operating_year >= 2017) %>% 
  mutate(area_m2 = st_area(.),
         radius_m = sqrt(area_m2/pi)) %>% 
  filter(!state_code %in% c("HI","AK"))


# substation
hif <- read_csv("../data/raw_data/substation_tract_0.csv") %>% 
  filter(TYPE == "SUBSTATION") %>% 
  filter(STATUS == "IN SERVICE") %>% 
  filter(!STATE_ABBR %in% c("HI","AK")) %>% 
  mutate(X = LONGITUDE,
         Y = LATITUDE) %>% 
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326) %>% 
  dplyr::select(NAME, STATUS,STATE,STCOFIPS,COUNTY,CITY,X,Y, MAX_VOLT) %>% 
  rename(fips = STCOFIPS) %>% 
  mutate(NAME = str_to_lower(NAME),
         CITY = str_to_lower(CITY),
         COUNTY = str_to_lower(COUNTY))


tt <- st_nearest_feature(solar, hif) # find the substation index nearest for each PV project
solar_sub <- hif[tt,] %>%  # identify nearest substations
  distinct()


write_csv(solar_sub, path = "./data/solar_sub.csv",
          append = FALSE)


#################################
# identifying queue matched substation for voltage confirm
vt <- solar_sub %>% 
  filter(MAX_VOLT > 0) 

vt$MAX_VOLT %>% mean()
