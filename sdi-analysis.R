
library(tidyverse)
library(dplyr)
library(sf)
library(Hmisc)
library(arrow)
library(sfarrow)
library(geohashTools)
library(scales)
#library(nngeo)
library(lwgeom)
library(patchwork)
library(boot)

options(scipen = 99999)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
source('aggregation_func.R') # should be in the ~/kblock-analysis directory

# Box link -- require special permissions from SDI
# https://uchicago.box.com/s/tzmk3jhlra7ehbxsjfk6sfm6k1d3xnp9
dir.create('data/sdi-analysis')
dir.create('data/sdi-analysis/data')
dir.create('data/sdi-analysis/viz')

# Aggregate block level data ----------------------------------------------

if (!file.exists(paste0("data/africa_data.parquet"))) {
  curl::multi_download("dsbprylw7ncuq.cloudfront.net/AF/africa_data.parquet", "data/africa_data.parquet", resume = TRUE)
} else {
  df_combined <- read_parquet(paste0("data/africa_data.parquet"))
}

sum_cols = c("k_complexity_weighted_landscan_un", "k_complexity_weighted_worldpop_un", "landscan_population_un", "worldpop_population_un")

divide_cols = list('k_complexity_weighted_landscan_un' = c('k_complexity_weighted_landscan_un','landscan_population_un'),
                   'k_complexity_weighted_worldpop_un' = c('k_complexity_weighted_worldpop_un','worldpop_population_un'))

transform_cols = c("k_complexity_weighted_landscan_un", 'landscan_population_un', 'worldpop_population_un')

df_k <- generate_crosstabs(data =  df_combined %>% mutate(total = 'Total'), 
                           group_by_cols = c("urban_id", "urban_center_name", "class_urban_hierarchy"),
                           crosstab_cols = c("total"),
                           sum_cols = sum_cols, divide_cols = divide_cols, 
                           transform_cols = transform_cols, transform_func_list = list(log10 = log_10),
                           group_by_agg_cols_list = list('urban' = "urban_id"),
                           group_by_agg_func_list = list(sum = sum, share = share),
                           agg_cols = c('landscan_population_un', 'worldpop_population_un'))

urban0s <- df_k %>% filter(landscan_population_un == 0) %>% select(urban_id) %>% pull()

df_k_sdmad <- df_combined %>%
  filter(!(urban_id %in% urban0s )) %>%
  mutate(
    random_jitter = runif(n = nrow(.), min = 0, max = .01),
    k_complexity_jitter = k_complexity + random_jitter
  ) %>%
  group_by(urban_id) %>% 
  summarise(k_complexity_sd = Hmisc::wtd.var(x = k_complexity, weights = landscan_population_un, na.rm=TRUE),
            k_complexity_mad = matrixStats::weightedMad(x = k_complexity_jitter, w = landscan_population_un,  na.rm = TRUE),
            k_complexity_75 = Hmisc::wtd.quantile(x = k_complexity, weights = landscan_population_un, probs= .75, na.rm=FALSE),
            k_complexity_25 = Hmisc::wtd.quantile(x = k_complexity, weights = landscan_population_un, probs= .25, na.rm=FALSE)
            ) %>%
  ungroup()


df_k <- df_k %>%
  left_join(., df_k_sdmad, by = c('urban_id'='urban_id'))

df_k_total <- generate_crosstabs(data =  df_combined, 
                           group_by_cols = c("area_type"),
                           crosstab_cols = c("k_labels"),
                           sum_cols = sum_cols, divide_cols = divide_cols, 
                           transform_cols = transform_cols, transform_func_list = list(log10 = log_10),
                           group_by_agg_cols_list = list("area_type" = "area_type"),
                           group_by_agg_func_list = list(sum = sum, share = share),
                           agg_cols = c('landscan_population_un', 'worldpop_population_un')) %>%
  filter(area_type == 'Urban')

rm(df_combined)
gc()

# Load country boundaries -------------------------------------------------

filedir <- paste0(tempdir(), '/countries/')
unlink(filedir, recursive = TRUE)
dir.create(filedir)
country_shp <- paste0('https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/cultural/ne_110m_admin_0_countries.zip')
download.file(url = country_shp , destfile = paste0(filedir, basename(country_shp)))
unzip(paste0(filedir,basename(country_shp )), exdir= filedir)
list.files(path = filedir)
world <- st_read(fs::dir_ls(filedir, regexp = "\\.shp$")[1])

iso_code_list <- c( 'AGO', 'BDI', 'BEN', 'BFA', 'BWA', 'CAF', 'CIV', 'CMR', 'COD', 'COG', 'COM', 'CPV', 'DJI', 'ERI', 'ESH', 'ETH', 'GAB', 'GHA', 'GIN', 'GMB', 'GNB', 'GNQ', 'KEN', 'LBR', 'LSO', 'MDG', 'MLI', 'MOZ', 'MRT', 'MUS', 'MWI', 'NAM', 'NER', 'NGA', 'RWA', 'SDN', 'SEN', 'SLE', 'SOM', 'SSD', 'STP', 'SWZ', 'SYC', 'TCD', 'TGO', 'TZA', 'UGA', 'ZAF', 'ZMB', 'ZWE')

subsaharan_africa <- world %>%
  select(ISO_A3, NAME_EN, geometry) %>%
  filter(ISO_A3 %in% iso_code_list) %>%
  st_make_valid()
rm(world)

# Load settlement boundaries ----------------------------------------------

# Requies special access permissions from SDI
# https://uchicago.app.box.com/folder/235077315882
filedir = 'data/sdi-analysis/boundaries/CSV Files'
files_list <- fs::dir_ls(filedir, regexp = "\\.csv$")
files_list 

sdi_boundaries <- map_dfr(.x = files_list, .f = function(x) {
  print(x)
  sdi_data <- read_csv(x) %>% 
    rename_all(list(tolower)) %>%
    select_all(~gsub("\\s+|\\.|\\/", "_", .)) %>%
    rename(geometry = section_c_c2_boundary) %>%
    filter(geometry != 'n/a') %>%
    mutate(geometry =paste0("POLYGON ((", gsub(pattern = ";", replacement = ",", x = geometry) , "))")) %>%
    mutate(geometry = gsub(pattern = ',))', replacement = '))', x = geometry)) %>%
    st_as_sf(wkt = 'geometry') %>%
    st_zm() %>%
    mutate(row_id = row_number()) %>%
    rename(id = `_id`,
           country = section_b_b3_country, 
           province = section_b_b4_province, 
           city = section_b_b5_city, 
           municipality = section_b_b6_municipality, 
           settlement_name_community = section_b_b7_settlement_name_community, 
           settlement_name_municipality = section_b_b8_settlement_name_municipality) %>%
    select(row_id, id, settlement_name_municipality, settlement_name_community, municipality, city, province, country)
  
  sdi_data <- sdi_data %>%
    st_coordinates() %>%
    as_data_frame() %>%
    rename(latitude = X, 
           longitude = Y) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
    group_by(L2) %>%
    summarise(geometry = st_combine(geometry)) %>%
    ungroup() %>%
    st_cast("POLYGON") %>%
    left_join(., sdi_data %>% st_drop_geometry(), by = c('L2'='row_id'))
})

sf_use_s2(FALSE)

sdi_boundaries_valid <- sdi_boundaries  %>%
  st_make_valid() %>%
  filter(!(country %in% c("Philippines","Phlippines","PHILIPPINES","india","INDIA"))) %>%
  st_join(., subsaharan_africa) %>%
  mutate(lon = map_dbl(geometry, ~st_point_on_surface(.x)[[1]]),
         lat = map_dbl(geometry, ~st_point_on_surface(.x)[[2]])) %>%
  mutate(geohash_4 = gh_encode(lat, lon, precision = 4L)) %>%
  mutate(ISO_A3 = case_when(country == 'Sierra Leone' ~ 'SLE',
                            country == 'SIERRA LEONE' ~ 'SLE',
                            country == 'South Africa' ~ 'ZAF',
                            TRUE ~ as.character(ISO_A3)),
         NAME_EN = case_when(country == 'Sierra Leone' ~ 'Sierra Leone',
                             country == 'SIERRA LEONE' ~ 'Sierra Leone',
                             country == 'South Africa' ~ 'South Africa',
                             TRUE ~ as.character(NAME_EN))) 

st_write(sdi_boundaries_valid, 'data/sdi-analysis/data/sdi_boundaries.geojson', delete_dsn = TRUE)

# -------------------------------------------------------------------------
# Run Python code

# import geopandas as gpd
# import pandas as pd
# 
# sdi_boundaries = gpd.read_file('data/sdi-analysis/data/sdi_boundaries.geojson')
# 
# geo_output = gpd.GeoDataFrame({'block_id': pd.Series(dtype = 'object'), 'L2': pd.Series(dtype = 'float64'), 'id': pd.Series(dtype = 'float64'), 'settlement_name_municipality': pd.Series(dtype = 'object'), 'settlement_name_community': pd.Series(dtype = 'object'), 'municipality': pd.Series(dtype = 'object'), 'city': pd.Series(dtype = 'object'), 'province': pd.Series(dtype = 'object'), 'country': pd.Series(dtype = 'object'), 'ISO_A3': pd.Series(dtype = 'object'), 'NAME_EN': pd.Series(dtype = 'object'), 'lon': pd.Series(dtype = 'float64'), 'lat': pd.Series(dtype = 'float64'), 'geohash_4': pd.Series(dtype = 'object'), 'geometry': gpd.GeoSeries(dtype = 'geometry')}).set_crs(epsg=4326)
# 
# for i in ['BWA', 'ZWE', 'LBR', 'KEN', 'NAM', 'NGA', 'SLE', 'ZAF', 'SWZ', 'ZMB', 'GHA', 'MWI', 'TZA', 'UGA', 'BEN']:
#   print(i)
# geo_blocks = gpd.read_parquet('data/africa_geodata.parquet', 
#                               columns=['block_id', 'geometry'], 
#                               memory_map = True,
#                               filters = [('country_code', 'in', [i])])
# 
# geo_blocks_i = gpd.overlay(df1 = geo_blocks, df2 = sdi_boundaries, how='intersection', keep_geom_type = True, make_valid = True)
# geo_output = pd.concat([geo_output, geo_blocks_i], ignore_index=True)
# 
# geo_output.to_parquet('data/sdi-analysis/data/sdi_boundaries_overlay.parquet')

# Load services data ------------------------------------------------------

services_dir <- 'data/sdi-analysis/services'
services_list <- fs::dir_ls(services_dir, regexp = "\\.csv$")
services_list

sdi_services <- map_dfr(.x = services_list, .f = function(x) {
  print(x)
  sdi_data <- read_csv(x) %>%
    rename_all(list(tolower)) %>%
    select_all(~gsub("\\s+|\\.|\\/", "_", .)) %>%
    rename(id = `_id`) %>%
    mutate_all(function(x) gsub("other|n/a",NA_character_,x)) %>%
    select(id, section_b_b1_settlement_name_community, section_c_c1_service_type, section_c__c2_gps_latitude, section_c__c2_gps_longitude, section_d_d1_water_type, section_d_d1_toilet_type, section_d_d2_service_manager, section_d_d3_service_status, section_d_d4_water_quality, section_d_d5_water_provider) 
})

sdi_services <- sdi_services %>%
  filter(!is.na(section_c__c2_gps_longitude) & !is.na(section_c__c2_gps_latitude)) %>%
  st_as_sf(coords = c("section_c__c2_gps_longitude", "section_c__c2_gps_latitude"), crs = 4326, agr = "constant") %>% 
  filter(section_c_c1_service_type %in% c("water_point", "WATER POINT", "BORE HOLE", "BOREHOLE", "toilet", "TOILET", "garbage", "GARBAGE DISPOSAL", "DRAINAGE", "water_pipe")) %>%
  mutate(service_type = case_when(section_c_c1_service_type %in% c("water_point", "WATER POINT") ~ 'water point',
                                  section_c_c1_service_type %in% c("BORE HOLE", "BOREHOLE") ~ 'borehole',
                                  section_c_c1_service_type %in% c("toilet", "TOILET") ~ 'toilet',
                                  section_c_c1_service_type %in% c("garbage", "GARBAGE DISPOSAL") ~ 'garbage',
                                  section_c_c1_service_type %in% c("DRAINAGE") ~ 'drainage',
                                  section_c_c1_service_type %in% c("water_pipe") ~ 'water pipe',
                                  TRUE ~ ''),
         water_service = case_when(service_type %in% c('water pipe', 'water point','borehole') ~ 1, TRUE ~ 0),
         # water_type_nontap = case_when(section_d_d1_water_type %in% c("wells", "well", "dams", "dam", "BOREHOLE", "boreholes", "rivers", "springs", "hand_pump", "faucet_pump", "hand_washpump") ~ 1, TRUE ~ 0),
         toilet_type_nonpiped = case_when( section_d_d1_toilet_type %in% c("pit_latrine", "pour_flush", "YARD TOILET", "ecosan", "chemical") ~ 1, TRUE ~ 0),
         water_service_broken = case_when(water_service == 1 & section_d_d3_service_status %in% c("broken", "NOT WORKING") ~ 1, TRUE ~ 0),
         water_service_notsafe = case_when(water_service == 1 & section_d_d4_water_quality == "not_safe" ~ 1, TRUE ~ 0),
         water_service_broken_notsafe = case_when(water_service == 1 & (water_service_broken == 1 | water_service_notsafe == 1) ~ 1, TRUE ~ 0))

# sdi_services %>%
#   st_drop_geometry() %>%
#   summarize_at(vars(water_service, toilet_type_nonpiped, water_service_broken, water_service_notsafe, water_service_broken_notsafe), list(sum))

# Load profiles data ------------------------------------------------------

profiles_dir <- 'data/sdi-analysis/settlement profiles'
profiles_list <- fs::dir_ls(profiles_dir, regexp = "\\.csv$")
profiles_list
  
sdi_profiles <- map_dfr(.x = profiles_list, .f = function(x) {
  print(x)
  sdi_data <- read_csv(x) %>%
    select(`_id`, `section_B/_B1_GPS_latitude`, `section_B/_B1_GPS_longitude`, 
           `section_B/B14_Status`, 
           `section_C/C1_Structures_Residential`, `section_C/C11_Population_Estimate`, `section_C/C12_Total_Population`, 
           `section_D/D1_Location_Problems/canal`, `section_D/D1_Location_Problems/slope`, `section_D/D1_Location_Problems/flood_prone_area`, `section_D/D1_Location_Problems/garbage_dump`, `section_D/D1_Location_Problems/road_side`, `section_D/D1_Location_Problems/open_drains`, `section_D/D1_Location_Problems/water_body`, `section_D/D1_Location_Problems/railway_track`, `section_D/D1_Location_Problems/mine_dump`, `section_D/D1_Location_Problems/sinking_soil`, `section_D/D1_Location_Problems/industrial_hazards`, `section_D/D1_Location_Problems/under_power_lines`, 
           `section_D/D2_Location_Dangerous`, 
           `section_D/D4_Natural_Disasters/fires`, `section_D/D4_Natural_Disasters/floods`,
           `section_E/E1A_Eviction_Threats`, `section_E/E2A_Current_Eviction_Threat`, 
           `section_G/G1_Sewer_Line`, `section_G/G2_Sewer_Connected`, `section_G/G3_Toilets_Pay`, `section_G/G5_OpenDefecation_Percentage`, `section_G/G6_BucketSystem_Percentage`, `section_G/G7_10_Toilet_Types/individual_toilets`, 
           `section_F/F1_Quality`, `section_L/L5_Road_Type`) %>%
    rename_all(list(tolower)) %>%
    select_all(~gsub("\\/", "__", .)) %>%
    select_all(~gsub("\\s+|\\.", "__", .)) %>%
    mutate_all(function(x) gsub("other|n/a", NA_character_, x)) %>%
    filter(!is.na(section_b___b1_gps_latitude) | !is.na(section_b___b1_gps_longitude)) %>%
    rename(id = `_id`) %>%
    mutate_at(vars(section_c__c1_structures_residential, section_c__c11_population_estimate, section_c__c12_total_population, section_g__g5_opendefecation_percentage,	section_g__g6_bucketsystem_percentage), list(as.numeric))
})

sdi_profiles <- sdi_profiles %>%
  filter(!is.na(section_b___b1_gps_latitude) & !is.na(section_b___b1_gps_longitude)) %>%
  st_as_sf(coords = c("section_b___b1_gps_longitude", "section_b___b1_gps_latitude"), crs = 4326, agr = "constant") %>% 
  mutate(slum_conditions = case_when( (section_g__g5_opendefecation_percentage > 50 | section_g__g6_bucketsystem_percentage > 50 | 
                                         section_d__d2_location_dangerous == 'yes'	|
                                         section_e__e1a_eviction_threats	== 'yes' | section_e__e2a_current_eviction_threat == 'yes' |
                                         section_g__g1_sewer_line == 'no' | section_g__g2_sewer_connected  == 'no' |
                                         section_b__b14_status == 'undeclared_illegal_unprotected' | section_f__f1_quality == 'not_safe' |
                                         section_l__l5_road_type == 'dirt_paths' | 
                                         section_g__g7_10_toilet_types__individual_toilets	== 'False' | 
                                         section_d__d4_natural_disasters__fires == 'True' | section_d__d4_natural_disasters__floods == 'True') ~ 1, 
                                      TRUE ~ 0),
         location_problems = case_when(section_d__d1_location_problems__flood_prone_area == 'True' | section_d__d1_location_problems__garbage_dump == 'True'  | section_d__d1_location_problems__railway_track == 'True' | section_d__d1_location_problems__mine_dump == 'True' | section_d__d1_location_problems__sinking_soil == 'True' | section_d__d1_location_problems__industrial_hazards == 'True' | section_d__d1_location_problems__under_power_lines == 'True' ~ 1, TRUE ~ 0),
         count_location = case_when( section_d__d1_location_problems__flood_prone_area %in% c('True','False') | section_d__d1_location_problems__garbage_dump %in% c('True','False') | section_d__d1_location_problems__railway_track %in% c('True','False') | section_d__d1_location_problems__mine_dump %in% c('True','False') | section_d__d1_location_problems__sinking_soil %in% c('True','False') | section_d__d1_location_problems__industrial_hazards %in% c('True','False') | section_d__d1_location_problems__under_power_lines %in% c('True','False') ~ 1, TRUE ~ 0),
         poor_sanitation = case_when((section_g__g5_opendefecation_percentage > 50 | section_g__g6_bucketsystem_percentage > 50) ~ 1, TRUE ~ 0),
         count_sanitation = case_when((section_g__g5_opendefecation_percentage > 0 | section_g__g6_bucketsystem_percentage > 0) ~ 1, TRUE ~ 0),
         eviction_risk = case_when((section_e__e1a_eviction_threats	== 'yes' | section_e__e2a_current_eviction_threat == 'yes') ~ 1, TRUE ~ 0),
         count_eviction = case_when(section_e__e1a_eviction_threats %in% c('yes','no') | section_e__e2a_current_eviction_threat %in% c('yes','no') ~ 1, TRUE ~ 0),
         status_undeclared_illegal = case_when(section_b__b14_status == 'undeclared_illegal_unprotected' ~ 1, TRUE ~ 0),
         count_status = case_when(section_b__b14_status %in% c("declared_legal_protected", "undeclared_illegal_unprotected", "resettled") ~ 1, TRUE ~ 0),
         no_sewerline = case_when((section_g__g1_sewer_line == 'no' | section_g__g2_sewer_connected  == 'no') ~ 1, TRUE ~ 0 ),
         count_sewerline = case_when((section_g__g1_sewer_line %in% c('yes','no') | section_g__g2_sewer_connected  %in% c('yes','no')) ~ 1, TRUE ~ 0 ),
         water_unsafe = case_when(section_f__f1_quality == 'not_safe' ~ 1, TRUE ~ 0),
         count_water = case_when(section_f__f1_quality %in% c("safe", "not_safe")  ~ 1, TRUE ~ 0),
         enviro_dangers = case_when((section_d__d2_location_dangerous == 'yes' | section_d__d4_natural_disasters__fires == 'True' | section_d__d4_natural_disasters__floods == 'True') ~ 1, TRUE ~ 0),
         count_enviro = case_when((section_d__d2_location_dangerous %in% c('yes','no') | section_d__d4_natural_disasters__fires %in% c('True','False') | section_d__d4_natural_disasters__floods %in% c('True','False')) ~ 1, TRUE ~ 0))

# Collapse to settlement IDs ---------------------------------------------

sdi_services2 <- sdi_services %>%
  st_transform(4326) %>%
  st_join(., sdi_boundaries_valid) %>% #, st_nn, k = 1
  filter(!is.na(ISO_A3)) %>%
  st_drop_geometry() %>%
  mutate(count_services = 1) %>%
  group_by(id.y) %>%
  summarize_at(vars(count_services, water_service, toilet_type_nonpiped, water_service_broken, water_service_notsafe, water_service_broken_notsafe), list(sum)) %>%
  ungroup() 

sdi_profiles2 <- sdi_profiles %>%
  st_transform(4326) %>%
  st_join(., sdi_boundaries_valid) %>%
  filter(!is.na(ISO_A3)) %>%
  st_drop_geometry() %>%
  rename(residential_structures = section_c__c1_structures_residential, 
         settlement_population = section_c__c12_total_population) %>%
  mutate(count_profile_responses = 1,
         opendefecation_population = settlement_population*(section_g__g5_opendefecation_percentage/100),
         bucketsystem_population = settlement_population*(section_g__g6_bucketsystem_percentage/100) ) %>%
  group_by(id.y) %>%
  summarize_at(vars(count_profile_responses, residential_structures, settlement_population,
                    opendefecation_population, bucketsystem_population, 
                    poor_sanitation, count_sanitation, location_problems, count_location, eviction_risk, count_eviction, status_undeclared_illegal, count_status, no_sewerline, count_sewerline, water_unsafe, count_water, enviro_dangers, count_enviro), list(sum)) %>%
  ungroup() 

# Join blocks to SDI boundaries -------------------------------------------

area_data <- arrow::open_dataset('data/africa_data.parquet') %>% 
  select(block_id, block_geohash, block_area_m2, building_area_m2, building_count, average_building_area_m2, building_to_block_area_ratio, k_complexity, landscan_population_un, worldpop_population_un, country_code, country_name, class_urban_hierarchy, urban_id, urban_center_name, urban_country_code, urban_country_name) %>%
  collect()

# sdi_boundaries_overlay.parquet is generated with Python code above
sdi_areas <- st_read_parquet('data/sdi-analysis/data/sdi_boundaries_overlay.parquet') %>%
  st_set_crs(sf::st_crs(4326)) %>%
  st_transform(3395) %>%
  mutate(sdi_area = as.numeric(st_area(.))) %>%
  left_join(., area_data, by = c('block_id' = 'block_id')) %>%
  st_transform(4326)

# aggregate population by ID and area share
sdi_urban_rank <- sdi_areas %>%
  st_drop_geometry() %>%
  mutate(block_area_share = sdi_area / block_area_m2, 
         landscan_population_un_allocated = landscan_population_un * block_area_share) %>%
  group_by(id, urban_id, urban_center_name, urban_country_code, urban_country_name) %>%
  summarize_at(vars(landscan_population_un_allocated), list(sum), na.rm = TRUE) %>%
  ungroup() %>%
  group_by(id) %>%
  mutate(rank_urban = row_number(desc(landscan_population_un_allocated))) %>%
  ungroup() %>%
  filter(rank_urban == 1)

# Join together -----------------------------------------------------------

sdi_areas2 <- sdi_areas %>%
  st_drop_geometry() %>%
  mutate(sdi_area_km2 = sdi_area * 1e-6,
         block_area_share = sdi_area / block_area_m2, 
         landscan_population_un_allocated = landscan_population_un * block_area_share, 
         worldpop_population_un_allocated = worldpop_population_un * block_area_share, 
         k_complexity_wt = landscan_population_un_allocated * k_complexity) %>%
  # settlement_name_municipality, settlement_name_community, municipality, city, province, country, ISO_A3, NAME_EN
  group_by(id) %>%
  summarize_at(vars(k_complexity_wt, landscan_population_un_allocated, worldpop_population_un_allocated, sdi_area, sdi_area_km2), list(sum), na.rm = TRUE) %>%
  ungroup() %>%
  mutate(k_complexity_avg = k_complexity_wt/landscan_population_un_allocated) %>%
  left_join(., sdi_urban_rank %>% select(id, urban_id, urban_center_name, urban_country_code, urban_country_name), by = c('id' = 'id')) %>%
  mutate(k_label = case_when(round(k_complexity_avg,0) >= 15  ~ "15+", 
                             TRUE ~ as.character(round(k_complexity_avg,0)))) %>%
  mutate(k_label = factor(k_label , levels = c('1','2','3','4','5','6','7','8','9','10','11','12','13','14','15+'))) %>%
  mutate(k_label_10 = case_when(round(k_complexity_avg,0) >= 10  ~ "10+", 
                             TRUE ~ as.character(round(k_complexity_avg,0)))) %>%
  mutate(k_label_10 = factor(k_label_10, levels = c('1','2','3','4','5','6','7','8','9','10+'))) %>%
  left_join(., sdi_profiles2 %>% select(id.y, residential_structures, settlement_population, opendefecation_population, bucketsystem_population, location_problems, count_location, poor_sanitation, count_sanitation, eviction_risk, count_eviction, status_undeclared_illegal, count_status, no_sewerline, count_sewerline, water_unsafe, count_water, enviro_dangers, count_enviro), by = c('id'='id.y')) %>%
  left_join(., sdi_services2 %>% select(id.y,  count_services, water_service, toilet_type_nonpiped, water_service_broken, water_service_notsafe, water_service_broken_notsafe), by = c('id'='id.y')) 

# Boundaries with attributes
st_write(sdi_boundaries_valid %>%
           select(id, geometry) %>%
           left_join(., sdi_areas2, by = c('id' = 'id')),
         'data/sdi-analysis/data/sdi_boundaries_settlements.geojson', delete_dsn = TRUE)

# Filter down SDI universe ------------------------------------------------

sdi_areas3 <- sdi_areas2 %>%
  mutate(settlement_count = 1) %>%
  filter(sdi_area_km2 <= 2) %>%
  mutate(water_point_broken_share = water_service_broken/water_service, 
         water_point_notsafe_share = water_service_notsafe/water_service, 
         water_point_broken_notsafe_share = water_service_broken_notsafe/water_service,
         opendefecation_share = opendefecation_population/settlement_population,
         location_problems_share = location_problems/count_location,
         poor_sanitation_share = poor_sanitation / count_sanitation, 
         eviction_risk_share = eviction_risk / count_eviction, 
         illegal_share = status_undeclared_illegal / count_status, 
         no_sewer_share = no_sewerline / count_sewerline, 
         unsafe_water_share = water_unsafe / count_water, 
         enviro_dangers_share = enviro_dangers / count_enviro) %>%
  mutate(water_point_broken_perkm2 = water_service_broken / sdi_area_km2,
         water_point_notsafe_perkm2 = water_service_notsafe / sdi_area_km2,
         water_point_broken_notsafe_perkm2 = water_service_broken_notsafe / sdi_area_km2,
         opendefecation_perkm2 = opendefecation_population/ sdi_area_km2,
         location_problems_perkm2 = location_problems/ sdi_area_km2,
         poor_sanitation_perkm2 = poor_sanitation /  sdi_area_km2,
         eviction_risk_perkm2 = eviction_risk / sdi_area_km2,
         illegal_perkm2 = status_undeclared_illegal / sdi_area_km2,
         no_sewer_perkm2 = no_sewerline / sdi_area_km2,
         unsafe_water_perkm2 = water_unsafe / sdi_area_km2,
         enviro_dangers_perkm2 = enviro_dangers / sdi_area_km2) %>%
  mutate(water_services_share_avg = rowMeans(across(all_of(c('water_point_broken_notsafe_share', 'unsafe_water_share'))), na.rm = TRUE),
         water_services_perkm2_avg = rowMeans(across(all_of(c('water_point_broken_notsafe_perkm2', 'unsafe_water_perkm2'))), na.rm = TRUE),
         all_services_share_avg = rowMeans(across(all_of(c('water_point_broken_notsafe_share', 'opendefecation_share', 'location_problems_share', 'poor_sanitation_share', 'eviction_risk_share', 'illegal_share', 'no_sewer_share', 'unsafe_water_share', 'enviro_dangers_share'))), na.rm = TRUE),
         all_services_perkm2_avg = rowMeans(across(all_of(c('water_point_broken_notsafe_perkm2', 'opendefecation_perkm2', 'location_problems_perkm2', 'poor_sanitation_perkm2', 'eviction_risk_perkm2', 'illegal_perkm2', 'no_sewer_perkm2', 'unsafe_water_perkm2', 'enviro_dangers_perkm2'))), na.rm = TRUE)
         ) %>%
  group_by(urban_id) %>%
  mutate(settlement_count_urban_sum = sum(settlement_count),
         water_services_perkm2_avg_rank = row_number(desc(water_services_perkm2_avg)),
         water_services_share_avg_rank = row_number(desc(water_services_share_avg)),
         all_services_perkm2_avg_rank = row_number(desc(all_services_perkm2_avg)),
         all_services_share_avg_rank = row_number(desc(all_services_share_avg))
         ) %>%
  ungroup() %>%
  filter(all_services_perkm2_avg_rank <= 50 ) # | water_services_perkm2_avg_rank <= 50) 

# Check the distributions -------------------------------------------------

# Generate urban averages --------------------------------------------------

df_kmad <- sdi_areas3 %>%
  st_drop_geometry() %>%
  mutate(n = ceiling(round(landscan_population_un_allocated/10,0))) %>%
  select(id, urban_id, n, k_complexity_avg) %>%
  uncount(weights = n) %>%
  mutate(
    random_jitter = runif(n = nrow(.), min = 0, max = .01),
    k_complexity_avg = k_complexity_avg + random_jitter
  ) %>% 
  group_by(urban_id) %>%
  summarise(k_settlement_mad2 = mad(k_complexity_avg)) %>%
  ungroup() %>%
  select(urban_id, k_settlement_mad2)
  
urban_distribution <- sdi_areas3 %>%
  st_drop_geometry() %>%
  mutate(
    random_jitter = runif(n = nrow(.), min = 0, max = .01),
    k_complexity_avg_jitter = k_complexity_avg + random_jitter
  ) %>%
  group_by(urban_id, urban_center_name, urban_country_name) %>%
  summarise(sdi_area_km2 = sum(sdi_area_km2),
            settlement_count = sum(settlement_count),
            k_settlement_sd = Hmisc::wtd.var(x = k_complexity_avg, weights = landscan_population_un_allocated, na.rm=TRUE), 
            k_complexity_wt = sum(k_complexity_wt),
            k_settlement_min = min(k_complexity_avg),
            k_settlement_max = max(k_complexity_avg),
            k_settlement_75 = Hmisc::wtd.quantile(x = k_complexity_avg, weights = landscan_population_un_allocated, probs= .9, na.rm=TRUE),
            k_settlement_25 = Hmisc::wtd.quantile(x = k_complexity_avg, weights = landscan_population_un_allocated, probs= .1, na.rm=TRUE),
            k_settlement_mad = matrixStats::weightedMad(x = k_complexity_avg_jitter, w = landscan_population_un_allocated,  na.rm = TRUE), 
            landscan_population_un_allocated = sum(landscan_population_un_allocated),
            .groups = 'keep'
            ) %>%
  ungroup() %>%
  mutate(k_complexity_avg = k_complexity_wt/landscan_population_un_allocated,
         sdi_area_km2 = sdi_area_km2/settlement_count,
         k_settlement_sd = sqrt(k_settlement_sd),
         k_settlement_mad = case_when(k_settlement_mad < 1 ~ 1, TRUE ~ k_settlement_mad)
         ) %>%
  left_join(., df_kmad, by = c('urban_id'='urban_id')) %>%
  arrange(desc(k_complexity_avg)) %>%
  left_join(., df_k %>% select(urban_id, class_urban_hierarchy, k_complexity_weighted_landscan_un, landscan_population_un, k_complexity_sd, k_complexity_mad, k_complexity_75, k_complexity_25),  #  
            by = c('urban_id' = 'urban_id')) %>%
  filter(landscan_population_un >= 1000000 & class_urban_hierarchy %in% c('1 - Core urban', '2 - Peripheral urban')) %>%
  filter(landscan_population_un_allocated >= 1000) %>%
  mutate(share_of_pop = landscan_population_un_allocated/landscan_population_un) %>%
  select(urban_id, urban_center_name, urban_country_name, 
         sdi_area_km2, settlement_count, share_of_pop, 
         landscan_population_un_allocated, landscan_population_un, share_of_pop, 
         k_complexity_avg, k_complexity_weighted_landscan_un, 
         k_settlement_sd, k_settlement_mad, k_settlement_75, k_settlement_25, k_settlement_min, k_settlement_max,
         k_complexity_sd, k_complexity_mad, k_complexity_75, k_complexity_25) %>%
  pivot_longer(cols = c(k_complexity_avg, k_complexity_weighted_landscan_un)) %>%
  mutate(name = case_when(name == 'k_complexity_avg' ~ "SDI settlements",
                          name == 'k_complexity_weighted_landscan_un' ~ 'Urban areas')) %>%
  mutate(urban_label = paste0(urban_center_name, ', ', urban_country_name)) %>%
  mutate(urban_label = factor(urban_label, levels = rev(c('Port Harcourt, Nigeria', 'Cotonou, Benin', 'Lagos, Nigeria', 'Freetown, Sierra Leone', 'Monrovia, Liberia', 'Durban, South Africa', 'Dar es Salaam, Tanzania', 'Kampala, Uganda', 'Accra, Ghana')))) %>%
  mutate(name = factor(name, levels = c('Urban areas',"SDI settlements"))) %>%
  mutate(k_mad = case_when(name == 'SDI settlements' ~ k_settlement_mad,
                           name == 'Urban areas' ~ k_complexity_mad),
         k_sd = case_when(name == 'SDI settlements' ~ k_settlement_sd,
                          name == 'Urban areas' ~ k_complexity_sd),
         k_75 = case_when(name == 'SDI settlements' ~ k_settlement_75, 
                          name == 'Urban areas' ~ k_complexity_75),
         k_25 = case_when(name == 'SDI settlements' ~ k_settlement_25,
                          name == 'Urban areas' ~ k_complexity_25),
         population = case_when(name == 'SDI settlements' ~ landscan_population_un_allocated,
                                name == 'Urban areas' ~ landscan_population_un)
         ) %>%
  select(urban_id, urban_label, name, sdi_area_km2, settlement_count, value, k_mad, k_sd, k_75, k_25, population, share_of_pop, k_settlement_min, k_settlement_max) %>%
  mutate(k_settlement_min = case_when(name == 'Urban areas'  ~ NA, TRUE ~ k_settlement_min),
         k_settlement_max = case_when(name == 'Urban areas'  ~ NA, TRUE ~ k_settlement_max))

caption_note <- urban_distribution %>% 
  filter(name == 'Urban areas') %>% 
  mutate(caption_label = 
           paste0(urban_label, ' (', settlement_count,' settlements accounting for ', round(share_of_pop*100,3), '% of ', comma(round(population/1000000,2)),'M urban population)')) %>%
  select(urban_id, caption_label)

(bar_sdi <- ggplot(urban_distribution, aes(x=urban_label, y=value, fill=name)) + 
    geom_bar(stat="identity", color="black", linewidth = .3, 
             position=position_dodge()) +
    geom_errorbar(aes(ymin= k_settlement_min, ymax= k_settlement_max), width=.2, alpha = 1, color = '#ffdf00', 
                  position=position_dodge(.9)) +
    geom_errorbar(aes(ymin= k_25 , ymax= k_75 ), width=.2,
                  position=position_dodge(.9)) +
  coord_flip() + 
    labs(y= 'Block complexity population weighted average', x = 'Urban area', fill = '', color =  '', subtitle = '') +
    scale_y_continuous(expand = c(0,0), limits = c(0, 19), oob=scales::squish) +
    scale_fill_manual(values = c('#4D96FF','#FF6B6B')) +
    theme_classic() +
    theme(legend.position = 'bottom',
          plot.subtitle = element_text(size = 12, hjust=.5, face = 'bold', color ='#333333'),
          axis.ticks = element_blank(), 
          axis.text = element_text(color = '#333333', size = 11),
          axis.title = element_text(color = '#333333', size = 11, face = 'bold'),
          legend.text = element_text(color = '#333333', size = 11)))


# Generate total distribution ---------------------------------------------

k_distribution <- rbind(sdi_areas3 %>%
  filter(!is.na(k_label_10)) %>%
  group_by(k_label_10) %>%
  summarize_at(vars(k_complexity_wt, landscan_population_un_allocated, sdi_area), list(sum), na.rm = TRUE) %>%
  ungroup() %>%
  mutate(k_complexity_avg = k_complexity_wt/landscan_population_un_allocated) %>%
  mutate(k_share = landscan_population_un_allocated/sum(landscan_population_un_allocated)) %>%
  select(k_label_10, landscan_population_un_allocated, k_share) %>%
  rename(population = landscan_population_un_allocated, 
         k = k_label_10) %>%
  mutate(group = 'SDI settlements'), 
  df_k_total %>%
    filter(group_val != 'Off-network') %>%
    mutate(group_val = factor(group_val, levels = c('1','2','3','4','5','6','7','8','9','10+'))) %>%
    select(group_val, landscan_population_un) %>%
    mutate(k_share = landscan_population_un/sum(landscan_population_un)) %>%
    rename(population = landscan_population_un, 
           k = group_val) %>%
    mutate(group = 'Urban areas')) %>%
  complete(k, group, fill = list(population = 0, k_share = 0)) %>%
  mutate(group = factor(group, levels = c('Urban areas',"SDI settlements")))

#grey2 <- c('#414141','#777777')
#colorhexes <- colorRampPalette(c('#93328E','#CF3F80','#F7626B','#FF925A','#FFC556','#F9F871'))(length(k_distribution$k)-2)

(histogram_sdi <- ggplot(data = k_distribution, aes(y = k, x = k_share, group = group,
                                                    fill = group)) +
    geom_bar(color = 'black', linewidth = .3, 
             stat="identity", position = position_dodge()) + 
    coord_flip()+
    labs(y= 'Block complexity', x = 'Population share', fill = '', color =  '', subtitle = '') +
    scale_x_continuous(expand = c(0,0), labels = label_percent())+ 
    scale_fill_manual(values = c('#4D96FF','#FF6B6B')) +
    # labs(subtitle = 'Validating block complexity with known settlements') +
    theme_classic() +
    theme(legend.position = 'bottom',
          plot.subtitle = element_text(size = 12, hjust=.5, face = 'bold', color ='#333333'),
          axis.ticks = element_blank(), 
          axis.text = element_text(color = '#333333', size = 11),
          axis.title = element_text(color = '#333333', size = 11, face = 'bold'),
          legend.text = element_text(color = '#333333', size = 11)
    ) ) 

annotation_bottom = paste0('Notes: For Figure A on left the upper / lower quartile are in black and min / max are in yellow (only for SDI samples). The sample size of SDI settlements: ', paste(unlist(caption_note$caption_label), collapse ="; "), '.')

print(annotation_bottom)
(sdi_validation <- bar_sdi + histogram_sdi + plot_layout(guides = 'collect') 
  & plot_annotation(tag_levels = list(c("A","B"))
                    #,caption = str_wrap(annotation_bottom, 203)
                    )
  & theme(legend.position = 'bottom',
          plot.caption = element_text(hjust = 0))
  )

ggsave(plot = sdi_validation, 
       filename = 'data/sdi-analysis/viz/sdi_validation_barcharts.pdf', width = 12, height = 5.64)

# Example map -------------------------------------------------------------

freetown <- sdi_areas %>%
  filter(urban_id %in% c('ghsl_1507')) %>% 
  mutate(k_label = case_when(round(k_complexity,0) >= 10  ~ "10+", 
                             TRUE ~ as.character(round(k_complexity,0)))) %>%
  mutate(k_label = factor(k_label , levels = c('1','2','3','4','5','6','7','8','9','10+'))) 
  
sle_boundaries <- sdi_boundaries_valid %>%
  filter(ISO_A3 == 'SLE') %>%
  st_join(., freetown %>% select(geometry), left = FALSE)

freetown_blocks <- st_read_parquet('data/sdi-analysis/freetown_blocks.parquet') %>%
  st_set_crs(sf::st_crs(4326))  %>% 
  mutate(k_10 = case_when(round(k_complexity,0) >= 10  ~ "10+", 
                             TRUE ~ as.character(round(k_complexity,0)))) %>%
  mutate(k_10 = factor(k_10 , levels = c('1','2','3','4','5','6','7','8','9','10+'))) 

grey2 <- c('#414141','#777777')
colorhexes <- colorRampPalette(c('#93328E','#CF3F80','#F7626B','#FF925A','#FFC556','#F9F871'))(10-2)




freetown_map <- ((ggplot() + 
  geom_sf(data = freetown_blocks %>%
            st_crop(x = ., y = sle_boundaries), linewidth = .1, alpha = .5) +
    geom_sf(data = sle_boundaries, color = '#4B4453', alpha = 0, linewidth = .5) + 
  geom_sf(data = freetown, aes(fill = k_label)) + 
  scale_fill_manual(name = '', values = c(grey2, colorhexes)) + 
  theme_void() + 
  theme(legend.position ='none', plot.margin=unit(c(t=0,r=0,b=0,l=0), "pt")) ) +
(ggplot() + 
    geom_sf(data = freetown_blocks %>%
              st_crop(x = ., y = sle_boundaries), aes(fill = k_10), color = 'white', linewidth = .1) +
    geom_sf(data = sle_boundaries, color = '#4B4453', linewidth = .5) + 
    scale_fill_manual(name = '', values = c(grey2, colorhexes)) + 
    theme_void() +
    theme(plot.margin=unit(c(t=0,r=0,b=40,l=0), "pt")) ) &
  plot_annotation(tag_levels = list(c("A","B"))))
   
  
ggsave(plot = freetown_map , 
       filename = 'data/sdi-analysis/viz/freetown_map.pdf', width = 14, height = 4)




  

  
  
  
  
  




