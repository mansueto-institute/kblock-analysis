
library(tidyverse)
library(dplyr)
library(sf)
library(arrow)
library(sfarrow)
library(geohashTools)
library(scales)
options(scipen = 99999)

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

# https://uchicago.app.box.com/folder/235077315882
filedir = '/Users/nm/Downloads/sdi_settlements data-selected/boundaries'
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
  mutate(geohash_4 = gh_encode(lat, lon, precision = 4L))

st_write(sdi_boundaries_valid, '/Users/nm/Downloads/sdi_settlements data-selected/sdi_boundaries.geojson', delete_dsn = TRUE)

# -------------------------------------------------------------------------
# Run Python code

# import geopandas as gpd
# import pandas as pd
# 
# sdi_boundaries = gpd.read_file('/Users/nm/Downloads/sdi_settlements data-selected/sdi_boundaries.geojson')
# 
# geo_output = gpd.GeoDataFrame({'block_id': pd.Series(dtype = 'object'), 'L2': pd.Series(dtype = 'float64'), 'id': pd.Series(dtype = 'float64'), 'settlement_name_municipality': pd.Series(dtype = 'object'), 'settlement_name_community': pd.Series(dtype = 'object'), 'municipality': pd.Series(dtype = 'object'), 'city': pd.Series(dtype = 'object'), 'province': pd.Series(dtype = 'object'), 'country': pd.Series(dtype = 'object'), 'ISO_A3': pd.Series(dtype = 'object'), 'NAME_EN': pd.Series(dtype = 'object'), 'lon': pd.Series(dtype = 'float64'), 'lat': pd.Series(dtype = 'float64'), 'geohash_4': pd.Series(dtype = 'object'), 'geometry': gpd.GeoSeries(dtype = 'geometry')}).set_crs(epsg=4326)
# 
# for i in ['BWA', 'ZWE', 'LBR', 'KEN', 'NAM', 'NGA', 'SLE', 'ZAF', 'SWZ', 'ZMB', 'GHA', 'MWI', 'TZA', 'UGA', 'BEN']:
#   print(i)
# geo_blocks = gpd.read_parquet('/Users/nm/Desktop/Projects/work/mnp-analysis/big-data.nosync/run3/africa_data/africa_geodata.parquet', 
#                               columns=['block_id', 'geometry'], 
#                               memory_map = True,
#                               filters = [('country_code', 'in', [i])])
# 
# geo_blocks_i = gpd.overlay(df1 = geo_blocks, df2 = sdi_boundaries, how='intersection', keep_geom_type = True, make_valid = True)
# geo_output = pd.concat([geo_output, geo_blocks_i], ignore_index=True)
# 
# geo_output.to_parquet('/Users/nm/Downloads/sdi_settlements data-selected/sdi_boundaries_overlay.parquet')

# -------------------------------------------------------------------------

area_data <- arrow::open_dataset('/Users/nm/Desktop/Projects/work/mnp-analysis/big-data.nosync/run3/africa_data/africa_data.parquet') %>% 
  select(block_id, block_geohash, block_area_m2, building_area_m2, building_count, average_building_area_m2, building_to_block_area_ratio, k_complexity, landscan_population_un, worldpop_population_un, country_code, country_name, class_urban_hierarchy, urban_id, urban_center_name, urban_country_code, urban_country_name) %>%
  collect()

sdi_areas <- st_read_parquet('/Users/nm/Downloads/sdi_settlements data-selected/sdi_boundaries_overlay.parquet') %>%
  st_set_crs(sf::st_crs(4326)) %>%
  st_transform(3395) %>%
  mutate(sdi_area = as.numeric(st_area(.))) %>%
  left_join(., area_data, by = c('block_id' = 'block_id'))

sdi_areas %>% st_drop_geometry() %>%
  arrange(urban_country_name, urban_id) %>%
  select(urban_center_name, urban_country_name) %>%
  distinct()
  
st_write(sdi_areas, '/Users/nm/Downloads/sdi_settlements data-selected/sdi_boundaries_overlay.geojson')

sdi_areas2 <- sdi_areas %>%
  mutate(ISO_A3 = case_when(country == 'Sierra Leone' ~ 'SLE',
                            country == 'SIERRA LEONE' ~ 'SLE',
                            country == 'South Africa' ~ 'ZAF',
                            TRUE ~ as.character(ISO_A3))) %>%
  st_drop_geometry() %>%
  mutate(sdi_area_km2 = sdi_area * 1e-6,
         block_area_share = sdi_area / block_area_m2, 
         landscan_population_un_allocated = landscan_population_un * block_area_share, 
         worldpop_population_un_allocated = worldpop_population_un * block_area_share, 
         k_complexity_wt = landscan_population_un_allocated * k_complexity) %>%
  group_by(L2, id, settlement_name_municipality, settlement_name_community, municipality, city, province, country, ISO_A3, NAME_EN) %>%
  summarize_at(vars(k_complexity_wt, landscan_population_un_allocated, worldpop_population_un_allocated, sdi_area, sdi_area_km2), list(sum), na.rm = TRUE) %>%
  ungroup() %>%
  mutate(k_complexity_avg = k_complexity_wt/landscan_population_un_allocated)

sdi_areas2 <- sdi_areas2 %>%
  mutate(k_label = case_when(round(k_complexity_avg,0) >= 15  ~ "15+", 
                             TRUE ~ as.character(round(k_complexity_avg,0)))) %>%
  mutate(k_label = factor(k_label , levels = c('1','2','3','4','5','6','7','8','9','10','11','12','13','14','15+'))) 



st_write(sdi_boundaries_valid %>%
           select(id, geometry) %>%
           left_join(., sdi_areas2 %>% filter(ISO_A3 == 'LBR' | country == 'Liberia'), by = c('id' = 'id')),
         '/Users/nm/Downloads/sdi_settlements data-selected/liberia_settlements.geojson', delete_dsn = TRUE)




sdi_areas_k_groups <- sdi_areas2 %>%
  filter(sdi_area_km2 <= 2) %>%
  filter(!is.na(k_label)) %>%
  group_by(k_label) %>%
  summarize_at(vars(k_complexity_wt, landscan_population_un_allocated, sdi_area), list(sum), na.rm = TRUE) %>%
  ungroup() %>%
  mutate(k_complexity_avg = k_complexity_wt/landscan_population_un_allocated) %>%
  mutate(k_share = landscan_population_un_allocated/sum(landscan_population_un_allocated))

grey2 <- c('#414141','#777777')
kdist = length(sdi_areas_k_groups$k_label)
colorhexes <- colorRampPalette(c('#93328E','#CF3F80','#F7626B','#FF925A','#FFC556','#F9F871'))(kdist-2)


(histogram_sdi <- ggplot() +
    geom_bar(data = sdi_areas_k_groups, aes(y = k_label, x = landscan_population_un_allocated, 
                                               fill = k_label), color = 'white', linewidth = .3, stat="identity") + 
    coord_flip() +
    labs(y= 'Block complexity', x = 'Population', fill = '', color =  '', subtitle = '') + 
    scale_fill_manual(values = c(grey2,colorhexes)) +
    scale_x_continuous(expand = c(0,0), labels = label_comma(), breaks = c(0,250000,500000,750000,1000000,1250000,1500000)) +
    #scale_x_continuous(expand = c(.005,.005), labels = label_comma(accuracy = 1L, scale =  0.000001, suffix = "M") ) +
    theme(# panel.background = element_rect(fill = "white"), 
          axis.ticks = element_blank(), 
          axis.text = element_text(color = '#333333', size = 12),
          axis.title = element_text(color = '#333333', size = 12, face = 'bold'),
          legend.text = element_text(color = '#333333', size = 14)
    ))

  
ggplot(sdi_areas2 %>% 
         filter(sdi_area_km2 <= 2), aes(x = k_complexity_avg)) +
  geom_histogram(aes(weight =  landscan_population_un_allocated), bins = 30) +
  scale_x_continuous(expand = c(0,0), limits = c(0,25)) +
  scale_y_continuous(expand = c(0,0), labels = label_comma() )

ggplot(sdi_areas2 %>% 
         filter(sdi_area_km2 <= 2)
       , aes(x = k_complexity_avg)) +
  geom_histogram(aes(weight =  landscan_population_un_allocated), bins = 30) +
  scale_x_continuous(expand = c(0,0), limits = c(0,25)) +
  scale_y_continuous(expand = c(0,0), labels = label_comma() )

sdi_areas_country <- sdi_areas2 %>%
  filter(sdi_area_km2 <= 2) %>%
  group_by(ISO_A3) %>%
  summarize_at(vars(k_complexity_wt, landscan_population_un_allocated, sdi_area), list(sum), na.rm = TRUE) %>%
  ungroup() %>%
  mutate(k_complexity_avg = k_complexity_wt/landscan_population_un_allocated)

sdi_areas_country %>% select(ISO_A3, k_complexity_avg ) %>% arrange(desc(k_complexity_avg))

sdi_areas_all <- sdi_areas2 %>%
  filter(sdi_area_km2 <= 2) %>%
  summarize_at(vars(k_complexity_wt, landscan_population_un_allocated, sdi_area), list(sum), na.rm = TRUE) %>%
  mutate(k_complexity_avg = k_complexity_wt/landscan_population_un_allocated)
  








# -------------------------------------------------------------------------


# write_parquet(area_data, '/Users/nm/Downloads/sdi_settlements data-selected/mnp_sdi_blocks.parquet')


# sdi_areas <- st_read_parquet('/Users/nm/Downloads/sdi_settlements data-selected/mnp_sdi_blocks_geom.parquet') %>%
#   st_set_crs(sf::st_crs(4326))
#   
# sdi_areas <- sdi_areas %>%
#   left_join(., area_data,
#              by = c('block_id' = 'block_id')) %>%
#   st_join(., sdi_boundaries_valid)


# sdi_areas <- arrow::open_dataset('/Users/nm/Desktop/Projects/work/mnp-analysis/big-data.nosync/run3/africa_data/africa_geodata.parquet') %>% 
#   select(block_id, country_code, block_geohash, geometry) %>%
#   filter(country_code == 'SLE' & block_id %in% unique(area_data$block_id))  %>%
#   read_sf_dataset()


  # import geopandas as gpd
  # import pandas as pd
  # blocks = pd.read_parquet('/Users/nm/Downloads/sdi_settlements data-selected/mnp_sdi_blocks.parquet')
  # geo_blocks = gpd.read_parquet('/Users/nm/Desktop/Projects/work/mnp-analysis/big-data.nosync/run3/africa_data/africa_geodata.parquet', 
  #                               columns=['block_id', 'geometry'], 
  #                               memory_map = True,
  #                               filters = [('block_id', 'in', blocks['block_id'].unique())])
  

#df <- read_parquet('/Users/nm/Desktop/Projects/work/mnp-analysis/big-data.nosync/run3/africa_data/africa_data.parquet')

# for (i in sort(unique(sdi_boundaries_valid$ISO_A3))) {
#   print(i)
# }




