

library(ggplot2)
library(sf)
library(tidyverse)
library(viridis)
library(patchwork)
library(scales)
library(tidygeocoder)
library(readxl)
library(readr)
library(writexl)
library(units)
library(ggsn)
library(arrow)
#library(geoarrow)
library(sfarrow)
library(osmdata)
library(viridis)
library(scatterpie)
library(ggrepel)
library(DescTools)
options(scipen = 9999)
options(lifecycle_verbosity = "warning")

# Load aggregation function
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
source('aggregation_func.R') # should be in the ~/kblock-analysis directory

# Read in data ------------------------------------------------------------
# Download block level data from this URL: dsbprylw7ncuq.cloudfront.net/AF/africa_data.parquet
if (!file.exists(paste0("data/africa_data.parquet"))) {
  curl::multi_download("dsbprylw7ncuq.cloudfront.net/AF/africa_data.parquet", "complexity-analysis/input-data/africa_data.parquet", resume = TRUE) 
  } else {
  df_combined <- read_parquet(paste0('data/africa_data.parquet'))
}
  
# Create output directories
dir.create('data/complexity-analysis')
dir.create('data/complexity-analysis/data')
dir.create('data/complexity-analysis/viz')
wd_output = 'data/complexity-analysis'

# K-complexity groupings --------------------------------------------------

df_combined <- df_combined %>%
  mutate(k_5 = case_when(k_labels == 'Off-network' ~ 'Off network',
                         nearest_external_street_meters >= 200 ~ 'Off network', 
                         k_complexity >= 31 ~ "31+",
                         TRUE ~ as.character(k_complexity)),
         k_4 = case_when(k_labels == 'Off-network' ~ 'Off network',
                         nearest_external_street_meters >= 200 ~ 'Off network', 
                         k_complexity >= 11 & k_complexity <= 15 ~ "11 to 15", 
                         k_complexity >= 16 & k_complexity <= 20 ~ "16 to 20", 
                         k_complexity >= 21 ~ "21+",
                         TRUE ~ as.character(k_complexity)),
         k_3 = case_when(k_labels == 'Off-network' ~ 'Off network',
                         nearest_external_street_meters >= 200 ~ 'Off network', 
                         k_complexity >= 10 ~ "10+",
                         TRUE ~ as.character(k_complexity)),
         k_2 = case_when(k_labels == 'Off-network' ~ 'Off network',
                         nearest_external_street_meters >= 200 ~ 'Off network', 
                         k_complexity >= 1 & k_complexity <= 3 ~ "1 to 3",
                         k_complexity >= 4 & k_complexity <= 9 ~ "4 to 9",
                         k_complexity >= 10 ~ "10+"),
         k_1 = case_when(k_labels == 'Off-network' ~ 'Off network',
                         nearest_external_street_meters >= 200 ~ 'Off network', 
                         TRUE ~ as.character('Total')),
         k_0 = 'Total')


df_combined <- df_combined %>%
  mutate(class_urban_paper = case_when(class_urban_hierarchy == "1 - Core urban" ~ "1 - Urban",
                                       class_urban_hierarchy == "2 - Peripheral urban" ~ "2 - Secondary urban",
                                       class_urban_hierarchy == "3 - Peri-urban" ~ '3 - Peri-urban',
                                       class_urban_hierarchy == "4 - Non-urban" ~ '4 - Rural'))


# k_5_order = c('1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','16','17','18','19','20','21','22','23','24','25','26','27','28','29','30','31+','Off network')        
# k_4_order = c("1","2","3","4","5","6","7","8","9","10","11 to 15","16 to 20","21+","Off network")
# k_3_order = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10+", "Off network")
# k_2_order = c("1 to 3","4 to 9", "10+",'Off network')
# k_1_order = c("Total",'Off network')
# 
# df_combined_prep <- df_combined %>%
#   mutate(k_5 = factor(k_5, levels = k_5_order),
#          k_4 = factor(k_4, levels = k_4_order),
#          k_3 = factor(k_3, levels = k_3_order),
#          k_2 = factor(k_2, levels = k_2_order),
#          k_1 = factor(k_1, levels = k_1_order)) 

# Reduce data -----------------------------------------------------

group_cols = c("k_5", "k_4", "k_3", "k_2", "k_1", "k_0", "country_code", "country_name", "continent", "area_type", "class_urban_hierarchy", "class_urban_periurban_nonurban", "class_urban_nonurban", "urban_id", "urban_center_name", "urban_country_code", "urban_country_name", "conurbation_id", "conurbation_area_name_short", "conurbation_country_code", "conurbation_country_name", "agglosid", "agglosname", "metropole", "urban_layer_code",
               'class_urban_paper')
collapse_cols = c("block_count", "block_area_m2", "block_hectares", "block_area_km2", "block_perimeter_meters", "building_area_m2", "building_count", "average_building_area_m2", "building_to_block_area_ratio", "parcel_count", "average_parcel_area_m2", "k_complexity_weighted_landscan_un", "k_complexity_weighted_worldpop_un", "landscan_population", "landscan_population_un", "landscan_population_un_density_hectare", "landscan_population_un_per_building_area_m2", "landscan_population_un_per_building", "worldpop_population", "worldpop_population_un", "worldpop_population_un_density_hectare", "worldpop_population_un_per_building_area_m2", "worldpop_population_un_per_building", "on_network_street_length_meters", "off_network_street_length_meters", "nearest_external_street_meters", 
                  "bldg_area_count_bin_01_0.50__log10_3.2", "bldg_area_count_bin_02_0.75__log10_5.6", "bldg_area_count_bin_03_1.00__log10_10", "bldg_area_count_bin_04_1.25__log10_17.8", "bldg_area_count_bin_05_1.50__log10_31.6", "bldg_area_count_bin_06_1.75__log10_56.2", "bldg_area_count_bin_07_2.00__log10_100", "bldg_area_count_bin_08_2.25__log10_177.8", "bldg_area_count_bin_09_2.50__log10_316.2", "bldg_area_count_bin_10_2.75__log10_562.3", "bldg_area_count_bin_11_3.00__log10_1000", "bldg_area_count_bin_12_3.25__log10_1778.3", "bldg_area_count_bin_13_3.50__log10_3162.3", "bldg_area_count_bin_14_3.75__log10_5623.4", "bldg_area_count_bin_15_4.00__log10_10000", 
                  "bldg_area_m2_bin_01_0.50__log10_3.2", "bldg_area_m2_bin_02_0.75__log10_5.6", "bldg_area_m2_bin_03_1.00__log10_10", "bldg_area_m2_bin_04_1.25__log10_17.8", "bldg_area_m2_bin_05_1.50__log10_31.6", "bldg_area_m2_bin_06_1.75__log10_56.2", "bldg_area_m2_bin_07_2.00__log10_100", "bldg_area_m2_bin_08_2.25__log10_177.8", "bldg_area_m2_bin_09_2.50__log10_316.2", "bldg_area_m2_bin_10_2.75__log10_562.3", "bldg_area_m2_bin_11_3.00__log10_1000", "bldg_area_m2_bin_12_3.25__log10_1778.3", "bldg_area_m2_bin_13_3.50__log10_3162.3", "bldg_area_m2_bin_14_3.75__log10_5623.4", "bldg_area_m2_bin_15_4.00__log10_10000")

# Reduce the row count before aggregation
df_combined_prep <- df_combined %>% 
  mutate(block_count = 1) %>%
  group_by_at(group_cols) %>%
  summarize_at(vars(all_of(collapse_cols)), list(sum)) %>%
  ungroup() 
  
rm(df_combined)
gc()

# Data dictionary -----------------------------------------------------

file_labels = data.frame(
  column_names = c("group_var", "group_val", "continent", "average_building_area_m2", "average_parcel_area_m2", "average_buildings_per_hectare", "building_to_block_area_ratio", "k_complexity_weighted_landscan_un", "k_complexity_weighted_worldpop_un", "landscan_population_un_density_hectare", "landscan_population_un_per_building_area_m2", "landscan_population_un_per_building", "worldpop_population_un_density_hectare", "worldpop_population_un_per_building_area_m2", "worldpop_population_un_per_building",
                   "block_count", "block_area_m2", "block_hectares", "block_area_km2", "block_perimeter_meters", "building_area_m2", "building_count", "parcel_count", "landscan_population", "landscan_population_un", "worldpop_population", "worldpop_population_un", "on_network_street_length_meters", "off_network_street_length_meters", "nearest_external_street_meters",
                   "bldg_area_count_bin_01_0.50__log10_3.2", "bldg_area_count_bin_02_0.75__log10_5.6", "bldg_area_count_bin_03_1.00__log10_10", "bldg_area_count_bin_04_1.25__log10_17.8", "bldg_area_count_bin_05_1.50__log10_31.6", "bldg_area_count_bin_06_1.75__log10_56.2", "bldg_area_count_bin_07_2.00__log10_100", "bldg_area_count_bin_08_2.25__log10_177.8", "bldg_area_count_bin_09_2.50__log10_316.2", "bldg_area_count_bin_10_2.75__log10_562.3", "bldg_area_count_bin_11_3.00__log10_1000", "bldg_area_count_bin_12_3.25__log10_1778.3", "bldg_area_count_bin_13_3.50__log10_3162.3", "bldg_area_count_bin_14_3.75__log10_5623.4", "bldg_area_count_bin_15_4.00__log10_10000",
                   "bldg_area_m2_bin_01_0.50__log10_3.2", "bldg_area_m2_bin_02_0.75__log10_5.6", "bldg_area_m2_bin_03_1.00__log10_10", "bldg_area_m2_bin_04_1.25__log10_17.8", "bldg_area_m2_bin_05_1.50__log10_31.6", "bldg_area_m2_bin_06_1.75__log10_56.2", "bldg_area_m2_bin_07_2.00__log10_100", "bldg_area_m2_bin_08_2.25__log10_177.8", "bldg_area_m2_bin_09_2.50__log10_316.2", "bldg_area_m2_bin_10_2.75__log10_562.3", "bldg_area_m2_bin_11_3.00__log10_1000", "bldg_area_m2_bin_12_3.25__log10_1778.3", "bldg_area_m2_bin_13_3.50__log10_3162.3", "bldg_area_m2_bin_14_3.75__log10_5623.4", "bldg_area_m2_bin_15_4.00__log10_10000",
                   "landscan_population_un_log10", "landscan_population_un_density_hectare_log10", "worldpop_population_un_log10", "worldpop_population_un_density_hectare_log10",'agg_sum_landscan_population_un_group_by_agglos', 'agg_share_landscan_population_un_group_by_agglos', 'agg_sum_worldpop_population_un_group_by_agglos', 'agg_share_worldpop_population_un_group_by_agglos'),

  column_labels = c("Block complexity aggregation level (also called k-complexity)", "Block complexity value (also called k-complexity)", "Continent", "Average building footprint area per block (meters squared)", "Average parcel area per block (meters squared)", "Average buildings per hectare", "Building footprint area to block area ratio", "Average population weighted block complexity (LandScan 2020)", "Average population weighted block complexity (LandScan 2020)", "Population per hectare (LandScan 2020)", "Population per building footprint area (meters squared, LandScan 2020)", "Average building population (LandScan 2020)", "Population per hectare (WorldPop 2020)", "Population per building footprint area (meters squared, WorldPop 2020)", "Average building population (WorldPop 2020)",
                    "Block count", "Block area (meters squared)", "Block area in hectares", "Block area (kilometers squared)", "Block perimeter (meters)", "Building footprint area (meters squared)", "Building count", "Parcel count", "Population (LandScan 2020)", "Population (LandScan 2020, UN adjusted)", "Population (WorldPop 2020)", "Population (WorldPop 2020, UN adjusted)", "Length of internal streets within blocks connected to external street network (meters)", "Length of internal streets within blocks not connected to external street network (meters)", "Distance between block and external streets. Applies only in situations when block is disconnected from street networks (meters)",
                    "Count of buildings with area under 5.6 meters squared", "Count of buildings with area 5.6 to 10 meters squared", "Count of buildings with area 10 to 17.8 meters squared", "Count of buildings with area 17.8 to 31.6 meters squared", "Count of buildings with area 31.6 to 56.2 meters squared", "Count of buildings with area 56.2 to 100 meters squared", "Count of buildings with area 100 to 177.8 meters squared", "Count of buildings with area 177.8 to 316.2 meters squared", "Count of buildings with area 316.2 to 562.3 meters squared", "Count of buildings with area 562.3 to 1,000 meters squared", "Count of buildings with area 1,000 to 1,778.3 meters squared", "Count of buildings with area 1,778.3 to 3,162.3 meters squared", "Count of buildings with area 3,162.3 to 5,623.4 meters squared", "Count of buildings with area 5,623.4 to 10,000 meters squared", "Count of buildings with area over 10,000 meters squared",
                    "Cumulative area of buildings under 5.6 meters squared", "Cumulative area of buildings 5.6 to 10 meters squared", "Cumulative area of buildings 10 to 17.8 meters squared", "Cumulative area of buildings 17.8 to 31.6 meters squared", "Cumulative area of buildings 31.6 to 56.2 meters squared", "Cumulative area of buildings 56.2 to 100 meters squared", "Cumulative area of buildings 100 to 177.8 meters squared", "Cumulative area of buildings 177.8 to 316.2 meters squared", "Cumulative area of buildings 316.2 to 562.3 meters squared", "Cumulative area of buildings 562.3 to 1,000 meters squared", "Cumulative area of buildings 1,000 to 1,778.3 meters squared", "Cumulative area of buildings 1,778.3 to 3,162.3 meters squared", "Cumulative area of buildings 3,162.3 to 5,623.4 meters squared", "Cumulative area of buildings 5,623.4 to 10,000 meters squared", "Cumulative area of buildings over 10,000 meters squared",
                    "Log base 10 population (LandScan 2020, UN adjusted)", "Log base 10 population per hectare (LandScan 2020, UN adjusted)", "Log base 10 population (WorldPop 2020, UN adjusted)", "Log base 10 population per hectare (WorldPop 2020, UN adjusted)", 'Aggregate sum of population (LandScan 2020) by {geographic grouping}', 'Share of population (LandScan 2020) by {geographic grouping}', 'Aggregate sum of population (WorldPop 2020) by {geographic grouping}', 'Share of population (WorldPop 2020) by {geographic grouping}'))

file_group_labels = data.frame(
  column_names = c('k_5', 'k_4', 'k_3', 'k_2', 'k_1', 'k_0', 'country_code', 'country_name', 'continent', 'area_type', 'class_urban_hierarchy', 'class_urban_periurban_nonurban', 'class_urban_nonurban', 'urban_id', 'urban_center_name', 'urban_country_code', 'urban_country_name', 'conurbation_id', 'conurbation_area_name_short', 'conurbation_country_code', 'conurbation_country_name', 'agglosid', 'agglosname', 'metropole', 'urban_layer_code'),
  column_labels = c("Block complexity (range is 1 to 30+, length of 32)", "Block complexity (range is 1 to 21+, length of 14)", "Block complexity (range is 1 to 10+, length of 11)", "Block complexity (4-categories)", "Block complexity (2-categories)", "Total (1-category)", "Country code (ISO 3166-1 alpha-3)", "Country name", "Continent", "Urban classification (Urban, Peri-urban, Non-urban)", "4-way urban classification ('1 - Core urban', '2 - Peripheral urban', '3 - Peri-urban', '4 - Non-urban')", "3-way urban classification ('1 - Core & peripheral urban', '2 - Peri-urban', '3 - Non-urban')", "2-way urban classification ('1 - Core, peripheral, & peri-urban', '2 - Non-urban')", "Urban center ID (based on GHSL)", "Urban center name (based on GHSL)", "Country code of urban center", "Country name of urban center", "Conurbation ID (10km buffer around urban centers)", "Conurbation name", "Country code of conurbation", "Country name of conurbation", "Agglomeration ID (based on Africapolis)", "Agglomeration name (based on Africapolis)", "Metropole (based on Africapolis)", "Urban layer code (matches to region_data)")) %>%
  mutate(grouping = case_when(column_names %in% c('k_5', 'k_4', 'k_3', 'k_2', 'k_1', 'k_0') ~ 'Block complexity aggregation levels', TRUE ~ as.character('Geographic aggregation levels')))

write_xlsx(list('dictionary_labels' = file_labels, 'dictionary_levels' = file_group_labels), 
           col_names = TRUE, format_headers = TRUE, path = paste0(wd_output,'/data/dictionary.xlsx'))


# Aggregation arguments ---------------------------------------------------

log_10 <- function(x) {
  x = replace_na(na_if(na_if(na_if(log10(x), NaN), Inf), -Inf), 0)
  x = ifelse(x < 1, 1, x)
  return(x)
}

share <- function(x) {
  x = replace_na(na_if(na_if(na_if(x / sum(x), NaN), Inf), -Inf), 0)
  return(x)
}

sum_cols = c("k_complexity_weighted_landscan_un", "k_complexity_weighted_worldpop_un", 
             "block_count", "block_area_m2", "block_hectares", "block_area_km2", "block_perimeter_meters", "building_area_m2", 
             "building_count", "parcel_count", "landscan_population", "landscan_population_un", "worldpop_population", "worldpop_population_un", 
             "on_network_street_length_meters", "off_network_street_length_meters", "nearest_external_street_meters", 
             "bldg_area_count_bin_01_0.50__log10_3.2", "bldg_area_count_bin_02_0.75__log10_5.6", "bldg_area_count_bin_03_1.00__log10_10", "bldg_area_count_bin_04_1.25__log10_17.8", "bldg_area_count_bin_05_1.50__log10_31.6", "bldg_area_count_bin_06_1.75__log10_56.2", "bldg_area_count_bin_07_2.00__log10_100", "bldg_area_count_bin_08_2.25__log10_177.8", "bldg_area_count_bin_09_2.50__log10_316.2", "bldg_area_count_bin_10_2.75__log10_562.3", "bldg_area_count_bin_11_3.00__log10_1000", "bldg_area_count_bin_12_3.25__log10_1778.3", "bldg_area_count_bin_13_3.50__log10_3162.3", "bldg_area_count_bin_14_3.75__log10_5623.4", "bldg_area_count_bin_15_4.00__log10_10000", 
             "bldg_area_m2_bin_01_0.50__log10_3.2", "bldg_area_m2_bin_02_0.75__log10_5.6", "bldg_area_m2_bin_03_1.00__log10_10", "bldg_area_m2_bin_04_1.25__log10_17.8", "bldg_area_m2_bin_05_1.50__log10_31.6", "bldg_area_m2_bin_06_1.75__log10_56.2", "bldg_area_m2_bin_07_2.00__log10_100", "bldg_area_m2_bin_08_2.25__log10_177.8", "bldg_area_m2_bin_09_2.50__log10_316.2", "bldg_area_m2_bin_10_2.75__log10_562.3", "bldg_area_m2_bin_11_3.00__log10_1000", "bldg_area_m2_bin_12_3.25__log10_1778.3", "bldg_area_m2_bin_13_3.50__log10_3162.3", "bldg_area_m2_bin_14_3.75__log10_5623.4", "bldg_area_m2_bin_15_4.00__log10_10000")

divide_cols = list('average_building_area_m2' = c('building_area_m2','building_count'),
                   'average_parcel_area_m2' = c('block_area_m2','parcel_count'),
                   'average_buildings_per_hectare' = c('building_count','block_hectares'),
                   'building_to_block_area_ratio' = c('building_area_m2','block_area_m2'),
                   'k_complexity_weighted_landscan_un' = c('k_complexity_weighted_landscan_un','landscan_population_un'),
                   'k_complexity_weighted_worldpop_un' = c('k_complexity_weighted_worldpop_un','worldpop_population_un'),
                   'landscan_population_un_density_hectare' = c('landscan_population_un','block_hectares'),
                   'landscan_population_un_per_building_area_m2' = c('landscan_population_un','building_area_m2'),
                   'landscan_population_un_per_building' = c('landscan_population_un','building_count'),
                   'worldpop_population_un_density_hectare' = c('worldpop_population_un','block_hectares'),
                   'worldpop_population_un_per_building_area_m2' = c('worldpop_population_un','building_area_m2'),
                   'worldpop_population_un_per_building' = c('worldpop_population_un','building_count'))

transform_cols = c('landscan_population_un', 'landscan_population_un_density_hectare', 
                   'worldpop_population_un', 'worldpop_population_un_density_hectare')

country_cols <- c("country_code", "country_name")
conurban_cols <- c("conurbation_id", "conurbation_area_name_short", "conurbation_country_code", "conurbation_country_name")
urban_cols <- c("urban_id", "urban_center_name", "urban_country_code", "urban_country_name")
agglos_cols <- c("agglosid", "agglosname", "metropole","country_code", "country_name")

# df_combined_prep %>%
#   select(country_code, country_name) %>%
#   distinct() %>%
#   arrange(country_code) %>%
#   print(n = 100)

# Continent ---------------------------------------------------------------

continent_df <- generate_crosstabs(data = df_combined_prep, 
                                   group_by_cols = c("continent"), 
                                   crosstab_cols = c("k_5", "k_4", "k_3", "k_2", "k_1", "k_0"),
                                   sum_cols = sum_cols, divide_cols = divide_cols, 
                                   transform_cols = transform_cols, transform_func_list = list(log10 = log_10),
                                   group_by_agg_cols_list = list('continent' = 'continent'),
                                   group_by_agg_func_list = list(sum = sum, share = share),
                                   agg_cols = c('landscan_population_un', 'worldpop_population_un'))

continent_area4_df <- generate_crosstabs(data = df_combined_prep, 
                                         group_by_cols = c("continent", "class_urban_hierarchy"), 
                                         crosstab_cols = c("k_5", "k_4", "k_3", "k_2", "k_1", "k_0"),
                                         sum_cols = sum_cols, divide_cols = divide_cols, 
                                         transform_cols = transform_cols, transform_func_list = list(log10 = log_10),
                                         group_by_agg_cols_list = list('continent' = c("continent"),
                                                                       'continent_4way' = c("continent", "class_urban_hierarchy")),
                                         group_by_agg_func_list = list(sum = sum, share = share),
                                         agg_cols = c('landscan_population_un', 'worldpop_population_un'))

continent_area3_df <- generate_crosstabs(data = df_combined_prep, 
                                         group_by_cols = c("continent", "class_urban_periurban_nonurban"), 
                                         crosstab_cols = c("k_5", "k_4", "k_3", "k_2", "k_1", "k_0"),
                                         sum_cols = sum_cols, divide_cols = divide_cols, 
                                         transform_cols = transform_cols, transform_func_list = list(log10 = log_10),
                                         group_by_agg_cols_list = list('continent' = c("continent"),
                                                                       'continent_3way' = c("continent", "class_urban_periurban_nonurban")),
                                         group_by_agg_func_list = list(sum = sum, share = share),
                                         agg_cols = c('landscan_population_un', 'worldpop_population_un'))

continent_area2_df <- generate_crosstabs(data = df_combined_prep, 
                                         group_by_cols = c("continent", "class_urban_nonurban"), 
                                         crosstab_cols = c("k_5", "k_4", "k_3", "k_2", "k_1", "k_0"),
                                         sum_cols = sum_cols, divide_cols = divide_cols, 
                                         transform_cols = transform_cols, transform_func_list = list(log10 = log_10),
                                         group_by_agg_cols_list = list('continent' = c("continent"),
                                                                       'continent_3way' = c("continent", "class_urban_nonurban")),
                                         group_by_agg_func_list = list(sum = sum, share = share),
                                         agg_cols = c('landscan_population_un', 'worldpop_population_un'))

write_xlsx(list('continent_all' = continent_df, 'continent_4way' = continent_area4_df, 
                'continent_3way' = continent_area3_df, 'continent_2way' = continent_area2_df, 
                'dictionary_labels' = file_labels, 'dictionary_levels' = file_group_labels), 
           col_names = TRUE, format_headers = TRUE, path = paste0(wd_output,'/data/continent_data.xlsx'))

rm(continent_df, continent_area4_df, continent_area3_df, continent_area2_df)

# Country -----------------------------------------------------------------

country_df <- generate_crosstabs(data = df_combined_prep, 
                                 group_by_cols = c(country_cols), 
                                 crosstab_cols = c("k_4", "k_3", "k_2", "k_1", "k_0"),
                                 sum_cols = sum_cols, divide_cols = divide_cols, 
                                 transform_cols = transform_cols, transform_func_list = list(log10 = log_10),
                                 group_by_agg_cols_list = list('country' = "country_code"),
                                 group_by_agg_func_list = list(sum = sum, share = share),
                                 agg_cols = c('landscan_population_un', 'worldpop_population_un'))

country_area4_df <- generate_crosstabs(data = df_combined_prep, 
                                       group_by_cols = c(country_cols, "class_urban_hierarchy"),
                                       crosstab_cols = c("k_4", "k_3", "k_2", "k_1", "k_0"),
                                       sum_cols = sum_cols, divide_cols = divide_cols, 
                                       transform_cols = transform_cols, transform_func_list = list(log10 = log_10),
                                       group_by_agg_cols_list = list('country' = "country_code",
                                                                     'country_4way' = c("country_code", "class_urban_hierarchy")),
                                       group_by_agg_func_list = list(sum = sum, share = share),
                                       agg_cols = c('landscan_population_un', 'worldpop_population_un'))

country_area3_df <- generate_crosstabs(data = df_combined_prep, 
                                       group_by_cols =  c(country_cols, "class_urban_periurban_nonurban"), 
                                       crosstab_cols = c("k_4", "k_3", "k_2", "k_1", "k_0"),
                                       sum_cols = sum_cols, divide_cols = divide_cols, 
                                       transform_cols = transform_cols, transform_func_list = list(log10 = log_10),
                                       group_by_agg_cols_list = list('country' = "country_code",
                                                                     'country_3way' = c("country_code", "class_urban_periurban_nonurban")),
                                       group_by_agg_func_list = list(sum = sum, share = share),
                                       agg_cols = c('landscan_population_un', 'worldpop_population_un'))

country_area2_df <- generate_crosstabs(data = df_combined_prep, 
                                       group_by_cols = c(country_cols, "class_urban_nonurban"), 
                                       crosstab_cols = c("k_4", "k_3", "k_2", "k_1", "k_0"),
                                       sum_cols = sum_cols, divide_cols = divide_cols, 
                                       transform_cols = transform_cols, transform_func_list = list(log10 = log_10),
                                       group_by_agg_cols_list = list('country' = "country_code",
                                                                     'country_2way' = c("country_code", "class_urban_nonurban")),
                                       group_by_agg_func_list = list(sum = sum, share = share),
                                       agg_cols = c('landscan_population_un', 'worldpop_population_un'))

write_xlsx(list('country_all' = country_df, 'country_4way' = country_area4_df, 
                'country_3way' = country_area3_df, 'country_2way' = country_area2_df), 
           col_names = TRUE, format_headers = TRUE, path = paste0(wd_output,'/data/country_data.xlsx'))

rm(country_df, country_area4_df, country_area3_df, country_area2_df)

# Urban -------------------------------------------------------------------

urban_df <- generate_crosstabs(data = df_combined_prep %>% filter(class_urban_hierarchy %in% c('1 - Core urban', '2 - Peripheral urban')), 
                               group_by_cols = c(urban_cols, "class_urban_hierarchy"),
                               crosstab_cols = c("k_3", "k_2", "k_1", "k_0"),
                               sum_cols = sum_cols, divide_cols = divide_cols, 
                               transform_cols = transform_cols, transform_func_list = list(log10 = log_10),
                               group_by_agg_cols_list = list('urban_center' = "urban_id"),
                               group_by_agg_func_list = list(sum = sum, share = share),
                               agg_cols = c('landscan_population_un', 'worldpop_population_un'))

write_excel_csv(x = urban_df, file = paste0(wd_output,'/data/urban_data.csv'))

rm(urban_df)

# Conurbations ------------------------------------------------------------

conurban_df <- generate_crosstabs(data = df_combined_prep %>% 
                                    filter(class_urban_hierarchy %in% c('1 - Core urban', '2 - Peripheral urban', '3 - Peri-urban')), 
                                  group_by_cols = c(conurban_cols),
                                  crosstab_cols = c("k_3", "k_2", "k_0"),
                                  sum_cols = sum_cols, divide_cols = divide_cols, 
                                  transform_cols = transform_cols, transform_func_list = list(log10 = log_10),
                                  group_by_agg_cols_list = list('conurbation' = "conurbation_id"),
                                  group_by_agg_func_list = list(sum = sum, share = share),
                                  agg_cols = c('landscan_population_un', 'worldpop_population_un'))

conurban_area4_df <- generate_crosstabs(data = df_combined_prep %>% 
                                          filter(class_urban_hierarchy %in% c('1 - Core urban', '2 - Peripheral urban', '3 - Peri-urban')), 
                                        group_by_cols = c(conurban_cols, "class_urban_hierarchy"),
                                        crosstab_cols = c("k_3", "k_2", "k_0"),
                                        sum_cols = sum_cols, divide_cols = divide_cols, 
                                        transform_cols = transform_cols, transform_func_list = list(log10 = log_10),
                                        group_by_agg_cols_list = list('conurbation' = "conurbation_id",
                                                                      'conurbation_4way' = c("conurbation_id","class_urban_hierarchy")),
                                        group_by_agg_func_list = list(sum = sum, share = share),
                                        agg_cols = c('landscan_population_un', 'worldpop_population_un'))

conurban_area3_df <- generate_crosstabs(data = df_combined_prep %>% 
                                          filter(class_urban_hierarchy %in% c('1 - Core urban', '2 - Peripheral urban', '3 - Peri-urban')), 
                                        group_by_cols = c(conurban_cols, "class_urban_periurban_nonurban"), 
                                        crosstab_cols = c("k_3", "k_2", "k_0"),
                                        sum_cols = sum_cols, divide_cols = divide_cols, 
                                        transform_cols = transform_cols, transform_func_list = list(log10 = log_10),
                                        group_by_agg_cols_list = list('conurbation' = "conurbation_id",
                                                                      'conurbation_3way' = c("conurbation_id", "class_urban_periurban_nonurban")),
                                        group_by_agg_func_list = list(sum = sum, share = share),
                                        agg_cols = c('landscan_population_un', 'worldpop_population_un'))

write_excel_csv(x = conurban_df, file = paste0(wd_output,'/data/conurban_data.csv'))
write_excel_csv(x = conurban_area4_df, file = paste0(wd_output,'/data/conurban_data_core_periph_peri.csv'))
write_excel_csv(x = conurban_area3_df, file = paste0(wd_output,'/data/conurban_data_urban_peri.csv'))

rm(conurban_df, conurban_area4_df, conurban_area3_df)

# Africapolis agglomerations ----------------------------------------------

agglos_df <- generate_crosstabs(data = df_combined_prep,
                                group_by_cols = agglos_cols, 
                                crosstab_cols = c("k_3", "k_2", "k_0"),
                                sum_cols = sum_cols, divide_cols = divide_cols, 
                                transform_cols = transform_cols, transform_func_list = list(log10 = log_10),
                                group_by_agg_cols_list = list('agglos' = c("agglosname","country_code")),
                                group_by_agg_func_list = list(sum = sum, share = share),
                                agg_cols = c('landscan_population_un', 'worldpop_population_un'))

agglos_df <- agglos_df %>% filter(agg_sum_landscan_population_un_group_by_agglos >= 50000)
write_excel_csv(x = agglos_df, file = paste0(wd_output,'/data/agglomeration_data.csv'))

rm(agglos_df)

# Visualization -----------------------------------------------------------


# World countries ---------------------------------------------------------

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
  filter(ISO_A3 %in% iso_code_list) %>% 
  select(ISO_A3) %>%
  mutate(lon = map_dbl(geometry, ~st_point_on_surface(.x)[[1]]),
         lat = map_dbl(geometry, ~st_point_on_surface(.x)[[2]]))

# Scatter pie chart -------------------------------------------------------

# By country
scatterpie_by_country_data <- generate_crosstabs(data = df_combined_prep, 
                                     group_by_cols = c(country_cols), 
                                     crosstab_cols = c("k_2"),
                                     sum_cols = sum_cols, divide_cols = divide_cols, 
                                     transform_cols = transform_cols, transform_func_list = list(log10 = log_10),
                                     group_by_agg_cols_list = list('country' = "country_code"),
                                     group_by_agg_func_list = list(sum = sum, share = share),
                                     agg_cols = c('landscan_population_un', 'worldpop_population_un'))

scatterpie_by_country_data <- scatterpie_by_country_data %>%
  select(country_name, country_code, group_val, landscan_population_un) %>%
  pivot_wider(id_cols = c(country_name, country_code), names_from = c(group_val), values_from = c(landscan_population_un)) %>%
  mutate(total = (`1 to 3` + `4 to 9` + `10+` + `Off network`),
         radius =  sqrt((total)/pi)/2000,
         radius_scaled = case_when(radius <= 1.4 ~ 1.4, radius >= 4 ~ 4, TRUE ~ as.numeric(radius)))

scatterpie_by_country_data <- subsaharan_africa %>%
  left_join(., scatterpie_by_country_data, by = c('ISO_A3'='country_code')) %>%
  mutate(lat = case_when(ISO_A3 == 'BEN' ~ lat + 1, ISO_A3 == 'TGO' ~ lat - 2.8, ISO_A3 == 'BDI' ~ lat - 1.3, ISO_A3 == 'CMR' ~ lat - 2, ISO_A3 == 'LBR' ~ lat - 1, ISO_A3 == 'GHA' ~ lat + .4, ISO_A3 == 'GNB' ~ lat - 1, ISO_A3 == 'RWA' ~ lat + .6, ISO_A3 == 'SEN' ~ lat + 1, TRUE ~ lat),
         lon = case_when(ISO_A3 == 'ZAF' ~ lon - 1.8, ISO_A3 == 'GMB' ~ lon - 2, ISO_A3 == 'SLE' ~ lon - 1, ISO_A3 == 'CMR' ~ lon - 1, ISO_A3 == 'ESH' ~ lon - 1, ISO_A3 == 'GNQ' ~ lon - 1, ISO_A3 == 'DJI' ~ lon + .5, TRUE ~ lon))

grey2 <- c('#414141') 
kcat = 4
colorhexes <- colorRampPalette(c('#93328E','#CF3F80','#F7626B','#FF925A','#FFC556','#F9F871'))(kcat-1)

(scatterpie_by_country <- ggplot() + 
    geom_sf(data = scatterpie_by_country_data, fill = '#D3D3D3', color = 'white', linewidth = .75) +
    geom_scatterpie(data = scatterpie_by_country_data %>% st_drop_geometry(), aes(x=lon, y=lat, group=country_name, r = radius_scaled), cols = c("1 to 3","4 to 9","10+","Off network"),  color='white', size = .3, alpha=1) +
    labs(subtitle = 'Block complexity\nby country population') + 
    geom_scatterpie_legend(scatterpie_by_country_data %>% st_drop_geometry() %>% filter(total >= 50000000) %>% pull(radius), x=-5, y=-20, n = 2, labeller=function(x) ifelse(((x*2000)^2)*pi > 1000000, paste0(round((((x*2000)^2)*pi )/1000000,-1),'M'), paste0(round((((x*2000)^2)*pi )/1000,-1),'K'))) +
    geom_label(data = scatterpie_by_country_data, aes(x = lon, y = lat, label = ISO_A3), label.padding =  unit(0.1, "lines"), label.size = 0, label.r =  unit(0.1, "lines"), fill = 'black', color = "white", size =1.7, fontface = 'bold') +
    scale_fill_manual(values = c(grey2, colorhexes), name = 'Block complexity') + 
    theme_void() + theme(plot.subtitle = element_text(size = 15, face="bold", hjust=.5, vjust = -5)))

# By country + conurbation

scatterpie_by_conurban_data <- generate_crosstabs(data = df_combined_prep, 
                                     group_by_cols = c(country_cols, 'class_urban_nonurban'), 
                                     crosstab_cols = c("k_2"),
                                     sum_cols = sum_cols, divide_cols = divide_cols, 
                                     transform_cols = transform_cols, transform_func_list = list(log10 = log_10),
                                     group_by_agg_cols_list = list('country' = "country_code"),
                                     group_by_agg_func_list = list(sum = sum, share = share),
                                     agg_cols = c('landscan_population_un', 'worldpop_population_un'))

scatterpie_by_conurban_data <- scatterpie_by_conurban_data %>%
  filter(class_urban_nonurban == "1 - Core, peripheral, & peri-urban") %>%
  select(country_name, country_code, group_val, landscan_population_un) %>%
  pivot_wider(id_cols = c(country_name, country_code), names_from = c(group_val), values_from = c(landscan_population_un), values_fill = 0 ) %>%
  mutate(total = (`1 to 3` + `4 to 9` + `10+` + `Off network`),
         radius =  sqrt((total)/pi)/2000,
         radius_scaled = case_when(radius <= 1.4 ~ 1.4, radius >= 4 ~ 4, TRUE ~ as.numeric(radius)))

scatterpie_by_conurban_data <- subsaharan_africa %>%
  left_join(.,  scatterpie_by_conurban_data, by = c('ISO_A3'='country_code')) %>%
  mutate(lat = case_when(ISO_A3 == 'BEN' ~ lat + 1, ISO_A3 == 'TGO' ~ lat - 2.8, ISO_A3 == 'BDI' ~ lat - 1.3, ISO_A3 == 'CMR' ~ lat - 2, ISO_A3 == 'LBR' ~ lat - 1, ISO_A3 == 'GHA' ~ lat + .4, ISO_A3 == 'GNB' ~ lat - 1, ISO_A3 == 'RWA' ~ lat + .6, ISO_A3 == 'SEN' ~ lat + 1, TRUE ~ lat),
         lon = case_when(ISO_A3 == 'ZAF' ~ lon - 1.8, ISO_A3 == 'GMB' ~ lon - 2, ISO_A3 == 'SLE' ~ lon - 1, ISO_A3 == 'CMR' ~ lon - 1, ISO_A3 == 'ESH' ~ lon - 1, ISO_A3 == 'GNQ' ~ lon - 1, ISO_A3 == 'DJI' ~ lon + .5, TRUE ~ lon))

grey2 <- c('#414141') 
kcat = 4
colorhexes <- colorRampPalette(c('#93328E','#CF3F80','#F7626B','#FF925A','#FFC556','#F9F871'))(kcat-1)

(scatterpie_by_conurban <- ggplot() + 
    geom_sf(data =  scatterpie_by_conurban_data, fill = '#D3D3D3',
            color = 'white', size = .75) +
    geom_scatterpie(data = scatterpie_by_conurban_data %>% st_drop_geometry(), 
                    aes(x=lon, y=lat, group=country_name, r = radius_scaled), 
                    cols = c("1 to 3","4 to 9","10+","Off network"),  color='white', size = .3, alpha=1) +
    labs(subtitle = 'Block complexity by\ncountry conurban population') + 
    geom_scatterpie_legend(scatterpie_by_conurban_data %>% st_drop_geometry() %>% filter(total >= 50000000) %>% pull(radius), x=-5, y=-20, n = 2, labeller=function(x) ifelse(((x*2000)^2)*pi > 1000000, paste0(round((((x*2000)^2)*pi )/1000000,-1),'M'), paste0(round((((x*2000)^2)*pi )/1000,-1),'K'))) +
    geom_label(data = scatterpie_by_conurban_data, aes(x = lon, y = lat, label = ISO_A3), label.padding =  unit(0.1, "lines"), label.size = 0, label.r =  unit(0.1, "lines"), fill = 'black', color = "white", size =1.7, fontface = 'bold') +
    scale_fill_manual(values = c(grey2,colorhexes), name = 'Block complexity') + 
    theme_void() + 
    theme(plot.margin=unit(c(t=3,r=20,b=5,l=5), "pt"),
          plot.subtitle = element_text(size = 15, face="bold", hjust=.5, vjust = -5)))

(scatterpie_charts <- scatterpie_by_country + scatterpie_by_conurban + 
    plot_layout(guides = 'collect') &
    plot_annotation(tag_levels = list(c("A","B"))) & 
    theme(plot.tag = element_text(size = 13))
  )

ggsave(plot = scatterpie_charts, filename = paste0(wd_output,'/viz/scatterpie_charts.pdf'), width = 12.4, height = 5.64)

rm(scatterpie_charts, scatterpie_by_country, scatterpie_by_conurban, scatterpie_by_country_data, scatterpie_by_conurban_data)
   

# Scatter by country-4way -------------------------------------------------

scatter_by_country_4way = generate_crosstabs(data = df_combined_prep, 
                                group_by_cols = c(country_cols, 'class_urban_paper'), 
                                crosstab_cols = c("k_0"),
                                sum_cols = sum_cols, divide_cols = divide_cols, 
                                transform_cols = transform_cols, transform_func_list = list(log10 = log_10),
                                group_by_agg_cols_list = list('country' = "country_code"),
                                group_by_agg_func_list = list(sum = sum, share = share),
                                agg_cols = c('landscan_population_un', 'worldpop_population_un'))

scatter_by_country_4way <- scatter_by_country_4way %>%
  mutate(country_name =  (gsub('Democratic Republic of the Congo', 'DR Congo', as.character(country_name))),
         country_name =  (gsub('Central African Republic', 'Centrafrique', as.character(country_name))),
         country_name =  (gsub('Sao Tome and Principe', 'São Tomé e Príncipe', as.character(country_name)))) %>%
  mutate(class_urban_hierarchy = factor(class_urban_paper, levels = c("1 - Urban","2 - Secondary urban","3 - Peri-urban","4 - Rural"))) %>% 
  mutate(k_reweight = landscan_population_un*k_complexity_weighted_landscan_un) %>%
  group_by(country_name) %>%
  mutate(k_reweight = sum(k_reweight)) %>%
  ungroup() %>%
  mutate(k_reweight = k_reweight/agg_sum_landscan_population_un_group_by_country)

# Scatter population, size, k-complexity
(scatter_by_country_4way_1 <- ggplot() +
    geom_point(data = scatter_by_country_4way %>% filter(landscan_population_un >= 10000), aes(x = reorder(country_name, (agg_sum_landscan_population_un_group_by_country)), y = log10(landscan_population_un), 
               fill = class_urban_paper, color = class_urban_paper, size = k_complexity_weighted_landscan_un), alpha = .75) +
    scale_size(range = c(1,10)) + 
    geom_text(data = scatter_by_country_4way %>% filter(landscan_population_un >= 10000), aes(x =  reorder(country_name, (agg_sum_landscan_population_un_group_by_country)), y = log10(landscan_population_un), label = round(k_complexity_weighted_landscan_un, 1)), size = 3, vjust = .5, color = '#333333', fontface='bold', check_overlap = TRUE) +
    scale_y_continuous(oob = scales::squish, breaks= c(1,2,3,4,5,6,7,8,9), labels = c('0',"100","1K","10K","100K","1M","10M","100M","1B"))+
    scale_fill_manual(values = c('#08519c','#9ecae1','#006d2c','#a1d99b')) +
    scale_color_manual(values = c('#08519c','#9ecae1','#006d2c','#a1d99b')) +
    labs(x= '', y = 'Population', size = 'Average block complexity', fill = '', color =  '') +
    coord_flip() +
    guides(color = guide_legend(override.aes = list(size = 8, alpha = 1))) +
    theme(legend.key = element_rect(fill = NA), legend.spacing.y = unit(0, "cm"),
          text = element_text(size = 13), legend.position = 'bottom', legend.box = 'vertical', legend.box.just = "center") +
    theme_minimal())

# Scatter population, size, k-complexity
(scatter_by_country_4way_2 <- ggplot() +
    geom_point(data = scatter_by_country_4way, aes(x = reorder(country_name, (k_reweight)), y = k_complexity_weighted_landscan_un, 
                                                   fill = class_urban_paper, color =  class_urban_paper, size = landscan_population_un), alpha = .75) +
    geom_text(data = scatter_by_country_4way %>% filter(landscan_population_un >= 10000), aes(x =  reorder(country_name, (k_reweight)), y =  k_complexity_weighted_landscan_un, label = ifelse(landscan_population_un >= 1000000, paste0(round(landscan_population_un/1000000,0),'M'),'' ) ), size = 3, vjust = .5, color = '#333333', fontface='bold', check_overlap = TRUE) + 
    labs(x= '', y = 'Average block complexity', size = 'Population', fill = '', color =  '') +
    coord_flip() +
    scale_fill_manual(values = c('#08519c','#9ecae1','#006d2c','#a1d99b')) +
    scale_color_manual(values = c('#08519c','#9ecae1','#006d2c','#a1d99b')) +
    scale_size_continuous(range = c(1,10), labels = label_comma(accuracy = 1L, scale =  0.000001, suffix = "M") ) +
    guides(color = guide_legend(override.aes = list(size = 8, alpha = 1))) +
    theme(legend.key = element_rect(fill = NA), legend.spacing.y = unit(0, "cm"),
          text = element_text(size = 13), legend.position = 'bottom', legend.box = 'vertical', legend.box.just = "center") +
    theme_minimal())

# Scatter population, size, k-complexity
(scatter_by_country_4way_3 <- ggplot() +
    geom_point(data = scatter_by_country_4way %>% filter(agg_sum_landscan_population_un_group_by_country >= 1000000, landscan_population_un >= 10000), 
               aes(x = log10(k_complexity_weighted_landscan_un), y = log10(landscan_population_un), 
                   fill = class_urban_paper, color =  class_urban_paper, size = landscan_population_un), alpha = .7) +
    coord_flip() +
    labs(x= 'Average block complexity', y = 'Population', size = '', fill = '', color =  '') +
    scale_fill_manual(values = c('#08519c','#9ecae1','#006d2c','#a1d99b'), guide = 'none') +
    scale_color_manual(values = c('#08519c','#9ecae1','#006d2c','#a1d99b'), guide = 'none') +
    scale_size(range = c(2, 20), breaks = c(10000,100000,1000000,10000000), labels = c('10K','100K','1M','10M')) +
    scale_x_continuous(oob = scales::squish, breaks= c(0, 0.30103, 0.4771213, 0.60206, 0.69897, 0.7781513, 0.845098, 0.90309, 0.9542425, 1,1.041393,1.079181,1.113943,1.146128,1.176091,1.20412,1.230449,1.255273,1.278754,1.30103,1.322219,1.342423,1.361728,1.380211,1.39794), labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25) )+
    scale_y_continuous(oob = scales::squish, breaks= c(1,2,3,4,5,6,7,8,9), labels = c('0',"100","1K","10K","100K","1M","10M","100M","1B"))+
    geom_text_repel(data = scatter_by_country_4way %>% filter(agg_sum_landscan_population_un_group_by_country >= 1000000, landscan_population_un >= 10000), 
                    seed = 1, segment.curvature = -0.1, point.padding = 0, box.padding = 0.4, max.iter = 30000, segment.square  = FALSE, segment.inflect = FALSE, 
                    min.segment.length = 0, max.overlaps = Inf, force = .1, force_pull = 10, aes(x = log10(k_complexity_weighted_landscan_un), y = log10(landscan_population_un), label = country_name), 
                    size = 3, vjust =.5, color = '#333333', fontface='bold') + 
    guides(color = guide_legend(override.aes = list(size = 6, alpha =1))) + ##
    theme(legend.key = element_rect(fill = NA), panel.background = element_rect(fill = "white"), 
          legend.spacing.y = unit(0, "cm"),
          axis.ticks = element_line(colour = "grey", linewidth = .2), 
          panel.grid.major = element_line(colour = "grey", linewidth = .2), 
          legend.text = element_text(size = 13, color = '#333333'),
          axis.text = element_text(size = 13, color = '#333333'),
          text = element_text(size = 13, color = '#333333'), legend.position = 'bottom', legend.box = 'vertical', legend.box.just = "center"))

ggsave(plot = scatter_by_country_4way_1, filename = paste0(wd_output,'/viz/scatter_population_country.pdf'), width = 8, height = 9)
ggsave(plot = scatter_by_country_4way_2, filename = paste0(wd_output,'/viz/scatter_complexity_country.pdf'), width = 8, height = 9)
ggsave(plot = scatter_by_country_4way_3, filename = paste0(wd_output,'/viz/scatter_population_vs_complexity_country.pdf'), height = 10, width = 12.9)

rm(scatter_by_country_4way, scatter_by_country_4way_1, scatter_by_country_4way_2, scatter_by_country_4way_3)

# Histograms --------------------------------------------------------------

histogram_africa_data <- generate_crosstabs(data = df_combined_prep, 
                                       group_by_cols = c('continent', 'class_urban_paper'), 
                                       crosstab_cols = c("k_5"),
                                       sum_cols = sum_cols, divide_cols = divide_cols, 
                                       transform_cols = transform_cols, transform_func_list = list(log10 = log_10),
                                       group_by_agg_cols_list = list('continent' = "continent"),
                                       group_by_agg_func_list = list(sum = sum, share = share),
                                       agg_cols = c('landscan_population_un', 'worldpop_population_un'))

histogram_africa_data <- histogram_africa_data %>% 
  mutate(group_val = str_wrap(group_val,width=3),
         group_val = factor(group_val, levels = c('1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','16','17','18','19','20','21','22','23','24','25','26','27','28','29','30','31+','Off\nnetwork'))) %>%
  group_by(group_val) %>%
  mutate(k_sum = sum(landscan_population_un)) %>%
  ungroup() %>%
  mutate(share = landscan_population_un / k_sum ) %>%
  mutate(class_urban_paper = factor(class_urban_paper, levels = c( "1 - Urban","2 - Secondary urban","3 - Peri-urban","4 - Rural"))) %>%
  arrange(factor(class_urban_paper, levels = rev(c( "1 - Urban","2 - Secondary urban","3 - Peri-urban","4 - Rural"))), group_val) %>%
  group_by(group_val) %>%
  mutate(pos_id_val = (cumsum(landscan_population_un) - 0.5*landscan_population_un)) %>%
  ungroup()

(histogram_africa <- ggplot() +
    geom_bar(data = histogram_africa_data, aes(y = group_val, x = landscan_population_un, 
                                               fill = class_urban_paper), color = 'white', linewidth = .3, stat="identity") + 
    coord_flip() + 
    labs(y= 'Block complexity', x = 'Population', fill = '', color =  '', subtitle = '') + 
    geom_text(data = histogram_africa_data %>% filter(class_urban_paper %in% c('2 - Secondary urban', '4 - Rural')), 
              aes(y = group_val, x = pos_id_val, label = ifelse(landscan_population_un >= 5000000, paste0(round(share*100,0),"%"),'')), 
              size = 4, vjust = .5, color = '#333333', fontface='bold') +
    geom_text(data = histogram_africa_data %>% filter(class_urban_paper %in% c('1 - Urban','3 - Peri-urban') ), 
              aes(y = group_val, x = pos_id_val, label = ifelse(landscan_population_un >= 5000000, paste0(round(share*100,0),"%"),'')), 
              size = 4, vjust = .5, color = '#ffffff', fontface='bold') +
    scale_fill_manual(values = c('#08519c','#9ecae1','#006d2c','#a1d99b')) +
    scale_x_continuous(expand = c(.005,.005), labels = label_comma(accuracy = 1L, scale =  0.000001, suffix = "M") ) +
    theme(legend.position = 'bottom', panel.background = element_rect(fill = "white"), 
          axis.ticks = element_blank(), panel.grid.major = element_blank(),
          axis.text = element_text(color = '#333333', size = 12),
          axis.title = element_text(color = '#333333', size = 12, face = 'bold'),
          legend.text = element_text(color = '#333333', size = 14)
          ))

area4_pie_data <- histogram_africa_data %>% 
  group_by(class_urban_paper) %>%
  summarize_at(vars(landscan_population_un), list(sum)) %>%
  ungroup() %>%
  mutate(area_sum = sum(landscan_population_un)) %>%
  ungroup() %>%
  mutate(share = landscan_population_un / area_sum) %>%
  mutate(class_urban_paper = factor(class_urban_paper, levels = c( "1 - Urban","2 - Secondary urban","3 - Peri-urban","4 - Rural"))) %>%
  arrange(factor(class_urban_paper, levels = rev(c( "1 - Urban","2 - Secondary urban","3 - Peri-urban","4 - Rural")))) %>%
  mutate(pos_id_val = (cumsum(share) - 0.5*share)) 

(area4_pie <- ggplot(data = area4_pie_data, 
                     aes(x="", y=share, fill=class_urban_paper)) +
    geom_bar(stat="identity", width=1.25, color="white") +
    coord_polar("y", start=0) +
    geom_text(data = area4_pie_data %>% 
                filter(class_urban_paper %in% c('1 - Urban','3 - Peri-urban') ), 
              aes(x = 1.2, y = pos_id_val, label =  ifelse(share >= .1, paste0(paste0(round(landscan_population_un/1000000,0),'M') ,'\n', paste0(round(share*100,0),'%' )) ,'')), 
              color = "#ffffff", size=6, fontface = 'bold') +
    geom_text(data = area4_pie_data %>% 
                filter(class_urban_paper %in% c('2 - Secondary urban', '4 - Rural')), 
              aes(x = 1.2, y = pos_id_val, label =  ifelse(share >= .1, paste0(paste0(round(landscan_population_un/1000000,0),'M') ,'\n', paste0(round(share*100,0),'%' )) ,'')), 
              color = "#333333", size=6, fontface = 'bold') +
    theme_void() + 
    scale_fill_manual(values = c('#08519c','#9ecae1','#006d2c','#a1d99b')) +
    theme(legend.position="none"))

k_pie_data <- generate_crosstabs(data = df_combined_prep, 
                                            group_by_cols = c('continent'), 
                                            crosstab_cols = c("k_3"),
                                            sum_cols = sum_cols, divide_cols = divide_cols, 
                                            transform_cols = transform_cols, transform_func_list = list(log10 = log_10),
                                            group_by_agg_cols_list = list('continent' = "continent"),
                                            group_by_agg_func_list = list(sum = sum, share = share),
                                            agg_cols = c('landscan_population_un', 'worldpop_population_un'))

k_pie_data <- k_pie_data %>% 
  group_by(group_val) %>%
  summarize_at(vars(landscan_population_un), list(sum)) %>%
  ungroup() %>%
  mutate(k_sum = sum(landscan_population_un)) %>%
  ungroup() %>%
  mutate(share = landscan_population_un / k_sum) %>%
  mutate(group_val = factor(group_val, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10+", "Off network"))) %>%
  arrange(factor(group_val, levels = rev(c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10+", "Off network")))) %>%
  mutate(pos_id_val = (cumsum(share) - 0.5*share)) 

grey2 <- c('#414141','#777777')
kdist = length(unique(k_pie_data$group_val))
colorhexes <- colorRampPalette(c('#93328E','#CF3F80','#F7626B','#FF925A','#FFC556','#F9F871'))(length(unique(k_pie_data$group_val))-2)

(k_pie <- ggplot(data = k_pie_data, 
                 aes(x="", y=share, fill=group_val)) +
    geom_bar(stat="identity", width=1.25, color="white") +
    coord_polar("y", start=0) +
    geom_text(data = k_pie_data %>% filter(!(group_val %in% c('1','2','3','4'))), 
              aes(x = 1.4, y = pos_id_val, label = ifelse(share >= .04, paste0(paste0(round(landscan_population_un/1000000,0),'M') ,'\n', paste0(round(share*100,0),'%' )) ,'')), 
              color = "#333333", size= 4, fontface = 'bold') +
    geom_text(data = k_pie_data %>% filter((group_val %in% c('1','2','3','4'))), 
              aes(x = 1.4, y = pos_id_val, label = ifelse(share >= .04, paste0(paste0(round(landscan_population_un/1000000,0),'M') ,'\n', paste0(round(share*100,0),'%' )) ,'')), 
              color = "#ffffff", size= 4, fontface = 'bold') +
    scale_fill_manual(values = c(grey2,colorhexes), name = 'Block complexity') + 
    theme_void() + 
    theme(
      legend.title = element_text(size = 13, color = "#333333", face = 'bold'),
      legend.text = element_text(size = 13, color = "#333333")) # , face = 'bold'
    )

(histogram_africa_pies <- histogram_africa + 
  inset_element(area4_pie, left = -.2, bottom = .1, right = 1, top = 1, align_to = 'panel') +
  inset_element(k_pie, left = .5, bottom = .1, right = 1, top = 1, align_to = 'panel') + 
    plot_layout() + plot_annotation(tag_levels = list(c('A','B','C'))) & 
  theme(plot.tag = element_text(size = 13)))

ggsave(plot = histogram_africa_pies, filename = paste0(wd_output,'/viz/histogram_africa_pies.pdf'), width = 15, height = 7)

rm(histogram_africa_data, histogram_africa, histogram_africa_pies)
rm(area4_pie, area4_pie_data, k_pie, k_pie_data)


# Building footprint distribution -----------------------------------------
  
histogram_buildings <- generate_crosstabs(data = df_combined_prep, 
                                         group_by_cols = c("continent", "class_urban_paper"), 
                                         crosstab_cols = c("k_0"),
                                         sum_cols = sum_cols, divide_cols = divide_cols, 
                                         transform_cols = transform_cols, transform_func_list = list(log10 = log_10),
                                         group_by_agg_cols_list = list('continent' = c("continent"),
                                                                       'continent_4way' = c("continent", "class_urban_paper")),
                                         group_by_agg_func_list = list(sum = sum, share = share),
                                         agg_cols = c('landscan_population_un', 'worldpop_population_un'))

histogram_buildings <- histogram_buildings %>%
  select(all_of(c("group_var", "group_val", "continent", 
                  "class_urban_paper",
                  "average_building_area_m2", "building_area_m2", "building_count", "bldg_area_count_bin_01_0.50__log10_3.2", "bldg_area_count_bin_02_0.75__log10_5.6", "bldg_area_count_bin_03_1.00__log10_10", "bldg_area_count_bin_04_1.25__log10_17.8", "bldg_area_count_bin_05_1.50__log10_31.6", "bldg_area_count_bin_06_1.75__log10_56.2", "bldg_area_count_bin_07_2.00__log10_100", "bldg_area_count_bin_08_2.25__log10_177.8", "bldg_area_count_bin_09_2.50__log10_316.2", "bldg_area_count_bin_10_2.75__log10_562.3", "bldg_area_count_bin_11_3.00__log10_1000", "bldg_area_count_bin_12_3.25__log10_1778.3", "bldg_area_count_bin_13_3.50__log10_3162.3", "bldg_area_count_bin_14_3.75__log10_5623.4", "bldg_area_count_bin_15_4.00__log10_10000"))) %>%
  pivot_longer(cols = -one_of(c("group_var", "group_val", "continent", 
                                "class_urban_paper",
                                "average_building_area_m2", "building_area_m2", "building_count")),
               names_to = "category", 
               values_to = "values") %>%
  mutate(bin_label = case_when(category == 'bldg_area_count_bin_01_0.50__log10_3.2' ~ '< 5.6', 
                               category == 'bldg_area_count_bin_02_0.75__log10_5.6' ~ '5.6 to\n10', 
                               category == 'bldg_area_count_bin_03_1.00__log10_10' ~ '10 to\n17', 
                               category == 'bldg_area_count_bin_04_1.25__log10_17.8' ~ '17 to\n31\n(parking\nspace)', 
                               category == 'bldg_area_count_bin_05_1.50__log10_31.6' ~ '31 to\n56', 
                               category == 'bldg_area_count_bin_06_1.75__log10_56.2' ~ '56 to\n100', 
                               category == 'bldg_area_count_bin_07_2.00__log10_100' ~ '100 to\n177', 
                               category == 'bldg_area_count_bin_08_2.25__log10_177.8' ~ '177 to\n316', 
                               category == 'bldg_area_count_bin_09_2.50__log10_316.2' ~ '316 to\n562\n(basketball\ncourt)', 
                               category == 'bldg_area_count_bin_10_2.75__log10_562.3' ~ '562 to\n1K', 
                               category %in% c('bldg_area_count_bin_11_3.00__log10_1000','bldg_area_count_bin_12_3.25__log10_1778.3',
                                               'bldg_area_count_bin_13_3.50__log10_3162.3','bldg_area_count_bin_14_3.75__log10_5623.4',
                                               'bldg_area_count_bin_15_4.00__log10_10000') ~ '> 1K')
                               ) %>%
  mutate(bin_label = factor(bin_label, levels = c('< 5.6', '5.6 to\n10', '10 to\n17', '17 to\n31\n(parking\nspace)', '31 to\n56', '56 to\n100', '100 to\n177', '177 to\n316', '316 to\n562\n(basketball\ncourt)', '562 to\n1K', '> 1K'))) %>%
  group_by(bin_label, class_urban_paper) %>%
  summarize_at(vars(values), list(sum)) %>%
  ungroup() %>%
  group_by(class_urban_paper) %>%
  mutate(values_by_class = sum(values)) %>%
  ungroup() %>%
  mutate(share = values/values_by_class)
  
histogram_buildings <- histogram_buildings %>%
  mutate(bin_label2 = case_when(bin_label == '< 5.6' ~ '2.8',
                                bin_label == '5.6 to\n10' ~ '7.8',
                                bin_label == '10 to\n17' ~ '13.5\n(parking\nspace)',
                                bin_label == '17 to\n31\n(parking\nspace)' ~ '24',
                                bin_label == '31 to\n56' ~ '43.5',
                                bin_label == '56 to\n100' ~ '78',
                                bin_label == '100 to\n177' ~ '138.5',
                                bin_label == '177 to\n316' ~ '246.5',
                                bin_label == '316 to\n562\n(basketball\ncourt)' ~ '439\n(basketball\ncourt)',
                                bin_label == '562 to\n1K' ~ '781',
                                bin_label == '> 1K' ~ '> 1K')) %>%
  mutate(bin_label2 = factor(bin_label2, levels = c('2.8', '7.8', '13.5\n(parking\nspace)', '24', '43.5', '78', '138.5', '246.5', '439\n(basketball\ncourt)', '781', '> 1K')))


(bldgs_bar <- ggplot(data = histogram_buildings, aes(x = bin_label2, y = values, 
                                                     fill = class_urban_paper)) +
    geom_bar(stat='identity') + 
    scale_x_discrete(expand = c(0.05,0.05)) +
    scale_y_continuous(expand = c(0, 0), labels = label_comma(accuracy = 1L, scale =  0.000001, suffix = "M") ) +
    scale_fill_manual(values = c('#08519c','#9ecae1','#006d2c','#a1d99b')) +
    labs(x = expression("Building footprint area, m"^2) ,
         y = 'Building count') +
    theme_classic() + 
    theme(legend.position = 'bottom',
          legend.title = element_blank()))

(bldgs_density <- ggplot(data = histogram_buildings, 
                         aes(x = bin_label2, y = share, 
                             group = class_urban_paper)) +
    geom_smooth(linewidth = 1.5, 
                aes(fill = class_urban_paper, color = class_urban_paper), se = FALSE,
                method = 'loess', span = 0.4, alpha = .9)+
    scale_x_discrete(expand = c(0.05,0.05)) +
    scale_fill_manual(values = c('#08519c','#9ecae1','#006d2c','#a1d99b')) +
    scale_color_manual(values = c('#08519c','#9ecae1','#006d2c','#a1d99b')) +
    labs(x = expression("Building footprint area, m"^2) ,
         y = 'Density') +
    theme_classic() + 
    theme(legend.position = 'none',
          legend.title = element_blank()) + guides(fill = "none", color = 'none') )

(bldgs_charts <- bldgs_bar + bldgs_density +
  plot_layout(guides = 'collect') &
    plot_annotation(tag_levels = list(c("A","B"))) & 
  theme(text = element_text(size = 14, color = '#333333'),
        plot.tag = element_text(size = 13),
        axis.text = element_text(size = 12,  color = '#333333'),
        legend.text = element_text(size = 12,  color = '#333333'),
        legend.position = 'bottom'))

ggsave(plot = bldgs_charts, filename = paste0(wd_output,'/viz/bldg_area_charts_2.pdf'), width = 15.672, height = 8) # dpi = 300, 
rm(bldgs_charts, bldgs_density, bldgs_bar)


# Urban center bars -------------------------------------------------------

urban_bars_data <- generate_crosstabs(data = df_combined_prep %>% 
                                        filter(class_urban_paper %in% c('1 - Urban')),
                              group_by_cols = c(urban_cols), 
                              crosstab_cols = c("k_4"),
                              sum_cols = sum_cols, divide_cols = divide_cols, 
                              transform_cols = transform_cols, transform_func_list = list(log10 = log_10),
                              group_by_agg_cols_list = list('urban' = "urban_id",
                                                            'country' = 'urban_country_code'),
                              group_by_agg_func_list = list(sum = sum, share = share),
                              agg_cols = c('landscan_population_un', 'worldpop_population_un'))

urban_bars_data  <- urban_bars_data  %>%
  mutate(group_val = factor(group_val, levels = rev(c('1','2','3','4','5','6','7','8','9','10', "11 to 15","16 to 20","21+","Off network")))) %>%
  filter(agg_sum_landscan_population_un_group_by_urban >= 2000000) %>%
  mutate(urban_country_name =  (gsub('Democratic Republic of the Congo ', 'DR Congo', as.character(urban_country_name)))) %>%
  mutate(urban_country_name =  (gsub('–', '-', as.character(urban_country_name)))) %>%
  mutate(urban_country_code =  (gsub('–', '-', as.character(urban_country_code)))) %>%
  mutate(urban_name = paste0(urban_center_name,', ',urban_country_code)) %>%
  mutate(k_reweight = landscan_population_un*k_complexity_weighted_landscan_un) %>%
  group_by(urban_id) %>%
  mutate(k_reweight = sum(k_reweight, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(k_reweight = k_reweight/agg_sum_landscan_population_un_group_by_urban) %>% 
  arrange(urban_id, factor(group_val, levels = (c('1','2','3','4','5','6','7','8','9','10', "11 to 15","16 to 20","21+","Off network")))) %>%
  group_by(urban_id) %>%
  mutate(pos_id_val = (cumsum(landscan_population_un) - 0.5*landscan_population_un),
         pos_id_share = (cumsum(agg_share_landscan_population_un_group_by_urban) - 0.5*agg_share_landscan_population_un_group_by_urban)) %>% 
  ungroup() 

grey2 <- c('#414141','#777777')
kdist = length(urban_bars_data$group_val)
colorhexes <- colorRampPalette(c('#93328E','#CF3F80','#F7626B','#FF925A','#FFC556','#F9F871'))(length(unique(urban_bars_data$group_val))-2)

(a <- ggplot() +
    geom_bar(data = urban_bars_data, aes(y = landscan_population_un, x = reorder(urban_name, agg_sum_landscan_population_un_group_by_urban), fill = group_val), stat="identity") +
    coord_flip(clip = "off") +
    scale_fill_manual(values = rev(c(grey2, colorhexes))) +
    scale_y_continuous(expand = c(0, 0), labels = label_comma(accuracy = 1L, scale =  0.000001, suffix = "M") ) +
    geom_text(data = urban_bars_data, aes(label=ifelse(landscan_population_un >= 1000000, paste0(round(landscan_population_un/1000000,0),"M" ),""), y = pos_id_val, x = urban_name), 
              color = 'white', size = 4) + # fontface = "bold", 
    theme_bw() + 
    labs(y = 'Population total', x = '', subtitle = '') + 
    theme(plot.margin=unit(c(t=0,r=20,b=0,l=5), "pt"),
          axis.title = element_text(size = 14, color = '#333333', face = 'bold' ),
          axis.text.x = element_text(size = 12, color = '#333333'),
          axis.text.y = element_text(color = '#333333', size = 13), 
          legend.text = element_text(color = '#333333', size = 14), # , face ='bold'
          legend.title = element_blank(),
          legend.key.width=unit(10,"pt"),
          legend.key.height=unit(2,"pt"),
          legend.position = 'bottom', 
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          plot.subtitle = element_text(size = 15, hjust=.5, color = '#333333')))

(b <- ggplot() +
    geom_bar(data = urban_bars_data, aes(y = agg_share_landscan_population_un_group_by_urban, x = reorder(urban_name, k_reweight), fill = group_val), stat="identity") +
    coord_flip(clip = "off") +
    scale_fill_manual(values = rev(c(grey2, colorhexes))) +
    scale_y_continuous(expand = c(0, 0), labels = scales::percent ) +
    geom_text(data = urban_bars_data, aes(label=ifelse(agg_share_landscan_population_un_group_by_urban >= 0.075, paste0(round(agg_share_landscan_population_un_group_by_urban*100,0),"%" ),""), y = pos_id_share, x = urban_name), 
              color = 'white',  size = 4) + #fontface = "bold",
    theme_bw() + 
    labs(y = 'Population proportion', x = '', subtitle = '') + 
    theme(plot.margin=unit(c(t=0,r=20,b=0,l=5), "pt"),
          legend.key.width=unit(10,"pt"),
          legend.key.height=unit(2,"pt"),
          legend.position = 'bottom',
          axis.title = element_text(size = 14, color = '#333333', face = 'bold'),
          axis.text.x = element_text(size = 12, color = '#333333'),
          axis.text.y = element_text(color = '#333333', size = 13), 
          legend.text = element_text(color = '#333333', size = 14), # , face ='bold'
          axis.title.y = element_blank(),
          legend.title = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          plot.subtitle = element_text(size = 15, hjust=.5, color = '#434343')))

(urban_bars <- a + b + plot_layout(guides = 'collect') + 
    plot_annotation(tag_levels = list(c("A","B"))) & 
    theme(plot.tag = element_text(size = 14)) & # &  plot_annotation(title = 'Urban areas') 
    theme(legend.position = 'bottom', plot.title = element_text(color = '#434343', size = 15, face="bold", hjust=.5, vjust = -2)) &
    guides(fill = guide_legend(nrow = 1, reverse = TRUE), color = guide_legend(nrow = 1, reverse = TRUE)) )

ggsave(plot = urban_bars, filename = paste0(wd_output,'/viz/urban_bars.pdf'), width = 15.672, height = 8) # dpi = 300, 
rm(a, b, urban_bars, urban_bars_data)

# Conurbation bars --------------------------------------------------------

conurban_bars_data <- generate_crosstabs(data = df_combined_prep %>% 
                                           filter(class_urban_paper %in% c('1 - Urban', '2 - Secondary urban', '3 - Peri-urban')), 
                              group_by_cols = c(conurban_cols), 
                              crosstab_cols = c("k_4"),
                              sum_cols = sum_cols, divide_cols = divide_cols, 
                              transform_cols = transform_cols, transform_func_list = list(log10 = log_10),
                              group_by_agg_cols_list = list('conurban' = "conurbation_id",
                                                            'country' = 'conurbation_country_code'),
                              group_by_agg_func_list = list(sum = sum, share = share),
                              agg_cols = c('landscan_population_un', 'worldpop_population_un'))

conurban_bars_data <- conurban_bars_data %>%
  mutate(group_val = factor(group_val, levels = rev(c('1','2','3','4','5','6','7','8','9','10', "11 to 15","16 to 20","21+","Off network")))) %>%
  filter(agg_sum_landscan_population_un_group_by_conurban >= 2000000) %>%
  mutate(country_name =  (gsub('Democratic Republic of the Congo ', 'DR Congo', as.character(conurbation_country_name))),
         conurbation_area_name_short = gsub('Ambatolampy Tsimahafotsy', 'Ambatolampy', as.character(conurbation_area_name_short))) %>%
  mutate(conurbation_country_name =  (gsub('–', '-', as.character(conurbation_country_name)))) %>%
  mutate(conurbation_country_code =  (gsub('–', '-', as.character(conurbation_country_code)))) %>%
  mutate(conurbation_name = paste0(conurbation_area_name_short,', ',conurbation_country_code)) %>%
  mutate(k_reweight = landscan_population_un*k_complexity_weighted_landscan_un) %>%
  group_by(conurbation_id) %>%
  mutate(k_reweight = sum(k_reweight)) %>%
  ungroup() %>%
  mutate(k_reweight = k_reweight/agg_sum_landscan_population_un_group_by_conurban)  %>% 
  arrange(conurbation_id, factor(group_val, levels = (c('1','2','3','4','5','6','7','8','9','10', "11 to 15","16 to 20","21+","Off network")))) %>%
  group_by(conurbation_id) %>%
  mutate(pos_id_val = (cumsum(landscan_population_un) - 0.5*landscan_population_un),
         pos_id_share = (cumsum(agg_share_landscan_population_un_group_by_conurban) - 0.5*agg_share_landscan_population_un_group_by_conurban)) %>% 
  ungroup() 

grey2 <- c('#414141','#777777')
kdist = length(conurban_bars_data$group_val)
colorhexes <- colorRampPalette(c('#93328E','#CF3F80','#F7626B','#FF925A','#FFC556','#F9F871'))(length(unique(conurban_bars_data$group_val))-2)

(a <- ggplot() +
    geom_bar(data = conurban_bars_data, aes(y = landscan_population_un, x = reorder(conurbation_name, agg_sum_landscan_population_un_group_by_conurban), fill = group_val), stat="identity") +
    coord_flip() +
    scale_fill_manual(values = rev(c(grey2, colorhexes))) +
    scale_y_continuous(expand = c(0, 0), labels = label_comma(accuracy = 1L, scale =  0.000001, suffix = "M") ) +
    geom_text(data = conurban_bars_data, aes(label=ifelse(landscan_population_un >= 1000000, paste0(round(landscan_population_un/1000000,0),"M" ),""), y = pos_id_val, x = conurbation_name), color = 'white', fontface = "bold", size = 3) +
    theme_bw() + 
    labs(y = 'Population', x = '', subtitle = '') + 
    theme(plot.margin=unit(c(t=0,r=20,b=0,l=5), "pt"),
          legend.key.width=unit(10,"pt"),
          legend.key.height=unit(2,"pt"),
          legend.position = 'bottom',
          axis.title = element_text(size = 14, color = '#333333', face = 'bold'),
          axis.text.x = element_text(size = 12, color = '#333333'),
          axis.text.y = element_text(color = '#333333', size = 13), 
          legend.text = element_text(color = '#333333', size = 14), # , face ='bold'
          axis.title.y = element_blank(),
          legend.title = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          plot.subtitle = element_text(size = 15, hjust=.5, color = '#434343')))

(b <- ggplot() +
    geom_bar(data = conurban_bars_data, aes(y = agg_share_landscan_population_un_group_by_conurban, x = reorder(conurbation_name, k_reweight), fill = group_val), stat="identity") +
    coord_flip() +
    scale_fill_manual(values = rev(c(grey2, colorhexes))) +
    scale_y_continuous(expand = c(0, 0), labels = scales::percent ) +
    geom_text(data = conurban_bars_data, aes(label=ifelse(agg_share_landscan_population_un_group_by_conurban >= 0.075, paste0(round(agg_share_landscan_population_un_group_by_conurban*100,0),"%" ),""), y = pos_id_share, x = conurbation_name), color = 'white', fontface = "bold", size = 3) +
    theme_bw() + 
    labs(y = 'Population', x = '', subtitle = '') + 
    theme(plot.margin=unit(c(t=0,r=20,b=0,l=5), "pt"),
          legend.key.width=unit(10,"pt"),
          legend.key.height=unit(2,"pt"),
          legend.position = 'bottom',
          axis.title = element_text(size = 14, color = '#333333', face = 'bold'),
          axis.text.x = element_text(size = 12, color = '#333333'),
          axis.text.y = element_text(color = '#333333', size = 13), 
          legend.text = element_text(color = '#333333', size = 14), # , face ='bold'
          axis.title.y = element_blank(),
          legend.title = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          plot.subtitle = element_text(size = 15, hjust=.5, color = '#434343')))

(conurban_bars <- a + b + plot_layout(guides = 'collect') &  
    plot_annotation(tag_levels = list(c("A","B"))) &
    theme(legend.position = 'bottom',
          plot.tag = element_text(size = 14),
          plot.title = element_text(color = '#434343', size = 15, face="bold", hjust=.5, vjust = -2)) &
    guides(fill = guide_legend(nrow = 1, reverse = TRUE), color = guide_legend(nrow = 1, reverse = TRUE) ))

ggsave(plot = conurban_bars, filename = paste0(wd_output,'/viz/conurban_bars.pdf'), width = 16, height = 8) # dpi = 300,
rm(a, b, conurban_bars, conurban_bars_data)


# Inequality charts -------------------------------------------------------

urban_ineq <- read_parquet(paste0('data/africa_data.parquet')) %>%
  filter(area_type == 'Urban') 

names(urban_ineq)
conurban_sd <- urban_ineq %>%
  mutate(k_tile = ntile(-k_complexity, n = 100)) %>%
  group_by(conurbation_id, conurbation_area_name_short, conurbation_country_code, conurbation_country_name) %>%
  mutate(landscan_population_un = ifelse(landscan_population_un == 0, 1, landscan_population_un)) %>%
  summarise(weighted_mean = stats::weighted.mean(x = k_complexity, w = landscan_population_un),
            weighted_sd = sqrt(sum( landscan_population_un * (k_complexity - weighted_mean)^2) / sum(landscan_population_un)),
            area_total = sum(landscan_population_un),
            gini = DescTools::Gini(x= k_tile, weights = landscan_population_un)) %>%
  ungroup() %>%
  filter(area_total >= 1000000) %>%
  mutate(k_quartile = ntile(-weighted_mean, n = 4))

urban_sd <- urban_ineq %>%
  mutate(k_tile = ntile(-k_complexity, n = 100)) %>%
  group_by(urban_id, urban_center_name, urban_country_code, urban_country_name) %>%
  mutate(landscan_population_un = ifelse(landscan_population_un == 0, 1, landscan_population_un)) %>%
  summarise(weighted_mean = stats::weighted.mean(x = k_complexity, w = landscan_population_un),
            weighted_sd = sqrt(sum( landscan_population_un * (k_complexity - weighted_mean)^2) / sum(landscan_population_un)),
            area_total = sum(landscan_population_un),
            gini = DescTools::Gini(x= k_tile, weights = landscan_population_un)) %>%
  ungroup() %>%
  filter(area_total >= 1000000) %>%
  mutate(k_quartile = ntile(-weighted_mean, n = 4))

agglos_sd <- urban_ineq %>%
  mutate(k_tile = ntile(-k_complexity, n = 100)) %>%
  group_by(agglosid, agglosname, metropole) %>%
  mutate(landscan_population_un = ifelse(landscan_population_un == 0, 1, landscan_population_un)) %>%
  summarise(weighted_mean = stats::weighted.mean(x = k_complexity, w = landscan_population_un),
            weighted_sd = sqrt(sum( landscan_population_un * (k_complexity - weighted_mean)^2) / sum(landscan_population_un)),
            area_total = sum(landscan_population_un),
            gini = DescTools::Gini(x= k_tile, weights = landscan_population_un)) %>%
  ungroup() %>%
  filter(area_total >= 1000000) %>%
  mutate(k_quartile = ntile(-weighted_mean, n = 4))

rm(urban_ineq)
gc()

scatter_gini <- ggplot() + 
  geom_point(data = urban_sd, 
             aes(x = weighted_mean, y = gini, size = area_total), alpha = .4) +
  geom_text_repel(data = urban_sd, seed = 1, segment.curvature = -0.1, point.padding = 0, box.padding = 0.4, max.iter = 30000, segment.square  = FALSE, segment.inflect = FALSE, min.segment.length = .2, max.overlaps = Inf, force = .5, force_pull = 10, 
                  aes(x = weighted_mean, y = gini, label = paste0(urban_center_name,", ", urban_country_code) ), size = 3, vjust =.5, color = '#333333', fontface='bold') +
  scale_size_continuous(range = c(1,10), labels = label_comma(accuracy = 1L, scale =  0.000001, suffix = "M") ) +
  theme_classic() +
  labs(y = 'Gini', x = 'Population-weighted Block Complexity', size = 'Population')

scatter_sd <- ggplot() + 
  geom_point(data = urban_sd, 
             aes(x = weighted_mean, y = weighted_sd, size = area_total), alpha = .4) +
  geom_text_repel(data = urban_sd, seed = 1, segment.curvature = -0.1, point.padding = 0, box.padding = 0.4, max.iter = 30000, segment.square  = FALSE, segment.inflect = FALSE, min.segment.length = .2, max.overlaps = Inf, force = .5, force_pull = 10, 
                  aes(x = weighted_mean, y = weighted_sd, label = paste0(urban_center_name,", ", urban_country_code) ), size = 3, vjust =.5, color = '#333333', fontface='bold') +
  scale_size_continuous(range = c(1,10), labels = label_comma(accuracy = 1L, scale =  0.000001, suffix = "M") ) +
  theme_classic() +
  labs(y = 'Population-weighted Standard Deviation', x = 'Population-weighted Block Complexity', size = 'Population')

scatter_gini <- ggplot() + 
  geom_point(data = conurban_sd , 
             aes(x = weighted_mean, y = gini, size = area_total), alpha = .4) +
  geom_text_repel(data = conurban_sd , seed = 1, segment.curvature = -0.1, point.padding = 0, box.padding = 0.4, max.iter = 30000, segment.square  = FALSE, segment.inflect = FALSE, min.segment.length = .2, max.overlaps = Inf, force = .5, force_pull = 10, 
                  aes(x = weighted_mean, y = gini, label = paste0(conurbation_area_name_short,", ", conurbation_country_code) ), size = 3, vjust =.5, color = '#333333', fontface='bold') +
  scale_size_continuous(range = c(1,10), labels = label_comma(accuracy = 1L, scale =  0.000001, suffix = "M") ) +
  theme_classic() +
  labs(y = 'Gini', x = 'Population-weighted Block Complexity', size = 'Population')

scatter_sd <- ggplot() + 
  geom_point(data = conurban_sd , 
             aes(x = weighted_mean, y = weighted_sd, size = area_total), alpha = .4) +
  geom_text_repel(data = conurban_sd , seed = 1, segment.curvature = -0.1, point.padding = 0, box.padding = 0.4, max.iter = 30000, segment.square  = FALSE, segment.inflect = FALSE, min.segment.length = .2, max.overlaps = Inf, force = .5, force_pull = 10, 
                  aes(x = weighted_mean, y = weighted_sd, label = paste0(conurbation_area_name_short,", ", conurbation_country_code) ), size = 3, vjust =.5, color = '#333333', fontface='bold') +
  scale_size_continuous(range = c(1,10), labels = label_comma(accuracy = 1L, scale =  0.000001, suffix = "M") ) +
  theme_classic() +
  labs(y = 'Population-weighted Standard Deviation', x = 'Population-weighted Block Complexity', size = 'Population')

scatter_gini + scatter_sd 

rm(conurban_sd, urban_sd, agglos_sd, scatter_gini, scatter_sd)

# -------------------------------------------------------------------------
# Download from this URL: dsbprylw7ncuq.cloudfront.net/AF/africa_geodata.parquet
if (!file.exists(paste0("data/africa_geodata.parquet"))) {
  curl::multi_download("dsbprylw7ncuq.cloudfront.net/AF/africa_geodata.parquet", "data/africa_geodata.parquet", resume = TRUE)
} else {
  print('africa_geodata.parquet already downloaded.')
}

area_data <- arrow::open_dataset(paste0('data/africa_geodata.parquet')) %>% 
  filter(urban_id %in% c('ghsl_3798','periurban_925')) %>% 
  read_sf_dataset() %>%
  st_set_crs(4326) %>%
  st_make_valid()

# Zoom map ----------------------------------------------------------------

sites <- data.frame(place=c('nga','sle','zmb'),
                    longitude=c(3.34926, -13.22970641, 28.2474993),
                    latitude=c(6.45811, 8.4759301, -15.3828024)) %>%
  st_as_sf(., coords = c("longitude", "latitude"), 
           crs = 4326, agr = "constant") %>%
  st_transform(3857) %>%
  st_buffer(., 700) %>% #st_bbox
  st_transform(4326) 

place_var = 'zmb'

area_box <- area_data %>% 
  st_transform(3857) %>% st_bbox(.) %>% st_as_sfc() %>% st_transform(4326) %>% st_centroid() %>%
  st_union(x = ., y = st_bbox(sites %>% filter(place == place_var)) %>% st_as_sfc()) %>%
  st_transform(3857) %>% st_buffer(., 3000) %>% st_bbox(.) %>% st_as_sfc() %>% st_transform(4326)

area_data_framed <- area_data %>%
  st_intersection(., area_box)

(zoom_inset <- ggplot() +
    geom_sf(data = area_data, aes(fill = landscan_population_un_density_hectare_log,
                                  color = landscan_population_un_density_hectare_log), color = alpha('white',0), 
            linewidth= 0, alpha = 1) +
    geom_sf(data = area_box, color = '#ffffff', fill = '#ffffff', alpha = 0, linewidth = 2) + 
    geom_sf(data = area_box, color = '#434343', fill = '#434343', alpha = 0, linewidth = 1) + 
    geom_sf(data = sites %>% filter(place == place_var), color = '#ffffff', fill = '#ffffff', alpha = 0, linewidth = 1.8) + 
    geom_sf(data = sites %>% filter(place == place_var), color = '#434343', fill = '#434343', alpha = 0, linewidth = .8) + 
    scale_fill_distiller(direction = -1, palette = 'Spectral', oob = scales::squish, limits= c(1, 2.5), 
                         breaks= c(1,2,3,4,5,6,7), labels = c('0',"100","1K","10K","100K","1M","10M")) +
    scale_color_distiller(direction = -1, palette = 'Spectral', oob = scales::squish, limits= c(1, 2.5), 
                          breaks= c(1,2,3,4,5,6,7), labels = c('0',"100","1K","10K","100K","1M","10M")) +
    theme_void() + theme(legend.position = 'none'))

(africa_zoom <- ggplot() +
    geom_sf(data = subsaharan_africa, fill = '#36454f', color = '#ffffff', linewidth = .5) + 
    geom_sf(data = data.frame(place=c('zmb'), longitude=c(28.2474993), latitude=c(-15.3828024)) %>%
              st_as_sf(., coords = c("longitude", "latitude"), crs = 4326, agr = "constant") %>%
              st_transform(3857) %>%
              st_buffer(., 200000) %>% 
              st_transform(4326), fill = '#ffffff', color = '#ffffff') +
    geom_sf(data = cbind(longitude=c(28.2474993, -4.682159), latitude=c(-15.3828024, -15.016642)) %>%
              st_linestring() %>%
              st_sfc() %>% 
              st_set_crs(4326), fill = '#ffffff', color = '#ffffff', linewidth = 3) +
    geom_sf(data = cbind(longitude=c(28.2474993, -4.682159), latitude=c(-15.3828024, -15.016642)) %>%
              st_linestring() %>%
              st_sfc() %>% 
              st_set_crs(4326), fill = '#3288bc', color = '#3288bc', linewidth = 1.5) +
    geom_sf(data = data.frame(place=c('zmb'), longitude=c(28.2474993), latitude=c(-15.3828024)) %>%
              st_as_sf(., coords = c("longitude", "latitude"), crs = 4326, agr = "constant") %>%
              st_transform(3857) %>%
              st_buffer(., 100000) %>%
              st_transform(4326), fill = '#3288bc', color = '#3288bc') +
    theme_void() + inset_element(zoom_inset, left = -.5, bottom = .12, right = 1, top = .55, align_to = 'full')
  )

df_combined_prep %>% group_by(country_code, country_name) %>%
  tally() %>% print(n = 100)

# buildings <- arrow::open_dataset('buildings_polygons_ZMB.parquet')) %>%
#   filter(gadm_code %in% unique(area_data_framed$gadm_code))%>% 
#   read_sf_dataset() %>%
#   st_set_crs(4326) %>%
#   st_make_valid() %>%
#   st_intersection(., sites %>% filter(place == place_var))

buildings <- st_read('data/buildings_lusaka.geojson')

# streets <- arrow::open_dataset('streets_ZMB.parquet')) %>%
#   read_sf_dataset() %>%
#   st_set_crs(4326) %>%
#   st_make_valid() %>%
#   st_intersection(., sites %>% filter(place == place_var))

streets <- st_read('data/streets_lusaka.geojson')

layers <- st_read('data/layer_lusaka.geojson')

width = st_distance(st_sf(geom = st_sfc(st_point(c(st_bbox(area_data_framed)$xmin, st_bbox(area_data_framed)$ymin)),
                                        st_point(c(st_bbox(area_data_framed)$xmax, st_bbox(area_data_framed)$ymin)), crs = 4326)))[2] %>%  drop_units()
height = st_distance(st_sf(geom = st_sfc(st_point(c(st_bbox(area_data_framed)$xmin, st_bbox(area_data_framed)$ymin)),
                                         st_point(c(st_bbox(area_data_framed)$xmin, st_bbox(area_data_framed)$ymax)), crs = 4326)))[2] %>%  drop_units()

width_tenth = round((width*.2)/1000,-1)
if (width_tenth < 1) {
  width_tenth = round((width*.2)/1000,0)
}
height_decdegs = abs(unname(st_bbox(area_data_framed)$ymax) - unname(st_bbox(area_data_framed)$ymin))

area_data %>% st_drop_geometry() %>% 
  dplyr::summarize(pop_dense = weighted.mean(landscan_population_un_density_hectare, landscan_population_un) ) %>% 
  pull() %>% round(.,0)

area_data_framed <- area_data_framed %>% 
  mutate(k_labels = gsub('-','\n',x = k_labels)) %>%
  mutate(k_labels = factor(k_labels, levels = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10+', 'Off\nnetwork')))  

grey2 <- c('#777777','#414141') 
kdist = max(as.integer(area_data_framed$k_labels))
colorhexes <- colorRampPalette(c('#93328E','#CF3F80','#F7626B','#FF925A','#FFC556','#F9F871'))(length(unique(area_data_framed$k_labels))-2)

(plot_k_discrete <- ggplot() +
    geom_sf(data = area_data_framed, aes(fill = as.factor(k_labels)), 
            color = '#ffffff', linewidth = .03) +
    #geom_sf(data = area_data_box, aes(fill = as.factor(k_labels)), color = alpha('#ffffff',0), linewidth = .01) +   
    geom_sf(data = sites %>% filter(place == place_var), color = 'white', 
            fill = 'white', alpha = 0, linewidth = 1.7) +
    geom_sf(data = sites %>% filter(place == place_var), color = '#434343', 
            fill = '#434343', alpha = 0, linewidth = .5) +
    scale_fill_manual(expand = c(0,0), values = c(grey2, colorhexes), name = 'Block complexity') + 
    theme_void() + theme(text = element_text(color = "#333333"),
                         legend.position = 'none',
                         legend.spacing.x = unit(1, 'pt'),
                         legend.text = element_text(size = 14),
                         axis.ticks =element_blank(),
                         panel.grid = element_blank(),
                         axis.title = element_blank(),
                         panel.border = element_blank(),
                         #panel.background = element_blank(),
                         plot.margin=unit(c(t=0,r=0,b=0,l=0), "pt"),
                         legend.title = element_blank(),
                         plot.caption = element_text(size = 11, hjust = .5, vjust = 5), #margin=margin(0,0,0,0)),
                         axis.text = element_blank()) +
    ggsn::scalebar(y.min = st_bbox(area_data_framed)$ymin - (height_decdegs*.03), 
                   x.min = st_bbox(area_data_framed)$xmin, 
                   y.max = st_bbox(area_data_framed)$ymax, 
                   x.max = st_bbox(area_data_framed)$xmax, 
                   location = 'bottomleft',
                   height = .01, box.fill = c('#333333','#ffffff'),
                   border.size = .4, st.color = '#333333', st.size = 2.5, box.color = '#333333',
                   dist = width_tenth/2, dist_unit = "km", transform = TRUE, model = "WGS84"))

(map_block <- ggplot() +
    geom_sf(data = area_data_framed %>% st_make_valid() %>% 
              st_intersection(., sites %>% filter(place == place_var)), 
            aes(fill = as.factor(k_labels)), color = '#ffffff', linewidth = .2) +
    geom_sf(data = buildings, color = 'grey', fill = 'black') +
    geom_sf(data = streets, color = 'white', fill = 'white', linewidth = 1) +
    geom_sf(data = sites %>% filter(place == place_var), color = 'white', fill = 'white', alpha = 0, linewidth = 1.5) +
    scale_fill_manual(expand = c(0,0), breaks =  c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10+', 'Off\nnetwork'), 
                      values = c(grey2, colorhexes), name = 'Block complexity') + 
    theme_void() +
    theme(legend.position = 'none'))

#grey2 <- c('#E8E7F4','#CEC8DA')
grey2 <- c('#E8E7F4','#777777') 
kdist = max(as.integer(area_data_framed$k_labels))
colorhexes <- colorRampPalette(c('#93328E','#CF3F80','#F7626B','#FF925A','#FFC556','#F9F871'))(length(unique(area_data_framed$k_labels))-2)

(l1 <- ggplot() +
    geom_sf(data = layers %>% filter(block_property == 'building-parcels',
                                     block_id %in% c("ZMB.5.6_2_9371")), 
            aes(fill = as.factor(k_complexity)), color = 'white', 
            alpha = 1, linewidth = .2) + 
    geom_sf(data = layers %>% filter(block_property == 'building-polygons',
                                     block_id %in% c("ZMB.5.6_2_9371")), fill = 'black', color = 'grey', 
            linewidth = .1, alpha = .8) +
    geom_sf(data = layers %>% filter(block_property == 'on-network-streets',
                                     block_id %in% c("ZMB.5.6_2_9371")), 
            fill = 'black', color = 'black', alpha = .8, linewidth = 2.5, lineend = 'round') +
    geom_sf(data = layers %>% filter(block_property == 'on-network-streets',
                                     block_id %in% c("ZMB.5.6_2_9371")), 
            fill = 'yellow', color = 'white', alpha = .8, linewidth = .7, lineend = 'round') + # linetype = "dashed", 
    scale_fill_manual(name = 'Block complexity', values = c(grey2, colorhexes), breaks = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10+', 'Off\nnetwork')) +
    labs(subtitle = '') +
    theme_void() + theme(legend.title = element_text(size = 14, face = 'bold'),
                         legend.text = element_text(size = 14),
                         plot.subtitle = element_text(size = 10),
                         axis.text = element_blank()))

(l2 <- ggplot() +
    geom_sf(data = layers %>% filter(block_property == 'building-parcels',
                                     block_id %in% c("ZMB.5.6_2_9339", "ZMB.5.6_2_9334", "ZMB.5.6_2_9335")), 
            aes(fill = as.factor(k_complexity)), color = 'white', 
            alpha = 1, linewidth  = .2) + 
    geom_sf(data = layers %>% filter(block_property == 'building-polygons',
                                     block_id %in% c("ZMB.5.6_2_9339", "ZMB.5.6_2_9334", "ZMB.5.6_2_9335")), fill = 'black', color = 'grey',  
            linewidth = .1, alpha = .8) +
    geom_sf(data = layers %>% filter(block_property == 'on-network-streets',
                                     block_id %in% c("ZMB.5.6_2_9339", "ZMB.5.6_2_9334", "ZMB.5.6_2_9335")), 
            fill = 'black', color = 'black', alpha = .8, linewidth = 2.5, lineend = 'round') +
    geom_sf(data = layers %>% filter(block_property == 'on-network-streets',
                                     block_id %in% c("ZMB.5.6_2_9339", "ZMB.5.6_2_9334", "ZMB.5.6_2_9335")), 
            fill = 'yellow', color = 'white', alpha = .8, linewidth = .7, lineend = 'round') + # linetype = "dashed", 
    scale_fill_manual(name = 'Block complexity', values = c(grey2, colorhexes), breaks = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10+', 'Off\nnetwork')) +
    labs(subtitle = '') +
    theme_void() + theme(legend.position = 'none',
                         plot.subtitle = element_text(size = 10),
                         axis.text = element_blank()))

design <- "
  AAAABBBBBB
  AAAABBBBBB
  CCCCDDDDEE
  CCCCDDDDEE
"

(layers_viz <- africa_zoom +  plot_k_discrete + map_block + l1 + l2 + 
    #plot_layout(design = design, guides = "collect", tag_level = 'new') + plot_annotation( tag_levels = c('A')) &
    plot_layout(design = design, guides = "collect") + plot_annotation(tag_levels = list(c("A", "B", "C", "D", "E", "F"))) & 
  theme(plot.tag = element_text(size = 12)))

ggsave(plot = layers_viz, filename = paste0(wd_output,'/viz/zoom_maps.pdf'),
       width = 12.3, height = 8)


# Maps --------------------------------------------------------------------

area_data <- arrow::open_dataset(paste0('data/africa_geodata.parquet')) %>% 
  filter(urban_id %in% c('ghsl_2125')) %>% 
  read_sf_dataset() %>%
  st_set_crs(4326) %>%
  st_make_valid()

# 'ghsl_2125 'Lagos'
# 'ghsl_3209 'Kinshasa'
# 'ghsl_3050 'Luanda'
# 'ghsl_3673 'Johannesburg'
# 'ghsl_5134 'Addis Ababa'
# 'ghsl_4335 'Khartoum'
# 'ghsl_5222 'Dar es Salaam'
# 'ghsl_1675 'Abidjan'
# 'ghsl_1910 'Accra'
# 'ghsl_4808 'Nairobi'
# 'ghsl_2717 'Kano'
# 'ghsl_4427 'Kampala'
# 'ghsl_3268 'Cape Town'
# 'ghsl_1452 'Dakar'

width = st_distance(st_sf(geom = st_sfc(st_point(c(st_bbox(area_data)$xmin, st_bbox(area_data)$ymin)),
                                        st_point(c(st_bbox(area_data)$xmax, st_bbox(area_data)$ymin)), crs = 4326)))[2] %>%  drop_units()
height = st_distance(st_sf(geom = st_sfc(st_point(c(st_bbox(area_data)$xmin, st_bbox(area_data)$ymin)),
                                         st_point(c(st_bbox(area_data)$xmin, st_bbox(area_data)$ymax)), crs = 4326)))[2] %>%  drop_units()

width_tenth = round((width*.2)/1000,-1)
if (width_tenth < 1) {
  width_tenth = round((width*.2)/1000,0)
}
height_decdegs = abs(unname(st_bbox(area_data)$ymax) - unname(st_bbox(area_data)$ymin))


(plot_popdensity_log <- ggplot() +
    geom_sf(data = area_data,
            aes(fill = landscan_population_un_density_hectare_log), 
            color = 'white', linewidth= .0075, alpha = 1) +
    labs(subtitle = "Population per hectare",
         caption = paste0(
           'Population: ', area_data %>% st_drop_geometry() %>%
             summarize_at(vars(landscan_population_un), list(sum)) %>% pull() %>% round(.,0) %>% comma(),
           '\n',
           'Average population per hectare: ', area_data %>% st_drop_geometry() %>% filter(landscan_population_un > 0) %>%
             summarize_at(vars(landscan_population_un, block_hectares), list(sum)) %>% mutate(pop_density = landscan_population_un/block_hectares) %>% pull() %>% round(.,1),
           '\n',
           'Median block population per hectare: ', area_data %>% st_drop_geometry() %>% 
             filter(landscan_population_un > 0) %>% summarize_at(vars(landscan_population_un_density_hectare), list(median)) %>% pull() %>% round(.,1), 
                          '')) +
    #' people per hectare','\n 1 hectare = 10k m^2 = 1.4 soccer fields = 2.2 Manhattan city blocks')) +
    scale_fill_distiller(direction = -1, palette = 'Spectral', name = 'Population\nper hectare', oob = scales::squish, limits= c(1, 3), 
                         breaks= c(1,2,3,4,5,6,7), 
                         labels = c('0',"100","1K","10K","100K","1M","10M")) +
    scale_color_distiller(direction = -1, palette = 'Spectral', name = 'Population\nper hectare', oob = scales::squish, limits= c(1, 3), 
                          breaks= c(1,2,3,4,5,6,7), 
                          labels = c('0',"100","1K","10K","100K","1M","10M")) +
    theme_bw() + 
    theme(
      legend.position = c(x = .5, y = 1),
      legend.direction = "horizontal",
      legend.key.width=unit(40,"pt"),
      legend.key.height=unit(5,"pt"),
      axis.ticks =element_blank(),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      plot.margin=unit(c(t=15,r=0,b=0,l=0), "pt"),
      plot.subtitle = element_text(size = 11, face="bold", vjust = 5, hjust = .5),
      plot.caption = element_text(size = 9, hjust = .5, vjust = 10),
      legend.title = element_blank(),
      text = element_text(color = "#333333")) +
    ggsn::scalebar(y.min = st_bbox(area_data)$ymin - (height_decdegs*.04), 
                   x.min = st_bbox(area_data)$xmin, 
                   y.max = st_bbox(area_data)$ymax, 
                   x.max = st_bbox(area_data)$xmax, 
                   location = 'bottomleft',
                   height = .01, box.fill = c('#333333','#ffffff'),
                   border.size = .4, st.color = '#333333', st.size = 2.5, box.color = '#333333',
                   dist = width_tenth/2, dist_unit = "km", transform = TRUE, model = "WGS84") )


ggsave(plot = plot_popdensity_log, filename = paste0(wd_output,'/viz/lagos_popdensity.pdf'),
       width = 10, height = 8)

(plot_populaton <- ggplot() +
    geom_sf(data = area_data,
            aes(fill = landscan_population_un_log), 
            color = 'white', linewidth = .0075, alpha = .8) +
    labs(subtitle = "Population",
         caption = paste0('Total population: ',comma(sum(area_data$landscan_population_un)),'\n',
                          'Average block population: ',comma(round(mean(area_data$landscan_population_un),2)))) + 
    scale_fill_distiller(palette = 'Spectral', name = 'Population', oob = scales::squish, limits= c(1, max(area_data$landscan_population_un_log)), breaks= c(1,2,3,4,5,6,7), labels = c('0',"100","1K","10K","100K","1M","10M")) + 
    scale_color_distiller(palette = 'Spectral', oob = scales::squish, limits= c(1, max(area_data$landscan_population_un_log)), breaks= c(1,2,3,4,5,6,7), labels = c('0',"100","1K","10K","100K","1M","10M")) +
    theme_bw() + 
    theme(
      plot.caption = element_text(size = 10, hjust = .5, vjust = 5),
      legend.position = c(.5, 1),
      legend.direction = "horizontal",
      legend.key.width=unit(40,"pt"),
      legend.key.height=unit(5,"pt"),
      axis.ticks =element_blank(),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      plot.margin = unit(c(t=15,r=0,b=0,l=0), "pt"),
      plot.subtitle = element_text(size = 11, face="bold", vjust = 5, hjust = .5),
      legend.title = element_blank(),
      text = element_text(color = "#333333")) +
    ggsn::scalebar(y.min = st_bbox(area_data)$ymin - (height_decdegs*.04), 
                   x.min = st_bbox(area_data)$xmin, 
                   y.max = st_bbox(area_data)$ymax, 
                   x.max = st_bbox(area_data)$xmax, 
                   location = 'bottomleft',
                   height = .01, box.fill = c('#333333','#ffffff'),
                   border.size = .4, st.color = '#333333', st.size = 2.5, box.color = '#333333',
                   dist = width_tenth/2, dist_unit = "km", transform = TRUE, model = "WGS84") )

plot_popdensity_log + plot_populaton 

# ggsave(plot =layers_viz, filename = paste0(wd_output,'/viz/layers_viz.pdf'),
#        width = 10, height = 8)

area_data <- area_data %>% 
  mutate(k_labels = gsub('-','\n',x = k_labels)) %>%
  mutate(k_labels = factor(k_labels, levels = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10+', 'Off\nnetwork')))  
grey2 <- c('#777777','#414141') 
kdist = max(as.integer(area_data$k_labels))
colorhexes <- colorRampPalette(c('#93328E','#CF3F80','#F7626B','#FF925A','#FFC556','#F9F871'))(length(unique(area_data$k_labels))-2)

(plot_k_discrete <- ggplot() +
    geom_sf(data = area_data, aes(fill = as.factor(k_labels)), color = '#ffffff', linewidth = .0075) +   
    scale_fill_manual(expand = c(0,0), values = c(grey2, colorhexes), name = 'Block complexity') + 
    labs(caption = paste0('Population-weighted average block complexity: ', area_data %>% st_drop_geometry() %>% 
                             summarise(wm_var = weighted.mean(as.integer(k_complexity), landscan_population_un)) %>% pull() %>% round(.,2))) +
    #guides(color = guide_legend(nrow = 1, label.position = "bottom", keywidth = 2, keyheight = 1), 
    #       fill =  guide_legend(nrow = 1, label.position = "bottom", keywidth = 2, keyheight = 1)) + 
    theme_void() + theme(text = element_text(color = "#333333"),
                       legend.position = 'none',
                       legend.spacing.x = unit(1, 'pt'),
                       legend.text = element_text(size = 10),
                       axis.ticks =element_blank(),
                       panel.grid = element_blank(),
                       axis.title = element_blank(),
                       panel.border = element_blank(),
                       #panel.background = element_blank(),
                       plot.margin=unit(c(t=0,r=0,b=0,l=0), "pt"),
                       legend.title = element_blank(),
                       plot.caption = element_text(size = 11, hjust = .5, vjust = 5), #margin=margin(0,0,0,0)),
                       axis.text = element_blank()) +
    ggsn::scalebar(y.min = st_bbox(area_data)$ymin - (height_decdegs*.03), 
                   x.min = st_bbox(area_data)$xmin, 
                   y.max = st_bbox(area_data)$ymax, 
                   x.max = st_bbox(area_data)$xmax, 
                   location = 'bottomleft',
                   height = .01, box.fill = c('#333333','#ffffff'),
                   border.size = .4, st.color = '#333333', st.size = 2.5, box.color = '#333333',
                   dist = width_tenth/2, dist_unit = "km", transform = TRUE, model = "WGS84"))

area_data_sum <- area_data %>% st_drop_geometry() %>%
  group_by(k_labels) %>%
  summarize_at(vars(block_hectares, landscan_population_un, worldpop_population_un), list(sum), na.rm = TRUE) %>%
  ungroup() %>%
  mutate(landscan_pop_density_hectare = landscan_population_un/block_hectares,
         worldpop_pop_density_hectare = worldpop_population_un/block_hectares) %>% 
  replace_na(list(block_pop_density_hectare = 0)) %>%
  mutate(landscan_population_sum = sum(landscan_population_un),
         worldpop_population_sum = sum(worldpop_population_un),
         landscan_population_share = landscan_population_un/landscan_population_sum,
         worldpop_population_share = worldpop_population_un/worldpop_population_sum) 

(bar_k_distrib <- ggplot(area_data_sum) +
    geom_bar(aes(y = landscan_population_un, x = k_labels, fill = k_labels), 
             position="dodge",  stat="identity") +
    geom_text(aes(x = k_labels, y =landscan_population_un, 
                  label = ifelse(landscan_population_share > .05, paste0(round(landscan_population_share*100,0),"%"),'')),
              size = 3, vjust =.5, hjust = 1.15, color = 'white', fontface='bold') +
    geom_text(aes(x = k_labels, y = landscan_population_un, 
                  label = ifelse(landscan_population_share <= .05, paste0(round(landscan_population_share*100,0),"%"),'')),
              size = 3, vjust =.5, hjust = -.15, color = "#333333", fontface='bold') +
    coord_flip() +
    scale_fill_manual(values = c(grey2, colorhexes)) +
    scale_y_continuous(breaks = scales::breaks_pretty(n = 6),
                       expand = expansion(mult = c(0, .1)),
                       limits = c(0, max(area_data_sum$landscan_population_un)),
                       labels = label_comma(accuracy = 1L, scale = 0.000001, suffix = "M") ) +
    theme_bw() + 
    labs(y = 'Population', x = '', subtitle = '') + #'Population distribution across k-complexity levels'
    theme(text = element_text(color = "#333333", size = 8, face = 'bold'),
          legend.position= "none",
          plot.margin=unit(c(t=0,r=0,b=10,l=0), "pt"),
          axis.ticks = element_blank(),
          axis.text = element_text(size = 8),
          panel.grid = element_blank(),
          panel.background = element_rect(fill='transparent'),
          panel.border = element_blank(),
          plot.background = element_rect(fill='transparent'),
          axis.title = element_text(face="bold", size = 10),
          plot.subtitle = element_text(size = 11, face="bold", hjust=.5)))

plot_k_discrete  + bar_k_distrib
  
#plot_k_discrete + inset_element(bar_k_distrib, left = .65, bottom = .4, right = .95, top = .9) + plot_popdensity_log 
#plot_k_discrete + inset_element(bar_k_distrib, left = .65, bottom = .4, right = .95, top = .9)

# Conurbation -------------------------------------------------------------

# Histogram for a single conurban area

conurbation_id_code = 'conurban_273'

histogram_conurban_data <- generate_crosstabs(data = df_combined_prep %>% filter(class_urban_hierarchy %in% c('1 - Core urban', '2 - Peripheral urban', '3 - Peri-urban')), 
                                              group_by_cols = c(conurban_cols, 'class_urban_hierarchy'), 
                                              crosstab_cols = c("k_4"),
                                              sum_cols = sum_cols, divide_cols = divide_cols, 
                                              transform_cols = transform_cols, transform_func_list = list(log10 = log_10),
                                              group_by_agg_cols_list = list('conurbation' = "conurbation_id"),
                                              group_by_agg_func_list = list(sum = sum, share = share),
                                              agg_cols = c('landscan_population_un', 'worldpop_population_un'))

k_order <- c('1','2','3','4','5','6','7','8','9','10', "11 to\n15","16 to\n20","21+","Off\nnetwork")
histogram_conurban_data <- histogram_conurban_data %>% filter(conurbation_id == conurbation_id_code) %>%
  mutate(group_val = str_wrap(group_val, width=6),
         group_val = factor(group_val, levels = k_order)) %>%
  group_by(group_val) %>%
  mutate(k_sum = sum(landscan_population_un)) %>%
  ungroup() %>%
  mutate(share = landscan_population_un / k_sum ) %>%
  mutate(class_urban_hierarchy = factor(class_urban_hierarchy, levels = c( "1 - Core urban","2 - Peripheral urban","3 - Peri-urban","4 - Non-urban"))) %>%
  arrange(factor(class_urban_hierarchy, levels = rev(c( "1 - Core urban","2 - Peripheral urban","3 - Peri-urban","4 - Non-urban"))), group_val) %>%
  group_by(group_val) %>%
  mutate(pos_id_val = (cumsum(landscan_population_un) - 0.5*landscan_population_un)) %>%
  ungroup() 

(histogram_conurban <- ggplot() +
    geom_bar(data = histogram_conurban_data, aes(y = group_val, x = landscan_population_un, fill = class_urban_hierarchy), color = 'white', size = .3, stat="identity") +
    coord_flip() + 
    labs(y= 'k-complexity', x = 'Population', fill = '', color =  '', subtitle = 'Lagos-Ikorodu (Nigeria)') + 
    geom_text(data =  histogram_conurban_data, aes(y = group_val, x = pos_id_val, label = ifelse(landscan_population_un >= 500000, paste0(round(share*100,0),"%"),'')), size = 3, vjust = .5, color = '#333333', fontface='bold') +
    scale_fill_manual(values = c('#4D96FF','#6BCB77','#FFD93D','#FF6B6B')) +
    scale_x_continuous(expand = c(.005,.005), labels = label_comma(accuracy = 1L, scale =  0.000001, suffix = "M") ) +
    theme(legend.position = 'bottom', panel.background = element_rect(fill = "white"), axis.ticks = element_line(colour = "grey", size = .2), panel.grid.major = element_line(colour = "grey", size = .2)))

#ggsave(plot = histogram_conurban, filename = paste0(wd_output,'/viz/histogram_conurban.pdf'), width = 6, height = 5.64)
rm(histogram_conurban_data, histogram_conurban)

# Scatter plots -----------------------------------------------------------

scatter_by_conurbation_4way <- generate_crosstabs(data = df_combined_prep %>% filter(class_urban_hierarchy %in% c('1 - Core urban', '2 - Peripheral urban', '3 - Peri-urban')), 
                                                  group_by_cols = c(conurban_cols, 'class_urban_hierarchy'), 
                                                  crosstab_cols = c("k_0"),
                                                  sum_cols = sum_cols, divide_cols = divide_cols, 
                                                  transform_cols = transform_cols, transform_func_list = list(log10 = log_10),
                                                  group_by_agg_cols_list = list('conurbation' = "conurbation_id",
                                                                                'country' = 'conurbation_country_code'),
                                                  group_by_agg_func_list = list(sum = sum, share = share),
                                                  agg_cols = c('landscan_population_un', 'worldpop_population_un'))

scatter_by_conurbation_4way <- scatter_by_conurbation_4way %>%
  mutate(conurbation_country_name=  (gsub('Democratic Republic of the Congo', 'DR Congo', as.character(conurbation_country_name))),
         conurbation_area_name_short = gsub('Ambatolampy Tsimahafotsy', 'Ambatolampy', as.character(conurbation_area_name_short))) %>% 
  #mutate(conurbation_name = paste0(conurbation_area_name_short,', ',conurbation_country_name)) %>%
  #mutate(conurbation_first_name = conurbation_area_name_short) %>%
  mutate(conurbation_country_name =  (gsub('–', '-', as.character(conurbation_country_name)))) %>%
  mutate(conurbation_country_code =  (gsub('–', '-', as.character(conurbation_country_code)))) %>%
  mutate(conurbation_name = paste0(conurbation_area_name_short,', ',conurbation_country_code)) %>%
  separate(col = conurbation_area_name_short, sep = '-', into = c('conurbation_first_name'), extra = 'drop') %>%
  mutate(conurbation_name_2line = paste0(conurbation_first_name,'\n',conurbation_country_code)) %>%
  filter(agg_sum_landscan_population_un_group_by_conurbation >= 2000000) %>%
  mutate(class_urban_hierarchy = factor(class_urban_hierarchy, levels = c( "1 - Core urban","2 - Peripheral urban","3 - Peri-urban","4 - Non-urban"))) %>% 
  mutate(k_reweight = landscan_population_un*k_complexity_weighted_landscan_un) %>%
  group_by(conurbation_country_code) %>%
  mutate(k_reweight = sum(k_reweight)) %>%
  ungroup() %>%
  mutate(k_reweight = k_reweight/agg_sum_landscan_population_un_group_by_country) 

(scatter_by_conurbation_4way_1 <- ggplot() +
    geom_point(data = scatter_by_conurbation_4way, aes(x = reorder(conurbation_name, agg_sum_landscan_population_un_group_by_conurbation), y = landscan_population_un_log10, fill = class_urban_hierarchy, color = class_urban_hierarchy, size = k_complexity_weighted_landscan_un), alpha = .75) +
    scale_size(range = c(1,10)) + 
    geom_text(data = scatter_by_conurbation_4way, aes(x = reorder(conurbation_name, agg_sum_landscan_population_un_group_by_conurbation), y = landscan_population_un_log10, label = round(k_complexity_weighted_landscan_un , 1)), size = 3, vjust = .5, color = '#333333', fontface='bold', check_overlap = TRUE) +
    scale_y_continuous(oob = scales::squish, breaks= c(1,2,3,4,5,6,7,8,9), labels = c('0',"100","1K","10K","100K","1M","10M","100M","1B"))+
    scale_fill_manual(values = c('#4D96FF','#6BCB77','#FFD93D','#FF6B6B')) +
    scale_color_manual(values = c('#4D96FF','#6BCB77','#FFD93D','#FF6B6B')) +
    labs(x= '', y = 'Population', size = 'Average block complexity', fill = '', color =  '') +
    guides(color = guide_legend(override.aes = list(size = 6))) +
    coord_flip() +
    guides(color = guide_legend(override.aes = list(size = 8, alpha = 1))) +
    theme(legend.key = element_rect(fill = NA), legend.spacing.y = unit(0, "cm"),
          text = element_text(size = 13), legend.position = 'bottom', 
          legend.box = 'vertical', legend.box.just = "center"))
#theme(legend.position = 'bottom', legend.key=element_blank()))

(scatter_by_conurbation_4way_2 <- ggplot() +
    geom_point(data = scatter_by_conurbation_4way, aes(x =  reorder(conurbation_name, k_reweight), y = k_complexity_weighted_landscan_un, fill = class_urban_hierarchy, color = class_urban_hierarchy, size = landscan_population_un), alpha = .75) +
    geom_text(data = scatter_by_conurbation_4way, aes(x =  reorder(conurbation_name, k_reweight ), y =  k_complexity_weighted_landscan_un, label = ifelse(landscan_population_un >= 100000, paste0(round(landscan_population_un/1000000,1),'M'),'' ) ), size = 3, vjust = .5, color = '#333333', fontface='bold', check_overlap = TRUE) +
    labs(x= '', y = 'Average K-complexity', size = 'Population', fill = '', color =  '') +
    coord_flip() +
    scale_fill_manual(values = c('#4D96FF','#6BCB77','#FFD93D','#FF6B6B')) +
    scale_color_manual(values = c('#4D96FF','#6BCB77','#FFD93D','#FF6B6B')) +
    scale_size_continuous(range = c(1,10), labels = label_comma(accuracy = 1L, scale =  0.000001, suffix = "M") ) +
    guides(color = guide_legend(override.aes = list(size = 8, alpha = 1))) +
    theme(legend.key = element_rect(fill = NA), legend.spacing.y = unit(0, "cm"),
          text = element_text(size = 13), legend.position = 'bottom', 
          legend.box = 'vertical', legend.box.just = "center"))
#guides(color = guide_legend(override.aes = list(size = 6))) +
#theme(legend.position = 'bottom', legend.key=element_blank()))

(scatter_by_conurbation_4way_3 <- ggplot() +
    geom_point(data = scatter_by_conurbation_4way %>% filter(agg_sum_landscan_population_un_group_by_conurbation >= 50000), 
               aes(x = log10(k_complexity_weighted_landscan_un), y = log10(landscan_population_un), fill = class_urban_hierarchy, color = class_urban_hierarchy, size = landscan_population_un ), alpha = .7) +
    coord_flip() +
    labs(x= 'Average block complexity', y = 'Population', size = '', fill = '', color =  '') +
    scale_fill_manual(values = c('#4D96FF','#6BCB77','#FFD93D','#FF6B6B')) +
    scale_color_manual(values = c('#4D96FF','#6BCB77','#FFD93D','#FF6B6B')) +
    scale_size(range = c(2, 20), breaks = c(10000,100000,1000000,10000000), labels = c('10K','100K','1M','10M')) +
    scale_x_continuous(oob = scales::squish, breaks= c(0, 0.30103, 0.4771213, 0.60206, 0.69897, 0.7781513, 0.845098, 0.90309, 0.9542425, 1,1.041393,1.079181,1.113943,1.146128,1.176091,1.20412,1.230449,1.255273,1.278754,1.30103,1.322219,1.342423,1.361728,1.380211,1.39794), labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25) )+
    scale_y_continuous(oob = scales::squish, breaks= c(1,2,3,4,5,6,7,8,9), labels = c('0',"100","1K","10K","100K","1M","10M","100M","1B"))+
    geom_text_repel(data = scatter_by_conurbation_4way %>% filter(agg_sum_landscan_population_un_group_by_conurbation >= 50000), seed = 1, segment.curvature = -0.1, point.padding = 0, box.padding = 0.4, max.iter = 30000, segment.square  = FALSE, segment.inflect = FALSE, min.segment.length = .2, max.overlaps = Inf, force = .5, force_pull = 10, 
                    aes(x = log10(k_complexity_weighted_landscan_un), y = log10(landscan_population_un), label = conurbation_name_2line ), size = 3, vjust =.5, color = '#333333', fontface='bold') + 
    guides(color = guide_legend(override.aes = list(size = 6, alpha =1))) +
    theme(legend.key = element_rect(fill = NA), panel.background = element_rect(fill = "white"), 
          legend.spacing.y = unit(0, "cm"),
          #legend.margin=margin(t=2,r=0,b=2,l=0),
          #legend.box.margin=margin(0,0,0,0),
          axis.ticks = element_line(colour = "grey", size = .2), panel.grid.major = element_line(colour = "grey", size = .2), 
          text = element_text(size = 13), legend.position = 'bottom', legend.box = 'vertical', legend.box.just = "center"))

#ggsave(plot = scatter_by_conurbation_4way_1, filename = paste0(wd_output,'/viz/scatter_population_conurbations.pdf'))
#ggsave(plot = scatter_by_conurbation_4way_2, filename = paste0(wd_output,'/viz/scatter_complexity_conurbations.pdf'))
ggsave(plot = scatter_by_conurbation_4way_3, filename = paste0(wd_output,'/viz/scatter_population_vs_complexity_conurbations.pdf'), width = 14, height = 12)
rm(scatter_by_conurbation_4way, scatter_by_conurbation_4way_1, scatter_by_conurbation_4way_2, scatter_by_conurbation_4way_3)


