
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
library(sfarrow)
library(osmdata)
library(viridis)
library(scatterpie)
library(ggrepel)
library(ggpmisc)
library(Hmisc)
library(tidymodels)
options(scipen = 999)
gc()

# -------------------------------------------------------------------------

# Load aggregation function
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
source('aggregation_func.R') # should be in the ~/kblock-analysis directory

# Read in data ------------------------------------------------------------

# Street data reported at GHSL level
df_streets <- read_parquet(paste0('data/streets_metrics.parquet'))
load("data/dhs-analysis/data/dhs_data.RData")
# Requires authorization from DHS: https://uchicago.box.com/s/anw4fxc376dgtgol9tt87tuozipnk0kj

# Concordance between GHSL-country layers and DHS regions
ghsl_to_dhs <- read_parquet(paste0('data/ghsl_to_dhs.parquet'))

# DHS indicators in PCA universe
indicator_list <- c("EM_OCCP_M_AGR", "EM_OCCP_M_MNS", "EM_OCCP_M_PRO", "EM_OCCP_W_AGR", "EM_OCCP_W_PRO", "HC_WIXQ_P_LOW", "HC_WIXQ_P_2ND", "HC_WIXQ_P_MID", "HC_WIXQ_P_4TH", "HC_WIXQ_P_HGH", "HC_WIXQ_P_GNI", "HC_HEFF_H_FRG", "HC_HEFF_H_MPH", "HC_HEFF_H_TLV", "HC_TRNS_H_CAR", "ED_EDAT_B_MYR", "ED_EDAT_M_MYR", "ED_EDAT_W_MYR", "ED_EDAT_B_NED", "ED_EDAT_B_SEC", "ED_EDAT_B_HGH", "ED_LITR_W_LIT", "ED_LITY_W_LIT", "ED_LITY_W_NRD", "ED_EDUC_W_SEH", "ED_EDAT_W_NED", "ED_EDAT_W_SEC", "ED_EDAT_W_HGH", "ED_MDIA_W_3MD", "ED_MDIA_W_N3M", "FP_NADM_W_MNT", "RH_DELP_C_DHF", "CM_ECMR_C_CMR", "CM_ECMR_C_IMR", "CM_ECMR_C_PNR", "CM_ECMR_C_U5M", "CH_VACC_C_BAS", "CN_IYCF_C_4FA", "CN_NUTS_C_HA2", "CN_NUTS_C_WA2", "CN_NUTS_C_WH2", "CN_ANMC_C_ANY", "WS_TLET_P_BAS", "WS_TLET_P_IMP", "WS_TLET_P_NFC", "WS_SRCE_P_BAS", "WS_SRCE_P_IMP", "WS_SRCE_P_IOP", "WS_SRCE_P_LTD", "WS_SRCE_P_NIM", "WS_SRCE_P_PIP", "WS_TIME_P_L30", "WS_TIME_P_M30", "WS_TIME_P_ONP", "HC_PPRM_H_MNP", "HC_OLDR_H_3GN", "HC_PPRM_H_12P", "HC_PPRM_H_34P", "HC_PPRM_H_56P", "HC_PPRM_H_7PP", "HC_CKPL_P_HSE", "HC_CKFL_P_CLN", "HC_CKFL_P_SLD", "HC_ELEC_P_ELC", "HC_ELEC_P_NEL", "HC_FLRM_P_ETH", "HC_FLRM_P_NAT")

# Load DHS data
df_dhs <- subnat_all_wide %>% st_drop_geometry() %>%
  select(SurveyId, REG_ID, all_of(indicator_list)) %>%
  mutate(across(all_of(indicator_list), .fns = ~(./100)))
  #select(SurveyId, REG_ID, contains(indicator_list)) %>%
#  mutate(across(all_of(indicator_list), .fns = ~(./100), .names = 'num_{.col}')) %>%
#  select(starts_with(c('SurveyId','REG_ID',"num_",'dw_'))) %>%
#  mutate_at(vars(starts_with('dw_')), list(as.numeric)) %>%
#  mutate(across(starts_with("num_"), ~ . * get(paste0("dw_", sub("num_", "", cur_column()))))) %>% 
#  distinct()

# Aggregate block level k complexity to GHSL levels -----------------------

if (!file.exists(paste0("data/africa_data.parquet"))) {
  curl::multi_download("dsbprylw7ncuq.cloudfront.net/AF/africa_data.parquet", "data/africa_data.parquet", resume = TRUE)
} else {
  df_combined <- read_parquet(paste0("data/africa_data.parquet"))
}

names(df_combined)
unique(df_combined$k_labels)

group_cols = c("k_labels_detailed","urban_layer_code","conurbation_id", "conurbation_area_name_short", "urban_id", "urban_center_name", "country_code","class_urban_hierarchy")
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

df_k <- generate_crosstabs(data = df_combined_prep %>% mutate(total = 'Total'), 
                               group_by_cols = c("urban_layer_code","conurbation_id", "conurbation_area_name_short", "urban_id", "urban_center_name", "country_code","class_urban_hierarchy"),
                               crosstab_cols = c("total"),
                               sum_cols = sum_cols, divide_cols = divide_cols, 
                               transform_cols = transform_cols, transform_func_list = list(log10 = log_10),
                               group_by_agg_cols_list = list('country' = "country_code"),
                               group_by_agg_func_list = list(sum = sum, share = share),
                               agg_cols = c('landscan_population_un', 'worldpop_population_un'))

df_k_country <- generate_crosstabs(data = df_combined_prep %>% mutate(total = 'Total'), 
                           group_by_cols = c("country_code"),
                           crosstab_cols = c("total"),
                           sum_cols = sum_cols, divide_cols = divide_cols, 
                           transform_cols = transform_cols, transform_func_list = list(log10 = log_10),
                           group_by_agg_cols_list = list('country' = "country_code"),
                           group_by_agg_func_list = list(sum = sum, share = share),
                           agg_cols = c('landscan_population_un', 'worldpop_population_un'))

df_k_detail_country <- generate_crosstabs(data = df_combined_prep %>% mutate(total = 'Total'), 
                                   group_by_cols = c("k_labels_detailed","country_code"),
                                   crosstab_cols = c("total"),
                                   sum_cols = sum_cols, divide_cols = divide_cols, 
                                   transform_cols = transform_cols, transform_func_list = list(log10 = log_10),
                                   group_by_agg_cols_list = list('country' = "country_code"),
                                   group_by_agg_func_list = list(sum = sum, share = share),
                                   agg_cols = c('landscan_population_un', 'worldpop_population_un'))

df_k_detail_country <- df_k_detail_country %>% 
  select(k_labels_detailed, country_code, landscan_population_un) %>%
  mutate(k_label_detailed = factor(k_labels_detailed, c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30+", "Off-network"))) %>%
  pivot_wider(id_cols = country_code, names_from = k_labels_detailed, values_from = landscan_population_un,
              values_fill = 0, names_prefix = 'k_') %>%
  relocate(all_of(c("country_code", "k_1", "k_2", "k_3", "k_4", "k_5", "k_6", "k_7", "k_8", "k_9", "k_10", "k_11", "k_12", "k_13", "k_14", "k_15", "k_16", "k_17", "k_18", "k_19", "k_20", "k_21", "k_22", "k_23", "k_24", "k_25", "k_26", "k_27", "k_28", "k_29", "k_30+", "k_Off-network"))) %>%
  select_all(~gsub("\\s+|\\.|\\/|,|\\*|-|\\+", "_", .)) %>% rename_all(list(tolower)) %>%
  rowwise() %>%
  mutate(total_pop = sum(c_across(k_1:k_off_network))) %>%
  mutate(k_1_range = sum(c_across(k_1:k_off_network))/total_pop) %>%
  mutate(k_2_range = sum(c_across(k_2:k_off_network))/total_pop) %>%
  mutate(k_3_range = sum(c_across(k_3:k_off_network))/total_pop) %>%
  mutate(k_4_range = sum(c_across(k_4:k_off_network))/total_pop) %>%
  mutate(k_5_range = sum(c_across(k_5:k_off_network))/total_pop) %>%
  mutate(k_6_range = sum(c_across(k_6:k_off_network))/total_pop) %>%
  mutate(k_7_range = sum(c_across(k_7:k_off_network))/total_pop) %>%
  mutate(k_8_range = sum(c_across(k_8:k_off_network))/total_pop) %>%
  mutate(k_9_range = sum(c_across(k_9:k_off_network))/total_pop) %>%
  mutate(k_10_range = sum(c_across(k_10:k_off_network))/total_pop) %>%
  mutate(k_11_range = sum(c_across(k_11:k_off_network))/total_pop) %>%
  mutate(k_12_range = sum(c_across(k_12:k_off_network))/total_pop) %>%
  mutate(k_13_range = sum(c_across(k_13:k_off_network))/total_pop) %>%
  mutate(k_14_range = sum(c_across(k_14:k_off_network))/total_pop) %>%
  mutate(k_15_range = sum(c_across(k_15:k_off_network))/total_pop) %>%
  mutate(k_16_range = sum(c_across(k_16:k_off_network))/total_pop) %>%
  mutate(k_17_range = sum(c_across(k_17:k_off_network))/total_pop) %>%
  mutate(k_18_range = sum(c_across(k_18:k_off_network))/total_pop) %>%
  mutate(k_19_range = sum(c_across(k_19:k_off_network))/total_pop) %>%
  mutate(k_20_range = sum(c_across(k_20:k_off_network))/total_pop) %>%
  mutate(k_21_range = sum(c_across(k_21:k_off_network))/total_pop) %>%
  mutate(k_22_range = sum(c_across(k_22:k_off_network))/total_pop) %>%
  mutate(k_23_range = sum(c_across(k_23:k_off_network))/total_pop) %>%
  mutate(k_24_range = sum(c_across(k_24:k_off_network))/total_pop) %>%
  mutate(k_25_range = sum(c_across(k_25:k_off_network))/total_pop) %>%
  mutate(k_26_range = sum(c_across(k_26:k_off_network))/total_pop) %>%
  mutate(k_27_range = sum(c_across(k_27:k_off_network))/total_pop) %>%
  mutate(k_28_range = sum(c_across(k_28:k_off_network))/total_pop) %>%
  mutate(k_29_range = sum(c_across(k_29:k_off_network))/total_pop) %>%
  mutate(k_30__range = sum(c_across(k_30_:k_off_network))/total_pop) %>%
  mutate(k_off_network_range = sum(c_across(k_off_network:k_off_network))/total_pop) 
  
rm(df_combined_prep, divide_cols, collapse_cols, sum_cols, transform_cols)
gc()

# Combined data -----------------------------------------------------------
#' 
#' # Join streets, k complexity, and DHS data together
#' df_streets_vehicular <- df_streets %>%
#'   filter(vehicular_classification == 'Vehicular roads') %>%
#'   group_by(urban_layer_code) %>%
#'   summarize_at(vars(street_length_meters), list(sum)) %>%
#'   ungroup()
#' 
#' df_k <- df_k %>%
#'   mutate(bldg_area_count = (bldg_area_count_bin_01_0.50__log10_3.2 + bldg_area_count_bin_02_0.75__log10_5.6 + bldg_area_count_bin_03_1.00__log10_10 + bldg_area_count_bin_04_1.25__log10_17.8),
#'          region_urban_pop = case_when(class_urban_hierarchy %in% c("1 - Core urban", "2 - Peripheral urban") ~ 1*landscan_population_un, TRUE ~ 0),
#'          region_nonurban_pop = case_when(class_urban_hierarchy %in% c("1 - Core urban", "2 - Peripheral urban") ~ 0 , TRUE ~ 1*landscan_population_un))
#'   
#' df_dhs_streets_k <- ghsl_to_dhs %>%
#'   select(REG_ID, SurveyId, ISO3_CountryCode, urban_layer_code, area_reg_part, area_reg_whole, area_share, area_rank) %>%
#'   left_join(., df_streets_vehicular, by = c('urban_layer_code')) %>%
#'   left_join(., df_k %>% select(urban_layer_code, k_complexity_weighted_landscan_un, region_urban_pop, region_nonurban_pop, landscan_population_un, block_hectares, building_area_m2, block_area_m2, bldg_area_count, building_count), by = c('urban_layer_code')) 
#' 
#' 
#' df_dhs_streets_k <- df_dhs_streets_k %>%
#'   mutate(ghsl_area_share = area_reg_part/block_area_m2,
#'          ghsl_area_share = ifelse(ghsl_area_share > 1, 1, ghsl_area_share)) %>%
#'   mutate(street_length_meters_wt = street_length_meters * ghsl_area_share,
#'          landscan_population_un_wt = landscan_population_un * ghsl_area_share,
#'          block_hectares_wt = block_hectares * ghsl_area_share,
#'          building_area_m2_wt = building_area_m2 * ghsl_area_share,
#'          block_area_m2_wt = block_area_m2 * ghsl_area_share,
#'          building_count_wt = building_count * ghsl_area_share,
#'          bldg_area_count_wt = bldg_area_count * ghsl_area_share,
#'          building_count_wt = building_count * ghsl_area_share,
#'          region_urban_pop_wt = region_urban_pop * ghsl_area_share,
#'          region_nonurban_pop_wt = region_nonurban_pop * ghsl_area_share,
#'          k_complexity_weighted_landscan_un_wt = (k_complexity_weighted_landscan_un*landscan_population_un_wt)
#'   ) %>%
#'   group_by(REG_ID, SurveyId, ISO3_CountryCode) %>%
#'   summarize_at(vars(area_reg_part, street_length_meters_wt, k_complexity_weighted_landscan_un_wt, landscan_population_un_wt, 
#'                     region_urban_pop_wt, region_nonurban_pop_wt, block_hectares_wt, building_area_m2_wt, block_area_m2_wt, building_count_wt, bldg_area_count_wt, building_count_wt), list(sum)) %>%
#'   ungroup() %>%
#'   mutate(
#'     street_density_ratio_km_to_km2 = (street_length_meters_wt*0.001) / (area_reg_part*1e-6),
#'     k_complexity = k_complexity_weighted_landscan_un_wt / landscan_population_un_wt,
#'     landscan_population_un_density_hectare = replace_na(landscan_population_un_wt/block_hectares_wt,0),
#'     region_urban_share = region_urban_pop_wt / landscan_population_un_wt,
#'     region_nonurban_share = region_nonurban_pop_wt / landscan_population_un_wt,
#'     building_to_block_area_ratio = replace_na(building_area_m2_wt/block_area_m2_wt,0),
#'     average_building_area_m2 = replace_na(building_area_m2_wt/building_count_wt,0),
#'     average_pop_per_building = replace_na(landscan_population_un_wt/building_count_wt,0),
#'     share_building_count_under_31m2 = replace_na(bldg_area_count_wt/building_count_wt,0)
#'   ) %>%
#'   left_join(., df_dhs, by = c('REG_ID'='REG_ID','SurveyId'='SurveyId'))
#' 
#' # Load up DHS labels
#' custom_labels <- list(
#'   'column_var' = c('EM_OCCP_M_AGR', 'EM_OCCP_M_MNS', 'EM_OCCP_M_MNU', 'EM_OCCP_M_PRO', 'EM_OCCP_W_AGR', 'EM_OCCP_W_DOM', 'EM_OCCP_W_MNU', 'EM_OCCP_W_PRO', 'HC_WIXQ_P_LOW', 'HC_WIXQ_P_2ND', 'HC_WIXQ_P_MID', 'HC_WIXQ_P_4TH', 'HC_WIXQ_P_HGH', 'HC_WIXQ_P_GNI', 'HC_HEFF_H_CMP', 'HC_HEFF_H_FRG', 'HC_HEFF_H_MPH', 'HC_HEFF_H_TLV', 'HC_TRNS_H_CAR', 'ED_EDAT_B_MYR', 'ED_EDAT_M_MYR', 'ED_EDAT_W_MYR', 'ED_EDAT_B_NED', 'ED_EDAT_B_SEC', 'ED_EDAT_B_HGH', 'ED_NARP_B_BTH', 'ED_NARS_B_BTH', 'ED_GARS_B_GPI', 'ED_LITR_W_LIT', 'ED_LITY_W_LIT', 'ED_LITY_W_NRD', 'ED_NARP_W_FEM', 'ED_NARS_W_FEM', 'ED_EDUC_W_SEH', 'ED_EDAT_W_NED', 'ED_EDAT_W_SEC', 'ED_EDAT_W_HGH', 'ED_MDIA_W_3MD', 'ED_MDIA_W_N3M', 'CO_MOBB_W_BNK', 'CO_MOBB_W_MOB', 'AN_NUTS_W_THN', 'FP_NADM_W_MNT', 'RH_DELP_C_DHF', 'CM_ECMR_C_CMR', 'CM_ECMR_C_IMR', 'CM_ECMR_C_PNR', 'CM_ECMR_C_U5M', 'CH_VACC_C_BAS', 'CN_IYCF_C_4FA', 'CN_NUTS_C_HA2', 'CN_NUTS_C_WA2', 'CN_NUTS_C_WH2', 'CN_ANMC_C_ANY', 'WS_HNDW_P_BAS', 'WS_HNDW_P_SOP', 'WS_TLOC_P_DWL', 'WS_TLET_P_BAS', 'WS_TLET_P_IMP', 'WS_TLET_P_NFC', 'WS_SRCE_P_BAS', 'WS_SRCE_P_IMP', 'WS_SRCE_P_IOP', 'WS_SRCE_P_LTD', 'WS_SRCE_P_NIM', 'WS_SRCE_P_PIP', 'WS_TIME_P_L30', 'WS_TIME_P_M30', 'WS_TIME_P_ONP', 'HC_PPRM_H_MNP', 'HC_OLDR_H_3GN', 'HC_PPRM_H_12P', 'HC_PPRM_H_34P', 'HC_PPRM_H_56P', 'HC_PPRM_H_7PP', 'HC_CKPL_P_HSE', 'HC_CKFL_P_CLN', 'HC_CKFL_P_SLD', 'HC_ELEC_P_ELC', 'HC_ELEC_P_NEL', 'HC_FLRM_P_ETH', 'HC_FLRM_P_NAT', 'WS_HNDW_H_BAS', 'WS_HNDW_H_SOP', 'WS_TLOC_H_DWL', 'WS_TLET_H_BAS', 'WS_TLET_H_IMP', 'WS_TLET_H_NFC', 'WS_SRCE_H_BAS', 'WS_SRCE_H_IMP', 'WS_SRCE_H_IOP', 'WS_SRCE_H_LTD', 'WS_SRCE_H_NIM', 'WS_SRCE_H_PIP', 'WS_TIME_H_L30', 'WS_TIME_H_M30', 'WS_TIME_H_ONP', 'HC_CKPL_H_HSE', 'HC_CKFL_H_CLN', 'HC_CKFL_H_SLD', 'HC_ELEC_H_ELC', 'HC_ELEC_H_NEL', 'HC_FLRM_H_ETH', 'HC_FLRM_H_NAT'),
#'   'order' = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104),
#'   'positive_dim' = c(0, 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 1, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1, 1, 0, 1, 0, 0, 0),
#'   'unit' = c('Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Households', 'Households', 'Households', 'Households', 'Households', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Households', 'Households', 'Households', 'Households', 'Households', 'Households', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Households', 'Households', 'Households', 'Households', 'Households', 'Households', 'Households', 'Households', 'Households', 'Households', 'Households', 'Households', 'Households', 'Households', 'Households', 'Households', 'Households', 'Households', 'Households', 'Households', 'Households', 'Households'),
#'   'household_duplicate' = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
#'   'category' = c("Economic Wellbeing", "Economic Wellbeing", "Economic Wellbeing", "Economic Wellbeing", "Economic Wellbeing", "Economic Wellbeing", "Economic Wellbeing", "Economic Wellbeing", "Economic Wellbeing", "Economic Wellbeing", "Economic Wellbeing", "Economic Wellbeing", "Economic Wellbeing", "Economic Wellbeing", "Economic Wellbeing", "Economic Wellbeing", "Economic Wellbeing", "Economic Wellbeing", "Economic Wellbeing", "Education and Literacy", "Education and Literacy", "Education and Literacy", "Education and Literacy", "Education and Literacy", "Education and Literacy", "Education and Literacy", "Education and Literacy", "Education and Literacy", "Education and Literacy", "Education and Literacy", "Education and Literacy", "Education and Literacy", "Education and Literacy", "Education and Literacy", "Education and Literacy", "Education and Literacy", "Education and Literacy", "Education and Literacy", "Education and Literacy", "Education and Literacy", "Education and Literacy", "Children and Women's Health", "Children and Women's Health", "Children and Women's Health", "Children and Women's Health", "Children and Women's Health", "Children and Women's Health", "Children and Women's Health", "Children and Women's Health", "Children and Women's Health", "Children and Women's Health", "Children and Women's Health", "Children and Women's Health", "Children and Women's Health", "Water, Sanitation and Hygiene", "Water, Sanitation and Hygiene", "Water, Sanitation and Hygiene", "Water, Sanitation and Hygiene", "Water, Sanitation and Hygiene", "Water, Sanitation and Hygiene", "Water, Sanitation and Hygiene", "Water, Sanitation and Hygiene", "Water, Sanitation and Hygiene", "Water, Sanitation and Hygiene", "Water, Sanitation and Hygiene", "Water, Sanitation and Hygiene", "Water, Sanitation and Hygiene", "Water, Sanitation and Hygiene", "Water, Sanitation and Hygiene", "Household Characteristics", "Household Characteristics", "Household Characteristics", "Household Characteristics", "Household Characteristics", "Household Characteristics", "Household Characteristics", "Household Characteristics", "Household Characteristics", "Household Characteristics", "Household Characteristics", "Household Characteristics", "Household Characteristics", "Water, Sanitation and Hygiene", "Water, Sanitation and Hygiene", "Water, Sanitation and Hygiene", "Water, Sanitation and Hygiene", "Water, Sanitation and Hygiene", "Water, Sanitation and Hygiene", "Water, Sanitation and Hygiene", "Water, Sanitation and Hygiene", "Water, Sanitation and Hygiene", "Water, Sanitation and Hygiene", "Water, Sanitation and Hygiene", "Water, Sanitation and Hygiene", "Water, Sanitation and Hygiene", "Water, Sanitation and Hygiene", "Water, Sanitation and Hygiene", "Household Characteristics", "Household Characteristics", "Household Characteristics", "Household Characteristics", "Household Characteristics", "Household Characteristics", "Household Characteristics"),
#'   'subcategory' = c("Livelihoods", "Livelihoods", "Livelihoods", "Livelihoods", "Livelihoods", "Livelihoods", "Livelihoods", "Livelihoods", "Wealth", "Wealth", "Wealth", "Wealth", "Wealth", "Wealth", "Household effects", "Household effects", "Household effects", "Household effects", "Household effects", "Education", "Education", "Education", "Education", "Education", "Education", "Education", "Education", "Women's education", "Women's education", "Women's education", "Women's education", "Women's education", "Women's education", "Women's education", "Women's education", "Women's education", "Women's education", "Women's education", "Women's education", "Women's education", "Women's education", "Women's health", "Women's health", "Women's health", "Children's health", "Children's health", "Children's health", "Children's health", "Children's health", "Children's health", "Children's health", "Children's health", "Children's health", "Children's health", "Handwashing", "Handwashing", "Sanitation", "Sanitation", "Sanitation", "Sanitation", "Water", "Water", "Water", "Water", "Water", "Water", "Water", "Water", "Water", "Crowding", "Crowding", "Crowding", "Crowding", "Crowding", "Crowding", "Cooking", "Cooking", "Cooking", "Electricity", "Electricity", "Floor materials", "Floor materials", "Handwashing", "Handwashing", "Sanitation", "Sanitation", "Sanitation", "Sanitation", "Water", "Water", "Water", "Water", "Water", "Water", "Water", "Water", "Water", "Cooking", "Cooking", "Cooking", "Electricity", "Electricity", "Floor materials", "Floor materials")
#' ) %>% as_tibble()
#' 
#' dhs_dict_df <- rdhs::dhs_indicators() %>%
#'   select(Definition, MeasurementType, ShortName, IndicatorId, Level1, Level2, Level3, Label) %>%
#'   rename_all(list(tolower)) %>%
#'   relocate(indicatorid, definition, label, level1, level2, level3, shortname, measurementtype) %>%
#'   left_join(., custom_labels, by = c('indicatorid' = 'column_var')) %>%
#'   filter(!is.na(order)) %>%
#'   arrange(order)
#' 
#' # Analysis ----------------------------------------------------------------
#' 
#' # DHS correlation with street density vs K
#' (cor_streets_k <- rcorr(df_dhs_streets_k %>%
#'     select_at(all_of(c(indicator_list,"street_density_ratio_km_to_km2","k_complexity"))) %>% as.matrix(),
#'                   type=c('spearman')) %>% tidy() %>% filter(column1 %in% c('k_complexity', "street_density_ratio_km_to_km2")) %>%
#'   pivot_wider(names_from = 'column1', values_from = c(estimate, p.value, n)) %>%
#'   inner_join(., dhs_dict_df , by = c('column2' = 'indicatorid')) %>%
#'     rename(column_var = column2) %>%
#'     select(category, subcategory, label, estimate_street_density_ratio_km_to_km2, estimate_k_complexity, p.value_street_density_ratio_km_to_km2, p.value_k_complexity, n_street_density_ratio_km_to_km2, n_k_complexity, definition, column_var) %>%
#'   mutate(larger_coefficient = case_when(abs(estimate_street_density_ratio_km_to_km2) > abs(estimate_k_complexity) ~ 'Street density',
#'                                         TRUE ~ as.character('k-complexity')),
#'          not_significant_street_density  = case_when( p.value_street_density_ratio_km_to_km2 > .01 ~ 1,
#'                                         TRUE ~ 0),
#'          not_significant_k_complexity = case_when( p.value_k_complexity > .01 ~ 1,
#'                                        TRUE ~ 0))
#' )
#' 
#' # DHS PC1 correlation with street density vs K
#' df_dhs_streets_k_pca <- df_dhs_streets_k %>% drop_na()
#' pca_in <- df_dhs_streets_k_pca %>% select_at(all_of(c(indicator_list))) 
#' sapply(pca_in, function(X) sum(is.na(X)))
#' 
#' pca_out <- prcomp(pca_in, center = TRUE, scale. = TRUE)
#' screeplot(pca_out, npcs = 70, type = "lines")
#' df_dhs_streets_k_pca <- merge(df_dhs_streets_k_pca, as.data.frame(pca_out[["x"]]), by = 0, all = TRUE) %>% select(-one_of(c('Row.names'))) 
#' 
#' (cor_streets_k_pc1 <- rcorr(df_dhs_streets_k_pca %>% 
#'         select(street_density_ratio_km_to_km2, k_complexity, PC1) %>% as.matrix(),
#'       type=c('spearman')) %>% tidy() %>% filter(column1 == 'PC1'))
#' 
#' 
#' # Regressions using street density and K as a function of DHS PC1
#' pca_reg_data <- df_dhs_streets_k_pca %>% 
#'   select(SurveyId, REG_ID, ISO3_CountryCode, PC1, street_density_ratio_km_to_km2, k_complexity,
#'          region_urban_share, region_nonurban_share, share_building_count_under_31m2, building_to_block_area_ratio, landscan_population_un_density_hectare) %>%
#'   mutate(country = paste0(tolower(gsub("\\s+|\\.|'|\\/", "_", ISO3_CountryCode))))
#' 
#' 
#' pca_reg_data <-  pca_reg_data %>%
#'   cbind(., scale( pca_reg_data %>% 
#'                     select(all_of(c('street_density_ratio_km_to_km2', 'k_complexity',
#'                                     'region_urban_share', 'region_nonurban_share','share_building_count_under_31m2', 'building_to_block_area_ratio', 'landscan_population_un_density_hectare'))), center = TRUE, scale = TRUE) %>% as.data.frame() %>% 
#'           rename_with(., .fn = ~ paste0("norm_", .x),
#'                       .cols = everything())) %>%
#'   mutate(region_urban_dummy = ifelse(region_urban_share > .7, 1, 0))
#' 
#' pca_country_dummies <- pca_reg_data %>% 
#'   select(country) %>% 
#'   distinct() %>% 
#'   mutate(keep_countries = row_number()) 
#' 
#' country_fixed_effects <- pca_country_dummies %>% filter(keep_countries > 1) %>% select(country) %>% mutate_at(vars(country), list(as.character)) %>% mutate(country = paste0('country_',country)) %>% pull() 
#' country_dummies_drop <- pca_country_dummies %>% filter(keep_countries == 1) %>% select(country) %>% mutate_at(vars(country), list(as.character)) %>% mutate(country = paste0('country_',country)) %>% pull() 
#' 
#' pca_reg_data <- pca_reg_data %>% 
#'   recipe( ~ .) %>% step_dummy(country, one_hot = TRUE)  %>% prep() %>% bake(NULL) %>% select(-one_of(country_dummies_drop)) %>% 
#'   #mutate(dummy = 1) %>% tidyr::spread(country, dummy, fill = 0) %>% select(-one_of(country_dummies_drop)) %>%
#'   distinct() 
#' 
#' lm_model <- linear_reg()
#' # w/o country fixed effects
#' linear_reg_single_imp <- lm_model %>% 
#'   fit(PC1 ~  .,  data = pca_reg_data %>% select_at(all_of(c('PC1', 'norm_k_complexity', 'norm_street_density_ratio_km_to_km2'))))
#' (imp_df_1 <- tidy(linear_reg_single_imp, conf.int = TRUE))
#' 
#' # w country fixed effects
#' linear_reg_countries_imp <-  lm_model %>% 
#'   fit(PC1 ~ ., data = pca_reg_data %>% select_at(all_of(c('PC1', 'norm_k_complexity', 'norm_street_density_ratio_km_to_km2', country_fixed_effects))))
#' (imp_df_2 <-tidy(linear_reg_countries_imp, conf.int = TRUE))
#' 
#' # w country fixed effects + specifications
#' linear_reg_countries_controls_imp <-  lm_model %>% 
#'   fit(PC1 ~ ., data = pca_reg_data %>% select_at(all_of(c('PC1', 'norm_k_complexity', 'norm_street_density_ratio_km_to_km2', country_fixed_effects,
#'                                                           #'region_urban_dummy', # 'norm_region_nonurban_share', 
#'                                                           'norm_share_building_count_under_31m2', 'norm_building_to_block_area_ratio', 'norm_landscan_population_un_density_hectare'))))
#' (imp_df_3 <-tidy(linear_reg_countries_controls_imp, conf.int = TRUE))
#' imp_df_3 %>% print(n=100)
#' 
#' # Write to xlsx tables
#' dir.create('data/dhs-analysis')
#' dir.create('data/dhs-analysis/data')
#' 
#' write_xlsx(list(
#'   'DHS indicator correlations' = cor_streets_k,
#'   'DHS PC1 correlations' = cor_streets_k_pc1,
#'   'Regression' = imp_df_1,
#'   'Regression country effects' = imp_df_2, 
#'   'Regression country controls' = imp_df_3), 
#'            col_names = TRUE, format_headers = TRUE, path = paste0('data/dhs-analysis/data/street_density_vs_k.xlsx'))

# -------------------------------------------------------------------------

street_validation <- read_csv('data/streets_validation.csv') %>%
  mutate(osm_cia_ratio = osm_total_streets_km / cia_roadways_km,
         osm_ecopia_ratio = osm_total_streets_km / ecopia_ml_roads_km,
         osm_irf_ratio = osm_total_streets_km / irf_all_roads_km,
         osm_vehic_cia_ratio = osm_vehicular_streets_km / cia_roadways_km,
         osm_vehic_ecopia_ratio = osm_vehicular_streets_km / ecopia_ml_roads_km,
         osm_vehic_irf_ratio = osm_vehicular_streets_km / irf_all_roads_km)

names(df_k_country)
street_validation <- street_validation %>%
  left_join(., df_k_country %>%select(country_code, k_complexity_weighted_landscan_un),
            by = c('country_code' = 'country_code'))

rcorr(street_validation %>% select(osm_vehicular_streets_km, osm_total_streets_km, 
                                   cia_roadways_km, irf_all_roads_km, ecopia_ml_roads_km)
      %>% as.matrix(), type=c('spearman')) %>% tidy() %>% 
  filter(column2 %in% c('osm_vehicular_streets_km', 'osm_total_streets_km'),
         !(column1 %in% c('osm_vehicular_streets_km', 'osm_total_streets_km'))) %>%
  arrange(column2, column1) %>%
    rename(osm_var = column2,
           external_source = column1)

rcorr(street_validation %>% select(k_complexity_weighted_landscan_un, osm_cia_ratio, osm_ecopia_ratio, osm_irf_ratio, osm_vehic_cia_ratio, osm_vehic_ecopia_ratio, osm_vehic_irf_ratio)
      %>% as.matrix(), type=c('spearman')) %>% tidy() %>% 
  filter(column2 %in% c('k_complexity_weighted_landscan_un')) %>%
  arrange(column2, column1) 


# -------------------------------------------------------------------------


dhs_list <- subnat_all_wide %>% select(ISO3_CountryCode) %>% st_drop_geometry() %>% distinct() %>% pull()

dhs_region_streets <- data.frame("ISO3_CountryCode" = as.character(),
                                 "REG_ID" = as.character(),
                                 "highway_length_meters" = as.numeric(),
                                 "region_area_m2" = as.numeric()) 

for (i in dhs_list) {
  print(i)
  
  highways <- arrow::open_dataset(paste0('/Users/nm/Downloads/osm-line/parquet/',i,'-linestring.parquet')) %>%
    filter(!is.na(highway)) %>%
    filter(highway %in% c('motorway', 'trunk', 'primary', 'secondary', 'tertiary',  'unclassified', 'residential', 'motorway_link',  'trunk_link',  'road', 'primary_link',  'secondary_link',  'living_street')) %>%
    select(highway, geometry)  %>%
    read_sf_dataset() %>%
    st_set_crs(4326) %>%
    st_union()
  
  regions_i <- subnat_all_wide %>%
    filter(ISO3_CountryCode == i) %>%
    select(ISO3_CountryCode, SVYYEAR, CNTRYNAMEE, DHSREGEN, SVYID, REG_ID, REGCODE, REGNAME)
  
  for (j in unique(regions_i$REG_ID)) {
    print(j)
    regions_j <- regions_i %>%
      filter(REG_ID == j) %>%
      st_transform(3395) %>%
      mutate(region_area_m2 = as.numeric(st_area(.))) %>%
      st_simplify(x = ., preserveTopology = TRUE, dTolerance = units::set_units(500,m)) %>%
      st_make_valid() %>%
      st_transform(4326)
    
    highways_j <- highways %>%
      #sf::st_crop(x = . , y = regions_j) %>%
      #mutate(in_zone = ifelse(sf::st_intersects(., st_bbox(regions_j), sparse = F), "Yes", "No")) %>% 
      #filter(in_zone == 'Yes') 
      st_intersection(., regions_j) %>%
      st_drop_geometry() %>%
      st_transform(3395) %>%
      st_as_sf() %>%
      mutate(highway_length_meters = as.numeric(st_length(.))) %>%
      st_transform(4326) %>%
      st_drop_geometry() %>%
      mutate(ISO3_CountryCode = i,
             REG_ID = j,
             region_area_m2 = regions_j$region_area_m2) %>%
      relocate(ISO3_CountryCode, REG_ID, highway_length_meters, region_area_m2)
    
    dhs_region_streets <- rbind(dhs_region_streets, highways_j)
  }
}

write_csv(dhs_region_streets, '/Users/nm/Desktop/kblock-analysis/data/streets_dhs_regions.csv')



# -------------------------------------------------------------------------




# Validation check --------------------------------------------------------

# rsync -avz --exclude '*polygon*' nmarchio@midway.rcc.uchicago.edu:/project2/bettencourt/mnp/update/inputs/osm/parquet /Users/nm/Downloads/osm-line
osm_summary <- read_csv('data/streets_summary.csv')

vehicular_classes <- c('motorway', 'trunk', 'primary', 'secondary', 'tertiary',  'unclassified', 'residential', 'motorway_link',  'trunk_link',  'road', 'primary_link',  'secondary_link',  'living_street')

osm_summary <- osm_summary %>%
  mutate(street_type = case_when( highway %in% vehicular_classes ~ 'Vehicular streets',
         TRUE ~ 'Non-vehicular streets')) %>%
  group_by(street_type , country_code) %>%
  summarize_at(vars(length), list(sum)) %>%
  ungroup()

write_csv(osm_summary, '/Users/nm/Desktop/lengths.csv')



# Ohsome code
# https://gist.github.com/nmarchio/9ca322c404f42baeaa36728ccf725b81
  