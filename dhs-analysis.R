

library(tidyverse)
library(rdhs)
library(scales)
library(sf)
library(rmapshaper)
library(patchwork)
library(ggrepel)
library(arrow)
library(sfarrow)
library(readxl)
library(Hmisc)
library(tidymodels)
library(readr)
library(viridis)
library(ggpubr)
library(broom)
library(betareg)
library(ggpmisc)
library(kableExtra)
library(xtable)
options(scipen = 9999)
gc()

# https://dhsprogram.com/pubs/pdf/DHSG1/Guide_to_DHS_Statistics_DHS-7_v2.pdf

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

dir.create('data/dhs-analysis')
dir.create('data/dhs-analysis/data')
dir.create('data/dhs-analysis/viz')
wd_path = getwd()

load_saved = TRUE

# Setup configuration -----------------------------------------------------

# RData file
if (load_saved == TRUE) {
  # Download from https://drive.google.com/file/d/19LuFTTm-C1fikBc64xTpmf_zrNIcsTOd/view?usp=drive_link
  # Or from https://uchicago.box.com/s/anw4fxc376dgtgol9tt87tuozipnk0kj
  # copy /dhs-source-data into /data folder
  load(paste0(wd_path,"/data/dhs-source-data/dhs_data.RData"))
  subnat_to_blocks <- read_parquet(paste0(wd_path,'/data/dhs-source-data/blocks_to_dhs.parquet'))
} else {
  # Make sure to download the block files (this is done in the complexity-analysis.R script)
  if (!file.exists(paste0("data/africa_data.parquet"))) {
    curl::multi_download("dsbprylw7ncuq.cloudfront.net/AF/africa_data.parquet", "data/africa_data.parquet", resume = TRUE)
  }
  if (!file.exists(paste0("data/africa_geodata.parquet"))) {
    curl::multi_download("dsbprylw7ncuq.cloudfront.net/AF/africa_geodata.parquet", "data/africa_geodata.parquet", resume = TRUE)
  }
}

# Load staging files
# Box.com link with access to staging files:
# https://uchicago.box.com/s/anw4fxc376dgtgol9tt87tuozipnk0kj
# subnat_indicators <- read_csv(paste0(wd_path,'/data/dhs_download.csv'))
# subnat_all <- st_read(paste0(wd_path,'/data/dhs_geographies.geojson'))
# subnat_all_wide <- st_read(paste0(wd_path,'/data/dhs_all_wide.geojson'))

# CHANGE TO DHS USER LOGIN CREDENTIAL
# Follow instructions here: # https://docs.ropensci.org/rdhs/articles/introduction.html
dhs_user_email = 'nmarchio@uchicago.edu'
dhs_project_name = "Identifying neighborhoods and detecting service deficits in sub-Saharan Africa from complete buildings footprint data"

# -------------------------------------------------------------------------

# # UN cached
# if (load_saved == TRUE) {
#   un_slums_k <- read_csv(paste0(wd_path,'/data/un_slums_k.csv'))
#   un_services_cities_k <- read_csv(paste0(wd_path,'/data/un_services_cities_k.csv'))
#   urban_k <- read_csv(paste0(wd_path,'/data/aggregated_urban_k.csv'))
#   city_k <- read_csv(paste0(wd_path,'/data/aggregated_city_k.csv'))
# }
# 
# # DHS cached
# if (load_saved == TRUE) {
#   subnat_all_wide <- st_read(paste0(wd_path,'/data/dhs_all_wide.geojson'))
#   k_data_subnat <- read_csv(paste0(wd_path,'/data/k_data_subnational.csv'))
# }

#names(subnat_all_wide)
#sapply(subnat_all_wide, function(X) sum(is.na(X)))

# Check if indicator exists in DHS and MIS
# fulluni_indicators <- dhs_data(surveyIds = c('SN2020MIS','MR2020DHS'),
#                              breakdown = "subnational")

# Load DHS Survey Data ---------------------------------------------------------

if (load_saved == FALSE) {
  
  # config <- get_rdhs_config()
  set_rdhs_config(email = dhs_user_email, 
                  project = dhs_project_name,
                  cache_path = paste0(wd_path),
                  timeout = 180)
  
  #Country list
  country_list <- c('AGO', 'BEN', 'BWA', 'BFA', 'CMR', 'CPV', 'CAF', 'TCD', 'COG', 'COD', 'CIV', 'GNQ', 'SWZ', 'GAB', 'GMB', 'GHA', 'GIN', 'LSO', 'LBR', 'MLI', 'MRT', 'NAM', 'NER', 'NGA', 'STP', 'SEN', 'SLE', 'ZAF', 'TGO')         
  
  # Indicator list
  indicator_list <- c('AN_NUTS_W_SHT', 'AN_NUTS_W_THN', 'AN_ANEM_W_ANY', 'CH_ARIS_C_ADV', 'CH_DIAR_C_DIA', 'CH_VACC_C_DP1', 'CH_VACC_C_DP2', 'CH_VACC_C_BAS', 'CH_VACC_C_AP2', 'CN_IYCF_C_4FA', 'CN_IYCF_C_MNA', 'CN_NUTS_C_HA3', 'CN_NUTS_C_HA2', 'CN_NUTS_C_WH3', 'CN_NUTS_C_WH2', 'CN_NUTS_C_WA3', 'CN_NUTS_C_WA2', 'CN_ANMC_C_ANY', 'CO_MOBB_W_BNK', 'CO_MOBB_W_MOB', 'ML_ITNA_P_ACC', 'ML_NETP_H_ITN', 'ML_NETP_H_IT2', 'ML_NETC_C_IT1', 'ML_NETU_P_IT1', 'ML_NETW_W_IT1', 
                      'ED_EDAT_B_MYR', 'ED_EDAT_M_MYR', 'ED_EDAT_W_MYR', 'ED_EDUC_M_MYR', 'ED_EDUC_W_MYR', 'ED_EDAT_B_NED', 'ED_EDAT_B_SPR', 'ED_EDAT_B_CPR', 'ED_EDAT_B_SSC', 'ED_EDAT_B_CSC', 'ED_EDAT_B_HGH', 'ED_EDAT_B_PRI', 'ED_EDAT_B_SEC', 'ED_EDAT_M_PRI', 'ED_EDAT_M_SEC', 'ED_EDAT_W_NED', 'ED_EDAT_W_SPR', 'ED_EDAT_W_CPR', 'ED_EDAT_W_SSC', 'ED_EDAT_W_CSC', 'ED_EDAT_W_HGH', 'ED_EDAT_W_PRI', 'ED_EDAT_W_SEC', 'ED_MDIA_W_NWS', 'ED_MDIA_W_TLV', 'ED_MDIA_W_RDO', 'ED_MDIA_W_3MD', 'ED_MDIA_W_N3M', 'ED_EDUC_M_PRI', 'ED_EDUC_M_SEH', 'ED_EDUC_W_PRI', 'ED_EDUC_W_SEH', 'ED_LITR_M_LIT', 'ED_LITR_W_LIT', 'ED_LITY_M_LIT', 'ED_LITY_W_SCH', 'ED_LITY_W_RDW', 'ED_LITY_W_RDP', 'ED_LITY_W_NRD', 'ED_LITY_W_NCD', 'ED_LITY_W_BLD', 'ED_LITY_W_LIT', 'ED_NARP_B_BTH', 'ED_NARP_M_MAL', 'ED_NARP_W_FEM', 'ED_NARS_B_BTH', 'ED_NARS_M_MAL', 'ED_NARS_W_FEM', 'ED_GARP_B_GPI', 'ED_GARS_B_GPI', 'ED_NARP_B_GPI', 'ED_NARS_B_GPI',
                      'EM_EMPL_M_EMC', 'EM_EMPL_M_ENC', 'EM_EMPL_W_EMC', 'EM_EMPL_W_ENC', 'EM_OCCP_M_PRO', 'EM_OCCP_M_CLR', 'EM_OCCP_M_SAL', 'EM_OCCP_M_MNS', 'EM_OCCP_M_MNU', 'EM_OCCP_M_DOM', 'EM_OCCP_M_AGR', 'EM_OCCP_M_OTH', 'EM_OCCP_W_PRO', 'EM_OCCP_W_CLR', 'EM_OCCP_W_SAL', 'EM_OCCP_W_MNS', 'EM_OCCP_W_MNU', 'EM_OCCP_W_DOM', 'EM_OCCP_W_AGR', 'EM_OCCP_W_OTH', 'EM_OCCP_W_TOT', 
                      'HC_PPRM_H_MNP', 'HC_HEFF_H_CMP', 'HC_CKFL_H_SLD', 'HC_CKFL_H_CLN', 'HC_CKFL_P_SLD', 'HC_CKFL_P_CLN', 'HC_ELEC_H_ELC', 'HC_ELEC_H_NEL', 'HC_ELEC_P_ELC', 'HC_ELEC_P_NEL', 'HC_FLRM_H_NAT', 'HC_FLRM_H_ETH', 'HC_FLRM_P_NAT', 'HC_FLRM_P_ETH', 'HC_HEFF_H_RDO', 'HC_HEFF_H_TLV', 'HC_HEFF_H_MPH', 'HC_HEFF_H_NPH', 'HC_HEFF_H_FRG', 'HC_TRNS_H_BIK', 'HC_TRNS_H_SCT', 'HC_TRNS_H_CAR', 'HC_OLDR_H_3GN', 'HC_PPRM_H_12P', 'HC_PPRM_H_34P', 'HC_PPRM_H_56P', 'HC_PPRM_H_7PP', 'HC_CKPL_H_HSE', 'HC_CKPL_P_HSE', 'HC_WIXQ_P_LOW', 'HC_WIXQ_P_2ND', 'HC_WIXQ_P_MID', 'HC_WIXQ_P_4TH', 'HC_WIXQ_P_HGH', 'HC_WIXQ_P_GNI', 
                      'FP_NADM_W_UNT', 'FP_NADM_W_MNT', 'RH_DELP_C_DHF', 'CM_ECMR_C_NNR', 'CM_ECMR_C_PNR', 'CM_ECMR_C_IMR', 'CM_ECMR_C_CMR', 'CM_ECMR_C_U5M',
                      'WS_HNDW_H_OBS', 'WS_HNDW_H_SOP', 'WS_HNDW_H_BAS', 'WS_HNDW_H_LTD', 'WS_HNDW_P_OBS', 'WS_HNDW_P_SOP', 'WS_HNDW_P_BAS', 'WS_HNDW_P_LTD', 'WS_TLET_H_IMP', 'WS_TLET_H_NFC', 'WS_TLET_H_BAS', 'WS_TLET_H_LTD', 'WS_TLET_P_IMP', 'WS_TLET_P_NFC', 'WS_TLET_P_BAS', 'WS_TLET_P_LTD', 'WS_TLOC_H_DWL', 'WS_TLOC_P_DWL', 'WS_SRCE_H_IMP', 'WS_SRCE_H_PIP', 'WS_SRCE_H_NIM', 'WS_SRCE_H_IOP', 'WS_SRCE_H_BAS', 'WS_SRCE_H_LTD', 'WS_SRCE_P_IMP', 'WS_SRCE_P_PIP', 'WS_SRCE_P_NIM', 'WS_SRCE_P_IOP', 'WS_SRCE_P_BAS', 'WS_SRCE_P_LTD', 'WS_TIME_H_ONP', 'WS_TIME_H_L30', 'WS_TIME_H_M30', 'WS_TIME_P_ONP', 'WS_TIME_P_L30', 'WS_TIME_P_M30', 'WS_WTRT_H_APP', 'WS_WTRT_P_APP')
  
  indicator_list <- indicator_list %>% unique()

  # Configure DHS metadata
  meta_tags <- dhs_tags()
  meta_surveychar <- dhs_survey_characteristics()
  meta_surveys <- dhs_surveys()
  
  meta_indicators <- dhs_indicators() %>%
    filter(IndicatorId %in% indicator_list)
  dhs_indicator_list <- meta_indicators %>% select(IndicatorId) %>% distinct() %>% pull()
  
  meta_countryids <- dhs_countries() %>%
    filter(SubregionName %in% c("Middle Africa","Western Africa","Southern Africa","North Africa")) %>%
    filter(UNSTAT_CountryCode %in% country_list)
  dhs_country_list <- meta_countryids %>% select(DHS_CountryCode) %>% distinct() %>% pull()
  
  meta_data <- dhs_datasets() %>%
    filter(DHS_CountryCode %in% dhs_country_list,
           SurveyYear >= 2010,
           FileFormat == 'Flat ASCII data (.dat)',
           FileType %in% c("Geospatial Covariates","Geographic Data", "Wealth Index",
                           "Household Member Recode", "Household Recode"))
  
  meta_data_subset <- meta_data %>%
    select(any_of(c("SurveyNum","SurveyId","FileType","SurveyYear","DHS_CountryCode","CountryName"))) %>%
    mutate(flag = 1) %>%
    pivot_wider(.,names_from = FileType, values_from = flag) %>%
    filter(!is.na(`Household Member Recode`) & !is.na(`Geographic Data`) & !is.na(`Household Recode`)) %>%
    group_by(DHS_CountryCode) %>%
    mutate(rank = row_number(desc(SurveyYear))) %>%
    ungroup() %>% filter(rank <= 3)
  
  survey_list <- unique(meta_data_subset$SurveyId)
  survey_list <- setdiff(survey_list, c("SL2016MIS","AO2011MIS")) # surveys without GIS information
  
  # Download DHS Survey Data
  if (!file.exists(paste0(wd_path,'/data/dhs_download.csv'))) {

    # subnat_indicators <- dhs_data(surveyIds = survey_list,
    #                                 indicatorIds = dhs_indicator_list,
    #                                 breakdown = "subnational")
    
    dhs_indicator_chunk <- base::split(dhs_indicator_list, ceiling(seq_along(dhs_indicator_list)/ceiling(length(dhs_indicator_list)/2)))
    subnat_indicators_1 <- dhs_data(surveyIds = survey_list,
                                    indicatorIds = dhs_indicator_chunk$`1`,
                                    breakdown = "subnational")
    subnat_indicators_2 <- dhs_data(surveyIds = survey_list,
                                    indicatorIds = dhs_indicator_chunk$`2`,
                                    breakdown = "subnational")
    subnat_indicators <- rbind(subnat_indicators_1, subnat_indicators_2)
    subnat_indicators %>% group_by(IndicatorId) %>% tally() %>% print(n=200)
    
    write_csv(subnat_indicators, paste0(wd_path,'/data/dhs_download.csv'))
    
    # all_indicators <- dhs_data(surveyIds = survey_list, 
    #                            indicatorIds = dhs_indicator_list,
    #                            breakdown = "all",
    #                            returnGeometry=TRUE,
    #                            f = 'geoJSON')
    #write_csv(subnat_indicators, paste0(wd_path,'/data/dhs_download_micro.csv'))
    
  } else {
    subnat_indicators <- read_csv(paste0(wd_path,'/data/dhs_download.csv'))
  }
  
  # Pivot DHS data wide
  subnat_wide <- subnat_indicators %>% 
    filter(IsPreferred == 1) %>%
    #select(-any_of(c('Indicator','DenominatorUnweighted', 'DenominatorWeighted'))) 
    select(any_of(c('IndicatorId','Value','DenominatorWeighted','DenominatorUnweighted','SurveyId', 'RegionId', 'SurveyYearLabel', 'SurveyType', 'SurveyYear', 'DHS_CountryCode', 'CountryName'))) %>%
    distinct() %>%  # drops dups in  'GN2012DHS', 'SN2020MIS'
    pivot_wider(.,
                id_cols = c('SurveyId', 'RegionId', 'SurveyYearLabel', 'SurveyType', 'SurveyYear', 'DHS_CountryCode', 'CountryName'),
                names_from = c('IndicatorId'), 
                values_from = c('Value','DenominatorWeighted','DenominatorUnweighted')) %>%
    rename_at(vars(starts_with('Value_')), ~ str_replace(.x,"Value_","")) %>% 
    rename_at(vars(starts_with('DenominatorWeighted_')), ~ str_replace(.x,"DenominatorWeighted_","dw_")) %>% 
    rename_at(vars(starts_with('DenominatorUnweighted_')), ~ str_replace(.x,"DenominatorUnweighted_","du_"))
    
  # sapply(subnat_wide, function(X) sum(is.na(X)))
  
  # if (exists('subnat_all') && is.data.frame(get('subnat_all'))) {
  #   survey_list <- setdiff(unique(meta_data_subset$SurveyId),unique(subnat_all$SurveyId)) 
  #   survey_list <- setdiff(survey_list, c("SL2016MIS","AO2011MIS"))
  # } else {
  #   survey_list <- unique(meta_data_subset$SurveyId)
  #   survey_list <- setdiff(survey_list, c("SL2016MIS","AO2011MIS"))
  # }
  
  # Download DHS region boundaries
  if (!file.exists(paste0(wd_path,'/data/dhs_geographies.geojson'))) {
    # Download boundaries
    for (i in survey_list) {
      print(i)
      geo <- download_boundaries(surveyId = i, method = "sf", server_sleep = 10)
      geo <- geo$sdr_subnational_boundaries
      print(names(geo))
      geo <- geo %>%
        ms_simplify(., keep = 0.05, keep_shapes = TRUE) %>%
        mutate(SurveyId = i)
      if (exists('subnat_all') && is.data.frame(get('subnat_all'))) {
        subnat_all <- rbind(subnat_all, geo)
      } else { 
        subnat_all <- geo 
      } 
    }
    st_write(subnat_all, paste0(wd_path,'/data/dhs_geographies.geojson'), delete_dsn = TRUE)
  } else {
    subnat_all <- st_read(paste0(wd_path,'/data/dhs_geographies.geojson'))
  }
  
  # Throw an error if DHS geographies are missing for a survey in survey_list
  stopifnot(setequal(survey_list, subnat_all %>% st_drop_geometry() %>% select(SurveyId) %>% unique() %>% pull()))
  
  # Join DHS data and DHS region boundaries
  if (!file.exists(paste0(wd_path,'/data/dhs_all_wide.geojson'))) {
    # subnat_all_wide <- st_read(paste0(wd_path,'/data/dhs_all_wide.geojson'))
    subnat_all_wide <- subnat_all %>%
      left_join(., subnat_wide, by = c('SurveyId'='SurveyId','REG_ID'='RegionId')) %>%
      filter(!is.na(DHS_CountryCode)) %>%
      select(-any_of(c('REGNOTES', 'CNTRYNAMEF', 'CNTRYNAMES', 'DHSREGFR', 'DHSREGSP'))) %>%
      left_join(., meta_countryids %>% select(ISO2_CountryCode, ISO3_CountryCode) %>% distinct(),
                by = c('ISO' = 'ISO2_CountryCode')) #%>%
      # mutate(HC_WIXQ_P_LOW_2ND = HC_WIXQ_P_LOW + HC_WIXQ_P_2ND,
      #        HC_WIXQ_P_LOW_4TH = HC_WIXQ_P_LOW + HC_WIXQ_P_2ND + HC_WIXQ_P_MID +  HC_WIXQ_P_4TH,
      #        HC_PPRM_H_37P = HC_PPRM_H_34P + HC_PPRM_H_56P + HC_PPRM_H_7PP,
      #        ED_EDAT_B_NED_PR =	ED_EDAT_B_NED	+ ED_EDAT_B_SPR +	ED_EDAT_B_CPR,
      #        ED_EDAT_B_SC	=	ED_EDAT_B_SSC	+ ED_EDAT_B_CSC)
    
    sapply(subnat_all_wide , function(X) sum(is.na(X)))
    st_write(subnat_all_wide, paste0(wd_path,'/data/dhs_all_wide.geojson'), delete_dsn = TRUE)
  } else {
    subnat_all_wide <- st_read(paste0(wd_path,'/data/dhs_all_wide.geojson'))
  }
  
  # Construct DHS region to MNP block crosswalk
  if (!file.exists(paste0(wd_path,'/data/blocks_to_dhs.parquet'))) {
    sf_use_s2(FALSE) 
    subnat_to_blocks <- purrr::map_dfr(.x = unique(subnat_all_wide$SurveyId), .f = function(i) {
      print(i)
      subnat_surveyid <- subnat_all_wide %>% filter(SurveyId == i) %>%
        st_cast("POLYGON") %>% st_make_valid() %>%
        select(ISO3_CountryCode,DHS_CountryCode,SurveyId,SurveyYear,REGCODE,REG_ID,REGNAME,CNTRYNAMEE,DHSREGEN)
      country_iso <- subnat_surveyid %>% st_drop_geometry() %>% select(ISO3_CountryCode) %>% distinct() %>% pull()
      
      # iso_blocks <- st_read_parquet(paste0('/Users/nm/Downloads/blocks/blocks_',country_iso,'.parquet')) %>% 
      #   st_make_valid() %>% mutate(is_valid = st_is_valid(geometry)) %>%
      #   filter(is_valid == TRUE) %>% select(-any_of('is_valid'))
      
      iso_blocks <- arrow::open_dataset(paste0('data/africa_geodata.parquet')) %>% 
        select(block_id, gadm_code, country_code, geometry) %>%
        filter(country_code %in% c(country_iso)) %>% 
        read_sf_dataset() %>%
        st_set_crs(4326) %>%
        st_make_valid() # %>% mutate(is_valid = st_is_valid(geometry)) %>%
        #filter(is_valid == TRUE) %>% select(-any_of('is_valid'))
      
      print('within join')
      iso_blocks_within <- iso_blocks %>%
        st_join(., subnat_surveyid, join = st_within, left = FALSE)
      print('intersect join')
      iso_blocks_intersect <- iso_blocks %>%
        filter(!(block_id %in% unique(iso_blocks_within$block_id))) %>%
        st_join(., subnat_surveyid, largest = TRUE, left = FALSE)
      iso_blocks_xwalk <- rbind(iso_blocks_within %>% st_drop_geometry(),
                                iso_blocks_intersect %>% st_drop_geometry())
    })
    write_parquet(subnat_to_blocks, paste0(wd_path,'/data/blocks_to_dhs.parquet'))
    #write_csv(subnat_to_blocks, paste0(wd_path,'/data/blocks_to_dhs.csv'))
  } else {
    subnat_to_blocks <- read_parquet(paste0(wd_path,'/data/blocks_to_dhs.parquet'))
    #subnat_to_blocks <- read_csv(paste0(wd_path,'/data/blocks_to_dhs.csv'))
  }

  # Aggregate MNP data to DHS regions using subnat_to_blocks crosswalk

  if (!file.exists(paste0(wd_path,'/data/k_data_subnational.csv'))) {
    k_data <- read_parquet(paste0('data/africa_data.parquet')) %>%
      mutate(region_core_urban = case_when(class_urban_hierarchy == "1 - Core urban" ~ landscan_population_un, TRUE ~ as.numeric(0)),
             region_peripheral_urban = case_when(class_urban_hierarchy == "2 - Peripheral urban" ~ landscan_population_un, TRUE ~ as.numeric(0)),
             region_peri_urban = case_when(class_urban_hierarchy== "3 - Peri-urban" ~ landscan_population_un, TRUE ~ as.numeric(0)),
             region_non_urban = case_when(class_urban_hierarchy == "4 - Non-urban" ~ landscan_population_un, TRUE ~ as.numeric(0)),
             population_k_1 = case_when(k_labels == '1' ~ landscan_population_un, TRUE ~ as.numeric(0)),
             population_k_2 = case_when(k_labels == '2' ~ landscan_population_un, TRUE ~ as.numeric(0)),
             population_k_3 = case_when(k_labels == '3' ~ landscan_population_un, TRUE ~ as.numeric(0)),
             population_k_4 = case_when(k_labels == '4' ~ landscan_population_un, TRUE ~ as.numeric(0)),
             population_k_5 = case_when(k_labels == '5' ~ landscan_population_un, TRUE ~ as.numeric(0)),
             population_k_6 = case_when(k_labels == '6' ~ landscan_population_un, TRUE ~ as.numeric(0)),
             population_k_7 = case_when(k_labels == '7' ~ landscan_population_un, TRUE ~ as.numeric(0)),
             population_k_8 = case_when(k_labels == '8' ~ landscan_population_un, TRUE ~ as.numeric(0)),
             population_k_9 = case_when(k_labels == '9' ~ landscan_population_un, TRUE ~ as.numeric(0)),
             population_k_10plus = case_when(k_labels == '10+' ~ landscan_population_un, TRUE ~ as.numeric(0)),
             population_off_network = case_when(k_labels == 'Off-network' ~ landscan_population_un, TRUE ~ as.numeric(0)))
    
    agg_list <- c("region_core_urban", "region_peripheral_urban", "region_peri_urban", "region_non_urban", "k_complexity_average", "block_area_m2", "block_hectares", "block_area_km2", "block_perimeter_meters", "building_area_m2", "building_count", "parcel_count", "k_complexity", "landscan_population_un", 
                  "population_k_1", "population_k_2", "population_k_3", "population_k_4", "population_k_5", "population_k_6", "population_k_7", "population_k_8", "population_k_9", "population_k_10plus", "population_off_network", 
                  "bldg_area_count_bin_01_0.50__log10_3.2", "bldg_area_count_bin_02_0.75__log10_5.6", "bldg_area_count_bin_03_1.00__log10_10", "bldg_area_count_bin_04_1.25__log10_17.8", "bldg_area_count_bin_05_1.50__log10_31.6", "bldg_area_count_bin_06_1.75__log10_56.2", "bldg_area_count_bin_07_2.00__log10_100", "bldg_area_count_bin_08_2.25__log10_177.8", "bldg_area_count_bin_09_2.50__log10_316.2", "bldg_area_count_bin_10_2.75__log10_562.3", "bldg_area_count_bin_11_3.00__log10_1000", "bldg_area_count_bin_12_3.25__log10_1778.3", "bldg_area_count_bin_13_3.50__log10_3162.3", "bldg_area_count_bin_14_3.75__log10_5623.4", "bldg_area_count_bin_15_4.00__log10_10000", "bldg_area_m2_bin_01_0.50__log10_3.2", "bldg_area_m2_bin_02_0.75__log10_5.6", "bldg_area_m2_bin_03_1.00__log10_10", "bldg_area_m2_bin_04_1.25__log10_17.8", "bldg_area_m2_bin_05_1.50__log10_31.6", "bldg_area_m2_bin_06_1.75__log10_56.2", "bldg_area_m2_bin_07_2.00__log10_100", "bldg_area_m2_bin_08_2.25__log10_177.8", "bldg_area_m2_bin_09_2.50__log10_316.2", "bldg_area_m2_bin_10_2.75__log10_562.3", "bldg_area_m2_bin_11_3.00__log10_1000", "bldg_area_m2_bin_12_3.25__log10_1778.3", "bldg_area_m2_bin_13_3.50__log10_3162.3", "bldg_area_m2_bin_14_3.75__log10_5623.4", "bldg_area_m2_bin_15_4.00__log10_10000")
    
    k_data_subnat <- subnat_to_blocks %>%
      left_join(., k_data, by = c('block_id'='block_id'))  %>%
      mutate(k_complexity_average = k_complexity*landscan_population_un) %>%
      group_by(REG_ID, DHS_CountryCode, SurveyId) %>%
      summarize_at(vars(all_of(agg_list)), list(sum)) %>%
      ungroup() %>%
      mutate(k_complexity_average = k_complexity_average/landscan_population_un)
    rm(k_data)
    gc()
    write_csv(k_data_subnat, paste0(wd_path,'/data/k_data_subnational.csv')) 
  } else {
    k_data_subnat <- read_csv(paste0(wd_path,'/data/k_data_subnational.csv'))
  } 
}

# Load UN Data Sources ---------------------------------------------------------

if (load_saved == FALSE) {

  # Urban Population Living in Slums by Country or Area 2000-2020
  un_slums <- read_csv(paste0('data/un-habitat/urban_population_in_slums_2020.csv')) %>%
    filter(!is.na(country_code)) %>%
    mutate(percent_2020_coalesced = coalesce(percent_2020, percent_2018, percent_2016, percent_2014),
           percent_2020_coalesced = percent_2020_coalesced/100) %>%
    select(country_code, percent_2020_coalesced)
  
  # Population with Improved Water, Improved Sanitation and Other Urban Basic Services in Cities, Selected Countries (Percent)
  # https://data.unhabitat.org/pages/access-to-basic-services-in-cities-and-urban-areas
  un_services_cities <- read_xlsx(path = paste0('data/un-habitat/population_with_services_city.xlsx'), sheet = 'data')%>%
    select_all(~gsub("\\s+|\\.|\\/|,|\\*|-", "_", .)) %>%
    rename_all(list(tolower)) %>% filter(m49class %in% c('Sub-Saharan Africa','Western Asia and Northern Africa')) %>%
    group_by(country, city_region) %>%
    mutate(most_recent = row_number(desc(year))) %>%
    ungroup() %>%
    filter(most_recent == 1) %>%
    mutate(city_region_id = paste0(tolower(str_replace_all(iconv(country, from = 'UTF-8', to = 'ASCII//TRANSLIT'), "[[:punct:]]|\\s+", "_")),'_::_',  
                                   tolower(str_replace_all(iconv(city_region, from = 'UTF-8', to = 'ASCII//TRANSLIT'), "[[:punct:]]|\\s+", "_")))) %>%
    mutate(match_code = case_when( city_region_id == 'angola_::_luanda' ~ 'ghsl_3050', city_region_id == 'benin_::_cotonou' ~ 'ghsl_2078', 
                                   city_region_id == 'benin_::_djouguo' ~ 'ghsl_2354', city_region_id == 'benin_::_porto_novo' ~ 'ghsl_2101', 
                                   city_region_id == 'botswana_::_francistown' ~ 'ghsl_3706', city_region_id == 'botswana_::_gaborone' ~ 'ghsl_3587', 
                                   city_region_id == 'burkina_faso_::_ouagadougou' ~ 'ghsl_1799', city_region_id == 'burundi_::_bujumbura' ~ 'ghsl_4078', 
                                   city_region_id == 'cameroon_::_bafoussam' ~ 'ghsl_2901', city_region_id == 'cameroon_::_douala' ~ 'ghsl_2850', 
                                   city_region_id == 'cameroon_::_garoua' ~ 'ghsl_3071', city_region_id == 'cameroon_::_yaound_e' ~ 'ghsl_2961', 
                                   city_region_id == 'central_african_republic_::_bangui' ~ 'ghsl_3363', city_region_id == 'chad_::_n_djam_ena' ~ 'ghsl_3186', 
                                   city_region_id == 'comoros_::_moroni' ~ 'ghsl_5588',
                                   #city_region_id == 'congo_::_brazaville' ~ 'ghsl_3211', city_region_id == 'gabon_::_libreville' ~ 'ghsl_2834', 
                                   city_region_id == 'congo_::_brazzaville' ~ 'ghsl_3211', city_region_id == 'gabon_::_libreville_port_gentil' ~ 'ghsl_2834', 
                                   city_region_id == 'congo_drc_::_kinshasa' ~ 'ghsl_3209', city_region_id == 'cote_d_ivoire_::_abidjan' ~ 'ghsl_1675', 
                                   city_region_id == 'ethiopia_::_addis_ababa' ~ 'ghsl_5134', city_region_id == 'ethiopia_::_awasa' ~ 'ghsl_5126', 
                                   city_region_id == 'ethiopia_::_gonder' ~ 'ghsl_4847', city_region_id == 'ethiopia_::_mekele' ~ 'ghsl_5210', city_region_id == 'ethiopia_::_nazret' ~ 'ghsl_5246', 
                                   city_region_id == 'gambia_::_banjul' ~ 'ghsl_1455', city_region_id == 'ghana_::_accra' ~ 'ghsl_1910',
                                   city_region_id == 'ghana_::_kumasi' ~ 'ghsl_1785', city_region_id == 'ghana_::_takoradi' ~ 'ghsl_1779', 
                                   city_region_id == 'guinea_::_bok_e' ~ 'ghsl_1497', city_region_id == 'guinea_::_conakry' ~ 'ghsl_1502', 
                                   city_region_id == 'guinea_::_faranah' ~ 'ghsl_1527', city_region_id == 'guinea_::_kankan' ~ 'ghsl_1539', 
                                   city_region_id == 'guinea_::_kindia' ~ 'ghsl_1512', city_region_id == 'guinea_::_lab_e' ~ 'ghsl_1515', 
                                   city_region_id == 'guinea_::_mamou' ~ 'ghsl_1518', city_region_id == 'guinea_::_n_z_er_ekor_e' ~ 'ghsl_1545', 
                                   city_region_id == 'guinea_bissau_::_bafat_a' ~ 'ghsl_1492', city_region_id == 'guinea_bissau_::_gab_u' ~ 'ghsl_1498', 
                                   city_region_id == 'guinea_bissau_::_sab' ~ 'ghsl_1477', city_region_id == 'kenya_::_kisumu' ~ 'ghsl_4608', 
                                   city_region_id == 'kenya_::_mombasa' ~ 'ghsl_5329', city_region_id == 'kenya_::_nairobi' ~ 'ghsl_4808', 
                                   city_region_id == 'kenya_::_nakuru' ~ 'ghsl_4729', city_region_id == 'lesotho_::_maseru' ~ 'ghsl_3631', 
                                   city_region_id == 'liberia_::_monrovia' ~ 'ghsl_1526', city_region_id == 'madagascar_::_antananarivo' ~ 'ghsl_5792', 
                                   city_region_id == 'malawi_::_blantyre' ~ 'ghsl_4558', city_region_id == 'malawi_::_lilongwe' ~ 'ghsl_4495', 
                                   city_region_id == 'malawi_::_mzuzu_city' ~ 'ghsl_4526', city_region_id == 'malawi_::_zomba_city' ~ 'ghsl_4589', 
                                   city_region_id == 'mali_::_bamako' ~ 'ghsl_1553', city_region_id == 'mauritania_::_nouakchott' ~ 'ghsl_1474', 
                                   city_region_id == 'mozambique_::_maputo' ~ 'ghsl_4220', city_region_id == 'namibia_::_windhoek' ~ 'ghsl_3260', 
                                   city_region_id == 'niger_::_niamey' ~ 'ghsl_2067', city_region_id == 'nigeria_::_abuja' ~ 'ghsl_2565', 
                                   city_region_id == 'nigeria_::_akure' ~ 'ghsl_2312', city_region_id == 'nigeria_::_damaturu' ~ 'ghsl_2981', 
                                   city_region_id == 'nigeria_::_effon_alaiye' ~ 'ghsl_2284', city_region_id == 'nigeria_::_ibadan' ~ 'ghsl_2189', 
                                   city_region_id == 'nigeria_::_kano' ~ 'ghsl_2717', city_region_id == 'nigeria_::_lagos' ~ 'ghsl_2125', 
                                   city_region_id == 'nigeria_::_zaria' ~ 'ghsl_2625', city_region_id == 'rwanda_::_kigali' ~ 'ghsl_4172', 
                                   city_region_id == 's~ao_tom_e_and_pr_incipe_::_s~ao_tom_e' ~ 'ghsl_2485', city_region_id == 'senegal_::_dakar' ~ 'ghsl_1452', 
                                   city_region_id == 'sierra_leone_::_freetown' ~ 'ghsl_1507', city_region_id == 'south_africa_::_capetown' ~ 'ghsl_3268', 
                                   city_region_id == 'south_africa_::_durban' ~ 'ghsl_3868', city_region_id == 'south_africa_::_port_elizabeth' ~ 'ghsl_3505', 
                                   city_region_id == 'south_africa_::_pretoria' ~ 'ghsl_3698', city_region_id == 'south_africa_::_west_rand' ~ 'ghsl_3673', 
                                   city_region_id == 'eswatini_::_manzini' ~ 'ghsl_4080', city_region_id == 'tanzania_::_arusha' ~ 'ghsl_4800', 
                                   city_region_id == 'tanzania_::_dar_es_salaam' ~ 'ghsl_5222', city_region_id == 'togo_::_lom_e' ~ 'ghsl_2020', 
                                   city_region_id == 'uganda_::_kampala' ~ 'ghsl_4427', city_region_id == 'zambia_::_chingola' ~ 'ghsl_3786', 
                                   city_region_id == 'zambia_::_lusaka' ~ 'ghsl_3798', city_region_id == 'zambia_::_ndola' ~ 'ghsl_3859', 
                                   city_region_id == 'zimbabwe_::_bulawayo' ~ 'ghsl_3777', city_region_id == 'zimbabwe_::_harare' ~ 'ghsl_4171',
                                   TRUE ~ as.character(''))) %>%
    filter(match_code != '')
  
  agg_list <- c("k_complexity_average", "block_area_m2", "block_hectares", "block_area_km2", "block_perimeter_meters", "building_area_m2", "building_count", "parcel_count", "k_complexity", "landscan_population_un", "population_k_1", "population_k_2", "population_k_3", "population_k_4", "population_k_5", "population_k_6", "population_k_7", "population_k_8", "population_k_9", "population_k_10plus", "population_off_network", "bldg_area_count_bin_01_0.50__log10_3.2", "bldg_area_count_bin_02_0.75__log10_5.6", "bldg_area_count_bin_03_1.00__log10_10", "bldg_area_count_bin_04_1.25__log10_17.8", "bldg_area_count_bin_05_1.50__log10_31.6", "bldg_area_count_bin_06_1.75__log10_56.2", "bldg_area_count_bin_07_2.00__log10_100", "bldg_area_count_bin_08_2.25__log10_177.8", "bldg_area_count_bin_09_2.50__log10_316.2", "bldg_area_count_bin_10_2.75__log10_562.3", "bldg_area_count_bin_11_3.00__log10_1000", "bldg_area_count_bin_12_3.25__log10_1778.3", "bldg_area_count_bin_13_3.50__log10_3162.3", "bldg_area_count_bin_14_3.75__log10_5623.4", "bldg_area_count_bin_15_4.00__log10_10000", "bldg_area_m2_bin_01_0.50__log10_3.2", "bldg_area_m2_bin_02_0.75__log10_5.6", "bldg_area_m2_bin_03_1.00__log10_10", "bldg_area_m2_bin_04_1.25__log10_17.8", "bldg_area_m2_bin_05_1.50__log10_31.6", "bldg_area_m2_bin_06_1.75__log10_56.2", "bldg_area_m2_bin_07_2.00__log10_100", "bldg_area_m2_bin_08_2.25__log10_177.8", "bldg_area_m2_bin_09_2.50__log10_316.2", "bldg_area_m2_bin_10_2.75__log10_562.3", "bldg_area_m2_bin_11_3.00__log10_1000", "bldg_area_m2_bin_12_3.25__log10_1778.3", "bldg_area_m2_bin_13_3.50__log10_3162.3", "bldg_area_m2_bin_14_3.75__log10_5623.4", "bldg_area_m2_bin_15_4.00__log10_10000")
  urban_k <- read_parquet(paste0(wd_input,'/africa_data.parquet')) %>%
    filter(class_urban_hierarchy %in% c("1 - Core urban", "2 - Peripheral urban", "3 - Peri-urban")) %>% 
    mutate(population_k_1 = case_when(k_labels == '1' ~ landscan_population_un, TRUE ~ as.numeric(0)),
           population_k_2 = case_when(k_labels == '2' ~ landscan_population_un, TRUE ~ as.numeric(0)),
           population_k_3 = case_when(k_labels == '3' ~ landscan_population_un, TRUE ~ as.numeric(0)),
           population_k_4 = case_when(k_labels == '4' ~ landscan_population_un, TRUE ~ as.numeric(0)),
           population_k_5 = case_when(k_labels == '5' ~ landscan_population_un, TRUE ~ as.numeric(0)),
           population_k_6 = case_when(k_labels == '6' ~ landscan_population_un, TRUE ~ as.numeric(0)),
           population_k_7 = case_when(k_labels == '7' ~ landscan_population_un, TRUE ~ as.numeric(0)),
           population_k_8 = case_when(k_labels == '8' ~ landscan_population_un, TRUE ~ as.numeric(0)),
           population_k_9 = case_when(k_labels == '9' ~ landscan_population_un, TRUE ~ as.numeric(0)),
           population_k_10plus = case_when(k_labels == '10+' ~ landscan_population_un, TRUE ~ as.numeric(0)),
           population_off_network = case_when(k_labels == 'Off-network' ~ landscan_population_un, TRUE ~ as.numeric(0))) %>%             
    mutate(k_complexity_average = k_complexity*landscan_population_un) %>%
    group_by(country_code) %>%
    summarize_at(vars(all_of(agg_list)), list(sum)) %>% # , na.rm = TRUE
    ungroup() %>%
    mutate(k_complexity_average = k_complexity_average/landscan_population_un) 
  
  un_slums_k <- un_slums %>%
    left_join(., urban_k, by = c('country_code'='country_code'))
  
  city_k <- read_parquet(paste0('data/africa_data.parquet')) %>%
    mutate(population_k_1 = case_when(k_labels == '1' ~ landscan_population_un, TRUE ~ as.numeric(0)),
           population_k_2 = case_when(k_labels == '2' ~ landscan_population_un, TRUE ~ as.numeric(0)),
           population_k_3 = case_when(k_labels == '3' ~ landscan_population_un, TRUE ~ as.numeric(0)),
           population_k_4 = case_when(k_labels == '4' ~ landscan_population_un, TRUE ~ as.numeric(0)),
           population_k_5 = case_when(k_labels == '5' ~ landscan_population_un, TRUE ~ as.numeric(0)),
           population_k_6 = case_when(k_labels == '6' ~ landscan_population_un, TRUE ~ as.numeric(0)),
           population_k_7 = case_when(k_labels == '7' ~ landscan_population_un, TRUE ~ as.numeric(0)),
           population_k_8 = case_when(k_labels == '8' ~ landscan_population_un, TRUE ~ as.numeric(0)),
           population_k_9 = case_when(k_labels == '9' ~ landscan_population_un, TRUE ~ as.numeric(0)),
           population_k_10plus = case_when(k_labels == '10+' ~ landscan_population_un, TRUE ~ as.numeric(0)),
           population_off_network = case_when(k_labels == 'Off-network' ~ landscan_population_un, TRUE ~ as.numeric(0))) %>%      
    mutate(k_complexity_average = k_complexity*landscan_population_un)  %>%
    group_by(urban_id, urban_center_name, urban_country_name) %>%
    summarize_at(vars(all_of(agg_list)), list(sum), na.rm = TRUE) %>%
    ungroup() %>%
    mutate(k_complexity_average = k_complexity_average/landscan_population_un) 
  
  un_services_cities_k <- city_k %>%
    inner_join(., un_services_cities, 
               by = c('urban_id'='match_code')) %>%
    filter(!is.nan(k_complexity_average)) %>%
    #select(urban_id, urban_center_name, country_name, region, m49class, country, city_region, city_region_id, year, number_of_cases, total_improved_water , access_to_basic_drinking_water_services , total_improved_sanitation , access_to_basic_sanitation_services , basic_hand_washing_facility , durable_housing_durable_floor_material , durable_housing_durable_wall_material , durable_housing_durable_roof_material , sufficient_living_area , other_basic_services_telephone , other_basic_services_mobile_phone , other_basic_services_electricity , type_of_main_cooking_fuel_clean_fuel_total_clean_fuel , solid_fuel , source, most_recent, k_complexity_average, landscan_population_un, population_k_1, population_k_2, population_k_3, population_k_4, population_k_5, population_k_6, population_k_7, population_k_8, population_k_9, population_k_10plus, population_off_network) %>%
    #drop_na(total_improved_water, total_improved_sanitation, access_to_basic_drinking_water_services, access_to_basic_sanitation_services, durable_housing_durable_floor_material, durable_housing_durable_wall_material, sufficient_living_area, other_basic_services_electricity, type_of_main_cooking_fuel_clean_fuel_total_clean_fuel, solid_fuel) %>%
    #drop_na(improved_water_sources_piped_water_sources_piped_water, improved_water_sources_piped_water_sources_public_tap, improved_water_sources_piped_water_sources_total_piped, improved_water_sources_non_piped_water_sources_borehole_tubewell, improved_water_sources_non_piped_water_sources_protected_well, improved_water_sources_non_piped_water_sources_protected_spring, improved_water_sources_non_piped_water_sources_rainwater, improved_water_sources_non_piped_water_sources_delivered_water, improved_water_sources_non_piped_water_sources_packaged_water, improved_water_sources_non_piped_water_sources_total_non_piped, total_improved_water, access_to_basic_drinking_water_services, improved_sanitation_facilities_sewered_facilities_connection_to_sewerage, improved_sanitation_facilities_non_sewered_facilities_vip_toilet_facility, improved_sanitation_facilities_non_sewered_facilities_flush_septic, improved_sanitation_facilities_non_sewered_facilities_flush_to_pit_latrine, improved_sanitation_facilities_non_sewered_facilities_pit_latrine_with_slab, improved_sanitation_facilities_non_sewered_facilities_total_non_sewered, total_improved_sanitation, access_to_basic_sanitation_services, durable_housing_durable_floor_material, durable_housing_durable_wall_material, durable_housing_durable_roof_material, sufficient_living_area, other_basic_services_mobile_phone, other_basic_services_electricity, type_of_main_cooking_fuel_clean_fuel_electricity, type_of_main_cooking_fuel_clean_fuel_lpg_natural_gas, type_of_main_cooking_fuel_clean_fuel_total_clean_fuel, solid_fuel) %>%
    filter(#landscan_population_un >= 200000,
      year >= 2010)
  
  write_csv(urban_k, paste0(wd_path,'/data/aggregated_urban_k.csv'))
  write_csv(un_slums_k, paste0(wd_path,'/data/un_slums_k.csv'))
  
  write_csv(city_k, paste0(wd_path,'/data/aggregated_city_k.csv'))
  write_csv(un_services_cities_k, paste0(wd_path,'/data/un_services_cities_k.csv'))
}

# -------------------------------------------------------------------------

# Analyze data ------------------------------------------------------------

# Join k data and DHS data
subnat_all_wide_k <- subnat_all_wide %>%
  filter(SurveyId  !=  'SN2020MIS') %>% # survey with many missing
  inner_join(., k_data_subnat, by = c('REG_ID'='REG_ID','SurveyId'='SurveyId','DHS_CountryCode'='DHS_CountryCode'))

# indicators_list_wide <- c('FP_NADM_W_UNT', 'FP_NADM_W_MNT', 'CM_ECMR_C_NNR', 'CM_ECMR_C_PNR', 'CM_ECMR_C_IMR', 'CM_ECMR_C_CMR', 'CM_ECMR_C_U5M', 'RH_DELP_C_DHF', 'CH_VACC_C_DP1', 'CH_VACC_C_DP2', 'CH_VACC_C_BAS', 'CH_VACC_C_AP2', 'CH_ARIS_C_ADV', 'CH_DIAR_C_DIA', 'CN_NUTS_C_HA3', 'CN_NUTS_C_HA2', 'CN_NUTS_C_WH3', 'CN_NUTS_C_WH2', 'CN_NUTS_C_WA3', 'CN_NUTS_C_WA2', 'CN_IYCF_C_4FA', 'CN_IYCF_C_MNA', 'CN_ANMC_C_ANY', 'ML_NETP_H_ITN', 'ML_NETP_H_IT2', 'ML_ITNA_P_ACC', 'ML_NETU_P_IT1', 'ML_NETC_C_IT1', 'ML_NETW_W_IT1', 'CO_MOBB_W_MOB', 'ED_EDAT_W_NED', 'ED_EDAT_W_SPR', 'ED_EDAT_W_CPR', 'ED_EDAT_W_SSC', 'ED_EDAT_W_CSC', 'ED_EDAT_W_HGH', 'ED_EDAT_W_PRI', 'ED_EDAT_W_SEC', 'ED_EDAT_W_MYR', 'ED_EDAT_M_PRI', 'ED_EDAT_M_SEC', 'ED_EDAT_M_MYR', 'ED_EDAT_B_NED', 'ED_EDAT_B_SPR', 'ED_EDAT_B_CPR', 'ED_EDAT_B_SSC', 'ED_EDAT_B_CSC', 'ED_EDAT_B_HGH', 'ED_EDAT_B_PRI', 'ED_EDAT_B_SEC', 'ED_EDAT_B_MYR', 'ED_NARP_W_FEM', 'ED_NARP_M_MAL', 'ED_NARP_B_BTH', 'ED_NARP_B_GPI', 'ED_GARP_B_GPI', 'ED_NARS_W_FEM', 'ED_NARS_M_MAL', 'ED_NARS_B_BTH', 'ED_NARS_B_GPI', 'ED_GARS_B_GPI', 'ED_EDUC_W_PRI', 'ED_EDUC_W_SEH', 'ED_EDUC_W_MYR', 'ED_EDUC_M_PRI', 'ED_EDUC_M_SEH', 'ED_EDUC_M_MYR', 'ED_LITR_W_LIT', 'ED_LITR_M_LIT', 'ED_LITY_W_SCH', 'ED_LITY_W_RDW', 'ED_LITY_W_RDP', 'ED_LITY_W_NRD', 'ED_LITY_W_NCD', 'ED_LITY_W_BLD', 'ED_LITY_W_LIT', 'ED_LITY_M_LIT', 'ED_MDIA_W_NWS', 'ED_MDIA_W_TLV', 'ED_MDIA_W_RDO', 'ED_MDIA_W_3MD', 'ED_MDIA_W_N3M', 'EM_EMPL_W_EMC', 'EM_EMPL_M_EMC', 'AN_NUTS_W_SHT', 'AN_NUTS_W_THN', 'AN_ANEM_W_ANY', 'EM_EMPL_W_ENC', 'EM_EMPL_M_ENC', 'CO_MOBB_W_BNK', 'EM_OCCP_W_PRO', 'EM_OCCP_W_CLR', 'EM_OCCP_W_SAL', 'EM_OCCP_W_MNS', 'EM_OCCP_W_MNU', 'EM_OCCP_W_DOM', 'EM_OCCP_W_AGR', 'EM_OCCP_W_OTH', 'EM_OCCP_W_TOT', 'EM_OCCP_M_PRO', 'EM_OCCP_M_CLR', 'EM_OCCP_M_SAL', 'EM_OCCP_M_MNS', 'EM_OCCP_M_MNU', 'EM_OCCP_M_AGR', 'EM_OCCP_M_OTH', 'WS_SRCE_H_IMP', 'WS_SRCE_H_PIP', 'WS_SRCE_H_NIM', 'WS_SRCE_H_IOP', 'WS_SRCE_H_BAS', 'WS_SRCE_H_LTD', 'WS_SRCE_P_IMP', 'WS_SRCE_P_PIP', 'WS_SRCE_P_NIM', 'WS_SRCE_P_IOP', 'WS_SRCE_P_BAS', 'WS_SRCE_P_LTD', 'WS_TIME_H_ONP', 'WS_TIME_H_L30', 'WS_TIME_H_M30', 'WS_TIME_P_ONP', 'WS_TIME_P_L30', 'WS_TIME_P_M30', 'WS_WTRT_H_APP', 'WS_WTRT_P_APP', 'WS_TLET_H_IMP', 'WS_TLET_H_NFC', 'WS_TLET_H_BAS', 'WS_TLET_H_LTD', 'WS_TLET_P_IMP', 'WS_TLET_P_NFC', 'WS_TLET_P_BAS', 'WS_TLET_P_LTD', 'WS_HNDW_H_OBS', 'WS_HNDW_H_SOP', 'WS_HNDW_H_BAS', 'WS_HNDW_H_LTD', 'WS_HNDW_P_OBS', 'WS_HNDW_P_SOP', 'WS_HNDW_P_BAS', 'WS_HNDW_P_LTD', 'HC_ELEC_H_ELC', 'HC_ELEC_H_NEL', 'HC_ELEC_P_ELC', 'HC_ELEC_P_NEL', 'HC_FLRM_H_NAT', 'HC_FLRM_H_ETH', 'HC_FLRM_P_NAT', 'HC_FLRM_P_ETH', 'HC_CKPL_H_HSE', 'HC_CKPL_P_HSE', 'HC_CKFL_H_SLD', 'HC_CKFL_H_CLN', 'HC_CKFL_P_SLD', 'HC_CKFL_P_CLN', 'HC_PPRM_H_12P', 'HC_PPRM_H_34P', 'HC_PPRM_H_56P', 'HC_PPRM_H_7PP', 'HC_PPRM_H_MNP', 'HC_HEFF_H_RDO', 'HC_HEFF_H_TLV', 'HC_HEFF_H_MPH', 'HC_HEFF_H_NPH', 'HC_HEFF_H_CMP', 'HC_HEFF_H_FRG', 'HC_TRNS_H_BIK', 'HC_TRNS_H_SCT', 'HC_TRNS_H_CAR', 'HC_WIXQ_P_LOW', 'HC_WIXQ_P_2ND', 'HC_WIXQ_P_MID', 'HC_WIXQ_P_4TH', 'HC_WIXQ_P_HGH', 'HC_WIXQ_P_GNI', 'HC_OLDR_H_3GN', 'EM_OCCP_M_DOM', 'WS_TLOC_H_DWL', 'WS_TLOC_P_DWL')
indicators_list_wide <- c('EM_OCCP_M_AGR', 'EM_OCCP_M_MNS', 'EM_OCCP_M_MNU', 'EM_OCCP_M_PRO', 'EM_OCCP_W_AGR', 'EM_OCCP_W_DOM', 'EM_OCCP_W_MNU', 'EM_OCCP_W_PRO', 'HC_WIXQ_P_LOW', 'HC_WIXQ_P_2ND', 'HC_WIXQ_P_MID', 'HC_WIXQ_P_4TH', 'HC_WIXQ_P_HGH', 'HC_WIXQ_P_GNI', 'HC_HEFF_H_CMP', 'HC_HEFF_H_FRG', 'HC_HEFF_H_MPH', 'HC_HEFF_H_TLV', 'HC_TRNS_H_CAR', 'ED_EDAT_B_MYR', 'ED_EDAT_M_MYR', 'ED_EDAT_W_MYR', 'ED_EDAT_B_NED', 'ED_EDAT_B_SEC', 'ED_EDAT_B_HGH', 'ED_NARP_B_BTH', 'ED_NARS_B_BTH', 'ED_GARS_B_GPI', 'ED_LITR_W_LIT', 'ED_LITY_W_LIT', 'ED_LITY_W_NRD', 'ED_NARP_W_FEM', 'ED_NARS_W_FEM', 'ED_EDUC_W_SEH', 'ED_EDAT_W_NED', 'ED_EDAT_W_SEC', 'ED_EDAT_W_HGH', 'ED_MDIA_W_3MD', 'ED_MDIA_W_N3M', 'CO_MOBB_W_BNK', 'CO_MOBB_W_MOB', 'AN_NUTS_W_THN', 'FP_NADM_W_MNT', 'RH_DELP_C_DHF', 'CM_ECMR_C_CMR', 'CM_ECMR_C_IMR', 'CM_ECMR_C_PNR', 'CM_ECMR_C_U5M', 'CH_VACC_C_BAS', 'CN_IYCF_C_4FA', 'CN_NUTS_C_HA2', 'CN_NUTS_C_WA2', 'CN_NUTS_C_WH2', 'CN_ANMC_C_ANY', 'WS_HNDW_P_BAS', 'WS_HNDW_P_SOP', 'WS_TLOC_P_DWL', 'WS_TLET_P_BAS', 'WS_TLET_P_IMP', 'WS_TLET_P_NFC', 'WS_SRCE_P_BAS', 'WS_SRCE_P_IMP', 'WS_SRCE_P_IOP', 'WS_SRCE_P_LTD', 'WS_SRCE_P_NIM', 'WS_SRCE_P_PIP', 'WS_TIME_P_L30', 'WS_TIME_P_M30', 'WS_TIME_P_ONP', 'HC_PPRM_H_MNP', 'HC_OLDR_H_3GN', 'HC_PPRM_H_12P', 'HC_PPRM_H_34P', 'HC_PPRM_H_56P', 'HC_PPRM_H_7PP', 'HC_CKPL_P_HSE', 'HC_CKFL_P_CLN', 'HC_CKFL_P_SLD', 'HC_ELEC_P_ELC', 'HC_ELEC_P_NEL', 'HC_FLRM_P_ETH', 'HC_FLRM_P_NAT', 'WS_HNDW_H_BAS', 'WS_HNDW_H_SOP', 'WS_TLOC_H_DWL', 'WS_TLET_H_BAS', 'WS_TLET_H_IMP', 'WS_TLET_H_NFC', 'WS_SRCE_H_BAS', 'WS_SRCE_H_IMP', 'WS_SRCE_H_IOP', 'WS_SRCE_H_LTD', 'WS_SRCE_H_NIM', 'WS_SRCE_H_PIP', 'WS_TIME_H_L30', 'WS_TIME_H_M30', 'WS_TIME_H_ONP', 'HC_CKPL_H_HSE', 'HC_CKFL_H_CLN', 'HC_CKFL_H_SLD', 'HC_ELEC_H_ELC', 'HC_ELEC_H_NEL', 'HC_FLRM_H_ETH', 'HC_FLRM_H_NAT')


subnat_all_wide_k %>% st_drop_geometry() %>% 
  select(DHSREGEN, REGCODE, CountryName, DHS_CountryCode) %>% distinct() %>% nrow()

subnat_all_wide_k %>% st_drop_geometry() %>% 
  select(CountryName, DHS_CountryCode) %>% distinct() %>% nrow()

subnat_all_wide_k %>% st_drop_geometry() %>% 
  select(SurveyId) %>% distinct() %>% nrow()

subnat_all_wide_k %>% st_drop_geometry() %>% 
  select(SVYYEAR) %>% distinct()

# 238 administrative regions across 22 countries and 40 unique surveys taking place between 2010 and 2021, producing 367 unique survey observations

subnat_all_wide_k  <- subnat_all_wide_k %>% filter(!is.na(k_complexity)) %>%
  mutate(population_k_1_3 = population_k_1 + population_k_2 + population_k_3,
         population_k_4_plus = population_k_4 + population_k_5 + population_k_6 + population_k_7 + population_k_8 + population_k_9 + population_k_10plus + population_off_network) %>%
  mutate(across(all_of(indicators_list_wide), .fns = ~./100)) %>%
  mutate(region_non_urban_share = region_non_urban/(region_core_urban+region_peripheral_urban+region_peri_urban+region_non_urban))
  # mutate(across(c(CM_ECMR_C_IMR, RH_DELP_C_DHF, ED_EDAT_B_NED, ED_EDAT_B_CPR, ED_EDAT_B_SSC, ED_EDAT_B_CSC, ED_EDAT_B_HGH, ED_EDAT_B_MYR, ED_LITR_W_LIT, WS_TIME_H_ONP, WS_TIME_P_ONP, WS_SRCE_H_IOP, WS_SRCE_P_IOP, WS_SRCE_P_IMP, WS_SRCE_H_BAS, WS_SRCE_P_BAS, WS_SRCE_H_LTD, WS_SRCE_P_LTD, WS_TLET_H_IMP, WS_TLET_P_IMP, WS_TLET_H_BAS, WS_TLET_H_LTD, WS_TLET_P_BAS, WS_TLET_P_LTD, HC_ELEC_H_ELC, HC_ELEC_P_ELC, HC_FLRM_H_NAT, HC_FLRM_P_NAT, HC_FLRM_H_ETH, HC_FLRM_P_ETH, HC_PPRM_H_12P, HC_PPRM_H_34P, HC_PPRM_H_56P, HC_PPRM_H_7PP, HC_PPRM_H_MNP, HC_WIXQ_P_LOW, HC_WIXQ_P_2ND, HC_WIXQ_P_MID, HC_WIXQ_P_4TH, HC_WIXQ_P_HGH, HC_WIXQ_P_LOW_2ND, HC_WIXQ_P_LOW_4TH, HC_WIXQ_P_LOW_2ND, HC_WIXQ_P_LOW_4TH, HC_PPRM_H_37P, ED_EDAT_B_NED_PR, ED_EDAT_B_SC),
  #          .fns = ~./100))
  
names(subnat_all_wide_k )
sapply(subnat_all_wide_k, function(X) sum(is.na(X)))

custom_labels <- list(
  'column_var' = c('EM_OCCP_M_AGR', 'EM_OCCP_M_MNS', 'EM_OCCP_M_MNU', 'EM_OCCP_M_PRO', 'EM_OCCP_W_AGR', 'EM_OCCP_W_DOM', 'EM_OCCP_W_MNU', 'EM_OCCP_W_PRO', 'HC_WIXQ_P_LOW', 'HC_WIXQ_P_2ND', 'HC_WIXQ_P_MID', 'HC_WIXQ_P_4TH', 'HC_WIXQ_P_HGH', 'HC_WIXQ_P_GNI', 'HC_HEFF_H_CMP', 'HC_HEFF_H_FRG', 'HC_HEFF_H_MPH', 'HC_HEFF_H_TLV', 'HC_TRNS_H_CAR', 'ED_EDAT_B_MYR', 'ED_EDAT_M_MYR', 'ED_EDAT_W_MYR', 'ED_EDAT_B_NED', 'ED_EDAT_B_SEC', 'ED_EDAT_B_HGH', 'ED_NARP_B_BTH', 'ED_NARS_B_BTH', 'ED_GARS_B_GPI', 'ED_LITR_W_LIT', 'ED_LITY_W_LIT', 'ED_LITY_W_NRD', 'ED_NARP_W_FEM', 'ED_NARS_W_FEM', 'ED_EDUC_W_SEH', 'ED_EDAT_W_NED', 'ED_EDAT_W_SEC', 'ED_EDAT_W_HGH', 'ED_MDIA_W_3MD', 'ED_MDIA_W_N3M', 'CO_MOBB_W_BNK', 'CO_MOBB_W_MOB', 'AN_NUTS_W_THN', 'FP_NADM_W_MNT', 'RH_DELP_C_DHF', 'CM_ECMR_C_CMR', 'CM_ECMR_C_IMR', 'CM_ECMR_C_PNR', 'CM_ECMR_C_U5M', 'CH_VACC_C_BAS', 'CN_IYCF_C_4FA', 'CN_NUTS_C_HA2', 'CN_NUTS_C_WA2', 'CN_NUTS_C_WH2', 'CN_ANMC_C_ANY', 'WS_HNDW_P_BAS', 'WS_HNDW_P_SOP', 'WS_TLOC_P_DWL', 'WS_TLET_P_BAS', 'WS_TLET_P_IMP', 'WS_TLET_P_NFC', 'WS_SRCE_P_BAS', 'WS_SRCE_P_IMP', 'WS_SRCE_P_IOP', 'WS_SRCE_P_LTD', 'WS_SRCE_P_NIM', 'WS_SRCE_P_PIP', 'WS_TIME_P_L30', 'WS_TIME_P_M30', 'WS_TIME_P_ONP', 'HC_PPRM_H_MNP', 'HC_OLDR_H_3GN', 'HC_PPRM_H_12P', 'HC_PPRM_H_34P', 'HC_PPRM_H_56P', 'HC_PPRM_H_7PP', 'HC_CKPL_P_HSE', 'HC_CKFL_P_CLN', 'HC_CKFL_P_SLD', 'HC_ELEC_P_ELC', 'HC_ELEC_P_NEL', 'HC_FLRM_P_ETH', 'HC_FLRM_P_NAT', 'WS_HNDW_H_BAS', 'WS_HNDW_H_SOP', 'WS_TLOC_H_DWL', 'WS_TLET_H_BAS', 'WS_TLET_H_IMP', 'WS_TLET_H_NFC', 'WS_SRCE_H_BAS', 'WS_SRCE_H_IMP', 'WS_SRCE_H_IOP', 'WS_SRCE_H_LTD', 'WS_SRCE_H_NIM', 'WS_SRCE_H_PIP', 'WS_TIME_H_L30', 'WS_TIME_H_M30', 'WS_TIME_H_ONP', 'HC_CKPL_H_HSE', 'HC_CKFL_H_CLN', 'HC_CKFL_H_SLD', 'HC_ELEC_H_ELC', 'HC_ELEC_H_NEL', 'HC_FLRM_H_ETH', 'HC_FLRM_H_NAT'),
  'order' = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104),
  'positive_dim' = c(0, 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 1, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1, 1, 0, 1, 0, 0, 0),
  'unit' = c('Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Households', 'Households', 'Households', 'Households', 'Households', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Households', 'Households', 'Households', 'Households', 'Households', 'Households', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Population', 'Households', 'Households', 'Households', 'Households', 'Households', 'Households', 'Households', 'Households', 'Households', 'Households', 'Households', 'Households', 'Households', 'Households', 'Households', 'Households', 'Households', 'Households', 'Households', 'Households', 'Households', 'Households'),
  'household_duplicate' = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
  'category' = c("Economic Wellbeing", "Economic Wellbeing", "Economic Wellbeing", "Economic Wellbeing", "Economic Wellbeing", "Economic Wellbeing", "Economic Wellbeing", "Economic Wellbeing", "Economic Wellbeing", "Economic Wellbeing", "Economic Wellbeing", "Economic Wellbeing", "Economic Wellbeing", "Economic Wellbeing", "Economic Wellbeing", "Economic Wellbeing", "Economic Wellbeing", "Economic Wellbeing", "Economic Wellbeing", "Education and Literacy", "Education and Literacy", "Education and Literacy", "Education and Literacy", "Education and Literacy", "Education and Literacy", "Education and Literacy", "Education and Literacy", "Education and Literacy", "Education and Literacy", "Education and Literacy", "Education and Literacy", "Education and Literacy", "Education and Literacy", "Education and Literacy", "Education and Literacy", "Education and Literacy", "Education and Literacy", "Education and Literacy", "Education and Literacy", "Education and Literacy", "Education and Literacy", "Children and Women's Health", "Children and Women's Health", "Children and Women's Health", "Children and Women's Health", "Children and Women's Health", "Children and Women's Health", "Children and Women's Health", "Children and Women's Health", "Children and Women's Health", "Children and Women's Health", "Children and Women's Health", "Children and Women's Health", "Children and Women's Health", "Water, Sanitation and Hygiene", "Water, Sanitation and Hygiene", "Water, Sanitation and Hygiene", "Water, Sanitation and Hygiene", "Water, Sanitation and Hygiene", "Water, Sanitation and Hygiene", "Water, Sanitation and Hygiene", "Water, Sanitation and Hygiene", "Water, Sanitation and Hygiene", "Water, Sanitation and Hygiene", "Water, Sanitation and Hygiene", "Water, Sanitation and Hygiene", "Water, Sanitation and Hygiene", "Water, Sanitation and Hygiene", "Water, Sanitation and Hygiene", "Household Characteristics", "Household Characteristics", "Household Characteristics", "Household Characteristics", "Household Characteristics", "Household Characteristics", "Household Characteristics", "Household Characteristics", "Household Characteristics", "Household Characteristics", "Household Characteristics", "Household Characteristics", "Household Characteristics", "Water, Sanitation and Hygiene", "Water, Sanitation and Hygiene", "Water, Sanitation and Hygiene", "Water, Sanitation and Hygiene", "Water, Sanitation and Hygiene", "Water, Sanitation and Hygiene", "Water, Sanitation and Hygiene", "Water, Sanitation and Hygiene", "Water, Sanitation and Hygiene", "Water, Sanitation and Hygiene", "Water, Sanitation and Hygiene", "Water, Sanitation and Hygiene", "Water, Sanitation and Hygiene", "Water, Sanitation and Hygiene", "Water, Sanitation and Hygiene", "Household Characteristics", "Household Characteristics", "Household Characteristics", "Household Characteristics", "Household Characteristics", "Household Characteristics", "Household Characteristics"),
  'subcategory' = c("Livelihoods", "Livelihoods", "Livelihoods", "Livelihoods", "Livelihoods", "Livelihoods", "Livelihoods", "Livelihoods", "Wealth", "Wealth", "Wealth", "Wealth", "Wealth", "Wealth", "Household effects", "Household effects", "Household effects", "Household effects", "Household effects", "Education", "Education", "Education", "Education", "Education", "Education", "Education", "Education", "Women's education", "Women's education", "Women's education", "Women's education", "Women's education", "Women's education", "Women's education", "Women's education", "Women's education", "Women's education", "Women's education", "Women's education", "Women's education", "Women's education", "Women's health", "Women's health", "Women's health", "Children's health", "Children's health", "Children's health", "Children's health", "Children's health", "Children's health", "Children's health", "Children's health", "Children's health", "Children's health", "Handwashing", "Handwashing", "Sanitation", "Sanitation", "Sanitation", "Sanitation", "Water", "Water", "Water", "Water", "Water", "Water", "Water", "Water", "Water", "Crowding", "Crowding", "Crowding", "Crowding", "Crowding", "Crowding", "Cooking", "Cooking", "Cooking", "Electricity", "Electricity", "Floor materials", "Floor materials", "Handwashing", "Handwashing", "Sanitation", "Sanitation", "Sanitation", "Sanitation", "Water", "Water", "Water", "Water", "Water", "Water", "Water", "Water", "Water", "Cooking", "Cooking", "Cooking", "Electricity", "Electricity", "Floor materials", "Floor materials")
) %>% as_tibble()

dhs_dict_df <- dhs_indicators() %>%
  select(Definition, MeasurementType, ShortName, IndicatorId, Level1, Level2, Level3, Label) %>%
  rename_all(list(tolower)) %>%
  relocate(indicatorid, definition, label, level1, level2, level3, shortname, measurementtype) %>%
  left_join(., custom_labels, by = c('indicatorid' = 'column_var')) %>%
  filter(!is.na(order)) %>%
  arrange(order)


# Correlations table ------------------------------------------------------

streets_dhs_regions <- read_csv('data/streets_dhs_regions.csv') %>%
  mutate(vehicular_highway_km = highway_length_meters*0.001,
        region_area_km2  = region_area_m2*1e-6, 
        street_density_ratio_km_to_km2 = vehicular_highway_km/region_area_km2)

subnat_all_wide_k <- subnat_all_wide_k %>%
  left_join(., streets_dhs_regions %>% select(REG_ID, street_density_ratio_km_to_km2), 
            by = c('REG_ID'='REG_ID'))

# DHS subnational data vs population weighted K
(cor_dhs <- rcorr(subnat_all_wide_k %>% st_drop_geometry() %>%
                    select_at(c('k_complexity_average', indicators_list_wide)) %>%
                    relocate(all_of(c('k_complexity_average', indicators_list_wide))) %>% as.matrix(),
                  #select(k_complexity_average, CM_ECMR_C_IMR, RH_DELP_C_DHF, ED_EDAT_B_NED, ED_EDAT_B_CPR, ED_EDAT_B_SSC, ED_EDAT_B_CSC, ED_EDAT_B_HGH, ED_EDAT_B_MYR, ED_LITR_W_LIT, WS_TIME_H_ONP, WS_TIME_P_ONP, WS_SRCE_H_IOP, WS_SRCE_P_IOP, WS_SRCE_P_IMP, WS_SRCE_H_BAS, WS_SRCE_P_BAS, WS_SRCE_H_LTD, WS_SRCE_P_LTD, WS_TLET_H_IMP, WS_TLET_P_IMP, WS_TLET_H_BAS, WS_TLET_H_LTD, WS_TLET_P_BAS, WS_TLET_P_LTD, HC_ELEC_H_ELC, HC_ELEC_P_ELC, HC_FLRM_H_NAT, HC_FLRM_P_NAT, HC_FLRM_H_ETH, HC_FLRM_P_ETH, HC_PPRM_H_12P, HC_PPRM_H_34P, HC_PPRM_H_56P, HC_PPRM_H_7PP, HC_PPRM_H_MNP, HC_WIXQ_P_LOW, HC_WIXQ_P_2ND, HC_WIXQ_P_MID, HC_WIXQ_P_4TH, HC_WIXQ_P_HGH, HC_WIXQ_P_LOW_2ND, HC_WIXQ_P_LOW_4TH, HC_WIXQ_P_GNI) %>% as.matrix(),
                  type=c('spearman')) %>% tidy() %>% filter(column2 == 'k_complexity_average') %>%
   inner_join(., dhs_dict_df , by = c('column1' = 'indicatorid')) %>%
   rename(column_var = column1) %>%
   select(column_var, category, subcategory, definition, label, estimate, n, p.value, level1, level2, level3, shortname, measurementtype, order, positive_dim, unit, household_duplicate) %>% 
   arrange(order) %>%
   mutate(universe = 'Regions', source = 'DHS') %>% as.data.frame() %>%
   rename(column_label = definition))

(cor_dhs_2 <- rcorr(subnat_all_wide_k %>% st_drop_geometry() %>%
                    select_at(c('street_density_ratio_km_to_km2', indicators_list_wide)) %>%
                    relocate(all_of(c('street_density_ratio_km_to_km2', indicators_list_wide))) %>% as.matrix(),
                  type=c('spearman')) %>% tidy() %>% filter(column2 == 'street_density_ratio_km_to_km2') %>%
    inner_join(., dhs_dict_df, by = c('column1' = 'indicatorid')) %>%
    select(column1, estimate, n, p.value) %>%
    rename(column_var = column1,
           estimate_street_density = estimate, 
           n_street_density = n, 
           p.value_street_density = p.value))

(cor_dhs_log <- rcorr(subnat_all_wide_k %>% st_drop_geometry() %>%
                        select_at(c('k_complexity_average', indicators_list_wide)) %>%
                        relocate(all_of(c('k_complexity_average', indicators_list_wide))) %>% 
                        mutate_all(log) %>% 
                        mutate_all(function(x) ifelse(is.infinite(x), 0, x)) %>%
                        mutate_all(function(x) ifelse(is.na(x), 0, x)) %>%
                        mutate_all(function(x) ifelse(is.nan(x), 0, x)) %>%
                        as.matrix(),
                    type=c('spearman')) %>% tidy() %>% filter(column2 == 'k_complexity_average') %>%
    inner_join(., dhs_dict_df, by = c('column1' = 'indicatorid')) %>%
    select(column1, estimate, n, p.value) %>%
    rename(column_var = column1,
           log_v_log_k_complexity = estimate, 
           n_log_v_log_k_complexity = n, 
           p.value_log_v_log_k_complexity = p.value))

(cor_dhs_urban <- rcorr(subnat_all_wide_k %>% st_drop_geometry() %>%
                    filter(region_non_urban_share < .5) %>%
                    select_at(c('k_complexity_average', indicators_list_wide)) %>%
                    relocate(all_of(c('k_complexity_average', indicators_list_wide))) %>% as.matrix(),
                  #select(k_complexity_average, CM_ECMR_C_IMR, RH_DELP_C_DHF, ED_EDAT_B_NED, ED_EDAT_B_CPR, ED_EDAT_B_SSC, ED_EDAT_B_CSC, ED_EDAT_B_HGH, ED_EDAT_B_MYR, ED_LITR_W_LIT, WS_TIME_H_ONP, WS_TIME_P_ONP, WS_SRCE_H_IOP, WS_SRCE_P_IOP, WS_SRCE_P_IMP, WS_SRCE_H_BAS, WS_SRCE_P_BAS, WS_SRCE_H_LTD, WS_SRCE_P_LTD, WS_TLET_H_IMP, WS_TLET_P_IMP, WS_TLET_H_BAS, WS_TLET_H_LTD, WS_TLET_P_BAS, WS_TLET_P_LTD, HC_ELEC_H_ELC, HC_ELEC_P_ELC, HC_FLRM_H_NAT, HC_FLRM_P_NAT, HC_FLRM_H_ETH, HC_FLRM_P_ETH, HC_PPRM_H_12P, HC_PPRM_H_34P, HC_PPRM_H_56P, HC_PPRM_H_7PP, HC_PPRM_H_MNP, HC_WIXQ_P_LOW, HC_WIXQ_P_2ND, HC_WIXQ_P_MID, HC_WIXQ_P_4TH, HC_WIXQ_P_HGH, HC_WIXQ_P_LOW_2ND, HC_WIXQ_P_LOW_4TH, HC_WIXQ_P_GNI) %>% as.matrix(),
                  type=c('spearman')) %>% tidy() %>% filter(column2 == 'k_complexity_average') %>%
    inner_join(., dhs_dict_df, by = c('column1' = 'indicatorid')) %>%
    rename(column_var = column1) %>% 
    as.data.frame() %>% 
  select(column_var, estimate, n, p.value) %>%
  rename(estimate_urban = estimate,
         n_urban = n, 
         p.value_urban = p.value))

(cor_dhs_urban_2 <- rcorr(subnat_all_wide_k %>% st_drop_geometry() %>%
                          filter(region_non_urban_share < .5) %>%
                          select_at(c('street_density_ratio_km_to_km2', indicators_list_wide)) %>%
                          relocate(all_of(c('street_density_ratio_km_to_km2', indicators_list_wide))) %>% as.matrix(),
                        type=c('spearman')) %>% tidy() %>% filter(column2 == 'street_density_ratio_km_to_km2') %>%
    inner_join(., dhs_dict_df, by = c('column1' = 'indicatorid')) %>%
    rename(column_var = column1) %>% 
    as.data.frame() %>% 
    select(column_var, estimate, n, p.value) %>%
    rename(estimate_urban_street_density = estimate,
           n_urban_street_density = n, 
           p.value_urban_street_density = p.value))

(cor_dhs <- cor_dhs %>%
    left_join(., cor_dhs_urban, by = c('column_var' = 'column_var')) %>%
    left_join(., cor_dhs_2, by = c('column_var' = 'column_var')) %>%
    left_join(., cor_dhs_urban_2, by = c('column_var' = 'column_var')) %>%
    left_join(., cor_dhs_log, by = c('column_var' = 'column_var')) %>%
    relocate(column_var, category, subcategory, column_label, label, estimate, n, p.value, estimate_urban, n_urban, p.value_urban))

(cor_dhs_trim <- cor_dhs %>%
  filter(household_duplicate == 0))

cor_dhs_trim <- cor_dhs_trim %>%
  mutate(larger_coefficient = case_when(abs(estimate_street_density) > abs(estimate) ~ 'Street density', TRUE ~ as.character('k-complexity')),
         larger_coefficient_urban = case_when(abs(estimate_urban_street_density) > abs(estimate_urban) ~ 'Street density', TRUE ~ as.character('k-complexity')),
         larger_coefficient_k_complexity = case_when(abs(estimate_street_density) < abs(estimate) ~ 1, TRUE ~ 0),
         larger_coefficient_k_complexity_urban = case_when(abs(estimate_urban_street_density) < abs(estimate_urban) ~ 1, TRUE ~ 0),
         larger_coefficient_street_density = case_when(abs(estimate_street_density) > abs(estimate) ~ 1, TRUE ~ 0),
         larger_coefficient_street_density_urban = case_when(abs(estimate_urban_street_density) > abs(estimate_urban) ~ 1, TRUE ~ 0),
         not_significant_street_density  = case_when( p.value_street_density > .01 ~ 1, TRUE ~ 0),
         not_significant_k_complexity = case_when(p.value > .01 ~ 1, TRUE ~ 0),
         not_significant_street_density_urban  = case_when( p.value_urban_street_density > .01 ~ 1, TRUE ~ 0),
         not_significant_k_complexity_urban = case_when(p.value_urban > .01 ~ 1, TRUE ~ 0),
         indicator_count = 1)


(insig_street_density <- cor_dhs_trim %>% filter(not_significant_street_density == 1) %>%
  filter(p.value_street_density >= .05) %>%
  select(subcategory, label, estimate_street_density, n_street_density, p.value_street_density))


(cor_summary_streets_v_k <- cor_dhs_trim %>% summarize_at(vars(indicator_count, 
                                   larger_coefficient_k_complexity, larger_coefficient_street_density,
                                   not_significant_street_density, not_significant_k_complexity, 
                                   larger_coefficient_k_complexity_urban, larger_coefficient_street_density_urban, 
                                   not_significant_street_density_urban, not_significant_k_complexity_urban), list(sum)) %>%
  pivot_longer(cols = everything()) %>%
  mutate(share = value / nrow(cor_dhs_trim)))


#cor_dhs %>% select(label, estimate, n, p.value, level1, level2, column_var) %>% as_tibble() %>% print(n = 200)
# write_csv(cor_dhs %>% as_tibble(),
#           '/Users/nm/Desktop/dhs_corr.csv')

# Urban Population Living in Slums by Country 2018 vs population weighted K
(cor_unslums <- rcorr(un_slums_k %>% select(k_complexity_average, percent_2020_coalesced) %>% as.matrix(),
                 type=c('spearman')) %>% tidy() %>%
    rename(column_var = column1) %>% select(column_var, estimate, n, p.value) %>% 
    mutate(column_label = case_when(column_var == 'percent_2020_coalesced' ~ "Proportion of country's urban population living in slum households",
                                    TRUE ~ as.character('')),
      universe = 'Urban areas',
      source = 'UN-Habitat') %>% as.data.frame())

# UN Habitat Services Data vs population weighted K (at city level)
(cor_unhab_1 <- rcorr(un_services_cities_k %>% select(k_complexity_average, improved_water_sources_non_piped_water_sources_total_non_piped, improved_sanitation_facilities_non_sewered_facilities_total_non_sewered, solid_fuel, improved_water_sources_piped_water_sources_piped_water, improved_sanitation_facilities_sewered_facilities_connection_to_sewerage, type_of_main_cooking_fuel_clean_fuel_total_clean_fuel) %>% as.matrix(),
                      type=c('spearman')) %>% 
    tidy() %>% filter(column2 == 'k_complexity_average') %>%
    rename(column_var = column1) %>% 
    select(column_var, estimate, n, p.value) %>% 
    mutate(column_label = case_when(column_var == 'improved_water_sources_non_piped_water_sources_total_non_piped' ~ 'Percentage of population with non-piped water sources (borehole/tubewell, protected well/spring, delivered/packaged)', #  (borehole, tubewell, protected well, protected spring, rainwater, delivered water, bottled and sached water)
                                    column_var == 'improved_sanitation_facilities_non_sewered_facilities_total_non_sewered' ~ 'Percentage of population with non-sewered facilities (composting toilet, flush septic, pit latrines)', #  (composting toilet, ventilated improved pit, flush septic, flush to pit latrine, pit latrine with slab)
                                    column_var == 'solid_fuel' ~ 'Percentage of population with solid fuel (coal, lignite, charcoal, wood, straw, shrub, grass, etc.)',
                                    column_var == 'improved_water_sources_piped_water_sources_piped_water' ~ 'Percentage of population with piped water into dwelling or plot (piped water, public tap)',
                                    column_var == 'improved_sanitation_facilities_sewered_facilities_connection_to_sewerage' ~ 'Percentage of population with improved sanitation facilities with connection to sewerage',
                                    column_var == 'type_of_main_cooking_fuel_clean_fuel_total_clean_fuel' ~ 'Percentage of population with clean fuel (electricity, LPG, natural gas, biogas)',
                                    TRUE ~ as.character('')),
           universe = 'Cities',
           source = 'UN-Habitat') %>% as.data.frame() )

(cor_unhab_2 <- rcorr(un_services_cities_k %>% select(k_complexity_average, 
                            durable_housing_durable_floor_material,
                            durable_housing_durable_roof_material,
                            other_basic_services_telephone,
                            other_basic_services_mobile_phone,
                            other_basic_services_electricity
                            #durable_housing_durable_wall_material, sufficient_living_area
                            ) %>% as.matrix(),
      type=c('spearman')) %>% 
  tidy() %>% filter(column2 == 'k_complexity_average') %>%
  rename(column_var = column1) %>% 
  select(column_var, estimate, n, p.value)  %>% 
    mutate(column_label = case_when(
      column_var == 'durable_housing_durable_floor_material' ~ 'Percentage of population with durable floor material',
      column_var == 'durable_housing_durable_roof_material' ~ 'Percentage of population with durable roof material',
      column_var == 'other_basic_services_telephone' ~ 'Percentage of population with telephone',
      column_var == 'other_basic_services_mobile_phone' ~ 'Percentage of population with mobile phone',
      column_var == 'other_basic_services_electricity' ~ 'Percentage of population with electricity',
      TRUE ~ as.character(''))) %>%
  mutate(universe = 'Cities',
         source = 'UN-Habitat') %>% as.data.frame() )

cor_un <- rbind(cor_unslums, cor_unhab_1, cor_unhab_2) %>%
  mutate(p.value = round(p.value,8)) %>%
  # mutate(column_group = case_when(
  #   column_var %in% c('HC_PPRM_H_12P', 'HC_PPRM_H_34P', 'HC_PPRM_H_56P', 'HC_PPRM_H_7PP', 'HC_PPRM_H_MNP') ~ 'Crowding',
  #   column_var %in% c('ED_EDAT_B_NED', 'ED_EDAT_B_CPR', 'ED_EDAT_B_SSC', 'ED_EDAT_B_CSC', 'ED_EDAT_B_HGH', 'ED_EDAT_B_MYR', 'ED_LITR_W_LIT') ~ 'Education',
  #   column_var %in% c('CM_ECMR_C_IMR', 'RH_DELP_C_DHF') ~ 'Health',
  #   column_var %in% c('HC_FLRM_H_NAT', 'HC_FLRM_P_NAT', 'HC_FLRM_H_ETH', 'HC_FLRM_P_ETH', 'solid_fuel', 'type_of_main_cooking_fuel_clean_fuel_total_clean_fuel', 'durable_housing_durable_floor_material', 'durable_housing_durable_roof_material') ~ 'Housing',
  #   column_var %in% c('WS_TLET_H_IMP', 'WS_TLET_P_IMP', 'WS_TLET_H_BAS', 'WS_TLET_H_LTD', 'WS_TLET_P_BAS', 'WS_TLET_P_LTD', 'improved_sanitation_facilities_non_sewered_facilities_total_non_sewered', 'improved_sanitation_facilities_sewered_facilities_connection_to_sewerage') ~ 'Sanitation',
  #   column_var %in% c('percent_2020_coalesced') ~ 'Slums',
  #   column_var %in% c('HC_ELEC_H_ELC', 'HC_ELEC_P_ELC', 'other_basic_services_telephone', 'other_basic_services_mobile_phone', 'other_basic_services_electricity') ~ 'Utilities',
  #   column_var %in% c('WS_TIME_H_ONP', 'WS_TIME_P_ONP', 'WS_SRCE_H_IOP', 'WS_SRCE_P_IOP', 'WS_SRCE_P_IMP', 'WS_SRCE_H_BAS', 'WS_SRCE_P_BAS', 'WS_SRCE_H_LTD', 'WS_SRCE_P_LTD', 'improved_water_sources_non_piped_water_sources_total_non_piped', 'improved_water_sources_piped_water_sources_piped_water') ~ 'Water',
  #   column_var %in% c('HC_WIXQ_P_LOW', 'HC_WIXQ_P_2ND', 'HC_WIXQ_P_MID', 'HC_WIXQ_P_4TH', 'HC_WIXQ_P_HGH', 'HC_WIXQ_P_LOW_2ND', 'HC_WIXQ_P_LOW_4TH', 'HC_WIXQ_P_GNI') ~ 'Wealth',
  #   TRUE ~ as.character(''))) %>%
  relocate(column_label, estimate, p.value, n, universe, source, column_var)

cor_un %>% select(column_label, estimate, p.value, n, universe, source) %>% as_tibble() %>% print(n = 100, width = 200)
rm(cor_unslums, cor_unhab_1, cor_unhab_2)

write_excel_csv(cor_un, paste0(wd_path,'/data/dhs-analysis/data/correlations_table_un.csv'))
write_excel_csv(cor_dhs, paste0(wd_path,'/data/dhs-analysis/data/correlations_table_dhs.csv'))

# Show the correlation with pop density, building metrics, (hypothesis is these will have a weaker relationship)
# Look at the correlation -- scatter and annotate big outliers

# Regressions and Predictions tables --------------------------------------

# rcorr(subnat_all_wide_k %>% st_drop_geometry() %>% 
#   mutate(landscan_population_un_density_hectare = replace_na(landscan_population_un/block_hectares,0),
#          building_to_block_area_ratio = replace_na(building_area_m2/block_area_m2,0),
#          average_building_area_m2 = replace_na(building_area_m2/building_count,0),
#          region_total = region_core_urban + region_peripheral_urban + region_peri_urban + region_non_urban,
#          region_urban = region_core_urban + region_peripheral_urban, 
#          region_urban_share = region_urban/region_total,
#          average_pop_per_building = replace_na(landscan_population_un/building_count,0)) %>%
#   select(all_of(c('k_complexity_average', 'region_urban_share', 'average_building_area_m2', 'building_to_block_area_ratio', 'landscan_population_un_density_hectare', 'average_pop_per_building'))) %>%
#   as.matrix(),
#   type=c('spearman')) %>% 
#   tidy() %>% filter(column2 == 'k_complexity_average')

sections <- c("SurveyYear", "SurveyId", "REG_ID" ,"DHSREGEN", "CNTRYNAMEE")
# specifications <- c('k_complexity_average', 'region_nonurban_share', 'landscan_population_un_density_hectare', 'building_to_block_area_ratio', 'average_building_area_m2')
# specifications <- c('k_complexity_average', 'building_to_block_area_ratio', 'average_building_area_m2', 'block_area_m2', 'average_pop_per_building')

specifications <- c('k_complexity_average', 'street_density_ratio_km_to_km2', 'region_urban_share', 'share_building_count_under_31m2', 'building_to_block_area_ratio', 'landscan_population_un_density_hectare') # , 'average_pop_per_building' 
# average buildings size, pod density, 
reg_data <- subnat_all_wide_k %>% st_drop_geometry() %>%
  mutate(landscan_population_un_density_hectare = replace_na(landscan_population_un/block_hectares,0),
         building_to_block_area_ratio = replace_na(building_area_m2/block_area_m2,0),
         average_building_area_m2 = replace_na(building_area_m2/building_count,0),
         average_pop_per_building = replace_na(landscan_population_un/building_count,0),
         share_building_count_under_31m2 = replace_na((bldg_area_count_bin_01_0.50__log10_3.2+bldg_area_count_bin_02_0.75__log10_5.6+bldg_area_count_bin_03_1.00__log10_10+bldg_area_count_bin_04_1.25__log10_17.8)/building_count,0),
         share_population_4plus = replace_na((population_k_4 + population_k_5 + population_k_6 + population_k_7 + population_k_8 + population_k_9 + population_k_10plus + population_off_network) /landscan_population_un,0) ) %>%
  mutate(region_total = region_core_urban + region_peripheral_urban + region_peri_urban + region_non_urban,
         region_urban = region_core_urban + region_peripheral_urban, 
         region_urban_share = region_urban/region_total,
         region_periurban_share = region_peri_urban/region_total,
         region_nonurban_share =  region_non_urban/region_total,
         region_conurban_share = (region_urban+region_peri_urban)/region_total) %>%
  rowwise() %>%
  select(all_of(c(sections, specifications)),
         all_of(contains(cor_dhs %>% select(column_var) %>% pull())) 
         #cor_dhs %>% select(column_var) %>% pull()
         ) 

unique(cor_dhs$measurementtype)

cor_list <- cor_dhs %>% 
  #filter(!(column_var %in% c("HC_WIXQ_P_LOW_2ND", "HC_WIXQ_P_LOW_4TH", 'CM_ECMR_C_IMR', 'ED_EDAT_B_CPR', 'ED_EDAT_B_HGH', 'ED_EDAT_B_CSC', 'ED_EDAT_B_MYR', 'HC_PPRM_H_MNP', 'HC_WIXQ_P_GNI'))) %>%
  #filter(source == 'DHS') %>%
  filter(measurementtype %in% c("Percent", "Ratio", "Rate")) %>%
  select(column_var, column_label, estimate, p.value ) %>% 
  distinct() %>% as.list()

if (exists("combined_reg_output")) {rm(combined_reg_output)}
if (exists("combined_pred_output")) {rm(combined_pred_output)}


for (i in seq_along(unique(cor_list$column_var))) {
#for (i in seq_along(c('ED_LITR_W_LIT', 'WS_TLET_H_IMP', 'WS_SRCE_H_IOP', 'HC_WIXQ_P_HGH'))) {
  
  print(paste0('Progress: ',i,' - ',round(i/length(cor_list$column_var),4)*100,'%'))
  print(cor_list %>% pluck(1, i))
  print(cor_list %>% pluck(2, i))
  print(cor_list %>% pluck(3, i))
  
  reg_data_i <- reg_data %>%
    rename_with( ~ 'dv', all_of(cor_list %>% pluck(1, i))) %>% # all of warning
    rename_with( ~ 'dv_denom', all_of(paste0('dw_',cor_list %>% pluck(1, i)))) %>% # all of warning
    mutate_at(vars(c('dv','dv_denom')), as.numeric) %>%
    filter_at(vars(dv, dv_denom), all_vars(!is.na(.))) %>%
    mutate(dv = case_when(dv == 0 ~ .0001,
                          dv == 1 ~ .9999,
                          TRUE ~ dv)) %>%
    mutate(dv_numer = round(dv*dv_denom,0),
           dv_target = dv_numer,
           dv_nontarget = dv_denom - dv_numer) %>% 
    #select_all(~gsub("\\s+|\\.|\\/", "_", .)) %>%
    #rename_all(list(tolower)) %>%
    mutate(country = paste0(tolower(gsub("\\s+|\\.|'|\\/", "_", CNTRYNAMEE))))
  
  if(nrow(reg_data_i) == 0) next
  
  reg_data_i <- reg_data_i %>%
    cbind(., scale( reg_data_i %>% 
                      select(all_of(c(specifications))), center = TRUE, scale = TRUE) %>% as.data.frame() %>% 
            rename_with(., .fn = ~ paste0("norm_", .x),
                        .cols = everything()))
  
  country_dummies <- reg_data_i %>% 
    arrange(SurveyYear) %>% 
    select(country) %>% 
    distinct() %>% 
    mutate(keep_countries = row_number()) 
  
  country_fixed_effects <- country_dummies %>% filter(keep_countries > 1) %>% select(country) %>% mutate_at(vars(country), list(as.character)) %>% mutate(country = paste0('country_',country)) %>% pull() 
  country_dummies_drop <- country_dummies %>% filter(keep_countries == 1) %>% select(country) %>% mutate_at(vars(country), list(as.character)) %>% mutate(country = paste0('country_',country)) %>% pull() 
  
  reg_data_i <- reg_data_i %>% 
    recipe( ~ .) %>% step_dummy(country, one_hot = TRUE)  %>% prep() %>% bake(NULL) %>% select(-one_of(country_dummies_drop)) %>% 
    #mutate(dummy = 1) %>% tidyr::spread(country, dummy, fill = 0) %>% select(-one_of(country_dummies_drop)) %>%
    distinct() 
  
  # Binomial model
  binomial_model <- linear_reg() %>% set_engine("glm", family = binomial(link = "logit")) 
  
  binomial_single <- binomial_model %>% fit(reg_data_i %>% select(dv_target, dv_nontarget) %>% as.matrix() ~ .,  data = reg_data_i %>% select_at(all_of(c('dv_target', 'dv_nontarget', 'k_complexity_average'))))
  binomial_countries <- binomial_model %>% fit(reg_data_i %>% select(dv_target, dv_nontarget) %>% as.matrix() ~ ., data = reg_data_i %>% select_at(all_of(c('dv_target', 'dv_nontarget', 'k_complexity_average', country_fixed_effects))))
  binomial_controls <- binomial_model %>%  fit(reg_data_i %>% select(dv_target, dv_nontarget) %>% as.matrix() ~ ., data = reg_data_i %>% select_at(all_of(c('dv_target', 'dv_nontarget', specifications, country_fixed_effects))))
  
  binomial_single_imp <- binomial_model %>% fit(reg_data_i %>% select(dv_target, dv_nontarget) %>% as.matrix() ~ .,  data = reg_data_i %>% select_at(all_of(c('dv_target', 'dv_nontarget', 'norm_k_complexity_average'))))
  binomial_countries_imp <- binomial_model %>% fit(reg_data_i %>% select(dv_target, dv_nontarget) %>% as.matrix() ~ ., data = reg_data_i %>% select_at(all_of(c('dv_target', 'dv_nontarget', 'norm_k_complexity_average', country_fixed_effects))))
  binomial_controls_imp <- binomial_model %>%  fit(reg_data_i %>% select(dv_target, dv_nontarget) %>% as.matrix() ~ ., data = reg_data_i %>% select_at(all_of(c('dv_target', 'dv_nontarget', purrr::map_chr(specifications, ~ paste0("norm_", .)), country_fixed_effects))))
  
  # Quasibinomial model
  quasibinomial_model <- linear_reg() %>% set_engine("glm", family = quasibinomial(link = "logit")) 
  
  quasibinomial_single <- quasibinomial_model %>% fit(reg_data_i %>% select(dv_target, dv_nontarget) %>% as.matrix() ~ .,  data = reg_data_i %>% select_at(all_of(c('dv_target', 'dv_nontarget', 'k_complexity_average'))))
  quasibinomial_countries <- quasibinomial_model %>% fit(reg_data_i %>% select(dv_target, dv_nontarget) %>% as.matrix() ~ ., data = reg_data_i %>% select_at(all_of(c('dv_target', 'dv_nontarget', 'k_complexity_average', country_fixed_effects))))
  quasibinomial_controls <- quasibinomial_model %>%  fit(reg_data_i %>% select(dv_target, dv_nontarget) %>% as.matrix() ~ ., data = reg_data_i %>% select_at(all_of(c('dv_target', 'dv_nontarget', specifications, country_fixed_effects))))
  
  quasibinomial_single_imp <- quasibinomial_model %>% fit(reg_data_i %>% select(dv_target, dv_nontarget) %>% as.matrix() ~ .,  data = reg_data_i %>% select_at(all_of(c('dv_target', 'dv_nontarget', 'norm_k_complexity_average'))))
  quasibinomial_countries_imp <- quasibinomial_model %>% fit(reg_data_i %>% select(dv_target, dv_nontarget) %>% as.matrix() ~ ., data = reg_data_i %>% select_at(all_of(c('dv_target', 'dv_nontarget', 'norm_k_complexity_average', country_fixed_effects))))
  quasibinomial_controls_imp <- quasibinomial_model %>%  fit(reg_data_i %>% select(dv_target, dv_nontarget) %>% as.matrix() ~ ., data = reg_data_i %>% select_at(all_of(c('dv_target', 'dv_nontarget', purrr::map_chr(specifications, ~ paste0("norm_", .)), country_fixed_effects))))
  
  # Beta model
  beta_single <- betareg(formula = dv ~ k_complexity_average, weights = dv_denom, data = reg_data_i ) 
  beta_countries <- betareg(formula = as.formula(paste('dv', paste(c('k_complexity_average', country_fixed_effects), collapse=" + "), sep=" ~ ")), weights = dv_denom, data = reg_data_i) 
  beta_controls <- betareg(formula = as.formula(paste('dv', paste(c(specifications, country_fixed_effects), collapse=" + "), sep=" ~ ")), weights = dv_denom, data = reg_data_i) 
  
  beta_single_imp <- betareg(formula = dv ~ k_complexity_average, weights = dv_denom, data = reg_data_i ) 
  beta_countries_imp <- betareg(formula = as.formula(paste('dv', paste(c('norm_k_complexity_average', country_fixed_effects), collapse=" + "), sep=" ~ ")), weights = dv_denom, data = reg_data_i) 
  beta_controls_imp <- betareg(formula = as.formula(paste('dv', paste(c(purrr::map_chr(specifications, ~ paste0("norm_", .)), country_fixed_effects), collapse=" + "), sep=" ~ ")), weights = dv_denom, data = reg_data_i) 

  # logistic regression is best for proportional data when dv is binomial
  # beta regression is appropriate when exact distribution of dv is unknown
  # quasibinomial model is best when dv is over-dispersed (doesn't follow assumed binomial variance)
  dv_var = cor_list %>% pluck(1, i)
  dv_label = cor_list %>% pluck(2, i)
  dv_correl = cor_list %>% pluck(3, i)
  
  reg_output <- rbind(
    tidy(binomial_single, conf.int = TRUE) %>% mutate_if(is.numeric, round, 7) %>% mutate(distribution = 'binomial', predictors = 'k', dv_var = dv_var, dv_label = dv_label, dv_correl = dv_correl) %>%
      bind_cols(glance(binomial_single) %>% mutate(pseudo.r.squared = 1 - (deviance / null.deviance)) %>% select(pseudo.r.squared, df.null, logLik, AIC, BIC, df.residual, nobs)) %>% 
      cbind(., tidy(binomial_single_imp, conf.int = TRUE) %>% mutate_if(is.numeric, round, 7) %>% select(estimate) %>% rename(importance = estimate)),
    tidy(binomial_countries, conf.int = TRUE) %>% mutate_if(is.numeric, round, 7) %>% mutate(distribution = 'binomial', predictors = 'k_fe', dv_var = dv_var, dv_label = dv_label, dv_correl = dv_correl) %>%
      bind_cols(glance(binomial_countries) %>% mutate(pseudo.r.squared = 1 - (deviance / null.deviance)) %>% select(pseudo.r.squared, df.null, logLik, AIC, BIC, df.residual, nobs)) %>%
      cbind(., tidy(binomial_countries_imp, conf.int = TRUE) %>% mutate_if(is.numeric, round, 7) %>% select(estimate) %>% rename(importance = estimate)),
    tidy(binomial_controls, conf.int = TRUE) %>% mutate_if(is.numeric, round, 7) %>% mutate(distribution = 'binomial', predictors = 'k_controls_fe', dv_var = dv_var, dv_label = dv_label, dv_correl = dv_correl) %>%
      bind_cols(glance(binomial_controls) %>% mutate(pseudo.r.squared = 1 - (deviance / null.deviance)) %>% select(pseudo.r.squared, df.null, logLik, AIC, BIC, df.residual, nobs)) %>%
      cbind(., tidy(binomial_controls_imp, conf.int = TRUE) %>% mutate_if(is.numeric, round, 7) %>% select(estimate) %>% rename(importance = estimate)),
    
    tidy(beta_single, conf.int = TRUE) %>% mutate_if(is.numeric, round, 7) %>% select(-one_of('component')) %>% mutate(distribution = 'beta', predictors = 'k', dv_var = dv_var, dv_label = dv_label, dv_correl = dv_correl) %>%
      bind_cols(glance(beta_single) %>% select(pseudo.r.squared, df.null, logLik, AIC, BIC, df.residual, nobs)) %>%
      cbind(., tidy(beta_single_imp, conf.int = TRUE) %>% mutate_if(is.numeric, round, 7) %>% select(estimate) %>% rename(importance = estimate)),
    tidy(beta_countries, conf.int = TRUE) %>% mutate_if(is.numeric, round, 7) %>% select(-one_of('component')) %>% mutate(distribution = 'beta', predictors = 'k_fe', dv_var = dv_var, dv_label = dv_label, dv_correl = dv_correl) %>%
      bind_cols(glance(beta_countries) %>% select(pseudo.r.squared, df.null, logLik, AIC, BIC, df.residual, nobs)) %>%
      cbind(., tidy(beta_countries_imp, conf.int = TRUE) %>% mutate_if(is.numeric, round, 7) %>% select(estimate) %>% rename(importance = estimate)),
    tidy(beta_controls, conf.int = TRUE) %>% mutate_if(is.numeric, round, 7) %>% select(-one_of('component')) %>% mutate(distribution = 'beta', predictors = 'k_controls_fe', dv_var = dv_var, dv_label = dv_label, dv_correl = dv_correl) %>%
      bind_cols(glance(beta_controls) %>% select(pseudo.r.squared, df.null, logLik, AIC, BIC, df.residual, nobs)) %>%
      cbind(., tidy(beta_controls_imp, conf.int = TRUE) %>% mutate_if(is.numeric, round, 7) %>% select(estimate) %>% rename(importance = estimate)),
    
    tidy(quasibinomial_single, conf.int = TRUE) %>% mutate_if(is.numeric, round, 7) %>% mutate(distribution = 'quasibinomial', predictors = 'k', dv_var = dv_var, dv_label = dv_label, dv_correl = dv_correl) %>%
      bind_cols(glance(quasibinomial_single) %>% mutate(pseudo.r.squared = 1 - (deviance / null.deviance)) %>% select(pseudo.r.squared, df.null, logLik, AIC, BIC, df.residual, nobs)) %>%
      cbind(., tidy(quasibinomial_single_imp, conf.int = TRUE) %>% mutate_if(is.numeric, round, 7) %>% select(estimate) %>% rename(importance = estimate)),
    tidy(quasibinomial_countries, conf.int = TRUE) %>% mutate_if(is.numeric, round, 7) %>% mutate(distribution = 'quasibinomial', predictors = 'k_fe', dv_var = dv_var, dv_label = dv_label, dv_correl = dv_correl) %>%
      bind_cols(glance(quasibinomial_countries) %>% mutate(pseudo.r.squared = 1 - (deviance / null.deviance)) %>% select(pseudo.r.squared, df.null, logLik, AIC, BIC, df.residual, nobs)) %>%
      cbind(., tidy(quasibinomial_countries_imp, conf.int = TRUE) %>% mutate_if(is.numeric, round, 7) %>% select(estimate) %>% rename(importance = estimate)),
    tidy(quasibinomial_controls, conf.int = TRUE) %>% mutate_if(is.numeric, round, 7) %>% mutate(distribution = 'quasibinomial', predictors = 'k_controls_fe', dv_var = dv_var, dv_label = dv_label, dv_correl = dv_correl) %>%
      bind_cols(glance(quasibinomial_controls) %>% mutate(pseudo.r.squared = 1 - (deviance / null.deviance)) %>% select(pseudo.r.squared, df.null, logLik, AIC, BIC, df.residual, nobs)) %>%
      cbind(., tidy(quasibinomial_controls_imp, conf.int = TRUE) %>% mutate_if(is.numeric, round, 7) %>% select(estimate) %>% rename(importance = estimate))
  ) # %>% filter(term == 'k_complexity_average')
  
  reg_output <- reg_output %>%
    mutate(estimate_log_odds = comma(exp(estimate))) 
  
  if (exists("combined_reg_output")) {
    combined_reg_output <- rbind(combined_reg_output, reg_output)
  } else {
    combined_reg_output <- reg_output
  }
  
  # reg_output_trunc <- reg_output %>% filter(term == 'k_complexity_average')
  pred_output <- reg_data_i %>%
    select(SurveyYear, SurveyId, REG_ID, DHSREGEN, CNTRYNAMEE, dv, dv_numer, dv_denom) %>%
    mutate(dv_var = dv_var, dv_label = dv_label, dv_correl = dv_correl) %>%
    bind_cols(predict(binomial_single, reg_data_i) %>% rename(pred_binomial_k = .pred),
              predict(binomial_countries, reg_data_i) %>% rename(pred_binomial_k_fe = .pred),
              predict(binomial_controls, reg_data_i) %>% rename(pred_binomial_k_fe_controls = .pred),
              predict(quasibinomial_single, reg_data_i)  %>% rename(pred_quasibinomial_k = .pred),
              predict(quasibinomial_countries, reg_data_i)  %>% rename(pred_quasibinomial_k_fe = .pred),
              predict(quasibinomial_controls, reg_data_i)  %>% rename(pred_quasibinomial_k_fe_controls = .pred),
              data.frame(.pred = predict(beta_single, reg_data_i ) %>% as.vector()) %>% rename(pred_beta_k = .pred),
              data.frame(.pred = predict(beta_countries, reg_data_i ) %>% as.vector()) %>% rename(pred_beta_k_fe = .pred),
              data.frame(.pred = predict(beta_controls, reg_data_i ) %>% as.vector()) %>% rename(pred_beta_k_fe_controls = .pred))

  if (exists("combined_pred_output")) {
    combined_pred_output <- rbind(combined_pred_output, pred_output)
  } else {
    combined_pred_output <- pred_output
  }
}

combined_reg_output <- combined_reg_output %>%
  mutate(expected_sign = case_when(term == 'k_complexity_average' ~ 
                                     sign(estimate)==sign(dv_correl))) %>%
  relocate(distribution, predictors, dv_var, dv_label, dv_correl, term, estimate, estimate_log_odds, importance, p.value, expected_sign,  pseudo.r.squared)

write_excel_csv(combined_pred_output, paste0(wd_path,'/data/dhs-analysis/data/predictions_table.csv'))
write_excel_csv(combined_reg_output , paste0(wd_path,'/data/dhs-analysis/data/regression_table.csv'))

rm(reg_data_i, reg_output, pred_output)
rm(binomial_controls, binomial_countries, binomial_single, binomial_model, 
   beta_single, beta_countries, beta_controls,
   quasibinomial_controls, quasibinomial_countries, quasibinomial_single, quasibinomial_model)

#rm(reg_data_i, reg_output, combined_reg_output, pred_output, combined_pred_output)

pred_summary <- combined_pred_output %>%
  mutate(pred_binomial_k = pred_binomial_k * dv_denom,
         pred_beta_k = pred_beta_k * dv_denom,
         pred_quasibinomial_k = pred_quasibinomial_k * dv_denom) %>%
  group_by(CNTRYNAMEE, dv_var, dv_label, dv_correl) %>%
  summarize_at(vars(dv_numer, dv_denom, 
                    pred_binomial_k, pred_beta_k, pred_quasibinomial_k), list(sum)) %>%
  ungroup() %>%
  mutate(pred_binomial_k = pred_binomial_k / dv_denom,
         pred_beta_k = pred_beta_k / dv_denom,
         pred_quasibinomial_k = pred_quasibinomial_k / dv_denom) %>%
  mutate(dv = dv_numer / dv_denom, 
         resid_binomial_k = abs(dv - pred_binomial_k),
         resid_beta_k = abs(dv - pred_beta_k),
         resid_quasibinomial_k = abs(dv - pred_quasibinomial_k)) 

#combined_pred_output 'predictions_table'
#combined_reg_output 'regression_table'

# binom.test(x = sum(reg_data_i$dv_numer), n = sum(reg_data_i$dv_denom)) %>% tidy()%>% mutate_if(is.numeric, round, 7) 
# one unit increase in the predictor X leads to an increase (or decrease) in the odds (probability an event will occur/1-probability event will occur) by a factor of eβ where β is the regression coefficient for X.
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5860877/#:~:text=The%20logistic%20regression%20model%20is,of%20a%20logit%20link%20function.

# PCA Prep ----------------------------------------------------------------

sections <- c("SurveyYear", "SurveyId", "REG_ID", "REGCODE", "DHSREGEN", "CNTRYNAMEE", "ISO")
complexity_features <- c('k_complexity_average', 'street_density_ratio_km_to_km2', "landscan_population_un_density_hectare", "building_to_block_area_ratio", "average_building_area_m2", "region_urban_share", "region_nonurban_share", "population_k_1", "population_k_2", "population_k_3", "population_k_4", "population_k_5", "population_k_6", "population_k_7", "population_k_8", "population_k_9", "population_k_10plus", "population_off_network", "bldg_area_count_bin_01_0.50__log10_3.2", "bldg_area_count_bin_02_0.75__log10_5.6", "bldg_area_count_bin_03_1.00__log10_10", "bldg_area_count_bin_04_1.25__log10_17.8", "bldg_area_count_bin_05_1.50__log10_31.6", "bldg_area_count_bin_06_1.75__log10_56.2", "bldg_area_count_bin_07_2.00__log10_100", "bldg_area_count_bin_08_2.25__log10_177.8", "bldg_area_count_bin_09_2.50__log10_316.2", "bldg_area_count_bin_10_2.75__log10_562.3", "bldg_area_count_bin_11_3.00__log10_1000", "bldg_area_count_bin_12_3.25__log10_1778.3")
dhs_features <- cor_dhs %>% select(column_var) %>% pull()

dhs_latex_labels <- list(
  indicatorid = c("EM_OCCP_M_AGR", "EM_OCCP_M_MNS", "EM_OCCP_M_MNU", "EM_OCCP_M_PRO", "EM_OCCP_W_AGR", "EM_OCCP_W_DOM", "EM_OCCP_W_MNU", "EM_OCCP_W_PRO", "HC_WIXQ_P_LOW", "HC_WIXQ_P_2ND", "HC_WIXQ_P_MID", "HC_WIXQ_P_4TH", "HC_WIXQ_P_HGH", "HC_WIXQ_P_GNI", "HC_HEFF_H_CMP", "HC_HEFF_H_FRG", "HC_HEFF_H_MPH", "HC_HEFF_H_TLV", "HC_TRNS_H_CAR", "ED_EDAT_B_MYR", "ED_EDAT_M_MYR", "ED_EDAT_W_MYR", "ED_EDAT_B_NED", "ED_EDAT_B_SEC", "ED_EDAT_B_HGH", "ED_NARP_B_BTH", "ED_NARS_B_BTH", "ED_GARS_B_GPI", "ED_LITR_W_LIT", "ED_LITY_W_LIT", "ED_LITY_W_NRD", "ED_NARP_W_FEM", "ED_NARS_W_FEM", "ED_EDUC_W_SEH", "ED_EDAT_W_NED", "ED_EDAT_W_SEC", "ED_EDAT_W_HGH", "ED_MDIA_W_3MD", "ED_MDIA_W_N3M", "CO_MOBB_W_BNK", "CO_MOBB_W_MOB", "AN_NUTS_W_THN", "FP_NADM_W_MNT", "RH_DELP_C_DHF", "CM_ECMR_C_CMR", "CM_ECMR_C_IMR", "CM_ECMR_C_PNR", "CM_ECMR_C_U5M", "CH_VACC_C_BAS", "CN_IYCF_C_4FA", "CN_NUTS_C_HA2", "CN_NUTS_C_WA2", "CN_NUTS_C_WH2", "CN_ANMC_C_ANY", "WS_SRCE_P_BAS", "WS_SRCE_P_IMP", "WS_SRCE_P_IOP", "WS_SRCE_P_LTD", "WS_SRCE_P_NIM", "WS_SRCE_P_PIP", "WS_TIME_P_L30", "WS_TIME_P_M30", "WS_TIME_P_ONP", "WS_TLOC_P_DWL", "WS_TLET_P_BAS", "WS_TLET_P_IMP", "WS_TLET_P_NFC", "WS_HNDW_P_BAS", "WS_HNDW_P_SOP", "HC_PPRM_H_MNP", "HC_OLDR_H_3GN", "HC_PPRM_H_12P", "HC_PPRM_H_34P", "HC_PPRM_H_56P", "HC_PPRM_H_7PP", "HC_CKPL_P_HSE", "HC_CKFL_P_CLN", "HC_CKFL_P_SLD", "HC_ELEC_P_ELC", "HC_ELEC_P_NEL", "HC_FLRM_P_ETH", "HC_FLRM_P_NAT"),
  latex_label = c("% of men employed in agriculture", "% of men employed in skilled manual labor", "% of men employed in unskilled manual labor", "% of men employed in prof., technical, mgmt.", "% of women employed in agriculture", "% of women employed in domestic labor", "% of women employed in unskilled manual labor", "% of women employed in prof., technical, mgmt.", "% in the lowest wealth quintile", "% in the second wealth quintile", "% in the middle wealth quintile", "% in the fourth wealth quintile", "% in the highest wealth quintile", "Wealth index Gini coefficient", "% of households possessing a computer", "% of households possessing a refrigerator", "% of households possessing a mobile telephone", "% of households possessing a television", "% of households possessing a private car", "Median years of education (both sexes)", "Median years of education (males)", "Median years of education (females)", "% of age 6+ with no education", "% of age 6+ who attended secondary school", "% of age 6+ who attended higher education", "Net primary school attendance rate", "Net secondary school attendance rate", "Gross parity index for gross secondary school", "% of women who are literate", "% of young women who are literate", "% of young women who cannot read at all", "Net primary school female attendance rate", "Net secondary school female attendance rate", "% of women with secondary or higher education", "% of females age 6+ with no education", "% of females age 6+ who attended secondary school", "% of females age 6+ who attended higher education", "% of women w/weekly access to newspaper, TV, radio", "% of women with no access to mass media", "% of women who have a bank account", "% of women who own a mobile phone", "% of women who are thin with sub-18.5 BMI", "% of married women met need for family planning", "% of live births delivered at a health facility", "Child mortality rate", "Infant mortality rate", "Postneonatal mortality rate", "Under-five mortality rate", "% of 12-23 month old who received all 8 basic vaccinations", "% of children age 6-23 months fed 5+ food groups", "% of children stunted (below -2 SD of height for age)", "% of children underweight (below -2 SD of weight for age)", "% of children wasted (below -2 SD of weight for height)", "% of children under age 5 classified as having any anemia", "% of population with basic water service", "% of population using an improved water source", "% of population with improved water source on premises", "% of population with limited water service", "% of population using an unimproved water source", "% of population using water piped into dwelling", "% of population with water under 30 min. round trip", "% of population with water over 30 min. round trip", "% of population with water on the premises", "% of population with sanitation facility in own dwelling", "% of population with basic sanitation service", "% of population with an improved sanitation facility", "% of population using open defecation", "% of population with soap & water for handwashing", "% of population with soap available for handwashing", "Mean persons per sleeping room", "% of households with 3 generations", "% of households with 1-2 persons per sleeping room", "% of households with 3-4 persons per sleeping room", "% of households with 5-6 persons per sleeping room", "% of households with 7 + persons per sleeping room", "% of population cooking in the house", "% of population using clean fuel for cooking", "% of population using solid fuel for cooking", "% of population with electricity", "% of population with no electricity", "% of population with earth/sand floors", "% of population with natural floors")
) %>% as.data.frame()

subnat_pca <- subnat_all_wide_k %>% st_drop_geometry() %>%
  mutate(landscan_population_un_density_hectare = landscan_population_un/block_hectares,
         building_to_block_area_ratio = replace_na(building_area_m2/block_area_m2,0),
         average_building_area_m2 = replace_na(building_area_m2/building_count,0),
         region_total = region_core_urban + region_peripheral_urban + region_peri_urban + region_non_urban,
         region_urban = region_core_urban + region_peripheral_urban, 
         region_urban_share = region_urban/region_total,
         region_nonurban_share =  region_non_urban/region_total) %>%
  mutate(across(all_of(c('population_k_1', 'population_k_2', 'population_k_3', 'population_k_4', 'population_k_5', 'population_k_6', 'population_k_7', 'population_k_8', 'population_k_9', 'population_k_10plus', 'population_off_network')),
         .fns = ~ .x / landscan_population_un)) %>%
  mutate(across(all_of(c("bldg_area_count_bin_01_0.50__log10_3.2", "bldg_area_count_bin_02_0.75__log10_5.6", "bldg_area_count_bin_03_1.00__log10_10", "bldg_area_count_bin_04_1.25__log10_17.8", "bldg_area_count_bin_05_1.50__log10_31.6", "bldg_area_count_bin_06_1.75__log10_56.2", "bldg_area_count_bin_07_2.00__log10_100", "bldg_area_count_bin_08_2.25__log10_177.8", "bldg_area_count_bin_09_2.50__log10_316.2", "bldg_area_count_bin_10_2.75__log10_562.3", "bldg_area_count_bin_11_3.00__log10_1000", "bldg_area_count_bin_12_3.25__log10_1778.3", "bldg_area_count_bin_13_3.50__log10_3162.3", "bldg_area_count_bin_14_3.75__log10_5623.4", "bldg_area_count_bin_15_4.00__log10_10000")),
         .fns = ~ .x / building_count)) %>%
  select(all_of(c(sections, complexity_features, 'landscan_population_un')),
         all_of(dhs_features)) %>%
  mutate(row_na = rowSums(is.na(select(., all_of(dhs_features))))) %>%
  group_by(CNTRYNAMEE, REGCODE) %>%
  mutate(rank_recent = row_number(desc(SurveyYear)),
         rank_na = row_number(-desc(row_na))) %>%
  ungroup() %>% 
  #filter(rank_recent == 1) %>%
  #filter(rank_na == 1) %>%
  select(-one_of(c('rank_recent','rank_na','row_na'))) 

sapply(subnat_pca , function(X) sum(is.na(X)))

pca_missing <- colSums(is.na(subnat_pca)) %>% as.data.frame() %>% tibble::rownames_to_column(., 'column_var' ) %>% rename(missing_count = 2)
pca_notmissing <- colSums(!is.na(subnat_pca)) %>% as.data.frame() %>% tibble::rownames_to_column(., 'column_var' ) %>% rename(nonmissing_count = 2)

exclude_from_pca_list <- pca_missing %>% 
  filter(missing_count > 120) %>% select(column_var) %>% pull() 
  #filter(missing_count >= 70) %>% select(column_var) %>% pull() 

subnat_pca <- subnat_pca %>% 
  select(-one_of(exclude_from_pca_list,
                 dhs_dict_df %>% filter(household_duplicate == 1) %>% select(indicatorid) %>% pull())
         ) %>%
  drop_na()

pca_in <-  subnat_pca %>% 
  select(-one_of(sections,
                 'landscan_population_un',
                 complexity_features)) 

(num_dhs_indicators <- length(names(pca_in)))
nrow(pca_in)

# List of PCA vars:
# "EM_OCCP_M_AGR", "EM_OCCP_M_MNS", "EM_OCCP_M_PRO", "EM_OCCP_W_AGR", "EM_OCCP_W_PRO", "HC_WIXQ_P_LOW", "HC_WIXQ_P_2ND", "HC_WIXQ_P_MID", "HC_WIXQ_P_4TH", "HC_WIXQ_P_HGH", "HC_WIXQ_P_GNI", "HC_HEFF_H_FRG", "HC_HEFF_H_MPH", "HC_HEFF_H_TLV", "HC_TRNS_H_CAR", "ED_EDAT_B_MYR", "ED_EDAT_M_MYR", "ED_EDAT_W_MYR", "ED_EDAT_B_NED", "ED_EDAT_B_SEC", "ED_EDAT_B_HGH", "ED_LITR_W_LIT", "ED_LITY_W_LIT", "ED_LITY_W_NRD", "ED_EDUC_W_SEH", "ED_EDAT_W_NED", "ED_EDAT_W_SEC", "ED_EDAT_W_HGH", "ED_MDIA_W_3MD", "ED_MDIA_W_N3M", "FP_NADM_W_MNT", "RH_DELP_C_DHF", "CM_ECMR_C_CMR", "CM_ECMR_C_IMR", "CM_ECMR_C_PNR", "CM_ECMR_C_U5M", "CH_VACC_C_BAS", "CN_IYCF_C_4FA", "CN_NUTS_C_HA2", "CN_NUTS_C_WA2", "CN_NUTS_C_WH2", "CN_ANMC_C_ANY", "WS_TLET_P_BAS", "WS_TLET_P_IMP", "WS_TLET_P_NFC", "WS_SRCE_P_BAS", "WS_SRCE_P_IMP", "WS_SRCE_P_IOP", "WS_SRCE_P_LTD", "WS_SRCE_P_NIM", "WS_SRCE_P_PIP", "WS_TIME_P_L30", "WS_TIME_P_M30", "WS_TIME_P_ONP", "HC_PPRM_H_MNP", "HC_OLDR_H_3GN", "HC_PPRM_H_12P", "HC_PPRM_H_34P", "HC_PPRM_H_56P", "HC_PPRM_H_7PP", "HC_CKPL_P_HSE", "HC_CKFL_P_CLN", "HC_CKFL_P_SLD", "HC_ELEC_P_ELC", "HC_ELEC_P_NEL", "HC_FLRM_P_ETH", "HC_FLRM_P_NAT"

# ISO Table ---------------------------------------------------------------

iso_table <- subnat_pca %>% rename(Country = CNTRYNAMEE) %>% select(ISO, Country) %>% distinct() %>% arrange(ISO) %>%
  mutate(Country = gsub(pattern = 'Congo Democratic Republic', replacement= 'DR Congo', x = Country))

# Select PCA input variables ----------------------------------------------

pca_out <- prcomp(pca_in, center = TRUE, scale. = TRUE)
screeplot(pca_out, npcs = 70, type = "lines")

subnat_clusters <- merge(subnat_pca, as.data.frame(pca_out[["x"]]), by = 0, all = TRUE) %>% select(-one_of(c('Row.names'))) #%>%
  #mutate(#HC_WIXQ_BOTTOM60 = (HC_WIXQ_P_LOW + HC_WIXQ_P_2ND + HC_WIXQ_P_MID)/(HC_WIXQ_P_LOW + HC_WIXQ_P_2ND + HC_WIXQ_P_MID + HC_WIXQ_P_4TH+HC_WIXQ_P_HGH),
  #  HC_WIXQ_BOTTOM80 = (HC_WIXQ_P_LOW + HC_WIXQ_P_2ND + HC_WIXQ_P_MID + HC_WIXQ_P_4TH)/(HC_WIXQ_P_LOW + HC_WIXQ_P_2ND + HC_WIXQ_P_MID + HC_WIXQ_P_4TH+HC_WIXQ_P_HGH))

# pca_in_econ <- subnat_pca %>% select(any_of(dhs_dict_df %>% filter(category == "Economic Wellbeing") %>% select(indicatorid) %>% pull())) %>% 
#   select(-any_of(exclude_from_pca_list)) %>% drop_na() %>%
#   prcomp(., center = TRUE, scale. = TRUE) %>% pluck(5) %>% as.data.frame() %>% select(PC1) %>% rename(pc1_econ = PC1)
# pca_in_educ <- subnat_pca %>% select(any_of(dhs_dict_df %>% filter(category == "Education and Literacy") %>% select(indicatorid) %>% pull())) %>% 
#   select(-any_of(exclude_from_pca_list)) %>% drop_na() %>%
#   prcomp(., center = TRUE, scale. = TRUE) %>% pluck(5) %>% as.data.frame() %>% select(PC1) %>% rename(pc1_educ = PC1)
# pca_in_health <- subnat_pca %>% select(any_of(dhs_dict_df %>% filter(category == "Children and Women's Health") %>% select(indicatorid) %>% pull())) %>% 
#   select(-any_of(exclude_from_pca_list)) %>% drop_na() %>%
#   prcomp(., center = TRUE, scale. = TRUE) %>% pluck(5) %>% as.data.frame() %>% select(PC1) %>% rename(pc1_health = PC1)
# pca_in_wash <- subnat_pca %>% select(any_of(dhs_dict_df %>% filter(category == "Water, Sanitation and Hygiene") %>% select(indicatorid) %>% pull())) %>%
#   select(-any_of(exclude_from_pca_list)) %>% drop_na() %>%
#   prcomp(., center = TRUE, scale. = TRUE) %>% pluck(5) %>% as.data.frame() %>% select(PC1) %>% rename(pc1_wash = PC1)
# pca_in_house <- subnat_pca %>% select(any_of(dhs_dict_df %>% filter(category == "Household Characteristics") %>% select(indicatorid) %>% pull())) %>% 
#   select(-any_of(exclude_from_pca_list)) %>% drop_na() %>%
#   prcomp(., center = TRUE, scale. = TRUE) %>% pluck(5) %>% as.data.frame() %>% select(PC1) %>% rename(pc1_house = PC1)

# PCA Reg -----------------------------------------------------------------

pca_reg_data <- subnat_clusters %>% 
  select(SurveyId, REG_ID, PC1) %>% 
  left_join(., reg_data, by = c('SurveyId' = 'SurveyId', 'REG_ID'='REG_ID')) %>%
  mutate(country = paste0(tolower(gsub("\\s+|\\.|'|\\/", "_", CNTRYNAMEE))))

pca_reg_data <-  pca_reg_data %>%
  cbind(., scale( pca_reg_data %>% 
                    select(all_of(c(specifications))), center = TRUE, scale = TRUE) %>% as.data.frame() %>% 
          rename_with(., .fn = ~ paste0("norm_", .x),
                      .cols = everything()))

pca_country_dummies <- pca_reg_data %>% 
  arrange(SurveyYear) %>% 
  select(country) %>% 
  distinct() %>% 
  mutate(keep_countries = row_number()) 

country_fixed_effects <- pca_country_dummies %>% filter(keep_countries > 1) %>% select(country) %>% mutate_at(vars(country), list(as.character)) %>% mutate(country = paste0('country_',country)) %>% pull() 
country_dummies_drop <- pca_country_dummies %>% filter(keep_countries == 1) %>% select(country) %>% mutate_at(vars(country), list(as.character)) %>% mutate(country = paste0('country_',country)) %>% pull() 

pca_reg_data <- pca_reg_data %>% 
  recipe( ~ .) %>% step_dummy(country, one_hot = TRUE)  %>% prep() %>% bake(NULL) %>% select(-one_of(country_dummies_drop)) %>% 
  #mutate(dummy = 1) %>% tidyr::spread(country, dummy, fill = 0) %>% select(-one_of(country_dummies_drop)) %>%
  distinct() 

cor_pc1_k <- rcorr(pca_reg_data %>% 
        select(PC1, k_complexity_average) %>% as.matrix(),
      type=c('spearman')) %>% tidy() %>% select(estimate, n, p.value) %>%
  mutate(col = 'k_complexity_average')

cor_pc1_street <- rcorr(pca_reg_data %>% 
        select(PC1, street_density_ratio_km_to_km2) %>% as.matrix(),
      type=c('spearman')) %>% tidy() %>% select(estimate, n, p.value) %>%
  mutate(col = 'street_density_ratio_km_to_km2')


# Without street density --------------------------------------------------

lm_model <- linear_reg()
linear_reg_single <- lm_model %>% fit(PC1 ~ k_complexity_average,  data = pca_reg_data)
linear_reg_countries <- lm_model %>% fit(PC1 ~ ., data = pca_reg_data %>% select_at(all_of(c('PC1', 'k_complexity_average', country_fixed_effects))))
linear_reg_controls <- lm_model %>%  fit(PC1 ~ ., data = pca_reg_data %>% select_at(all_of(c('PC1', specifications, country_fixed_effects))))

linear_reg_single_imp <- lm_model %>% fit(PC1 ~ norm_k_complexity_average,  data = pca_reg_data)
linear_reg_countries_imp <-  lm_model %>% fit(PC1 ~ ., data = pca_reg_data %>% select_at(all_of(c('PC1', 'norm_k_complexity_average', country_fixed_effects))))
linear_reg_controls_imp <- lm_model %>%  fit(PC1 ~ ., data = pca_reg_data %>% select_at(all_of(c('PC1', purrr::map_chr(specifications, ~ paste0("norm_", .)), country_fixed_effects))))  

reg_pca_output <- rbind(tidy(linear_reg_single, conf.int = TRUE) %>% mutate_if(is.numeric, round, 7) %>% 
                          mutate(distribution = 'linear', predictors = 'k', dv_var = 'PC1', dv_label = 'Principal Component 1') %>%
                          cbind(.,glance(linear_reg_single)) %>%
                          cbind(., tidy(linear_reg_single_imp, conf.int = TRUE) %>% mutate_if(is.numeric, round, 7) %>% select(estimate) %>% rename(importance = estimate)),
                        tidy(linear_reg_countries, conf.int = TRUE) %>% mutate_if(is.numeric, round, 7) %>% 
                          mutate(distribution = 'linear', predictors = 'k_fe', dv_var = 'PC1', dv_label = 'Principal Component 1') %>%
                          cbind(.,glance(linear_reg_countries))  %>%
                          cbind(., tidy(linear_reg_countries_imp, conf.int = TRUE) %>% mutate_if(is.numeric, round, 7) %>% select(estimate) %>% rename(importance = estimate)),
                        tidy(linear_reg_controls, conf.int = TRUE) %>% mutate_if(is.numeric, round, 7) %>% 
                          mutate(distribution = 'linear', predictors = 'k_controls_fe', dv_var = 'PC1', dv_label = 'Principal Component 1') %>%
                          cbind(.,glance(linear_reg_controls)) %>%
                          cbind(., tidy(linear_reg_controls_imp, conf.int = TRUE) %>% mutate_if(is.numeric, round, 7) %>% select(estimate) %>% rename(importance = estimate))
                        )

write_excel_csv(reg_pca_output, paste0(wd_path,'/data/dhs-analysis/data/pc1_regression_table.csv'))

# With street density --------------------------------------------------

lm_model <- linear_reg()
linear_reg_single <- lm_model %>% fit(PC1 ~ k_complexity_average + street_density_ratio_km_to_km2,  data = pca_reg_data)
linear_reg_countries <- lm_model %>% fit(PC1 ~ ., data = pca_reg_data %>% select_at(all_of(c('PC1', 'k_complexity_average', 'street_density_ratio_km_to_km2', country_fixed_effects))))
linear_reg_controls <- lm_model %>%  fit(PC1 ~ ., data = pca_reg_data %>% select_at(all_of(c('PC1', specifications, 'street_density_ratio_km_to_km2', country_fixed_effects))))

linear_reg_single_imp <- lm_model %>% fit(PC1 ~ norm_k_complexity_average + norm_street_density_ratio_km_to_km2,  data = pca_reg_data)
linear_reg_countries_imp <-  lm_model %>% fit(PC1 ~ ., data = pca_reg_data %>% select_at(all_of(c('PC1', 'norm_k_complexity_average', 'norm_street_density_ratio_km_to_km2', country_fixed_effects))))
linear_reg_controls_imp <- lm_model %>%  fit(PC1 ~ ., data = pca_reg_data %>% select_at(all_of(c('PC1', purrr::map_chr(specifications, ~ paste0("norm_", .)), 'norm_street_density_ratio_km_to_km2', country_fixed_effects))))  

reg_pca_output_street_density <- rbind(tidy(linear_reg_single, conf.int = TRUE) %>% mutate_if(is.numeric, round, 7) %>% 
                          mutate(distribution = 'linear', predictors = 'k', dv_var = 'PC1', dv_label = 'Principal Component 1') %>%
                          cbind(.,glance(linear_reg_single)) %>%
                          cbind(., tidy(linear_reg_single_imp, conf.int = TRUE) %>% mutate_if(is.numeric, round, 7) %>% select(estimate) %>% rename(importance = estimate)),
                        tidy(linear_reg_countries, conf.int = TRUE) %>% mutate_if(is.numeric, round, 7) %>% 
                          mutate(distribution = 'linear', predictors = 'k_fe', dv_var = 'PC1', dv_label = 'Principal Component 1') %>%
                          cbind(.,glance(linear_reg_countries))  %>%
                          cbind(., tidy(linear_reg_countries_imp, conf.int = TRUE) %>% mutate_if(is.numeric, round, 7) %>% select(estimate) %>% rename(importance = estimate)),
                        tidy(linear_reg_controls, conf.int = TRUE) %>% mutate_if(is.numeric, round, 7) %>% 
                          mutate(distribution = 'linear', predictors = 'k_controls_fe', dv_var = 'PC1', dv_label = 'Principal Component 1') %>%
                          cbind(.,glance(linear_reg_controls)) %>%
                          cbind(., tidy(linear_reg_controls_imp, conf.int = TRUE) %>% mutate_if(is.numeric, round, 7) %>% select(estimate) %>% rename(importance = estimate))
)

write_excel_csv(reg_pca_output_street_density, paste0(wd_path,'/data/dhs-analysis/data/pc1_regression_table_street_density.csv'))


# Predictions -------------------------------------------------------------

names(pca_reg_data)
pca_reg_data_pred_output <- pca_reg_data %>%
  select(SurveyId, REG_ID, PC1, SurveyYear, DHSREGEN, CNTRYNAMEE, PC1,
         k_complexity_average, street_density_ratio_km_to_km2, region_urban_share, share_building_count_under_31m2, building_to_block_area_ratio, landscan_population_un_density_hectare) %>%
  bind_cols(predict(linear_reg_single, pca_reg_data) %>% rename(pred_single = .pred),
            predict(linear_reg_countries, pca_reg_data) %>% rename(pred_countries = .pred),
            predict(linear_reg_controls, pca_reg_data) %>% rename(pred_controls = .pred)) %>%
  mutate(abs_resid_single = abs(PC1 - pred_single),
         abs_resid_countries = abs(PC1 - pred_countries),
         abs_resid_controls = abs(PC1 - pred_controls),
         resid_single = (PC1 - pred_single),
         resid_countries = (PC1 - pred_countries),
         resid_controls = (PC1 - pred_controls),
         )

pca_resid_summary <- pca_reg_data_pred_output %>%
  group_by(CNTRYNAMEE) %>%
  summarise(
    PC1_mean = mean(PC1),
    pred_single_mean = mean(pred_single),
    pred_countries_mean = mean(pred_countries),
    pred_controls_mean = mean(pred_controls),
    resid_single_mean = mean(resid_single),
    resid_countries_mean = mean(resid_countries),
    resid_controls_mean = mean(resid_controls),
    abs_resid_single_mean = mean(abs_resid_single),
    abs_resid_countries_mean = mean(abs_resid_countries),
    abs_resid_controls_mean = mean(abs_resid_controls)
  ) %>%
  ungroup()
            
pca_resid_corr <- rcorr(pca_reg_data_pred_output %>% 
        select(k_complexity_average, street_density_ratio_km_to_km2, region_urban_share, share_building_count_under_31m2, building_to_block_area_ratio, landscan_population_un_density_hectare,
               resid_single , resid_countries, resid_controls) %>% as.matrix(), type=c('spearman')) %>% 
  tidy() %>%
  filter(column1 %in% c('resid_single','resid_controls','resid_countries'),
         !(column2 %in% c('resid_single','resid_controls','resid_countries'))) %>%
  mutate(estimate_abs = abs(estimate)) %>%
  arrange(column1,estimate_abs )

# write_excel_csv(pca_resid_corr, paste0(wd_path,'/data/dhs-analysis/data/pca_resid_corr.csv'))
# write_excel_csv(pca_reg_data_pred_output, paste0(wd_path,'/data/dhs-analysis/data/pca_region_residuals.csv'))
# write_excel_csv(pca_resid_summary, paste0(wd_path,'/data/dhs-analysis/data/pca_country_residuals.csv'))

writexl::write_xlsx(list('correlations_residuals' = pca_resid_corr,
                         'regional_residuals' = pca_reg_data_pred_output,
                         'coutry_avg_residuals' = pca_resid_summary), 
                    col_names = TRUE, format_headers = TRUE, 
                    path = paste0(wd_path,'/data/dhs-analysis/data/pca_residual_analysis.xlsx'))


# glance(linear_reg_countries_imp )
# DHS component vs block complexity ---------------------------------------

survey_table <- subnat_pca %>% select(CNTRYNAMEE, SurveyYear) %>% rename(country = CNTRYNAMEE) %>% distinct() %>% arrange(SurveyYear) %>% pivot_wider(names_from = 'SurveyYear', values_from = c(SurveyYear))
survey_table <- subnat_all_wide_k %>% st_drop_geometry() %>% select(CNTRYNAMEE, SurveyYear) %>% rename(country = CNTRYNAMEE) %>% distinct() %>% arrange(SurveyYear) %>% 
  mutate_at(vars(SurveyYear), as.character) %>% pivot_wider(names_from = 'SurveyYear', values_from = c(SurveyYear), values_fill = list(SurveyYear = '')) #%>%
  #mutate(year = paste0(`2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, `2018`, `2019`, `2020`, `2021`, collapse = ', '))

screeplot(pca_out, npcs = 10, type = "lines")
pca_loadings <- pca_out$rotation %>% as.data.frame() %>% select(PC1, PC2, PC3, PC4, PC5, PC6) %>% arrange(-PC1) %>%
  tibble::rownames_to_column(., "dhs_variable") %>%
  left_join(., dhs_dict_df, by = c('dhs_variable'='indicatorid'))
pca_loadings %>% as_tibble() %>% print(n = 100)
# pearson correlation between component 1 and the inputs
(pca_var_explained <- summary(pca_out)$importance %>% as.data.frame())

# write_excel_csv(pca_var_explained, paste0(wd_path,'/analysis/','pca_var_explained_34DHS.csv'))

writexl::write_xlsx(list('pca_regression' = reg_pca_output,
                         'pca_regression_street' = reg_pca_output_street_density,
                         'pca_loadings' = pca_loadings, 
                         'pca_var_explained' = pca_var_explained), 
                    col_names = TRUE, format_headers = TRUE, 
                    path = paste0(wd_path,'/data/dhs-analysis/data/pca_var_analysis.xlsx'))

# Latex -------------------------------------------------------------------

latex_table <- cor_dhs %>% 
  select(column_var, category, subcategory, estimate, n, order) %>%
  inner_join(., dhs_latex_labels, by = c('column_var'='indicatorid')) %>%
  left_join(., pca_loadings %>% select(dhs_variable, PC1, PC2), by = c('column_var'='dhs_variable')) %>%
  mutate(estimate  = round(estimate, 4),
         PC1 = round(PC1, 3),
         PC2 = round(PC2, 3)) %>%
  relocate(category, subcategory, latex_label, estimate, n, PC1, PC2, order, column_var) %>%
  mutate(in_pca = case_when(!is.na(PC1) ~ 'Yes', TRUE ~ as.character(''))) %>%
  mutate_at(c('PC1','PC2'), as.character) %>%
  mutate_at(c('PC1','PC2'), ~replace_na(.,"")) %>%
  group_by(category, subcategory) %>%
  mutate(subcat_count = n(),
         subcat_order = row_number(-desc(order))) %>%
  ungroup() %>%
  group_by(category) %>%
  mutate(cat_count = n()) %>%
  ungroup() %>%
  mutate(latex_label = gsub('&','/\\&',latex_label),
         latex_label = gsub('%','\\\\%',latex_label),
         latex_format = paste0("& ",latex_label,' & ',estimate,' & ',n,' & ',in_pca,' & ',PC1,' & ',PC2,' \\\\'),
         latex_format = case_when(subcat_order > 1 ~ paste('& ',latex_format),
                                  TRUE ~ as.character(latex_format))
           )


latex_code_df <- data.frame(latex_format=character())
                            
for (i in unique(latex_table$category)) {

  latex_table_i <- latex_table  %>%
    filter(category == i) 
  
  cat_label <- latex_table_i %>% select(category, cat_count) %>% distinct() %>%
    mutate(cat_label = paste0('\\hline \\multirowcell{',cat_count,'}{\\rotatebox[origin=c]{90}{ ',category,' }} ')) %>%
    select(cat_label) %>% pull()
  #print(cat_label)
  latex_code_df <- rbind(latex_code_df, list(latex_format = c(cat_label)) %>% as.data.frame())
  
  for (j in unique(latex_table_i$subcategory)) {
    latex_table_j <- latex_table_i %>%
      filter(subcategory == j)
    subcat_label <- latex_table_j  %>% select(subcategory, subcat_count) %>% distinct() %>%
      mutate(subcat_label = paste0('\\cline{2-7} & ','\\multirow{',subcat_count,'}{*}{\\rotatebox[origin=c]{0}{',subcategory,'}} ')) %>%
      select(subcat_label) %>% pull()
    #print(subcat_label)
    #print(latex_table_j %>% select(subcat_label))
    
    latex_code_df <- rbind(latex_code_df, list(latex_format = c(subcat_label)) %>% as.data.frame())
  
    latex_code_df <- rbind(latex_code_df, latex_table_j %>% select(latex_format))

    }
}

write_csv(latex_code_df, paste0(wd_path,'/data/dhs-analysis/data/latex_table.csv'))

# 
# Street density vs Block complexity 
# library(ggrepel)
# ggplot() + 
#   geom_point(data = subnat_clusters, 
#              aes(x = log10(k_complexity_average), y = PC1,
#                  color = ISO, size = log10(landscan_population_un)), alpha = .4) +
#   labs(x = 'Block complexity', y = 'PC1', color = 'ISO') + 
#   scale_x_continuous(oob = scales::squish, breaks= c(0, 0.30103, 0.4771213, 0.60206, 0.69897, 0.7781513, 0.845098, 0.90309, 0.9542425, 1,1.041393,1.079181,1.113943,1.146128,1.176091,1.20412,1.230449,1.255273,1.278754,1.30103,1.322219,1.342423,1.361728,1.380211,1.39794, 1.414973, 1.431364, 1.447158, 1.462398, 1.477121), labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30) )+
#   geom_text_repel(data = subnat_clusters, seed = 1, segment.curvature = -0.1, point.padding = 0, box.padding = 0.4, max.iter = 30000, segment.square  = FALSE, segment.inflect = FALSE, min.segment.length = .2, max.overlaps = Inf, force = .5, force_pull = 10, 
#                   aes(x = log10(k_complexity_average), y = PC1, label = paste0(ISO) ), size = 3, vjust =.5, color = '#333333', fontface='bold') +
#   scale_size_continuous(range = c(1,10), labels = label_comma(accuracy = 1L, scale =  0.000001, suffix = "M") ) +
#   theme_classic() + theme(legend.position = 'none') +
# ggplot() + 
#   geom_point(data = subnat_clusters, 
#              aes(x = street_density_ratio_km_to_km2, y = PC1,
#                  color = ISO, size = log10(landscan_population_un)), alpha = .4) +
#   labs(x = 'Street density ratio (km per km2)', y = 'PC1', color = 'ISO') +
#   scale_x_continuous(limits = c(0,1.5)) +
#   #scale_x_continuous(oob = scales::squish,  breaks= c(0, 0.30103, 0.4771213, 0.60206, 0.69897, 0.7781513, 0.845098, 0.90309, 0.9542425, 1,1.041393,1.079181,1.113943,1.146128,1.176091,1.20412,1.230449,1.255273,1.278754,1.30103,1.322219,1.342423,1.361728,1.380211,1.39794, 1.414973, 1.431364, 1.447158, 1.462398, 1.477121),  labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30) )+
#   geom_text_repel(data = subnat_clusters, seed = 1, segment.curvature = -0.1, point.padding = 0, box.padding = 0.4, max.iter = 30000, segment.square  = FALSE, segment.inflect = FALSE, min.segment.length = .2, max.overlaps = Inf, force = .5, force_pull = 10, 
#                   aes(x = street_density_ratio_km_to_km2, y = PC1, label = paste0(ISO) ), size = 3, vjust =.5, color = '#333333', fontface='bold') +
#   scale_size_continuous(range = c(1,10), labels = label_comma(accuracy = 1L, scale =  0.000001, suffix = "M") ) +
#   theme_classic() + theme(legend.position = 'none')

# DHS v PCA Scatter -------------------------------------------------------

# DHS PCA scatter
design3 <- "
  AABBE
  CCDDE
"

(dhs_k_v_pca <- (
  (ggplot(data = subnat_clusters, aes(x = log10(k_complexity_average), y = PC1, color = ED_LITR_W_LIT)) +
     geom_smooth(method = "lm", formula =  'y ~ (x)', se = FALSE, color = '#333333') +
     geom_point(aes(size = landscan_population_un_density_hectare), alpha = .85) + 
     scale_color_viridis(label = scales::percent, limits = c(0,1), name = 'Percent of\nDHS indicator   ') +
     scale_size_continuous(range = c(4,15), breaks = c(10, 40, 160), name = 'Population\nper hectare') +
     scale_x_continuous(expand = c(0,0.01), oob = scales::squish, limits = c(0.3, 1.477121), breaks = c(0.30103, 0.69897, 1, 1.30103, 1.477121), labels = c(2, 5, 10, 20, 30)) +
     labs(subtitle ='Percent of women\nwho are literate', x = '', y = '') +
     geom_text(data = subnat_clusters, aes(x = log10(k_complexity_average), y = PC1, label = ISO), color = 'white', check_overlap = TRUE, fontface = 'bold', size = 2) +
     theme_classic() + theme(plot.subtitle = element_text(face = 'bold', hjust = .5),
                             legend.title = element_text(face = 'bold'),
                             legend.key.width = unit(1.4, 'cm'),
                             axis.text = element_text(size = 13))) +
    (ggplot(data = subnat_clusters, aes(x = log10(k_complexity_average), y = PC1, color = WS_SRCE_P_IOP)) +
       geom_smooth(method = "lm", formula =  'y ~ (x)', se = FALSE, color = '#333333') +
       geom_point(aes(size = landscan_population_un_density_hectare), alpha = .85) + 
       scale_color_viridis(label = scales::percent, limits = c(0,1), name = 'Percent of\nDHS indicator   ') +
       scale_size_continuous(range = c(4,15), breaks = c(10, 40, 160), name = 'Population\nper hectare') +
       scale_x_continuous(expand = c(0,0.01), oob = scales::squish, limits = c(0.3, 1.477121), breaks = c(0.30103, 0.69897, 1, 1.30103, 1.477121), labels = c(2, 5, 10, 20, 30)) +
       labs(subtitle ='Percent of population with\nan improved water source on the premises', x = '', y = '') +
       geom_text(data = subnat_clusters, aes(x = log10(k_complexity_average), y = PC1, label = ISO), color = 'white', check_overlap = TRUE, fontface = 'bold', size = 2) +
       theme_classic() + theme(plot.subtitle = element_text(face = 'bold', hjust = .5),
                               legend.title = element_text(face = 'bold'),
                               legend.key.width = unit(1.4, 'cm'),
                               axis.text = element_text(size = 13))) +
    (ggplot(data = subnat_clusters, aes(x = log10(k_complexity_average), y = PC1, color = WS_TLET_P_IMP)) +
       geom_smooth(method = "lm", formula =  'y ~ (x)', se = FALSE, color = '#333333') +
       geom_point(aes(size = landscan_population_un_density_hectare ), alpha = .85) + 
       scale_color_viridis(label = scales::percent, limits = c(0,1), name = 'Percent of\nDHS indicator   ') +
       scale_size_continuous(range = c(4,15), breaks = c(10, 40, 160), name = 'Population\nper hectare') +
       scale_x_continuous(expand = c(0,0.01), oob = scales::squish, limits = c(0.3, 1.477121), breaks = c(0.30103, 0.69897, 1, 1.30103, 1.477121), labels = c(2, 5, 10, 20, 30)) +
       labs(subtitle = 'Percentage of population with\nan improved sanitation facility', x = '', y = '') +
       geom_text(data = subnat_clusters, aes(x = log10(k_complexity_average), y = PC1, label = ISO), color = 'white', check_overlap = TRUE, fontface = 'bold', size = 2) +
       theme_classic() + theme(plot.subtitle = element_text(face = 'bold', hjust = .5),
                               legend.title = element_text(face = 'bold'),
                               legend.key.width = unit(1.4, 'cm'),
                               axis.text = element_text(size = 13))) +
    (ggplot(data = subnat_clusters, aes(x = log10(k_complexity_average), y = PC1, color = HC_WIXQ_P_HGH)) +
       geom_smooth(method = "lm", formula =  'y ~ (x)', se = FALSE, color = '#333333') +
       geom_point(aes(size = landscan_population_un_density_hectare ), alpha = .85) + 
       scale_color_viridis(label = scales::percent,limits = c(0,1), name = 'Percent of\nDHS indicator   ') +
       scale_size_continuous(range = c(4,15), breaks = c(10, 40, 160), name = 'Population\nper hectare') +
       scale_x_continuous(expand = c(0,0.01), oob = scales::squish, limits = c(0.3, 1.477121), breaks = c(0.30103, 0.69897, 1, 1.30103, 1.477121), labels = c(2, 5, 10, 20, 30)) +
       labs(subtitle = 'Percent of de jure population\nin the top 20 percentile of wealth', x = '', y = '') +
       geom_text(data = subnat_clusters, aes(x = log10(k_complexity_average), y = PC1, label = ISO), color = 'white', check_overlap = TRUE, fontface = 'bold', size = 2) +
       theme_classic() + theme(plot.subtitle = element_text(face = 'bold', hjust = .5),
                               legend.title = element_text(face = 'bold'),
                               legend.key.width = unit(1.4, 'cm'),
                               axis.text = element_text(size = 13))) +
    (ggplot() + ggplot2::annotate(geom = "table", x=0, y = 0, label = list(iso_table), size = 4) + theme_void()) + 
    plot_layout(design = design3, guides = 'collect', ncol = 2) & 
    plot_annotation(tag_levels = list(c("A", "B", "C", "D"))) &
    theme(legend.position="bottom",
          plot.tag = element_text(size = 15),
          legend.box.margin = margin(t = 30, r = 0, b = 0, l = 0, unit = "pt"),
          plot.subtitle = element_text(size = 14),
          legend.title = element_text(size = 13),
          legend.text = element_text(size = 13),
          legend.box.just = 'bottom') 
  ))


#
(dhs_k_v_pca_grob <- gridExtra::grid.arrange(patchwork::patchworkGrob(dhs_k_v_pca), 
                        left = text_grob(paste0('PC1 of PCA on DHS indicators'), rot = 90, vjust = 1, size = 15, face = "bold"), 
                        bottom = text_grob("Block complexity", rot = 0, vjust = -9, size = 15, face = "bold") ) )

# subnat_clusters %>% select(ISO) %>% distinct()
# "PCA on the following dimensions: percentage of de jure population with an improved sanitation facility; with an improved water source on the premises; with basic sanitation service; basic water service; earth/sand floors; with electricity; with limited sanitation service; with limited water service; with natural floors; with water on the premises; in each wealth quintile; living in households whose main source of drinking water is an improved source; mean number of persons per sleeping room; and percentage of households with 1 to 2, 3 to 4, 5 to 6, or 7 or more persons per sleeping room; percentage of women who are literate."

ggsave(plot = dhs_k_v_pca_grob, filename = paste0(wd_path,'/data/dhs-analysis/viz/scatter_k_PC1.pdf'), width = 16, height = 12)  
rm(dhs_k_v_pca_grob)
# 4 x 3  ------------------------------------------------------------------

(dhs_k_v_pca_4x3 <- (ggplot(data = subnat_clusters, aes(x = log10(k_complexity_average), y = PC1, color = EM_OCCP_M_AGR)) +
   geom_smooth(method = "lm", formula =  'y ~ (x)', se = FALSE, color = '#333333') +
   geom_point(aes(size = landscan_population_un_density_hectare), alpha = .85) + 
   scale_color_viridis(label = scales::percent, limits = c(0,1), name = 'Percent of\nDHS indicator   ') +
   scale_size_continuous(range = c(4,15), breaks = c(10, 40, 160), name = 'Population\nper hectare') +
   scale_x_continuous(expand = c(0,0.01), oob = scales::squish, limits = c(0.3, 1.477121), breaks = c(0.30103, 0.69897, 1, 1.30103, 1.477121), labels = c(2, 5, 10, 20, 30)) +
   labs(subtitle ='Percent of men employed\nin agriculture', x = '', y = '') +
   geom_text(data = subnat_clusters, aes(x = log10(k_complexity_average), y = PC1, label = ISO), color = 'white', check_overlap = TRUE, fontface = 'bold', size = 2) +
   theme_classic() + theme(plot.subtitle = element_text(face = 'bold', hjust = .5),
                           legend.title = element_text(face = 'bold'),
                           legend.key.width = unit(1.4, 'cm'),
                           axis.text = element_text(size = 13))) +
  (ggplot(data = subnat_clusters %>%
            mutate(bottom60 = HC_WIXQ_P_LOW + HC_WIXQ_P_2ND + HC_WIXQ_P_MID), aes(x = log10(k_complexity_average), y = PC1, color = bottom60)) +
     geom_smooth(method = "lm", formula =  'y ~ (x)', se = FALSE, color = '#333333') +
     geom_point(aes(size = landscan_population_un_density_hectare), alpha = .85) + 
     scale_color_viridis(label = scales::percent, limits = c(0,1), name = 'Percent of\nDHS indicator   ') +
     scale_size_continuous(range = c(4,15), breaks = c(10, 40, 160), name = 'Population\nper hectare') +
     scale_x_continuous(expand = c(0,0.01), oob = scales::squish, limits = c(0.3, 1.477121), breaks = c(0.30103, 0.69897, 1, 1.30103, 1.477121), labels = c(2, 5, 10, 20, 30)) +
     labs(subtitle ='Percent of population\nin bottom 60 percentile of wealth', x = '', y = '') +
     geom_text(data = subnat_clusters, aes(x = log10(k_complexity_average), y = PC1, label = ISO), color = 'white', check_overlap = TRUE, fontface = 'bold', size = 2) +
     theme_classic() + theme(plot.subtitle = element_text(face = 'bold', hjust = .5),
                             legend.title = element_text(face = 'bold'),
                             legend.key.width = unit(1.4, 'cm'),
                             axis.text = element_text(size = 13))) +
  (ggplot(data = subnat_clusters, aes(x = log10(k_complexity_average), y = PC1, color = HC_HEFF_H_FRG)) +
     geom_smooth(method = "lm", formula =  'y ~ (x)', se = FALSE, color = '#333333') +
     geom_point(aes(size = landscan_population_un_density_hectare), alpha = .85) +
     scale_color_viridis(label = scales::percent, limits = c(0,1), name = 'Percent of\nDHS indicator   ') +
     scale_size_continuous(range = c(4,15), breaks = c(10, 40, 160), name = 'Population\nper hectare') +
     scale_x_continuous(expand = c(0,0.01), oob = scales::squish, limits = c(0.3, 1.477121), breaks = c(0.30103, 0.69897, 1, 1.30103, 1.477121), labels = c(2, 5, 10, 20, 30)) +
     labs(subtitle ='Percent of households\npossessing a refrigerator', x = '', y = '') +
     geom_text(data = subnat_clusters, aes(x = log10(k_complexity_average), y = PC1, label = ISO), color = 'white', check_overlap = TRUE, fontface = 'bold', size = 2) +
     theme_classic() + theme(plot.subtitle = element_text(face = 'bold', hjust = .5),
                             legend.title = element_text(face = 'bold'),
                             legend.key.width = unit(1.4, 'cm'),
                             axis.text = element_text(size = 13))) +
  (ggplot(data = subnat_clusters, aes(x = log10(k_complexity_average), y = PC1, color = ED_EDUC_W_SEH)) +
     geom_smooth(method = "lm", formula =  'y ~ (x)', se = FALSE, color = '#333333') +
     geom_point(aes(size = landscan_population_un_density_hectare), alpha = .85) +
     scale_color_viridis(label = scales::percent, limits = c(0,1), name = 'Percent of\nDHS indicator   ') +
     scale_size_continuous(range = c(4,15), breaks = c(10, 40, 160), name = 'Population\nper hectare') +
     scale_x_continuous(expand = c(0,0.01), oob = scales::squish, limits = c(0.3, 1.477121), breaks = c(0.30103, 0.69897, 1, 1.30103, 1.477121), labels = c(2, 5, 10, 20, 30)) +
     labs(subtitle ='Percentage of women with\nsecondary or higher education', x = '', y = '') +
     geom_text(data = subnat_clusters, aes(x = log10(k_complexity_average), y = PC1, label = ISO), color = 'white', check_overlap = TRUE, fontface = 'bold', size = 2) +
     theme_classic() + theme(plot.subtitle = element_text(face = 'bold', hjust = .5),
                             legend.title = element_text(face = 'bold'),
                             legend.key.width = unit(1.4, 'cm'),
                             axis.text = element_text(size = 13))) +
  (ggplot(data = subnat_clusters, aes(x = log10(k_complexity_average), y = PC1, color = ED_EDAT_W_NED)) +
     geom_smooth(method = "lm", formula =  'y ~ (x)', se = FALSE, color = '#333333') +
     geom_point(aes(size = landscan_population_un_density_hectare), alpha = .85) +
     scale_color_viridis(label = scales::percent, limits = c(0,1), name = 'Percent of\nDHS indicator   ') +
     scale_size_continuous(range = c(4,15), breaks = c(10, 40, 160), name = 'Population\nper hectare') +
     scale_x_continuous(expand = c(0,0.01), oob = scales::squish, limits = c(0.3, 1.477121), breaks = c(0.30103, 0.69897, 1, 1.30103, 1.477121), labels = c(2, 5, 10, 20, 30)) +
     labs(subtitle ='Percentage of female population\nage 6+ with no education', x = '', y = '') +
     geom_text(data = subnat_clusters, aes(x = log10(k_complexity_average), y = PC1, label = ISO), color = 'white', check_overlap = TRUE, fontface = 'bold', size = 2) +
     theme_classic() + theme(plot.subtitle = element_text(face = 'bold', hjust = .5),
                             legend.title = element_text(face = 'bold'),
                             legend.key.width = unit(1.4, 'cm'),
                             axis.text = element_text(size = 13))) +
  (ggplot(data = subnat_clusters, aes(x = log10(k_complexity_average), y = PC1, color = ED_MDIA_W_N3M)) +
     geom_smooth(method = "lm", formula =  'y ~ (x)', se = FALSE, color = '#333333') +
     geom_point(aes(size = landscan_population_un_density_hectare), alpha = .85) + 
     scale_color_viridis(label = scales::percent, limits = c(0,1), name = 'Percent of\nDHS indicator   ') +
     scale_size_continuous(range = c(4,15), breaks = c(10, 40, 160), name = 'Population\nper hectare') +
     scale_x_continuous(expand = c(0,0.01), oob = scales::squish, limits = c(0.3, 1.477121), breaks = c(0.30103, 0.69897, 1, 1.30103, 1.477121), labels = c(2, 5, 10, 20, 30)) +
     labs(subtitle ='Percent of women with\nno access to mass media', x = '', y = '') +
     geom_text(data = subnat_clusters, aes(x = log10(k_complexity_average), y = PC1, label = ISO), color = 'white', check_overlap = TRUE, fontface = 'bold', size = 2) +
     theme_classic() + theme(plot.subtitle = element_text(face = 'bold', hjust = .5),
                             legend.title = element_text(face = 'bold'),
                             legend.key.width = unit(1.4, 'cm'),
                             axis.text = element_text(size = 13))) +
  (ggplot(data = subnat_clusters, aes(x = log10(k_complexity_average), y = PC1, color = RH_DELP_C_DHF)) +
     geom_smooth(method = "lm", formula =  'y ~ (x)', se = FALSE, color = '#333333') +
     geom_point(aes(size = landscan_population_un_density_hectare), alpha = .85) + 
     scale_color_viridis(label = scales::percent, limits = c(0,1), name = 'Percent of\nDHS indicator   ') +
     scale_size_continuous(range = c(4,15), breaks = c(10, 40, 160), name = 'Population\nper hectare') +
     scale_x_continuous(expand = c(0,0.01), oob = scales::squish, limits = c(0.3, 1.477121), breaks = c(0.30103, 0.69897, 1, 1.30103, 1.477121), labels = c(2, 5, 10, 20, 30)) +
     labs(subtitle ='Percent of live births\ndelivered at a health facility', x = '', y = '') +
     geom_text(data = subnat_clusters, aes(x = log10(k_complexity_average), y = PC1, label = ISO), color = 'white', check_overlap = TRUE, fontface = 'bold', size = 2) +
     theme_classic() + theme(plot.subtitle = element_text(face = 'bold', hjust = .5),
                             legend.title = element_text(face = 'bold'),
                             legend.key.width = unit(1.4, 'cm'),
                             axis.text = element_text(size = 13))) +
  (ggplot(data = subnat_clusters, aes(x = log10(k_complexity_average), y = PC1, color = CM_ECMR_C_CMR)) +
     geom_smooth(method = "lm", formula =  'y ~ (x)', se = FALSE, color = '#333333') +
     geom_point(aes(size = landscan_population_un_density_hectare), alpha = .85) + 
     scale_color_viridis(label = scales::percent, limits = c(0,1), name = 'Percent of\nDHS indicator   ') +
     scale_size_continuous(range = c(4,15), breaks = c(10, 40, 160), name = 'Population\nper hectare') +
     scale_x_continuous(expand = c(0,0.01), oob = scales::squish, limits = c(0.3, 1.477121), breaks = c(0.30103, 0.69897, 1, 1.30103, 1.477121), labels = c(2, 5, 10, 20, 30)) +
     labs(subtitle ='Child mortality rate\n(ages 1 to 5)', x = '', y = '') +
     geom_text(data = subnat_clusters, aes(x = log10(k_complexity_average), y = PC1, label = ISO), color = 'white', check_overlap = TRUE, fontface = 'bold', size = 2) +
     theme_classic() + theme(plot.subtitle = element_text(face = 'bold', hjust = .5),
                             legend.title = element_text(face = 'bold'),
                             legend.key.width = unit(1.4, 'cm'),
                             axis.text = element_text(size = 13))) +
  (ggplot(data = subnat_clusters, aes(x = log10(k_complexity_average), y = PC1, color = WS_TLET_P_NFC)) +
     geom_smooth(method = "lm", formula =  'y ~ (x)', se = FALSE, color = '#333333') +
     geom_point(aes(size = landscan_population_un_density_hectare), alpha = .85) + 
     scale_color_viridis(label = scales::percent, limits = c(0,1), name = 'Percent of\nDHS indicator   ') +
     scale_size_continuous(range = c(4,15), breaks = c(10, 40, 160), name = 'Population\nper hectare') +
     scale_x_continuous(expand = c(0,0.01), oob = scales::squish, limits = c(0.3, 1.477121), breaks = c(0.30103, 0.69897, 1, 1.30103, 1.477121), labels = c(2, 5, 10, 20, 30)) +
     labs(subtitle ='Percent of population using\nopen defecation (no toilet facility)', x = '', y = '') +
     geom_text(data = subnat_clusters, aes(x = log10(k_complexity_average), y = PC1, label = ISO), color = 'white', check_overlap = TRUE, fontface = 'bold', size = 2) +
     theme_classic() + theme(plot.subtitle = element_text(face = 'bold', hjust = .5),
                             legend.title = element_text(face = 'bold'),
                             legend.key.width = unit(1.4, 'cm'),
                             axis.text = element_text(size = 13))) +
  (ggplot(data = subnat_clusters, aes(x = log10(k_complexity_average), y = PC1, color = WS_TIME_P_ONP)) +
     geom_smooth(method = "lm", formula =  'y ~ (x)', se = FALSE, color = '#333333') +
     geom_point(aes(size = landscan_population_un_density_hectare), alpha = .85) + 
     scale_color_viridis(label = scales::percent, limits = c(0,1), name = 'Percent of\nDHS indicator   ') +
     scale_size_continuous(range = c(4,15), breaks = c(10, 40, 160), name = 'Population\nper hectare') +
     scale_x_continuous(expand = c(0,0.01), oob = scales::squish, limits = c(0.3, 1.477121), breaks = c(0.30103, 0.69897, 1, 1.30103, 1.477121), labels = c(2, 5, 10, 20, 30)) +
     labs(subtitle ='Percent of population living in\nhouseholds with water on the premises', x = '', y = '') +
     geom_text(data = subnat_clusters, aes(x = log10(k_complexity_average), y = PC1, label = ISO), color = 'white', check_overlap = TRUE, fontface = 'bold', size = 2) +
     theme_classic() + theme(plot.subtitle = element_text(face = 'bold', hjust = .5),
                             legend.title = element_text(face = 'bold'),
                             legend.key.width = unit(1.4, 'cm'),
                             axis.text = element_text(size = 13))) +
  (ggplot(data = subnat_clusters, aes(x = log10(k_complexity_average), y = PC1, color = HC_ELEC_P_ELC)) +
     geom_smooth(method = "lm", formula =  'y ~ (x)', se = FALSE, color = '#333333') +
     geom_point(aes(size = landscan_population_un_density_hectare), alpha = .85) + 
     scale_color_viridis(label = scales::percent, limits = c(0,1), name = 'Percent of\nDHS indicator   ') +
     scale_size_continuous(range = c(4,15), breaks = c(10, 40, 160), name = 'Population\nper hectare') +
     scale_x_continuous(expand = c(0,0.01), oob = scales::squish, limits = c(0.3, 1.477121), breaks = c(0.30103, 0.69897, 1, 1.30103, 1.477121), labels = c(2, 5, 10, 20, 30)) +
     labs(subtitle ='Percent of population living in\nhouseholds with electricity', x = '', y = '') +
     geom_text(data = subnat_clusters, aes(x = log10(k_complexity_average), y = PC1, label = ISO), color = 'white', check_overlap = TRUE, fontface = 'bold', size = 2) +
     theme_classic() + theme(plot.subtitle = element_text(face = 'bold', hjust = .5),
                             legend.title = element_text(face = 'bold'),
                             legend.key.width = unit(1.4, 'cm'),
                             axis.text = element_text(size = 13))) +
  (ggplot(data = subnat_clusters, aes(x = log10(k_complexity_average), y = PC1, color = HC_FLRM_P_NAT)) +
     geom_smooth(method = "lm", formula =  'y ~ (x)', se = FALSE, color = '#333333') +
     geom_point(aes(size = landscan_population_un_density_hectare), alpha = .85) + 
     scale_color_viridis(label = scales::percent, limits = c(0,1), name = 'Percent of\nDHS indicator   ') +
     scale_size_continuous(range = c(4,15), breaks = c(10, 40, 160), name = 'Population\nper hectare') +
     scale_x_continuous(expand = c(0,0.01), oob = scales::squish, limits = c(0.3, 1.477121), breaks = c(0.30103, 0.69897, 1, 1.30103, 1.477121), labels = c(2, 5, 10, 20, 30)) +
     labs(subtitle ='Percent of population living in\nhouseholds with natural floors', x = '', y = '') +
     geom_text(data = subnat_clusters, aes(x = log10(k_complexity_average), y = PC1, label = ISO), color = 'white', check_overlap = TRUE, fontface = 'bold', size = 2) +
     theme_classic() + theme(plot.subtitle = element_text(face = 'bold', hjust = .5),
                             legend.title = element_text(face = 'bold'),
                             legend.key.width = unit(1.4, 'cm'),
                             axis.text = element_text(size = 13))) +
  #(ggplot() + ggplot2::annotate(geom = "table", x=0, y = 0, label = list(iso_table), size = 4) + theme_void()) + 
  plot_layout( guides = 'collect', ncol = 3) & 
  #plot_annotation(tag_levels = list(c("A", "B", "C", "D", ""))) &
  theme(legend.position="bottom",
        plot.tag = element_text(size = 15),
        legend.box.margin = margin(t = 30, r = 0, b = 0, l = 0, unit = "pt"),
        plot.subtitle = element_text(size = 14),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 13),
        legend.box.just = 'bottom') )

(dhs_k_v_pca_grob_4x3 <- gridExtra::grid.arrange(patchwork::patchworkGrob(dhs_k_v_pca_4x3), 
                                             left = text_grob(paste0('PC1 of PCA on DHS indicators'), rot = 90, vjust = 1, size = 15, face = "bold"), 
                                             bottom = text_grob("Block complexity", rot = 0, vjust = -9, size = 15, face = "bold") ) )

ggsave(plot = dhs_k_v_pca_grob_4x3, filename = paste0(wd_path,'/data/dhs-analysis/viz/scatter_k_PC1_4x3.pdf'), width = 12.3, height = 16)  
rm(dhs_k_v_pca_4x3 )

# OSM street completeness comparison ---------------------------------------------

k_country <- read_parquet(paste0('data/africa_data.parquet')) %>%
  mutate(k_complexity_wt = k_complexity * landscan_population_un) %>%
  group_by(country_code) %>%
  summarize_at(vars(k_complexity_wt, block_area_km2, landscan_population_un), list(sum)) %>%
  ungroup() %>%
  mutate(k_complexity_wt = k_complexity_wt/landscan_population_un)

street_validation <- read_csv('data/streets_validation.csv') %>%
  mutate(osm_cia_ratio = osm_total_streets_km / cia_roadways_km,
         osm_ecopia_ratio = osm_total_streets_km / ecopia_ml_roads_km,
         osm_irf_ratio = osm_total_streets_km / irf_all_roads_km,
         osm_vehic_cia_ratio = osm_vehicular_streets_km / cia_roadways_km,
         osm_vehic_ecopia_ratio = osm_vehicular_streets_km / ecopia_ml_roads_km,
         osm_vehic_irf_ratio = osm_vehicular_streets_km / irf_all_roads_km)

street_validation <- street_validation %>%
  left_join(., k_country  %>% select(country_code, k_complexity_wt , block_area_km2),
            by = c('country_code' = 'country_code'))

# Correlation of street length sources
(corr_osm_streets_v_external <- rcorr(street_validation %>% select(osm_vehicular_streets_km, osm_total_streets_km, 
                                   cia_roadways_km, irf_all_roads_km, ecopia_ml_roads_km)
      %>% as.matrix(), type=c('spearman')) %>% tidy() %>% 
  filter(column2 %in% c('osm_vehicular_streets_km', 'osm_total_streets_km'),
         !(column1 %in% c('osm_vehicular_streets_km', 'osm_total_streets_km'))) %>%
  arrange(column2, column1) %>%
  rename(osm_var = column2,
         external_source = column1))

# Correlation of OSM ratios and block complexity
(corr_osm_ratios_v_k <- rcorr(street_validation %>% select(k_complexity_wt, osm_cia_ratio, osm_ecopia_ratio, osm_irf_ratio, osm_vehic_cia_ratio, osm_vehic_ecopia_ratio, osm_vehic_irf_ratio)
      %>% as.matrix(), type=c('spearman')) %>% tidy() %>% 
  filter(column2 %in% c('k_complexity_wt')) %>%
  arrange(column2, column1) )

# Correlation of street density and block complexity
(corr_street_v_k <- rcorr(street_validation %>% 
        mutate(street_density_km_km2 = osm_vehicular_streets_km/block_area_km2) %>%
        select(k_complexity_wt, street_density_km_km2) %>%
       as.matrix(), type=c('spearman')) %>% tidy() %>% 
  filter(column2 %in% c('k_complexity_wt')) %>%
  arrange(column2, column1))

# Summary of OSM ratios
(completeness_summary <- street_validation %>%
  mutate(osm_cia_ratio_gte1 = case_when(osm_cia_ratio >= 1 ~ 1, TRUE ~ 0),
         cia_count = case_when(!is.na(osm_total_streets_km) ~ 1, TRUE ~ 0),
         osm_ecopia_ratio_gte1 = case_when(osm_ecopia_ratio >= 1 ~ 1, TRUE ~ 0),
         ecopia_count = case_when(!is.na(ecopia_ml_roads_km) ~ 1, TRUE ~ 0),
         osm_irf_ratio_gte1 = case_when(osm_irf_ratio >= 1 ~ 1, TRUE ~ 0),
         irf_count = case_when(!is.na(irf_all_roads_km) ~ 1, TRUE ~ 0),
         osm_vehic_cia_ratio_gte1 = case_when(osm_vehic_cia_ratio >= 1 ~ 1, TRUE ~ 0),
         osm_vehic_ecopia_ratio_gte1 = case_when(osm_vehic_ecopia_ratio >= 1 ~ 1, TRUE ~ 0),
         osm_vehic_irf_ratio_gte1 = case_when(osm_vehic_irf_ratio >= 1 ~ 1, TRUE ~ 0)) %>%
  summarize_at(vars(cia_count, ecopia_count, irf_count, osm_cia_ratio_gte1, osm_ecopia_ratio_gte1, osm_irf_ratio_gte1, osm_vehic_cia_ratio_gte1, osm_vehic_ecopia_ratio_gte1, osm_vehic_irf_ratio_gte1), list(sum)) %>%
  pivot_longer(cols = everything()))

# Correlations at each K level
k_country_k_15 <- read_parquet(paste0('data/africa_data.parquet')) %>%
  group_by(k_labels_detailed, country_code, country_name) %>%
  summarize_at(vars(landscan_population_un), list(sum)) %>%
  ungroup() %>%
  mutate(k_labels_15 = case_when(k_labels_detailed %in% c("15", "16", "17","18","19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30+") ~ '15+',
                                 TRUE ~ as.character(k_labels_detailed)),) %>%
  group_by(k_labels_15, country_code, country_name) %>%
  summarize_at(vars(landscan_population_un), list(sum)) %>%
  ungroup() %>%
  group_by(country_code, country_name) %>%
  mutate(shr_landscan_population_un = landscan_population_un / sum(landscan_population_un)) %>%
  ungroup() %>%
  select(country_code, country_name, k_labels_15, shr_landscan_population_un)

k_15 <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15+", "Off-network")

k_country_k_15 <- k_country_k_15 %>%
  left_join(., street_validation %>% select(country_code,
                     osm_cia_ratio, osm_ecopia_ratio, osm_irf_ratio, 
                     osm_vehic_cia_ratio, osm_vehic_ecopia_ratio, osm_vehic_irf_ratio,
                     osm_vehicular_streets_km, block_area_km2) %>%
              mutate(street_density = osm_vehicular_streets_km / block_area_km2),
            by =c('country_code'='country_code')) %>%
  mutate(k_labels_15 = factor(k_labels_15, k_15)) 

ratio_v_thresh <- data.frame('k' = character(),'column1' = character(),'column2' = character(),'estimate' = numeric(),'n' = numeric(),'p.value' = numeric())

for (i in k_15) {
  for (j in c("street_density", "osm_cia_ratio", "osm_ecopia_ratio", "osm_irf_ratio", "osm_vehic_cia_ratio", "osm_vehic_ecopia_ratio", "osm_vehic_irf_ratio")) {
    print(i)
    ratio_i_j <- rcorr(k_country_k_15 %>% filter(k_labels_15 == i) %>%
            select_at(c('shr_landscan_population_un', j)) %>% as.matrix(),
          type=c('spearman')) %>% tidy() %>%
      mutate(k = i)
    ratio_v_thresh <- rbind(ratio_v_thresh, ratio_i_j)
  }
}

ratio_v_thresh <- ratio_v_thresh %>%
  mutate(k = factor(k, k_15)) 

ggplot(data = ratio_v_thresh %>% filter(column1 == "osm_ecopia_ratio"),
       aes(x = k, y = estimate)) +
  geom_hline(yintercept=0, linetype="dashed", color = "#FF6F91") + 
  #geom_rect(mapping = aes(xmin = "1", xmax = "5", ymin = -Inf, ymax = Inf), fill = "#FF6F91", color = alpha("white",0), alpha = 0.01) +
  geom_point( size = 1) +
  #geom_point(aes(size = shr_landscan_population_un), alpha = .8, color = '#845EC2') +
  geom_line(group = 1) + 
  #scale_y_continuous(expand = c(0,0), limits = c(-.75,.7)) + 
  scale_size_continuous( labels = label_percent(), name = 'Share of population') +
  labs(subtitle = '', y = '', x = '') + 
  theme_classic() +
  theme(plot.subtitle = element_text(hjust = .5),
        legend.position = 'bottom')


# Histograms to characterize gradients of block complexity -----------------------------

k_15 <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15+", "Off-network")

k_thresh_pop_ratio <- read_parquet(paste0('data/africa_data.parquet')) %>%
  mutate(k_labels_15 = case_when(k_labels_detailed %in% c("15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30+") ~ '15+',
                                 TRUE ~ as.character(k_labels_detailed)),
         building_to_block_area_ratio_bucket = case_when(building_to_block_area_ratio <= .001 ~ '< 0.1%',
                                                         building_to_block_area_ratio > .001 & building_to_block_area_ratio <= .005 ~ '0.1 to 0.5%',
                                                         building_to_block_area_ratio > .005 & building_to_block_area_ratio <= .01 ~ '0.5 to 1%',
                                                         building_to_block_area_ratio > .01 & building_to_block_area_ratio <= .05 ~ '1 to 5%',
                                                         building_to_block_area_ratio > .05 & building_to_block_area_ratio <= .1 ~ '5 to 10%',
                                                         building_to_block_area_ratio > .1 ~ '> 10%')) %>%
  group_by(k_labels_15, building_to_block_area_ratio_bucket) %>%
  summarize_at(vars(landscan_population_un), list(sum)) %>%
  ungroup() %>%
  mutate(k_labels_15 = factor(k_labels_15, k_15),
         building_to_block_area_ratio_bucket = factor(building_to_block_area_ratio_bucket, 
                                                      c('< 0.1%', '0.1 to 0.5%', '0.5 to 1%', '1 to 5%', '5 to 10%', '> 10%'))) %>%
  arrange(desc(k_labels_15)) %>% 
  mutate(cum_landscan_population_un = cumsum(landscan_population_un),
         tot_landscan_population_un = sum(landscan_population_un),
         shr_landscan_population_un = landscan_population_un/tot_landscan_population_un,
         cum_shr_landscan_population_un = cum_landscan_population_un/tot_landscan_population_un) 

k_thresh_pop_size <- read_parquet(paste0('data/africa_data.parquet')) %>%
  mutate(k_labels_15 = case_when(k_labels_detailed %in% c("15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30+") ~ '15+',
                                 TRUE ~ as.character(k_labels_detailed)),
         block_area_km2_bucket = case_when(block_area_km2 < .01 ~ '< 0.01 km2',
                                           block_area_km2 > .01 & block_area_km2 <= .1 ~ '0.01 to 0.1 km2',
                                           block_area_km2 > .1 & block_area_km2 <= 1 ~ '0.1 to 1 km2',
                                           block_area_km2 > 1 &  block_area_km2 <= 10  ~ '1 to 10 km2',
                                           block_area_km2 > 10 & block_area_km2 <= 100  ~ '10 to 100 km2',
                                           block_area_km2 > 100 & block_area_km2 <= 500  ~ '100 to 500 km2',
                                           block_area_km2 > 500 ~ '> 500 km2')) %>%
  group_by(k_labels_15, block_area_km2_bucket) %>%
  summarize_at(vars(landscan_population_un), list(sum)) %>%
  ungroup() %>%
  mutate(k_labels_15 = factor(k_labels_15, k_15),
         block_area_km2_bucket = factor(block_area_km2_bucket, c('< 0.01 km2', '0.01 to 0.1 km2', '0.1 to 1 km2', '1 to 10 km2', '10 to 100 km2', '100 to 500 km2', '> 500 km2'))) %>%
  arrange(desc(k_labels_15)) %>% 
  mutate(cum_landscan_population_un = cumsum(landscan_population_un),
         tot_landscan_population_un = sum(landscan_population_un),
         shr_landscan_population_un = landscan_population_un/tot_landscan_population_un,
         cum_shr_landscan_population_un = cum_landscan_population_un/tot_landscan_population_un) 

k_thresh_pop_area <- read_parquet(paste0('data/africa_data.parquet')) %>%
  mutate(k_labels_15 = case_when(k_labels_detailed %in% c("15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30+") ~ '15+',
                                 TRUE ~ as.character(k_labels_detailed))) %>%
  group_by(k_labels_15, area_type) %>%
  summarize_at(vars(landscan_population_un), list(sum)) %>%
  ungroup() %>%
  mutate(area_type = ifelse(area_type == 'Non-urban', 'Rural', area_type)) %>%
  mutate(k_labels_15 = factor(k_labels_15, k_15),
         area_type = factor(area_type, c('Urban', 'Peri-urban', 'Rural'))) %>%
  arrange(desc(k_labels_15)) %>% 
  mutate(cum_landscan_population_un = cumsum(landscan_population_un),
         tot_landscan_population_un = sum(landscan_population_un),
         shr_landscan_population_un = landscan_population_un/tot_landscan_population_un,
         cum_shr_landscan_population_un = cum_landscan_population_un/tot_landscan_population_un) 

(bar_15plus_ratio <- ggplot() +
    geom_bar(data = k_thresh_pop_ratio, aes(x = k_labels_15, y = shr_landscan_population_un, 
                                            fill = building_to_block_area_ratio_bucket),  
             color = '#333333',
             position="stack",  stat="identity") +
    scale_y_continuous(expand = c(0, 0), labels = scales::percent ) +
    scale_fill_manual(name = 'Building to block\narea ratio', values = c('#F9F871', '#FF9671', '#D65DB1', '#2C73D2', '#00C9A7', '#f8f1e1')) + #  '#C4FCEF'
    labs(subtitle = 'Most k = 15+ blocks have less than 0.5% of area covered in buildings',
         y = 'Share of population', x = 'Block complexity') + 
    theme_classic() +
    theme(#legend.position = 'bottom',
      plot.subtitle = element_text(hjust = .5)))

(bar_15plus_size <- ggplot() +
    geom_bar(data = k_thresh_pop_size, aes(x = k_labels_15, y = shr_landscan_population_un, 
                                           fill = block_area_km2_bucket),  
             color = '#333333',
             position="stack",  stat="identity") +
    scale_y_continuous(expand = c(0, 0), labels = scales::percent ) +
    scale_fill_manual(name = 'Block area size', values = c('#F9F871', '#FF9671', '#D65DB1', '#845EC2', '#2C73D2', '#00C9A7', '#f8f1e1')) + # '#C4FCEF'
    labs(subtitle = 'Most k = 15+ blocks are over 10 km2',
         y = 'Share of population', x = 'Block complexity') + 
    theme_classic() +
    theme(#legend.position = 'bottom',
      plot.subtitle = element_text(hjust = .5)))

(bar_15plus_area <- ggplot() +
    geom_bar(data = k_thresh_pop_area , aes(x = k_labels_15, y = shr_landscan_population_un, 
                                            fill = area_type),  
             color = '#333333',
             position="stack",  stat="identity") +
    scale_y_continuous(expand = c(0, 0), labels = scales::percent ) +
    scale_fill_manual(name = 'Urban / rural', values = c('#F9F871', '#FF9671', '#D65DB1', '#845EC2', '#2C73D2', '#00C9A7', '#f8f1e1')) +   # '#C4FCEF'
    labs(subtitle = 'Most k = 15+ blocks are rural',
         y = 'Share of population', x = 'Block complexity') + 
    theme_classic() +
    theme(#legend.position = 'bottom',
      plot.subtitle = element_text(hjust = .5)))

# Combine the bar charts
(bar_15 <- bar_15plus_ratio + bar_15plus_size + bar_15plus_area & 
    plot_layout(ncol = 1) &
    plot_annotation(tag_levels = list(c("A","B", "C"))) & 
    theme(plot.tag = element_text(size = 13)))

ggsave(plot =  bar_15, filename = paste0(wd_path,'/data/dhs-analysis/viz/bar_chart_k_15plus.pdf'), width = 10, height = 11)  

# Correlation of population in block complexity levels vs PC1 -----------------------------------------------------

# Box.com link with access to staging files:
# Access subnat_to_blocks here: https://uchicago.box.com/s/anw4fxc376dgtgol9tt87tuozipnk0kj
# subnat_to_blocks <- read_parquet(paste0(wd_path,'/data/blocks_to_dhs.parquet'))
k_levels_vs_pc1 <- read_parquet(paste0('data/africa_data.parquet')) %>%
  inner_join(., subnat_to_blocks, by = c('block_id'='block_id')) 

k_levels_vs_pc1 <- k_levels_vs_pc1 %>%
  mutate(k_complexity_wt = k_complexity * landscan_population_un) %>%
  group_by(k_labels_detailed, ISO3_CountryCode, DHS_CountryCode, SurveyId, SurveyYear, REGCODE, REG_ID, REGNAME, CNTRYNAMEE, DHSREGEN) %>%
  summarize_at(vars(k_complexity_wt, landscan_population_un), list(sum)) %>%
  ungroup() %>%
  mutate(k_labels_15 = case_when(k_labels_detailed %in% c("15", "16", "17","18","19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30+") ~ '15+',
                                 TRUE ~ as.character(k_labels_detailed)),) %>%
  group_by(k_labels_15, ISO3_CountryCode, DHS_CountryCode, SurveyId, SurveyYear, REGCODE, REG_ID, REGNAME, CNTRYNAMEE, DHSREGEN) %>%
  summarize_at(vars(k_complexity_wt, landscan_population_un), list(sum)) %>%
  ungroup() %>%
  mutate(k_complexity_wt = k_complexity_wt/landscan_population_un)

k_15 <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15+", "Off-network")

k_levels_vs_pc1 <- k_levels_vs_pc1 %>%
  mutate(k_labels_15 = factor(k_labels_15, k_15)) %>%
  arrange(SurveyId, REG_ID, desc(k_labels_15)) %>%
  group_by(SurveyId, REG_ID) %>% 
  mutate(cum_landscan_population_un = cumsum(landscan_population_un),
         tot_landscan_population_un = sum(landscan_population_un),
         shr_landscan_population_un = landscan_population_un/tot_landscan_population_un,
         cum_shr_landscan_population_un = cum_landscan_population_un/tot_landscan_population_un) %>%
  ungroup() %>%
  select(ISO3_CountryCode, DHS_CountryCode, SurveyId, SurveyYear, REGCODE, REG_ID, REGNAME, CNTRYNAMEE, DHSREGEN, 
         k_labels_15, cum_landscan_population_un, tot_landscan_population_un,
         shr_landscan_population_un, cum_shr_landscan_population_un, landscan_population_un) 
  
k_levels_vs_pc1 <- k_levels_vs_pc1 %>%
  left_join(., subnat_clusters %>% 
              select(REG_ID, street_density_ratio_km_to_km2, k_complexity_average, PC1),
            by = c('REG_ID' = 'REG_ID'))
  
pc1_k_corr <- data.frame('k' = character(),'column1' = character(),'column2' = character(),'estimate' = numeric(),'n' = numeric(),'p.value' = numeric())

for (i in k_15) {
  k_thresh_i <- rcorr(k_levels_vs_pc1 %>% filter(k_labels_15 == i) %>%
          select_at(c('shr_landscan_population_un', 'PC1')) %>% as.matrix(),
                       type=c('spearman')) %>% tidy() %>%
      mutate(k = i)
  pc1_k_corr <- rbind(pc1_k_corr, k_thresh_i)
}

k_thresh_corr <- k_levels_vs_pc1 %>%
  group_by(k_labels_15) %>%
  summarise(
    landscan_population_un = sum(landscan_population_un),
    pc1_cor_shr=stats::cor(shr_landscan_population_un, PC1, use =  "complete.obs", method = "spearman")
    ) %>%
  ungroup() %>%
  mutate(shr_landscan_population_un = landscan_population_un/sum(landscan_population_un)) %>%
  left_join(., pc1_k_corr %>%
              select(estimate, n, p.value, k), by = c('k_labels_15'='k')) %>%
  mutate(k_labels_15 = factor(k_labels_15, k_15 ))

# Spearman correlation between population share at each level of k and PC1 across DHS regions
(line_correl_k_thresh <- ggplot(data = k_thresh_corr,
             aes(x = k_labels_15, y = estimate)) +
    geom_hline(yintercept=0, linetype="dashed", color = "#FF6F91") + 
    geom_rect(mapping = aes(xmin = "1", xmax = "5", ymin = -Inf, ymax = Inf), fill = "#FF6F91", color = alpha("white",0), alpha = 0.01) +
    geom_point( size = 1) +
    geom_point(aes(size = shr_landscan_population_un), alpha = .8, color = '#845EC2') +
    geom_line(group = 1) + 
    scale_y_continuous(expand = c(0,0), limits = c(-.75,.7)) + 
    scale_size_continuous( labels = label_percent(), name = 'Share of population') +
    labs(#subtitle = 'Block complexity correlation with PC1 begins to slightly weaken\nwhen k = 15+ and for off-network blocks across DHS regions',
         y = 'Spearman correlation with PC1 at each block complexity level', x = 'Population share at each block complexity level correlated with PC1') + 
    theme_classic() +
    theme(plot.subtitle = element_text(hjust = .5),
          plot.margin=unit(c(t=3,r=30,b=5,l=5), "pt"),
          legend.position = 'bottom'))

# Block complexity vs OSM completeness ---------------------------------------------

k_15_data <- read_parquet(paste0('data/africa_data.parquet')) %>%
  mutate(block_complexity_wt = landscan_population_un * k_complexity) %>%
  mutate(k_labels_15 = case_when(k_labels_detailed %in% c("15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30+", "Off-network") ~ '15+',
                                 TRUE ~ as.character(k_labels_detailed)), 
         area_type = case_when(area_type %in% c('Peri-urban', 'Non-urban') ~ 'nonurban',
                                TRUE ~ 'urban')) %>%
  group_by(area_type, k_labels_15, country_code, country_name) %>%
  summarize_at(vars(block_complexity_wt, landscan_population_un, block_area_km2), list(sum), na.rm = TRUE) %>%
  ungroup()

# Average block complexity by country 
k_15_complexity <- k_15_data  %>%
  group_by(country_code) %>%
  summarize_at(vars(block_complexity_wt, landscan_population_un), list(sum)) %>%
  ungroup() %>%
  mutate(block_complexity_wt =block_complexity_wt/ landscan_population_un ) 
  
# Population share by urban/rural area and country
k_15_ratios_area <- k_15_data  %>%
  group_by(area_type, country_code) %>%
  summarize_at(vars(landscan_population_un, block_area_km2), list(sum)) %>%
  ungroup() %>%
  group_by(country_code) %>% 
  mutate(pop_share_country = landscan_population_un/ sum(landscan_population_un)) %>%
  ungroup() %>%
  group_by(area_type) %>% 
  mutate(pop_share_type = landscan_population_un/ sum(landscan_population_un)) %>%
  ungroup() %>%
  select(country_code, area_type, pop_share_country, pop_share_type) %>%
  pivot_wider(id_cols = country_code, names_from = area_type, values_from = c(pop_share_country, pop_share_type), values_fill = 0)

# Population share by block complexity and country
k_15 <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15+", "Off-network")
k_15_ratios_levels <- k_15_data %>%
  group_by(k_labels_15, country_code, country_name) %>%
  summarize_at(vars(landscan_population_un, block_area_km2), list(sum)) %>%
  ungroup() %>%
  mutate(k_labels_15 = factor(k_labels_15, k_15)) %>%
  arrange(desc(k_labels_15)) %>% 
  group_by(k_labels_15) %>% 
  mutate(pop_share_k = landscan_population_un/sum(landscan_population_un),
         area_share_k = block_area_km2 / sum(block_area_km2 ) ) %>%
  ungroup() %>%
  group_by(country_code) %>%
  mutate(pop_share_country = landscan_population_un/sum(landscan_population_un)) %>%
  ungroup() %>%
  filter(k_labels_15 == '15+') %>%
  mutate(pop_density_k = landscan_population_un/block_area_km2,
         ratio_pop_area = pop_share_k / area_share_k )

k_15_ratios <- street_validation %>%
  select(country_code,  osm_cia_ratio, osm_vehic_cia_ratio, osm_ecopia_ratio, osm_vehic_ecopia_ratio) %>%
  left_join(., k_15_ratios_area, by =  c('country_code'='country_code')) %>%
  left_join(., k_15_ratios_levels , by =  c('country_code'='country_code')) %>%
  left_join(., k_15_complexity %>% select(country_code, block_complexity_wt)) %>%
  mutate(ratio_15_nonurban = pop_share_k / pop_share_type_nonurban, by =  c('country_code'='country_code'))
          #= pop_share_country / pop_share_country_nonurban)

# Scatter comparing average block complexity and OSM street to CIA ratio
(scatter_15plus_outliers <- ggplot() +
    geom_hline(yintercept= 0,  color = "#FF6F91") + 
    #geom_vline(xintercept= 1,  color = "#FF6F91") +
    geom_point(data = k_15_ratios,
               aes(x = block_complexity_wt, y = log10(osm_vehic_cia_ratio), 
                   size = landscan_population_un) , color = '#845EC2', alpha = 0.8) +
    geom_text_repel(data = k_15_ratios,
                    seed = 1, segment.curvature = -0.1, point.padding = 0, box.padding = 0.4, max.iter = 30000, segment.square  = FALSE, segment.inflect = FALSE, 
                    min.segment.length = 0, max.overlaps = Inf, force = .1, force_pull = 10, 
                    aes(x = block_complexity_wt, y = log10(osm_vehic_cia_ratio), label = country_name), 
                    size = 3, vjust =.5, color = '#333333', fontface='bold') + 
    #geom_text(x = 4, y = log10(.5), label = 'OSM reports less vehicular roadway than CIA') + 
    #geom_text(x = 4, y = log10(15), label = 'OSM reports more vehicular roadway than CIA') +
    #scale_x_continuous(labels = scales::percent, breaks = c(0, .05, .1, .15, .2, .25, .3)) +
    scale_y_continuous(breaks = c( log10(.5), log10(1), log10(10), log10(100)), labels = c( .5, 1, 10, 100)) +
    labs(#subtitle = 'South Sudan and Eritrea have relatively high block complexity\nand report less roadway in OSM than the CIA World Factbook',
         #x = "Ratio of population share in k = 15+ / off-network blocks to non-urban population share",
         x = "Average block complexity (weighted to population)",
         y = "OSM completeness\n(ratio of OSM vehicular street distance to CIA roadway distance)") +
    theme_classic() +
    theme(legend.position = 'none', 
          plot.subtitle = element_text(hjust = .5)))

line_correl_k_thresh  + scatter_15plus_outliers
(k_thresh_charts <- line_correl_k_thresh  + scatter_15plus_outliers & 
  #plot_layout(ncol = 2) &
  plot_annotation(tag_levels = list(c("A", "B"))) & 
  theme(plot.tag = element_text(size = 13)))

ggsave(plot = k_thresh_charts, filename = paste0(wd_path,'/data/dhs-analysis/viz/scatter_k_thresh.pdf'), width = 17, height = 7)  


# Write analytics table ---------------------------------------------------


writexl::write_xlsx(list('corr_dhs' = cor_dhs,
                         'corr_dhs_k_v_street' = cor_summary_streets_v_k,
                         'corr_un' = cor_un, 
                         'low_pval_street' = insig_street_density,
                         'pc1_regs_k' = reg_pca_output,
                         'pc1_regs_k_w_street' = reg_pca_output_street_density,
                         'pca_loadings' = pca_loadings, 
                         'pca_var_exp' = pca_var_explained,
                         'pc1_corr' = rbind(cor_pc1_k, cor_pc1_street),
                         'pc1_corr_k_thres' = k_thresh_corr,
                         'latex_table' = latex_table, #latex_code_df,
                         'corr_street_v_k' = corr_street_v_k,
                         'corr_street_sources' = corr_osm_streets_v_external,
                         'corr_osm_ratios_v_k' = corr_osm_ratios_v_k,
                         'corr_osm_ratios_v_k_levels' = ratio_v_thresh,
                         'osm_ratio_grt1' = completeness_summary,
                         'street_data' = street_validation,
                         'indicator_preds' = combined_pred_output, 
                         'indicator_regs' = combined_reg_output ),
                         #'offnet_country' = rbind(k_data_offnet_country, k_data_offnet_15_country),
                         #'offnet_all' = rbind(k_data_offnet_all, k_data_offnet_15_all)),
                    col_names = TRUE, format_headers = TRUE,
                    path = paste0(wd_path,'/data/dhs-analysis/data/dhs_analysis_tables.xlsx'))


# Appendix ----------------------------------------------------------------

# Map of sub-national DHS data -----------------------------------------------

map_theme <- theme_bw() +
  theme(legend.title = element_blank(),
        #legend.position = 'bottom',
        #text = element_text(size = 10),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text = element_blank(),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = 'bold'),
        plot.title = element_text(hjust = 0.5, face = 'bold', size = 14),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank())

dir.create(paste0(wd_path,'/data/dhs-analysis/viz/maps'))

pc1_map_data <- subnat_all_wide_k %>%
  select(SurveyId, REG_ID, REGNAME, CountryName, geometry) %>%
  inner_join(., pca_reg_data_pred_output %>%
              select(SurveyId, REG_ID, PC1, pred_single, pred_countries, pred_controls, 
                     resid_single, resid_countries, resid_controls),
            by = c('SurveyId'='SurveyId','REG_ID'='REG_ID'))


sprintf("U+0177")
text(1, 1, paste0("b", sprintf("\U+0177"), "aːu"))

library(Cairo)
library(rnaturalearth)
library(rnaturalearthdata)
world <- ne_countries(scale = 50)
#iso_code_list <- c( 'AGO', 'BDI', 'BEN', 'BFA', 'BWA', 'CAF', 'CIV', 'CMR', 'COD', 'COG', 'COM', 'CPV', 'DJI', 'ERI', 'ESH', 'ETH', 'GAB', 'GHA', 'GIN', 'GMB', 'GNB', 'GNQ', 'KEN', 'LBR', 'LSO', 'MDG', 'MLI', 'MOZ', 'MRT', 'MUS', 'MWI', 'NAM', 'NER', 'NGA', 'RWA', 'SDN', 'SEN', 'SLE', 'SOM', 'SSD', 'STP', 'SWZ', 'SYC', 'TCD', 'TGO', 'TZA', 'UGA', 'ZAF', 'ZMB', 'ZWE')        
iso_code_list <- c("AGO", "BEN", "BFA", "CMR", "COD", "CIV", "GAB", "GHA", "GIN", "LSO", "LBR", "MLI", "MRT", "NAM", "NER", "NGA", "SLE", "ZAF", "TGO")
subsaharan_africa <- world %>%
  filter(adm0_a3 %in% iso_code_list) %>% 
  select(adm0_a3) 

(pc1_map <- ggplot() +
      geom_sf(data = pc1_map_data, aes(fill = PC1), color = 'white') +
      geom_sf(data = subsaharan_africa, fill = 'white', color = '#333333', alpha = 0, linewidth = .3) + 
      labs(subtitle = "Observed PC1 (y)") +
      scale_fill_distiller(palette = 'Spectral',  labels = scales::comma, limits = c(-10,15), oob = scales::squish) +
    ggplot() +
      geom_sf(data = pc1_map_data, aes(fill = pred_controls), color = 'white') +
      geom_sf(data = subsaharan_africa, fill = 'white', color = '#333333', alpha = 0, linewidth = .3) + 
      labs(subtitle = "Predicted PC1 (ŷ)") +
      scale_fill_distiller(palette = 'Spectral',  labels = scales::comma, limits = c(-10,15), oob = scales::squish ) +
    ggplot() +
      geom_sf(data = pc1_map_data, aes(fill = (resid_controls)), color = 'white') +
      geom_sf(data = subsaharan_africa, fill = 'white', color = '#333333', alpha = 0, linewidth = .3) + 
      labs(subtitle = "Residual (y - ŷ)") +
      scale_fill_distiller(palette = 'Spectral',  labels = scales::comma, limits = c(-10,15), oob = scales::squish ) +
    plot_layout(guides = "collect") +
    plot_annotation(title = 'Country effects and controls model', tag_levels = 'A') &
    map_theme)

ggsave(plot = pc1_map, device=cairo_pdf, filename = paste0(wd_path,'/data/dhs-analysis/viz/maps/pc1_map.pdf'), width = 12, height = 6)  


# -------------------------------------------------------------------------

for (i in seq_along(unique(subnat_all_wide$SurveyId))) {
  print(i)
  (scode = unique(subnat_all_wide$SurveyId)[i])
  print(scode)
  (title_country = subnat_all_wide  %>% st_drop_geometry() %>% filter(SurveyId == scode) %>% select(SurveyYear) %>% distinct() %>% pull())
  (title_year = subnat_all_wide %>% st_drop_geometry() %>% filter(SurveyId == scode) %>% select(CNTRYNAMEE) %>% distinct() %>% pull())
  
  check_humandev <- subnat_all_wide_k %>% filter(SurveyId == scode) %>% st_drop_geometry() %>% select(CM_ECMR_C_IMR, RH_DELP_C_DHF, ED_LITR_W_LIT, ED_EDAT_B_NED, ED_EDAT_B_SSC, ED_EDAT_B_HGH) %>% drop_na() %>% nrow() > 0
  if (check_humandev == TRUE) {
    dhs_humandev <- (
      ggplot() +
        geom_sf(data = subnat_all_wide_k %>% filter(SurveyId == scode), 
                aes(fill = CM_ECMR_C_IMR), color = 'white') +
        labs(subtitle = "Probability of dying\nbefore the first\nbirthday per 1,000 live births") +
        scale_fill_distiller(direction = -1, palette = 'Spectral',  labels = scales::percent ) +
        ggplot() +
        geom_sf(data = subnat_all_wide_k %>% filter(SurveyId == scode), 
                aes(fill = RH_DELP_C_DHF), color = 'white') +
        labs(subtitle = "Percentage of live\nbirths delivered\nat a health facility") +
        scale_fill_distiller(direction = -1, palette = 'Spectral',  labels = scales::percent ) +
        ggplot() +
        geom_sf(data = subnat_all_wide_k %>% filter(SurveyId == scode), 
                aes(fill = ED_LITR_W_LIT), color = 'white') +
        labs(subtitle = "Percentage of women\nwho are literate") +
        scale_fill_distiller(direction = -1, palette = 'Spectral',  labels = scales::percent )) / 
      (ggplot() +
         geom_sf(data = subnat_all_wide_k %>% filter(SurveyId == scode), 
                 aes(fill = ED_EDAT_B_NED), color = 'white') +
         labs(subtitle = "Percentage of household\npopulation age 6+\nwith no education") +
         scale_fill_distiller(direction = -1, palette = 'Spectral',  labels = scales::percent ) +
         ggplot() +
         geom_sf(data = subnat_all_wide_k %>% filter(SurveyId == scode), 
                 aes(fill = ED_EDAT_B_SSC), color = 'white') +
         labs(subtitle = "Percentage of household\npopulation age 6+\nwho completed primary education") +
         scale_fill_distiller(direction = -1, palette = 'Spectral',  labels = scales::percent ) +
         ggplot() +
         geom_sf(data = subnat_all_wide_k %>% filter(SurveyId == scode), 
                 aes(fill = ED_EDAT_B_HGH), color = 'white') +
         labs(subtitle = "Percentage of household\npopulation age 6+\nwho attended higher education") +
         scale_fill_distiller(direction = -1, palette = 'Spectral',  labels = scales::percent )) +
      plot_annotation(title = paste0(title_country,' ',title_year,' | Health and Education')) & 
      map_theme
    ggsave(plot = dhs_humandev, filename = paste0(wd_path,'/data/viz/maps/',title_country,'-',title_year,'-','humandev.png'), width = 12, height = 6, dpi = 300)  
  }
  #dhs_humandev 
  
  check_infra <- subnat_all_wide_k %>% filter(SurveyId == scode) %>% st_drop_geometry() %>% select(WS_SRCE_H_IOP, WS_TLET_H_IMP, HC_ELEC_H_ELC, HC_FLRM_H_NAT, HC_PPRM_H_12P, HC_PPRM_H_34P) %>% drop_na() %>% nrow() > 0
  if (check_infra == TRUE) {
    dhs_infra <- (
      ggplot() +
        geom_sf(data = subnat_all_wide_k %>% filter(SurveyId == scode), 
                aes(fill = WS_SRCE_H_IOP), color = 'white') +
        labs(subtitle = "Percentage of households\nwith an improved water source\non the premises") +
        scale_fill_distiller(limits = c(0,1), direction = -1, palette = 'Spectral',  labels = scales::percent ) +
        ggplot() +
        geom_sf(data = subnat_all_wide_k %>% filter(SurveyId == scode), 
                aes(fill = WS_TLET_H_IMP), color = 'white') +
        labs(subtitle = "Percentage of households\nwith an improved\nsanitation facility") +
        scale_fill_distiller(limits = c(0,1), direction = -1, palette = 'Spectral',  labels = scales::percent ) +
        ggplot() +
        geom_sf(data = subnat_all_wide_k %>% filter(SurveyId == scode), 
                aes(fill = HC_ELEC_H_ELC), color = 'white') +
        labs(subtitle = "Percentage of households\nwith electricity") +
        scale_fill_distiller(limits = c(0,1), direction = -1, palette = 'Spectral',  labels = scales::percent )) /
      (ggplot() +
         geom_sf(data = subnat_all_wide_k %>% filter(SurveyId == scode), 
                 aes(fill = HC_FLRM_H_NAT), color = 'white') +
         labs(subtitle = "Percentage of households\nwith natural floors") +
         scale_fill_distiller(limits = c(0,1), direction = -1, palette = 'Spectral',  labels = scales::percent ) +
         ggplot() +
         geom_sf(data = subnat_all_wide_k %>% filter(SurveyId == scode), 
                 aes(fill = HC_PPRM_H_12P), color = 'white') +
         labs(subtitle = "Percentage of\nhouseholds with 1-2 persons\nper sleeping room") +
         scale_fill_distiller(limits = c(0,1), direction = -1, palette = 'Spectral',  labels = scales::percent ) +
         ggplot() +
         geom_sf(data = subnat_all_wide_k %>% filter(SurveyId == scode), 
                 aes(fill = HC_PPRM_H_34P), color = 'white') +
         labs(subtitle = "Percentage of\nhouseholds with 3-4 persons\nper sleeping room") +
         scale_fill_distiller(limits = c(0,1), direction = -1, palette = 'Spectral',  labels = scales::percent )) +
      #plot_layout(guides = 'collect') &
      plot_annotation(title = paste0(title_country,' ',title_year,' | Household Characteristics')) & 
      map_theme #+ theme(legend.position = 'bottom')
    ggsave(plot = dhs_infra, filename = paste0(wd_path,'/data/viz/maps/',title_country,'-',title_year,'-','infra.png'), width = 12, height = 6, dpi = 400)  
  }
  #dhs_infra
  
  check_wealth <- subnat_all_wide_k %>% filter(SurveyId == scode) %>% st_drop_geometry() %>% select(HC_WIXQ_P_LOW, HC_WIXQ_P_2ND, HC_WIXQ_P_MID, HC_WIXQ_P_4TH, HC_WIXQ_P_HGH) %>% drop_na() %>% nrow() > 0
  if (check_wealth > 0) {
    dhs_wealth <- (
      ggplot() +
        geom_sf(data = subnat_all_wide_k %>% filter(SurveyId == scode), 
                aes(fill = HC_WIXQ_P_LOW), color = 'white') +
        labs(subtitle = "Percentage of population\nin the lowest wealth quintile") +
        scale_fill_distiller(limits = c(0,1), direction = -1, palette = 'Spectral',  labels = scales::percent ) +
        ggplot() +
        geom_sf(data = subnat_all_wide_k %>% filter(SurveyId == scode), 
                aes(fill = HC_WIXQ_P_2ND), color = 'white') +
        labs(subtitle = "Percentage of population\nin the second wealth quintile") +
        scale_fill_distiller(limits = c(0,1), direction = -1, palette = 'Spectral',  labels = scales::percent ) +
        ggplot() +
        geom_sf(data = subnat_all_wide_k %>% filter(SurveyId == scode), 
                aes(fill = HC_WIXQ_P_MID), color = 'white') +
        labs(subtitle = "Percentage of population\nin the middle wealth quintile") +
        scale_fill_distiller(limits = c(0,1), direction = -1, palette = 'Spectral',  labels = scales::percent )) /
      (ggplot() +
         geom_sf(data = subnat_all_wide_k %>% filter(SurveyId == scode), 
                 aes(fill = HC_WIXQ_P_4TH), color = 'white') +
         labs(subtitle = "Percentage of population\nin the fourth wealth quintile") +
         scale_fill_distiller(limits = c(0,1), direction = -1, palette = 'Spectral',  labels = scales::percent ) +
         ggplot() +
         geom_sf(data = subnat_all_wide_k %>% filter(SurveyId == scode), 
                 aes(fill = HC_WIXQ_P_HGH), color = 'white') +
         labs(subtitle = "Percentage of population\nin the highest wealth quintile") +
         scale_fill_distiller(limits = c(0,1), direction = -1, palette = 'Spectral',  labels = scales::percent ) +
         ggplot() +
         geom_sf(data = subnat_all_wide_k %>% filter(SurveyId == scode), 
                 aes(fill = HC_WIXQ_P_GNI), color = 'white') +
         labs(subtitle = "Gini coefficient\n") +
         scale_fill_distiller(direction = -1, palette = 'Spectral',  labels = scales::percent ))  +
      plot_annotation(title = paste0(title_country,' ',title_year,' | Wealth')) & 
      map_theme
    ggsave(plot = dhs_wealth, filename = paste0(wd_path,'/data/viz/maps/',title_country,'-',title_year,'-','wealth.png'), width = 12, height = 6, dpi = 300)  
  }
  #dhs_wealth
  
  check_k <- subnat_all_wide_k %>% filter(SurveyId == scode) %>% st_drop_geometry() %>% select(k_complexity_average, population_k_4_plus, landscan_population_un, block_hectares, region_core_urban, region_non_urban) %>% drop_na() %>% nrow() > 0
  if (check_k > 0) {
    dhs_k <- (
      ggplot() +
        geom_sf(data = subnat_all_wide_k %>% filter(SurveyId == scode), 
                aes(fill = k_complexity_average), color = 'white') +
        labs(subtitle = "Population weighted\nblock complexity") +
        scale_fill_distiller(limits = c(0, max(subnat_all_wide_k %>% st_drop_geometry() %>% filter(SurveyId == scode) %>% select(k_complexity_average) %>% pull() )), direction = -1, palette = 'Spectral',  labels = scales::comma ) + 
        ggplot() +
        geom_sf(data = subnat_all_wide_k %>% filter(SurveyId == scode), 
                aes(fill = population_k_4_plus/landscan_population_un), color = 'white') +
        labs(subtitle = "Percentage of population\nin block with\n4+ layers of buildings") +
        scale_fill_distiller(limits = c(0,1), direction = -1, palette = 'Spectral',  labels = scales::percent ) +
        ggplot() +
        geom_sf(data = subnat_all_wide_k %>% filter(SurveyId == scode), 
                aes(fill = (population_k_6+population_k_7+population_k_8+population_k_9+population_k_10plus+population_off_network)/landscan_population_un), color = 'white') +
        labs(subtitle = "Percentage of population\nin block with\n6+ layers of buildings") +
        scale_fill_distiller(limits = c(0,1), direction = -1, palette = 'Spectral',  labels = scales::percent )) /
      (ggplot() +
         geom_sf(data = subnat_all_wide_k %>% filter(SurveyId == scode), 
                 aes(fill = replace_na(na_if(na_if(na_if(log10(landscan_population_un / block_hectares), NaN), Inf), -Inf), 1) ), color = 'white') +
         labs(subtitle = "Population per hectare") +
         scale_fill_distiller(direction = -1, palette = 'Spectral', 
                              name = 'Population\nper hectare', oob = scales::squish, 
                              breaks= c(1,2,3), labels = c('1',"100","1K")) +
         ggplot() +
         geom_sf(data = subnat_all_wide_k %>% filter(SurveyId == scode), 
                 aes(fill = (region_core_urban+region_peripheral_urban) / landscan_population_un), color = 'white') +
         labs(subtitle = "Percentage of population\nin urban areas") +
         scale_fill_distiller(limits = c(0,1), direction = -1, palette = 'Spectral',  labels = scales::percent ) +
         ggplot() +
         geom_sf(data = subnat_all_wide_k %>% filter(SurveyId == scode), 
                 aes(fill = region_non_urban / landscan_population_un), color = 'white') +
         labs(subtitle = "Percentage of population\nin non-urban areas") +
         scale_fill_distiller(limits = c(0,1), direction = -1, palette = 'Spectral',  labels = scales::percent )) +
      plot_annotation(title = paste0(title_country,' ',title_year,' | Block complexity')) & 
      map_theme
    ggsave(plot = dhs_k, filename = paste0(wd_path,'/data/viz/maps/',title_country,'-',title_year,'-','complexity.png'), width = 12, height = 6, dpi = 300)  
  }
  #dhs_k
}

# Africa Map --------------------------------------------------------------

africa_map_data <- subnat_all_wide_k %>% 
  group_by(CNTRYNAMEE, REGCODE) %>%
  mutate(rank = row_number(desc(SurveyYear))) %>%
  ungroup() %>% filter(rank == 1)

dhs_humandev <- (
  ggplot() +
    geom_sf(data = africa_map_data %>% filter(!is.na(CM_ECMR_C_IMR)), aes(fill = CM_ECMR_C_IMR), linewidth = .01, color = 'white') +
    labs(subtitle = "Probability of dying\nbefore the first\nbirthday per 1,000 live births") +
    scale_fill_distiller(direction = -1, palette = 'Spectral',  labels = scales::percent ) +
    ggplot() +
    geom_sf(data = africa_map_data %>% filter(!is.na(RH_DELP_C_DHF)), aes(fill = RH_DELP_C_DHF), linewidth = .01, color = 'white') +
    labs(subtitle = "Percentage of live\nbirths delivered\nat a health facility") +
    scale_fill_distiller(direction = -1, palette = 'Spectral',  labels = scales::percent ) +
    ggplot() +
    geom_sf(data = africa_map_data %>% filter(!is.na(ED_LITR_W_LIT)), aes(fill = ED_LITR_W_LIT), linewidth = .01, color = 'white') +
    labs(subtitle = "Percentage of women\nwho are literate") +
    scale_fill_distiller(direction = -1, palette = 'Spectral',  labels = scales::percent )) / 
  (ggplot() +
     geom_sf(data = africa_map_data %>% filter(!is.na(ED_EDAT_B_NED)),  aes(fill = ED_EDAT_B_NED), linewidth = .01, color = 'white') +
     labs(subtitle = "Percentage of household\npopulation age 6+\nwith no education") +
     scale_fill_distiller(direction = -1, palette = 'Spectral',  labels = scales::percent ) +
     ggplot() +
     geom_sf(data = africa_map_data %>% filter(!is.na(ED_EDAT_B_SSC)),  aes(fill = ED_EDAT_B_SSC), linewidth = .01, color = 'white') +
     labs(subtitle = "Percentage of household\npopulation age 6+\nwho completed primary education") +
     scale_fill_distiller(direction = -1, palette = 'Spectral',  labels = scales::percent ) +
     ggplot() +
     geom_sf(data = africa_map_data %>% filter(!is.na(ED_EDAT_B_HGH)),  aes(fill = ED_EDAT_B_HGH), linewidth = .01, color = 'white') +
     labs(subtitle = "Percentage of household\npopulation age 6+\nwho attended higher education") +
     scale_fill_distiller(direction = -1, palette = 'Spectral',  labels = scales::percent )) +
  plot_annotation(title = paste0('sub-Saharan Africa | Health and Education')) & 
  map_theme
dhs_humandev
ggsave(plot = dhs_humandev, filename = paste0(wd_path,'/data/viz/maps/','africa-humandev.pdf'), width = 10, height = 8)  

dhs_infra <- (
  ggplot() +
    geom_sf(data = africa_map_data, aes(fill = WS_SRCE_H_IOP), linewidth = .01, color = 'white') +
    labs(subtitle = "Percentage of households\nwith an improved water source\non the premises") +
    scale_fill_distiller(limits = c(0,1), direction = -1, palette = 'Spectral',  labels = scales::percent ) +
    ggplot() +
    geom_sf(data = africa_map_data, aes(fill = WS_TLET_H_IMP), linewidth = .01, color = 'white') +
    labs(subtitle = "Percentage of households\nwith an improved\nsanitation facility") +
    scale_fill_distiller(limits = c(0,1), direction = -1, palette = 'Spectral',  labels = scales::percent ) +
    ggplot() +
    geom_sf(data = africa_map_data, aes(fill = HC_ELEC_H_ELC), linewidth = .01, color = 'white') +
    labs(subtitle = "Percentage of households\nwith electricity") +
    scale_fill_distiller(limits = c(0,1), direction = -1, palette = 'Spectral',  labels = scales::percent )) /
  (ggplot() +
     geom_sf(data = africa_map_data, aes(fill = HC_FLRM_H_NAT), linewidth = .01, color = 'white') +
     labs(subtitle = "Percentage of households\nwith natural floors") +
     scale_fill_distiller(limits = c(0,1), direction = -1, palette = 'Spectral',  labels = scales::percent ) +
     ggplot() +
     geom_sf(data = africa_map_data, aes(fill = HC_PPRM_H_12P), linewidth = .01, color = 'white') +
     labs(subtitle = "Percentage of\nhouseholds with 1-2 persons\nper sleeping room") +
     scale_fill_distiller(limits = c(0,1), direction = -1, palette = 'Spectral',  labels = scales::percent ) +
     ggplot() +
     geom_sf(data = africa_map_data, aes(fill = HC_PPRM_H_34P), linewidth = .01, color = 'white') +
     labs(subtitle = "Percentage of\nhouseholds with 3-4 persons\nper sleeping room") +
     scale_fill_distiller(limits = c(0,1), direction = -1, palette = 'Spectral',  labels = scales::percent )) +
  #plot_layout(guides = 'collect') &
  plot_annotation(title = paste0('sub-Saharan Africa | Household Characteristics')) & 
  map_theme #+ theme(legend.position = 'bottom')
dhs_infra
ggsave(plot = dhs_infra, filename = paste0(wd_path,'/data/viz/maps/','africa-infra.pdf'), width = 10, height = 8)  

dhs_wealth <- (
  ggplot() +
    geom_sf(data = africa_map_data, aes(fill = HC_WIXQ_P_LOW), linewidth = .01, color = 'white') +
    labs(subtitle = "Percentage of population\nin the lowest wealth quintile") +
    scale_fill_distiller(limits = c(0,1), direction = -1, palette = 'Spectral',  labels = scales::percent ) +
    ggplot() +
    geom_sf(data = africa_map_data, aes(fill = HC_WIXQ_P_2ND), linewidth = .01, color = 'white') +
    labs(subtitle = "Percentage of population\nin the second wealth quintile") +
    scale_fill_distiller(limits = c(0,1), direction = -1, palette = 'Spectral',  labels = scales::percent ) +
    ggplot() +
    geom_sf(data = africa_map_data, aes(fill = HC_WIXQ_P_MID), linewidth = .01, color = 'white') +
    labs(subtitle = "Percentage of population\nin the middle wealth quintile") +
    scale_fill_distiller(limits = c(0,1), direction = -1, palette = 'Spectral',  labels = scales::percent )) /
  (ggplot() +
     geom_sf(data = africa_map_data, aes(fill = HC_WIXQ_P_4TH), linewidth = .01, color = 'white') +
     labs(subtitle = "Percentage of population\nin the fourth wealth quintile") +
     scale_fill_distiller(limits = c(0,1), direction = -1, palette = 'Spectral',  labels = scales::percent ) +
     ggplot() +
     geom_sf(data = africa_map_data, aes(fill = HC_WIXQ_P_HGH), linewidth = .01, color = 'white') +
     labs(subtitle = "Percentage of population\nin the highest wealth quintile") +
     scale_fill_distiller(limits = c(0,1), direction = -1, palette = 'Spectral',  labels = scales::percent ) +
     ggplot() +
     geom_sf(data = africa_map_data, aes(fill = HC_WIXQ_P_GNI), linewidth = .01, color = 'white') +
     labs(subtitle = "Gini coefficient\n") +
     scale_fill_distiller(direction = -1, palette = 'Spectral',  labels = scales::percent ))  +
  plot_annotation(title = paste0('sub-Saharan Africa | Wealth')) & 
  map_theme
dhs_wealth
ggsave(plot = dhs_wealth, filename = paste0(wd_path,'/data/dhs-analysis/viz/maps/africa-wealth.pdf'), width = 10, height = 8)  

dhs_k <- (
  ggplot() +
    geom_sf(data = africa_map_data, aes(fill = k_complexity_average), linewidth = .01, color = 'white') +
    labs(subtitle = "Population weighted\nblock complexity") +
    scale_fill_distiller(limits = c(0, max(africa_map_data %>% st_drop_geometry() %>% select(k_complexity_average) %>% pull() )), direction = -1, palette = 'Spectral',  labels = scales::comma ) + 
    ggplot() +
    geom_sf(data = africa_map_data, aes(fill = population_k_4_plus/landscan_population_un), linewidth = .01, color = 'white') +
    labs(subtitle = "Percentage of population\nin block with\n4+ layers of buildings") +
    scale_fill_distiller(limits = c(0,1), direction = -1, palette = 'Spectral',  labels = scales::percent ) +
    ggplot() +
    geom_sf(data = africa_map_data, aes(fill = (population_k_6+population_k_7+population_k_8+population_k_9+population_k_10plus+population_off_network)/landscan_population_un), linewidth = .01, color = 'white') +
    labs(subtitle = "Percentage of population\nin block with\n6+ layers of buildings") +
    scale_fill_distiller(limits = c(0,1), direction = -1, palette = 'Spectral',  labels = scales::percent )) /
  (ggplot() +
     geom_sf(data = africa_map_data, aes(fill = replace_na(na_if(na_if(na_if(log10(landscan_population_un / block_hectares), NaN), Inf), -Inf), 1) ), linewidth = .01, color = 'white') +
     labs(subtitle = "Population per hectare") +
     scale_fill_distiller(direction = -1, palette = 'Spectral', name = 'Population\nper hectare', oob = scales::squish, breaks= c(1,2,3), labels = c('1',"100","1K")) +
     ggplot() +
     geom_sf(data = africa_map_data, aes(fill = (region_core_urban+region_peripheral_urban) / landscan_population_un), linewidth = .01, color = 'white') +
     labs(subtitle = "Percentage of population\nin urban areas") +
     scale_fill_distiller(limits = c(0,1), direction = -1, palette = 'Spectral',  labels = scales::percent ) +
     ggplot() +
     geom_sf(data = africa_map_data, aes(fill = region_non_urban / landscan_population_un), linewidth = .01, color = 'white') +
     labs(subtitle = "Percentage of population\nin non-urban areas") +
     scale_fill_distiller(limits = c(0,1), direction = -1, palette = 'Spectral',  labels = scales::percent )) +
  plot_annotation(title = paste0('sub-Saharan Africa | Block complexity')) & 
  map_theme
dhs_k
ggsave(plot = dhs_k, filename = paste0(wd_path,'/data/dhs-analysis/viz/maps/africa-complexity.pdf'), width = 10, height = 8)  


# How to process street density for the DHS -------------------------------

# rsync -avz --exclude '*polygon*' nmarchio@midway.rcc.uchicago.edu:/project2/bettencourt/mnp/update/inputs/osm/parquet /Users/nm/Downloads/osm-line
dhs_list <- subnat_all_wide %>% select(ISO3_CountryCode) %>% st_drop_geometry() %>% distinct() %>% pull()

dhs_region_streets <- data.frame("ISO3_CountryCode" = as.character(),
                                 "REG_ID" = as.character(),
                                 "highway_length_meters" = as.numeric(),
                                 "region_area_m2" = as.numeric()) 

for (i in dhs_list) {
  print(i)
  
  highways <- arrow::open_dataset(paste0('osm-line/parquet/',i,'-linestring.parquet')) %>%
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

write_csv(dhs_region_streets, 'data/streets_dhs_regions.csv')

# osm_missing <- read_delim('https://geodata-eu-central-1-kontur-public.s3.eu-central-1.amazonaws.com/kontur_reports/osm_missing_roads.csv', delim = ';') %>%
#   group_by(Country) %>%
#   summarize_at(vars(`OSM roads length, km`, `Facebook roads length, km`), list(sum)) %>%
#   ungroup()

# Citations ---------------------------------------------------------------

# The DHS Program Indicator Data API, The Demographic and Health Surveys (DHS) Program. ICF International.
# Funded by the United States Agency for International Development (USAID). Available from api.dhsprogram.com. [Accessed 02-10-2023]
# ICF. The DHS Program Spatial Data Repository. Funded by USAID. spatialdata.dhsprogram.com.  [Accessed 02-10-2023].
# -
# ICF. 2010-2021. Demographic and Health Surveys (various) [Datasets]. Funded by USAID. Rockville, Maryland: ICF [Distributor].
# Instituto Nacional de Estatistica (INE) [Angola]. 2015. AO2015DHS.
# Institut National de la Statistique et de l’Analyse Économique (INSAE) [Benin]. 2012,2017. BJ2012DHS,BJ2017DHS.
# Institut National de la Statistique et de la Démographie (INSD) [Burkina Faso]. 2010,2014,2017. BF2010DHS,BF2014MIS,BF2017MIS.
# Institut National de la Statistique (INS) [Cameroon]. 2011,2018. CM2011DHS,CM2018DHS.
# Institut National de la Statistique des Études Économique et Démographiques (INSEED) [Chad]. 2014. TD2014DHS.
# Ministère du Plan (MDP) [Congo Democratic Republic]. 2013. CD2013DHS.
# Institut National de la Statistique (INS) [Cote d'Ivoire]. 2012. CI2012DHS.
# Direction Générale des Statistiques (DGS) [Gabon]. 2012. GA2012DHS.
# The Gambia Bureau of Statistics (GBOS) [Gambia]. 2019. GM2019DHS.
# Ghana Statistical Service (GSS) [Ghana]. 2014,2016,2019. GH2014DHS,GH2016MIS,GH2019MIS.
# Institut National de la Statistique (INS) [Guinea]. 2012,2018,2021. GN2012DHS,GN2018DHS,GN2021MIS.
# Ministry of Health (MOH) [Lesotho]. 2014. LS2014DHS.
# Liberia Institute of Statistics and Geo-information Services (LISGIS) [Liberia]. 2013,2016,2019. LB2013DHS,LB2016MIS,LB2019DHS.
# Institut National de la Statistique (INSTAT) [Mali]. 2015,2018,2021. ML2015MIS,ML2018DHS,ML2021MIS.
# Office National de la Statistique (ONS) [Mauritania]. 2020. MR2020DHS.
# Ministry of Health and Social Services (MOHSS) [Namibia]. 2013. NM2013DHS.
# Institut National de la Statistique (INS) [Niger]. 2012. NI2012DHS.
# National Population Commission (NPC) [Nigeria]. 2015, 2018, 2021. NG2015MIS,NG2018DHS,NG2021MIS.
# l’Agence Nationale de la Statistique et de la Démographie (ANSD) [Senegal]. 2018,2019,2020. SN2018DHS,SN2019DHS,SN2020MIS.
# Statistics Sierra Leone (SSL) [Sierra Leone]. 2013,2019. SL2013DHS,SL2019DHS.
# Statistics South Africa (STATS SA) [South Africa]. 2016. ZA2016DHS.
# Institut National de la Statistique des Etudes Economiques et Démographiques (INSEED) [Togo]. 2013,2017. TG2013DHS,TG2017MIS.
# -
# A slum household is defined as a household that lacks one of more of 5 basic services: access to improved water, access to improved sanitation, sufficient living area, quality/durability of structure and security of tenure. For this dataset, only the first four basic services were considered due to gaps in availability of data on security of tenure.
# The share of urban population living in slum households per country and region, based on 4 out of 5 household shelter deprivations defined by UN-Habitat as indicators of informality:
# 1. lack of access to improved water,
# 2. lack of access to improved sanitation,
# 3. lack of sufficient living area and quality/durability of structure. Security of tenure is the fifth deprivation that is not included due to data limitations.
# (a) A “slum household” is a household in which the inhabitants suffer one or more of the following “household deprivations”: 1) Lack of access to improved water services, 2) Lack of access to improved sanitation facilities, 3) Lack of sufficient living area, 4) Lack of housing durability and 5) Lack of security of tenure. For these calculations, only the first four deprivations were used.
# (b) Slum population calculated based on World Urbanization Prospects: The 2018 Revision
# Source: United Nations Human Settlement Programme (UN-Habitat), Global Urban Indicators Database 2020
# https://data.unhabitat.org/pages/housing-slums-and-informal-settlements
# -
# Population (%) with Improved Water, Improved Sanitation and Other Urban Basic Services in Cities, Selected Countries
# Source: United Nations Human Settlement Programme (UN-Habitat), Global Urban Indicators Database 2020
# https://data.unhabitat.org/pages/access-to-basic-services-in-cities-and-urban-areas
# -
# Citations metadata
# meta_surveys <- dhs_surveys() %>%
#   filter(SurveyId %in% unique(subnat_all_wide$SurveyId)) %>%
#   select(SurveyId, ImplementingOrg, SurveyType, SurveyYear, CountryName, PublicationDate, NumberofHouseholds, NumberOfMen, NumberOfWomen) %>%
#   mutate_if(., is.character, .funs = function(x){return(`Encoding<-`(x, "UTF-8"))}) %>%
#   mutate_at(vars(NumberOfMen, NumberOfWomen), as.numeric) %>%
#   replace_na(., list(NumberOfMen = 0, NumberOfWomen = 0)) %>%
#   mutate(SampleNumber = NumberOfMen + NumberOfWomen)
# write_excel_csv(meta_surveys, paste0(wd_path,'/data/metadata/','metadata.csv'))





