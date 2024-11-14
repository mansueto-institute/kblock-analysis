
library(dplyr)
library(tidyverse)
library(arrow)
library(sfarrow)
library(sf)
library(fitdistrplus)
library(scales)
library(patchwork)
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")

options(scipen = 99999)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
source('aggregation_func.R') 

# Aggregate block level data ----------------------------------------------

if (!file.exists(paste0("data/africa_data.parquet"))) {
  curl::multi_download("dsbprylw7ncuq.cloudfront.net/AF/africa_data.parquet", "data/africa_data.parquet", resume = TRUE)
} else {
  df_combined <- read_parquet(paste0("data/africa_data.parquet"))
}

sum_cols = c("k_complexity_weighted_landscan_un", "landscan_population_un")
divide_cols = list('k_complexity_weighted_landscan_un' = c('k_complexity_weighted_landscan_un','landscan_population_un'))
transform_cols = c("k_complexity_weighted_landscan_un", 'landscan_population_un')

# Aggregate country level by k
df_country <- generate_crosstabs(data =  df_combined,
                           group_by_cols = c("country_code", "country_name"),
                           crosstab_cols = c("k_complexity"),
                           sum_cols = sum_cols, divide_cols = divide_cols, 
                           transform_cols = transform_cols, transform_func_list = list(log10 = log_10),
                           group_by_agg_cols_list = list('country' = "country_code"),
                           group_by_agg_func_list = list(sum = sum, share = share),
                           agg_cols = c('landscan_population_un'))

# Aggregate country level total
df_country_avg  <- generate_crosstabs(data = df_combined %>% mutate(total = 1), 
                                 group_by_cols = c("country_code", "country_name"),
                                 crosstab_cols = c('total'),
                                 sum_cols = sum_cols, divide_cols = divide_cols, 
                                 transform_cols = transform_cols, transform_func_list = list(log10 = log_10),
                                 group_by_agg_cols_list = list('country' = "country_code"),
                                 group_by_agg_func_list = list(sum = sum, share = share),
                                 agg_cols = c('landscan_population_un'))

# Aggregate urban level by k
df_urban <- generate_crosstabs(data =  df_combined,
                           group_by_cols = c("urban_id", "urban_center_name", "class_urban_hierarchy", "urban_country_code", "urban_country_name"),
                           crosstab_cols = c("k_complexity"),
                           sum_cols = sum_cols, divide_cols = divide_cols, 
                           transform_cols = transform_cols, transform_func_list = list(log10 = log_10),
                           group_by_agg_cols_list = list('urban' = "urban_id"),
                           group_by_agg_func_list = list(sum = sum, share = share),
                           agg_cols = c('landscan_population_un'))

# Aggregate urban level total
df_urban_avg  <- generate_crosstabs(data = df_combined %>% mutate(total = 1), 
                                      group_by_cols = c("urban_id", "urban_center_name", "class_urban_hierarchy", "urban_country_code", "urban_country_name"),
                                      crosstab_cols = c("total"),
                                      sum_cols = sum_cols, divide_cols = divide_cols, 
                                      transform_cols = transform_cols, transform_func_list = list(log10 = log_10),
                                      group_by_agg_cols_list = list('urban' = "urban_id"),
                                      group_by_agg_func_list = list(sum = sum, share = share),
                                      agg_cols = c('landscan_population_un'))

# Filter to top 50 cities
df_urban_avg <- df_urban_avg %>%
  filter(class_urban_hierarchy == '1 - Core urban') %>%
  mutate(size_rank = row_number(desc(landscan_population_un))) %>%
  filter(size_rank <= 50)

# Filter to top 50 cities
df_urban <- df_urban %>% 
  filter(urban_id %in% unique(df_urban_avg$urban_id))

# Aggregate continent level by k
df_continent <- generate_crosstabs(data =  df_combined,
                               group_by_cols = c("continent"),
                               crosstab_cols = c("k_complexity"),
                               sum_cols = sum_cols, divide_cols = divide_cols, 
                               transform_cols = transform_cols, transform_func_list = list(log10 = log_10),
                               group_by_agg_cols_list = list('continent' = "continent"),
                               group_by_agg_func_list = list(sum = sum, share = share),
                               agg_cols = c('landscan_population_un'))

# Aggregate urban level by k
df_urban_level <- generate_crosstabs(data =  df_combined,
                               group_by_cols = c("continent", "class_urban_periurban_nonurban"),
                               crosstab_cols = c("k_complexity"),
                               sum_cols = sum_cols, divide_cols = divide_cols,
                               transform_cols = transform_cols, transform_func_list = list(log10 = log_10),
                               group_by_agg_cols_list = list('continent' = "continent"),
                               group_by_agg_func_list = list(sum = sum, share = share),
                               agg_cols = c('landscan_population_un'))

df_country <- df_country %>%
  select(group_var, group_val, 
                country_code, country_name, 
                landscan_population_un, landscan_population_un_log10) %>%
  rename(geo_code = country_code, 
         geo_name = country_name) %>%
  mutate(geo_group = 'country')
  
df_urban <- df_urban %>%
  filter(class_urban_hierarchy != '4 - Non-urban', class_urban_hierarchy != '3 - Peri-urban') %>%
  group_by(urban_id) %>%
  mutate(agg_sum_landscan_population_un = sum(landscan_population_un)) %>%
  ungroup() %>%
  filter(class_urban_hierarchy == '1 - Core urban'
         #,agg_sum_landscan_population_un >= 2000000
         ) %>%
  mutate(urban_country_code =  (gsub('–', '-', as.character(urban_country_code)))) %>%
  mutate(urban_name = paste0(urban_center_name,', ',urban_country_code)) %>%
  dplyr::select(group_var, group_val, 
         urban_id, urban_name,
         landscan_population_un, landscan_population_un_log10) %>%
  rename(geo_code = urban_id, 
         geo_name = urban_name) %>%
  mutate(geo_group = 'urban')

df_urban_level <- df_urban_level %>%
  dplyr::select(group_var, group_val, 
                class_urban_periurban_nonurban,
                landscan_population_un, landscan_population_un_log10) %>%
  mutate(geo_code = class_urban_periurban_nonurban) %>%
  rename(geo_name = class_urban_periurban_nonurban) %>%
  mutate(geo_group = 'urbanlevel')

df_continent <- df_continent %>%
  mutate(continent_code = continent) %>%
  dplyr::select(group_var, group_val, 
         continent_code, continent, 
         landscan_population_un, landscan_population_un_log10) %>%
  rename(geo_code = continent_code, 
         geo_name = continent) %>%
  mutate(geo_group = 'continent')

# Combine all aggregation levels
combined_distributions <- rbind(df_continent, df_country, df_urban, df_urban_level)

combined_distributions <- combined_distributions %>%
  group_by(geo_code) %>%
  mutate(share_landscan_population_un = landscan_population_un/sum(landscan_population_un)) %>%
  ungroup() 

# write_csv(combined_distributions_uncount , 'dhs-analysis/data/distributions_synth.csv')

results = list(geo_group = c(), geo_code = c(), geo_name = c(), method = c(), distname = c(), loglik = c(), aic = c(), bic = c(), n = c(), param1_name = c(), param1_val = c(), param2_name = c(), param2_val = c()) %>% as.data.frame()

# Distributions to fit
# moment matching estimation for Poisson
# maximum likelihood estimation for Log normal, Exponential, Gamma, Logistic, Negative binomial, Geometric, Weibull 
dist_list <- list(method = c("mme", "mle", "mle", "mle", "mle", "mle", "mle", "mle"),
                  distri = c("pois", "lnorm", "exp", "gamma", "logis", "nbinom", "geom", "weibull")) #%>% as.data.frame()

# Loop through geographies and fit each distribution type
for (i in unique(combined_distributions$geo_code)) {
  print(i)
  subset <- combined_distributions %>%
    filter(geo_code == i)
  if (nrow(subset) > 5) {
    out <- map2_dfr(.x = dist_list[[1]], .y = dist_list[[2]], .f = function(x , y) {
      print(y)
      res <- fitdistrplus::fitdist(data = subset$group_val, method = x, distr = y, weights = round(subset$landscan_population_un,0))
      result <- list(geo_group = unique(subset$geo_group), geo_code = i, geo_name = unique(subset$geo_name), method = res$method, distname = res$distname, 
                     loglik = res$loglik, aic = res$aic, bic = res$bic, n = res$n,
                     param1_name = names(res$estimate[1]), param1_val = pluck(res$estimate, 1), param2_name = names(res$estimate[2]), param2_val = pluck(res$estimate, 2))
      return(result)
    })
    results <- rbind(results, out)
  }
}

# Assemble results
results_full <- results %>% 
  mutate(distribution = case_when(distname == "pois" ~ "Poisson",
                                  distname == "lnorm" ~ "Log-normal",
                                  distname == "exp" ~ "Exponential",
                                  distname == "unif" ~ "Uniform",
                                  distname == "gamma" ~ "Gamma",
                                  distname == "logis" ~ "Logistic",
                                  distname == "nbinom" ~ "Negative binomial",
                                  distname == "geom" ~ "Geometric",
                                  distname == "cauchy" ~ "Cauchy",
                                  distname == "norm" ~ "Normal",
                                  distname == "weibull" ~ "Weibull",
                                  TRUE ~ ''),
         dist_desc = case_when(distname == "pois" ~ paste0("λ = ",round(param1_val,3)),
                                   distname == "lnorm" ~ paste0("μ = ",round(param1_val,3), " σ = ",round(param2_val,3)), # \mu \sigma
                                   distname == "exp" ~ paste0("μ = ",round(param1_val,3)),
                                   distname == "gamma" ~ paste0("α = ",round(param1_val,3), " β = ",round(param2_val,3)), # \alpha # \beta
                                   distname == "logis" ~ paste0("μ = ",round(param1_val,3), " σ = ",round(param2_val,3)),
                                   distname == "nbinom" ~ paste0("φ= ",round(param1_val,3), " μ = ",round(param2_val,3)), # \phi
                                   distname == "geom" ~ paste0("G(p) = ",round(param1_val,3)),
                                   distname == "weibull" ~ paste0("k= ",round(param1_val,3), " λ = ",round(param2_val,3)), # \lambda
                                   TRUE ~ ''),
         dist_desc_latex = case_when(distname == "pois" ~ paste0("$\\lambda$ = ",round(param1_val,3)),
                               distname == "lnorm" ~ paste0("$\\mu$ = ",round(param1_val,3), " $\\sigma$ = ",round(param2_val,3)), # \mu \sigma
                               distname == "exp" ~ paste0("$\\mu$ = ",round(param1_val,3)),
                               distname == "gamma" ~ paste0("$\\alpha$ = ",round(param1_val,3), " $\\beta$ = ",round(param2_val,3)), # \alpha # \beta
                               distname == "logis" ~ paste0("$\\mu$ = ",round(param1_val,3), " $\\sigma$ = ",round(param2_val,3)),
                               distname == "nbinom" ~ paste0("$\\phi$ = ",round(param1_val,3), " $\\mu$ = ",round(param2_val,3)), # \phi
                               distname == "geom" ~ paste0("\\textit{G(p)} = ",round(param1_val,3)),
                               distname == "weibull" ~ paste0("\\textit{k} = ",round(param1_val,3), " $\\lambda$ = ",round(param2_val,3)), # \lambda
                               TRUE ~ '')
  ) %>%
  group_by(geo_code, geo_name) %>%
  mutate(best_fit_rank = row_number(-desc(bic))) %>%
  ungroup()
  
# Filter to best fit distribution
results_best <- results_full %>%
  filter(best_fit_rank == 1)

results_best %>%
  filter(geo_group == 'country') %>%
  group_by(distname) %>%
  tally() %>%
  mutate(shr = percent(n/sum(n)))

results_best %>%
  filter(geo_group == 'urban') %>%
  group_by(distname) %>%
  tally() %>%
  mutate(shr = percent(n/sum(n)))

write_csv(results_best, 'data/dhs-analysis/data/distributions_output.csv')

# Distribution chart function ---------------------------------------------

generate_distribution_plots <- function(area_code, area_label, title_size = 14, subtitle_size = 10) {
  
  if (area_label == '') {
    area_label <- combined_distributions %>% filter(geo_code == area_code) %>%
      select(geo_name) %>% distinct() %>% pull()
  }
  
  fitted_res <- results_best %>% filter(geo_code == area_code) %>% dplyr::select(geo_code, geo_name, param1_val, param2_val, distribution, dist_desc)
  
  fit_lnorm <- results %>% filter(geo_code == area_code) %>% filter(distname == 'lnorm') %>% dplyr::select(param1_val, param2_val)
  fit_gamma <- results %>% filter(geo_code == area_code) %>% filter(distname == 'gamma') %>% dplyr::select(param1_val, param2_val)
  fit_logis <- results %>% filter(geo_code == area_code) %>% filter(distname == 'logis') %>% dplyr::select(param1_val, param2_val)
  fit_weibu <- results %>% filter(geo_code == area_code) %>% filter(distname == 'weibull') %>% dplyr::select(param1_val, param2_val)
  fit_pois <- results %>% filter(geo_code == area_code) %>% filter(distname == 'pois') %>% dplyr::select(param1_val, param2_val)
  
  #bins_count <- combined_distributions %>% filter(geo_code == area_code) %>% filter(group_val <= 20)
  
  ggplot(combined_distributions %>% filter(geo_code == area_code) %>% filter(group_val <= 20), 
         aes(x = group_val, weight = landscan_population_un))+
    geom_histogram(binwidth=1, aes(y = after_stat(density)), alpha = .4, color = '#333333') +
    stat_function(fun = dlnorm,   linewidth = 1, xlim = c(0,20), args = list(mean = fit_lnorm[[1]], sd = fit_lnorm[[2]]), aes(color = "Log-normal")) + 
    stat_function(fun = dgamma,   linewidth = 1, xlim = c(0,20), args = list(shape = fit_gamma[[1]], rate = fit_gamma[[2]]), aes(color = "Gamma")) + 
    stat_function(fun = dlogis,   linewidth = 1, xlim = c(0,20), args = list(location = fit_logis[[1]], scale = fit_logis[[2]]), aes(color = "Logistic")) +
    stat_function(fun = dweibull, linewidth = 1, xlim = c(0,20), args = list(shape = fit_weibu[[1]], scale = fit_weibu[[2]]), aes(color = 'Weibull')) +
    geom_function(fun = dpois,    linewidth = 1, xlim = c(0,20), n=21, args = list(lambda = fit_pois[[1]]), aes(color = 'Poisson')) +
    scale_color_manual(values = c("Log-normal"= '#2C73D2' , "Gamma"= '#D65DB1', "Logistic" = '#FF9671', 'Weibull'= '#F9F871', 'Poisson' = '#00C9A7')) +
    guides(color=guide_legend(title="Distribution")) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    labs(title = area_label, subtitle = paste0('Best fit: ', unique(fitted_res$distribution),' ',unique(fitted_res$dist_desc)), x = 'Block complexity', y = 'Density') +
    theme_classic() + theme(plot.title = element_text(face = 'bold', size = 14), 
                            plot.subtitle = element_text(face = 'italic', size = 10)) 
  
}

# Create distribution charts ----------------------------------------------

africa_chart <- generate_distribution_plots(area_code = 'Africa', area_label = 'sub-Saharan Africa')
urban_chart <- generate_distribution_plots(area_code = '1 - Core & peripheral urban', area_label = 'Urban areas')
peri_chart <- generate_distribution_plots(area_code = '2 - Peri-urban', area_label = 'Peri-urban areas')
rural_chart <- generate_distribution_plots(area_code = '3 - Non-urban', area_label = 'Rural areas')

(agg_chart <- africa_chart + urban_chart + peri_chart + rural_chart +
  plot_layout(guides = 'collect') & 
  plot_annotation(tag_levels = 'A') & 
  theme(legend.position = 'bottom', plot.caption = element_text(hjust = 0)))

ggsave(plot = agg_chart, 
       filename = 'data/dhs-analysis/viz/africa_dist_combo.pdf', width = 11, height = 8, device = cairo_pdf)


chart_lagos <- generate_distribution_plots(area_code = 'ghsl_2125', area_label = 'Lagos, NGA', title_size = 10, subtitle_size = 8) 
chart_kinshasa <- generate_distribution_plots(area_code = 'ghsl_3209', area_label = 'Kinshasa, COD-COG', title_size = 10, subtitle_size = 8) 
chart_luanda <- generate_distribution_plots(area_code = 'ghsl_3050', area_label = 'Luanda, AGO', title_size = 10, subtitle_size = 8) 
chart_johannesburg <- generate_distribution_plots(area_code = 'ghsl_3673', area_label = 'Johannesburg, ZAF', title_size = 10, subtitle_size = 8) 
chart_addis <- generate_distribution_plots(area_code = 'ghsl_5134', area_label = 'Addis Ababa, ETH', title_size = 10, subtitle_size = 8) 
chart_khartoum <- generate_distribution_plots(area_code = 'ghsl_4335', area_label = 'Khartoum, SDN', title_size = 10, subtitle_size = 8) 
chart_dar <- generate_distribution_plots(area_code = 'ghsl_5222', area_label = 'Dar es Salaam, TZA', title_size = 10, subtitle_size = 8) 
chart_abidjan <- generate_distribution_plots(area_code = 'ghsl_1675', area_label = 'Abidjan, CIV', title_size = 10, subtitle_size = 8) 
chart_accra <- generate_distribution_plots(area_code = 'ghsl_1910', area_label = 'Accra, GHA', title_size = 10, subtitle_size = 8) 
chart_nairobi <- generate_distribution_plots(area_code = 'ghsl_4808', area_label = 'Nairobi, KEN', title_size = 10, subtitle_size = 8) 
chart_kampala <- generate_distribution_plots(area_code = 'ghsl_4427', area_label = 'Kampala, UGA', title_size = 10, subtitle_size = 8) 
chart_dakar <- generate_distribution_plots(area_code = 'ghsl_1452', area_label = 'Dakar, SEN', title_size = 10, subtitle_size = 8) 

(urban_combo_chart <- chart_lagos + chart_kinshasa + chart_luanda + chart_johannesburg + 
    chart_addis + chart_khartoum + chart_dar + chart_abidjan +
    chart_accra + chart_nairobi + chart_kampala + chart_dakar +
    plot_layout(guides = 'collect') & 
    #plot_annotation(tag_levels = 'A') & 
    plot_annotation(tag_levels = list(c("E", "F", "G", "H", 'I', 'J', "K", "L", "M", "N", 'O', 'P'))) &
    theme(legend.position = 'bottom', plot.caption = element_text(hjust = 0)))

library(Cairo)

ggsave(plot = urban_combo_chart, 
       filename = 'data/dhs-analysis/viz/urban_dist_combo.pdf', width = 16, height = 12, device = cairo_pdf)

# chart_kano <- generate_distribution_plots(area_code = 'ghsl_2717', area_label = 'Kano, NGA')
# chart_cape <- generate_distribution_plots(area_code = 'ghsl_3268', area_label = 'Cape Town, ZAF')
# chart_ibadan <- generate_distribution_plots(area_code = 'ghsl_2189', area_label = 'Ibadan, NGA')
# chart_durban <- generate_distribution_plots(area_code = 'ghsl_3868', area_label = 'Durban, ZAF')
# chart_kumasi <- generate_distribution_plots(area_code = 'ghsl_1785', area_label = 'Kumasi, GHA')
# chart_yaounde <- generate_distribution_plots(area_code = 'ghsl_2961', area_label = 'Yaounde, CMR')
# chart_douala <- generate_distribution_plots(area_code = 'ghsl_2850', area_label = 'Douala, CMR')
# chart_onitsha <- generate_distribution_plots(area_code = 'ghsl_2484', area_label = 'Onitsha, NGA')
# chart_bamako <- generate_distribution_plots(area_code = 'ghsl_1553', area_label = 'Bamako, MLI')
# chart_maputo <- generate_distribution_plots(area_code = 'ghsl_4220', area_label = 'Maputo, MOZ')
# chart_conakry <- generate_distribution_plots(area_code = 'ghsl_1502', area_label = 'Conakry, GIN')
# chart_ouagadougou <- generate_distribution_plots(area_code = 'ghsl_1799', area_label = 'Ouagadougou, BFA')
# chart_mogadishu <- generate_distribution_plots(area_code = 'ghsl_5765', area_label = 'Mogadishu, SOM')
# chart_lusaka <- generate_distribution_plots(area_code = 'ghsl_3798', area_label = 'Lusaka, ZMB')
# chart_antananarivo <- generate_distribution_plots(area_code = 'ghsl_5792', area_label = 'Antananarivo, MDG')

# Create best fit table for paper --------------------------------------------------

results_best %>%
  filter(geo_group == 'country') %>%
  group_by(distname) %>%
  tally() %>%
  mutate(shr = percent(n/sum(n)))

results_best %>%
  filter(geo_group == 'urban') %>%
  group_by(distname) %>%
  tally() %>%
  mutate(shr = percent(n/sum(n)))
  
country_dist_tbl1 <- results_best %>%
  filter(geo_group == 'country') %>%
  select(geo_code, geo_name, distribution, dist_desc_latex) %>%
  left_join(., df_country_avg %>% select(country_code, k_complexity_weighted_landscan_un), by = c('geo_code' = 'country_code'))

urban_dist_tbl1 <- results_best %>%
  filter(geo_group == 'urban') %>%
  select(geo_code, geo_name, distribution, dist_desc_latex) %>%
  left_join(., df_urban_avg %>% select(urban_id, k_complexity_weighted_landscan_un), by = c('geo_code' = 'urban_id'))

country_dist_tbl1 <- country_dist_tbl1 %>% select(geo_name, distribution, dist_desc_latex) %>%
  mutate(country_dist = paste0(distribution,' (',dist_desc_latex,')'),
         order = row_number(-desc(geo_name))) %>%
  select(order, geo_name, country_dist) %>% rename(country = geo_name)
urban_dist_tbl1 <- urban_dist_tbl1 %>% select(geo_name, distribution, dist_desc_latex) %>%
  mutate(urban_dist = paste0(distribution,' (',dist_desc_latex,')'),
         order = row_number(-desc(geo_name))) %>%
  select(order, geo_name, urban_dist) %>% rename(urban = geo_name)

dist_tbl1 <- country_dist_tbl1 %>%
  left_join(., urban_dist_tbl1, by = c('order'='order'))

write_excel_csv(x = dist_tbl1, file = 'data/dhs-analysis/data/dist_tbl.csv')

