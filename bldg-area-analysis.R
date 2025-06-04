

library(ggplot2)
library(sf)
library(tidyverse)
library(viridis)
library(patchwork)
library(scales)
library(readxl)
library(readr)
library(writexl)
library(units)
library(arrow)
library(sfarrow)
library(fitdistrplus)
library(viridis)
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")
options(scipen = 9999)
options(lifecycle_verbosity = "warning")

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
source('aggregation_func.R') 

dir.create('data/bldg-area-analysis')

# -------------------------------------------------------------------------

log_10 <- function(x) {
  x = replace_na(na_if(na_if(na_if(log10(x), NaN), Inf), -Inf), 0)
  x = ifelse(x < 1, 1, x)
  return(x)
}

# Run this code to create the building distribution data file below:
# https://gist.github.com/nmarchio/5a955a3f5fba6d6c7389ac37a511accc

# -------------------------------------------------------------------------

# Download the prepped building distribution file
# dsbprylw7ncuq.cloudfront.net/_points/building_distribution_data.parquet
combo2 <- read_parquet('data/bldg-area-analysis/building_distribution_data.parquet')

# Building distributions all africa ---------------------------------------

histogram_buildings <- combo2 %>%
  mutate(class_urban_paper = factor(class_urban_paper, levels = c("1 - Urban", "2 - Secondary urban", '3 - Peri-urban', '4 - Rural'))) %>%
  rename(building_area = building_area_bucket) %>%
  mutate(bin_label = case_when(building_area < 5.6 ~ '< 5.6',
                               building_area >= 5.6 & building_area < 10 ~ '5.6 to\n10',
                               building_area >= 10 & building_area < 17 ~ '10 to\n17',
                               building_area >= 17 & building_area < 31 ~ '17 to\n31\n(parking\nspace)',
                               building_area >= 31 & building_area < 56 ~ '31 to\n56',
                               building_area >= 56 & building_area < 100 ~ '56 to\n100',
                               building_area >= 100 & building_area < 177 ~ '100 to\n177',
                               building_area >= 177 & building_area < 316 ~ '177 to\n316',
                               building_area >= 316 & building_area < 562 ~ '316 to\n562\n(basketball\ncourt)',
                               building_area >= 562 & building_area < 1000 ~ '562 to\n1K',
                               building_area >= 1000 ~ '> 1K'),
         bin_label2 = case_when(bin_label == '< 5.6' ~ '2.8',
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
  mutate( 
    bin_label = factor( bin_label , levels = c("< 5.6", "5.6 to\n10", "10 to\n17", "17 to\n31\n(parking\nspace)", "31 to\n56", "56 to\n100", "100 to\n177", "177 to\n316", "316 to\n562\n(basketball\ncourt)", "562 to\n1K", "> 1K")),
    bin_label2 = factor(bin_label2, levels = c('2.8', '7.8', '13.5\n(parking\nspace)', '24', '43.5', '78', '138.5', '246.5', '439\n(basketball\ncourt)', '781', '> 1K')))

hist_med <- histogram_buildings %>% 
  group_by(class_urban_paper) %>%
  arrange(building_area) %>%
  mutate(cumulative_sum = cumsum(count),
         total = sum(count)) %>%
  ungroup() %>%
  mutate(diff = cumulative_sum - total/2 ) %>%
  filter(diff > 0) %>%
  group_by(class_urban_paper) %>%
  mutate(median_row = row_number(-desc(diff))) %>%
  ungroup() %>%
  filter(median_row == 1)

histogram_buildings_sum <- histogram_buildings %>%
  group_by(bin_label2, class_urban_paper ) %>%
  summarize(count = sum(count)) %>%
  ungroup()  

(bldgs_bar <- ggplot(data = histogram_buildings_sum, aes(x = bin_label2, y = count, 
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

histogram_buildings_synth <- histogram_buildings %>%
  group_by(class_urban_paper, building_area) %>%
  summarise(count = sum(count)) %>%
  ungroup() %>%
  mutate(count =  round(count/100,0) ) %>%
  uncount(count) 


(bldgs_density <- ggplot() + 
    geom_density(data = histogram_buildings_synth , linewidth = 1, bw = 2,
                 aes(x = building_area, #  y = after_stat(density),
                     color = class_urban_paper, group = class_urban_paper), alpha = .6) +
    scale_x_continuous(expand = c(0.05,0.05), 
                       limits = c(0,120),
                       labels = c('2.8', '7.8', '13.5\n(parking\nspace)', '24', '43.5', '78', '138.5', '246.5', '439\n(basketball\ncourt)', '781', '> 1K'),
                       breaks = c(2.8,7.8,13.5,24,43.5,78,138.5,246.5, 439, 781, 1000)) +
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

#ggsave(plot = bldgs_charts, filename = 'data/bldg-area-analysis/bldgs_charts.pdf', width = 10, height = 8) # dpi = 300, 
ggsave(plot = bldgs_charts, filename = 'data/bldg-area-analysis/bldgs_charts.pdf', width = 15.672, height = 8) # dpi = 300, 

# rm(bldgs_charts, bldgs_density, bldgs_bar)
