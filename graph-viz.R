

library(tidyverse)
library(sf)
library(viridis)
library(patchwork)
library(scales)
library(units)
options(scipen = 999)
options(lifecycle_verbosity = "warning")

# Read in functions -------------------------------------------------------

source('graph_funcs.R')

# Creat output directory
dir.create('graph-viz')
wd_output = 'graph-viz'

# Import data -------------------------------------------------------------

layers <- st_read('data/layer_lusaka.geojson')

graph_data <- layers %>% filter(block_id %in% c("ZMB.5.6_2_9371"))

grey2 <- c('#E8E7F4','#777777') 
kdist = max(as.integer(graph_data$k_complexity))
colorhexes <- colorRampPalette(c('#93328E','#CF3F80','#F7626B','#FF925A','#FFC556','#F9F871'))(length(unique(graph_data$k_complexity))-2)

graph_points <- graph_data %>% filter(block_property == 'building-parcels',
                                      block_id %in% c("ZMB.5.6_2_9371")) %>%
  st_cast(., "POLYGON") %>%
  st_point_on_surface() 

# Visualizations ----------------------------------------------------------

poly_in <- graph_data %>% filter(block_property == 'building-parcels') %>% 
  st_cast(., "POLYGON")

line_in <- graph_data %>% filter(block_property == 'on-network-streets') %>% 
  select(block_property)

graph_lines <- generate_graph(graph_polys = poly_in, graph_exclude = line_in)
graph_polys <- convert_to_poly(lines = graph_lines)

graph_duals <- extract_duals(graph_sf = graph_polys)


(vizgraph <- ggplot() +
    geom_sf(data = graph_data %>% filter(block_property == 'building-parcels'), 
            aes(fill = as.factor(k_complexity)), color = 'white', 
            alpha = .8, linewidth = .2)  +
    geom_sf(data = graph_lines, color = '#333333', alpha = .8, linewidth = .4) +
    geom_sf(data = graph_points, color = 'black', alpha = .8, size = .9, linewidth = .01) +
    geom_sf(data = graph_points, color = 'white', alpha = .8, size = .5, linewidth = .01) +
    geom_sf(data = line_in, fill = '#4E8397', color = '#4E8397', alpha = .8, linewidth = 1.5, lineend = 'round') +
    scale_fill_manual(expand = c(0,0), name = 'Block complexity', values = c(grey2, colorhexes), breaks = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10+', 'Off\nnetwork')) +
    labs(subtitle = '') +
    theme_void() + theme(legend.title = element_text(size = 14, face = 'bold'),
                         legend.text = element_text(size = 14),
                         legend.position = 'none',
                         plot.subtitle = element_text(size = 10),
                         axis.text = element_blank()))

alpha_param = .3
(vizlattice <- ggplot() +
    geom_sf(data = poly_in %>% st_union(), color = 'black', alpha = 0, size = .3, linewidth = .06) +
    geom_sf(data = graph_lines, color = '#333333', alpha = .8, linewidth = .4) +
    geom_sf(data = graph_points, color = 'black', alpha = .8, size = .9, linewidth = .01) +
    geom_sf(data = graph_duals %>% filter(part == 'dual') %>% filter(iteration %in% c(1)), aes(fill = as.character(iteration)), alpha = alpha_param) +
    geom_sf(data = graph_duals %>% filter(part == 'dual') %>% filter(iteration %in% c(2)), aes(fill = as.character(iteration)), alpha = alpha_param) +
    geom_sf(data = graph_duals %>% filter(part == 'dual') %>% filter(iteration %in% c(3)), aes(fill = as.character(iteration)), alpha = alpha_param) +
    geom_sf(data = graph_duals %>% filter(part == 'dual') %>% filter(iteration %in% c(4)), aes(fill = as.character(iteration)), alpha = alpha_param) +
    geom_sf(data = graph_duals %>% filter(part == 'dual') %>% filter(iteration %in% c(5)), aes(fill = as.character(iteration)), alpha = alpha_param) +
    geom_sf(data = graph_duals %>% filter(part == 'dual') %>% filter(iteration %in% c(6)), aes(fill = as.character(iteration)), alpha = alpha_param) +
    geom_sf(data = graph_duals %>% filter(part == 'dual') %>% filter(iteration %in% c(7)), aes(fill = as.character(iteration)), alpha = alpha_param) +
    geom_sf(data = graph_duals %>% filter(part == 'dual') %>% filter(iteration %in% c(8)), aes(fill = as.character(iteration)), alpha = alpha_param) +
    scale_fill_manual(expand = c(0,0), values = c(grey2, colorhexes), breaks = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10+', 'Off\nnetwork')) +
    # .2 without fill, 
    theme_void() + theme(legend.position = 'none') )
  
(vizgl <- vizgraph + vizlattice &
    plot_annotation(tag_levels = list(c("A","B"))) & 
    theme(plot.tag = element_text(size = 13)))

ggsave(plot = vizgl, filename = paste0(wd_output,'/complexity-graph.pdf'),
       width = 12.3, height = 5)

ggsave(plot = vizgraph, filename = paste0(wd_output,'/complexity-graph_parta.pdf'),
       width = 6.15, height = 5)

(vizdual <- vizlattice +
    # ggplot() +
    # #geom_sf(data = poly_in, aes(fill = as.factor(k_complexity)), color = 'white', alpha = .8, linewidth = .2) +
    # geom_sf(data = poly_in, color = 'black', alpha = 0, size = .3, linewidth = .06) +
    # geom_sf(data = graph_points, color = 'black', alpha = .8, size = .3, linewidth = .01) +
    # #geom_sf(data = graph_duals %>% filter(part == 'primal') %>% filter(iteration %in% c(0)), aes(fill = as.character(iteration)), alpha = .9) +
    # scale_fill_manual(values = c(grey2, colorhexes), breaks = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10+', 'Off\nnetwork')) +
    # theme_void() + theme(legend.position = 'none') +
    ggplot() +
    #geom_sf(data = graph_points, color = 'black', alpha = .8, size = .3, linewidth = .01) +
    geom_sf(data = poly_in, color = 'black', alpha = 0, size = .3, linewidth = .06) +
    geom_sf(data = graph_duals %>% filter(part == 'primal') %>% filter(iteration %in% c(1)), aes(fill = as.character(iteration)), alpha = 6) +
    geom_sf(data = graph_duals %>% filter(part == 'dual') %>% filter(iteration %in% c(1)), aes(fill = as.character(iteration)), alpha = .2) +
    scale_fill_manual(values = c(grey2, colorhexes), breaks = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10+', 'Off\nnetwork')) +
    theme_void() + theme(legend.position = 'none') +
    ggplot() +
    #geom_sf(data = graph_points, color = 'black', alpha = .8, size = .3, linewidth = .01) +
    geom_sf(data = poly_in, color = 'black', alpha = 0, size = .3, linewidth = .06) +
    geom_sf(data = graph_duals %>% filter(part == 'primal') %>% filter(iteration %in% c(2)), aes(fill = as.character(iteration)), alpha = 6) +
    geom_sf(data = graph_duals %>% filter(part == 'dual') %>% filter(iteration %in% c(2)), aes(fill = as.character(iteration)), alpha = .2) +
    scale_fill_manual(values = c(grey2, colorhexes), breaks = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10+', 'Off\nnetwork')) +
    theme_void() + theme(legend.position = 'none') +
    ggplot() +
    #geom_sf(data = graph_points, color = 'black', alpha = .8, size = .3, linewidth = .01) +
    geom_sf(data = poly_in, color = 'black', alpha = 0, size = .3, linewidth = .06) +
    geom_sf(data = graph_duals %>% filter(part == 'primal') %>% filter(iteration %in% c(3)), aes(fill = as.character(iteration)), alpha = .6) +
    geom_sf(data = graph_duals %>% filter(part == 'dual') %>% filter(iteration %in% c(3)), aes(fill = as.character(iteration)), alpha = .2) +
    scale_fill_manual(values = c(grey2, colorhexes), breaks = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10+', 'Off\nnetwork')) +
    theme_void() + theme(legend.position = 'none') +
    ggplot() +
    #geom_sf(data = graph_points, color = 'black', alpha = .8, size = .3, linewidth = .01) +
    geom_sf(data = poly_in, color = 'black', alpha = 0, size = .3, linewidth = .06) +
    geom_sf(data = graph_duals %>% filter(part == 'primal') %>% filter(iteration %in% c(4)), aes(fill = as.character(iteration)), alpha = .6) +
    geom_sf(data = graph_duals %>% filter(part == 'dual') %>% filter(iteration %in% c(4)), aes(fill = as.character(iteration)), alpha = .2) +
    scale_fill_manual(values = c(grey2, colorhexes), breaks = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10+', 'Off\nnetwork')) +
    theme_void() + theme(legend.position = 'none') +
    ggplot() +
    #geom_sf(data = graph_points, color = 'black', alpha = .8, size = .3, linewidth = .01) +
    geom_sf(data = poly_in, color = 'black', alpha = 0, size = .3, linewidth = .06) +
    geom_sf(data = graph_duals %>% filter(part == 'primal') %>% filter(iteration %in% c(5)), aes(fill = as.character(iteration)), alpha = .6) +
    geom_sf(data = graph_duals %>% filter(part == 'dual') %>% filter(iteration %in% c(5)), aes(fill = as.character(iteration)), alpha = .2) +
    scale_fill_manual(values = c(grey2, colorhexes), breaks = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10+', 'Off\nnetwork')) +
    theme_void() + theme(legend.position = 'none') +
    ggplot() +
    #geom_sf(data = graph_points, color = 'black', alpha = .8, size = .3, linewidth = .01) +
    geom_sf(data = poly_in, color = 'black', alpha = 0, size = .3, linewidth = .06) +
    geom_sf(data = graph_duals %>% filter(part == 'primal') %>% filter(iteration %in% c(6)), aes(fill = as.character(iteration)), alpha = .6) +
    geom_sf(data = graph_duals %>% filter(part == 'dual') %>% filter(iteration %in% c(6)), aes(fill = as.character(iteration)), alpha = .2) +
    scale_fill_manual(values = c(grey2, colorhexes), breaks = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10+', 'Off\nnetwork')) +
    theme_void() + theme(legend.position = 'none') +
    ggplot() +
    #geom_sf(data = graph_points, color = 'black', alpha = .8, size = .3, linewidth = .01) +
    geom_sf(data = poly_in, color = 'black', alpha = 0, size = .3, linewidth = .06) +
    geom_sf(data = graph_duals %>% filter(part == 'primal') %>% filter(iteration %in% c(7)), aes(fill = as.character(iteration)), alpha = .6) +
    geom_sf(data = graph_duals %>% filter(part == 'dual') %>% filter(iteration %in% c(7)), aes(fill = as.character(iteration)), alpha = .2) +
    scale_fill_manual(values = c(grey2, colorhexes), breaks = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10+', 'Off\nnetwork')) +
    theme_void() + theme(legend.position = 'none') +
    ggplot() +
    #geom_sf(data = graph_points, color = 'black', alpha = .8, size = .3, linewidth = .01) +
    geom_sf(data = poly_in, color = 'black', alpha = 0, size = .3, linewidth = .06) +
    geom_sf(data = graph_duals %>% filter(part == 'primal') %>% filter(iteration %in% c(8)), aes(fill = as.character(iteration)), alpha = .6) +
    geom_sf(data = graph_duals %>% filter(part == 'dual') %>% filter(iteration %in% c(8)), aes(fill = as.character(iteration)), alpha = .2) +
    scale_fill_manual( values = c(grey2, colorhexes), breaks = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10+', 'Off\nnetwork')) +
    theme_void() + theme(legend.position = 'none') )

ggsave(plot = vizdual, filename = paste0(wd_output,'/complexity-graph-progression.pdf'),
       width = 15, height = 10)
