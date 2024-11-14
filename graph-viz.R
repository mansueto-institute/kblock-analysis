

library(tidyverse)
library(sf)
library(viridis)
library(patchwork)
library(scales)
library(units)
options(scipen = 999)
options(lifecycle_verbosity = "warning")

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

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

(vizgraph_e <- vizgraph &
    plot_annotation(tag_levels = list(c("E"))) & 
    theme(plot.tag = element_text(size = 13)))

ggsave(plot = vizgraph_e, filename = paste0(wd_output,'/complexity-graph_parte.pdf'),
       width = 6.15, height = 5)


# -------------------------------------------------------------------------


voronoi_initial <- graph_points %>% select(geometry) %>%
  st_cast(., "MULTIPOINT") %>%
  st_cast(., "POINT") %>%
  dplyr::summarize(geometry = st_union(geometry)) %>%
  st_voronoi(.) %>%
  st_collection_extract(., type = "POLYGON") %>%
  st_intersection(., poly_in %>% st_union())

(vizprep <- (ggplot() +
              #geom_sf(data = poly_in, color = 'black', alpha = 0, size = .3, linewidth = .06) +
              geom_sf(data = graph_data %>% filter(block_property == 'building-polygons', block_id %in% c("ZMB.5.6_2_9371")) ) +
              geom_sf(data = line_in, fill = '#2C73D2', color = '#2C73D2', alpha = 1, linewidth = .5, lineend = 'round') +
              labs(subtitle = 'A', caption= 'Buildings as polygons in parcels') + theme_void() + theme(legend.position = 'none', plot.caption = element_text(hjust = .3, vjust = 5))) +
  (ggplot() +  
     #geom_sf(data = poly_in, color = 'black', alpha = 0, size = .3, linewidth = .06) +
     geom_sf(data = graph_points, color = '#333333', alpha = .8, size = .2, linewidth = .01)  +
     geom_sf(data = line_in, fill = '#2C73D2', color = '#2C73D2', alpha = 1, linewidth = .5, lineend = 'round') +
     labs(subtitle = 'B', caption= 'Buildings as points in parcels')  + theme_void() + theme(legend.position = 'none', plot.caption = element_text(hjust = .3, vjust = 5))) +
  (ggplot() +  
     geom_sf(data = voronoi_initial, color = 'black', alpha = 0, size = .3, linewidth = .06) +
     geom_sf(data = graph_points, color = '#333333', alpha = .8, size = .2, linewidth = .01)  +
     geom_sf(data = line_in, fill = '#2C73D2', color = '#2C73D2', alpha = 1, linewidth = .5, lineend = 'round') +
     labs(subtitle = 'C', caption= 'Buildings as points and parcels as Voronoi cells')  + theme_void() + theme(legend.position = 'none', plot.caption = element_text(hjust = .3, vjust = 5))) +
  (ggplot() +  
     geom_sf(data = graph_lines, color = '#333333', alpha = .5, linewidth = .2) +
     geom_sf(data = graph_points, color = '#333333', alpha = .8, size = .3, linewidth = .01) +
     geom_sf(data = line_in, fill = '#2C73D2', color = '#2C73D2', alpha = 1, linewidth = .5, lineend = 'round') +
     labs(subtitle = 'D', caption= 'Intial connected graph')  + theme_void() + theme(legend.position = 'none', plot.caption = element_text(hjust = .3, vjust = 5))) ) # Delaunay Trianguation
  

(vizdual <- (ggplot() +
    geom_sf(data = line_in, fill = '#2C73D2', color = '#2C73D2', alpha = 1, linewidth = .5, lineend = 'round') +
    #geom_sf(data = graph_points, color = '#333333', alpha = .8, size = .3, linewidth = .01) +
    geom_sf(data = graph_duals %>% filter(part == 'dual') %>% filter(iteration %in% c(1)), aes(fill = as.character(iteration)), alpha = 1, color = 'black') +
    geom_sf(data = graph_duals %>% filter(part == 'primal') %>% filter(iteration %in% c(1)), aes(fill = as.character(iteration)), alpha = 0, color = 'white', linewidth = .3) +
    scale_fill_manual(values = c(grey2, colorhexes), breaks = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10+', 'Off\nnetwork')) +
    labs(subtitle = 'A', caption= 'Iteration 1, remove cells with direct street access')  + theme_void() + theme(legend.position = 'none', plot.caption = element_text(hjust = .3, vjust = 5))) +
(ggplot() +
   geom_sf(data = line_in, fill = '#2C73D2', color = '#2C73D2', alpha = 1, linewidth = .5, lineend = 'round') +
   #geom_sf(data = graph_points, color = '#333333', alpha = .8, size = .3, linewidth = .01) +
   geom_sf(data = graph_duals %>% filter(part == 'dual') %>% filter(iteration %in% c(2)), aes(fill = as.character(iteration)), alpha = 1, color = 'black') +
   geom_sf(data = graph_duals %>% filter(part == 'primal') %>% filter(iteration %in% c(2)), aes(fill = as.character(iteration)), alpha = 0, color = 'white', linewidth = .3) +
   scale_fill_manual(values = c(grey2, colorhexes), breaks = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10+', 'Off\nnetwork')) +
   labs(subtitle = 'B', caption= 'Iteration 2 of dual graph')  + theme_void() + theme(legend.position = 'none', plot.caption = element_text(hjust = .3, vjust = 5))) +
(ggplot() +
   geom_sf(data = line_in, fill = '#2C73D2', color = '#2C73D2', alpha = 1, linewidth = .5, lineend = 'round') +
   #geom_sf(data = graph_points, color = '#333333', alpha = .8, size = .3, linewidth = .01) +
   geom_sf(data = graph_duals %>% filter(part == 'dual') %>% filter(iteration %in% c(3)), aes(fill = as.character(iteration)), alpha = 1, color = 'black') +
   geom_sf(data = graph_duals %>% filter(part == 'primal') %>% filter(iteration %in% c(3)), aes(fill = as.character(iteration)), alpha = 0, color = 'white', linewidth = .3) +
   scale_fill_manual(values = c(grey2, colorhexes), breaks = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10+', 'Off\nnetwork')) +
   labs(subtitle = 'C', caption= 'Iteration 3 of dual graph')  + theme_void() + theme(legend.position = 'none', plot.caption = element_text(hjust = .3, vjust = 5))) +
(ggplot() +
   geom_sf(data = line_in, fill = '#2C73D2', color = '#2C73D2', alpha = 1, linewidth = .5, lineend = 'round') +
   #geom_sf(data = graph_points, color = '#333333', alpha = .8, size = .3, linewidth = .01) +
   geom_sf(data = graph_duals %>% filter(part == 'dual') %>% filter(iteration %in% c(4)), aes(fill = as.character(iteration)), alpha = 1, color = 'black') +
   geom_sf(data = graph_duals %>% filter(part == 'primal') %>% filter(iteration %in% c(4)), aes(fill = as.character(iteration)), alpha = 0, color = 'white', linewidth = .3) +
   scale_fill_manual(values = c(grey2, colorhexes), breaks = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10+', 'Off\nnetwork')) +
   labs(subtitle = 'D', caption= 'Iteration 4 of dual graph')  + theme_void() + theme(legend.position = 'none', plot.caption = element_text(hjust = .3, vjust = 5))) +
(ggplot() +
   geom_sf(data = line_in, fill = '#2C73D2', color = '#2C73D2', alpha = 1, linewidth = .5, lineend = 'round') +
   #geom_sf(data = graph_points, color = '#333333', alpha = .8, size = .3, linewidth = .01) +
   geom_sf(data = graph_duals %>% filter(part == 'dual') %>% filter(iteration %in% c(5)), aes(fill = as.character(iteration)), alpha = 1, color = 'black') +
   geom_sf(data = graph_duals %>% filter(part == 'primal') %>% filter(iteration %in% c(5)), aes(fill = as.character(iteration)), alpha = 0, color = 'white', linewidth = .3) +
   scale_fill_manual(values = c(grey2, colorhexes), breaks = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10+', 'Off\nnetwork')) +
   labs(subtitle = 'E', caption= 'Iteration 5 of dual graph')  + theme_void() + theme(legend.position = 'none', plot.caption = element_text(hjust = .3, vjust = 5))) +
(ggplot() +
   geom_sf(data = line_in, fill = '#2C73D2', color = '#2C73D2', alpha = 1, linewidth = .5, lineend = 'round') +
   #geom_sf(data = graph_points, color = '#333333', alpha = .8, size = .3, linewidth = .01) +
   geom_sf(data = graph_duals %>% filter(part == 'dual') %>% filter(iteration %in% c(6)), aes(fill = as.character(iteration)), alpha = 1, color = 'black') +
   geom_sf(data = graph_duals %>% filter(part == 'primal') %>% filter(iteration %in% c(6)), aes(fill = as.character(iteration)), alpha = 0, color = 'white', linewidth = .3) +
   scale_fill_manual(values = c(grey2, colorhexes), breaks = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10+', 'Off\nnetwork')) +
   labs(subtitle = 'F', caption= 'Iteration 6 of dual graph')  + theme_void() + theme(legend.position = 'none', plot.caption = element_text(hjust = .3, vjust = 5))) +
(ggplot() +
   geom_sf(data = line_in, fill = '#2C73D2', color = '#2C73D2', alpha = 1, linewidth = .5, lineend = 'round') +
   #geom_sf(data = graph_points, color = '#333333', alpha = .8, size = .3, linewidth = .01) +
   geom_sf(data = graph_duals %>% filter(part == 'dual') %>% filter(iteration %in% c(7)), aes(fill = as.character(iteration)), alpha = 1, color = 'black') +
   geom_sf(data = graph_duals %>% filter(part == 'primal') %>% filter(iteration %in% c(7)), aes(fill = as.character(iteration)), alpha = 0, color = 'white', linewidth = .3) +
   scale_fill_manual(values = c(grey2, colorhexes), breaks = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10+', 'Off\nnetwork')) +
   labs(subtitle = 'G', caption= 'Iteration 7 of dual graph')  + theme_void() + theme(legend.position = 'none', plot.caption = element_text(hjust = .3, vjust = 5))) +
(ggplot() +
   geom_sf(data = line_in, fill = '#2C73D2', color = '#2C73D2', alpha = 1, linewidth = .5, lineend = 'round') +
   #geom_sf(data = graph_points, color = '#333333', alpha = .8, size = .3, linewidth = .01) +
   geom_sf(data = graph_duals %>% filter(part == 'dual') %>% filter(iteration %in% c(8)), aes(fill = as.character(iteration)), alpha = 1, color = 'black') +
   geom_sf(data = graph_duals %>% filter(part == 'primal') %>% filter(iteration %in% c(8)), aes(fill = as.character(iteration)), alpha = 0, color = 'white', linewidth = .3) +
   scale_fill_manual(values = c(grey2, colorhexes), breaks = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10+', 'Off\nnetwork')) +
   labs(subtitle = 'H', caption= 'Iteration 8 with final dual graph, thus Block Complexity = 8')  + theme_void() + theme(legend.position = 'none', plot.caption = element_text(hjust = .3, vjust = 5))) 
)



ggsave(plot = vizprep + plot_layout(nrow = 2), filename = paste0(wd_output,'/complexity-graph-prep.pdf'),
       width = 8, height = 6)



ggsave(plot = vizdual + plot_layout(nrow = 2), filename = paste0(wd_output,'/complexity-graph-progression.pdf'),
       width = 15, height = 6)





