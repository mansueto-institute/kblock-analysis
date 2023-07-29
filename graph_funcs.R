
library(tidyverse)
library(sf)
library(units)
options(scipen = 999)
options(lifecycle_verbosity = "warning")

# Functions ---------------------------------------------------------------

st_rook = function(a, b = a) st_relate(a, b, pattern = "F***1****")

st_queen <- function(a, b = a) st_relate(a, b, pattern = "F***T****")

convert_to_poly <- function(lines) {
  
  polys <- lines %>%
    dplyr::summarize(geometry = st_union(geometry)) %>%
    st_line_merge() %>%
    st_polygonize(.) %>%
    st_collection_extract(., type = "POLYGON") 
  
  return(polys)
}

generate_graph <- function(graph_polys, graph_exclude) {
  
  graph_polys <- graph_polys %>% mutate(parcel_id = row_number())
  
  a <- graph_polys %>%
    st_point_on_surface() 
  
  b <- graph_polys %>%
    st_join(x = ., y = graph_polys %>%
              select(parcel_id) %>% rename(parcel_id_touch = parcel_id), 
            join = st_touches) %>%
    st_point_on_surface() %>%
    select(parcel_id, parcel_id_touch)
  
  c <- a %>%
    select(parcel_id) %>%
    right_join(., b %>% st_drop_geometry(),
               by=c('parcel_id'='parcel_id_touch'), multiple = "all") %>%
    rename(parcel_id_touch = parcel_id,
           parcel_id = parcel_id.y)
  
  d <- rbind(b, c) %>%
    group_by(parcel_id, parcel_id_touch) %>%
    dplyr::summarize(geometry = st_union(geometry)) %>%
    ungroup() 
  
  e <- d %>% sf::st_cast(., "LINESTRING") %>%
    st_join(x = ., y = graph_exclude %>% select(geometry) %>%
              mutate(exclude_features = TRUE),
            join = st_intersects) %>%
    filter(is.na(exclude_features)) %>%
    select(geometry)
  
  return(e)
  
}


generate_dual <- function(graph_sf) { #, exclude_border = FALSE
  
  graph_sf <- graph_sf %>%
    mutate(graph_id = row_number())
  
  graph_1 <- st_join(x = graph_sf, y = graph_sf, join = st_rook) %>%
    rename(graph_id = graph_id.x,
           graph_id_touch = graph_id.y) %>%
    st_centroid() 
  
  graph_2 <- graph_sf %>% st_centroid() %>% rename(graph_id_touch = graph_id) %>%
    right_join(., graph_1 %>%
                 st_drop_geometry(),
               by = c('graph_id_touch'='graph_id_touch'), multiple = 'all')
  
  dual <- rbind(graph_1, graph_2) %>%
    group_by(graph_id, graph_id_touch)%>%
    dplyr::summarize(geometry = st_union(geometry)) %>%
    ungroup() 
  
  dual <- dual %>%
    mutate(gtype = st_geometry_type(.)) %>%
    filter(gtype == 'MULTIPOINT') %>% 
    sf::st_cast(., "LINESTRING")  %>%
    select(geometry)
  
  dual <- convert_to_poly(dual)
  
  return(dual)
}

generate_voronoi <- function(triangulated_sf) {
  
  triangulated_sf <- triangulated_sf %>%
    st_cast(., "MULTIPOINT") %>%
    st_cast(., "POINT") %>%
    dplyr::summarize(geometry = st_union(geometry)) %>%
    st_voronoi(.) %>%
    st_collection_extract(., type = "POLYGON")  %>%
    st_intersection(., triangulated_sf %>% st_union()) %>%
    st_join(x = ., y = triangulated_sf %>% 
              st_union() %>% 
              st_boundary() %>% st_as_sf() %>%
              mutate(exclude_features = TRUE),
            join = st_intersects) %>%
    filter(is.na(exclude_features)) %>%
    select(geometry)
  
  return(triangulated_sf)
}

extract_duals <- function(graph_sf) {
  
  l <- nrow(graph_sf)
  primal_df <- graph_sf
  iter <- 0
  
  output_df <- graph_sf %>% 
    mutate(part = 'primal', iteration = iter) %>%
    select(part, iteration, geometry) 
  
  while (l > 1) {
    
    print(iter)
    iter = iter + 1
    dual_df <- generate_dual(graph_sf = primal_df %>% select(geometry))
    primal_df <- generate_voronoi(triangulated_sf = dual_df %>% select(geometry))
    dual_df <- dual_df %>%
      mutate(part = 'dual', iteration = iter) %>%
      select(part, iteration, geometry)
    primal_df <- primal_df %>% 
      mutate(part = 'primal', iteration = iter) %>%
      select(part, iteration, geometry)
    
    output_df <- rbind(output_df, dual_df, primal_df)
    
    l <- nrow(primal_df)
  }
  return(output_df)
}


# f <- e %>%
#   dplyr::summarize(geometry = st_union(geometry)) %>%
#   st_line_merge() 
# 
# g <- f %>%
#   #sf::st_cast(., "MULTILINESTRING") %>%
#   st_polygonize(.) %>%
#   st_collection_extract(., type = "POLYGON")
