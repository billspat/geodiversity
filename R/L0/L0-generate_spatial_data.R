
# Load packages
library(tidyverse)
library(sf)

# Load in data_dir location
source("./R/config.R")

# Import NEON data 
dom <- st_read(paste0(data_dir, "NEON_spatial/NEON_domains/NEON_Domains.shp"), quiet=T)
site <- st_read(paste0(data_dir, "NEON_spatial/NEON_sites/terrestrialSamplingBoundaries.shp"), quiet=T)
plt <- st_read(paste0(data_dir, "NEON_spatial/NEON_TOS_Plot_Points/NEON_TOS_Plot_Points.shp"), quiet=T) # %>% 
# filter(subtype == "mammalGrid") %>% 
# st_write(paste0(data_dir, "NEON_spatial/NEON_small_mammal_plots/NEON_small_mammal_plots.shp"))

# Define functions 
id_circle_center <- function(d){
  du <- st_union(d)
  b <- lwgeom::st_minimum_bounding_circle(du)
  c <- st_centroid(b)
  return(c)
}

dist_to_circle_center <- function(d, dist_metric = "maximum"){
  du <- st_union(d)
  b <- lwgeom::st_minimum_bounding_circle(du)
  c <- st_centroid(b)
  dp <- d %>% st_cast("POINT")
  dist <- st_distance(dp, c)
  if(dist_metric == "maximum"){
    return(as.numeric(max(dist)))
  }
  if(dist_metric == "median"){
    return(as.numeric(median(dist)))
  }
}

################################################################################
##### SITE SCALE ##### 

# start <- proc.time()
test <- plt %>% 
  st_make_valid() %>% 
  nest(.by = siteID) %>% 
  mutate(centroid = lapply(data, function(x) st_centroid(st_union(x)) %>% st_set_crs(4326)), 
         circle_center = lapply(data, function(x) id_circle_center(x)), 
         max_dist_circ = lapply(data, function(x) dist_to_circle_center(x)), 
         med_dist_circ = lapply(data, function(x) dist_to_circle_center(x, dist_metric = "median"))) %>% 
  unnest(cols = c(centroid, max_dist_circ, med_dist_circ, circle_center)) %>% 
  mutate(buff_dist = max(max_dist_circ)) %>% 
  mutate(site_poly = st_buffer(circle_center, dist = buff_dist))
# proc.time()-start

site_circle_center <- test %>% 
  dplyr::select(siteID, circle_center) %>% 
  rename(geometry = circle_center) %>% 
  st_as_sf()

site_radii <- test %>% 
  dplyr::select(siteID, site_poly) %>% 
  rename(geometry = site_poly) %>% 
  st_as_sf()

dom_radii <- test %>% 
  dplyr::select(siteID, circle_center) %>% 
  rename(geometry = circle_center) %>% # 100 km centroid around 
  st_as_sf() %>% 
  st_buffer(100000)


################################################################################
##### DOMAIN SCALE ##### 
# This code generates a single domain-scale polygon for each domain. In MSU MSB 
# meeting on 9/5/24 we decided to instead take a "nesting dolls" approach where
# each site has its own domain-scale polygon. 

# Domain Level 
test2 <- site %>% 
  st_make_valid() %>% 
  nest(.by = domainName) %>% 
  mutate(centroid = lapply(data, function(x) st_centroid(st_union(x)) %>% st_set_crs(4326)), 
         max_dist_cent = lapply(data, function(x) max_dist_to_centroid(x)), 
         circle_center = lapply(data, function(x) id_circle_center(x)), 
         max_dist_circ = lapply(data, function(x) max_dist_to_circle_center(x))) %>% 
  unnest(cols = c(max_dist_cent, centroid, max_dist_circ, circle_center)) %>% 
  mutate(buff_dist = max(max_dist_circ)) %>% 
  mutate(dom_poly = st_buffer(circle_center, dist = buff_dist))

dom_circle_center <- test2 %>% 
  dplyr::select(domainName, circle_center) %>% 
  rename(geometry = circle_center) %>% 
  st_as_sf()

dom_radii <- test2 %>% 
  dplyr::select(domainName, dom_poly) %>% 
  rename(geometry = dom_poly) %>% 
  st_as_sf()

