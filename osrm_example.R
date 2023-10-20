## ************************************************************************* ##
##                                                                           ##
## Script name: osrm_example.R                                               ##
##                                                                           ##
## Purpose of script: Provide examples of usage of osrm package to           ##
##                    calculate travel times, routes and isochrones          ##
##                                                                           ##
## Author: Richard Blackwell                                                 ##
##                                                                           ##
## Date Created: 2023-10-19                                                  ##
##                                                                           ##
## Email: richard.blackwell@healthinnovationsouthwest.com                    ##
##                                                                           ##
## ************************************************************************* ##

# 0. Load Libraries and Define Functions ----
# *******************************************

library(tidyverse)  # for general data processing
library(osrm)       # for the travel time analysis
library(parallel)   # for the parallel processing
library(leaflet)    # for interactive mapping of the travel time analysis
library(sf)         # for shapefile manipulation
library(htmlwidgets) # for saving the interactive maps

# * 0.1. Set OSRM options ----
# ````````````````````````````
options(osrm.server = 'http://router.project-osrm.org/') 
options(osrm.profile = 'car') 

# * 0.2. Define functions ----
# ````````````````````````````
# Create the route function using osrm
fnCreateRoute <- function(x){
  osrmRoute(src = unname(as.numeric(x[c('src_lng', 'src_lat')])),
            dst = unname(as.numeric(x[c('dst_lng', 'dst_lat')])))
}

# 1. Load the source and destination data ----
# ********************************************

# We will use the local acute hospitals in Devon...
df_dst <- read.csv('destinations.csv')

# ...and the population weighted centroids for the output areas in Devon
df_src <- read.csv('OA21_PWC.csv')

# Load the output area to ICB lookup
df_lu <- read.csv('OA21_LSOA21_MSOA21_ICB22_LAD22_EW_LU.csv') %>%
  filter(ICB22CDH == 'QJK')

# Filter the sources to just Devon output areas
df_src <- df_src %>% semi_join(df_lu, by = c('OA21CD' = 'oa21cd'))

# Convert the dataframe into sf with CRS 27700 (Easting/Northing) and 
# then transform in CRS 4326 (Longitude/Latitude) and save the coordinates 
# as lng/lat and drop the geometry
df_src <- df_src %>% 
  st_as_sf(coords = c('x','y'), dim = 'XY', crs = 27700) %>% 
  st_transform(crs = 4326) %>%
  mutate(lng = unname(st_coordinates(.)[,1]), lat = unname(st_coordinates(.)[,2])) %>%
  st_drop_geometry()

# 2. Calculate the car travel routes from source to destination ----
# ******************************************************************

# Create a data frame of each source to destination pairing using expand_grid
df_journeys <- expand_grid(
  df_src %>% transmute(oa21cd = OA21CD, lat, lng), 
  df_dst %>% select(orgcd, lat, lng), 
  .name_repair = 'universal') %>%
  rename_with(.fn = ~c('oa21cd', 'src_lat', 'src_lng', 'orgcd', 'dst_lat', 'dst_lng'))

# * 2.1. Create the parallel processing routine for the travel time calculation ----
# ``````````````````````````````````````````````````````````````````````````````````

# Create the clusters for parallelisation
n_cores <- detectCores()
# Leave one cluster free
clust <- makeCluster(n_cores - 1)
# Export the route creation function
clusterExport(clust, c('fnCreateRoute','osrmRoute'))
# Get the routes
res <- parApply(clust, X = df_journeys, MARGIN = 1, FUN = fnCreateRoute)
# Stop the clusters
stopCluster(clust)

# Transform the result into a sf object for mapping purposes
sf_journeys <- df_journeys %>% 
  bind_cols(bind_rows(res)) %>%
  st_as_sf() %>%
  select(-c('src','dst'))

# 3. Create the combined isochrones ----
# **************************************

# Choose a maximum travel time for the final band 
max_duration <- 240

# Add the 10 min travel time band to sf_journeys and convert back into a dataframe
df_journeys <- sf_journeys %>% 
  st_drop_geometry() %>%
  mutate(band = cut(duration, breaks = c(seq(0,60,10), max_duration)))

# Load the 2021 Output Areas shapefile and transform to CRS 4326
sf_oa21 <- st_read(dsn = 'OA21', layer= 'OA21') %>%
  st_transform(crs = 4326) %>%
  semi_join(df_lu, by = c('OA21CD' = 'oa21cd')) %>%
  st_make_valid()

# Select the closest site by travel time (duration) for all sites
sf_all <- sf_oa21 %>% 
  inner_join(df_journeys, by = c('OA21CD' = 'oa21cd')) %>%
  group_by(OA21CD) %>% 
  slice_min(n = 1, order_by = duration, with_ties = FALSE) %>%
  ungroup() %>% 
  # Group all the same travel bands and summarise into polygons
  group_by(band) %>%
  summarise() %>%
  ungroup()

# Select the closest site by travel time (duration) for all sites excluding Torbay
sf_exc_tor <- sf_oa21 %>% 
  inner_join(df_journeys %>% filter(orgcd != 'RA901'), by = c('OA21CD' = 'oa21cd')) %>%
  group_by(OA21CD) %>% 
  slice_min(n = 1, order_by = duration, with_ties = FALSE) %>%
  ungroup() %>% 
  # Group all the same travel bands and summarise into polygons
  group_by(band) %>%
  summarise() %>%
  ungroup()

# Select the closest site by travel time (duration) for all sites excluding NDDH
sf_exc_nddh <- sf_oa21 %>% 
  inner_join(df_journeys %>% filter(orgcd != 'RBZ12'), by = c('OA21CD' = 'oa21cd')) %>%
  group_by(OA21CD) %>% 
  slice_min(n = 1, order_by = duration, with_ties = FALSE) %>%
  ungroup() %>% 
  # Group all the same travel bands and summarise into polygons
  group_by(band) %>%
  summarise() %>%
  ungroup()

# Select the closest site by travel time (duration) for all sites excluding RDE
sf_exc_rde <- sf_oa21 %>% 
  inner_join(df_journeys %>% filter(orgcd != 'RH801'), by = c('OA21CD' = 'oa21cd')) %>%
  group_by(OA21CD) %>% 
  slice_min(n = 1, order_by = duration, with_ties = FALSE) %>%
  ungroup() %>% 
  # Group all the same travel bands and summarise into polygons
  group_by(band) %>%
  summarise() %>%
  ungroup()

# Select the closest site by travel time (duration) for all sites excluding Derriford
sf_exc_ply <- sf_oa21 %>% 
  inner_join(df_journeys %>% filter(orgcd != 'RK950'), by = c('OA21CD' = 'oa21cd')) %>%
  group_by(OA21CD) %>% 
  slice_min(n = 1, order_by = duration, with_ties = FALSE) %>%
  ungroup() %>% 
  # Group all the same travel bands and summarise into polygons
  group_by(band) %>%
  summarise() %>%
  ungroup()

# 4. Display the combined isochrones ----
# ***************************************

palBand <- colorFactor(palette = c('(0,10]' = '#1a9850', '(10,20]' = '#91cf60',
                                   '(20,30]' = '#d9ef8b','(30,40]' = '#fee08b',
                                   '(40,50]' = '#fc8d59','(50,60]' = '#d73027',
                                   '(60,240]' = '#a50026'), 
                       domain = c('(0,10]', '(10,20]', '(20,30]', '(30,40]', '(40,50]', '(50,60]', '(60,240]'),
                       na.color = '#cecece')

# Create the map
map <- leaflet() %>%
  addTiles() %>%
  # Add the overall layer - polygons and markers
  addPolygons(data = sf_all,
              stroke = FALSE,
              fillColor = ~palBand(band),
              fillOpacity = 0.7,
              popup = ~band,
              group = 'Overall') %>%
  addCircleMarkers(data = df_dst,
                   radius = 5,
                   weight = 3,
                   fillColor = 'white',
                   fillOpacity = 1,
                   popup = ~orgnm,
                   group = 'Overall') %>%
  # Add the layer excluding Torbay - polygons and markers
  addPolygons(data = sf_exc_tor,
              stroke = FALSE,
              fillColor = ~palBand(band),
              fillOpacity = 0.7,
              popup = ~band,
              group = 'Exc. Torbay') %>%
  addCircleMarkers(data = df_dst %>% filter(orgcd != 'RA901'),
                   radius = 5,
                   weight = 3,
                   fillColor = 'white',
                   fillOpacity = 1,
                   popup = ~orgnm,
                   group = 'Exc. Torbay') %>%
  # Add the layer excluding NDDH - polygons and markers
  addPolygons(data = sf_exc_nddh,
              stroke = FALSE,
              fillColor = ~palBand(band),
              fillOpacity = 0.7,
              popup = ~band,
              group = 'Exc. NDDH') %>%
  addCircleMarkers(data = df_dst %>% filter(orgcd != 'RBZ12'),
                   radius = 5,
                   weight = 3,
                   fillColor = 'white',
                   fillOpacity = 1,
                   popup = ~orgnm,
                   group = 'Exc. NDDH') %>%
  # Add the layer excluding RD&E - polygons and markers
  addPolygons(data = sf_exc_rde,
              stroke = FALSE,
              fillColor = ~palBand(band),
              fillOpacity = 0.7,
              popup = ~band,
              group = 'Exc. RD&E') %>%
  addCircleMarkers(data = df_dst %>% filter(orgcd != 'RH801'),
                   radius = 5,
                   weight = 3,
                   fillColor = 'white',
                   fillOpacity = 1,
                   popup = ~orgnm,
                   group = 'Exc. RD&E') %>%
  # Add the layer excluding Derriford - polygons and markers
  addPolygons(data = sf_exc_ply,
              stroke = FALSE,
              fillColor = ~palBand(band),
              fillOpacity = 0.7,
              popup = ~band,
              group = 'Exc. Derriford') %>%
  addCircleMarkers(data = df_dst %>% filter(orgcd != 'RK950'),
                   radius = 5,
                   weight = 3,
                   fillColor = 'white',
                   fillOpacity = 1,
                   popup = ~orgnm,
                   group = 'Exc. Derriford') %>%
  # Add the travel time band legend
  addLegend(position = 'bottomright', 
            title = 'Travel Time Band\n(minutes)',
            colors = c('(0,10]' = '#1a9850', '(10,20]' = '#91cf60',
                       '(20,30]' = '#d9ef8b','(30,40]' = '#fee08b',
                       '(40,50]' = '#fc8d59','(50,60]' = '#d73027',
                       '(60,240]' = '#a50026'),
            labels = c('0-10 mins', '10-20 mins', '20-30 mins', 
                       '30-40 mins', '40-50 mins', '50-60 mins', 
                       'Over 60 mins'),
            opacity = 0.7) %>%
  # Add the layers control and hide all bar the overall layer
  addLayersControl(baseGroups = c('Overall', 'Exc. Torbay', 'Exc. NDDH', 'Exc. RD&E', 'Exc. Derriford')) %>%
  hideGroup(c('Exc. Torbay', 'Exc. NDDH', 'Exc. RD&E', 'Exc. Derriford'))
map

