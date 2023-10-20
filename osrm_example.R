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
df_lu <- read.csv('OA21_ICB23_LU.csv') %>%
  filter(ICB23CDH == 'QJK')

# Filter the sources to just Devon output areas
df_src <- df_src %>% semi_join(df_lu, by = 'OA21CD')

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
  semi_join(df_lu, by = 'OA21CD') %>%
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
  hideGroup(c('Exc. Torbay', 'Exc. NDDH', 'Exc. RD&E', 'Exc. Derriford')) %>%
  # Add acknowledgements
  addControl(
    html = paste0('Source: Office for National Statistics licensed under the Open Government Licence v.3.0<br>',
                  'Contains OS data &copy Crown copyright and database right [2023]<br>',
                  'Data: <a href="http://www.openstreetmap.org/copyright">&copy OpenStreetMap contributors, ODbL 1.0</a><br>',
                  'Routing: <a href="http://project-osrm.org/">OSRM</a>'),
    position = "bottomleft", layerId = NULL, className = "info legend"
  )
map

# Save the interactive map as a single file html
saveWidget(map, 'combined_isochrones.html')

# 5. Use osrm::osrmIsochrone to create the single point isochrones for each site ----
# ***********************************************************************************

# Create the breaks as 10 minute bands up to 1 hour journey time
bands <- seq(0, 60, 10)

# Set the low and high resolution values
lo_res <- 30
hi_res <- 120

sf_iso_tor_lo <- osrm::osrmIsochrone(loc = c(df_dst$lng[df_dst$orgcd=='RA901'],
                                          df_dst$lat[df_dst$orgcd=='RA901']),
                                     breaks = bands,
                                     res = lo_res)
sf_iso_tor_hi <- osrm::osrmIsochrone(loc = c(df_dst$lng[df_dst$orgcd=='RA901'],
                                          df_dst$lat[df_dst$orgcd=='RA901']),
                                     breaks = bands,
                                     res = hi_res)
sf_iso_tor_vhi <- osrm::osrmIsochrone(loc = c(df_dst$lng[df_dst$orgcd=='RA901'],
                                             df_dst$lat[df_dst$orgcd=='RA901']),
                                     breaks = bands,
                                     res = 240)

palBand <- colorFactor(palette = 'RdYlGn', domain = c(1:6), na.color = '#cecece', reverse =  TRUE)

map <- leaflet() %>%
  addTiles() %>%
  # Add the low resolution layer
  addPolygons(data = sf_iso_tor_lo,
              stroke = FALSE,
              fillColor = ~palBand(id),
              fillOpacity = 0.7,
              popup = ~paste0(isomin, '-', isomax, ' mins'),
              group = 'Low Res') %>%
  # Add the high resolution layer
  addPolygons(data = sf_iso_tor_hi,
              stroke = FALSE,
              fillColor = ~palBand(id),
              fillOpacity = 0.7,
              popup = ~paste0(isomin, '-', isomax, ' mins'),
              group = 'High Res') %>%
  # Add the very high resolution layer
  addPolygons(data = sf_iso_tor_vhi,
              stroke = FALSE,
              fillColor = ~palBand(id),
              fillOpacity = 0.7,
              popup = ~paste0(isomin, '-', isomax, ' mins'),
              group = 'Very High Res') %>%
  # Add the location marker
  addCircleMarkers(data = df_dst %>% filter(orgcd=='RA901'),
                   radius = 5,
                   weight = 3,
                   fillColor = 'white',
                   fillOpacity = 1,
                   popup = ~orgnm,
                   group = 'Location') %>%
  # Add the travel time band legend
  addLegend(position = 'bottomright', 
            title = 'Travel Time Band\n(minutes)',
            colors = c('#1a9850','#91cf60','#d9ef8b','#fee08b','#fc8d59','#d73027'),
            labels = c('0-10 mins', '10-20 mins', '20-30 mins', 
                       '30-40 mins', '40-50 mins', '50-60 mins'),
            opacity = 0.7) %>%
  # Add the layers control and hide all bar the overall layer
  addLayersControl(baseGroups = c('Low Res', 'High Res', 'Very High Res'), overlayGroups = 'Location') %>%
  hideGroup(c('High Res')) %>%
  # Add acknowledgements
  addControl(
    html = paste0('Source: Office for National Statistics licensed under the Open Government Licence v.3.0<br>',
                  'Contains OS data &copy Crown copyright and database right [2023]<br>',
                  'Data: <a href="http://www.openstreetmap.org/copyright">&copy OpenStreetMap contributors, ODbL 1.0</a><br>',
                  'Routing: <a href="http://project-osrm.org/">OSRM</a>'),
    position = "bottomleft", layerId = NULL, className = "info legend"
  )
map

# Save the interactive map as a single file html
saveWidget(map, 'single_isochrone.html')

# 6. An example of route mapping ----
# ***********************************

map <- leaflet() %>%
  addTiles() %>%
  # Add the source marker
  addCircleMarkers(data = df_src %>% filter(OA21CD=='E00102794'),
                   radius = 5,
                   weight = 3,
                   fillColor = 'white',
                   fillOpacity = 1,
                   popup = ~OA21CD,
                   group = 'Location') %>%
  # Add the destination marker
  addCircleMarkers(data = df_dst %>% filter(orgcd=='RA901'),
                   radius = 5,
                   weight = 3,
                   fillColor = 'white',
                   fillOpacity = 1,
                   popup = ~orgnm,
                   group = 'Location') %>%
  # Add the route
  addPolylines(data = sf_journeys %>% filter(oa21cd=='E00102794' & orgcd=='RA901'),
               weight = 3,
               popup = ~paste0('Duration: ', round(duration, 1), ' mins<br>Distance: ', round(distance, 1), ' km'),
               group = 'Route') %>%
  # Add acknowledgements
  addControl(
    html = paste0('Source: Office for National Statistics licensed under the Open Government Licence v.3.0<br>',
                  'Contains OS data &copy Crown copyright and database right [2023]<br>',
                  'Data: <a href="http://www.openstreetmap.org/copyright">&copy OpenStreetMap contributors, ODbL 1.0</a><br>',
                  'Routing: <a href="http://project-osrm.org/">OSRM</a>'),
    position = "bottomleft", layerId = NULL, className = "info legend"
  )
map

# Save the interactive map as a single file html
saveWidget(map, 'route.html')