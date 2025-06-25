source("Rscripts/00-preamble.R")

# the aim of this script is to claculate the geographic distance of a species
# native range centroid from the geographic center of europe.

# load data
dr_non_native <- read_csv("Data/dr_non_native.csv")
relatedness <- read_csv("Data/relatedness.csv")

# calculate Europe centroid -----------------------------------------------

# calculate mean centroid of Europe with wcvp data
# get Europe's botanical country abbreviations
europe <- get_wgsrpd3_codes("Europe")
tdwg <- st_read("bot_countries/wgsrpd-master/level3/level3.shp")
europe_shape <- tdwg %>% 
  filter(LEVEL3_COD %in% europe) %>% 
  st_geometry() %>% 
  st_transform(8857) %>%
  st_make_valid()

# have a look
plot(europe_shape)

# merge all European geometries into one
europe_union <- st_union(europe_shape)

# calculate the centroid of the merged Europe geometry
europe_projected_center <- st_centroid(europe_union)

# extract the centroid's coordinates (longitude, latitude)
centroid_coords_center <- st_coordinates(europe_centroid_center)

# print the results
mean_centroid_europe_center <- data.frame(
  mean_longitude = centroid_coords_center[1],
  mean_latitude = centroid_coords_center[2]
)

print(mean_centroid_europe_center)

# create a vector with the rounded coordinates of the mean centroid of Europe
center_of_europe <- c(longitude = 28.21111, latitude = 59.55993)   



# species centroids --------------------------------------------------------

# initialize a empty dataframe for the results
mean_centroids <- data.frame(
  species = character(), 
  mean_longitude = numeric(), 
  mean_latitude = numeric(), 
  distance_to_europe_center = numeric(),
  stringsAsFactors = FALSE
)


#plant <- dr_non_native[310,1]$taxon_name
# loop for every non-native plant in Europe
for (plant in unique(dr_non_native$taxon_name)) {
  # Extract distribution data
  distribution_data <- wcvp_distribution(plant)
  
  # Transformation into a projected CRS (e.g. EPSG:3857)
  distribution_data <- st_transform(distribution_data, crs = 3857)
  
  # Calculate the centroids of the distribution areas
  country_centroids <- st_centroid(distribution_data)
  
  # Transformation back to WGS 84 (EPSG:4326) for longitude and latitude
  country_centroids <- st_transform(country_centroids, crs = 4326)
  
  # Filter by native occurrences
  native_centroids <- country_centroids %>% filter(occurrence_type == "native")
  
  if (nrow(native_centroids) == 0) {
    # Use introduced occurrences if no native occurrences are available
    introduced_centroids <- country_centroids %>% filter(occurrence_type == "introduced")
    
    if (nrow(introduced_centroids) == 0) {
      message(paste("No occurrence data for:", plant))
      next  # Skip if no data is available
    }
    
    centroid_coords <- as.data.frame(st_coordinates(introduced_centroids))
  } else {
    centroid_coords <- as.data.frame(st_coordinates(native_centroids))
  }
  
  # Calculate the mean centroid
  mean_centroid <- centroid_coords %>%
    summarize(
      mean_longitude = mean(X, na.rm = TRUE),
      mean_latitude = mean(Y, na.rm = TRUE)
    )
  
  # Create an `sf` object for the mean centroid
  mean_centroid_sf <- st_sfc(st_point(c(mean_centroid$mean_longitude, mean_centroid$mean_latitude)), crs = 4326)
  mean_centroid_coords <- st_coordinates(mean_centroid_sf)
  
  # Calculate the distance to the center of Europe in kilometers
  distance_to_europe <- distGeo(
    c(mean_centroid_coords[1], mean_centroid_coords[2]), 
    c(center_of_europe["longitude"], center_of_europe["latitude"])
  ) / 1000  # Conversion from meters to kilometers
  
  # Add the results to the dataframe
  mean_centroids <- rbind(mean_centroids, data.frame(
    species = plant,
    mean_longitude = mean_centroid_coords[1],
    mean_latitude = mean_centroid_coords[2],
    distance_to_europe_center = distance_to_europe
  ))
  
  
}

# write and read 
write.csv(mean_centroids, "Data/mean_centroids.csv", row.names = FALSE)
mean_centroids <- read.csv("Data/mean_centroids.csv")


# combine centroids with all other data -----------------------------------

mean_centroids_filled <- left_join(relatedness, mean_centroids, by = c("taxon_name" = "species"))
View(mean_centroids_filled)

# save filled data table with all possible effects on the interaction number on non-native plants
write.csv(mean_centroids_filled, "Data/mean_centroids_filled.csv", row.names = FALSE)



# inspect -----------------------------------------------------------------
mean_centroids_filled <- read_csv("Data/mean_centroids_filled.csv")
# check how many inside Europe
sum(mean_centroids_filled$distance_to_europe_center < 2500, na.rm = TRUE)


# viz centroids for SI
# Get country borders as an sf object
countries <- ne_countries(scale = "medium", returnclass = "sf")

# plotting the distribution of non-native plants with mean centroids
ggplot() +
  geom_sf(data = countries, fill = NA, color = "black") + 
  geom_point(data = mean_centroids_filled %>% 
               filter(distance_to_europe_center > 2500), 
             aes(x = mean_longitude, y = mean_latitude), 
             color = "#792c9e", 
             size = 1, 
             shape = 21, 
             fill = "#d1b4e0", 
             alpha = 0.7) +
  geom_point(aes(x = 28.21111, y = 59.55993), 
             color = "black", 
             size = 3, 
             shape = 21,
             fill = "orange") +
  labs(title = " ",
       x = "", y = " ") +
  theme_minimal() +
  theme(legend.position = "bottom",
          panel.grid = element_blank())


# save
showtext_opts(dpi=600)
ggsave(filename = "Figures/fig-si-centroids.png",
       bg = "white",
       height = 3.59,
       width = 6.66,
       dpi = 600)
showtext_opts(dpi=96)



