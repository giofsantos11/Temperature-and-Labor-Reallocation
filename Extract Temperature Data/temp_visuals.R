rm(list = ls())

## Using ERA-5 data
if (!require("reticulate")) install.packages("reticulate")
if (!require("ncdf4")) install.packages("ncdf4") #to manage netCDF format

# Load libraries
library(ncdf4)
library(tidyverse)
library(reticulate)
library(lubridate)
library(raster)
library(sf)

helper <- "/Users/angelosantos/Library/CloudStorage/OneDrive-GeorgeMasonUniversity-O365Production/Temperature and Labor Reallocation/Program Files/Function"

# Load functions from functions.R
source(paste0(helper, "/function_get_temp.R"))

# Install the CDS API
conda_install("r-reticulate", "cdsapi", pip = TRUE)

# Import python CDS-API
cdsapi <- import('cdsapi')

# Start the connection
server <- cdsapi$Client()

years <- 2008:2016
temp_data <- lapply(as.character(years), get_temperature_data, server)
names(temp_data) <- paste0("mean_temp_df_", years)
list2env(temp_data, envir = .GlobalEnv)

#-------------------------------------------------------------------------
#-------------------------------------------------------------------------
# Create charts

# Add the shape file directory
shploc <- "/Users/angelosantos/Library/CloudStorage/OneDrive-GeorgeMasonUniversity-O365Production/Temperature and Labor Reallocation/Data/Raw Data/tza_adm0"
shpadm1loc <- "/Users/angelosantos/Library/CloudStorage/OneDrive-GeorgeMasonUniversity-O365Production/Temperature and Labor Reallocation/Data/Raw Data/geoBoundaries-TZA-ADM1-all"

# Where to store the outputs
vizloc <- "/Users/angelosantos/Library/CloudStorage/OneDrive-GeorgeMasonUniversity-O365Production/Temperature and Labor Reallocation/Outputs/Visuals"
#-------------------------------------------------------------------------
#-------------------------------------------------------------------------
# 1. Map of Tanzania showing changes in temperature between two years
# Update the start and end year below

startyear <- 2008
endyear <- 2015

#-------------------------------------------------------------------------
# Merge the data frames
mean_temp_df_diff <- merge(get(paste0("mean_temp_df_", startyear)), 
                           get(paste0("mean_temp_df_", endyear)), by = c("lon", "lat"))

# Calculate the difference in mean temperature
mean_temp_df_diff$diff_mean_temp <- mean_temp_df_diff$mean_temp.y - mean_temp_df_diff$mean_temp.x
mean_temp_df_diff <- mean_temp_df_diff[, c("lon", "lat", "diff_mean_temp")]

# Convert the mean temperature data to a raster object
r <- rasterFromXYZ(mean_temp_df_diff, crs = CRS("+init=epsg:4326"))
# plot(r)

# Superimpose the map of Tanzania
# Download and read the shapefile of Tanzania
tanzania <- st_read(paste0(shploc, "/tza_adm0.shp"))

# Project the shapefile to match the raster
tanzania_sf <- st_transform(tanzania, crs(r))
tanzania_sp <- as(tanzania, "Spatial")

# Crop
tanzania_sp.crop <- mask(r, tanzania_sp)

# Produce the map
temp_map <- paste0(vizloc, "/tza_map_temp_diff_", startyear, "_", endyear, ".png")
png(filename = temp_map, width = 800, height = 600)

par(mar = c(2, 2, 5, 2))  # Set smaller margins (default is c(5, 4, 4, 2) + 0.1)
map_title <- paste0("Noon-time temperature (°C) difference between ", startyear, " and ", endyear)
wrapped_title <- strwrap(map_title, width = 50)  # Adjust the width as needed
wrapped_title <- paste(wrapped_title, collapse = "\n")


tanzania_sp.crop <- mask(r, tanzania_sp)
plot(tanzania_sp.crop, main = wrapped_title, axes = FALSE, box = FALSE, cex.main = 2, font.main = 1)
plot(tanzania_sp, add = TRUE)

dev.off()

#-------------------------------------------------------------------------
# 2. Create histogram indicators

temp_histo <- paste0(vizloc, "/tza_histogram_temp_diff_", startyear, "_", endyear, ".png")
png(filename = temp_histo, width = 600, height = 400)

mean_val <- mean(mean_temp_df_diff$diff_mean_temp)

ggplot(mean_temp_df_diff, aes(x = diff_mean_temp)) +
  geom_histogram(color = "black", fill = "skyblue", bins = 20) +
  geom_vline(xintercept = mean_val, linetype = "dashed", color = "red") +
  annotate("text", x = 0.83, y = 450, label = paste0("Mean: ", round(mean_val, 2), "°C"), color = "red") +
  labs(x = "Difference in Mean Temperature (°C)", y = "Frequency") +
  theme_bw()

dev.off()

#-------------------------------------------------------------------------
# 3. District map of Tanzania
tanzania_adm1 <- st_read(paste0(shpadm1loc, "/geoBoundaries-TZA-ADM1.shp"))

# Load required library
library(tibble)

# Create a named vector with shapeIDs
tanzania_shapeIDs <- c(
  "Dodoma" = "TZ-03",
  "Arusha" = "TZ-01",
  "Kilimanjaro" = "TZ-09",
  "Tanga" = "TZ-25",
  "Morogoro" = "TZ-16",
  "Pwani" = "TZ-19",
  "Dar_es_Salaam" = "TZ-02",
  "Lindi" = "TZ-12",
  "Mtwara" = "TZ-17",
  "Ruvuma" = "TZ-21",
  "Iringa" = "TZ-04",
  "Mbeya" = "TZ-14",
  "Singida" = "TZ-23",
  "Tabora" = "TZ-24",
  "Rukwa" = "TZ-20",
  "Kigoma" = "TZ-08",
  "Shinyanga" = "TZ-22",
  "Kagera" = "TZ-05",
  "Mwanza" = "TZ-18",
  "Mara" = "TZ-13",
  "Manyara" = "TZ-26"
)

# Create a data frame with region data and shapeIDs
district_data <- tibble(
  District = names(tanzania_shapeIDs),
  shapeISO = tanzania_shapeIDs,
  `2006` = c(84, 68, 72, 81, 84, 75, 13, 76, 80, 87, 82, 81, 93, 88, 86, 78, 78, 86, 73, 86, 87),
  `2014` = c(87, 62, 66, 73, 80, 75, 4, 91, 82, 84, 70, 49, 74, 67, 76, 78, 87, 92, 76, 84, 82)
)

# Merge shape file
merged_adm01 <- right_join(district_data, tanzania_adm1, by = "shapeISO")

# Assuming you have raster data for 2006 and 2014
r_2005 <- rasterFromXYZ(mean_temp_df_2005, crs = CRS("+init=epsg:4326"))
r_2013 <- rasterFromXYZ(mean_temp_df_2013, crs = CRS("+init=epsg:4326"))

# Extract raster values for each region
temp_2005_by_region <- extract(r_2005, tanzania_adm1)
temp_2013_by_region <- extract(r_2013, tanzania_adm1)

# Calculate mean temperature for each region
mean_temp_2005 <- sapply(temp_2005_by_region, mean, na.rm = TRUE)
mean_temp_2013 <- sapply(temp_2013_by_region, mean, na.rm = TRUE)

# Add mean temperature to the district_data dataframe
merged_adm01 <- merged_adm01 %>%
  mutate(`Mean_2005` = mean_temp_2005,
         `Mean_2013` = mean_temp_2013)

# Calculate the change in columns between 2014 and 2006
merged_adm01 <- merged_adm01 %>%
  mutate(Change = `2014` - `2006`)

# Calculate the change in temperature between 2006 and 2014
merged_adm01 <- merged_adm01 %>%
  mutate(Temp_Change = Mean_2013 - Mean_2005)

merged_data_filtered <- merged_adm01 %>%
  filter(Temp_Change >= -1)

scatter <- paste0(vizloc, "/scatter_diff_", startyear, "_", endyear, ".png")
png(filename = scatter, width = 600, height = 400)

# Create a scatterplot with ggplot2
ggplot(merged_data_filtered, aes(x = Temp_Change, y = Change)) +
  geom_point() +  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  labs(x = "Change in Temperature (°C) ", y = "%pt change in Agricultural Employment (2014 - 2006)") +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

dev.off()
# Plot the shapefile
plot(tanzania_adm1)


plot(tanzania_adm1)

