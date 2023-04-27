
# Main code

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

helper <- "/Users/angelosantos/Library/CloudStorage/OneDrive-GeorgeMasonUniversity-O365Production/Temperature and Labor Reallocation/Program Files/Functions"

# Load functions from functions.R
source(paste0(helper, "/function_get_temp_xy_year.R"))


# Install the CDS API
conda_install("r-reticulate", "cdsapi", pip = TRUE)

# Import python CDS-API
cdsapi <- import('cdsapi')

# Start the connection
server <- cdsapi$Client()

interview_dates <- c("2012-10-01", "2013-09-30", "2016-11-01")
latitude <- 0.5
longitude <- 29.583
past_year_avg_temps <- get_past_year_avg_temp(interview_dates, latitude, longitude)

