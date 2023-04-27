# Define a function to retrieve data for a given year
get_temperature_data <- function(year, server) {
  # Create the query
  query <- r_to_py(list(
    variable= "2m_temperature",
    product_type= "reanalysis",
    year= year,
    month= str_pad(1:12,2,"left","0"),
    day= str_pad(1:31,2,"left","0"),
    time= str_c(12,"00",sep=":")%>%str_pad(5,"left","0"),
    format= "netcdf",
    area = "0.5/29.583/-11.733/40.45"
  ))
  
  # Query to get the ncdf
  server$retrieve("reanalysis-era5-single-levels", query, paste0("era5_ta_", year, ".nc"))
  
  # Open the connection with the ncdf file
  nc <- nc_open(paste0("era5_ta_", year, ".nc"))
  
  # Extract lon, lat, time, and data
  lat <- ncvar_get(nc, 'latitude')
  lon <- ncvar_get(nc, 'longitude')
  t <- ncvar_get(nc, "time")
  data <- ncvar_get(nc, "t2m")
  
  # Close the connection with the ncdf file
  nc_close(nc)
  
  # Calculate the mean temperature for the year
  mean_temp <- apply(data, c(1, 2), mean)
  
  # Convert the mean temperature data to a data frame with latitude and longitude
  lonlat <- expand.grid(lon = lon, lat = lat)
  mean_temp_df <- data.frame(lon = lonlat$lon, lat = lonlat$lat, mean_temp = c(mean_temp))
  mean_temp_df$mean_temp <- mean_temp_df$mean_temp - 273.15
  
  return(mean_temp_df)
}
