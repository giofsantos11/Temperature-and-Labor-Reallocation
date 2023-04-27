# Define a function to retrieve data for a given date range
get_temperature_data <- function(start_date, end_date, latitude, longitude, server) {
  date_seq <- seq(as.Date(start_date), as.Date(end_date), by = "month")
  years <- unique(format(date_seq, "%Y"))
  months <- unique(format(date_seq, "%m"))
  
  # Create the query
  query <- r_to_py(list(
    variable= "2m_temperature",
    product_type= "reanalysis",
    year= years,
    month= months,
    day= str_pad(1:31,2,"left","0"),
    time= str_c(12,"00",sep=":")%>%str_pad(5,"left","0"),
    format= "netcdf",
    area = "0.5/29.583/-11.733/40.45"
  ))
  
  # Generate the file name based on the start and end dates
  file_name <- paste0("era5_ta_", start_date, "_to_", end_date, ".nc")
  
  # Query to get the ncdf
  server$retrieve("reanalysis-era5-single-levels", query, file_name)
  
  # Open the connection with the ncdf file
  nc <- nc_open(file_name)
  
  # Extract lon, lat, time, and data
  lat <- ncvar_get(nc, 'latitude')
  lon <- ncvar_get(nc, 'longitude')
  t <- ncvar_get(nc, "time")
  data <- ncvar_get(nc, "t2m")
  
  # Close the connection with the ncdf file
  nc_close(nc)
  
  # Calculate the mean temperature for the date range
  mean_temp <- apply(data, c(1, 2), mean)
  
  # Find the closest lat and lon indices
  lat_idx <- which.min(abs(lat - latitude))
  lon_idx <- which.min(abs(lon - longitude))
  
  # Extract the temperature at the given location
  location_temp <- mean_temp[lon_idx, lat_idx] - 273.15
  
  return(location_temp)

}

get_past_year_avg_temp <- function(interview_dates, latitude, longitude) {
  past_year_avg_temps <- sapply(interview_dates, function(interview_date) {
    start_date <- as.Date(interview_date) - 365
    end_date <- as.Date(interview_date) - 1
    location_temp <- get_temperature_data(start_date, end_date, latitude, longitude, server)
    return(location_temp)
  })
  
  return(past_year_avg_temps)
}


