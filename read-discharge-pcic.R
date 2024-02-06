library(ncdf4)
library(terra)
library(jsonlite)
library(stringr)
library(lubridate)
# Read the JSON data into R
json_data <- fromJSON("https://data.pacificclimate.org/portal/hydro_model_out/catalog/catalog.json")

# Filter the data
filtered_data <- json_data[str_detect(names(json_data), "BASEFLOW") & str_detect(names(json_data), "CanESM2_rcp45")]

# Open the dataset
url <- filtered_data[[1]]

nc_data <- nc_open(url)

# Print an overview of the file
# print(nc_data)


# Read variables
time <- ncvar_get(nc_data, "time")
lat <- ncvar_get(nc_data, "lat")
lon <- ncvar_get(nc_data, "lon")

# Inspect the variables
# print(time)
# print(lat)
# print(lon)

# Convert these dates into "days since 1945-1-1"
origin <- as.Date("1945-01-01")
dates <- seq(as_date("2019-07-13"), as_date("2021-10-29"), by = "day")

target_dates <- as.Date(dates)
days_since_origin <- as.numeric(target_dates - origin)

# Now, days_since_origin contains the number of days since 1945-1-1 for each target date
# print(days_since_origin)

# Find the closest indices for your target days
date_indices <- sapply(days_since_origin, function(day) which.min(abs(time - day)))

# Define your bounding box
target_lat_min <- 53.32
target_lat_max <- 54.89
target_lon_min <- -127.53
target_lon_max <- -122.92

# Find indices that match the bounding box
lat_indices <- which(lat >= target_lat_min & lat <= target_lat_max)
lon_indices <- which(lon >= target_lon_min & lon <= target_lon_max)

# print(lat_indices)
# print(lon_indices)

# Close the ncdf4 file connection
nc_close(nc_data)


# Using terra
r <- rast(url)
r_subset <- crop(r, ext(target_lon_min, target_lon_max, target_lat_min, target_lat_max))