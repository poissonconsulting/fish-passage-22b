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
print(time)
print(lat)
# print(lon)

# Convert these dates into "days since 1945-1-1"
origin <- as.Date("1945-01-01")
date_start <- as.Date("2019-07-13")
date_end <- as.Date("2021-10-29")
# dates <- seq(as_date("2019-07-13"), as_date("2021-10-29"), by = "day")

# target_dates <- as.Date(dates)
days_ind_start <- as.numeric(date_start - origin)
days_ind_end <- as.numeric(date_end - origin)


# Define your bounding box
lat_min <- 53.32
lat_max <- 54.89
lon_min <- -127.53
lon_max <- -122.92

# Find indices that are closest to your target lat/lon
lat_min_idx <- max(lat[lat < lat_min])
lat_max_idx <- min(lat[lat > lat_max])
lon_min_idx <- max(lon[lon < lon_min])
lon_max_idx <- min(lon[lon > lon_max])


# Close the ncdf4 file connection
nc_close(nc_data)


# gpt4 suggestion:  Using terra
r <- rast(url)
r_subset <- crop(r, ext(target_lon_min, target_lon_max, target_lat_min, target_lat_max))