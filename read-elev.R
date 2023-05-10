source("header.R")

# Elev ----
dir <- "~/Poisson/Data/fish-passage/2022/Data/SSN/Elev"

elev_files <- list.files(dir, recursive = TRUE, full.names = TRUE, pattern = "\\.tif$")

elev_rast <- map(
  .x = elev_files,
  .f = \(x) raster::raster(x)
)

projection <- "+proj=aea +lat_0=45 +lon_0=-126 +lat_1=50 +lat_2=58.5 +x_0=1000000 +y_0=0 +datum=WGS84 +units=m +no_defs"

elev <- do.call(raster::merge, elev_rast)

elev <- raster::projectRaster(elev, crs = crs)

rm(elev_rast, elev_files)

sbf_save_object(1, x_name = "test", sub = "ssn")

raster::writeRaster(
  elev, 
  filename = "output/objects/ssn/elev.tif",
  format="GTiff",
  overwrite = TRUE
)
