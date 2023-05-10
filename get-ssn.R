source("header.R")

### Note: openSTARS package written with deprecated `rgrass7`. There will be many warnings that can be ignored.

sbf_set_sub("ssn")

source("init_grass.R")

# Load files into GRASS - this is an example:
# dem_path <- system.file("extdata", "nc", "elev_ned_30m.tif", package = "openSTARS")
# sites_path <- system.file("extdata", "nc", "sites_nc.shp", package = "openSTARS")
# streams_path <- system.file("extdata", "nc", "streams.shp", package = "openSTARS")
# preds_v_path <- system.file("extdata", "nc", "pointsources.shp", package = "openSTARS")
# preds_r_path <- system.file("extdata", "nc", "landuse_r.tif", package = "openSTARS")


## Need elevation .tif file for nechako
dem_path <- "output/objects/ssn/elev.tif"
## Need locations of sites
sites_path <- "output/objects/ssn/site.shp"

import_data(dem = dem_path, sites = sites_path)

# Derive streams from DEM
# 6500 threshold used by Ecofish to eliminate complex confluences
derive_streams(accum_threshold = 8000, condition = TRUE, clean = TRUE)

# Check and correct complex confluences
cj <- check_compl_confluences()
if(cj){
  correct_compl_confluences()
}

# calculate slope as potential predictor
execGRASS("r.slope.aspect", flags = c("overwrite","quiet"),
          parameters = list(
            elevation = "dem",
            slope = "slope"
          ))

# Prepare edges
calc_edges()
calc_attributes_edges(input_raster = c("slope"),
                      stat_rast = c("max"),
                      attr_name_rast = c("maxSlo"))

# Prepare site
calc_sites()

# Usually, only one of the following methods is needed. The exact one takes
# longer to run
# approximate potential predictor variables for each site based on edge values
# calc_attributes_sites_approx(input_attr_name = c("maxSlo"),
#                              output_attr_name = c("maxSloA"),
#                              stat = c("max"))

# exact potential predictor variables for each site based on catchments
calc_attributes_sites_exact(input_raster = c("slope"),
                            attr_name_rast = c("maxSloEx"),
                            stat_rast = c("max"))

# Plot data
library(sp)
dem <- readRAST("dem", ignore.stderr = TRUE, plugin = FALSE)
sites <- readVECT("sites", ignore.stderr = TRUE)
sites_orig <-  readVECT("sites_o", ignore.stderr = TRUE)
edges <- readVECT("edges", ignore.stderr = TRUE)
plot(dem, col = terrain.colors(20))
lines(edges, col = "blue")
points(sites_orig, pch = 4)
cols <- colorRampPalette(c("blue", "red"))(length(sites$H2OArea))[rank(sites$H2OArea)]
points(sites, pch = 16, col = cols)

# Write data to SSN Folder
ssn_dir <- file.path(getwd(), "nc.ssn")
export_ssn(ssn_dir, delete_directory = TRUE)

# Check if all files are ok
library(SSN)
check_ssn(ssn_dir)

# Load into SSN-package
ssn_obj <- importSSN(ssn_dir, o.write = TRUE)
print(ssn_obj)
