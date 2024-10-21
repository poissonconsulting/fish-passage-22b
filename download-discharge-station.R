source("header.R")

# make a bash file that can be run with `bash pcic_dl.sh` on commandline.
pcic_dl_sh(
  pcic_url_catalog = "https://data.pacificclimate.org/portal/hydro_model_out/catalog/catalog.json",
  # define the type of data ie. Baseflow or Runoff
  var = "BASEFLOW",
  # define the model and scenario - should seperate
  model = "ACCESS1-0_rcp85",
  # define the location of the data directory to download to
  dir_data = "~/Dropbox/New Graph/fish-passage-22/Data/Discharge/pcic",
  # Define the bounding box of the area of interest
  lat_min = 52.5,
  lat_max = 56.5,
  lon_min = -128,
  lon_max = -122.5,
  # define the date range of interest
  date_start = bounding_dates[1],
  date_end = bounding_dates[2],
  append = FALSE
)

pcic_dl_sh(
  dir_data = file.path(dir, "Data/Discharge/pcic")
)
pcic_dl_sh(
  dir_data = file.path(dir, "Data/Discharge/pcic"),
  var = "RUNOFF",
  append = TRUE
)

# run the file to generate the output
system2("bash", args = "pcic_dl.sh")

## Add baseflow and runoff to create discharge.  Not sure if we can/should do that first as it was
# done after getting averages in bcfishpass. 
