source("header.R")

sbf_set_sub("read", "discharge")

# This is discharge data for the entire Fraser River basin from the 
# Global Runoff Data Centre (GRDC)
# Downloaded on 2023-10-31 from
# https://portal.grdc.bafg.de/applications/public.html?publicuser=PublicUser#dataDownload/Subregions
### Not going to be able to programatically download this data.
dir <- file.path(dir, "Data/Discharge/GRDC text")
files <- list.files(dir, full.names = TRUE) %>% 
  as_list()

discharge <- 
  map(
    .x = files, 
    .f = \(x) {
      read_csv(x, id = "file")
    }
  ) %>% 
  list_rbind()

rm(dir, files)

sbf_save_datas()

if (FALSE) {
  sbf_compare_data_archive()
}