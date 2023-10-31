source("header.R")

sbf_set_sub("read", "discharge")

# Downloaded for sites "08JA015" "08JA023" "08JE004"
# during the period 2019-07-13 to 2021-10-29 
# from the Historical Hydrometric Database from the Canadian Water Office
# https://wateroffice.ec.gc.ca/search/historical_e.html
# on 2023-10-30

paths <- list.files(
  "~/Poisson/Data/fish-passage/2022/Data/Discharge",
  full.names = TRUE
)

discharge <- bind_rows(
  read_csv(paths[1], skip = 1),
  read_csv(paths[2], skip = 1),
  read_csv(paths[3], skip = 1)
)

discharge_meta <- read_csv(paths[4])

discharge_flag <-
  tribble(
    ~flag,  ~flag_description,
    "A",      "Partial Day",
    "D",              "Dry",
    "R",          "Revised",
    "B",   "Ice Conditions",
    "E",        "Estimated"
  )

rm(paths)

sbf_save_datas()

if (FALSE) {
  sbf_compare_data_archive()
}