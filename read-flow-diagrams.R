source("header.R")

sbf_set_sub("flow-diagrams")
dir <- file.path(dir, "Flow Diagrams")
root_folder <- getwd()

file.copy(
  from = file.path(dir, "energy.png"),
  to = root_folder,
  overwrite = TRUE
)
file.copy(
  from = file.path(dir, "discharge_1.png"),
  to = root_folder,
  overwrite = TRUE
)
file.copy(
  from = file.path(dir, "discharge_2.png"),
  to = root_folder,
  overwrite = TRUE
)
file.copy(
  from = file.path(dir, "discharge_3.png"),
  to = root_folder,
  overwrite = TRUE
)

