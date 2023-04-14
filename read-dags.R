source("header.R")

sbf_set_sub("dags")
dir <- file.path(dir, "DAGs")
root_folder <- getwd()

file.copy(
  from = file.path(dir, "discharge_dag.png"),
  to = root_folder,
  overwrite = TRUE
)
file.copy(
  from = file.path(dir, "water_temperature_dag.png"),
  to = root_folder,
  overwrite = TRUE
)
file.copy(
  from = file.path(dir, "fish_density_dag.png"),
  to = root_folder,
  overwrite = TRUE
)