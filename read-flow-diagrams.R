source("header.R")

sbf_set_sub("flow-diagrams")
dir <- file.path(dir, "Flow Diagrams")

sbf_save_png(
  x = file.path(dir, "energy.png"),
  report = FALSE
)

sbf_save_png(
  x = file.path(dir, "discharge_1.png"),
  report = FALSE
)

sbf_save_png(
  x = file.path(dir, "discharge_2.png"),
  report = FALSE
)

sbf_save_png(
  x = file.path(dir, "discharge_3.png"),
  report = FALSE
)