source("header.R")

sbf_set_sub("read", "discharge")
sbf_load_datas()

# Discharge ----
discharge_meta %<>% 
  filter(`Station Number` %in% unique(discharge$ID)) %>% 
  select(id = `Station Number`, Latitude, Longitude)

discharge %<>% 
  rename_with(snakecase::to_snake_case)

sbf_set_sub("clean", "discharge")
sbf_save_datas()