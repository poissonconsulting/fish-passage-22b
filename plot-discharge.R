source("header.R")

sbf_set_sub("query")
sbf_load_datas()

gp <- ggplot(discharge) +
  geom_line(aes(x = date, y = discharge, colour = site, group = site)) +
  ylab("Discharge (m^3/s)") +
  xlab("Date") +
  labs(colour = "Site") +
  NULL

sbf_open_window(8, 6)
sbf_print(gp)

sbf_save_plot(
  x_name = "discharge",
  report = FALSE,
  caption = "Mean daily discharge by date and site"
)

discharge %<>% 
  group_by(date) %>% 
  mutate(discharge = as.vector(scale(discharge))) %>% 
  ungroup() %>% 
  left_join(water_temp_site, join_by(site)) %>% 
  ps_activate_sfc()

gp <- mapview(discharge, zcol = "discharge")
gp
