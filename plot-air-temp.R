source("header.R")

sbf_set_sub("tidy", "air-temp")
sbf_load_datas()

gp <- ggplot(air_temp, aes(x = date, y = mean_temp)) +
  geom_point(size = 0.1) +
  geom_line(linewidth = 0.1) +
  scale_x_date() +
  facet_wrap(~site) +
  xlab("Date") +
  ylab("Air temperature (˚C)") +
  NULL

sbf_open_window(8, 6)
sbf_print(gp)

sbf_save_plot(
  x_name = "air-temp", 
  report = FALSE,
  caption = "ERA5 modelled air temperature (˚C) closest to each site, by date."
)