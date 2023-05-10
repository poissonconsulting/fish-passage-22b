source("header.R")

sbf_set_sub("tidy")
sbf_load_datas()

gp <- ggplot(data = water_temp, aes(x = date_time, y = temp)) +
  geom_point(aes(colour = flag), alpha = 0.1, size = 0.0001) + 
  scale_color_disc_poisson(order = c("black", "red", "blue")) +
  facet_wrap(~site) +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  xlab("Date") +
  ylab("Temperature (˚C)") +
  NULL

sbf_open_window(14, 8)
sbf_print(gp)

sbf_save_plot(
  x_name = "temp_sites",
  sub = "temp",
  report = TRUE,
  caption = "Water temperature (degrees C) by date and site"
)
