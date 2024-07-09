source("header.R")

sbf_set_sub("query")
sbf_load_datas()

water_temp$date_time <- dtt_date_time(
  water_temp$date, 
  water_temp$time, 
  tz = tz_analysis
)

gp <- ggplot(data = water_temp, aes(x = date_time, y = temp)) +
  geom_line(aes(colour = flag, group = "flag"), alpha = 0.5) + 
  scale_color_disc_poisson(order = c("black", "red", "blue")) +
  facet_wrap(~site) +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  xlab("Date") +
  ylab("Water Temperature (˚C)") +
  NULL

sbf_open_window(14, 8)
sbf_print(gp)

sbf_save_plot(
  x_name = "temp_sites",
  report = FALSE,
  caption = "Water temperature (˚C) by date and site"
)