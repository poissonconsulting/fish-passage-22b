source("header.R")

sbf_set_sub("query")
sbf_load_datas()

gp <- ggplot(discharge) +
  geom_line(aes(x = date, y = discharge, colour = station_id, group = station_id)) +
  scale_colour_disc_poisson() +
  ylab(expression("Discharge"~(m^3/s))) +
  xlab("Date") +
  labs(colour = "Station ID") +
  NULL

sbf_open_window(6, 4)
sbf_print(gp)

sbf_save_plot(
  x_name = "discharge",
  report = FALSE,
  caption = "Mean daily discharge (cubic metres per second) by date and site"
)