source("header.R")

sbf_set_sub("query")
sbf_load_datas()

gp <- ggplot(discharge) +
  geom_line(aes(x = date, y = mean_discharge, colour = station_id, group = station_id)) +
  ylab(expression("Discharge"~(m^3/s))) +
  xlab("Date") +
  NULL

sbf_open_window(4, 4)
sbf_print(gp)

sbf_save_plot(
  x_name = "discharge",
  report = FALSE,
  caption = "Mean daily discharge (cubic metres per second) by date and site"
)