source("header.R")

sbf_set_sub("tidy", "discharge")
sbf_load_datas()

gp <- ggplot(discharge) +
  geom_line(aes(x = date, y = discharge, colour = site, group = site)) +
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

gp <- ggplot(discharge) +
  geom_line(aes(x = date, y = level, colour = site, group = site)) +
  ylab("Level (m)") +
  xlab("Date") +
  NULL

sbf_open_window(4, 4)
sbf_print(gp)

sbf_save_plot(
  x_name = "depth",
  report = FALSE,
  caption = "Mean daily water level (m) by date and site"
)