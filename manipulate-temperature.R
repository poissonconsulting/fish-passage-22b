source("header.R")

sbf_load_datas("query")


gp <- ggplot(data = data) +
  aes(x = X, y = Y, colour = type) +
  geom_point() +
  scale_colour_disc_poisson()

sbf_open_window()
sbf_print(gp)

sbf_set_sub("temperature")
sbf_save_data(data, "data")

if(FALSE) {
  sbf_compare_data_archive()
}
