source("header.R")

data <- tibble(X = 1:10, Y = rnorm(10, 1:10),
               type = c("a", "a", "b", "b", "c", "c", "d", "d", "e", "e"))

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
