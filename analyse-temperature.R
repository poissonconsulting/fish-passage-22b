source("header.R")

sbf_set_sub("temperature")

source("models-temperature.R")

data <- sbf_load_data("data")

analysis <- analyse(model, data = data)
sbf_save_object(analysis)

sbf_open_pdf("mcmc")
plot(analysis)
sbf_close_pdf()
