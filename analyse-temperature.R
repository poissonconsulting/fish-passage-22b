source("header.R")

sbf_set_sub("temperature")

sbf_load_datas()
sbf_load_objects()

source("models-temperature.R")

analysis <- analyse(model, data = data, nthin = 1L)
sbf_save_object(analysis)

div <- partition_div(analysis$stanfit)
paste("There were", nrow(div[[1]]), "divergent transitions")

sbf_open_pdf("mcmc")
plot(analysis)
sbf_close_pdf()
