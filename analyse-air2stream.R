source("header.R")

sbf_set_sub("temperature-air2stream")

sbf_load_datas()
sbf_load_objects()

source("models-air2stream.R")

analysis <- analyse(model, data = data, nthin = 5L)
sbf_save_object(analysis)

div <- partition_div(analysis$stanfit)
print(paste("There were", nrow(div[[1]]), "divergent transitions"))

sbf_open_pdf("mcmc")
plot(analysis)
sbf_close_pdf()

sbf_open_pdf("mcmc-random")
plot(analysis, param_type = "random")
sbf_close_pdf()
