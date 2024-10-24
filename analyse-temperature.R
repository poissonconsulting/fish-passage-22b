source("header.R")

message("using subsetted data from air2stream model to do proper model comparison with WAIC")
sbf_set_sub("temperature-air2stream")

sbf_load_datas()
sbf_load_objects()

sbf_set_sub("temperature")

source("models-temperature.R")

analysis <- analyse(model, data = data, nthin = 1L)
sbf_save_object(analysis)

div <- partition_div(analysis$stanfit)
print(paste("There were", nrow(div[[1]]), "divergent transitions"))

sbf_open_pdf("mcmc")
plot(analysis)
sbf_close_pdf()
