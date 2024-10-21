source("header.R")

sbf_set_sub("temperature-air2stream")

sbf_load_datas()
sbf_load_objects()

source("models-air2stream.R")

analysis <- analyse(model, data = data)
sbf_save_object(analysis)

div <- partition_div(analysis$stanfit)
print(paste("There were", nrow(div[[1]]), "divergent transitions"))

sbf_open_pdf("mcmc")
plot(analysis)
sbf_close_pdf()
