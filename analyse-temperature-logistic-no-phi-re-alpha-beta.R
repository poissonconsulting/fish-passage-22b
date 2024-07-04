source("header.R")

sbf_set_sub("temperature")

sbf_load_datas()
sbf_load_objects()

sbf_set_sub("temperature", "logistic-no-phi-re-alpha-beta")

source("models-temperature-logistic-no-phi-re-alpha-beta.R")

analysis <- analyse(model, data = data, nthin = 2L)
sbf_save_object(analysis)

div <- partition_div(analysis$stanfit)
print(paste("There were", nrow(div[[1]]), "divergent transitions"))

sbf_open_pdf("mcmc")
plot(analysis)
sbf_close_pdf()
