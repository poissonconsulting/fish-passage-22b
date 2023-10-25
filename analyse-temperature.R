source("header.R")

sbf_set_sub("temperature")

source("models-temperature.R")

data <- sbf_load_data("data")

D <- sbf_load_object("D") # downstream hydrologic distance matrix
W <- sbf_load_object("W") # weighting matrix
H <- sbf_load_object("H") # total hydrologic distance matrix
flow_con_mat <- sbf_load_object("flow_con_mat") # flow connectivity matrix
E <- sbf_load_object("E") # euclidean distance matrix

analysis <- analyse(model, data = data, nthin = 1L)
sbf_save_object(analysis)

div <- partition_div(analysis$stanfit)
paste("There were", nrow(div[[1]]), "divergent transitions")

sbf_open_pdf("mcmc")
plot(analysis)
sbf_close_pdf()
