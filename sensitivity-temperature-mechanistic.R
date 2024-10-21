source("header.R")

sbf_set_sub("temperature-mechanistic")
W <- sbf_load_object("W")
E <- sbf_load_object("E")
H <- sbf_load_object("H")
D <- sbf_load_object("D")
flow_con_mat <- sbf_load_object("flow_con_mat")

source("sensitivity.R")
