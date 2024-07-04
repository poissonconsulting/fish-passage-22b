source("header.R")

# make a bash file that can be run with `bash pcic_dl.sh` on commandline.
pcic_dl_sh(dir_data = file.path(dir, "Data/Discharge/pcic"))
pcic_dl_sh(var = "RUNOFF", 
           dir_data = file.path(dir, "Data/Discharge/pcic"),
           append = TRUE)

# run the file to generate the output
system2("bash", args = "pcic_dl.sh")

## Add baseflow and runoff to create discharge.  Not sure if we can/should do that first as it was
# done after getting averages in bcfishpass.  
