source("header.R")

sbf_copy_db(file.path("~/Poisson/Data/fish-passage/2022/dbs",
                      "fish-passage-21.sqlite"),
            db_name = "fish-passage-21", 
            exists = FALSE)
