# running this script replaces it with the schema to recreate the database

source("header.R")

sbf_set_db_name("fish-passage-21")

sql <- sbf_query_db("SELECT sql FROM sqlite_master")
sql <- sql[[1]]
sql <- sql[!is.na(sql)]
sql <- sql[!grepl("CREATE TABLE readwritesqlite_", sql)]
sql <- vapply(sql, function(x) paste0("sbf_execute_db(\"", x, "\")\n"),
              character(1))

sql <- c("source(\"header.R\")\n",
         "sbf_set_db_name(\"ldr-rb-spawning\")\n",
         "sbf_create_db()\n",
         "# populate individual tables using sbf_save_data_to_db(name_of_table)",
         "# or populate all tables using sbf_save_datas_to_db()\n",
         sql,
         "\n sbf_save_db_to_workbook()\n")

writeLines(sql, "create-database.R")
