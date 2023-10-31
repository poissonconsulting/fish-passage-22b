source("header.R")

sbf_set_db_name("fish-passage-22")

sbf_create_db()

sbf_load_datas(sub = "tidy")

sbf_execute_db("CREATE TABLE water_temp_site (
               site TEXT NOT NULL,
               site_description TEXT NOT NULL,
               elev REAL NOT NULL,
               start_date TEXT,
               end_date TEXT,
               geometry BLOB NOT NULL,
               CHECK(
               LENGTH(site) = 3 AND
               elev > 0 AND
               elev < 4000 AND
               start_date < end_date
               ),
               PRIMARY KEY (site))")

sbf_save_data_to_db(water_temp_site)

sbf_execute_db("CREATE TABLE water_temp_flag (
               flag TEXT NOT NULL,
               flag_description TEXT NOT NULL,
               CHECK(
               LENGTH(flag) = 1
               ),
               PRIMARY KEY (flag))")

sbf_save_data_to_db(water_temp_flag)

sbf_execute_db("CREATE TABLE water_temp (
               site TEXT NOT NULL,
               date TEXT NOT NULL,
               time TEXT NOT NULL,
               temp REAL NOT NULL,
               flag TEXT NOT NULL,
               comment TEXT,
               CHECK(
               LENGTH(site) = 3 AND
               LENGTH(flag) = 1 AND
               temp > -10 AND
               temp < 50
               ),
               PRIMARY KEY (site, date, time),
               FOREIGN KEY (site) REFERENCES water_temp_site(site),
               FOREIGN KEY (flag) REFERENCES water_temp_flag(flag))")

sbf_save_data_to_db(water_temp)

sbf_execute_db("CREATE TABLE discharge_flag (
               flag TEXT NOT NULL,
               flag_description TEXT NOT NULL,
               CHECK(
               LENGTH(flag) = 1
               ),
               PRIMARY KEY (flag))")

sbf_save_data_to_db(discharge_flag)

sbf_execute_db("CREATE TABLE discharge (
               site TEXT NOT NULL,
               station_id TEXT NOT NULL,
               date TEXT NOT NULL,
               discharge REAL,
               level REAL,
               flag_level TEXT,
               flag_discharge TEXT,
               CHECK(
               LENGTH(site) = 3 AND
               LENGTH(flag_level) = 1 AND
               LENGTH(flag_discharge) = 1 AND
               discharge > 0 AND
               discharge < 100 AND
               level > 0 AND
               level < 500
               ),
               PRIMARY KEY (station_id, date),
               FOREIGN KEY (site) REFERENCES water_temp_site(site),
               FOREIGN KEY (flag_level) REFERENCES discharge_flag(flag),
               FOREIGN KEY (flag_discharge) REFERENCES discharge_flag(flag))")

sbf_save_data_to_db(discharge)
