source("header.R")

sbf_set_db_name("fish-passage-22")

sbf_create_db()

sbf_load_datas(sub = "tidy/water-temp")
sbf_load_datas(sub = "tidy/discharge")

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

sbf_execute_db("CREATE TABLE discharge_site (
               station_id TEXT NOT NULL,
               river TEXT NOT NULL,
               station TEXT NOT NULL,
               latitude REAL NOT NULL,
               longitude REAL NOT NULL,
               altitude REAL,
               CHECK(
               LENGTH(station_id) = 7 AND
               latitude > 53 AND
               latitude < 55 AND
               longitude < -122 AND
               longitude > -128 AND 
               altitude > 0 AND
               altitude < 2500
               ),
               PRIMARY KEY (station_id))")

sbf_save_data_to_db(discharge_site)

sbf_execute_db("CREATE TABLE discharge (
               station_id TEXT NOT NULL,
               date TEXT NOT NULL,
               mean_discharge REAL NOT NULL,
               CHECK(
               DATE(date) IS date AND
               mean_discharge > 0 AND
               mean_discharge < 1000
               ),
               PRIMARY KEY (station_id, date),
               FOREIGN KEY (station_id) REFERENCES discharge_site(station_id))")

sbf_save_data_to_db(discharge)
