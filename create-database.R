source("header.R")

sbf_set_db_name("fish-passage-22")

sbf_create_db()

sbf_load_datas(sub = "tidy/water-temp")
sbf_load_datas(sub = "tidy/discharge")
sbf_load_datas(sub = "tidy/air-temp")
sbf_load_datas(sub = "tidy/solar-rad")
sbf_load_datas(sub = "tidy/dewpoint-temp")
sbf_load_datas(sub = "tidy/surface-pressure")

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

sbf_execute_db("CREATE TABLE air_temp_site (
               cell_id TEXT NOT NULL,
               geometry BLOB NOT NULL,
               PRIMARY KEY(cell_id))")

sbf_save_data_to_db(air_temp_site)

sbf_execute_db("CREATE TABLE air_temp (
               cell_id TEXT NOT NULL,
               site TEXT NOT NULL,
               date TEXT NOT NULL,
               time TEXT NOT NULL,
               air_temp REAL NOT NULL,
               CHECK(
               DATE(date) IS date AND
               time == STRFTIME('%H:%M:%S', time) AND
               air_temp > -50 AND
               air_temp < 50
               ),
               PRIMARY KEY (cell_id, site, date, time),
               FOREIGN KEY (cell_id) REFERENCES air_temp_site(cell_id),
               FOREIGN KEY (site) references water_temp_site(site))")

sbf_save_data_to_db(air_temp)

sbf_execute_db("CREATE TABLE solar_rad_site (
               cell_id TEXT NOT NULL,
               geometry BLOB NOT NULL,
               PRIMARY KEY(cell_id))")

sbf_save_data_to_db(solar_rad_site)

sbf_execute_db("CREATE TABLE solar_rad (
               cell_id TEXT NOT NULL,
               site TEXT NOT NULL,
               date TEXT NOT NULL,
               time TEXT NOT NULL,
               solar_rad REAL NOT NULL,
               CHECK(
               DATE(date) IS date AND
               time == STRFTIME('%H:%M:%S', time) AND
               solar_rad >= 0 AND
               solar_rad < 5e7
               ),
               PRIMARY KEY (cell_id, site, date, time),
               FOREIGN KEY (cell_id) REFERENCES solar_rad_site(cell_id),
               FOREIGN KEY (site) references water_temp_site(site))")

sbf_save_data_to_db(solar_rad)

sbf_execute_db("CREATE TABLE dewpoint_temp_site (
               cell_id TEXT NOT NULL,
               geometry BLOB NOT NULL,
               PRIMARY KEY(cell_id))")

sbf_save_data_to_db(dewpoint_temp_site)

sbf_execute_db("CREATE TABLE dewpoint_temp (
               cell_id TEXT NOT NULL,
               site TEXT NOT NULL,
               date TEXT NOT NULL,
               time TEXT NOT NULL,
               dewpoint_temp REAL NOT NULL,
               CHECK(
               DATE(date) IS date AND
               time == STRFTIME('%H:%M:%S', time) AND
               dewpoint_temp >= -50 AND
               dewpoint_temp < 40
               ),
               PRIMARY KEY (cell_id, site, date, time),
               FOREIGN KEY (cell_id) REFERENCES dewpoint_temp_site(cell_id),
               FOREIGN KEY (site) references water_temp_site(site))")

sbf_save_data_to_db(dewpoint_temp)

sbf_execute_db("CREATE TABLE surface_pressure_site (
               cell_id TEXT NOT NULL,
               geometry BLOB NOT NULL,
               PRIMARY KEY(cell_id))")

sbf_save_data_to_db(surface_pressure_site)

sbf_execute_db("CREATE TABLE surface_pressure (
               cell_id TEXT NOT NULL,
               site TEXT NOT NULL,
               date TEXT NOT NULL,
               time TEXT NOT NULL,
               surface_pressure REAL NOT NULL,
               CHECK(
               DATE(date) IS date AND
               time == STRFTIME('%H:%M:%S', time) AND
               surface_pressure >= 1e4 AND
               surface_pressure < 1e6
               ),
               PRIMARY KEY (cell_id, site, date, time),
               FOREIGN KEY (cell_id) REFERENCES surface_pressure_site(cell_id),
               FOREIGN KEY (site) references water_temp_site(site))")

sbf_save_data_to_db(surface_pressure)

sbf_execute_db("CREATE TABLE discharge (
               site TEXT NOT NULL,
               date TEXT NOT NULL,
               discharge REAL NOT NULL,
               CHECK(
               DATE(date) IS date AND
               discharge >= 0 AND
               discharge <= 2000
               ),
               PRIMARY KEY (site, date),
               FOREIGN KEY (site) REFERENCES water_temp_site(site)
               )")

sbf_save_data_to_db(discharge)
