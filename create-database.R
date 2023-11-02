source("header.R")

sbf_set_db_name("fish-passage-22")

sbf_create_db()

sbf_load_datas(sub = "tidy/water-temp")
sbf_load_datas(sub = "tidy/discharge")
sbf_load_datas(sub = "tidy/weather")

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
# 
sbf_execute_db("CREATE TABLE discharge_site (
               station_id TEXT NOT NULL,
               river TEXT NOT NULL,
               station TEXT NOT NULL,
               altitude REAL,
               geometry BLOB NOT NULL,
               CHECK(
               LENGTH(station_id) = 7 AND
               altitude > 0 AND
               altitude < 2500
               ),
               PRIMARY KEY (station_id))")

sbf_save_data_to_db(discharge_site)

sbf_execute_db("CREATE TABLE discharge (
               station_id TEXT NOT NULL,
               date TEXT NOT NULL,
               discharge REAL NOT NULL,
               CHECK(
               DATE(date) IS date AND
               discharge > 0 AND
               discharge < 1000
               ),
               PRIMARY KEY (station_id, date),
               FOREIGN KEY (station_id) REFERENCES discharge_site(station_id))")

sbf_save_data_to_db(discharge)

sbf_execute_db("CREATE TABLE weather_site (
               cell_id TEXT NOT NULL,
               geometry BLOB NOT NULL,
               PRIMARY KEY(cell_id))")

sbf_save_data_to_db(weather_site)

sbf_execute_db("CREATE TABLE weather_temp (
               cell_id TEXT NOT NULL,
               site TEXT NOT NULL,
               date TEXT NOT NULL,
               time TEXT NOT NULL,
               air_temp REAL NOT NULL,
               evap_water REAL NOT NULL,
               evap_total REAL NOT NULL,
               snowmelt REAL NOT NULL,
               runoff_subsurface REAL NOT NULL,
               runoff_surface REAL NOT NULL,
               net_solar_rad REAL NOT NULL,
               precip REAL NOT NULL,
               soil_vol_1 REAL NOT NULL,
               CHECK(
               DATE(date) IS date AND
               time == STRFTIME('%H:%M:%S', time) AND
               air_temp > -50 AND
               air_temp < 50 AND
               snowmelt >= 0 AND
               runoff_subsurface >= 0 AND
               runoff_surface >= 0 AND
               net_solar_rad >= 0 AND
               precip >= 0 AND 
               soil_vol_1 > 0 
               ),
               PRIMARY KEY (cell_id, site, date, time),
               FOREIGN KEY (cell_id) REFERENCES weather_site(cell_id),
               FOREIGN KEY (site) references water_temp_site(site))")

sbf_save_data_to_db(weather_temp)

sbf_execute_db("CREATE TABLE weather_discharge (
               cell_id TEXT NOT NULL,
               station_id TEXT NOT NULL,
               date TEXT NOT NULL,
               time TEXT NOT NULL,
               air_temp REAL NOT NULL,
               evap_water REAL NOT NULL,
               evap_total REAL NOT NULL,
               snowmelt REAL NOT NULL,
               runoff_subsurface REAL NOT NULL,
               runoff_surface REAL NOT NULL,
               net_solar_rad REAL NOT NULL,
               precip REAL NOT NULL,
               soil_vol_1 REAL NOT NULL,
               CHECK(
               DATE(date) IS date AND
               time == STRFTIME('%H:%M:%S', time) AND
               air_temp > -50 AND
               air_temp < 50 AND
               snowmelt >= 0 AND
               runoff_subsurface >= 0 AND
               runoff_surface >= 0 AND
               net_solar_rad >= 0 AND
               precip >= 0 AND 
               soil_vol_1 > 0 
               ),
               PRIMARY KEY (cell_id, station_id, date, time),
               FOREIGN KEY (cell_id) REFERENCES weather_site(cell_id),
               FOREIGN KEY (station_id) references discharge_site(station_id))")

sbf_save_data_to_db(weather_discharge)

sbf_execute_db("CREATE TABLE us_weather_discharge_obs (
               station_id TEXT NOT NULL,
               date TEXT NOT NULL,
               mean_us_snowmelt REAL NOT NULL,
               mean_us_runoff_surface REAL NOT NULL,
               mean_us_precip REAL NOT NULL,
               upstream_area REAL NOT NULL,
               CHECK(
               DATE(date) IS date AND
               mean_us_snowmelt >= 0 AND
               mean_us_runoff_surface >= 0 AND
               mean_us_precip >= 0 AND 
               upstream_area > 1000
               ),
               PRIMARY KEY (station_id, date),
               FOREIGN KEY (station_id) REFERENCES discharge_site(station_id))")

sbf_save_data_to_db(us_weather_discharge_obs)

sbf_execute_db("CREATE TABLE us_weather_discharge_pred (
               site TEXT NOT NULL,
               date TEXT NOT NULL,
               mean_us_snowmelt REAL NOT NULL,
               mean_us_runoff_surface REAL NOT NULL,
               mean_us_precip REAL NOT NULL,
               upstream_area REAL NOT NULL,
               CHECK(
               DATE(date) IS date AND
               mean_us_snowmelt >= 0 AND
               mean_us_runoff_surface >= 0 AND
               mean_us_precip >= 0 AND 
               upstream_area > 1000
               ),
               PRIMARY KEY (site, date),
               FOREIGN KEY (site) REFERENCES water_temp_site(site))")

sbf_save_data_to_db(us_weather_discharge_pred)
