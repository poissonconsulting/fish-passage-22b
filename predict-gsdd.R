source("header.R")

sbf_set_sub("temperature", "logistic-no-phi-re-alpha-beta-gamma")

analysis <- sbf_load_object("analysis")
data <- data_set(analysis)

message("If using a final model with phi, use a different prediction term")

interpolated_temp <-
  predict(analysis, new_data = data, term = "eTemp") %>% 
  select(site, date, temperature = estimate) %>% 
  arrange(site, date) %>% 
  group_by(site) %>%
  group_split() %>% 
  map(
    .f = function(y) {
      site <- y$site[1]
      y_zoo <- zoo::zoo(y$temperature, y$date)
      y_filled <- zoo::na.approx(y_zoo, xout = seq(start(y_zoo), end(y_zoo), by = "day"), na.rm = FALSE)
      y <- tibble(
          site = site,
          date = seq(start(y_zoo), end(y_zoo), by = "day"),
          temperature = as.numeric(y_filled)
        )
      y
    }
  ) %>% 
  bind_rows() %>% 
  mutate(annual = factor(dtt_year(date)))

interpolated_temp %>% 
  filter(site == "CNE") %>% 
  gsdd::gsdd() # start/end dates & other arguments??

gp <- ggplot(interpolated_temp) +
  geom_line(aes(x = date, y = temperature)) +
  facet_wrap(~site) +
  xlab("Date") +
  ylab(expression(paste("Daily Water Temperature (", degree, "C)"))) + 
  guides(x = guide_axis(angle = 45)) +
  NULL

sbf_open_window()
sbf_print(gp)
