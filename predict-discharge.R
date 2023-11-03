source("header.R")

options(mb.parallel = FALSE)

sbf_set_sub("discharge")

analysis <- sbf_load_object("analysis")
data <- data_set(analysis)

glance <- glance(analysis)
coef <- coef(analysis, include_constant = FALSE, simplify = TRUE) %>%
  mutate(across(estimate:svalue, \(x) signif(x, 3))) %>% 
  filter(!(str_detect(term, "y_mis")))

glance %>% print()
coef %>% print(n = nrow(.)) 

tidy <- tidy(analysis) %>% 
  mutate(ess = esr * niters(analysis) * nchains(analysis))

# chk_true(nrow(tidy %>% filter(ess < 100)) == 0)

sbf_save_table(glance, caption = "Model convergence")
sbf_save_table(coef, caption = "Model coefficients")

precip <- predict(analysis, "precip")

gp <- ggplot(precip) +
  geom_line(aes(x = precip, y = estimate)) +
  geom_line(aes(x = precip, y = lower), linetype = "dotted") +
  geom_line(aes(x = precip, y = upper), linetype = "dotted") +
  xlab(expression("Precipitation"~(m^3/s))) +
  ylab(expression("Discharge"~(m^3/s))) +
  NULL

sbf_open_window()
sbf_print(gp)

# Compare predictions to real values
precipitation <- 
  xnew_data(data, xobs_only(site, week)) %>% 
  select(-precip) %>% 
  left_join(data %>% select(site, week, precip), join_by(site, week)) %>% 
  predict(analysis, new_data = .) %>% 
  arrange(week, site) %>% 
  mutate(type = "prediction") %>% 
  bind_rows(data %>% mutate(type = "observation") %>% rename(estimate = discharge))

gp <- ggplot(precipitation) +
  geom_pointrange(aes(x = week, y = estimate, ymin = lower, ymax = upper, colour = type)) +
  scale_colour_disc_poisson() +
  xlab("Week") +
  facet_grid(rows = vars(site)) +
  ylab(expression("Discharge"~(m^3/s))) +
  NULL

sbf_open_window()
sbf_print(gp)
