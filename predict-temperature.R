source("header.R")

options(mb.parallel = FALSE)

sbf_set_sub("temperature")

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

chk_true(nrow(tidy %>% filter(ess < 100)) == 0)

sbf_save_table(glance, caption = "Model convergence")
sbf_save_table(coef, caption = "Model coefficients")

# Predict mean temp
mean_temp <- predict(analysis, "doy")

gp <- ggplot(mean_temp) +
  geom_line(aes(x = doy, y = estimate)) +
  geom_line(aes(x = doy, y = lower), linetype = "dotted") +
  geom_line(aes(x = doy, y = upper), linetype = "dotted") +
  xlab("Day of Year") +
  ylab("Temperature (˚C)") +
  NULL

sbf_open_window(3, 3)
sbf_print(gp)

# # Predict tail-up semivariogram
tu <- predict(analysis, "H", term = "eTU") %>% 
  mutate(type = "Tail-up")

td <- predict(analysis, "H", term = "eTD") %>% 
  mutate(type = "Tail-down")

ed <- predict(analysis, "H", term = "eED") %>% 
  mutate(type = "Euclidean")

covariance <- bind_rows(tu, td, ed) %>% 
  mutate(type = factor(type)) %>% 
  filter(H < 10000)

gp <- ggplot(covariance) +
  geom_line(aes(x = H, y = estimate)) +
  geom_line(aes(x = H, y = lower), linetype = "dotted") +
  geom_line(aes(x = H, y = upper), linetype = "dotted") +
  facet_grid(rows = vars(type)) +
  xlab("Distance (km)") +
  ylab("Covariance") +
  NULL

sbf_open_window(4, 5)
sbf_print(gp)

