source("header.R")

options(mb.parallel = FALSE)

sbf_set_sub("temperature")

analysis <- sbf_load_object("analysis")
data <- data_set(analysis)

glance <- glance(analysis)
coef <- coef(analysis, include_constant = FALSE, simplify = TRUE) %>%
  mutate(across(estimate:svalue, signif, 3))

glance %>% print()
coef %>% print(n = nrow(.)) 

sbf_save_table(glance, caption = "Model convergence")
sbf_save_table(coef, caption = "Model coefficients")

count <- predict(analysis, new_data = "X")

gp <- ggplot(data = count) +
  aes(x = X, y = estimate) +
  geom_pointrange(aes(ymin = lower, ymax = upper)) +

sbf_open_window(3)
sbf_print(gp)

sbf_save_plot(x_name = "x", report = FALSE, caption = "The predicted relationship between Y and X (with 95% CIs)")
