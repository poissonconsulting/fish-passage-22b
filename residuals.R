analysis <- sbf_load_object("analysis")
data <- data_set(analysis)

data$residual <- residuals(analysis)$estimate

gp <- ggplot(data = data, aes(x = residual)) +
  geom_histogram(binwidth = 1/2, color = "white")

sbf_open_window()
sbf_print(gp)

sbf_save_plot(x_name = "res_hist", report = FALSE)

data$fit <- fitted(analysis)$estimate

gp <- ggplot(data = data, aes(x = fit, y = residual)) +
  geom_point(alpha = 1/2) +
  geom_hline(yintercept = 0, linetype = "dotted")

sbf_open_window()
sbf_print(gp)

sbf_save_plot(x_name = "res_fit", report = FALSE)

ppc <-  posterior_predictive_check(analysis)

ppc %>% print()

sbf_save_table(ppc, caption = "Model posterior predictive checks")
