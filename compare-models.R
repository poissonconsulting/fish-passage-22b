source("header.R")

# Calculate R^2 ----
calculate_Rsq <- function(obs, pred) {
  chk_equal(length(obs), length(pred))
  pred <- pred[!is.na(obs)]
  obs <- obs[!is.na(obs)]
  ssr <- sum((obs - pred)^2)
  sst <- sum((obs - mean(obs))^2)
  Rsq <- 1 - (ssr / sst)
  return(Rsq)
}

## logistic ----
sbf_set_sub("temperature")
analysis <- sbf_load_object("analysis")
data <- data_set(analysis)
obs <- data$water_temp
pred <- fitted(analysis)$estimate
Rsq_logistic <- calculate_Rsq(obs, pred)

## air2water ----
sbf_set_sub("temperature-air2stream")
analysis <- sbf_load_object("analysis")
data <- data_set(analysis)
obs <- data$water_temp
pred <- fitted(analysis)$estimate
Rsq_a2w <- calculate_Rsq(obs, pred)

Rsq_table <- 
  tibble(model = c("air2stream", "logistic"),
                    "$R^2$"= c(Rsq_a2w, Rsq_logistic))
Rsq_table

sbf_save_table(
  Rsq_table, 
  report = FALSE, 
  sub = "compare-temperature", 
  caption = "The coefficient of determination, $R^2$, is the proportion of variance explained by the model. $R^2$ values are between 0 and 1; values closer to 1 explain more variation.")

# WAIC
# Calculate WAIC/PSIS ----
## Logistic ----
sbf_set_sub("temperature")
sbf_load_objects()
analysis <- sbf_load_object("analysis")
data <- data_set(analysis)

log_lik <- mcmc_derive_data(
  analysis, 
  new_expr = "for (i in 1:nObs) {eLogLik[i] <- log_lik[i]}", 
  new_data = xnew_data(data, week), 
  term = "eLogLik"
)

arr <- aperm(unclass(as.mcarray(log_lik$mcmc)), perm = c(2, 3, 1), resize = TRUE)
r_eff <- relative_eff(exp(arr))
waic_logistic <- waic(arr)
psis_logistic <- psis(-arr, r_eff = r_eff)
loo_logistic <- loo(arr, r_eff = r_eff, save_psis = TRUE, cores = getDoParWorkers())

## air2stream -----
sbf_set_sub("temperature-air2stream")
analysis <- sbf_load_object("analysis")
data <- data_set(analysis)

log_lik <- mcmc_derive_data(
  analysis, 
  new_expr = "for (i in 1:nObs) {eLogLik[i] <- log_lik[i]}", 
  new_data = xnew_data(data, week), 
  term = "eLogLik"
)

arr <- aperm(unclass(as.mcarray(log_lik$mcmc)), perm = c(2, 3, 1), resize = TRUE)
r_eff <- relative_eff(exp(arr))
waic_air2stream <- waic(arr)
psis_air2stream <- psis(-arr, r_eff = r_eff)
loo_air2stream <- loo(arr, r_eff = r_eff, save_psis = TRUE, cores = getDoParWorkers())


# pareto k values
k_logistic <- tibble(k = pareto_k_values(psis_logistic),
                     id = 1:length(k),
                     models = rep("logistic", length(k)))
k_air2stream <- tibble(k = pareto_k_values(psis_air2stream), 
                id = 1:length(k),
                models = rep("air2water", length(k)))
k_values <- bind_rows(k_logistic, k_air2stream) 

gp <- ggplot(k_values, aes(x = id, y = k)) +
  facet_wrap(vars(models)) +
  geom_point(size = 0.1) +
  geom_hline(yintercept = 0.7, colour = pois_cols(colours = "red"),
             linetype = "dashed") +
  ylab("Pareto k-value") +
  xlab("Data point")

sbf_open_window(width = 8, height = 4)
sbf_print(gp)

sbf_save_plot(x_name = "psis", report = FALSE,
              sub = "compare-temperature", caption = "Pareto k-values of logistic and air2water temperature models. The dashed red line indicates where k = 0.7.")

k_table_logistic <- pareto_k_table(psis_logistic) |>
  as_tibble() |> 
  mutate(k_range = c("(-Inf, 0.5]", "(0.5, 0.7]", "(0.7, 1]", "(1, Inf)"),
         count = Count,
         percent = Proportion * 100,
         n_eff = `Min. n_eff`) |>
  select(k_range, count, percent, n_eff)

sbf_save_table(k_table_logistic, report = FALSE, caption = "PSIS k-value diagnostics for logistic temperature model", 
               sub = "compare-temperature")

k_table_air2stream <- pareto_k_table(psis_air2stream) |>
  as_tibble() |> 
  mutate(k_range = c("(-Inf, 0.5]", "(0.5, 0.7]", "(0.7, 1]", "(1, Inf)"),
         count = Count,
         percent = Proportion * 100,
         n_eff = `Min. n_eff`) |>
  select(k_range, count, percent, n_eff)

sbf_save_table(k_table_air2stream, report = FALSE, caption = "PSIS k-value diagnostics for air2water temperature model", sub = "temperature-air2stream")

# IC summaries ------------------------------------------------------------
ic_calcs <- list(logistic = list(waic = waic_logistic,
                                 loo = loo_logistic),
                 a2w = list(waic = waic_air2stream,
                            loo = loo_air2stream))

waic_table <- ic_table(ic_calcs, ic = "waic")
psis_table <- ic_table(ic_calcs, ic = "psis")

psis_table$model[psis_table$model == "a2w"] <- "air2stream"
waic_table$model[waic_table$model == "a2w"] <- "air2stream"

psis_table
waic_table

sbf_save_table(waic_table, report = FALSE, sub = "compare-temperature", caption = "WAIC diagnostics")

sbf_save_table(psis_table, report = FALSE, sub = "compare-temperature", caption = "Model comparison using Pareto Smoothed Importance-Sampling Leave-One-Out Cross-Validation (PSIS) criterion.
               'ic' is the information criterion value (IC) on the deviance scale; 'se' is the standard error of the IC; 'npars' is the number of effective parameters,
               'delta ic' is the difference between the model's IC and the minimum IC; 'delta se' is the standard error of the difference in IC; 
               'weight' summarizes the relative support for each model; and 'k outliers' is the proportion of data points with Pareto $\\hat{k}$ values exceeding 0.7.")
