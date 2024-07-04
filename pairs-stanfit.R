source("header.R")

# Nechako
analysis <- readRDS("~/Analyses/fish-passage-22/output/objects/temperature/analysis.rds")

fit <- analysis$stanfit

sbf_open_window(16, 10)

pairs(fit, pars = c("alpha_td", "sigma_td", "alpha_tu", "sigma_tu", "alpha_ed", "sigma_ed", "bIntercept", "bDoy", "bDoy2", "var_nug", "lp__"))


# SSN bayes example
analysis <- readRDS("~/Analyses/fish-passage-22/output/objects/ssnbayes-example/analysis.rds")

fit <- analysis$stanfit

sbf_open_window(10, 8)

pairs(fit, pars = c("alpha_tu", "alpha_td", "alpha_ed", "sigma_tu", "sigma_td", "sigma_ed", "beta", "var_nug", "lp__"))
