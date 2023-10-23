description <- c(
  "`bY`" = "Intercept for `eY`",
  "`bX`" = "Effect of `X` on `bY`",
  "`sY`" = "SD of residual variation in `Y`",
  "`eY[i]`" = "Expected value of `y[i]`",
  "`Y[i]`" = "The `i`^th^ Y value"
)

description <- tibble(
  Parameter = names(description),
  Description = description
)

description %<>% arrange(Parameter)

sbf_save_table(description, caption = "Parameter descriptions.")

model <- model(
  read_file("temperature.stan"),
  new_expr = "
    for(i in 1:nObs) {
      
    }",
  new_expr_vec = TRUE,
  select_data = list(
    
  )
)

sbf_save_block(template(model), "template", caption = "Model description.")
