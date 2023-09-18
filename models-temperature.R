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

model <- model("model{
  bY ~ dnorm(0, 2^-2)
  bX ~ dnorm(0, 2^-2)
  sY ~ dnorm(0, 2^-2) T(0,)

  for (i in 1:nObs) {
    eY[i] <- bY + bX * X[i]
    Y[i] ~ dnorm(eY[i], sY^-2)
  }
}",
new_expr = "
for(i in 1:nObs) {
    prediction[i] <- bY + bX * X[i]
    fit[i] <- prediction[i]
    residual[i] <- res_norm(Y[i], fit[i], sY)
}"
)

sbf_save_block(template(model), "template", caption = "Model description.")
