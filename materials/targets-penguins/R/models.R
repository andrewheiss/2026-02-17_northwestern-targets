make_model1_freq <- function(df) {
  x <- lm(body_mass ~ bill_len, data = df)
  return(x)
}

make_model2_freq <- function(df) {
  x <- lm(body_mass ~ bill_len + species, data = df)
  return(x)
}

make_model_freq <- function(df, formula) {
  x <- lm(formula, data = df)
  return(x)
}

make_model1_bayes <- function(df) {
  x <- brm(
    bf(body_mass ~ bill_len), 
    data = df,
    chains = 4, iter = 2000, seed = 1234
  )
  return(x)
}

make_model2_bayes <- function(df) {
  model1 <- brm(
    bf(body_mass ~ bill_len + species), 
    data = df,
    chains = 4, iter = 2000, seed = 1234
  )
}
