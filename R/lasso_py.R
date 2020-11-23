#' @title lasso() function
#' @description to build a lasso regression model using python code
#' @param form a formula;
#' @param df a data frame used for the function;
#' @param contrasts a list of contrasts for factor variables
#' @param lambda a constant for penalty
#' @examples
#' data(iris)
#' library(reticulate)
#' library(casl)
#' fit_model <- lasso(Sepal.Length ~ ., iris, contrasts = list(Species = "contr.sum"), lambda = 0.1)
#' @export


lasso <- function(form, df, contrasts = NULL, lambda){
  np <- import("numpy", as = "np", convert = FALSE)
  df_no_na <- model.frame(form, df)
  if(is.null(contrasts)){
    X<-model.matrix(form, df)}
  else(X<-model.matrix(form, df, contrasts.arg=contrasts))
  yname <- as.character(form)[2]
  y <- matrix(df_no_na[, yname], ncol = 1)
  n <- nrow(X)

  X <- r_to_py(X)
  y <- r_to_py(y)
  #convert X into an orthogonal matrix
  X_orth <- np$linalg$qr(X)[0]
  Xty <- (X_orth$T)$dot(y)
  #calculate the second part of the soft-threshold function
  max <- np$maximum(np$subtract(np$abs(Xty), np$multiply(lambda, n)), 0)
  #estimate betas using the l1-penalized estimator
  beta <- py_to_r(np$multiply(np$sign(Xty), max))
  ret <- list(coefficients = beta)
  class(ret) <- "my_lasso"
  ret
}
