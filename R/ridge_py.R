#' @title ridge() function
#' @description to build a ridge regression model using python code
#' @param form a formula;
#' @param df a data frame used for the function;
#' @param contrasts a list of contrasts for factor variables
#' @param lambda a constant for penalty
#' @examples
#' data(iris)
#' library(reticulate)
#' fit_linear_model <- ridge_py(Sepal.Length ~ .- Species, iris, lambda = 0.1)
#' @export

ridge_py <- function(form, df, contrasts = NULL, lambda = 0){
  np <- import("numpy", as = "np", convert = FALSE)
  df_no_na <- model.frame(form, df)
  if(is.null(contrasts)){
    X<-model.matrix(form, df)}
  else(X<-model.matrix(form, df, contrasts.arg=contrasts))
  yname <- as.character(form)[2]
  y <- matrix(df_no_na[, yname], ncol = 1)


  svd_x <- r_to_py(np$linalg$svd(X, full_matrices = FALSE))
  D <- np$diag(np$divide(svd_x[[1]], np$add(np$power(svd_x[[1]], 2), lambda)))

  beta <- py_to_r((svd_x[[2]]$T)$dot(D)$dot((svd_x[[0]])$T)$dot(y))
  ret <- list(coefficients = beta)
  class(ret) <- "my_lm_ridge"
  ret
}

