#' @title ridge() function
#' @description another way to build a linear model. ridge regression for ordinary least squares
#' @param form a formula;
#' @param df a data frame used for the function;
#' @param contrasts a list of contrasts for factor variables
#' @param lambda a constant for penalty
#' @examples
#' data(iris)
#' fit_linear_model <- ridge(Sepal.Length ~ ., iris, lambda = 0.1)
#' @export

ridge <- function(form, df, contrasts = NULL, lambda = 0){
  df_no_na <- model.frame(form,df)
  X <- model.matrix(form, df_no_na, contrasts)
  yname <- as.character(form)[2]
  y <- matrix(df_no_na[,yname],ncol = 1)

  svd_x <- svd(X)
  D <- diag(svd_x$d/(svd_x$d^2 + lambda))
  beta <- svd_x$v %*% D %*% t(svd_x$u) %*% y
  ret <- list(coefficients = beta)
  class(ret) <- "my_lm_ridge"
  ret
}
