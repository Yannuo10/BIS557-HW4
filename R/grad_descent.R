#' @title grad_descent() function
#' @description another way to build a linear model. gradient descent for ordinary least squares
#' @param form a formula;
#' @param data a data frame used for the function;
#' @param contrasts a list of contrasts for factor variables
#' @param itr number of iteration
#' @examples
#' data(iris)
#' fit_linear_model <- grad_descent(Sepal.Length ~ ., iris, contrasts = list(Species = "contr.sum"))
#' @export



grad_descent <- function(form, data, contrasts = NULL, itr = 1e6){
  df <- model.frame(form, data)

  if (is.null(contrasts)) {
    X <- model.matrix(form, df)
  }
  else (X <- model.matrix(form, df, contrasts.arg=contrasts))

  y_name <- as.character(form)[2]
  Y <- matrix(df[, y_name], ncol=1)
  beta <- matrix(rep(1, length(colnames(X))), nrow=length(colnames(X)))
  L <- 0.0001
  for (i in 1:itr){
    pd <- (-2)* t(X) %*% Y + 2 * t(X) %*% X %*% beta
    beta <- beta - L * pd
  }

  ret <- list(coefficients=beta)
  ret
}



