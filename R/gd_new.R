#' @title grad_descent() function
#' @description another way to build a linear model. gradient descent for ordinary least squares
#' @param form a formula;
#' @param data a data frame used for the function;
#' @param contrasts a list of contrasts for factor variables
#' @param itr number of iteration
#' @param L learning rate for gradient descent
#' @param tolerance allowed tolerance for two residuals
#' @examples
#' data(iris)
#' fit_linear_model <- grad_descent(Sepal.Length ~ ., iris, contrasts = list(Species = "contr.sum"))
#' @export



gd_new <- function(form, data, contrasts=NULL, itr=1e5, L=0.0001, tolerance=1e-10){
  set.seed(123)
  rows <- runif(nrow(data))>0.25
  df1 <- data[rows, ]
  df2 <- data[!rows, ]

  df11 <- model.frame(form, df1)
  if(is.null(contrasts)){
    X_train <- model.matrix(form, df11)}
  else(X_train <- model.matrix(form, df11, contrasts.arg=contrasts))

  y_name <- as.character(form)[2]
  Y_train <- matrix(df11[, y_name], ncol=1)


  df22 <- model.frame(form, df2)
  if(is.null(contrasts)){
    X_test <- model.matrix(form, df22)}
  else(X_test <- model.matrix(form, df22, contrasts.arg=contrasts))

  y_name <- as.character(form)[2]
  Y_test <- matrix(df22[, y_name], ncol=1)



  beta <- matrix(rep(1, length(colnames(X_train))), nrow=length(colnames(X_train)))
  res <- function(beta,X,Y) {
    drop(t(Y)%*%Y+t(beta)%*%t(X)%*%X%*%beta-2*t(Y)%*%X%*%beta)
  }

  i.itr <- 0
  dif <- 1
  while ((i.itr<itr) & (dif>tolerance)){
    pd<-(-2)*t(X_train)%*%Y_train+2*t(X_train)%*%X_train%*%beta
    res1<-res(beta, X_train, Y_train)
    beta<-beta-L*pd
    res2<-res(beta, X_test, Y_test)
    dif<-abs(res1-res2)
    i.itr<-i.itr+1
  }

  ret <- list(coefficients = beta)
  ret
}




























