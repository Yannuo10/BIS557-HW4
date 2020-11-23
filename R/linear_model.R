#' @title linear_model() function
#' @description one way to build a linear model
#' @param form a formula;
#' @param dataframe a data frame;
#' @param contrasts a list of contrasts for factor variables
#' @examples
#' data(iris)
#' fit_linear_model <- linear_model(Sepal.Length ~ ., iris, contrasts = list(Species = "contr.sum"))
#' @export


linear_model<-function(form, dataframe, contrasts=NULL){
  df<-model.frame(form, dataframe)

  if(is.null(contrasts)){
   X<-model.matrix(form, df)}
  else(X<-model.matrix(form, df, contrasts.arg=contrasts))

  y<-as.character(form)[2]
  Y<-matrix(df[,y],ncol=1)
  beta<-qr.solve(qr(X), Y)
  beta_names<-rownames(beta)
  beta<-as.numeric(beta)
  beta[beta==0]<-NA

  names(beta)<-beta_names
  ret<-list(coefficients=beta)
  class(ret) <- "my_lm"
  ret
}
















