install.packages("ggplot2")
library("ggplot2")



linreg <- function(formula, data ){
  if(!(class(formula)=="formula")){
    stop("The first argument should be a formula object")
  }
  X <- model.matrix(formula,data = data)
  dvars <- all.vars(formula)[1]
  y <- as.matrix(data[,dvars])


  # regression coefficient calculated using :  inverse(traspose(X)* X) *  trasnpose(X)*y
  regressioncoeff <- solve(t(X)%*%X) %*% t(X)%*%y   # solve function do inverse , t(X) - transpose of X

  fittedvalues <- X %*% regressioncoeff  # calculating fitted values

  residuals <- y - fittedvalues   # calulating resuduals
  degreeoffreedom <- nrow(X) - ncol(X) # degree of freedom


  residualvariance <- (t(residuals) %*% residuals) * (1/degreeoffreedom) # residual varience


  #  In varience of regression coefficient matrix, the main diagonal gives varience and off diagonal
  #  gives covarience .
  varianceofregcoeff <- as.vector(residualvariance) * solve(t(X)%*%X) # varience of regression coefficient

  # To calculate tvalues, we need to divide the regression coefficient matrix by the varience of regression
  # coefficient. So we took only the diaginal elements from varianceofregcoeff matrix as those are the variance

  tvalues <- as.vector(regressioncoeff) / sqrt(diag((varianceofregcoeff))) # tvalues


  pvalues <- 2 * pt(abs(tvalues), degreeoffreedom)

  # creating object
  computedvalues <- list("Coefficients" = regressioncoeff, "FittedValues" = fittedvalues, "Residuals" = residuals,
                         "DegreeofFreedon" = degreeoffreedom, "ResidualVarience" = residualvariance,
                         "VarianceOfRegCoeff" = varianceofregcoeff, "tvalues" = tvalues, "pvalues" = pvalues, "call" = match.call())

  # assigning class to linreg - s3 class
  class(computedvalues) <- "linreg"

  return(computedvalues)

}



# print function
print.linreg <- function(x,...) {

  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"),
      "\n\n", sep = "")
  regco <- t(x[["Coefficients"]])
    cat("Coefficients:\n")
    print(format(regco[1,]), quote = FALSE)
  cat("\n")

}

# plot function


plot.linreg <- function(x,...){

  ggplot(data=data.frame(x[["FittedValues"]], y=x[["Residuals"]]), aes(x=x[["FittedValues"]], y=x[["Residuals"]])) + geom_point(shape=1, size=5) +
    geom_abline(mapping = aes(x=x[["FittedValues"]]), intercept = 0 ,slope = 3,data = NULL,na.rm = FALSE,
                show.legend = NA)
}





resid.linreg <- function(x,...) {
  as.vector(x[["Residuals"]])

}


pred.linreg <- function(x,...) {
  as.vector(x[["FittedValues"]])
}



mod_object<- linreg(Petal.Length~Species, data = iris)
plot(mod_object)















