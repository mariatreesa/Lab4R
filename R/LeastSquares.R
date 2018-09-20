



linreg <- function(formula = Petal.Length ~ Sepal.Width+Sepal.Length,data = iris){
  if(!(class(formula)=="formula")){
    stop("The first argument should be a formula object")
  }

  X <- model.matrix(formula,data = data)
  dvars <- setdiff(all.vars(formula),colnames(X) )
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
  computedvalues <- list("RegressionCoefficients" = regressioncoeff, "FittedValues" <- fittedvalues, "Residuals" = residuals,
                         "DegreeofFreedon" = degreeoffreedom, "ResidualVarience" = residualvariance,
                         "VarianceOfRegCoeff" = varianceofregcoeff, "tvalues" = tvalues, "pvalues" = pvalues)


  # assigning class to linreg - s3 class
  class(computedvalues) <- "linreg"

  return(computedvalues)

}





