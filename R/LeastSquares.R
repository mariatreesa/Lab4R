#' Implementation of Liner Regression function linreg, an alternative for lm model.
#' Authors: Maria Treesa Sebastian(marse306), Brian Masinde(brima748), Omkar(omkbh878)
#'
#' @name linreg
#' @param formula as formula object
#' @param data as dataframe
#'
#' @return computedvalues  an object linreg as an s3 class.
#'
#' @export linreg
#' @examples linreg(formula = Petal.Length ~ Species, data = iris)
#' @importFrom ggplot2 ggplot aes xlab ylab ggtitle stat_summary geom_point


# the linreg function

linreg <- function(formula, data ){
  if(!(class(formula)=="formula")){
    stop("The first argument should be a formula object")
  }
  X <- model.matrix(formula,data )
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

  tvalues <- regressioncoeff / sqrt(diag((varianceofregcoeff))) # tvalues


  pvalues <- 2 * pt(abs(tvalues), degreeoffreedom, lower.tail = FALSE)

  # calculate significant codes for p values
  signi <- symnum(pvalues, corr = FALSE, na = FALSE,
                  cutpoints = c(0,0.001, 0.01, 0.05, 0.1, 1),
                  symbols = c("***", "**", "*", ".", " "))


  # creating object
  computedvalues <- list("Coefficients" = regressioncoeff, "FittedValues" = fittedvalues, "Residuals" = residuals,
                         "DegreeofFreedon" = degreeoffreedom, "ResidualVarience" = residualvariance,
                         "VarianceOfRegCoeff" = varianceofregcoeff, "tvalues" = tvalues, "pvalues" = pvalues,"signi" = signi, "call" = match.call())

  # assigning class to linreg - s3 class
  class(computedvalues) <- "linreg"

  return(computedvalues)

}


#' Implementation of Print function using linreg , an alternative for lm.print.
#' @name print.linreg
#'
#' @param x as object of linreg
#'
#' @param ...  optional parameter
#'
#' @return Nothing
#' @export
#'
print.linreg <- function(x,...) {

  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"),
      "\n\n", sep = "")
  regcoeff <- t(x[["Coefficients"]])
  cat("Coefficients:\n")
  print(format(regcoeff[1,]), quote = FALSE)
  cat("\n")

}

#' Generic plot function
#'
#' @name plot
#'
#' @param x as s3 object
#'
#' @param ...  optional parameter
#'
#' @return nothing
#' @export
#'
plot <- function (x,...) {
  UseMethod("plot", x)
}
#' Implementation of plot function using linreg , an alternative for lm.plot.
#'
#' @name plot.linreg
#'
#' @param x as object of linreg
#'
#' @param ...  optional parameter
#'
#' @return Prints the two plots to replicate plot.lm
#'
#' @export
#'
plot.linreg <- function(x,...){
  op <- par(ask=TRUE)
  for (i in 1:2)
  {
    residual_values <- x[["Residuals"]]
    fitted_values <- x[["FittedValues"]]
    plot_data <- data.frame(residual_values,fitted_values)
    residualvariance <- x[["ResidualVarience"]]
    formula <- x[["call"]][["formula"]]


    if(i==1){

      print(ggplot(data=plot_data,
                   aes(x= fitted_values, y = residual_values)) +
              xlab(paste("Fitted Values", "\n\t", "linreg(", formula[2], " ", formula[1], " ", formula[3],")" )) +
              ylab("Residuals") + geom_point(shape=1, size=5) +ggtitle("Residuals vs Fitted")+
              stat_summary(fun.y = median, color = "red", geom = "line", size=1))

    }
    if(i==2){

      print(ggplot(data = plot_data,
                   aes(x=fitted_values, y = sqrt(abs((residual_values - mean(residual_values)) /  as.vector(sqrt(residualvariance)))))) +
              geom_point(shape =1, size=5) +
              ggtitle("Scale-Location") +
              ylab(expression(sqrt(abs("Standardized Residuals")))) +
              xlab(paste("Fitted Values", "\n\t", "linreg(", formula[2], " ", formula[1], " ", formula[3],")" ))+
              stat_summary(fun.y = mean, color = "red", geom = "line", size=1))

    }
    on.exit(par(op))
    i<-i+1
  }
}


#' Generic resid function
#'
#' @name resid
#'
#' @param x as s3 object
#'
#' @param ...  optional parameter
#'
#' @return nothing
#' @export
#'
resid <- function (x,...) {
  UseMethod("resid", x)
}

#' Implementation of resid function using linreg , an alternative for lm.resid.
#'
#' @name resid.linreg
#'
#' @param x as object of linreg
#' @param ...  optional parameter
#'
#' @return Vector of residuals
#'
#'
#' @export
#'
resid.linreg <- function(x,...) {
  as.vector(x[["Residuals"]])

}


#' Generic pred function
#'
#' @name pred
#'
#' @param x as s3 object
#'
#' @param ...  optional parameter
#'
#' @return nothing
#' @export
#'
pred <- function (x, ...) {
  UseMethod("pred", x)
}

#' Implementation of pred function using linreg , an alternative for lm.pred.
#'
#' @name pred.linreg
#'
#' @param x as object of linreg
#'
#' @param ...  optional parameter
#'
#' @return Vector of Fitted values
#' @export
#'
pred.linreg <- function(x,...) {
  as.vector(x[["FittedValues"]])
}


#' Generic coef function
#'
#' @name coef
#'
#' @param x as s3 object
#'
#' @param ...  optional parameter
#'
#' @return nothing
#' @export
#'
coef <- function (x,...) {
  UseMethod("coef", x)
}
#' Implementation of coef function using linreg , an alternative for lm.coef.
#'
#' @name coef.linreg
#'
#' @param x  object of linreg
#' @param ...  optional parameter
#'
#'
#' @return Named Vector of Coefficients
#' @export
#'
coef.linreg <- function(x,...) {
  v <- as.data.frame(x[["Coefficients"]])
  cvec <- as.vector(x[["Coefficients"]])
  names(cvec) <- rownames(v)
  cvec
}


#' Generic summary function
#'
#' @name summary
#'
#' @param x as object
#'
#' @param ...  optional parameter
#'
#' @return nothing
#' @export
#'
summary <- function (x,...) {
  UseMethod("summary", x)
}
#' Implementation of summary function using linreg , an alternative for lm.summary.
#'
#' @name summary.linreg
#'
#' @param x as object of linreg
#'
#' @param ...  optional parameter
#'
#' @return summary of linreg with formula, data, coefficient matrix and residual standard error
#' @export
#'
summary.linreg <- function(x,...) {

  coef_matrix <- data.frame(
    estimate = round(x[["Coefficients"]], 5),
    std.error = round(sqrt(diag(
      x[["VarianceOfRegCoeff"]]
    )), 5),
    t_value = round(x[["tvalues"]], 2),
    p_value = ifelse((x[["pvalues"]]<2e-16),"<2e-16",x[["pvalues"]]),
    signi <- as.vector(x[["signi"]])
  )
  colnames(coef_matrix) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)","")



  # formula
  formula = paste(x[["call"]])
  cat(sep = "\n")
  cat("Call:")
  cat(sep = "\n")
  cat(paste(formula[1], "( formula = ", formula[2], ", data = ", formula[3], ")", sep = "" ))
  cat(sep = "\n")


  #-----coeff matrix
  cat(sep = "\n")
  cat(sep = "\n")
  cat("Coefficients:")
  cat(sep = "\n")
  print(coef_matrix)
  cat("---")


  #---- significant codes

  cat(sep = "\n")
  cat("Signif . codes ", attributes(x$signi)$"legend")
  cat(sep = "\n")

  #--- degree of freedon

  cat(sep = "\n")
  cat("Residual standard error:", round(sqrt(x[["ResidualVarience"]]), 2), "on", x[["DegreeofFreedon"]], "degrees of freedom")
  cat(sep = "\n")

}














