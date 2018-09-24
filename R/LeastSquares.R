#' Implementation of Liner Regression function linreg, an alternative for lm model.
#' Authors: Maria Treesa Sebastian(marse306), Brian Masinde(brima748), Omkar(omkbh878)
#'
#' @name linreg
#' @param formula as formula
#' @param data as dataframe
#'
#' @return computedvalues  an object of type s3 class.
#'
#' @export linreg
#' @examples linreg(formula = Petal.Length ~ Species, data = iris)
#'
#'

library("ggplot2")
library("gridExtra")

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

print <- function (x, ...) {
  UseMethod("print", x)
}
#' Implementation of Print function using linreg , an alternative for lm.print.
#' @name print.linreg
#'
#' @param x as object of linreg
#'
#' @param ...  optional parameter
#'
#' @return Nothing
#'
#' @examples print(linreg(formula = Petal.Length ~ Species, data = iris))
#'
print.linreg <- function(x,...) {

  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"),
      "\n\n", sep = "")
  regco <- t(x[["Coefficients"]])
    cat("Coefficients:\n")
    print(format(regco[1,]), quote = FALSE)
  cat("\n")

}

plot <- function (x, ...) {
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
#' @return Two plots  Residual vs Fitted   and  Scale-Location
#'
#' @examples plot(linreg(formula = Petal.Length ~ Species, data = iris))
#'
plot.linreg <- function(x,...){


  plot_data <- data.frame(x[["Residuals"]],x[["FittedValues"]])
  residual_values <- x[["Residuals"]]
  fitted_values <- x[["FittedValues"]]
  residualvariance <- x[["ResidualVarience"]]
  formula <- x[["call"]][["formula"]]

   p1 <-  ggplot(data=plot_data,
         aes(x= fitted_values, y = residual_values)) +
   xlab(paste("Fitted Values", "\n\t", "linreg(", formula[2], " ", formula[1], " ", formula[3],")" )) +
   ylab("Residuals") + geom_point(shape=1, size=5) +ggtitle("Residuals vs Fitted")

   plot1 <- p1+ stat_summary(fun.y = median, color = "red", geom = "line", size=1)

  p2 <- ggplot(data = plot_data,
  aes(x=fitted_values, y = sqrt(abs((residual_values - mean(residual_values)) /  as.vector(sqrt(residualvariance)))))) +
   geom_point(shape =1, size=5) +
   ggtitle("Scale-Location") +
   ylab(expression(sqrt(abs("Standardized Residuals")))) +
   xlab(paste("Fitted Values", "\n\t", "linreg(", formula[2], " ", formula[1], " ", formula[3],")" ))

  plot2 <- p2+ stat_summary(fun.y = mean, color = "red", geom = "line", size=1)


 return(grid.arrange(Residual_vs_Fitted = plot1,
             Scale_Location = plot2, ncol =2 ))

}



resid <- function (x, ...) {
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
#' @examples resid(linreg(formula = Petal.Length ~ Species, data = iris))
#'
resid.linreg <- function(x,...) {
  as.vector(x[["Residuals"]])

}

#pred function
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
#'
#' @return Vector of Fitted values
#'
#' @examples pred(linreg(formula = Petal.Length ~ Species, data = iris))
#'
pred.linreg <- function(x,...) {
  as.vector(x[["FittedValues"]])
}


coef <- function(x, ...){
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
#'
#' @examples coef(linreg(formula = Petal.Length ~ Species, data = iris))
#'
coef.linreg <- function(x,...) {
  v <- as.data.frame(x[["Coefficients"]])
  cvec <- as.vector(x[["Coefficients"]])
  names(cvec) <- rownames(v)
  cvec
}

summary <- function(x, ...){
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
#' @return summary of linreg with formula data, coefficient matrix and residual standard error
#'
#' @examples summary(linreg(formula = Petal.Length ~ Species, data = iris))
#'
summary.linreg <- function(x,...) {

  coef_matrix <- data.frame(
    estimate = round(x[["Coefficients"]], 2),
    std.error = round(sqrt(diag(
      x[["VarianceOfRegCoeff"]]
    )), 2),
    t_value = round(x[["tvalues"]], 2),
    p_value = x[["pvalues"]],
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

  #--- degree of freedon

  cat(sep = "\n")
  cat("Residual standard error:", round(sqrt(x[["ResidualVarience"]]), 2), "on", x[["DegreeofFreedon"]], "degrees of freedom")
  cat(sep = "\n")

}














