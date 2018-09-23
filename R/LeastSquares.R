#' Implementation of Liner Regression function linreg, an alternative for lm model.
#' Authors: Maria Treesa Sebastian(marse306), Brian Masinde(brima748), Omkar(omkbh878)
#'
#' @param formula as formula
#' @param data as dataframe
#'
#' @return An object of class linreg with all the regression variables.
#'
#' @export linreg as function
#'
#' @examples linreg(formula = Petal.Length ~ Species, data = iris)
#'
#'
install.packages("ggplot2")
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

  # creating object
  computedvalues <- list("Coefficients" = regressioncoeff, "FittedValues" = fittedvalues, "Residuals" = residuals,
                         "DegreeofFreedon" = degreeoffreedom, "ResidualVarience" = residualvariance,
                         "VarianceOfRegCoeff" = varianceofregcoeff, "tvalues" = tvalues, "pvalues" = pvalues, "call" = match.call())

  # assigning class to linreg - s3 class
  class(computedvalues) <- "linreg"

  return(computedvalues)

}



#' @param x as object of linreg
#'
#' @export print.linreg as function
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

# plot function
#' @param x as object of linreg
#'
#' @export plot.linreg as function
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

   plot1 <-  ggplot(data=plot_data,
         aes(x= fitted_values, y = residual_values)) +
   xlab(paste("Fitted Values", "\n\t", "linreg(", formula[2], " ", formula[1], " ", formula[3],")" )) +
   ylab("Residuals") + geom_point(shape=1, size=5) + geom_smooth(method = "loess",
                                              color = "red",
                                              se = FALSE) +ggtitle("Residuals vs Fitted")

 plot2 <- p2 <- ggplot(data = plot_data,
  aes(x=fitted_values, y = sqrt(abs((residual_values - mean(residual_values)) /  as.vector(sqrt(residualvariance)))))) +
   geom_point(shape =1, size=5) +
   geom_smooth(method = "loess",
               color = "red",
               se = FALSE) +
   ggtitle("Scale-Location") +
   ylab(expression(sqrt(abs("Standardized Residuals")))) +
   xlab(paste("Fitted Values", "\n\t", "linreg(", formula[2], " ", formula[1], " ", formula[3],")" ))

 return(grid.arrange(Residual_vs_Fitted = plot1,
             Scale_Location = plot2, ncol =2 ))

}





resid <- function (x, ...) {
  UseMethod("resid", x)
}


# Resid function
#' @param x as object of linreg
#'
#' @export resid.linreg as function
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

# pred function
#' @param x as object of linreg
#'
#' @export pred.linreg as function
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

# coef function
#' @param x as object of linreg
#'
#' @export coef.linreg as function
#'
#' @return Named Vector of Coefficients
#'
#' @examples coef(linreg(formula = Petal.Length ~ Species, data = iris))
#'
coef.linreg <- function(x = mod_object,...) {
  v <- as.data.frame(x[["Coefficients"]])
  cvec <- as.vector(x[["Coefficients"]])
  names(cvec) <- rownames(v)
  cvec
}

#Summary function
summary <- function(x, ...){
  UseMethod("summary", x)
}

# calculate coefficients

# summary function
#' @param x as object of linreg
#'
#' @export summary.linreg as function
#'
#' @return summary of linreg with formula data, coefficient matrix and residual standard error
#'
#' @examples summary(linreg(formula = Petal.Length ~ Species, data = iris))
#'
summary.linreg <- function(x,...) {

  # coefficients
  coef_matrix <- data.frame(
    estimate = round(x[["Coefficients"]], 2),
    std.error = round(sqrt(diag(
      x[["VarianceOfRegCoeff"]]
    )), 2),
    t_value = round(x[["tvalues"]], 2),
    p_value = x[["pvalues"]]
  )
  colnames(coef_matrix) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")



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















