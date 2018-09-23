## ----imp1, message=FALSE, warning=FALSE, paged.print=FALSE---------------
data(faithful)

model_obj <- linreg(eruptions ~ waiting, data= faithful)

print(model_obj)

## ----sum, message=FALSE, warning=FALSE, paged.print=FALSE----------------
summary(model_obj)

## ----imp2, eval=FALSE----------------------------------------------------
#  data(iris)
#  
#  model2<-linreg_QR(Petal.Length ~ Species, data = iris)
#  
#  print(model2)

