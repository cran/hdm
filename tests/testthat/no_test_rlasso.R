
library(hdm)

# DPG
DPG_rlasso <- function(n, p, px){
  
  X <- matrix(rnorm(n*p), ncol=p)
  beta <- c(rep(2,px), rep(0,p-px))
  intercept <- 5
  y <- intercept + X %*% beta + rnorm(n)
  
  list(X = X, y = y, beta = beta)
}

# DPG + rlasso
DPG_coef_rlasso <- function(n, p, px){
  ret <- DPG_rlasso(n, p, px)
  rlasso(ret[[1]], ret[[2]])
}

# Beispielwerte
set.seed(2)
ret <- DPG_rlasso(200, 100, 10)
X <- ret$X
y <- ret$y
beta <- ret$beta
frame <- as.data.frame(cbind(y, X))
colnames(frame) <- c("y", paste0("x", 1:100))
rm(ret)

######################################################################################

# default 
set.seed(2)
summary(DPG_coef_rlasso(50, 150, 20), all = FALSE)
summary(DPG_coef_rlasso(100, 120, 15), all = FALSE)
summary(lassofit <- DPG_coef_rlasso(200, 100, 10), all = FALSE)
summary(DPG_coef_rlasso(500, 20, 10), all = FALSE)
summary(DPG_coef_rlasso(500, 10, 3), all = FALSE)
summary(DPG_coef_rlasso(5000, 5, 1), all = FALSE)

str(lassofit)

## Gute Schaetzungen der Koeffizientenwerte, fuer p groesser n jedoch nicht mehr !
## Im output ist alles vorhanden. Einige Objekte sind hier trotz Eindimensionalitaet
## Matrizen (lambda, loadings, sigma, residuals)


# intercept TRUE/FALSE
summary(rlasso(X, y, intercept = TRUE), all = FALSE)
summary(rlasso(X, y, intercept = FALSE), all = FALSE)

## Schaetzwerte aendern sich mit intercept = F stark


# post TRUE/FALSE
summary(rlasso(X, y, post = TRUE), all = FALSE)
summary(rlasso(X, y, post = FALSE), all = FALSE)

## Schaetzwerte werden deutlich geringer fuer post = F


# normalize TRUE/FALSE
summary(rlasso(X, y, normalize = TRUE), all = FALSE)
summary(rlasso(X, y, normalize = FALSE), all = FALSE)

## keine Aenderung der Koeffizientenwerte


# keine Variable hat Einfluss
ret2 <- DPG_rlasso(200, 100, 0)
X2 <- ret2$X
y2 <- ret2$y
rm(ret2)
lasso_Ohne_Einfluss <- rlasso(X2, y2)
lasso_Ohne_Einfluss2 <- rlasso(X2, y2, intercept = FALSE)

## hat keine Variable Einfluss, so wird Intercept falsch berechnet

# homoscedastic TRUE/FALSE/"none"
str(rlasso(X, y, penalty = list(homoscedastic = FALSE, X.dependent.lambda = FALSE)))
str(rlasso(X, y, penalty = list(homoscedastic = TRUE, X.dependent.lambda = FALSE)))
str(rlasso(X, y, penalty = list(homoscedastic = "none", X.dependent.lambda = FALSE)))
str(rlasso(X, y, penalty = list(homoscedastic = "none", X.dependent.lambda = FALSE, lambda.start = 100)))

## keine Aenderung der Koeffizientenwerte
## lambdas bei homoscedastic = T fuer alle Koeffizienten gleich 
## fuer "none" lambda.start vorgeben noetig (entspricht dann dem lambda0)
## c und gamma werden auf default gesetzt


# X.design independent/dependent
str(rlasso(X, y, penalty = list(homoscedastic = FALSE, X.dependent.lambda = FALSE)))
str(rlasso(X, y, penalty = list(homoscedastic = FALSE, X.dependent.lambda = TRUE)))
str(rlasso(X, y, penalty = list(homoscedastic = FALSE, X.dependent.lambda = TRUE, numSim = 1000)))

## keine Aenderung der Koeffizientenwerte
## lambda aendert sich auch durch X.design Aenderung oder numSim Aenderung (bei X-dependent)


# formula Methode
all.equal(rlasso(X, y), rlasso(y ~ X))
all.equal(rlasso(X, y), rlasso(y ~., data = frame))
coef(rlasso(X, y))
coef(rlasso(y ~ X))
attributes(residuals(rlasso(X, y)))
attributes(residuals(rlasso(y ~ X)))

## Ergebnisse sind dieselben. Namen stimmen nicht ganz ueberein. Einflussvariablen werden zB bei 
## default V1...Vp und bei formula X1...Xn genannt. Attribute (z.B. bei den Residuen) stimmen
## nicht ganz ueberein


#############################################################################################

# Methoden

# summary und print
print(rlasso(X, y), all = "FALSE")
print(rlasso(X, y), digits = 3)

summary(rlasso(X, y), all = "FALSE")
summary(rlasso(X, y), digits = 3)

# model.matrix
all.equal(model.matrix(rlasso(X, y)), X)
all.equal(model.matrix(rlasso(y ~ X)), X)
all.equal(model.matrix(rlasso(y ~., data = frame)), X)

## bei formula bekommt die model.matrix noch zusaetzliche Attribute

# predict
# newdatas 
newdata <- as.data.frame(matrix(rnorm(20000), ncol = 100))
colnames(newdata) <- paste0("Var", 1:100)
newdata2 <- as.data.frame(matrix(rnorm(30000), ncol = 150))
## bei newdata2 kommen die Variablennamen nun im spaeteren Modell vor (V1,...V100)

# ohne newdata
all.equal(predict(rlasso(X, y)), rlasso(X, y)$intercept.value + X %*% rlasso(X, y)$coef)
all.equal(predict(rlasso(y ~ X)), rlasso(y ~ X)$intercept.value + X %*% rlasso(y ~ X)$coef)
predict(rlasso(y ~ x1 + x2, data = frame))
predict(rlasso(y ~ x7, data = frame))
predict(rlasso(X, y, intercept = FALSE)) 
predict(rlasso(X, rnorm(200)))
predict(rlasso(X, rnorm(200), intercept = FALSE))

# mit newdata
predict(rlasso(X, y), newdata = newdata)
predict(rlasso(y ~ X), newdata = newdata) 
predict(rlasso(y ~ x1 + x2, data = frame), newdata = newdata) # erwartet fehler, da unpassende Variablenzahl
predict(rlasso(y ~ x7, data = frame), newdata = newdata) # ebenso
predict(rlasso(X, y, intercept = FALSE), data = newdata)

#mit  newdata2
predict(rlasso(X, y), newdata = newdata2)
predict(rlasso(y ~ X), newdata = newdata2) # erwartet Fehler, da im Gegensatz zu oben kein passender Name
predict(rlasso(y ~ x1 + x2, data = frame), newdata = newdata2)
predict(rlasso(y ~ x7, data = frame), newdata = newdata2) 
predict(rlasso(X, y, intercept = FALSE), data = newdata2)

## predict verknuepft bei neuem Datensatz entweder ueber Namen 
## oder ueber die Position (nur bei gleicher Variablenzahl)
## scheint zu funktionieren


# lauft nicht durch:
predict(rlasso(frame[, 2:101], y))
l <- list(X = X,y = y)
predict(rlasso(l$X, l$y))
model.matrix(rlasso(l$X, l$y))
