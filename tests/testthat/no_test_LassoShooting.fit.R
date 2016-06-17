
library(hdm)
library(glmnet)

DPG_lassoShooting <- function(n, p, px, lambda0 = 110, min = 0.85, max = 1.15){
  
  X <- matrix(rnorm(n*p), ncol=p)
  beta <- c(rep(2,px), rep(0,p-px))
  y <- X %*% beta + rnorm(n)
  loadings <- runif(p, min = min, max = max)
  lambda <- lambda0 * loadings
  
  list(X = X, y = y, beta = beta, lambda = lambda, lambda0 = lambda0, loadings = loadings)
}

# Vergleich mit glmnet
# Funktion mit Differenzenvektor als Rueckgabewert
compare_with_glmnet <- function(n, p, px, lambda0 = 110, min = 0.85, max = 1.15){
  
  data <- DPG_lassoShooting(n, p, px, lambda0, min, max)
  
  fit_Shooting <- LassoShooting.fit(data$X, data$y, data$lambda)
  
  fit_glmnet <- glmnet(data$X, data$y, lambda = data$lambda0 / (2*n), penalty.factor = data$loadings)
  
  fit_Shooting$coefficients - as.vector(coef(fit_glmnet))[-1]
}
set.seed(2)
compare_with_glmnet(100, 120, 15)
compare_with_glmnet(200, 100, 10)
compare_with_glmnet(500, 20, 10)
compare_with_glmnet(500, 10, 3)

# sehr aehnliche Werte werden geschaetzt. Fuer n=200, p=100 Unterschiede bis zu 0,04.
# Fuer n=500,p=20 Unterschiede nur noch bei der dritten Nachkommastelle
