set.seed(1)
n = 100 #sample size
p = 200 # number of variables
s = 3 # nubmer of non-zero variables
X = matrix(rnorm(n*p), ncol=p)
colnames(X) = paste("V", 1:p, sep="")
beta = c(rep(3,s), rep(0,p-s))
y = 1 + X%*%beta + rnorm(n)

lasso.reg = rlasso(y~X, post=TRUE, intercept=FALSE)
lasso.reg = rlasso(X, y, post=TRUE, intercept=FALSE)
lasso.reg = rlasso(y~X, post=FALSE, intercept=TRUE)
lasso.reg = rlasso(y~X, post=TRUE, intercept=TRUE, penalty=list(lambda=0.1))

pred1 <- predict(lasso.reg)
pred2 <- predict(lasso.reg, newdata=X)
Xnew = matrix(rnorm(n/2*p), ncol=p)
pred1 <- predict(lasso.reg, newdata=Xnew)

data <- as.data.frame(cbind(y,X))
colnames(data) <- c("y", paste("V", 1:p, sep=""))
lasso.reg = rlasso(y~ V1+ V2 + V3 + V4 , post=TRUE, intercept=TRUE, data=data)
pred3 <- predict(lasso.reg)
pred4 <- predict(lasso.reg, newdata=data)

lasso.reg = rlasso(y~ V1+ V2 + V3 + V4 , post=TRUE, intercept=TRUE, data=data)
pred5 <- predict(lasso.reg)
pred6 <- predict(lasso.reg, newdata=data)

rhs <- paste("V", 1:100, sep="", collapse="+")
form <- as.formula(paste("y", "~", rhs))
lasso <- rlasso(form, data=data)
pred7 <- predict(lasso.reg)


#### Test significance test:

head(lasso.reg$dev)
lasso.reg = rlasso(y~X, post=TRUE, intercept=TRUE, model=FALSE)
summary(lasso.reg, all=FALSE)
lasso.reg = rlasso(y~X, post=TRUE, intercept=TRUE, model=TRUE)
summary(lasso.reg, all=FALSE)

lasso.reg = rlasso(y~X, post=TRUE, intercept=FALSE, model=TRUE)
summary(lasso.reg, all=FALSE)

set.seed(123456)
R <- 500
pvalues <- rep(NA, R)
n = 100 #sample size
p = 20 # number of variables
s = 0 # nubmer of non-zero variables
beta = c(rep(3,s), rep(0,p-s))
for (i in 1:R) {
X = matrix(rnorm(n*p), ncol=p)
y = 1 + X%*%beta + rnorm(n)
lasso.reg = rlasso(y~X, post=TRUE, intercept=TRUE, model=TRUE)
pvalues[i] <- summary(lasso.reg)$pval
}



############################################################
set.seed(1234)
n = 100
n1 = 50
p = 20
s = 10
beta <- c(rep(1,s), rep(0,p-s))
X <- matrix(rnorm(n*p), ncol=p)
y <- X%*%beta  + rnorm(n)

Xo <- matrix(rnorm(n1*p), ncol=p)
yo <- Xo%*%beta  + rnorm(n1)
fo <- Xo%*%beta

# lasso.reg1a <- rlasso(y~X, post=FALSE, intercept=TRUE) 
# Results1a <- mean((fo-Xo%*%coef(lasso.reg1a))^2)
# lasso.reg1a$coefficients[1:s]

# Lasso with default penalty choice
lasso.reg1b <- rlasso(y~X, post=FALSE, intercept=FALSE)
Results1b <- mean((fo-Xo%*%coef(lasso.reg1b))^2)
lasso.reg1b$coefficients[1:s]
print("MSE", Results1b)


# Lasso with half of the penalty
lasso.reg1c <- rlasso(y~X, post=FALSE, intercept=FALSE, penalty=list(homoscedastic = "none", 
                                                                   lambda.start=2.2*sqrt(n)*qnorm(1-0.1/(2*p))/2)) 
Results1c <- mean((fo-Xo%*%coef(lasso.reg1c))^2)
lasso.reg1c$coefficients[1:s]
print("MSE", Results1c)

# Post-Lasso
lasso.reg.post <- rlasso(y~X, post=TRUE, intercept=FALSE)
Results2 <- mean((fo-Xo%*%coef(lasso.reg.post))^2)
lasso.reg.post$coefficients[1:s]
print("MSE", Results2)

################################ test sup score test
set.seed(12345)
R <- 500
pvector <- numeric(length = R)
for (i in 1:R) {
n = 100 #sample size
p = 50 # number of variables
s = 3 # nubmer of non-zero variables
X = matrix(rnorm(n*p), ncol=p)
colnames(X) = paste("V", 1:p, sep="")
beta = c(rep(1,s), rep(0,p-s))
y = 0 + X%*%beta + rnorm(n)

lasso.reg = rlasso(y~X, post=TRUE, intercept=FALSE)
pvector[i] <- summary(lasso.reg)$pvalue
}
mean(pvector < 0.05)