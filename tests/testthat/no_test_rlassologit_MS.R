set.seed(1)
n = 100 #sample size
p = 200 # number of variables
s = 3 # nubmer of non-zero variables
X = matrix(rnorm(n*p), ncol=p)
colnames(X) = paste("V", 1:p, sep="")
beta = c(rep(3,s), rep(0,p-s))
intercept = 1
P <- exp(intercept + X %*% beta)/(1 + exp(intercept + X %*% beta))
y <- numeric(length = n)
for(i in 1:n){
  y[i] <- sample(x = c(1,0), size = 1, prob = c(P[i],1 - P[i]))
}


lasso.reg = rlassologit(y~X, post=TRUE, intercept=FALSE)
lasso.reg = rlassologit(X, y, post=TRUE, intercept=FALSE)
lasso.reg = rlassologit(y~X, post=FALSE, intercept=TRUE)
lasso.reg = rlassologit(y~X, post=TRUE, intercept=TRUE, penalty=list(lambda=0.1))

pred1 <- predict(lasso.reg)
pred2 <- predict(lasso.reg, newdata=X)
Xnew = matrix(rnorm(n/2*p), ncol=p)
pred1 <- predict(lasso.reg, newdata=Xnew)

data <- as.data.frame(cbind(y,X))
colnames(data) <- c("y", paste("V", 1:p, sep=""))
lasso.reg = rlassologit(y~ V1+ V2 + V3 + V4 , post=TRUE, intercept=TRUE, data=data)
pred3 <- predict(lasso.reg)
pred4 <- predict(lasso.reg, newdata=data)

lasso.reg = rlassologit(y~ V1+ V2 + V3 + V4 , post=TRUE, intercept=TRUE, data=data)
pred5 <- predict(lasso.reg)
pred6 <- predict(lasso.reg, newdata=data)

rhs <- paste("V", 1:100, sep="", collapse="+")
form <- as.formula(paste("y", "~", rhs))
lasso <- rlassologit(form, data=data)
pred7 <- predict(lasso.reg)


#### Test significance test:

head(lasso.reg$dev)
lasso.reg = rlassologit(y~X, post=TRUE, intercept=TRUE, model=FALSE)
summary(lasso.reg, all=FALSE)
lasso.reg = rlassologit(y~X, post=TRUE, intercept=TRUE, model=TRUE)
summary(lasso.reg, all=FALSE)

lasso.reg = rlassologit(y~X, post=TRUE, intercept=FALSE, model=TRUE)
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
  colnames(X) = paste("V", 1:p, sep="")
  beta = c(rep(3,s), rep(0,p-s))
  intercept = 1
  P <- exp(intercept + X %*% beta)/(1 + exp(intercept + X %*% beta))
  y <- numeric(length = n)
  for(i in 1:n){
    y[i] <- sample(x = c(1,0), size = 1, prob = c(P[i],1 - P[i]))
  }
lasso.reg = rlassologit(y~X, post=TRUE, intercept=TRUE, model=TRUE)
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

# lasso.reg1a <- rlassologit(y~X, post=FALSE, intercept=TRUE) 
# Results1a <- mean((fo-Xo%*%coef(lasso.reg1a))^2)
# lasso.reg1a$coefficients[1:s]

# Lasso with default penalty choice
lasso.reg1b <- rlassologit(y~X, post=FALSE, intercept=FALSE)
Results1b <- mean((fo-Xo%*%coef(lasso.reg1b))^2)
lasso.reg1b$coefficients[1:s]
print("MSE", Results1b)


# Lasso with half of the penalty
lasso.reg1c <- rlassologit(y~X, post=FALSE, intercept=FALSE, penalty=list(homoscedastic = "none", 
                                                                   lambda.start=2.2*sqrt(n)*qnorm(1-0.1/(2*p))/2)) 
Results1c <- mean((fo-Xo%*%coef(lasso.reg1c))^2)
lasso.reg1c$coefficients[1:s]
print("MSE", Results1c)

# Post-Lasso
lasso.reg.post <- rlassologit(y~X, post=TRUE, intercept=FALSE)
Results2 <- mean((fo-Xo%*%coef(lasso.reg.post))^2)
lasso.reg.post$coefficients[1:s]
print("MSE", Results2)


################################### test rlassologitEffects
set.seed(1)
n = 100 #sample size
p = 50 # number of variables
s = 3 # nubmer of non-zero variables
X = matrix(rnorm(n*p), ncol=p)
colnames(X) = paste("V", 1:p, sep="")
beta = c(rep(3,s), rep(0,p-s))
intercept = 1
P <- exp(intercept + X %*% beta)/(1 + exp(intercept + X %*% beta))
y <- numeric(length = n)
for(i in 1:n){
  y[i] <- sample(x = c(1,0), size = 1, prob = c(P[i],1 - P[i]))
}

xd <- X[,2:50]
d <- X[,1]

test <- rlassologitEffect(x=xd, d=d, y=y)
test <- rlassologitEffects(X,y, index=c(1,2,40))
test <- rlassologitEffects(y ~ X, I = ~ V1 + V2)
