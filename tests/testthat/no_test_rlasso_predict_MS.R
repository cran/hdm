set.seed(1)
n = 100 #sample size
p = 100 # number of variables
s = 3 # nubmer of variables with non-zero coefficients
X = Xnames = matrix(rnorm(n*p), ncol=p)
colnames(Xnames) <- paste("V", 1:p, sep="")
beta = c(rep(5,s), rep(0,p-s))
Y = X%*%beta + rnorm(n)
Xnew = Xnewnames = matrix(rnorm(n*p), ncol=p)  # new X
colnames(Xnewnames) <- paste("V", 1:p, sep="")
Ynew =  Xnew%*%beta + rnorm(n)  #new Y

###
reg <- rlasso(Y~Xnames)


dat = as.data.frame(cbind(Y, Xnames))
colnames(dat)[1] <- "Y"

# plain

lasso.reg1 = rlasso(Y~X,post=TRUE)
lasso.reg1b = rlasso(Y~X,post=FALSE)
yhat1 = predict(lasso.reg1, newdata=Xnew)
yhat1b = predict(lasso.reg1)
lasso.reg2 = rlasso(Y~X, data=dat, post=TRUE)
yhat2 = predict(lasso.reg2, newdata=Xnew)
yhat2b = predict(lasso.reg2)
MSE2b <- mean((Ynew-yhat2b)^2)
lasso.reg3 = rlasso(x=X, y=Y, post=TRUE)
yhat3 = predict(lasso.reg3, newdata=Xnew)
yhat3b = predict(lasso.reg3)

head(cbind(Ynew, yhat1, yhat2, yhat3))
mean((Ynew-yhat1)^2)
mean((Ynew-yhat2)^2)
mean((Ynew-yhat3)^2)
head(cbind(Y, yhat1b, yhat2b, yhat3b))
mean((Y-yhat1b)^2)
mean((Y-yhat2b)^2)
mean((Y-yhat3b)^2)

# formula
Xnew2 <- Xnew[,1:4]
Xnew3 <- Xnewnames[,1:4]
lasso.reg1f = rlasso(Y~V1+ V2+ V3 + V4, data=dat,post=TRUE)
yhat1f = predict(lasso.reg1f, newdata=Xnew)
yhat1ff = predict(lasso.reg1f, newdata=Xnew2)
yhat1fff = predict(lasso.reg1f, newdata=Xnew3)
yhat1bff = predict(lasso.reg1f, newdata=dat)
yhat1bf = predict(lasso.reg1f)

lasso.reg2f = rlasso(Y~(V1+ V2+ V3 + V4)^2, data=dat,post=TRUE)
yhat2f = predict(lasso.reg2f, newdata=Xnew)
yhat2ff = predict(lasso.reg2f, newdata=Xnew2)
yhat2fff = predict(lasso.reg2f, newdata=Xnew3)
yhat2bf = predict(lasso.reg2f)
yhat2bff = predict(lasso.reg2f, newdata=dat)

head(cbind(Ynew, yhat2fff))
mean((Ynew-yhat2fff)^2)
head(cbind(Y, yhat2bf, yhat2bff))
mean((Y-yhat2bf)^2)
mean((Y-yhat2bff)^2)
