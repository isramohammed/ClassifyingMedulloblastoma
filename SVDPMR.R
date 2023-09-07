# required packages
library(ggplot2)
library(caret)
library(msgl)
# SVDPMR function requires the following arguments:
# x: design matrix of size n*p
# y: classes factor of size n*1
# j: number of the new reduced features
# lam: the value of the penalization parameter
# SVDPMR function produced the following values:
# MinMax: the parameters of min-max scaling method
# singular: the right singular vectors of the design matrix
# model: penalized multinomial regression models corresponding to the sequence of lambda from min to max 
SVDPMR<-function(x,y,j,lam)
{
# scaling data
pp = preProcess(x, method = "range")
x<-predict(pp, x)
x<-as.matrix(x)
# reducing features using singular value decomposition method
svd1<-svd(x)
d<-diag(svd1$d)
V<-svd1$v
o<-sum(d[1:j,1:j]^2)/sum(d^2)
if(o>=.99){
V10 <- as.matrix(V[, 1:j])
x<-x%*%V10}
else {stop}
# fitting the penalized multinomial regression models
f1<-msgl::fit(x,t(y),alpha=0,d=10,standardize =T, intercept = FALSE,lambda=lam)
res = list(MinMax=pp,singular=V10,model = f1)
}
