##############Author :Vidya
##############Date created: 01-11-2019


## testing for ADF test
library(aTSA)


Xt=alpha+beta*t+g*Xt-1+et


# linear and no drift
m1 <- lm(yt ~ xt1 - 1)
##both drift and linear
m3 <- lm(yt ~ xt1 + t)

# The standard Dickey-Fuller test (with constant and time-trend)
res <- lm(yt ~ xt1 + 1 + tt)   

ADF=coeff/S.E.


### Find out what if one type fails################3

### significance of the coeffecients 

aTSA::adf.test(rw)

aTSA::adf.test(rw_1)

##################################################################
#necessity of PACF

library(isdals)
install.packages("ppcor")
library(ppcor)


cor(df)

f.pr=predict(lm(y~x2,data=df))

T.pr = predict( lm(x1~x2,data=df) )

cor( (df$y- f.pr), (df$x1- T.pr) )

### can be performed with the function pcor

rand_pro=ts(rnorm(100))
rand_pro

acf(rand_pro,type = "partial")


###########################################

#Depth of differencing

#y=constant + weighted sum of last p values of y + weighted sum of the last q forecast errors

library(stats)

data(jj)

plot(jj)

par(mfrow=c(1,1))

jj1<- diff(log(jj))
acf(jj1)

acf(jj1,type="partial")

fit1<-arima(jj1,order=c(4,1,0))


obs<-autocorrelation coefficients
STATISTIC <- n * sum(obs^2)
PVAL <- 1 - pchisq(STATISTIC, lag)
H0 there is no auto correlation
H1:there is autocorrelation

residuals=fit1$residuals
plot(residuals)


stats::Box.test(residuals,lag=log(length(residuals)))

acf(residuals)
