##############Author :Vidya
##############Date created: 01-11-2019

install.packages("astsa")

library(stats)
library(datasets)
library(forecast)
library(astsa)
library(tseries)


data(births)
birth_data <- ts(births, frequency = 12, start = c(1946, 1))

plot.ts(birth_data)


################################################################

rand_pro=ts(rnorm(100))
rand_pro

#lags yk=cov(Xt,Xt+k)

#using pearson correlartion 

mean=mean(rand_pro)

#S0
var=sum((rand_pro-mean)^2/length(rand_pro))

#s1

md1<-(rand_pro[1:99]-mean)
md2<-(rand_pro[2:100]-mean)

s1=sum(md1*md2)/length(rand_pro)

cov<-stats::acf(rand_pro,type="covariance")

cov
#correlation r=Ck/C0
### at lag 1 r=1

cor<-stats::acf(rand_pro)
cor


###########################################################################################

y=bx1+cx2+dx3+c1

y=signal+noise

births_data_comp<-decompose(birth_data)

plot(births_data_comp)

###################################################################

#Xt- stock price 
#The stock price is affected by announcements made three days ago

Xt=Zt+aZ(t-1)+bZ(t-2)


z=rnorm(10000)

# Introduce a variable
ma=NULL

##Loop for generating MA(2) process

for(i in 3:10000){
  ma[i]=z[i]+0.7*z[i-1]+0.16*z[i-2]
}


ma2=ma[3:10000]

ma2=ts(ma2)


# plot the process and plot its ACF
plot.ts(ma2)
stats::acf(ma2)   # the plotcuts off at 2



#################################################################

x=NULL
x[1]=0


for(i in 2:2000)
{
  x[i]=x[i-1]+rnorm(1)
}


X1=0
X2=0+Z1

X3=X2+Z2


rw<-ts(x)

plot(rw,lwd=2)

stats::acf(rw)  

##############################################################


Xt=X(t-1)+Zt

Zt=Xt-X(t-1)

Zt=delta(Xt)## random process 

rw_1<-diff(rw)

plot(rw_1)


stats::acf(rw)
stats::acf(rw,type = "partial")

################################understanding ACF


set.seed(2016); N=1000; phi = 1;
Z = rnorm(N,0,1); X=NULL; X[1] = Z[1];
for (t in 2:N) {
  X[t] = Z[t] + phi*X[t-1] ;
}
X.ts = ts(X)
par(mfrow=c(2,1))
plot(X.ts,main="AR(1) Time Series on White Noise, phi=.4")
X.acf = acf(X.ts, main="AR(1) Time Series on White Noise, phi=.4")



set.seed(2017)
X.ts <- arima.sim(list(ar = c(.7, .2)), n=1000)
par(mfrow=c(2,1))
plot(X.ts,main="AR(2) Time Series, phi1=.7, phi2=.2")
X.acf = acf(X.ts, main="Autocorrelation of AR(2) Time Series")


set.seed(2016); N=1000; phi = .4;
Z = rnorm(N,0,1)
X=NULL;
X[1] = Z[1];
for (t in 2:N) {
  X[t] = Z[t] + phi*X[t-1] ;
}
X.ts = ts(X)
X.acf = acf(X.ts)
X.acf$acf


