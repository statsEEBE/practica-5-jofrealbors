
#poblacion

mu<-95.3
sigma<- 5.7



curve(dnorm(x,mean=mu,sd=sigma),xlim=c(80,120))
#set.seed(123)
rnorm(1,mu,sigma)


#mostra aleatoria tamany 4
Y<- function(i)(sum(rnorm(4,mu,sigma)))
Y10000<- sapply(1:10000,Y)
Y10000
hist(Y10000)
mean(Y10000)

4*mu

#variancia suma mostral
4*sigma^2
var(Y10000)

###
hist(Y10000,freq=FALSE)
curve(dnorm(x,mean=4*mu,sd=sqrt(4)*sigma),add=TRUE)


### b) 3249
Y<- function(i)(sum(rnorm(100,mu,sigma)))
Y10000<- sapply(1:10000,Y)
var(Y10000)

#en teoria
100*sigma^2


#c)  0.08704
1-pnorm(103,mu,sigma)

Y<- function(i)(sum(rnorm(1,mu,sigma)))
Y100000<- sapply(1:100000,Y)
hist(Y100000)
mean(Y100000>103)


#d) n=4

Xbar<- function(i)(mean(rnorm(4,mu,sigma)))
Xbar100000<- sapply(1:100000,Xbar)
hist(Xbar100000)
mean(Xbar100000<98)

#e) està malament(es volia posar 32 enlloc de 98), 0.52486

Ssq<- function(i)(var(rnorm(100,mu,sigma)))
Ssq100000<- sapply(1:100000,Ssq)
hist(Ssq100000)

mean(Ssq100000>32)

1-pchisq((100-1)*32/sigma^2,100-1)
hist(Ssq100000*(100-1)/sigma^2,prob=TRUE)
curve(dchisq(x,100-1),add=TRUE,col="red")
(100-1)*32/sigma^2



