


####################
###question 2###
####################

#### part a
##hypothesis test 1
## fail to reject the null that the true mean is <= 0.833
5/30
25/30
n=30
x=rnorm(n,mean=0.16667,sd=1)
sort(x)
xbar=mean(x)
sdx=sd(x)
alpha = 0.05
tq = qt(1-alpha, n-1)
c(qt(1-alpha, n-1), qt(alpha,n-1, lower.tail = F))
mu0=0.833
tstat=sqrt(n)*(xbar-mu0)/sdx;c(tstat,tq,pt(tstat,n-1,lower.tail=F))
xbar-tq*sdx/sqrt(n)
t.test(x,alternative="greater",mu=mu0)

##hypothesis test 2
## reject the null that the true mean > 0.833
# is less than 0.833
x=rnorm(n,mean=0.833,sd=1)
sort(x)
xbar=mean(x)
sdx=sd(x)
alpha = 0.05
tq = qt(1-alpha, n-1)
c(qt(1-alpha, n-1), qt(alpha,n-1, lower.tail = F))
mu0=0.1667
tstat=sqrt(n)*(xbar-mu0)/sdx;c(tstat,tq,pt(tstat,n-1,lower.tail=F))
xbar-tq*sdx/sqrt(n)
t.test(x,alternative="greater",mu=mu0)

#### part b
### hypothesis test 1
## fail to reject the null 
7/30
23/30
n=30
x=rnorm(n,mean=0.2333333,sd=1)
sort(x)
xbar=mean(x)
sdx=sd(x)
alpha = 0.05
tq = qt(1-alpha, n-1)
c(qt(1-alpha, n-1), qt(alpha,n-1, lower.tail = F))
mu0=0.7666667
tstat=sqrt(n)*(xbar-mu0)/sdx;c(tstat,tq,pt(tstat,n-1,lower.tail=F))
xbar-tq*sdx/sqrt(n)
t.test(x,alternative="greater",mu=mu0)

##hypothesis test 2
## reject the null that the true mean > 0.766
x=rnorm(n,mean=0.7666667,sd=1)
sort(x)
xbar=mean(x)
sdx=sd(x)
alpha = 0.05
tq = qt(1-alpha, n-1)
c(qt(1-alpha, n-1), qt(alpha,n-1, lower.tail = F))
mu0=0.2333333
tstat=sqrt(n)*(xbar-mu0)/sdx;c(tstat,tq,pt(tstat,n-1,lower.tail=F))
xbar-tq*sdx/sqrt(n)
t.test(x,alternative="greater",mu=mu0)


####################
###question 3###
####################

### n=10 and y=1, reject null that probability = 1/2, accept
# alternative that the probability of wins <1/5
n=10; y=1
2*pbinom(0,10,1/2, lower.tail=F)
binom.test(0,10,0.5, alternative = "less")

### n=30 and y=5, reject null that probability =1//2, accept
# alternative that the probability of wins <1/5
n=30; y=5
2*pbinom(4,29,1/2, lower.tail=F)
binom.test(4,19,0.5, alternative = "less")


####################
###question 4###
####################

####################
###question 5###
####################

avg = 1100
sd = 95

####################
###question 6###
####################

### n=1000 and y=550, reject null that the probability = 1/2
n=1000; y=550
2*pbinom(550,1000,1/2, lower.tail=F)
binom.test(550,1000,0.5)


####################
###question 7###
####################

# if 

####################
###question 8###
####################

n=36; p0=0.5; y=23; xbar=y/n;
tstat1=sqrt(n)*(xbar-p0)/sqrt(xbar*(1-xbar))
tstat2=sqrt(n)*(xbar-p0)/sqrt(p0*(1-p0))
round(c(xbar,tstat1,tstat2^2,2*pnorm(abs(tstat1),lower.tail=F),
        2*pnorm(abs(tstat2),lower.tail=F),pchisq(tstat2^2,df=1,lower.tail=F)),6)

prop.test(y,n,p0,correct=F)

####################
###question 9###
####################




