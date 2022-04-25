


####################
###question 2###
#####################

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


##binomial way part a
n=30; y=25
# so ,yes, one of them is better than the other
binom.test(25,30,0.5, alternative = "greater")

## binomial way part b
### accept the alternative that A < B
binom.test(7,30,0.7666667, alternative = "less")



####################
###question 3###
####################

### n=10 and y=1, reject null that probability = 1/2, accept
# alternative that the probability of wins <1/2
n=10; y=1
pbinom(1,10,0.5, lower.tail=T)
binom.test(1,10,0.5, alternative = "less")
# 1- pbinom(9,10,0.5)


### n=30 and y=5, reject null that probability =1//2, accept
# alternative that the probability of wins <1/2
n=30; y=5
pbinom(5,30,1/2, lower.tail=T)
binom.test(5,30,0.5, alternative = "less")
# 1-pbinom(25,30,0.5)


####################
###question 4###
####################

np = 215*0.94
np
var = np*(1-0.94)
var
sqrt(var)
((200+0.5)-202.1)/(3.482)
## using the pnorm
pnorm(-0.459, lower.tail = T)


## the proability one is available to everyone is 32.28%
pbinom(200, 215, 0.94, lower.tail = T)

## second way
1-pbinom(14,215,.06)



####################
###question 5###
####################

avg = 1100
sd = 95
n=50
se = sd/sqrt(n)

### STEP 1: get the t's
tstat_1=sqrt(n)*(1110-1100)/sdx
tstat_2=sqrt(n)*(1075-1100)/sdx

### STEP 2: get the p
pnorm(tstat, lower.tail = T)
pnorm(tstat_2, lower.tail = T)

### step 3: subtract the differences
0.7716594 - 0.0313857


####################
###question 6###
####################

### normal approximation
550/1000
np = 1000*0.5
np
var = np*(1-0.5)
var
sd = sqrt(var)
sd
(550-500)/(sd)
## very unlikely that this coin is fair, that we
# see 550 if it is fair, so it's not
2*pnorm(3.794733, lower.tail = F)

### binomial way
n=1000; y=550
2*pbinom(550,1000,1/2, lower.tail=F)
## two tailed binomial test
binom.test(550,1000,0.5)



####################
###question 7###
####################

### with dbinom, add them?
n=10; p=1/2;
dbinom(1,n,p)
dbinom(10,n,p)
0.009765625*2


### part b
n=10; p=1/10;
dbinom(1,n,p) + dbinom(10,n,p)



####################
###question 8###
####################
avg = 8
sdx = 0.4
n=36

### find test statistic
tstat_1=sqrt(n)*(8.1-avg)/sdx
## find the probability in the upper tail
pnorm(tstat_1, lower.tail = F)
### 6% chance it exceeds 8.1



####################
###question 9###
####################




