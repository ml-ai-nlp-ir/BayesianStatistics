# sampling from a discrete distribution
p.true <- (1:10)/55
mu.true <- sum((1:10)*p.true)

s<-sample(1:10,1000,replace=T,prob=p.true)
hist(s,prob=T)
mean(s)

# simulation for exp(1)
set.seed(1234)
B <- 1000
u <- runif(B)
g <- function(y){
	x = - log(1-y)
	return(x)
}
# plot the simulated data
hist(g(u),prob=T,ylim=c(0,1))
fit=density(g(u))
lines(fit) # impose the nonparametric density estimate
tmp = seq(0,6,length=1000)
lines(tmp,dexp(tmp),col="red") # impose the true density
mean(g(u)) # sample mean; and the true mean should be 1