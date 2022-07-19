# ------------
#'*Packages*
# --------
install.packages(c("rstan"), repos="https://cloud.r-project.org/",dependencies=TRUE)
install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos"),dependencies=TRUE))
install.packages('rethinking',type='source')

# define grid
p_grid <- seq( from=0 , to=1 , length.out=20 )
# define prior
prior <- rep( 1 , 20 )
# compute likelihood at each value in grid
likelihood <- dbinom( 1 , size=1 , prob=p_grid ) * dbinom( 0 , size=1 , prob=p_grid ) * dbinom( 0 , size=1 , prob=p_grid )
# compute product of likelihood and prior
unstd.posterior <- likelihood * prior
# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)

plot(p_grid,likelihood)


# ------------
#'*Random*
# ------------
FALSE+TRUE+FALSE+TRUE #0+1+0+1 = 2


# ------------
#'*Lecture 03*
# 3.1
# ----

p_grid <- seq( from=0 , to=1 , length.out=50 )
prior <- rep(1,50)
likelihood <- dbinom( 3 , size=3 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)

plot(p_grid,posterior)

n_samples <- 5000
samples <- sample( p_grid , size=n_samples , replace=TRUE , prob=posterior )
plot(density(samples)$x, n_samples*density(samples)$y,  xlim=c(0, 1), type="l")

# 3.?
# ----

# we take 10 samples
n <- 10

# binomial distribution with
# mean = 0
# standard deviation = 1
alpha <- rnorm(n,60,10)
beta  <- rlorm(n,0,1) # we only expect positive slopes, so we use log norm

# if we wanted to visualize in 2D-space
# library(lattice) # is needed for stripplot
# stripplot(alpha)
# stripplot(beta)

plot(NULL, xlim=c(-2,2), ylim=c(-2,2), xlab="height", ylab="weight")
for (i in 1:n_samples)
  abline(alpha[i], beta[i], lwd=4, col=2) # lwd = line width
  # abline() = add one or more straight lines to a plot in R

# library(rethinking)
# globe.qa <- quap(
#   alist(
#     W ~ dbinom( W+L,p),
#     p ~ dunif(0,1)
#   ) ,
#   Data=list(W=3,L=3))

# R Code 2.7
W <- 6 ; L <- 3
curve( dbeta(x, W+1, L+1), from=0, to=1)
curve( dnorm(x, 0.67, 0.16), lty=2, add=TRUE)

# ------------
#'*Lecture 04*
# 4.1
# ----

library(rethinking)

runif(3,-1,1)
pos <- replicate( 1000 , sum( runif(16,-1,1) ) )
pos

# 4.2
# ----

library(rethinking)
data(Howell1)

d <- Howell1

plot(d$age,d$weight,col= "red"                    , pch=19, main = "Height to Bodyweight")
plot(d$age,d$weight,col=col.alpha("red",alpha=0.2), pch=19)
plot(d$age,d$weight,col=c("red","blue")[Howell1$male+1], pch=19)


d2 <- d[ d$age >= 18 , ]
plot(d2$age,d2$weight,col=col.alpha("red",alpha=0.2), pch=19)

  plot(d2$age[d2$male == 0],d2$weight[d2$male == 0],col=col.alpha(c("red"), alpha=0.3), pch=19)
points(d2$age[d2$male == 1],d2$weight[d2$male == 1],col=col.alpha(c("blue"),alpha=0.3), pch=19)


# 4.27

flist <- alist(
  height ~ dnorm( mu , sigma ) , 
  mu ~ dnorm( 178 , 20 ) ,
  sigma ~ dunif( 0 , 50 )
)

flist

# quap estimates the posterior by climbing it like a hill
m4.1 <- quap( flist , data=d2 )
m4.1

# provide Gaussian approximations for each parameter's marginal distribution
precis( m4.1 ) 

# ----
# 4.3
# ----

start <- list(
  mu =  mean(d2$height),
  sigma = sd(d2$height)
  )

start
# These start values are good guesses of the rough location of the MAP values

m4.1 <- quap( flist , data=d2 , start=start)
m4.1

#'*4.31*

# change the standard deviation of the prior to 0.1
# so it's a very narrow prior.

m4.2 <- quap(
  
  alist(
    
    height ~ dnorm( mu , sigma ), 
    mu ~ dnorm( 178 , 0.1 ),
    sigma ~ dunif( 0 , 50 )), 
  
  data=d2 )

precis( m4.2 )
# estimate for mu has hardly moved off the prior
# estimate for sigma has changed quite a lot
# even though we didn't change its prior at all
#   -> only way the model can explain data

#'*4.32*
# variance-covariance matrix
vcov( m4.1 )
# how each parameters relates to each other in the posterior distribution

diag( vcov( m4.1 ) )
#gives us diagonal values (so mu:mu and sigma:sigma)

#'*4.34*
#'sample vectors of values from a multi-dimensional Gaussian distribution
post <- extract.samples( m4.1 , n=1e4 ) # 10'000 values to be exact
head(post) # head() -> gets the first parts of a vector/matrix
# Each value is a sample from the posterior, 
# so the mean and standard deviation of each column will be very close to the MAP values from before

#'*4.35*
precis(post)

#'Compare these values to the output from precis(m4.1) :
precis(m4.1)

# what's the difference?
(precis(post)[c(1,2),c(1,2)])-(precis(m4.1)[c(1,2),c(1,2)])

# plot it (?) 
plot( post, col="purple")
plot( m4.1, col="yellow")

#'*4.36*
library(MASS)
# mvrnorm simulates random vectors of multivariate Gaussian values
post_sim <- mvrnorm( n=1e4 , mu=coef(m4.1) , Sigma=vcov(m4.1) )

#'*coef* extracts model coefficients from objects returned by modeling functions
coef(m4.1)
vcov(m4.1)
m4.1

head(post_sim)

#'*4.37*
# plot height and weight against one another
# how strongly do they co-vary?
plot( d2$height ~ d2$weight, col=col.alpha("red", alpha=0.3))
plot( d2$weight,  d2$height, col=col.alpha("red", alpha=0.3))
# this is literally the same thing, right (?)
line( d2$height ~ d2$weight)

#'*4.38*
plot(dnorm(seq(from=1, to=20),mean=0,sd=4),type="l")

# START HERE ---
dev.off()
par(mfrow=c(2,1))

# simulate a bunch of lines, implied by the priors for alpha and beta
set.seed(2971)
# we want 50 lines
N <- 50
# rnorm = random generation for the normal distribution
a <- rnorm( N , 178 , 20 )
b <- rnorm( N , 0 , 10 )

par(mfrow=c(2,1))
plot(a, main="a = rnorm( N , 178 , 20 )")
hist(a, main="")
par(mfrow=c(1,1))

#'*4.39*
# now we have 100 pairs of and values. Now to plot the lines:

dev.off()
par(mfrow=c(2,1))

plot(NULL, xlim=range(d2$weight), ylim=c(-100,400),
           xlab="weight",         ylab="height")

abline( h=0 , lty=2 )
abline( h=272 , lty=1 , lwd=0.5 )
mtext( "b ~ dnorm(0,10)" )

xbar <- mean(d2$weight)

for ( i in 1:N ){
  curve(a[i] + b[i]*(x - xbar),
        from=min(d2$weight),
        to=max(d2$weight),
        add=TRUE,
        col=col.alpha("black",0.2))
}

# we know that average height increases with average weight
# let's  restrict Beta (b) to positive values

## b ~ Log-Normal(0,1)
# b <- rlnorm( 1e4 , 0 , 1 )
# dens( b , xlim=c(0,5) , adj=0.1)

set.seed(2971)
N <- 100
a <- rnorm( N , 178 , 20 )
b <- rlnorm( N , 0 , 1 )

plot(NULL, xlim=range(d2$weight), ylim=c(-100,400),
     xlab="weight",         ylab="height")

abline( h=0 , lty=2 )
abline( h=272 , lty=1 , lwd=0.5 )
mtext( "b ~ rlnorm(0,10)" )

xbar <- mean(d2$weight)

for ( i in 1:N ){
  curve(a[i] + b[i]*(x - xbar),
        from=min(d2$weight),
        to=max(d2$weight),
        add=TRUE,
        col=col.alpha("black",0.06))
}

par(mfrow=c(1,1))
dev.off()

#'*4.42*
# Finding the posterior distribution
# incorporate our new model into quap

# load data again 
library(rethinking)
data(Howell1)
d <- Howell1
d2 <- d[ d$age >= 18 , ]

# define the average weight
xbar <- mean(d2$weight)

# fit model
m4.3 <- quap (
       
   alist(
         
        height ~ dnorm( mu , sigma ),
        mu <- a + b*( weight - xbar ),
           a ~ dnorm( 178 , 20 ),
           b ~ dlnorm( 0 , 1 ),
        sigma ~ dunif( 0 , 50 )),
        
  data=d2 )

m4.3
precis(m4.3)
precis(m4.3)[1,1]
precis(m4.3)[2,1]

# not working (??)
par(mfrow=c(1,1)); dev.off();
plot( d2$height ~ d2$weight,
      col=col.alpha("red", alpha=0.3),
      xlim=c(0,75), main="line is off.."
      ) ; abline(a=precis(m4.3)[1,1],b=precis(m4.3)[2,1])




# ------------
#'*Lecture 05*
# 5.1