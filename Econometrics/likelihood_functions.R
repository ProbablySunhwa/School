# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                   Likelihood  Functions
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# likelihood function with only factors
LL <- function(alpha, beta, sigma) {
  mu <-  alpha + x[,n]*beta 
  R = suppressWarnings(dnorm(y[,n],mu,sigma, log = TRUE))
  -sum(R)
}


# PPP likelihood function
LL.z <- function(alpha, beta, gamma, sigma) {
  mu <- alpha + x[,n]*beta + z[,n]*gamma
  R = suppressWarnings(dnorm(y[,n], mu, sigma, log = TRUE))
  -sum(R)
}
