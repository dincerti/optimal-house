source("code/func.R")
library("numDeriv")
library("testthat")
library("Rsolnp")

# SET UP ------------------------------------------------------------------------
theta <- .5
sigma <- .061
V <- c(.75, .45, .62)
delta <- .03
u <- c(0, 0, 0)
n <- length(V)
k <- (n + 1)/2
Gfun <- function(u, V, delta, sigma){
  pnorm(1/2 - u - V - delta, 0, sigma)
}

# TEST Q FOR EXPECTED NUMBER OF SEATS -------------------------------------------
# derivative of G
test_that("derG", {
  q1 <- grad(Gfun, x = 0, V = V[1], delta = delta[1], sigma = sigma)
  q2 <- dnorm(1/2 - 0 - V[1] - delta[1], 0, sigma)
  expect_equal(q1, -q2)
})

# optimization with logarithmic functional form
estar <- (dnorm(1/2 - 0 - V - delta, 0, sigma)/sum(dnorm(1/2 - 0 - V - delta, 0, sigma))) * 15
utility <- function(e, theta){
  return(theta * log(e) - estar)
}
theta.default <- theta; V.default <- V; delta.default <- delta; sigma.default <- sigma
min_seats <- function(e){
  u <- utility(e, theta)
  -sum(pnorm(1/2 - u - V - delta, 0, sigma))
}
bc <- function(e) {
  sum(e)
}
max_seats(e = c(5, 5, 5), theta = theta, V = V, delta = delta, sigma = sigma)
constrOptim(c(1, 12, 1), min_seats, grad = NULL, ui = c(-1, -1, -1), ci = -15)
gosolnp(pars =estar, #starting values (random - obviously need to be positive and sum to 15)
      fun = min_seats, #function to optimise
     eqfun = bc, #equality function 
     eqB = 15,   #the equality constraint
     LB = c(0,0,0), #lower bound for parameters i.e. greater than zero
     UB = c(15,15,15)) #upper bound for parameters (I just chose 100 randomly)


# TEST Q FOR MAXIMING PROBABILIY OF WINNING K SEATS -----------------------------
# probability winning more than k seats
prob_k <- function(u, k, V, delta){
  G <- pnorm(1/2 - u - V - delta)
  mu.s <- sum(G)
  sigma.s <- sum(G * (1- G))
  return(1 - pnorm((k - mu.s)/sigma.s))
}
prob_k(0, k = k, V, delta)

# Q with k-rule
Q_krule <- function(u, k, V, delta){
  G <- pnorm(1/2 - u - V - delta)
  g <- dnorm(1/2 - u - V - delta)
  mu.s <- sum(G)
  sigma.s <- sum(G * (1- G))
  x <- (k - mu.s)/sigma.s
  Q.mean <- (1/sigma.s) * dnorm(x) * g
  Q.var <- (1/sigma.s) * dnorm(x) * x * (1 - 2 * G) * g 
  return(Q.mean + Q.var)
}

test_that("Qseats", {
  q1 <- grad(prob_k, x = c(0, 0, 0), k = k, V = V, delta = delta)
  q2 <- Q_krule(0, k = k, V = V, delta = delta)
  expect_equal(q1, -q2)
})
