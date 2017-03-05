source("code/func.R")
library("numDeriv")

# SET UP ------------------------------------------------------------------------
V <- c(.55, .45, .55)
delta <- c(.02, .03, .05)
u <- c(0, 0, 0)
n <- length(V)
k <- (n + 1)/2

# TEST Q FOR EXPECTED NUMBER OF SEATS -------------------------------------------
# derivative of G
test_that("derG", {
  Gfun <- function(u, V, delta){
    pnorm(1/2 - u - V - delta)
  }
  q1 <- grad(Gfun, x = 0, V = V[1], delta = delta[1])
  q2 <- dnorm(1/2 - 0 - V[1] - delta[1])
  expect_equal(q1, -q2)
})

# optimization with logarithmic functional form

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
