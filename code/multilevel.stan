// multilevel regression
data {
  int<lower=0> N; // number of observations
  int<lower=0> K; // number of predictors
  int<lower=0> T; // number of years
  int<lower=1,upper=T> year[N]; // year for district
  matrix[N,K] x; // district predictors
  vector[N] y; // outcomes
} 
parameters {
  vector[T] delta; // intercept by year
  vector[K] beta; // district coefs
  real<lower=0,upper=1> sigma_delta; // year error
  real<lower=0,upper =1> sigma; // district error
} 
model {
  beta ~ normal(0, 1);
  delta ~ normal(0, sigma_delta);
  for (n in 1:N)
    y[n] ~ normal(delta[year[n]] + x[n] * beta, sigma);
}
// generated quantities {
//   vector[N] y_rep;
//   for (n in 1:N)
//   y_rep[n] <- normal_rng(delta[year[n]] + x[n] * beta, sigma);
// }