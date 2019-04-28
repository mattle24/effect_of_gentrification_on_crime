// STAN AR(k) model code from: https://mc-stan.org/docs/2_19/stan-users-guide/autoregressive-section.html
data {
  int<lower=0> N; // total number of neighborhoods
  int K; // number of parameters
  int pos; // position of gentrifying param
  matrix[N, K] X; // data matrix
  vector[N] y; // log crime rate next
}

parameters {
  vector[K] beta;
  real<lower=0> sigma;
}

model {
  // likelihood
  y ~ normal(X * beta, sigma);
  
  // priors
  // prior for gentrifying
  beta[pos] ~ normal(-5, 1); 
}
