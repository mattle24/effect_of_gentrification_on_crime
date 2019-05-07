// STAN AR(k) model code from: https://mc-stan.org/docs/2_19/stan-users-guide/autoregressive-section.html
data {
  int<lower=0> N; // total number of neighborhoods
  int K; // number of parameters
  int m;
  int pos[m]; // positions of gentrifying params
  matrix[N, K] X; // data matrix
  vector[N] y; // crime rate per 1K residents
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
  beta[5] ~ double_exponential(0, 1);
  beta[7] ~ double_exponential(0, 1);
}
