data {
  int<lower=1> n; // Number of observations
  int<lower=1> nvar; // Number of regression variables
  int<lower=1> auxvar; // Number of auxiliary variable
  int<lower=1> m; // Number of state-level random effects
  vector[n] y; // Sampled observations
  matrix[n, auxvar] x; // Covariates for sampled data
  array[n] int<lower=1, upper=m> state2; // State indicators for sampled data
  vector[nvar] mu_b; // Means for regression coefficients
  vector<lower=0>[nvar] sigma2_b; // Variance for regression coefficients
  real<lower=0> tau_ua; // Hyperparameter for sigma2.u
  real<lower=0> tau_ub; // Hyperparameter for sigma2.u
  real<lower=0> tau_va; // Hyperparameter for sigma2.v
  real<lower=0> tau_vb; // Hyperparameter for sigma2.v
  real<lower=0> phi_aa; // Hyperparameter for phi.a
  real<lower=0> phi_ab; // Hyperparameter for phi.a
  real<lower=0> phi_ba; // Hyperparameter for phi.b
  real<lower=0> phi_bb; // Hyperparameter for phi.b
}

parameters {
  vector[n] u; // Random effects for sampled data
  vector[m] f; // State-level random effects
  vector[nvar] b; // Regression coefficients
  vector<lower=0>[n] phi;
  real<lower=0> phi_a; // Hyperparameter for phi.a
  real<lower=0> phi_b; // Hyperparameter for phi.b
  real<lower=0> sigma2_u; // Variance for u
  real<lower=0> sigma2_v; // Variance for f
}

transformed parameters {
  vector[n] mu;
  vector<lower=0>[n] A;
  vector<lower=0>[n] B;

  for (i in 1:n) {
    mu[i] = inv_logit(b[1] + dot_product(b[2:nvar], x[i]) + f[state2[i]] + u[i]);
    A[i] = mu[i] * phi[i];
    B[i] = (1 - mu[i]) * phi[i];
  }
}

model {
  // Priors
  f ~ normal(0, sqrt(sigma2_v));
  u ~ normal(0, sqrt(sigma2_u));

  phi ~ gamma(phi_a, phi_b);
  phi_a ~ gamma(phi_aa, phi_ab);
  phi_b ~ gamma(phi_ba, phi_bb);

  // Regression coefficients
  for (k in 1:nvar) {
    b[k] ~ normal(mu_b[k], sqrt(sigma2_b[k]));
  }

  sigma2_u ~ inv_gamma(tau_ua, tau_ub);
  sigma2_v ~ inv_gamma(tau_va, tau_vb);

  // Likelihood
  for (i in 1:n){
    y[i] ~ beta(A[i], B[i]);
  }
}
