data {
  int<lower=1> n1; // Number of sampled observations
  int<lower=1> n2; // Number of nonsampled observations
  int<lower=1> nvar; // Number of regression variables
  int<lower=1> auxvar; // Number of auxiliary variable
  int<lower=1> m; // Number of Area
  vector[n1] y_sampled; // Sampled observations
  matrix[n1, auxvar] x_sampled; // Auxiliary Varibale for sampled data
  matrix[n2, auxvar] x_nonsampled; // Auxiliary Varibale for nonsampled data
  array[n1] int<lower=1, upper=m> state2_sampled; // Area indicators for sampled data
  array[n2] int<lower=1, upper=m> state2_nonsampled; // Area indicators for nonsampled data
  vector[nvar] mu_b; // Means for regression coefficients
  vector<lower=0>[nvar] sigma2_b; // Variance for regression coefficients
  real<lower=0> phi_aa; // Hyperparameter for phi.a
  real<lower=0> phi_ab; // Hyperparameter for phi.a
  real<lower=0> phi_ba; // Hyperparameter for phi.b
  real<lower=0> phi_bb; // Hyperparameter for phi.b
  real<lower=0> tau_ua; // Hyperparameter for sigma2.u
  real<lower=0> tau_ub; // Hyperparameter for sigma2.u
  real<lower=0> tau_va; // Hyperparameter for sigma2.v
  real<lower=0> tau_vb; // Hyperparameter for sigma2.v
}

parameters {
  vector[n1] u1; // Random effects (Subarea) for sampled data
  vector[n2] u2; // Random effects (Subarea) for nonsampled data
  vector[m] f; // Random effects (Area)
  vector[nvar] b; // Regression coefficients
  vector<lower=0>[n1] phi_sampled;
  vector<lower=0>[n2] phi_nonsampled;
  real<lower=0> phi_a; // Hyperparameter for phi.a
  real<lower=0> phi_b; // Hyperparameter for phi.b
  real<lower=0> sigma2_u; // Variance for u
  real<lower=0> sigma2_v; // Variance for f
}

transformed parameters {
  vector[n1] mu_sampled;
  vector[n2] mu_nonsampled;
  vector<lower=0>[n1] A;
  vector<lower=0>[n2] A_nonsampled;
  vector<lower=0>[n1] B;
  vector<lower=0>[n2] B_nonsampled;

  for (i in 1:n1) {
    mu_sampled[i] = inv_logit(b[1] + dot_product(b[2:nvar], x_sampled[i]) + f[state2_sampled[i]] + u1[i]);
    A[i] = mu_sampled[i] * phi_sampled[i];
    B[i] = (1 - mu_sampled[i]) * phi_sampled[i];
  }

  for (i in 1:n2) {
    mu_nonsampled[i] = inv_logit(b[1] + dot_product(b[2:nvar], x_nonsampled[i]) + f[state2_nonsampled[i]] + u2[i]);
    A_nonsampled[i] = mu_nonsampled[i] * phi_nonsampled[i];
    B_nonsampled[i] = (1 - mu_nonsampled[i]) * phi_nonsampled[i];
  }
}

model {
  // Priors
  f ~ normal(0, sqrt(sigma2_v));
  u1 ~ normal(0, sqrt(sigma2_u));
  u2 ~ normal(0, sqrt(sigma2_u));

  // Regression coefficients
  for (k in 1:nvar) {
    b[k] ~ normal(mu_b[k], sqrt(sigma2_b[k]));
  }

  phi_sampled ~ gamma(phi_a, phi_b);
  phi_nonsampled ~ gamma(phi_a, phi_b);
  phi_a ~ gamma(phi_aa, phi_ab);
  phi_b ~ gamma(phi_ba, phi_bb);
  sigma2_u ~ inv_gamma(tau_ua, tau_ub);
  sigma2_v ~ inv_gamma(tau_va, tau_vb);

  // Likelihood
  for (i in 1:n1){
    y_sampled[i] ~ beta(A[i], B[i]);
  }
}
