data {
  int<lower=1> T;
  int<lower=1> J;
  matrix[T, J] Y;
}
transformed data {
  /* --- Pre-computed means & demeaned series --- */
  vector[T] Y1_dm = to_vector(Y[, 1]) - mean(to_vector(Y[, 1]));
  vector[T] Y2_dm = to_vector(Y[, 2]) - mean(to_vector(Y[, 2]));

  /* Constant used in ULC eq. */
  real mu_pu = mean(to_vector(Y[, 1] - Y[, 4]));
}
parameters {
  /* --- State --- */
  real<lower=0> tau;                     // rw-sd
  vector[T]  eta;                        // innovation shocks (N(0,1))
  real       nhat_init;

  /* --- Inflation eq. --- */
  real<lower=0,upper=1> delta_pt;
  real beta1_pt;
  real beta2_pt;
  real beta3_pt;
  real<upper=0> gamma_pt;
  real<upper=0> lambda_pt;
  real alpha_pt;
  vector[2] xi_pt;
  real phi_pt;
  real<lower=0> sigma_pt;

  /* --- ULC eq. --- */
  vector[2] beta_pu;
  real<upper=0> gamma_pu;
  real<upper=0> lambda_pu;
  vector[2] xi_pu;
  real<lower=0> sigma_pu;
}
transformed parameters {
  vector[T] NAIRU;
  NAIRU[1] = nhat_init + eta[1] * tau;
  for (t in 2:T)
    NAIRU[t] = NAIRU[t-1] + eta[t] * tau;
}
model {
  /* --- Priors (tune to taste) --- */
  tau       ~ normal(0.1, 0.05);          // half-N since tau>0
  eta       ~ std_normal();               // non-centred innovations
  nhat_init ~ normal(5.5, 0.2);

  beta1_pt  ~ normal(0.24, 0.5);
  beta2_pt  ~ normal(0.16, 0.5);
  beta3_pt  ~ normal(0.18, 0.5);
  gamma_pt  ~ normal(-0.38, 0.5);
  phi_pt    ~ normal(0.06, 0.5);
  lambda_pt ~ normal(-0.7, 0.2);
  alpha_pt  ~ normal(0.1, 0.5);
  xi_pt     ~ normal(0, 3);
  sigma_pt  ~ normal(0.30, 0.5);          // half-normal

  beta_pu   ~ normal(0.3, 0.1);
  gamma_pu  ~ normal(-2, 2);
  lambda_pu ~ normal(-3, 2);
  xi_pu     ~ normal(0, 3);
  sigma_pu  ~ normal(1.17, 2);            // half-normal

  /* --- Likelihood --- */
  {
    vector[T] pt_hat;
    vector[T] pu_hat;

    /* Inflation (needs t ≥ 4) */
    for (t in 4:T) {
      pt_hat[t] =
          (1 - beta1_pt - beta2_pt - beta3_pt) * Y[t, 5] +
          phi_pt * Y1_dm[t - 1] +
          beta1_pt * Y[t - 1, 4] +
          beta2_pt * Y[t - 2, 4] +
          beta3_pt * Y[t - 3, 4] +
          gamma_pt * ((Y[t, 3] - NAIRU[t]) / Y[t, 3]) +
          alpha_pt * (Y2_dm[t - 1] - Y2_dm[t - 2]) +
          xi_pt[1] * Y[t, 6] +
          xi_pt[2] * Y[t, 7] +
          lambda_pt * (Y[t - 1, 3] - Y[t - 2, 3]) / Y[t, 3];
    }
    Y[4:T, 4] ~ normal(pt_hat[4:T], sigma_pt);

    /* ULC (needs t ≥ 3) */
    for (t in 3:T) {
      pu_hat[t] =
          mu_pu +
          (1 - beta_pu[1] - beta_pu[2]) * Y[t, 5] +
          beta_pu[1] * Y[t - 1, 4] +
          beta_pu[2] * Y[t - 2, 4] +
          gamma_pu * (1 - NAIRU[t] / Y[t, 3]) +
          lambda_pu * (Y[t - 1, 3] - Y[t - 2, 3]) / Y[t, 3] +
          xi_pu[1] * Y[t, 8] +
          xi_pu[2] * Y[t, 9];
    }
    Y[3:T, 1] ~ normal(pu_hat[3:T], sigma_pu);
  }
}
generated quantities {
  /* Same decomposition code as yours; copy-paste if still needed.
     Remember to rebuild residuals using the new NAIRU definition. */
}
