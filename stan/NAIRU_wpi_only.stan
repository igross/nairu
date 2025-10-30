// ──────────────────────────────────────────────────────────────────────────────
//  NAIRU model estimated with WPI only (no inflation equation).
//  • Wage equation structure mirrors the ULC block from the baseline model.
//  • Missing final-quarter WPI is inferred jointly with unemployment dynamics.
// ──────────────────────────────────────────────────────────────────────────────

data {
  int<lower=1> T;
  int<lower=1> J;
  matrix[T, J] Y;                    // cols: WPI, LUR, PIE, dummy3, dummy4
  int<lower=0, upper=1> wage_obs[T];
  int<lower=0, upper=T> missing_wage_index;
}

parameters {
  vector[T] NAIRU;
  real<lower = 4.9,  upper = 6.1>        nhat_init;
  vector<lower = -5,   upper = 7>[5]     wage_init;
  real<lower = 0,   upper = 0.40>        tau;
  real<lower = -2.3, upper = 3.7>        wage_missing;

  real<lower = -0.30, upper = 0.90>      delta_wage_0;
  real<lower = -5,   upper = 1>          gamma_wage_0;
  vector<lower = -4,   upper = 2>[2]     gamma_wage_lag;
  real<lower = -6,   upper = 0>          lambda_wage_0;
  vector<lower = -9,   upper = 9>[2]     xi_wage;
  real<lower = 0,    upper = 4.17>       eps_wage;
}

model {
  for (k in 1:2) {
    gamma_wage_lag[k] ~ normal(pow(0.5, k) * -2, 1);
  }

  // Prior mean uses mean(DLWPI / PIE_RBAQ) from docs/est_data.csv ≈ 1.2336.
  delta_wage_0  ~ normal(1.2336 , 0.10);
  gamma_wage_0  ~ normal(-2   , 1.00);
  lambda_wage_0 ~ normal(-3   , 1.00);
  xi_wage       ~ normal(0    , 3);
  eps_wage      ~ normal(2    , 1.00);

  tau          ~ normal(0.05 , 0.02);
  nhat_init    ~ normal(5.5  , 0.2);
  wage_init    ~ normal(1    , 2);
  wage_missing ~ normal(0.7  , 1);

  {
    vector[T] nairu_hat;
    vector[T] wage_hat;

    nairu_hat[1] = nhat_init;
    wage_hat[1:5] = wage_init;

    for (t in 2:T) nairu_hat[t] = NAIRU[t-1];

    for (t in 6:T) {
      real exp_now  = delta_wage_0 * Y[t, 3];
      real ugap_now = gamma_wage_0 * (1 - NAIRU[t] / Y[t, 2]);
      real mom_now  = lambda_wage_0 * (Y[t-1, 2] - Y[t-2, 2]) / Y[t, 2];

      for (k in 1:2) {
        ugap_now += gamma_wage_lag[k] * (1 - NAIRU[t-k] / Y[t-k, 2]);
      }

      wage_hat[t] = exp_now + ugap_now + mom_now
                   + xi_wage[1] * Y[t, 4] + xi_wage[2] * Y[t, 5];
    }

    target += normal_lpdf(NAIRU | nairu_hat, tau);

    for (t in 1:T) {
      if (wage_obs[t] == 1) {
        target += normal_lpdf(Y[t, 1] | wage_hat[t], eps_wage);
      }
    }

    if (missing_wage_index > 0) {
      target += normal_lpdf(wage_missing | wage_hat[missing_wage_index], eps_wage);
    }
  }
}
