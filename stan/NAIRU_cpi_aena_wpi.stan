// ──────────────────────────────────────────────────────────────────────────────
//  NAIRU model with two wage-growth series (AENA & WPI) replacing ULC.
//  • Inflation equation uses AENA (series 1) as the wage-cost input.
//  • Wage block estimates separate observation equations for AENA and WPI.
//  • Missing final-quarter observations for either wage are inferred jointly.
// ──────────────────────────────────────────────────────────────────────────────

data {
  int<lower=1> T;
  int<lower=1> J;
  matrix[T, J] Y;                      // cols: AENA, WPI, import, LUR, CPI, PIE, d1, d2, d3, d4
  int<lower=0, upper=1> wage_obs[T, 2];
  int<lower=0, upper=T> missing_wage_index[2];
}

transformed data {
  vector[T] import_demeaned;
  vector[T] wage1_demeaned;
  vector[T] wage2_demeaned;
  real import_mean = 0;
  real exp_mean = 0;
  real wage_sum[2];
  int wage_count[2];
  real wage_prior_mean[2];

  for (j in 1:2) {
    wage_sum[j] = 0;
    wage_count[j] = 0;
  }

  for (t in 1:T) {
    import_mean += Y[t, 3];
    exp_mean += Y[t, 6];
    for (j in 1:2) {
      if (wage_obs[t, j] == 1) {
        wage_sum[j] += Y[t, j];
        wage_count[j] += 1;
      }
    }
  }

  import_mean /= T;
  exp_mean /= T;

  for (j in 1:2) {
    if (wage_count[j] > 0) {
      wage_sum[j] /= wage_count[j];
    } else {
      wage_sum[j] = 0;
    }
  }

  for (j in 1:2) {
    if (wage_sum[j] != 0) {
      wage_prior_mean[j] = exp_mean / wage_sum[j];
    } else {
      wage_prior_mean[j] = 0;
    }
  }


  for (t in 1:T) {
    import_demeaned[t] = Y[t, 3] - import_mean;
    if (wage_obs[t, 1] == 1) {
      wage1_demeaned[t] = Y[t, 1] - wage_sum[1];
    } else {
      wage1_demeaned[t] = 0;
    }
    if (wage_obs[t, 2] == 1) {
      wage2_demeaned[t] = Y[t, 2] - wage_sum[2];
    } else {
      wage2_demeaned[t] = 0;
    }
  }
}

parameters {
  vector[T] NAIRU;
  real<lower = 4.9,  upper = 6.1>        nhat_init;
  vector<lower = -4.5, upper = 7.5>[7]   pthat_init;
  vector<lower = -5,   upper = 7>[5]     wage1_init;
  vector<lower = -5,   upper = 7>[5]     wage2_init;
  real<lower = 0,   upper = 0.40>        tau;
  vector<lower = -2.3, upper = 3.7>[2]   wage_missing;

  // Inflation equation
  real<lower = 0,    upper = 2>          delta_pt_0;
  real<lower = -0.54, upper = 0.66>      phi_pt_0;
  vector<lower = -0.60, upper = 0.60>[4] phi_pt_lag;
  real<lower = -0.98, upper = 0.22>      gamma_pt_0;
  vector<lower = -1.18, upper = 0.02>[4] gamma_pt_lag;
  real<lower = -1.30, upper = -0.10>     lambda_pt_0;
  real<lower = -0.50, upper = 0.70>      alpha_pt_0;
  vector<lower = -0.60, upper = 0.60>[4] alpha_pt_lag;
  vector<lower = -9,   upper = 9>[2]     xi_pt;
  real<lower = 0,    upper = 0.90>       eps_pt;

  // Wage equation for AENA
  real<lower = -0.30, upper = 0.90>      delta_wage1_0;
  real<lower = -5,   upper = 1>          gamma_wage1_0;
  vector<lower = -4,   upper = 2>[2]     gamma_wage1_lag;
  real<lower = -6,   upper = 0>          lambda_wage1_0;
  vector<lower = -9,   upper = 9>[2]     xi_wage1;
  real<lower = 0,    upper = 4.17>       eps_wage1;

  // Wage equation for WPI
  real<lower = -0.30, upper = 0.90>      delta_wage2_0;
  real<lower = -5,   upper = 1>          gamma_wage2_0;
  vector<lower = -4,   upper = 2>[2]     gamma_wage2_lag;
  real<lower = -6,   upper = 0>          lambda_wage2_0;
  vector<lower = -9,   upper = 9>[2]     xi_wage2;
  real<lower = 0,    upper = 4.17>       eps_wage2;
}

model {
  for (k in 1:4) {
    phi_pt_lag[k]   ~ normal(pow(0.5, k) * 0.06 , 0.50);
    gamma_pt_lag[k] ~ normal(pow(0.7, k) * -0.38, 0.50);
    alpha_pt_lag[k] ~ normal(pow(0.5, k) * 0.10 , 0.50);
  }
  for (k in 1:2) {
    gamma_wage1_lag[k] ~ normal(pow(0.5, k) * -2, 1);
    gamma_wage2_lag[k] ~ normal(pow(0.5, k) * -2, 1);
  }

  delta_pt_0  ~ normal(1, 0.1);
  phi_pt_0    ~ normal(0.06 , 0.50);
  gamma_pt_0  ~ normal(-0.38, 0.50);
  lambda_pt_0 ~ normal(-0.70, 0.50);
  alpha_pt_0  ~ normal(0.10 , 0.50);
  xi_pt       ~ normal(0    , 3);
  eps_pt      ~ normal(0.30 , 0.50);

  delta_wage1_0  ~ normal(wage_prior_mean[1], 0.50);
  gamma_wage1_0  ~ normal(-2   , 1.00);
  lambda_wage1_0 ~ normal(-3   , 1.00);
  xi_wage1       ~ normal(0    , 3);
  eps_wage1      ~ normal(2    , 1.00);

  delta_wage2_0  ~ normal(wage_prior_mean[2], 0.50);
  gamma_wage2_0  ~ normal(-2   , 1.00);
  lambda_wage2_0 ~ normal(-3   , 1.00);
  xi_wage2       ~ normal(0    , 3);
  eps_wage2      ~ normal(2    , 1.00);

  tau         ~ normal(0.05 , 0.02);

  nhat_init   ~ normal(5.5  , 0.2);
  pthat_init  ~ normal(1.5  , 2);
  wage1_init  ~ normal(1    , 2);
  wage2_init  ~ normal(1    , 2);
  wage_missing ~ normal(0.7 , 1);

  {
    vector[T] nairu_hat;
    vector[T] pt_hat;
    vector[T] wage1_hat;
    vector[T] wage2_hat;

    nairu_hat[1] = nhat_init;
    pt_hat[1:7]  = pthat_init;
    wage1_hat[1:5] = wage1_init;
    wage2_hat[1:5] = wage2_init;

    for (t in 2:T) nairu_hat[t] = NAIRU[t-1];

    for (t in 8:T) {
      real exp_now  = delta_pt_0 * Y[t, 6];
      real ugap_now = gamma_pt_0 * ((Y[t, 4] - NAIRU[t]) / Y[t, 4]);
      real mom_now  = lambda_pt_0 * (Y[t-1, 4] - Y[t-2, 4]) / Y[t, 4];
      real imp_now  = alpha_pt_0 * (import_demeaned[t-1] - import_demeaned[t-2]);
      real wage_now = phi_pt_0 * wage1_demeaned[t-1];

      for (k in 1:4) {
        ugap_now += gamma_pt_lag[k] * ((Y[t-k, 4] - NAIRU[t-k]) / Y[t-k, 4]);
        imp_now  += alpha_pt_lag[k] * (import_demeaned[t-1-k] - import_demeaned[t-2-k]);
        wage_now += phi_pt_lag[k]   * wage1_demeaned[t-1-k];
      }

      pt_hat[t] = exp_now + ugap_now + mom_now + imp_now + wage_now
                + xi_pt[1] * Y[t, 7] + xi_pt[2] * Y[t, 8];
    }

    for (t in 6:T) {
      real exp1  = delta_wage1_0 * Y[t, 6];
      real ugap1 = gamma_wage1_0 * (1 - NAIRU[t] / Y[t, 4]);
      real mom1  = lambda_wage1_0 * (Y[t-1, 4] - Y[t-2, 4]) / Y[t, 4];

      real exp2  = delta_wage2_0 * Y[t, 6];
      real ugap2 = gamma_wage2_0 * (1 - NAIRU[t] / Y[t, 4]);
      real mom2  = lambda_wage2_0 * (Y[t-1, 4] - Y[t-2, 4]) / Y[t, 4];

      for (k in 1:2) {
        ugap1 += gamma_wage1_lag[k] * (1 - NAIRU[t-k] / Y[t-k, 4]);
        ugap2 += gamma_wage2_lag[k] * (1 - NAIRU[t-k] / Y[t-k, 4]);
      }

      wage1_hat[t] = exp1 + ugap1 + mom1
                   + xi_wage1[1] * Y[t, 9] + xi_wage1[2] * Y[t, 10];

      wage2_hat[t] = exp2 + ugap2 + mom2
                   + xi_wage2[1] * Y[t, 9] + xi_wage2[2] * Y[t, 10];
    }

    target += normal_lpdf(NAIRU | nairu_hat, tau);
    target += normal_lpdf(Y[, 5] | pt_hat, eps_pt);

    for (t in 1:T) {
      if (wage_obs[t, 1] == 1) {
        target += normal_lpdf(Y[t, 1] | wage1_hat[t], eps_wage1);
      }
      if (wage_obs[t, 2] == 1) {
        target += normal_lpdf(Y[t, 2] | wage2_hat[t], eps_wage2);
      }
    }

    if (missing_wage_index[1] > 0) {
      target += normal_lpdf(wage_missing[1] | wage1_hat[missing_wage_index[1]], eps_wage1);
    }
    if (missing_wage_index[2] > 0) {
      target += normal_lpdf(wage_missing[2] | wage2_hat[missing_wage_index[2]], eps_wage2);
    }
  }
}
