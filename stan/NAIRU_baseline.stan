data {
  int<lower=1> T; // number of observations
  int<lower=1> J; // dimension of observations
  matrix[T, J] Y; // observations
}

transformed data {
  vector[T] Y2_demeaned; // Demeaned Y[:,2]
  vector[T] Y1_demeaned; // Demeaned Y[:,1]

  real Y2_mean = mean(col(Y, 2)); // Compute mean of column 2
  real Y1_mean = mean(col(Y, 1)); // Compute mean of column 1

  for (t in 1:T) {
    Y2_demeaned[t] = Y[t, 2] - Y2_mean; // Demean Y[t,2]
    Y1_demeaned[t] = Y[t, 1] - Y1_mean; // Demean Y[t,1]
  }
}

parameters {
  vector[T] NAIRU;
  vector[1] nhat_init;
  vector[3] pthat_init;
  vector[2] puhat_init;
 
  real ulc_missing;
  
  real<lower = 0> tau; // Variance of unobserved NAIRU series

  // Inflation Equation
  real<lower = 0, upper = 1> delta_pt; // Coefficient on inflation expectations
  real beta1_pt; // Coefficient on lagged inflation
  real beta2_pt; // Coefficient on lagged inflation
  real beta3_pt; // Coefficient on lagged inflation
  real<upper = 0> gamma_pt; // Coefficient on UR gap
  real<upper = 0> lambda_pt; // Coefficient on UR change
  real<lower = 0> eps_pt; // Variance of inflation equation error term
  real alpha_pt; // Coefficient on import prices
  vector[2] xi_pt; // Coefficient on dummies
  real phi_pt; // Constant

  // ULC Equation
  vector[2] beta_pu; // Coefficients on lagged inflation
  real<upper = 0> gamma_pu; // Coefficient on UR gap
  real<upper = 0> lambda_pu; // Coefficient on UR change
  vector[2] xi_pu; // Coefficient on dummies
  real<lower = 0> eps_pu; // Variance of ULC equation error term
}

model {
  // Priors
 
  beta1_pt ~ normal(0.24, 0.5);
  beta2_pt ~ normal(0.16, 0.5);
  beta3_pt ~ normal(0.18, 0.5);
  gamma_pt ~ normal(-0.38, 0.5);
  phi_pt ~ normal(0.06, 0.5);
  lambda_pt ~ normal(-0.7, 0.2);
  alpha_pt ~ normal(0.1, 0.5);
  xi_pt ~ normal(0, 3);
  eps_pt ~ normal(0.30, 0.5);

  beta_pu ~ normal(0.3, 0.1);
  gamma_pu ~ normal(-2, 2);
  lambda_pu ~ normal(-3, 2);
  eps_pu ~ normal(1.17, 2);
  xi_pu ~ normal(0, 3);

  tau ~ normal(0.1, 0.1);

  nhat_init ~ normal(5.5, 0.2);
  pthat_init ~ normal(1.5, 2);
  puhat_init ~ normal(1, 2);
  
  ulc_missing ~ normal(0.7,1);

  {
    vector[T] nairu_hat;
    vector[T] pt_hat;
    vector[T] pu_hat;

    nairu_hat[1] = nhat_init[1];
    pt_hat[1:3] = pthat_init[1:3];
    pu_hat[1:2] = puhat_init[1:2];

    for (t in 2:T) {
      nairu_hat[t] = NAIRU[t - 1];
    }

    for (t in 4:T) {
      pt_hat[t] = (1 - beta1_pt - beta2_pt - beta3_pt) * Y[t, 5]
                + phi_pt * Y1_demeaned[t - 1]
                + beta1_pt * Y[t - 1, 4]
                + beta2_pt * Y[t - 2, 4]
                + beta3_pt * Y[t - 3, 4]
                + gamma_pt * ((Y[t, 3] - NAIRU[t]) / Y[t, 3])
                + alpha_pt * (Y2_demeaned[t - 1] - Y2_demeaned[t - 2])
                + xi_pt[1] * Y[t, 6]
                + xi_pt[2] * Y[t, 7]
                + lambda_pt * (Y[t - 1, 3] - Y[t - 2, 3]) / Y[t, 3];
    }

    for (t in 3:T-1) {
      pu_hat[t] = mean(Y[,1]-Y[,4]) +
                (1 - beta_pu[1] - beta_pu[2]) * Y[t, 5]
                + beta_pu[1] * Y[t - 1, 4]
                + beta_pu[2] * Y[t - 2, 4]
                + gamma_pu * (1 - NAIRU[t] / Y[t, 3])
                + lambda_pu * (Y[t - 1, 3] - Y[t - 2, 3]) / Y[t, 3]
                + xi_pu[1] * Y[t, 8]
                + xi_pu[2] * Y[t, 9];
    }
    
    pu_hat[T] = ulc_missing;

    target += normal_lpdf(NAIRU | nairu_hat, tau);
    target += normal_lpdf(Y[, 4] | pt_hat, eps_pt);
    target += normal_lpdf(Y[, 1] | pu_hat, eps_pu);
  }
}




generated quantities {
  vector[T] pt_lags;
  vector[T] pt_dummies;
  vector[T] pt_unemploymentgap;
  vector[T] pt_momentum;
  vector[T] pt_expectations;
  vector[T] pt_import_prices;
  vector[T] pu_lags;
  vector[T] pu_dummies;
  vector[T] pu_momentum;
  vector[T] pu_unemploymentgap;
  vector[T] pu_expectations;
  vector[T] n_residuals;
  vector[T] pt_residuals;
  vector[T] pu_residuals;
 

  for (t in 4:T) {
    pt_lags[t] = beta1_pt * Y[t - 1, 4]
               + beta2_pt * Y[t - 2, 4]
               + beta3_pt * Y[t - 3, 4];

    pt_dummies[t] = xi_pt[1] * Y[t, 6]
                  + xi_pt[2] * Y[t, 7];

    pt_unemploymentgap[t] = gamma_pt * ((Y[t, 3] - NAIRU[t]) / Y[t, 3]);

    pt_momentum[t] = lambda_pt * (Y[t - 1, 3] - Y[t - 2, 3]) / Y[t, 3];

    pt_expectations[t] = (1 - beta1_pt - beta2_pt - beta3_pt) * Y[t, 5];

    pt_import_prices[t] = alpha_pt * (Y2_demeaned[t - 1] - Y2_demeaned[t - 2]);


    pt_residuals[t] = Y[t, 4] - (pt_lags[t] + pt_dummies[t] + pt_unemploymentgap[t] + pt_momentum[t] + pt_expectations[t] + pt_import_prices[t]);
  }

  for (t in 3:T) {
    pu_lags[t] = beta_pu[1] * Y[t - 1, 4]
               + beta_pu[2] * Y[t - 2, 4];

    pu_dummies[t] = xi_pu[1] * Y[t, 8]
                  + xi_pu[2] * Y[t, 9];

    pu_unemploymentgap[t] = gamma_pu * (1 - NAIRU[t] / Y[t, 3]);

    pu_momentum[t] = lambda_pu * (Y[t - 1, 3] - Y[t - 2, 3]) / Y[t, 3];

    pu_expectations[t] = (1 - beta_pu[1] - beta_pu[2]) * Y[t, 5];

    pu_residuals[t] = Y[t, 1] - (pu_lags[t] + pu_dummies[t] + pu_unemploymentgap[t] + pu_momentum[t] + pu_expectations[t]);
  }

  for (t in 1:T) {
    n_residuals[t] = NAIRU[t] - (t > 1 ? NAIRU[t - 1] : nhat_init[1]);
  }
}
