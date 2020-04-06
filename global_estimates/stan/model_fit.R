functions {
  real[] ode_model(real t, real[] y, real[] theta, real[] y_r, int[] y_i) {
    real beta_t;
    real mu_t;
    real dN_tests;
    real dydt[9];
    real S;
    real E;
    real I_a;
    real I_p;
    real I_su;
    real C;
    real R_s;
    real I_sk;
    real R_n;
    real beta_bar;
    real b_1;
    real b_2;
    real chi;
    real theta_a;
    real nu;
    real gamma_a;
    real gamma_prop;
    real gamma_s;
    real t_mu;
    real tau;
    real mu;
    real theta_p;
    real phi;
    real N;
    S = y[1]; 
    E = y[2];
    I_a = y[3];
    I_p = y[4]; 
    I_su = y[5];
    C = y[6];
    R_s = y[7];
    I_sk = y[8];
    R_n = y[9];
    beta_bar = theta[1];
    b_1 = theta[2];
    b_2 = theta[3];
    tau = theta[4];
    chi = theta[5];
    nu = theta[6];
    gamma_a = theta[7];
    gamma_prop = theta[8];
    gamma_s = theta[9];
    t_mu = theta[10];
    mu = theta[11];
    theta_a = theta[12];
    theta_p = theta[13];
    phi = theta[14];
    N = theta[15];
    //beta_t = (t < t_mu) ? beta_bar :  beta_bar * exp(- b_2 * (t - t_mu));
    beta_t = beta_bar * (1.0 - b_1/(1 + exp(-b_2 * (t - tau))));
    mu_t = (t < t_mu) ? 0 : mu;
    dN_tests = -1.67487+13.8278*t-6.26078*t^2+0.841799*t^3-0.0438224*t^4+0.000838706*t^5;
    //dSdt
    dydt[1] = -beta_t * ((theta_a * I_a + theta_p * I_p + I_sk + I_su) / N) * S;
    //dEdt
    dydt[2] = beta_t * ((theta_a * I_a + theta_p * I_p + I_sk + I_su) / N) * S - nu * E;
    //d(I_a)dt
    dydt[3] = chi * nu * E - gamma_a * I_a - dN_tests / (S + E + I_a + I_p + C) * I_a;
    //d(I_p)dt
    dydt[4] = (1 - chi) * nu * E - gamma_prop * I_p - dN_tests / (S + E + I_a + I_p + C) * I_p;   //d(I_su)dt
    //d(I_su)dt
    dydt[5] = (1 - phi) * gamma_prop * I_p - gamma_s * I_su - mu_t * I_su;
    //dCdt
    dydt[6] =  gamma_a * I_a + gamma_s * I_sk + gamma_s * I_su;
    //d(R_s)dt
    dydt[7] =  mu_t * I_sk + mu_t * I_su;
    //d(I_sk)dt
    dydt[8] =  phi * gamma_prop * I_p - gamma_s * I_sk - mu_t * I_sk; 
    //d(R_n)dt
    dydt[9] =  dN_tests / (S + E + I_a + I_p + C) * I_a + dN_tests / (S + E + I_a + I_p + C) * I_p;
    return dydt;
  }
  
  real dN_tests_fun(real t) 
  {
    return(-1.67487+13.8278*t-6.26078*t^2+0.841799*t^3-0.0438224*t^4+0.000838706*t^5);
  }
  
}
data {
  int<lower=1> t; // number of timepoints
  int<lower=0> y_obs_1[t]; 
  int<lower=0> y_obs_2[t];
  real t0; // Initial time value = 0
  real y0[9]; // Initial values for each of the 9 compartments
  real ts[t]; // Just 1:t
  real nu; // 1/latent period 
  real gamma_a; // 1/infectious period of asymptomatic     
  real gamma_prop; // 1/infectious period of presymptomatic 
  real gamma_s; // 1/infectious period of symptomatic 
  real t_mu; // time after which passive case finding starts
  real mu; // rate of removal after symptom onset through passive case finding
  //real theta_a; // infectiousness of pre-symptomatic relative to symptomatic
  //real theta_p; // infectiousness of pre-symptomatic relative to symptomatic
  real phi; // proportion pre-symptomatic
  int N; // Number of passengers onboard
}
transformed data {
  real y_r[0];
  int y_i[0];
}
parameters {
  real<lower=0, upper = 5> beta_bar; // initial transmission rate
  real<lower=0> b_1; // gradient of transmission rate
  real<lower=0> b_2; // gradient of transmission rate
  real<lower=0> tau;
  real<lower=0, upper = 1> chi; // proportion asymptomatic
  real<lower=0, upper = 1> theta_a; // infectiousness of asymptomatic relative to symptomatic
  real<lower=0, upper = 1> theta_p; // infectiousness of asymptomatic relative to symptomatic
  real<lower = 0> sigma1;
  real<lower = 0> sigma2;
}
model {
  real y_hat[t,9];
  real theta[15];
  real y_est_1[t];
  real y_est_2[t];
  beta_bar ~  normal(2.2, 1);
  b_1 ~ normal(0, 1) T[0,];
  b_2 ~ normal(0, 1) T[0,];
  tau ~ uniform(0, 32) T[0,];
  chi ~ uniform(0, 1);
  theta_a ~ normal(0, 1);
  theta_p ~ normal(0, 1);
  sigma1 ~ cauchy(0, 1) T[0,];
  sigma2 ~ cauchy(0, 1) T[0,];
  theta[1] = beta_bar;
  theta[2] = b_1;
  theta[3] = b_2;
  theta[4] = tau;
  theta[5] = chi;
  theta[6] = nu;
  theta[7] = gamma_a;
  theta[8] = gamma_prop;
  theta[9] = gamma_s;
  theta[10] = t_mu;
  theta[11] = mu;
  theta[12] = theta_a;
  theta[13] = theta_p;
  theta[14] = phi;
  theta[15] = N;
  y_hat = integrate_ode_bdf(ode_model, y0, t0, ts, theta, y_r, y_i);
  for(i in 1:t) {
    y_est_1[i] = phi*gamma_prop*y_hat[i, 4]; // < 0 ? 0 : y_hat[i, 8];
    y_est_2[i] = dN_tests_fun(i) / (y_hat[i, 1] + y_hat[i, 2] + y_hat[i, 3] + y_hat[i, 4] + y_hat[i, 6]) * y_hat[i, 3] + dN_tests_fun(i)/ (y_hat[i, 1] + y_hat[i, 2] + y_hat[i, 3] + y_hat[i, 4] + y_hat[i, 6]) * y_hat[i, 4]; // < 0 ? 0 : y_hat[i, 9];
    target += normal_lpdf(y_obs_1[i] | y_est_1[i], sigma1) +
      normal_lpdf(y_obs_2[i] | y_est_2[i], sigma2);
  }
}