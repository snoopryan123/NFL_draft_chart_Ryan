data {
  int<lower=0> n;                          // number of datapoints
  int<lower=0> m;                          // number of columns in the draft_pick basis matrix
  int<lower=0> num_pos;                    // number of positions
  real x[n];                               // draft pick x values 
  real xmat[n,m];                          // basis matrix of draft pick x values 
  int<lower=1,upper=num_pos> pos_idx[n];   // vector of position indices
  int<lower=0,upper=1> bust[n];            // vector of `bust` indicator values
  real<lower=0,upper=1> y[n];              // vector of response values `apy_cap_pct_2C`

  int<lower=0> n_new;                      
  real x_new[n_new];        
  real xmat_new[n_new,m];        
  int<lower=1,upper=num_pos> pos_idx_new[n_new];      
}
parameters {
  // positional parameters
  real alpha_0[num_pos];
  real alpha_1[num_pos];
  real beta[num_pos,m];  
  real gamma_0[num_pos];
  real gamma_1[num_pos];
  
  // top-level positionless mean parameters
  real alpha_0H;
  real alpha_1H;
  real beta_H[m];  
  real gamma_0H;
  real gamma_1H;
  
  // top-level positionless variance parameters
  real<lower=0> tau_alpha_0H;
  real<lower=0> tau_alpha_1H;
  real<lower=0> tau_beta_H[m];  
  real<lower=0> tau_gamma_0H;
  real<lower=0> tau_gamma_1H;
}
transformed parameters {
  real bust_linpred[n];
  real mu_linpred[n];
  real phi_linpred[n];
  real<lower=0, upper=1> mu[n];
  real<lower=0> phi[n];
  real<lower=0> shape1[n];
  real<lower=0> shape2[n];

  for (i in 1:n) {
    bust_linpred[i] = alpha_0[pos_idx[i]] + alpha_1[pos_idx[i]]*x[i];
    mu_linpred[i] = dot_product(xmat[i,:], beta[pos_idx[i],:]);
    phi_linpred[i] = gamma_0[pos_idx[i]] + gamma_1[pos_idx[i]]*x[i];

    mu[i] = inv_logit(mu_linpred[i]);
    phi[i] = exp(phi_linpred[i]);
    shape1[i] = mu[i]*phi[i];
    shape2[i] = (1-mu[i])*phi[i];
  }
  
}
model {
  // priors on top-level mean parameters
  alpha_0H ~ normal(0, 10);
  alpha_1H ~ normal(0, 10);
  beta_H ~ normal(0, 10);
  gamma_0H ~ normal(0, 10);
  gamma_1H ~ normal(0, 10);
  
  // priors on top-level variance parameters
  tau_alpha_0H ~ std_normal();
  tau_alpha_1H ~ std_normal();
  tau_beta_H   ~ std_normal();
  tau_gamma_0H ~ std_normal();
  tau_gamma_1H ~ std_normal();
  
  // priors on position-level parameters
  for (k in 1:num_pos) {
    alpha_0[k] ~ normal(alpha_0H, tau_alpha_0H);
    alpha_1[k] ~ normal(alpha_1H, tau_alpha_1H);
    beta[k]    ~ normal(beta_H, tau_beta_H);
    gamma_0[k] ~ normal(gamma_0H, tau_gamma_0H);
    gamma_1[k] ~ normal(gamma_1H, tau_gamma_1H);
  }
  
  // likelihood
  for (i in 1:n) {
    bust[i] ~ bernoulli_logit(bust_linpred[i]);
    if (bust[i] == 0) {  // not a bust, in the beta distributed right tail 
      y[i] ~ beta(shape1[i], shape2[i]);
    }
  }
}
generated quantities {
  real<lower=0, upper=1> bust_prob_new[n_new];
  real<lower=0, upper=1> mu_new[n_new];
  real<lower=0> phi_new[n_new];

  for (i in 1:n_new) {
    bust_prob_new[i] = inv_logit(alpha_0[pos_idx_new[i]] + alpha_1[pos_idx_new[i]]*x_new[i]);
    mu_new[i] = inv_logit(dot_product(xmat_new[i,:], beta[pos_idx_new[i],:]));
    phi_new[i] = exp(gamma_0[pos_idx_new[i]] + gamma_1[pos_idx_new[i]]*x_new[i]);
  }
}




