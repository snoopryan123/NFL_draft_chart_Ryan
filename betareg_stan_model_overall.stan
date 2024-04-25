data {
  int<lower=0> n;                      // number of datapoint
  int<lower=0> m;                      // number of columns in the draft_pick basis matrix
  real x[n];                           // draft pick x values 
  real xmat[n,m];                      // basis matrix of draft pick x values 
  int<lower=0,upper=1> bust[n];        // vector of `bust` indicator values
  real<lower=0,upper=1> y[n];          // vector of response values `apy_cap_pct_2C`

  int<lower=0> n_new;                      
  int<lower=0> m_new;         
  real x_new[n_new];        
  real xmat_new[n_new,m_new];        
}
parameters {
  real alpha_0;
  real alpha_1;
  //real alpha[m];    
  real beta[m];  
  real gamma_0;
  real gamma_1;
  //real gamma[m]; 
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
    bust_linpred[i] = alpha_0 + alpha_1*x[i];
    //bust_linpred[i] = dot_product(xmat[i,:], alpha);
    mu_linpred[i] = dot_product(xmat[i,:], beta);
    phi_linpred[i] = gamma_0 + gamma_1*x[i];
    //phi_linpred[i] = dot_product(xmat[i,:], gamma);
    
    mu[i] = inv_logit(mu_linpred[i]);
    phi[i] = exp(phi_linpred[i]);
    shape1[i] = mu[i]*phi[i];
    shape2[i] = (1-mu[i])*phi[i];
  }
}
model {
  // priors on top-level parameters
  alpha_0 ~ normal(0,10);
  alpha_1 ~ normal(0,10);
  beta ~ normal(0,10);
  gamma_0 ~ normal(0,10);
  gamma_1 ~ normal(0,10);
  
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
    bust_prob_new[i] = inv_logit(alpha_0 + alpha_1*x[i]);
    mu_new[i] = inv_logit(dot_product(xmat_new[i,:], beta));
    phi_new[i] = exp(gamma_0 + gamma_1*x_new[i]);
    //phi_new[i] = exp(dot_product(xmat_new[i,:], gamma));
  }
}




