
# Reproduce the analysis from our paper:
1. `A0_market_trade_value.R` -- fit the Weibull trade market value curve 
2. `A2ndContract2_ReplicateAndEDA.R` -- replication of Massey Thaler curves and EDA
3. `A2ndContract3_betaregOverallStan.R` -- fit the P(Y|x) position-agnostic beta regression model in `betareg_stan_model_overall.stan` and create associated value charts
4. `A2ndContract4_betaregByposStan.R` -- fit the P(Y|x,pos) position-specific beta regression model in `betareg_stan_model_bypos.stan` and create associated value charts
