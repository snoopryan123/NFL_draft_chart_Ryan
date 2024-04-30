
### analysis using just 2013+
post2013 = FALSE
post2013_str = if (post2013) "_post2013" else ""

##################
### load stuff ###
##################

library(tidyverse)
library(minpack.lm)

### plotting pre-sets
theme_set(theme_bw())
theme_update(
  text = element_text(size=20),
  plot.title = element_text(hjust = 0.5),
  axis.title = element_text(size=20),
  axis.text = element_text(size=20),
  legend.text = element_text(size=20),
  legend.title = element_text(size=20),
  panel.spacing = unit(2, "lines")
) 

### read trade market dataframe
df0 = read_csv("data_draft_trades_m24_Chris.csv", show_col_types = F)
if (post2013) {
  df0 = df0 %>% filter(year >= 2013)
}
names(df0)
years_str = paste0(min(df0$year),"-",max(df0$year))

#################
### functions ###
#################

### keep certain years years
table(df0$year)
years = sort(unique(df0$year)) ### keep all years
years

### function f for nonlinear regression
f <- function(lambda=0.146, beta=0.698, rho=0, df=df0, discount=FALSE, response=FALSE, years=NULL) {
  # browser()
  
  # assertthat::assert_that(lambda > 0)
  # assertthat::assert_that(beta > 0)
  # assertthat::assert_that(rho >= 0)
  
  ### dataframe to be used in this function
  if (!is.null(years)) {
    df = df %>% filter(year %in% years)
  } 
  
  ### draft pick numbers for the two teams
  t_L = df[,names(df)[str_detect(names(df), "up_pick") & str_detect(names(df), "_number")]]
  t_H = df[,names(df)[str_detect(names(df), "down_pick") & str_detect(names(df), "_number")]]
  
  ### separate out the first pick for team H
  t_H1 = t_H[,1]
  t_H2 = t_H[,2:ncol(t_H)]
  
  if (response) {
    ### return the response variable
    # return( t_H1[[1]] )
    return( log(t_H1)[[1]] )
  }
  
  ### replace NA with Infinity (which becomes 0 in the weibull sum)
  t_L_inf = t_L %>% replace(is.na(.), Inf)
  t_H2_inf = t_H2 %>% replace(is.na(.), Inf)
  assertthat::assert_that(sum(is.na(t_H1)) == 0)
  
  ### weibull values
  weibull_L = exp(-lambda*(t_L_inf - 1)^beta)
  weibull_H2 = exp(-lambda*(t_H2_inf - 1)^beta)
  
  # browser()
  ### num years in the future of each pick
  n_L = df[names(df)[str_detect(names(df), "up_pick") & str_detect(names(df), "next_year")]]
  n_H = df[names(df)[str_detect(names(df), "down_pick") & str_detect(names(df), "next_year")]]
  
  ### separate out the first pick for team H
  n_H1 = n_H[,1]
  n_H2 = n_H[,2:ncol(n_H)]
  
  ### replace NA with 0 (which becomes 1 in the discount factor)
  n_L_0 = n_L %>% replace(is.na(.), 0)
  n_H2_0 = n_H2 %>% replace(is.na(.), 0)
  assertthat::assert_that(sum(is.na(n_H1)) == 0)
  
  ### discount factors
  if (discount) {
    dfactor_L = (1 + rho)^n_L_0
    dfactor_H1 = (1 + rho)^n_H1
    dfactor_H1 = dfactor_H1[[1]]
    dfactor_H2 = (1 + rho)^n_H2_0
  } else {
    dfactor_L = 1
    dfactor_H2 = 1
    dfactor_H1 = 1
  }
  
  ### calculate and return f
  assertthat::assert_that(all(dim(weibull_L) == dim(dfactor_L)))
  assertthat::assert_that(all(dim(weibull_H2) == dim(dfactor_H2)))
  # weib_sum = rowSums(weibull_L/dfactor_L) - rowSums(weibull_H2/dfactor_H2)
  weib_sum = rowSums(weibull_L*dfactor_L) - rowSums(weibull_H2*dfactor_H2)
  
  # browser()
  assertthat::assert_that( all(dfactor_H1==1) | length(dfactor_H1) == length(weib_sum) )
  # d_weib_sum = dfactor_H1 * weib_sum
  d_weib_sum = weib_sum / dfactor_H1
  sum(d_weib_sum < 0)
  d_weib_sum_trunc = ifelse(d_weib_sum > 0, d_weib_sum, 1e-5)
  # log_d_weib_sum = -1/lambda * log(d_weib_sum)
  log_d_weib_sum = -1/lambda * log(d_weib_sum_trunc)
  log_d_weib_sum
  
  # browser()
  log_d_weib_sum_1beta = (log_d_weib_sum)^(1/beta) + 1
  f_RHS = log(log_d_weib_sum_1beta)
  #### f_RHS[is.na(f_RHS)] = 0 #FIXME
  f_RHS
}

### checks
# f(0.09, 0.8, 0, discount = F)
# f(0.09, 0.8, 0, discount = T)
# f(0.09, 0.8, 0.3, discount = T)
# f(0.09, 0.8, 1, discount = T)
# f(0.09, 0.8, 1.2, discount = T)
# f(0.09, 0.8, 1.4, discount = T)
# f(0.09, 0.8, 8, discount = T)

### value of the t^th pick relative to the first pick
v <- function(t, beta, lambda, rho=0, n=0) {
  exp(-lambda*(t-1)^beta) * (1+rho)^n
}

##########################
### Fit the parameters ###
##########################

OVERWRITE_PARAMS_DF = TRUE
# OVERWRITE_PARAMS_DF = FALSE
# TRACE_ME = TRUE
TRACE_ME = FALSE
BOOT_ME = TRUE
# BOOT_ME = FALSE
B = 100

params_df_filename = paste0("data_market_curve_weibull_params",post2013_str,".csv")
if (file.exists(params_df_filename) & !OVERWRITE_PARAMS_DF) {
  params_df = read_csv(params_df_filename)
} else {
  if (BOOT_ME) {
    bs = 0:B
  } else {
    bs = 0
  }
  betas_m1 = numeric(length(bs))
  lambdas_m1 = numeric(length(bs))
  betas_m2 = numeric(length(bs))
  lambdas_m2 = numeric(length(bs))
  rhos_m2 = numeric(length(bs))
  for (b in bs) {
    print(paste0("b=",b,"/B=",B))
    
    if (b != 0) {
      rows_b = sort(sample(1:nrow(df0), size=nrow(df0), replace=TRUE))
      df_b = df0[rows_b,]
    } else {
      df_b = df0
    }
    y_b = f(response=T, years = years, df=df_b)
    
    ### first model: no discount factor
    m1 <- nlsLM(
      y_b ~ f(lambda, beta, rho=0, df=df_b, discount = FALSE, response = FALSE, years = years), 
      start = c(lambda=0.146, beta=0.698), 
      lower = c(lambda=0, beta=0), 
      algorithm = "LM",
      trace = TRACE_ME,
      control = list(maxiter = 100)
    )
    m1
    sm1 = summary(m1)
    sm1_coeffs = sm1$coefficients
    beta_m1 = sm1_coeffs["beta","Estimate"]
    lambda_m1 = sm1_coeffs["lambda","Estimate"]
    betas_m1[b+1] = beta_m1
    lambdas_m1[b+1] = lambda_m1
    rhos_m1 = 0
    
    ### second model: no discount factor
    m2 <- nlsLM(
      y_b ~ f(lambda, beta, rho, df=df_b, discount = TRUE, response = FALSE), 
      start = c(lambda=0.0996, beta=0.745, rho=1), 
      # lower = c(lambda=0, beta=0, rho=0), 
      lower = c(lambda=0, beta=0, rho=-1), 
      # upper = c(lambda=Inf,beta=Inf,rho=Inf),
      algorithm = "LM",
      trace = TRACE_ME,
      control = list(maxiter = 100)
    )
    m2
    sm2 = summary(m2)
    sm2_coeffs = sm2$coefficients
    beta_m2 = sm2_coeffs["beta","Estimate"]
    lambda_m2 = sm2_coeffs["lambda","Estimate"]
    rho_m2 = sm2_coeffs["rho","Estimate"]
    betas_m2[b+1] = beta_m2
    lambdas_m2[b+1] = lambda_m2
    rhos_m2[b+1] = rho_m2
  }
  params_df = 
    tibble(b = bs, m=1, lambda=lambdas_m1, beta = betas_m1, rho = rhos_m1) %>%
    bind_rows(tibble(b = bs, m=2, lambda=lambdas_m2, beta = betas_m2, rho = rhos_m2))
  params_df
  write_csv(params_df, params_df_filename)
}
params_df

#################
### Visualize ###
#################

### confidence intervals of the parameters
df_params_CI = 
  params_df %>%
  group_by(m) %>%
  mutate(
    lambda_0 = first(lambda),
    beta_0 = first(beta),
    rho_0 = first(rho),
  ) %>%
  summarise(
    lambda_L = quantile(lambda, 0.05),
    lambda = unique(lambda_0),
    lambda_U = quantile(lambda, 0.95),
    beta_L = quantile(beta, 0.05),
    beta = unique(beta_0),
    beta_U = quantile(beta, 0.95),
    rho_L = quantile(rho, 0.05, na.rm=T),
    rho = unique(rho_0),
    rho_U = quantile(rho, 0.95, na.rm=T),
    .groups = "drop"
  )
df_params_CI

### bootstrapped trade market curves
df_market_curve_CI0 = tibble()
for (b_ in unique(params_df$b)) {
  for (m_ in unique(params_df$m)) {
    print(paste0("b=",b_,", m=",m_))
    
    params_df_bm = params_df %>% filter(b==b_, m==m_)
    params_df_bm
    
    df_plot_market_curves_bm = 
      tibble(
        b = b_, m = m_, t = 1:256,
        v = v(t, beta=params_df_bm$beta[1], lambda=params_df_bm$lambda[1]),
      ) 
    df_plot_market_curves_bm
    df_market_curve_CI0 = bind_rows(df_market_curve_CI0, df_plot_market_curves_bm)
  }
}
df_market_curve_CI0

### confidence intervals of the market curves
df_market_curve_CIs = 
  df_market_curve_CI0 %>%
  arrange(m,t,b) %>%
  group_by(m,t) %>%
  # mutate(v_0 = first(v)) %>%
  summarise(
    v = quantile(v, probs=c(0.025, 0.50, 0.975)),
    x = c("L", "M", "U"),
    .groups = "drop"
  ) %>%
  pivot_wider(values_from = "v", names_from = "x", names_prefix = "v_") %>%
  mutate(m=as.character(m)) %>%
  mutate(
    m = ifelse(m=="1", paste0("Ours (without\ndiscount factor,\n",years_str,")\n"), m),
    m = ifelse(m=="2", paste0("Ours (with\ndiscount factor,\n",years_str,")\n"), m),
  ) 
df_market_curve_CIs
write_csv(df_market_curve_CIs, paste0("data_market_curves_weibull",post2013_str,".csv"))

df_market_curves_OG = 
  tibble(
    t = 1:256,
    `Massey Thaler\n2013 (1983-2008)\n` = v(t, beta=0.764, lambda=0.0994),
    `Massey Thaler\n2013 (2001-2008)\n` = v(t, beta=0.698, lambda=0.146),
  ) %>%
  pivot_longer(-t, names_to="m", values_to="v")
df_market_curves_OG

#################
### Visualize ###
#################

plot_market_curves_boot = 
  df_market_curve_CIs %>%
  mutate(m = str_replace(m, "\\)\n", "\\)")) %>%
  ggplot(aes(x=t)) +
  facet_wrap(~ m) +
  geom_line(aes(y=v_M), linewidth=1, linetype="solid") +
  geom_line(aes(y=v_L), linewidth=1, linetype="dashed") +
  geom_line(aes(y=v_U), linewidth=1, linetype="dashed") +
  scale_x_continuous(breaks=seq(1,32*9,by=32)) +
  ylab("Estimated value\nrelative to No. 1 pick") +
  xlab("Draft pick") +
  labs(title = "Trade market curves")
# plot_market_curves_boot
ggsave(paste0("plots_weibull/plot_market_curves_boot",post2013_str,".png"),plot_market_curves_boot,width=11,height=5)

df_plot_market_curves = 
  bind_rows(
    df_market_curve_CIs %>% select(-v_L, v_U) %>% rename(v = v_M), 
    df_market_curves_OG
  )

plot_market_curves = 
  df_plot_market_curves %>%
  # filter(m==1) %>%
  # mutate(
  #   m = ifelse(m=="1", "Ours (without\ndiscount factor,\n2006-2023)\n", m),
  #   m = ifelse(m=="2", "Ours (with\ndiscount factor,\n2006-2023)\n", m),
  # ) %>%
  ggplot(aes(x=t, color=factor(m))) +
  geom_line(aes(y=v), linewidth=1) +
  scale_color_manual(values=brewer.pal(name="Set1",n=9)) +
  scale_x_continuous(breaks=seq(1,32*9,by=32)) +
  ylab("Estimated value\nrelative to No. 1 pick") +
  xlab("Draft pick") +
  labs(title = "Trade market curves")
ggsave(paste0("plots_weibull/plot_market_curves",post2013_str,".png"),plot_market_curves,width=10,height=5)

#################
### Visualize ###
#################

# ### view results
# df_results_params = 
#   tibble(
#     a = c("\U03BB", "SE(\U03BB)", "\U03B2", "SE(\U03B2)", "\U03C1", "SE(\U03C1)"),
#     b = c(lambda_m1, se_lambda_m1, beta_m1, se_beta_m1, NA, NA),
#     c = c(lambda_m2, se_lambda_m2, beta_m2, se_beta_m2, rho_m2, se_rho_m2),
#   )
# df_results_params
# 
# df_results_relPickVal = 
#   tibble(
#     a = paste0(ex_ts, "th pick"),
#     b = ex_vs_m1,
#     c = ex_vs_m2
#   )
# df_results_relPickVal
# 
# library(gt)
# 
# gt1 = 
#   gt(df_results_params) %>%
#   sub_missing(missing_text="") %>%
#   fmt_number(c(b,c), decimals = 3) %>%
#   cols_label(a = "", b="without discount factor", c="with discount factor")
# gt1
# 
# gt2 = 
#   gt(df_results_relPickVal) %>%
#   fmt_percent(c(b, c)) %>%
#   cols_label(a = "", b="without discount factor", c="with discount factor")
# gt2


