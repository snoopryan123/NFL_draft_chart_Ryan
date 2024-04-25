
### load stan stuff
library(rstan)
rstan_options(auto_write = TRUE)
cores = 1
NUM_ITS = 2500

### load header
source("A_AHeader_2ndContract.R")

##################################################################
### STAN BAYSEIAN bust_spike + beta regression model of P(y|x) ###
##################################################################

### training dataframe
df_train =
  players_2C %>%
  select(draft_pick, apy_cap_pct_2C, bust) 
df_train

### new data dataframe
df_new = 
  as_tibble(expand.grid(
    draft_pick = 1:256
  )) %>% mutate(i = 1:n())
df_new

### x spline matrices
df_to_x_spl_mat <- function(df) {
  # bs(df$draft_pick, degree=1, intercept=T)
  bs(df$draft_pick, degree=3, df=4, intercept=T)
}
x_spl_mat_train = df_to_x_spl_mat(df_train)
x_spl_mat_new = df_to_x_spl_mat(df_new)
head(x_spl_mat_train)
head(x_spl_mat_new)

### data for stan
df_stan_train_list <- list(
  n = nrow(df_train),
  m = ncol(x_spl_mat_train),
  x = df_train$draft_pick,
  xmat = x_spl_mat_train,
  bust = df_train$bust,
  y = df_train$apy_cap_pct_2C,
  
  n_new = nrow(df_new),
  m_new = ncol(x_spl_mat_new),
  x_new = df_new$draft_pick,
  xmat_new = x_spl_mat_new
)
df_stan_train_list

### stan file
filename = "betareg_stan_model_overall.stan"
STANMODEL <- stan_model(file = filename, model_name = filename)
STANMODEL

### train or load the stan model
# TRAIN_ME = TRUE
TRAIN_ME = FALSE
model_filename = paste0("model_fit_", str_remove_all(filename, "\\.stan"), ".rds")
if (TRAIN_ME | !file.exists(model_filename)) {
  # Train the model
  seed = 12345
  set.seed(seed)
  NUM_ITERS_IN_CHAIN = NUM_ITS
  model_fit <-
    sampling(
      STANMODEL,
      data = df_stan_train_list,
      iter = NUM_ITERS_IN_CHAIN,
      pars=c(
        "alpha_0", "alpha_1", "beta", "gamma_0", "gamma_1",
        "mu_new", "phi_new", "bust_prob_new"
      ),
      include=TRUE,
      chains = cores, #1 #cores,
      cores = cores, # HPCC
      seed = seed
    )
  ### save model
  saveRDS(model_fit, model_filename)
} else {
  model_fit = readRDS(model_filename)
}
model_fit

### posterior summary
post_summary_vals = summary(model_fit)$summary
df_post_vals =
  tibble(
    name = rownames(post_summary_vals),
    L = post_summary_vals[,"2.5%"],
    M = post_summary_vals[,"mean"],
    U = post_summary_vals[,"97.5%"]
  )
# df_post_vals
df_post_vals %>% filter( str_detect(name, "alpha|beta|gamma") )

### posterior draws
post_draws_mat = as.matrix(model_fit)
dim(post_draws_mat)
df_post_draws_0 = 
  as.data.frame(post_draws_mat) %>%
  mutate(draw = 1:n()) %>%
  pivot_longer(-draw) %>%
  filter(str_detect(name, "mu|phi|bust_prob")) %>%
  mutate(
    param = str_remove_all(str_replace_all(str_remove_all(name, "_new"), "[0-9]*", ""), "\\[|\\]"),
    i = extract_numeric(name)
    # i = readr::parse_number(name)
  ) 
df_post_draws_0
df_post_draws = 
  df_post_draws_0 %>%
  select(-name) %>%
  pivot_wider(names_from="param", values_from=c("value")) %>%
  mutate(
    shape1 = mu*phi,
    shape2 = (1-mu)*phi,
    sd = sqrt(mu*(1-mu)/(1+phi))
  )
df_post_draws

### posterior summary of mu and sd
df_post_summary_musd = 
  df_post_draws %>%
  select(draw, i, mu, sd, bust_prob) %>%
  pivot_longer(c("mu", "sd", "bust_prob"), names_to="quantity") %>%
  group_by(draw, quantity) %>%
  mutate(value_1 = value/first(value)) %>%
  group_by(i,quantity) %>%
  summarise(
    value_L =  quantile(value, .025),
    value_M = mean(value),
    value_U = quantile(value, .975),
    value_L1 =  quantile(value_1, .025),
    value_M1 = mean(value_1),
    value_U1 = quantile(value_1, .975),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = "quantity", values_from = all_of(starts_with("value")))
names(df_post_summary_musd) = str_remove_all(names(df_post_summary_musd), "value_")
df_post_summary_musd

### empirical conditional mean and s.d.
df_overall_emp_musd_tail =
  players_2C %>%
  filter(bust==0) %>%
  group_by(draft_pick) %>%
  summarise(
    emp_mean_tail = mean(apy_cap_pct_2C),
    emp_sd_tail = sd(apy_cap_pct_2C),
    .groups = "drop"
  ) 
df_overall_emp_musd_tail

### empirical bust prob.
df_overall_emp_bust_prob =
  players_2C %>%
  group_by(draft_pick) %>%
  summarise(
    emp_bust_prob = mean(bust),
    .groups = "drop"
  ) 
df_overall_emp_bust_prob

### empirical df
df_overall_emp = left_join(df_overall_emp_musd_tail, df_overall_emp_bust_prob)
df_overall_emp

### plot conditional mean mu(x) and standard deviation sd(x) and bust_prob(x)
df_plot_musdbp = left_join(df_new, df_post_summary_musd) %>% left_join(df_overall_emp)
df_plot_musdbp

df_plot_musdbp %>%
  ggplot(aes(x = draft_pick)) +
  geom_point(aes(y=emp_mean_tail)) +
  geom_ribbon(aes(ymin = L_mu, ymax = U_mu), fill="gray80") +
  geom_line(aes(y = M_mu), linewidth=1) +
  xlab("draft pick") + ylab("apy cap pct") +
  labs(title = "conditional mean \U03BC(x)") +
  scale_x_continuous(breaks=seq(1,32*9,by=32*2))

df_plot_musdbp %>%
  ggplot(aes(x = draft_pick)) +
  geom_point(aes(y=emp_sd_tail)) +
  geom_ribbon(aes(ymin = L_sd, ymax = U_sd), fill="gray80") +
  geom_line(aes(y = M_sd), linewidth=1) +
  xlab("draft pick") + ylab("apy cap pct") +
  labs(title = "conditional standard deviation sd(x)") +
  scale_x_continuous(breaks=seq(1,32*9,by=32*2))

df_plot_musdbp %>%
  filter(draft_pick < 255) %>%
  ggplot(aes(x = draft_pick)) +
  geom_point(aes(y=emp_bust_prob)) +
  geom_ribbon(aes(ymin = L_bust_prob, ymax = U_bust_prob), fill="gray80") +
  geom_line(aes(y = M_bust_prob), linewidth=1) +
  xlab("draft pick") + ylab("probability") +
  labs(title = "conditional bust probability bp(x)") +
  scale_x_continuous(breaks=seq(1,32*9,by=32*2))

### draft value curves using v(x) = mu(x)/mu(1)
df_plot_musdbp %>%
  ggplot(aes(x = draft_pick, y = M1_mu)) +
  geom_line(linewidth=1) +
  xlab("draft pick") +
  ylab("value relative to first pick") +
  labs(title = "posterior mean relative EV \U03BC(x)/\U03BC(x=1)") +
  scale_x_continuous(breaks=seq(1,32*9,by=32*2))

### posterior summary of the density
y_grid = seq(0.005,0.25,length.out=100)
y_grid

get_density_df <- function(y) {
  print(paste0("computing density for y=",y))
  df_post_draws %>% 
    select(i, draw, shape1, shape2) %>% 
    mutate(
      y = y,
      density = dbeta(y, shape1, shape2)
    ) %>%
    group_by(i,y) %>%
    summarise(
      density_L =  quantile(density, .025),
      density_M = mean(density),
      density_U = quantile(density, .975),
      .groups = "drop"
    )
}

# bind_rows(lapply(c(0.005, 0.01), get_density_df))
df_post_summary_density_0 = bind_rows(lapply(y_grid, get_density_df))
df_post_summary_density_0
df_post_summary_density = df_post_summary_density_0 %>% left_join(df_new)
df_post_summary_density

### plot posterior conditional density
df_post_summary_density %>%
  filter(draft_pick %in% c(seq(1,32*7,by=32/2))) %>%
  ggplot(aes(x=y, color=factor(draft_pick))) +
  geom_line(aes(y=density_M), linewidth=1) +
  xlab("apy cap pct") +
  ylab("density") +
  labs(title = "density (given not a bust)") +
  scale_color_discrete(name = "draft pick") +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank()
  ) 

### plot posterior conditional density
df_post_summary_density %>%
  filter(draft_pick %in% c(seq(1,32*7,by=32/1))) %>%
  ggplot(aes(x=y, color=factor(draft_pick), fill=factor(draft_pick))) +
  geom_line(aes(y=density_M), linewidth=1) +
  geom_ribbon(aes(ymin = density_L, ymax=density_U)) +
  xlab("apy cap pct") +
  ylab("density") +
  labs(title = "density (given not a bust)") +
  scale_color_discrete(name = "draft pick") +
  scale_fill_discrete(name = "draft pick") +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank()
  ) 

### plot posterior conditional density
plot_func_betareg_overall_density <- 
  function(ex_draft_picks, includeEmp=TRUE, includeErrorBars=TRUE) {
  p = 
    bind_rows(
      df_post_summary_density,
      players_2C %>% filter(bust==0) %>% select(draft_pick, apy_cap_pct_2C)
    ) %>%
    filter(draft_pick %in% ex_draft_picks) %>%
    ggplot() +
    facet_wrap(~ draft_pick) +
    xlab("apy cap pct") +
    ylab("density") +
    labs(title = "density (given not a bust)") +
    theme(
      axis.text.x = element_text(size = 10),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()
    ) 
  if (includeEmp) {
    p = p + 
      geom_histogram(aes(x = apy_cap_pct_2C, y=after_stat(density)), fill="gray80") +
      geom_density(aes(x = apy_cap_pct_2C), linewidth=1, color="gray60") 
  }
  if (includeErrorBars) {
    p = p + geom_ribbon(aes(x = y, ymin = density_L, ymax=density_U), 
                        fill="darkslategray2", color="darkslategray2") 
  }
  p = p + geom_line(aes(x = y, y = density_M), linewidth=1) 
  p
}

###
plot_func_betareg_overall_density(c(seq(1,32*7,by=32/2)),F,F)
plot_func_betareg_overall_density(c(seq(1,32*3,by=8), seq(32*3,32*5,by=16)),F,F)
plot_func_betareg_overall_density(c(seq(1,32*2,by=32/8)),F,F)
# plot_func_betareg_overall_density(c(seq(1,32*7,by=32/2)),F,T)
# plot_func_betareg_overall_density(c(seq(1,32*3,by=8), seq(32*3,32*5,by=16)),F,T)
# plot_func_betareg_overall_density(c(seq(1,32*2,by=32/8)),F,T)
###
plot_func_betareg_overall_density(c(seq(1,16,by=1)),T,F)
plot_func_betareg_overall_density(c(seq(17,32,by=1)),T,F)
plot_func_betareg_overall_density(c(seq(33,48,by=1)),T,F)
plot_func_betareg_overall_density(c(seq(49,64,by=1)),T,F)
###
plot_func_betareg_overall_density(c(seq(65,80,by=1)),T,F)
plot_func_betareg_overall_density(c(seq(81,112,by=2)),T,F)
plot_func_betareg_overall_density(c(seq(113,32*7,by=8)),T,F)
###

### posterior summary of tail probability
q_grid = c(seq(0.05, 0.15, by=0.01), 0.175, 0.20)
# q_grid = c(0.04, 0.05, 0.06)
q_grid

get_tail_prob_df <- function(q) {
  print(paste0("computing tail prob for q=",q))
  assertthat::assert_that(q > bust_cutoff)
  
  df_post_draws %>%
    mutate(
      q = q, 
      tail_prob_given_notbust = pbeta(q, shape1, shape2, lower.tail = F),
      tail_prob = (1-bust_prob)*tail_prob_given_notbust) %>%
    group_by(draw,q) %>%
    mutate(tail_prob_1 = tail_prob/first(tail_prob)) %>%
    group_by(i,q) %>%
    summarise(
      tail_prob_L = quantile(tail_prob, .025),
      tail_prob_M = mean(tail_prob),
      tail_prob_U = quantile(tail_prob, 0.975),
      tail_prob_1_L = quantile(tail_prob_1, .025),
      tail_prob_1_M = mean(tail_prob_1),
      tail_prob_1_U = quantile(tail_prob_1, 0.975),
      .groups = "drop"
    ) 
}

# bind_rows(lapply(c(0.08, 0.12), get_tail_prob_df))
df_post_summary_tail_prob_0 = bind_rows(lapply(q_grid, get_tail_prob_df))
df_post_summary_tail_prob_0
df_post_summary_tail_prob_1 = left_join(
  df_post_summary_tail_prob_0,
  df_plot_musdbp %>% select(draft_pick,i,all_of(contains("mu")))
)
df_post_summary_tail_prob_1
df_post_summary_tail_prob = df_post_summary_tail_prob_1 %>% left_join(df_new)
df_post_summary_tail_prob

### draft value curves using v_q(x) = P(y>q|x)

### plot posterior tail probability
df_post_summary_tail_prob %>%
  filter(draft_pick < 255) %>%
  ggplot(aes(x = draft_pick, y = tail_prob_M, color=factor(q))) +
  geom_line(linewidth=1) +
  xlab("draft pick") +
  ylab("probability") +
  labs(title = "posterior mean tail probability P(y>q|x)") +
  scale_color_discrete(name="q") +
  scale_x_continuous(breaks=seq(1,32*9,by=32*2))

### plot posterior relative tail probability
df_post_summary_tail_prob %>%
  filter(draft_pick < 255) %>%
  ggplot(aes(x = draft_pick, y = tail_prob_1_M, color=factor(q))) +
  geom_line(linewidth=1) +
  xlab("draft pick") +
  ylab("value relative to first pick") +
  labs(title = "posterior mean relative tail probability P(y>q|x)/P(y>q|x=1)") +
  # labs(title = "tail probability P(y>q|x)/P(y>q|x=1)") +
  scale_color_discrete(name="q") +
  scale_x_continuous(breaks=seq(1,32*9,by=32*2))

### plot posterior relative tail probability with performance value curve
df_post_summary_tail_prob %>%
  filter(draft_pick < 255) %>%
  ggplot(aes(x = draft_pick, y = tail_prob_1_M, color=factor(q))) +
  geom_line(aes(y = M1_mu), color="black", linetype="dashed") +
  geom_line(linewidth=1) +
  xlab("draft pick") +
  ylab("value relative to first pick") +
  labs(title = "posterior mean relative tail probability P(y>q|x)/P(y>q|x=1)") +
  # labs(title = "tail probability P(y>q|x)/P(y>q|x=1)") +
  scale_color_discrete(name="q") +
  scale_x_continuous(breaks=seq(1,32*9,by=32*2))


#FIXME
# add compensation curves
# add integral of S function



