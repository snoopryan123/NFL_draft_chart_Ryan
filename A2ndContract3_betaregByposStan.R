
###### STAN BAYSEIAN bust_spike + beta regression model of P(y|x)

##################
### load stuff ###
##################

### load stan stuff
library(rstan)
rstan_options(auto_write = TRUE)
cores = 1
NUM_ITS = 10000

### load header
source("A2ndContract1_Header.R")

############################
### train the Stan model ###
############################

### examine positions
table(players_2C$position)
table(players_2C$pos)

### training dataframe 
df_train =
  players_2C %>%
  select(draft_pick, apy_cap_pct_2C, bust, pos) %>%
  ### remove special teams positions
  filter(!pos %in% c("LS", "K", "P")) %>%
  ### position index
  mutate(pos_idx = as.numeric(as.factor(pos)))
# ### make the QB have index 1
# idx_of_qb = unique( (df_train %>% filter(pos=="QB"))$pos_idx )
# pos_of_idx_1 = unique( (df_train %>% filter(pos_idx==1))$pos )
# df_train = df_train %>% mutate(pos_idx = case_when(
#   pos == "QB" ~ 1, pos == pos_of_idx_1 ~ idx_of_qb, TRUE ~ pos_idx
# ))
### checks
df_train
table(df_train$pos)
length(table(df_train$pos))
df_pos_posidx = df_train %>% distinct(pos,pos_idx) %>% arrange(pos_idx)
df_pos_posidx
num_pos = nrow(df_pos_posidx)
num_pos

### new data dataframe
df_new = 
  as_tibble(expand.grid(
    draft_pick = 1:256, pos = df_pos_posidx$pos
  )) %>% left_join(df_pos_posidx) %>% mutate(i = 1:n())
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
  num_pos = num_pos,
  x = df_train$draft_pick,
  xmat = x_spl_mat_train,
  pos_idx = df_train$pos_idx,
  bust = df_train$bust,
  y = df_train$apy_cap_pct_2C,
  
  n_new = nrow(df_new),
  x_new = df_new$draft_pick,
  xmat_new = x_spl_mat_new,
  pos_idx_new = df_new$pos_idx
)
df_stan_train_list

### stan file
filename = "betareg_stan_model_bypos.stan"
STANMODEL <- stan_model(file = filename, model_name = filename)
STANMODEL

### train or load the stan model
TRAIN_ME = TRUE
# TRAIN_ME = FALSE
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
        "alpha_0H", "alpha_1H", "beta_H", "gamma_0H", "gamma_1H",
        "tau_alpha_0H", "tau_alpha_1H", "tau_beta_H", "tau_gamma_0H", "tau_gamma_1H",
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

### check convergence
df_summary = summary(model_fit)$summary
df_summary
vec_rhats = df_summary[,"Rhat"]
vec_rhats
vec_rhats1 = vec_rhats[!str_detect(names(vec_rhats), "_new") & !str_detect(names(vec_rhats), "lp__")]
vec_rhats1
hist(vec_rhats1)

#############################
### get posterior samples ###
#############################

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




