
###### STAN BAYSEIAN bust_spike + beta regression model of P(y|x)

##################
### load stuff ###
##################

### load stan stuff
library(rstan)
rstan_options(auto_write = TRUE)
cores = 1
NUM_ITS = 50000

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
  )) %>% left_join(df_pos_posidx) %>% 
  mutate(i = 1:n(), QB = ifelse(pos=="QB","QB","not QB"))
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

### posterior intervals
df_post_vals_0 = 
  df_post_vals %>%
  filter(str_detect(name, "mu|phi|bust_prob")) %>%
  mutate(
    param = str_remove_all(str_replace_all(str_remove_all(name, "_new"), "[0-9]*", ""), "\\[|\\]"),
    i = extract_numeric(name)
    # i = readr::parse_number(name)
  ) %>%
  select(-name) %>%
  pivot_longer(-c(param,i), names_to = "quantity") %>%
  pivot_wider(names_from = "param", values_from = "value") %>%
  mutate(
    shape1 = mu*phi,
    shape2 = (1-mu)*phi,
    sd = sqrt(mu*(1-mu)/(1+phi))
  ) 
df_post_vals_0
df_post_vals_1 = 
  df_post_vals_0 %>%
  pivot_wider(names_from=quantity, values_from=-c("i","quantity")) 
df_post_vals_1

# ### posterior draws
# post_draws_mat = as.matrix(model_fit)
# dim(post_draws_mat)
# df_post_draws_0 = 
#   as.data.frame(post_draws_mat) %>%
#   mutate(draw = 1:n()) %>%
#   pivot_longer(-draw) %>%
#   filter(str_detect(name, "mu|phi|bust_prob")) %>%
#   mutate(
#     param = str_remove_all(str_replace_all(str_remove_all(name, "_new"), "[0-9]*", ""), "\\[|\\]"),
#     i = extract_numeric(name)
#     # i = readr::parse_number(name)
#   ) 
# df_post_draws_0
# df_post_draws = 
#   df_post_draws_0 %>%
#   select(-name) %>%
#   pivot_wider(names_from="param", values_from=c("value")) %>%
#   mutate(
#     shape1 = mu*phi,
#     shape2 = (1-mu)*phi,
#     sd = sqrt(mu*(1-mu)/(1+phi))
#   )
# df_post_draws

##################################################
### visualize mu(x,pos), sd(x,pos), bp(x,pos)  ###
##################################################

### empirical conditional mean and s.d.
df_overall_emp_musd_tail =
  df_train %>%
  filter(bust==0) %>%
  group_by(draft_pick,pos) %>%
  summarise(
    emp_mean_tail = mean(apy_cap_pct_2C),
    emp_sd_tail = sd(apy_cap_pct_2C),
    .groups = "drop"
  ) 
df_overall_emp_musd_tail

### empirical bust prob.
df_overall_emp_bust_prob =
  df_train %>%
  group_by(draft_pick,pos) %>%
  summarise(
    emp_bust_prob = mean(bust),
    .groups = "drop"
  ) 
df_overall_emp_bust_prob

### empirical df
df_overall_emp = full_join(df_overall_emp_musd_tail, df_overall_emp_bust_prob, by = c("draft_pick", "pos"))
df_overall_emp

### plot conditional mean mu(x,pos) and standard deviation sd(x,pos) and bust_prob(x,pos)
df_plot_musdbp = 
  left_join(df_new, df_post_vals_1, by="i") %>% 
  left_join(df_overall_emp, by=c("draft_pick", "pos"))
df_plot_musdbp

###
plot_condMeanWithEmp_byPos = 
  df_plot_musdbp %>%
  ggplot(aes(x = draft_pick)) +
  facet_wrap(~pos) +
  geom_point(aes(y=emp_mean_tail)) +
  geom_ribbon(aes(ymin = mu_L, ymax = mu_U), fill="gray60", alpha=0.6) +
  geom_line(aes(y = mu_M), linewidth=1) +
  xlab("draft pick") + ylab("apy cap pct") +
  labs(
    title = "conditional mean \U03BC(x,pos) = E[Y|x,pos]",
    subtitle="given not a bust"
  ) +
  scale_x_continuous(breaks=seq(1,32*9,by=32*2))
# plot_condMeanWithEmp_byPos
ggsave("plots_byPos/plot_empWithCondMean_byPos.png", width=12, height=8)

###
plot_condSdWithEmp_byPos = 
  df_plot_musdbp %>%
  ggplot(aes(x = draft_pick)) +
  facet_wrap(~pos) +
  geom_point(aes(y=emp_sd_tail)) +
  geom_ribbon(aes(ymin = sd_L, ymax = sd_U), fill="gray60", alpha=0.6) +
  geom_line(aes(y = sd_M), linewidth=1) +
  xlab("draft pick") + ylab("apy cap pct") +
  labs(
    title = "conditional standard deviation sd(x,pos)",
    subtitle="given not a bust"
  ) +
  scale_x_continuous(breaks=seq(1,32*9,by=32*2))
ggsave("plots_byPos/plot_empWithCondSd_byPos.png", width=12, height=8)

###
plot_condBustProbWithEmp_byPos = 
  df_plot_musdbp %>%
  ggplot(aes(x = draft_pick)) +
  facet_wrap(~pos) +
  geom_point(aes(y=emp_bust_prob)) +
  geom_ribbon(aes(ymin = bust_prob_L, ymax = bust_prob_U), fill="gray60", alpha=0.6) +
  geom_line(aes(y = bust_prob_M), linewidth=1) +
  xlab("draft pick") + ylab("probability") +
  labs(title = "conditional bust probability bp(x,pos)") +
  scale_x_continuous(breaks=seq(1,32*9,by=32*2))
ggsave("plots_byPos/plot_empWithCondBustProb_byPos.png", width=12, height=8)

###
df_plot_musdbp_0 =
  df_post_vals_0 %>%
  pivot_longer(-c(i,quantity), names_to="param") %>%
  pivot_wider(names_from = quantity, values_from = value, names_prefix = "value_") %>%
  left_join(df_new, by="i") 
df_plot_musdbp_0

###
plot_condLines_byPos = 
  df_plot_musdbp_0 %>%
  filter(param %in% c("mu", "sd", "bust_prob")) %>%
  mutate(
    param = case_when(
      param == "bust_prob" ~ "bust probability bp(x,pos)",
      param == "mu" ~ "conditional mean \U03BC(x,pos)",
      param == "sd" ~ "conditional s.d. sd(x,pos)",
      TRUE ~ ""
    )
  ) %>%
  ggplot(aes(x=draft_pick, y=value_M, color=pos)) + 
  facet_wrap(~param, scales = "free_y") +
  geom_line(aes(linetype=QB), linewidth=1) +
  xlab("draft pick") + ylab("probability") +
  scale_x_continuous(breaks=seq(1,32*9,by=32*2))
ggsave("plots_byPos/plot_condLines_byPos.png", width=18, height=5)


###
plot_condLinesSE_byPos = 
  df_plot_musdbp_0 %>%
  filter(param %in% c("mu", "sd", "bust_prob")) %>%
  mutate(
    param = case_when(
      param == "bust_prob" ~ "bust probability bp(x,pos)",
      param == "mu" ~ "conditional mean \U03BC(x,pos)",
      param == "sd" ~ "conditional s.d. sd(x,pos)",
      TRUE ~ ""
    )
  ) %>%
  ggplot(aes(x=draft_pick, y=value_M, color=pos, fill=pos)) + 
  geom_ribbon(aes(ymin = value_L, ymax = value_U), alpha=0.6) +
  facet_wrap(~param, scales = "free_y") +
  geom_line(aes(linetype=QB), linewidth=1) +
  xlab("draft pick") + ylab("probability") +
  scale_x_continuous(breaks=seq(1,32*9,by=32*2))
ggsave("plots_byPos/plot_condLinesSE_byPos.png", width=18, height=5)

# ###
# plot_condLinesSE_byPos = 
#   df_plot_musdbp_0 %>%
#   filter(param %in% c("mu", "sd", "bust_prob")) %>%
#   mutate(
#     param = case_when(
#       param == "bust_prob" ~ "bust probability bp(x,pos)",
#       param == "mu" ~ "conditional mean \U03BC(x,pos)",
#       param == "sd" ~ "conditional s.d. sd(x,pos)",
#       TRUE ~ ""
#     )
#   ) %>%
#   ggplot(aes(x=draft_pick, y=value_M, color=pos)) + 
#   geom_ribbon(aes(ymin = value_L, ymax = value_U), fill="gray60", alpha=0.6) +
#   facet_wrap(~param, scales = "free_y") +
#   geom_line(aes(linetype=QB), linewidth=1) +
#   xlab("draft pick") + ylab("probability") +
#   scale_x_continuous(breaks=seq(1,32*9,by=32*2))
# ggsave("plots_byPos/plot_condLinesSE_byPos.png", width=18, height=5)
# 

#######

