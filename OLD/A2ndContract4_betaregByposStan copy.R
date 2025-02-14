
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
all(vec_rhats1 < 1.1)
# hist(vec_rhats1)

###############################################
### how much data is in each position group ###
###############################################

### positions
table(df_train$pos)

###
df_train %>%
  group_by(pos) %>%
  summarise(count=n()) %>%
  summarise(min(count), median(count), max(count), )

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
df_post_vals_byPos_0 = 
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
    sd = sqrt(mu*(1-mu)/(1+phi)),
    med = ifelse(
      0.5 - bust_prob > 0, ### median_in_tail
      qbeta(0.5 - bust_prob, shape1, shape2), ### median assuming the bust spike is uniform
      0.5 * bust_cutoff / bust_prob ### median of the bust spike
    ),
  ) 
df_post_vals_byPos_0
sum(is.na(df_post_vals_byPos_0))

df_post_vals_byPos = 
  df_post_vals_byPos_0 %>%
  pivot_wider(names_from=quantity, values_from=-c("i","quantity")) %>%
  left_join(df_new, by="i")
df_post_vals_byPos

df_post_vals_byQB = 
  df_post_vals_byPos %>%
  pivot_longer(-c(i,draft_pick,pos,pos_idx,QB)) %>%
  group_by(draft_pick,QB,name) %>%
  summarise(
    value = mean(value),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = name, values_from = value)
df_post_vals_byQB 

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
  df_post_vals_byPos %>% 
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
    title = "estimated \U03BC(x,pos)",
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
    title = "estimated sd(x,pos)",
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
  labs(title = "estimated bp(x,pos)") +
  scale_x_continuous(breaks=seq(1,32*9,by=32*2))
ggsave("plots_byPos/plot_empWithCondBustProb_byPos.png", width=12, height=8)

###
df_plot_musdbp_0 =
  df_post_vals_byPos_0 %>%
  pivot_longer(-c(i,quantity), names_to="param") %>%
  pivot_wider(names_from = quantity, values_from = value, names_prefix = "value_") %>%
  left_join(df_new, by="i") 
df_plot_musdbp_0

###
plot_condLines_byPos = 
  df_plot_musdbp_0 %>%
  filter(param %in% c("mu", "sd", "bust_prob")) %>%
  mutate(
    ordering = case_when(
      param == "mu" ~ 1,
      param == "sd" ~ 2,
      param == "bust_prob" ~ 3,
    ),
    param = case_when(
      param == "bust_prob" ~ "estimated bp(x,pos)",
      param == "mu" ~ "estimated \U03BC(x,pos)",
      param == "sd" ~ "estimated sd(x,pos)",
      TRUE ~ ""
    ),
    param = fct_reorder(param, ordering)
  ) %>%
  ggplot(aes(x=draft_pick, y=value_M, color=pos)) + 
  facet_wrap(~param, scales = "free_y") +
  geom_line(aes(linetype=QB), linewidth=1) +
  xlab("draft pick") + ylab("") +
  scale_x_continuous(breaks=seq(1,32*9,by=32*2))
ggsave("plots_byPos/plot_condLines_byPos.png", width=18, height=5)

###
plot_condLinesSE_byPos = 
  df_plot_musdbp_0 %>%
  filter(param %in% c("mu", "sd", "bust_prob")) %>%
  mutate(
    ordering = case_when(
      param == "mu" ~ 1,
      param == "sd" ~ 2,
      param == "bust_prob" ~ 3,
    ),
    param = case_when(
      param == "bust_prob" ~ "estimated bp(x,pos)",
      param == "mu" ~ "estimated \U03BC(x,pos)",
      param == "sd" ~ "estimated sd(x,pos)",
      TRUE ~ ""
    ),
    param = fct_reorder(param, ordering)
  ) %>%
  ggplot(aes(x=draft_pick, y=value_M, color=pos, fill=pos)) + 
  geom_ribbon(aes(ymin = value_L, ymax = value_U), alpha=0.6) +
  facet_wrap(~param, scales = "free_y") +
  geom_line(aes(linetype=QB), linewidth=1) +
  xlab("draft pick") + ylab("") +
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

###
df_post_vals_byQB_0 = 
  df_post_vals_byQB %>%
  pivot_longer(-c(draft_pick, QB, ), names_to="param") %>%
  mutate(
    quantity = str_sub(param, nchar(param), nchar(param)),
    param = str_remove_all(param, "_L|_M|_U")
  ) %>%
  pivot_wider(names_from = quantity, values_from = value, names_prefix = "value_")
df_post_vals_byQB_0

###
plot_condLinesSE_byQB = 
  df_post_vals_byQB_0 %>%
  filter(param %in% c("mu", "sd", "bust_prob")) %>%
  mutate(
    ordering = case_when(
      param == "mu" ~ 1,
      param == "sd" ~ 2,
      param == "bust_prob" ~ 3,
    ),
    param = case_when(
      param == "bust_prob" ~ "estimated bp(x,pos)",
      param == "mu" ~ "estimated \U03BC(x,pos)",
      param == "sd" ~ "estimated sd(x,pos)",
      TRUE ~ ""
    ),
    param = fct_reorder(param, ordering)
  ) %>%
  ggplot(aes(x=draft_pick, y=value_M, color=QB, fill=QB)) + 
  geom_ribbon(aes(ymin = value_L, ymax = value_U), alpha=0.6) +
  facet_wrap(~param, scales = "free_y") +
  geom_line(aes(linetype=QB), linewidth=1) +
  xlab("draft pick") + ylab("") +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(breaks=seq(1,32*9,by=32*2))
ggsave("plots_byPos/plot_condLinesSE_byQB.png", width=18, height=5)

###############################
### visualize median(x,pos) ###
###############################

df_summary_med_byQB = 
  df_post_vals_byQB %>%
  select(draft_pick, QB, all_of(starts_with("med")), all_of(starts_with("mu"))) %>%
  left_join(compensation_1C) %>%
  rename(compensation_M = rookie_contract_cap_pct) %>%
  pivot_longer(-c("draft_pick","QB")) %>%
  filter(name != "compensation_v1") %>%
  mutate(
    quantity = str_remove_all(name, "_L|_L1|_M|_M1|_U|_U1"),
    letter = str_sub(name,-1,-1),
  ) %>%
  mutate(
    quantity = ifelse(quantity == "med", "median", quantity),
    quantity = ifelse(quantity == "mu", "mean", quantity),
  ) %>%
  select(-name) %>%
  group_by(quantity, letter) %>%
  mutate(value1 = value/first(value)) %>%
  pivot_wider(names_from = c(letter), values_from=c(value, value1)) 
  # group_by(quantity, letter) %>%
  # mutate(value1 = value/first(value)) %>%
  # ungroup() %>%
  # pivot_wider(names_from = "quantity", values_from="value") 
df_summary_med_byQB

df_summary_med_byQB %>%
  # filter(letter=="M") %>%
  ggplot(aes(x = draft_pick, y = value_M, color = quantity)) +
  # geom_ribbon(aes(ymin = L, ymax = U, fill = quantity)) +
  facet_wrap(~QB) +
  geom_line(linewidth=2) +
  xlab("draft pick") +
  ylab("apy cap pct") +
  scale_color_brewer(name = "", palette="Set1") +
  scale_fill_brewer(name = "", palette="Set1") +
  scale_x_continuous(breaks=seq(1,32*9,by=32*2)) 

df_summary_med_byQB %>%
  # filter(letter=="M") %>%
  ggplot(aes(x = draft_pick, y = value1_M, color = quantity)) +
  # geom_ribbon(aes(ymin = L, ymax = U, fill = quantity)) +
  facet_wrap(~QB) +
  geom_line(linewidth=2) +
  xlab("draft pick") +
  ylab("apy cap pct") +
  scale_color_brewer(name = "", palette="Set1") +
  scale_fill_brewer(name = "", palette="Set1") +
  scale_x_continuous(breaks=seq(1,32*9,by=32*2)) 

#############################################################
### visualize conditional density P(y | x, QB, not bust)  ###
#############################################################

### posterior summary of the density
y_grid = seq(0.005,0.40,length.out=100)
y_grid

get_density_df_byQB <- function(y) {
  print(paste0("computing density for y=",y))
  df_post_vals_byQB %>% 
    select(draft_pick, QB, all_of(starts_with("shape")), all_of(starts_with("bust_prob")),) %>% 
    mutate(
      y = y,
      density_L = dbeta(y, shape1_L, shape2_L),
      density_M = dbeta(y, shape1_M, shape2_M),
      density_U = dbeta(y, shape1_U, shape2_U),
      density_times_bp_L = density_L*bust_prob_L,
      density_times_bp_M = density_M*bust_prob_M,
      density_times_bp_U = density_U*bust_prob_U,
    ) 
}

df_post_vals_byQB

# bind_rows(lapply(c(0.005, 0.01), get_density_df_byQB))
df_post_summary_density = bind_rows(lapply(y_grid, get_density_df_byQB))
df_post_summary_density

### plot posterior conditional density
plot_post_density_full_rd1 = 
  df_post_summary_density %>%
  filter(draft_pick %in% c(seq(1,32,by=2))) %>%
  ggplot(aes(x=y, color=fct_reorder(factor(draft_pick), -draft_pick))) +
  geom_line(aes(y=density_times_bp_M), linewidth=1) +
  facet_wrap(~QB) +
  xlab("apy cap pct") +
  ylab("density") +
  labs(title = "density of performance outcome Y") +
  scale_color_discrete(name = "draft pick") +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank()
  ) 
# plot_post_density_full_rd1
ggsave("plots_byPos/plot_post_density_full_rd1_byQB.png", width=11, height=5)

plot_post_density_full_rdsall = 
  df_post_summary_density %>%
  filter(draft_pick %in% c(seq(1,32*7,by=32/2))) %>%
  ggplot(aes(x=y, color=fct_reorder(factor(draft_pick), -draft_pick))) +
  geom_line(aes(y=density_times_bp_M), linewidth=1) +
  facet_wrap(~QB) +
  xlab("apy cap pct") +
  ylab("density") +
  labs(title = "density of performance outcome Y") +
  scale_color_discrete(name = "draft pick") +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank()
  ) 
# plot_post_density_full_rdsall
ggsave("plots_byPos/plot_post_density_full_rdsall_byQB.png", width=11, height=5)

# ### plot posterior conditional density
# plot_post_density_rd1 = 
#   df_post_summary_density %>%
#   filter(draft_pick %in% c(seq(1,32,by=2))) %>%
#   ggplot(aes(x=y, color=fct_reorder(factor(draft_pick), -draft_pick))) +
#   geom_line(aes(y=density_M), linewidth=1) +
#   facet_wrap(~QB) +
#   xlab("apy cap pct") +
#   ylab("density") +
#   labs(title = "density (given not a bust)") +
#   scale_color_discrete(name = "draft pick") +
#   theme(
#     axis.text.x = element_text(size = 10),
#     axis.text.y=element_blank(),
#     axis.ticks.y=element_blank()
#   ) 
# # plot_post_density_rd1
# ggsave("plots_byPos/plot_post_density_rd1_byQB.png", width=11, height=5)

# plot_post_density_rdsall = 
#   df_post_summary_density %>%
#   filter(draft_pick %in% c(seq(1,32*7,by=32/2))) %>%
#   ggplot(aes(x=y, color=fct_reorder(factor(draft_pick), -draft_pick))) +
#   geom_line(aes(y=density_M), linewidth=1) +
#   facet_wrap(~QB) +
#   xlab("apy cap pct") +
#   ylab("density") +
#   labs(title = "density (given not a bust)") +
#   scale_color_discrete(name = "draft pick") +
#   theme(
#     axis.text.x = element_text(size = 10),
#     axis.text.y=element_blank(),
#     axis.ticks.y=element_blank()
#   ) 
# # plot_post_density_rdsall
# ggsave("plots_byPos/plot_post_density_rdsall_byQB.png", width=11, height=5)

###################################################################
### G `GM value` function value curves V_G(x,QB) = E[G(Y)|x,QB] ###
###################################################################

### the step success function
G_step_func <- function(q) { function(y) { as.numeric(y>q) } }
### the G curve success function
G_Scurve_func <- function(a, b) { function(y) { pbeta(y, a, b) } }
### S curve string description
betaCdfStr <- function(a,b) { paste0("g(y) = S(\U003B1=",a,", \U03B2=",b,")(y)") }

### posterior summary of beta shape parameters and bust probability
df_post_summary_shapeparams_QB = 
  df_post_vals_byQB %>%
  select(draft_pick, QB, all_of(contains("shape")), all_of(contains("bust_prob"))) %>%
  left_join(compensation_1C) %>%
  select(-compensation_v1) %>%
  rename(cost = rookie_contract_cap_pct) %>%
  relocate(cost, .after = QB)
df_post_summary_shapeparams_QB

### G(y)•f(y|x) OR G(y-cost)•f(y|x)
G_times_density <- function(bust_prob, shape1, shape2, cost, G_func, surplus=FALSE) {
  function(y) {
    density_y =  ifelse(
      y > bust_cutoff,
      (1-bust_prob)*dbeta(y,shape1,shape2),
      bust_prob/bust_cutoff
    )
    if (surplus) {
      G_func(y-cost)*density_y
    } else {
      G_func(y)*density_y
    }
  }
}

### get dataframe of V_G(x,QB) = E[G(Y)|x,QB] = ∫ G(y)•f(y|x,QB) dy
### over each value of x,QB
get_df_V_G_QB <- function(
    G_func, desc="", surplus=FALSE, q=NA, type=""
) {
  df_V_G = as_tibble(expand.grid(draft_pick = 1:256, QB=c("QB", "not QB")))
  V_G <- function(j) {
    # dfx = df_post_summary_shapeparams_QB %>% filter(draft_pick == x, QB == qb)
    rowj = df_V_G[j,] ### j^th row
    x = rowj$draft_pick[1]
    qb = rowj$QB[1]
    print(paste0("computing V_G(x,QB) for draft pick x = ", x,", QB = ", qb))
    dfx = df_post_summary_shapeparams_QB %>% filter(draft_pick == x, QB == qb)
    dfx
    integrand_x = G_times_density(
      bust_prob=dfx$bust_prob_M, 
      shape1=dfx$shape1_M, shape2=dfx$shape2_M, 
      cost = dfx$cost,
      G_func=G_func,
      surplus=surplus
    )
    int_ = integrate(integrand_x, lower = 0, upper = 1)
    print(int_)
    int_$value
  }
  V_G_values = sapply(1:nrow(df_V_G), V_G)
  df_V_G$V_G = V_G_values
  # browser()
  QB_v1 = (df_V_G %>% filter(QB=="QB",draft_pick==1))$V_G
  df_V_G = 
    df_V_G %>% 
    mutate(
      V_G1 = V_G/QB_v1,
      # V_G1 = V_G/first(V_G),
      desc = desc,
      surplus = surplus,
      q=q,
      type=type
    )
  df_V_G
}

### get V_G(x) for 1 function G(y) = 1
df_V_G_1f = get_df_V_G_QB(G_func=function(y) { 1 }, desc=paste0("g(y) = 1"))
df_V_G_1f
### all values should be 1 (integrates to 1)
mean(abs(df_V_G_1f$V_G))

### get V_G(x) for identity function G(y) = y
df_V_G_id = get_df_V_G_QB(G_func=function(y) { y }, desc=paste0("g(y) = y"), type="linear")
df_V_G_id

### get V_G(x) for step function G(y) = 1{y>r}
pd_ = 1
# q_ = 0.05
q_ = 0.075
df_V_G_step_0 = get_df_V_G_QB(G_func=G_step_func(q=q_), desc=paste0("g(y) = 1{y>",percent(q_,pd_),"}"),q=q_, type="step")
df_V_G_step_0
# q_ = 0.0833333
q_ = 0.10
df_V_G_step_1 = get_df_V_G_QB(G_func=G_step_func(q=q_), desc=paste0("g(y) = 1{y>",percent(q_,pd_),"}"),q=q_, type="step")
df_V_G_step_1
# q_ = 0.11666667
q_ = 0.125
df_V_G_step_2 = get_df_V_G_QB(G_func=G_step_func(q=q_), desc=paste0("g(y) = 1{y>",percent(q_,pd_),"}"),q=q_, type="step")
df_V_G_step_2
q_ = 0.15
df_V_G_step_3 = get_df_V_G_QB(G_func=G_step_func(q=q_), desc=paste0("g(y) = 1{y>",percent(q_,pd_),"}"),q=q_, type="step")
df_V_G_step_3
# q_ = 0.20
# df_V_G_step_4 = get_df_V_G_QB(G_func=G_step_func(q=q_), desc=paste0("g(y) = 1{y>",percent(q_,0),"}"),q=q_, type="step")
# df_V_G_step_4

### get V_G(x) for G curve function 
get_df_V_G_Scurve <- function(a,b,surplus=FALSE) {
  get_df_V_G_QB(
    G_func=G_Scurve_func(a,b),
    desc = betaCdfStr(a,b),
    surplus = surplus,
    type="s"
  )
}
# df_V_G_Scurve_1 = get_df_V_G_Scurve(a=6, b=35)
df_V_G_Scurve_1 = get_df_V_G_Scurve(a=6, b=55)
df_V_G_Scurve_1
# df_V_G_Scurve_2 = get_df_V_G_Scurve(a=5, b=60)
df_V_G_Scurve_2 = get_df_V_G_Scurve(a=8, b=35)
df_V_G_Scurve_2

### visualize V_G(x)
# df_jj_1 = df_jj %>% rename(V_G1 = jj_v1) %>% mutate(desc = "Jimmy Johnson")
df_plot_V_G = 
  bind_rows(
    df_V_G_step_0,
    df_V_G_step_1,
    df_V_G_step_2,
    df_V_G_step_3,
    # df_V_G_step_4,
    # df_V_G_id,
    # df_V_G_Scurve_1,
    # df_V_G_Scurve_2,
    # bind_rows(df_trade_market_weibull %>% mutate(QB="QB"), df_trade_market_weibull %>% mutate(QB="not QB")),
    # bind_rows(df_jj_1 %>% mutate(QB="QB"), df_jj_1 %>% mutate(QB="not QB")),
    # bind_rows(df_jj_1 %>% mutate(QB="QB")),
    df_V_G_id %>% mutate(desc="expected\nperformance\nvalue"),
  ) #%>% select(-V_G) 
df_plot_V_G

###
plot_VG = 
  df_plot_V_G %>%
  
  # group_by(desc,QB) %>%
  # mutate(V_G1 = V_G1/first(V_G1)) %>%
  # ungroup() %>%
  
  mutate(q1 = percent(q,pd_)) %>%
  ggplot(aes(x=draft_pick, y = V_G1, color=factor(q1))) +
  facet_wrap(~QB) +
  
  # geom_line(linewidth=2, aes(color=desc), linetype="dashed", data = . %>% filter(type=="s")) +
  # geom_line(linewidth=2, aes(color=desc), linetype="dotted", data = . %>% filter(type=="linear")) +
  # geom_line(linewidth=2, aes(color=desc), linetype="solid", data = . %>% filter(type=="step")) +
  # scale_color_brewer(name="", palette = "Set1") +
  
  geom_line(linewidth=2, data = . %>% filter(!is.na(q))) +
  geom_line(linewidth=2, color="black", aes(linetype=desc), data = . %>% filter(is.na(q)) ) +
  scale_linetype_manual(name="", values = c(
    "longdash", "solid"
  )) +
  
  scale_color_brewer(name="eliteness cutoff\n(Percentage of cap)", palette = "Set1", na.translate=F) +
  ylab("value relative to first QB pick") +
  xlab("draft pick") +
  theme(legend.key.width=unit(2.5,"cm")) +
  scale_y_continuous(limits=c(0,1)) +
  scale_x_continuous(breaks=seq(1,32*9,by=32*2))
# plot_VG
ggsave("plots_byPos/plot_G_valueCurves_byQB.png",width=11, height=4)

###########################################
### Accounting for cost: surplus curves ###
###########################################

### posterior conditional surplus density
df_post_summary_density_surplus = 
  df_post_summary_density %>%
  left_join(compensation_1C) %>%
  rename(cost = rookie_contract_cap_pct) %>%
  mutate(s = y - cost) %>%
  relocate(s, .after = y) %>%
  relocate(cost, .after = s) %>%
  select(-compensation_v1) %>%
  arrange(draft_pick,y)
df_post_summary_density_surplus

### plot posterior conditional density
plot_post_surplus_density_full_rd1 = 
  df_post_summary_density_surplus %>%
  filter(draft_pick %in% c(seq(1,32,by=2))) %>%
  ggplot(aes(x=s, color=fct_reorder(factor(draft_pick), -draft_pick))) +
  geom_line(aes(y=density_times_bp_M), linewidth=1) +
  facet_wrap(~QB) +
  xlab("apy cap pct") +
  ylab("density") +
  labs(title = "density of surplus S") +
  scale_color_discrete(name = "draft pick") +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank()
  ) 
# plot_post_surplus_density_full_rd1
ggsave("plots_byPos/plot_post_surplus_density_full_rd1_byQB.png", width=11, height=5)

### plot posterior conditional density
plot_post_surplus_density_full_rdsall = 
  df_post_summary_density_surplus %>%
  filter(draft_pick %in% c(seq(1,32*7,by=32/2))) %>%
  ggplot(aes(x=s, color=fct_reorder(factor(draft_pick), -draft_pick))) +
  geom_line(aes(y=density_times_bp_M), linewidth=1) +
  facet_wrap(~QB) +
  xlab("apy cap pct") +
  ylab("density") +
  labs(title = "density of surplus S") +
  scale_color_discrete(name = "draft pick") +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank()
  ) 
ggsave("plots_byPos/plot_post_surplus_density_full_rdsall_byQB.png", width=11, height=5)

# ### plot posterior conditional density
# plot_post_surplus_density_rd1 = 
#   df_post_summary_density_surplus %>%
#   filter(draft_pick %in% c(seq(1,32,by=2))) %>%
#   ggplot(aes(x=s, color=fct_reorder(factor(draft_pick), -draft_pick))) +
#   geom_line(aes(y=density_M), linewidth=1) +
#   facet_wrap(~QB) +
#   xlab("apy cap pct") +
#   ylab("density") +
#   labs(title = "surplus density (given not a bust)") +
#   scale_color_discrete(name = "draft pick") +
#   theme(
#     axis.text.x = element_text(size = 10),
#     axis.text.y=element_blank(),
#     axis.ticks.y=element_blank()
#   ) 
# # plot_post_surplus_density_rd1
# ggsave("plots_byPos/plot_post_surplus_density_rd1_byQB.png", width=11, height=5)
# 
# ### plot posterior conditional density
# plot_post_surplus_density_rdsall = 
#   df_post_summary_density_surplus %>%
#   filter(draft_pick %in% c(seq(1,32*7,by=32/2))) %>%
#   ggplot(aes(x=s, color=fct_reorder(factor(draft_pick), -draft_pick))) +
#   geom_line(aes(y=density_M), linewidth=1) +
#   facet_wrap(~QB) +
#   xlab("apy cap pct") +
#   ylab("density") +
#   labs(title = "surplus density (given not a bust)") +
#   scale_color_discrete(name = "draft pick") +
#   theme(
#     axis.text.x = element_text(size = 10),
#     axis.text.y=element_blank(),
#     axis.ticks.y=element_blank()
#   ) 
# ggsave("plots_byPos/plot_post_surplus_density_rdsall_byQB.png", width=11, height=5)

### get V_G(x) for 1 function G(y) = 1
df_V_G_1fs = get_df_V_G_QB(G_func=function(y) { 1 }, desc=paste0("g(y) = 1"), surplus=TRUE)
df_V_G_1fs
### all values should be 1 (integrates to 1)
mean(abs(df_V_G_1fs$V_G))

### get V_G(x) for identity function G(y) = y
df_V_G_id_S = get_df_V_G_QB(G_func=function(y) { y }, desc=paste0("g(y) = y"), surplus=TRUE)
df_V_G_id_S

### get V_G(x) for step function G(y) = 1{y>r}
pd_ = 1
# q_ = 0.05
q_ = 0.075
df_V_G_step_0_S = get_df_V_G_QB(G_func=G_step_func(q=q_), desc=paste0("g(y) = 1{y>",q_,"}"), surplus=TRUE, q=q_)
df_V_G_step_0_S
# q_ = 0.0833333
q_ = 0.10
df_V_G_step_1_S = get_df_V_G_QB(G_func=G_step_func(q=q_), desc=paste0("g(y) = 1{y>",q_,"}"), surplus=TRUE, q=q_)
df_V_G_step_1_S
# q_ = 0.1166666667
q_ = 0.125
df_V_G_step_2_S = get_df_V_G_QB(G_func=G_step_func(q=q_), desc=paste0("g(y) = 1{y>",q_,"}"), surplus=TRUE, q=q_)
df_V_G_step_2_S
q_ = 0.15
df_V_G_step_3_S = get_df_V_G_QB(G_func=G_step_func(q=q_), desc=paste0("g(y) = 1{y>",q_,"}"), surplus=TRUE, q=q_)
df_V_G_step_3_S
# q_ = 0.20
# df_V_G_step_4_S = get_df_V_G_QB(G_func=G_step_func(q=q_), desc=paste0("g(y) = 1{y>",q_,"}"), surplus=TRUE, q=q_)
# df_V_G_step_4_S

### get V_G(x) for G curve function 
# df_V_G_Scurve_1_S = get_df_V_G_Scurve(a=6, b=35, surplus=TRUE)
df_V_G_Scurve_1_S = get_df_V_G_Scurve(a=6, b=55, surplus=TRUE)
df_V_G_Scurve_1_S
# df_V_G_Scurve_2_S = get_df_V_G_Scurve(a=5, b=60, surplus=TRUE)
df_V_G_Scurve_2_S = get_df_V_G_Scurve(a=8, b=35, surplus=TRUE)
df_V_G_Scurve_2_S

# ### visualize SV_G(x)
# df_plot_SV_G = 
#   bind_rows(
#     df_V_G_step_1_S,
#     df_V_G_step_2_S,
#     df_V_G_step_3_S,
#     df_V_G_Scurve_1_S,
#     df_V_G_Scurve_2_S,
#     # bind_rows(df_trade_market_weibull %>% mutate(QB="QB"), df_trade_market_weibull %>% mutate(QB="not QB")),
#     # bind_rows(df_jj_1 %>% mutate(QB="QB"), df_jj_1 %>% mutate(QB="not QB")),
#     bind_rows(df_jj_1 %>% mutate(QB="QB")),
#     df_V_G_id_S
#   ) #%>% select(-V_G) 
# df_plot_SV_G
# 
# ###
# plot_SVG = 
#   df_plot_SV_G %>%
#   ggplot(aes(x=draft_pick, y = V_G1, color=desc)) +
#   # ggplot(aes(x=draft_pick, y = V_G, color=desc)) +
#   facet_wrap(~QB) +
#   geom_line(linewidth=2) +
#   scale_color_brewer(name="E[g(S)|x,pos]/E[g(S)|x=1,qb]", palette = "Set2") +
#   # scale_color_brewer(name=bquote(paste('sv'['g']*'(x) = E[G(Y-cost)|x]')), palette = "Set2") +
#   xlab("draft pick") +
#   ylab("surplus value relative to first QB pick") +
#   # scale_y_continuous(limits=c(0,1)) +
#   scale_x_continuous(breaks=seq(1,32*9,by=32*2))
# # plot_SVG
# ggsave("plots_byPos/plot_G_surplusValueCurves_byQB.png",width=15,height=5)


### for CMSAC24 slides
df_plot_SV_G_2A = 
  bind_rows(
    df_V_G_id_S %>% mutate(desc = "expected\nsurplus\nvalue"),
    df_V_G_step_0_S,
    df_V_G_step_1_S,
    df_V_G_step_2_S,
    df_V_G_step_3_S,
  ) 
df_plot_SV_G_2A
plot_SVG_2A = 
  df_plot_SV_G_2A %>%
  mutate(q1 = percent(q,pd_)) %>%
  ggplot(aes(x=draft_pick, y = V_G1, color=factor(q1))) +
  facet_wrap(~QB) +
  geom_hline(yintercept=1, linetype="dashed", color="gray60", linewidth=1) +
  geom_hline(yintercept=0, linetype="dashed", color="gray60", linewidth=1) +
  geom_line(linewidth=2, data = . %>% filter(!is.na(q))) +
  geom_line(linewidth=2, color="black", aes(linetype=desc), data = . %>% filter(is.na(q)) ) +
  scale_linetype_manual(name="", values = c(
    "longdash", "solid"
  )) +
  scale_color_brewer(name="eliteness cutoff\n(Percentage of cap)", palette = "Set1", na.translate=F) +
  ylab("value relative to first QB pick") +
  xlab("draft pick") +
  theme(legend.key.width=unit(2.5,"cm")) +
  scale_x_continuous(breaks=seq(1,32*9,by=32*2))
# plot_SVG_2A
ggsave("plots_byPos/plot_G_surplusValueCurves_byQB.png",width=11,height=4)


# df_plot_SV_G_2B =
#   bind_rows(
#     df_V_G_id_S %>% mutate(
#       desc = "expected surplus value\nv(x,pos) ~ E(Y - cost(x)|x,pos)\n", a=1,
#     ),
#     df_V_G_step_025_S %>% mutate(
#       desc = "v(x,pos) ~ P(Y - cost(x) > 0.025|x,pos)", a=2,
#     ),
#     df_V_G_step_05_S %>% mutate(
#       desc = "v(x,pos) ~ P(Y - cost(x) > 0.05|x,pos)", a=3,
#     ),
#     df_V_G_step_1_S %>% mutate(
#       desc = "v(x,pos) ~ P(Y - cost(x) > 0.10|x,pos)", a=4,
#     ),
#     df_V_G_step_2_S %>% mutate(
#       desc = "v(x,pos) ~ P(Y - cost(x) > 0.15|x,pos)", a=5,
#     ),
#     # bind_rows(df_trade_market_weibull %>% mutate(QB="QB"), df_trade_market_weibull %>% mutate(QB="not QB")),
#   ) 
# df_plot_SV_G_2B
# plot_SVG_2B = 
#   df_plot_SV_G_2B %>%
#   # ggplot(aes(x=draft_pick, y = V_G1, color=desc)) +
#   ggplot(aes(x=draft_pick, y = V_G1, color=reorder(desc, a))) +
#   facet_wrap(~QB) +
#   geom_hline(yintercept=1, linetype="dashed", color="gray60", linewidth=1) +
#   geom_hline(yintercept=0, linetype="dashed", color="gray60", linewidth=1) +
#   geom_line(linewidth=2) +
#   scale_color_brewer(
#     name="",
#     palette = "Set1"
#   ) +
#   xlab("draft pick") +
#   ylab("surplus value relative to first QB pick") +
#   scale_x_continuous(breaks=seq(1,32*9,by=32*2))
# # plot_SVG_2B
# ggsave("plots_byPos/plot_G_surplusValueCurves_byQB_2B.png",width=15,height=5)

