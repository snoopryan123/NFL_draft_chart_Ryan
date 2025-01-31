
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

### posterior summary of alpha, beta, gamma
post_summary_vals = summary(model_fit)$summary
df_post_summary_alpha_beta_gamma =
  tibble(
    name = rownames(post_summary_vals),
    L = post_summary_vals[,"2.5%"],
    M = post_summary_vals[,"mean"],
    U = post_summary_vals[,"97.5%"]
  )
# df_post_vals
df_post_summary_alpha_beta_gamma %>% filter( str_detect(name, "alpha|beta|gamma") )
rm(post_summary_vals) #FIXME

### posterior draws
post_draws_mat = as.matrix(model_fit)
post_draws_mat[1:5,1:5]
dim(post_draws_mat)

### keep 1250 draws like the position-agnostic model from the previous file
### the computations take way too long with 10x the draws
post_draws_mat_thinned = post_draws_mat[seq(1,nrow(post_draws_mat),20),]
post_draws_mat_thinned[1:5,1:5]
dim(post_draws_mat_thinned)

### dataframe of posterior draws
df_post_draws_0 = 
  as.data.frame(post_draws_mat_thinned) %>%
  mutate(draw = 1:n()) %>%
  select(c(draw, all_of(contains("mu")), all_of(contains("phi")), all_of(contains("bust_prob"))))
# df_post_draws_0
df_post_draws_1 = 
  df_post_draws_0 %>%
  pivot_longer(-draw) %>%
  mutate(
    param = str_extract(name, "^[^_]+"),
    param = ifelse(param == "bust", "bust_prob", param),
    j = as.numeric(str_extract(name, "(?<=\\[)[^]]+(?=\\])"))
  )
df_post_draws_1
df_post_draws_2 = 
  df_post_draws_1 %>%
  select(draw, j, param, value) %>%
  pivot_wider(names_from = "param", values_from = "value")  %>%
  mutate(
    shape1 = mu*phi,
    shape2 = (1-mu)*phi,
    sd = sqrt(mu*(1-mu)/(1+phi)),
    # med = ifelse(
    #   0.5 - bust_prob > 0, ### median_in_tail
    #   qbeta(0.5 - bust_prob, shape1, shape2), ### median assuming the bust spike is uniform
    #   0.5 * bust_cutoff / bust_prob ### median of the bust spike
    # ),
  ) 
df_post_draws_2

### dataframe of posterior draws by position
df_post_draws_byPos = 
  df_post_draws_2 %>%
  rename(i = j) %>%
  left_join(df_new, by="i") %>%
  relocate(draft_pick, .after = i) %>%
  relocate(pos, .after = draft_pick) %>%
  relocate(pos_idx, .after = pos) %>%
  relocate(QB, .after = pos) 
df_post_draws_byPos

### dataframe of posterior draws by QB/not QB
df_post_draws_byQB = 
  df_post_draws_byPos %>%
  pivot_longer(-c(draw, i, draft_pick, pos, QB, pos_idx)) %>%
  group_by(draw, draft_pick, QB, name) %>%
  reframe(value = mean(value)) %>%
  pivot_wider(names_from = name, values_from = value)
df_post_draws_byQB 

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
  df_post_draws_byPos %>%
  group_by(draft_pick, pos) %>%
  reframe(
    mu_L = quantile(mu, 0.025),
    mu_M = mean(mu),
    mu_U = quantile(mu, 0.975),
    sd_L = quantile(sd, 0.025),
    sd_M = mean(sd),
    sd_U = quantile(sd, 0.975),
    bp_L = quantile(bust_prob, 0.025),
    bp_M = mean(bust_prob),
    bp_U = quantile(bust_prob, 0.975),
  ) %>% 
  left_join(df_overall_emp, by=c("draft_pick", "pos"))
df_plot_musdbp

###
plot_condMeanWithEmp_byPos = 
  df_plot_musdbp %>%
  ggplot(aes(x = draft_pick)) +
  facet_wrap(~pos) +
  geom_point(aes(y=emp_mean_tail), color="gray60") +
  geom_ribbon(aes(ymin = mu_L, ymax = mu_U), fill="darkslategray2", alpha=0.6) +
  geom_line(aes(y = mu_M), linewidth=1) +
  xlab("Draft position") +
  ylab("Percentage of cap") +
  labs(
    # title = "estimated \U03BC(x,pos)", subtitle="given not a bust"
    title = "Estimated \U03BC(x,pos) given not a bust"
  ) +
  scale_y_continuous(
    labels = percent_format(), 
    # limits=c(0, 0.25),
  ) +
  scale_x_continuous(breaks=seq(1,32*9,by=32*2))
# plot_condMeanWithEmp_byPos
ggsave("plots_byPos/plot_empWithCondMean_byPos.png", width=12, height=7)

###
plot_condSdWithEmp_byPos = 
  df_plot_musdbp %>%
  ggplot(aes(x = draft_pick)) +
  facet_wrap(~pos) +
  geom_point(aes(y=emp_sd_tail), color="gray60") +
  geom_ribbon(aes(ymin = sd_L, ymax = sd_U), fill="darkslategray2", alpha=0.6) +
  geom_line(aes(y = sd_M), linewidth=1) +
  xlab("Draft position") +
  ylab("Percentage of cap") +
  labs(
    # title = "estimated sd(x,pos)", subtitle="given not a bust"
    title = "Estimated sd(x,pos) given not a bust"
  ) +
  scale_y_continuous(
    labels = percent_format(), 
    # limits=c(0, 0.25),
  ) +
  scale_x_continuous(breaks=seq(1,32*9,by=32*2))
ggsave("plots_byPos/plot_empWithCondSd_byPos.png", width=12, height=7)

###
plot_condBustProbWithEmp_byPos = 
  df_plot_musdbp %>%
  ggplot(aes(x = draft_pick)) +
  facet_wrap(~pos) +
  geom_point(aes(y=emp_bust_prob), color="gray60") +
  geom_ribbon(aes(ymin = bp_L, ymax = bp_U), fill="darkslategray2", alpha=0.6) +
  geom_line(aes(y = bp_M), linewidth=1) +
  xlab("Draft position") +
  ylab("Probability") +
  labs(title = "Estimated bp(x,pos)") +
  scale_x_continuous(breaks=seq(1,32*9,by=32*2))
ggsave("plots_byPos/plot_empWithCondBustProb_byPos.png", width=12, height=7)

####################################################
### visualize mu(x,pos), sd(x,pos), bp(x,pos) V2 ###
####################################################

###
df_plot_musdbp_1 =
  df_plot_musdbp %>%
  select(-c(all_of(contains("emp")))) %>%
  pivot_longer(-c(draft_pick, pos), names_to="quantity") %>%
  mutate(param = str_sub(quantity,1,2), letter = str_sub(quantity,-1)) %>%
  select(-quantity) %>%
  mutate(QB = pos == "QB") %>%
  pivot_wider(names_from = letter, values_from = value, names_prefix = "value_") 
df_plot_musdbp_1

###
df_plot_musdbp_QB = 
  df_post_draws_byPos %>%
  group_by(draft_pick, QB) %>%
  reframe(
    mu_L = quantile(mu, 0.025),
    mu_M = mean(mu),
    mu_U = quantile(mu, 0.975),
    sd_L = quantile(sd, 0.025),
    sd_M = mean(sd),
    sd_U = quantile(sd, 0.975),
    bp_L = quantile(bust_prob, 0.025),
    bp_M = mean(bust_prob),
    bp_U = quantile(bust_prob, 0.975),
  ) 
df_plot_musdbp_QB

###
df_plot_musdbp_QB_1 =
  df_plot_musdbp_QB %>%
  pivot_longer(-c(draft_pick, QB), names_to="quantity") %>%
  mutate(param = str_sub(quantity,1,2), letter = str_sub(quantity,-1)) %>%
  select(-quantity) %>%
  pivot_wider(names_from = letter, values_from = value, names_prefix = "value_") 
df_plot_musdbp_1

### plot conditional mean mu(x,pos) and standard deviation sd(x,pos) and bust_prob(x,pos)
plot_condLinesByPos_mu = 
  df_plot_musdbp_1 %>%
  filter(param == "mu") %>%
  ggplot(aes(x=draft_pick)) + 
  geom_ribbon(aes(ymin = value_L, ymax = value_U, fill=pos), alpha=0.25) +
  geom_line(aes(y=value_M, color=pos), linewidth=1) +
  xlab("Draft position") +
  ylab("Percentage of cap") +
  theme(
    legend.text = element_text(size=16),
    legend.title = element_text(size=16),
  ) +
  labs(title = "Estimated \U03BC(x,pos)") +
  scale_y_continuous(labels = percent_format()) +
  scale_color_brewer(name="Position", palette = "Paired", direction=-1) +
  scale_fill_brewer(name="Position", palette = "Paired", direction=-1) +
  scale_x_continuous(breaks=seq(1,32*9,by=32*2))
# plot_condLinesByPos_mu

plot_condLinesByPos_sd = 
  df_plot_musdbp_1 %>%
  filter(param == "sd") %>%
  ggplot(aes(x=draft_pick)) + 
  geom_ribbon(aes(ymin = value_L, ymax = value_U, fill=pos), alpha=0.25) +
  geom_line(aes(y=value_M, color=pos), linewidth=1) +
  xlab("Draft position") +
  ylab("Percentage of cap") +
  theme(
    legend.text = element_text(size=16),
    legend.title = element_text(size=16),
  ) +
  labs(title = "Estimated sd(x,pos)") +
  scale_y_continuous(labels = percent_format()) +
  scale_color_brewer(name="Position", palette = "Paired", direction=-1) +
  scale_fill_brewer(name="Position", palette = "Paired", direction=-1) +
  scale_x_continuous(breaks=seq(1,32*9,by=32*2))
# plot_condLinesByPos_sd

plot_condLinesByPos_bp = 
  df_plot_musdbp_1 %>%
  filter(param == "bp") %>%
  ggplot(aes(x=draft_pick)) + 
  geom_ribbon(aes(ymin = value_L, ymax = value_U, fill=pos), alpha=0.25) +
  geom_line(aes(y=value_M, color=pos), linewidth=1) +
  xlab("Draft position") +
  ylab("Probability") +
  theme(
    legend.text = element_text(size=16),
    legend.title = element_text(size=16),
  ) +
  labs(title = "Estimated bp(x,pos)") +
  scale_color_brewer(name="Position", palette = "Paired", direction=-1) +
  scale_fill_brewer(name="Position", palette = "Paired", direction=-1) +
  scale_x_continuous(breaks=seq(1,32*9,by=32*2))
# plot_condLinesByPos_bp

# plot_condLinesByPos_legend = get_legend(plot_condLinesByPos_mu)
# as_ggplot(plot_condLinesByPos_legend)

plot_condLinesByPos = 
  plot_condLinesByPos_mu +
  theme(legend.position="none") +
  plot_condLinesByPos_sd +
  theme(legend.position="none") +
  plot_condLinesByPos_bp 
# plot_condLinesByPos
ggsave("plots_byPos/plot_condLinesSE_byPos.png", width=15, height=4)

### plot conditional mean mu(x,QB) and standard deviation sd(x,QB) and bust_prob(x,QB)
colorQB = brewer.pal(n=11, "Paired")[5]
# colorQB = "gold"
colors_QB = c("QB" = colorQB, "not QB" = "black")

plot_condLinesByQB_mu = 
  df_plot_musdbp_QB_1 %>%
  filter(param == "mu") %>%
  ggplot(aes(x=draft_pick)) + 
  geom_ribbon(aes(ymin = value_L, ymax = value_U, fill=QB), alpha=0.25) +
  geom_line(aes(y=value_M, color=QB), linewidth=1) +
  xlab("Draft position") +
  ylab("Percentage of cap") +
  theme(
    legend.text = element_text(size=16),
    legend.title = element_text(size=16),
  ) +
  labs(title = "Estimated \U03BC(x,pos)") +
  scale_y_continuous(labels = percent_format()) +
  scale_color_manual(name="Position", values = colors_QB) +
  scale_fill_manual(name="Position", values = colors_QB) +
  scale_x_continuous(breaks=seq(1,32*9,by=32*2))
# plot_condLinesByQB_mu

plot_condLinesByQB_sd = 
  df_plot_musdbp_QB_1 %>%
  filter(param == "sd") %>%
  ggplot(aes(x=draft_pick)) + 
  geom_ribbon(aes(ymin = value_L, ymax = value_U, fill=QB), alpha=0.25) +
  geom_line(aes(y=value_M, color=QB), linewidth=1) +
  xlab("Draft position") +
  ylab("Percentage of cap") +
  theme(
    legend.text = element_text(size=16),
    legend.title = element_text(size=16),
  ) +
  labs(title = "Estimated sd(x,pos)") +
  scale_y_continuous(labels = percent_format()) +
  scale_color_manual(name="Position", values = colors_QB) +
  scale_fill_manual(name="Position", values = colors_QB) +
  scale_x_continuous(breaks=seq(1,32*9,by=32*2))
# plot_condLinesByQB_sd

plot_condLinesByQB_bp = 
  df_plot_musdbp_QB_1 %>%
  filter(param == "bp") %>%
  ggplot(aes(x=draft_pick)) + 
  geom_ribbon(aes(ymin = value_L, ymax = value_U, fill=QB), alpha=0.25) +
  geom_line(aes(y=value_M, color=QB), linewidth=1) +
  xlab("Draft position") +
  ylab("Probability") +
  theme(
    legend.text = element_text(size=16),
    legend.title = element_text(size=16),
  ) +
  labs(title = "Estimated bp(x,pos)") +
  scale_color_manual(name="Position", values = colors_QB) +
  scale_fill_manual(name="Position", values = colors_QB) +
  scale_x_continuous(breaks=seq(1,32*9,by=32*2))
# plot_condLinesByQB_bp

# plot_condLinesByQB_legend = get_legend(plot_condLinesByQB_mu)
# as_ggplot(plot_condLinesByQB_legend)

plot_condLinesByQB = 
  plot_condLinesByQB_mu +
  theme(legend.position="none") +
  plot_condLinesByQB_sd +
  theme(legend.position="none") +
  plot_condLinesByQB_bp 
# plot_condLinesByQB
ggsave("plots_byPos/plot_condLinesSE_byQB.png", width=15, height=4)

#############################################################
### visualize conditional density P(y | x, QB, not bust)  ###
#############################################################

### posterior summary of the density
y_grid = seq(0.005,0.40,length.out=100)
y_grid

get_density_df_byQB <- function(y) {
  print(paste0("computing density for y=",y))
  df_post_draws_byQB %>% 
    select(draft_pick, QB, all_of(starts_with("shape")), all_of(starts_with("bust_prob"))) %>% 
    mutate(
      y = y,
      density = dbeta(y, shape1, shape2),
      density_times_bp = density*(1-bust_prob)
    ) %>%
    group_by(draft_pick,QB,y) %>%
    summarise(
      density_L =  quantile(density, .025),
      density_M = mean(density),
      density_U = quantile(density, .975),
      density_times_bp_L =  quantile(density_times_bp, .025),
      density_times_bp_M = mean(density_times_bp),
      density_times_bp_U = quantile(density_times_bp, .975),
      .groups = "drop"
    )
}

# bind_rows(lapply(c(0.005, 0.01), get_density_df_byQB))
df_post_summary_density = bind_rows(lapply(y_grid, get_density_df_byQB))
df_post_summary_density

### plot posterior conditional density
plot_post_density_full_rdsall = 
  df_post_summary_density %>%
  # filter(draft_pick %in% c(seq(1,32*7,by=32/2))) %>%
  filter(draft_pick %in% c(seq(1,32*7,by=32/1))) %>%
  ggplot(aes(x=y, 
             color=fct_reorder(factor(draft_pick), -draft_pick),
             fill=fct_reorder(factor(draft_pick), -draft_pick))
  ) +
  geom_line(aes(y=density_times_bp_M), linewidth=1) +
  geom_ribbon(aes(ymin = density_times_bp_L, ymax=density_times_bp_U), alpha = 0.35) +
  facet_wrap(~QB) +
  xlab("Percentage of cap") +
  ylab("Density") +
  scale_color_brewer(name="Draft\nposition", palette = "Set1", direction=-1) +
  scale_fill_brewer(name="Draft\nposition", palette = "Set1", direction=-1) +
  scale_x_continuous(labels = percent_format()) +
  theme(
    # axis.text.x = element_text(size = 10),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank()
  ) 
# plot_post_density_full_rdsall
ggsave("plots_byPos/plot_post_density_full_rdsall_byQB.png", width=11, height=4)

# ###
# plot_post_density_full_rd1 = 
#   df_post_summary_density %>%
#   filter(draft_pick %in% c(seq(1,32,by=2))) %>%
#   ggplot(aes(x=y, color=fct_reorder(factor(draft_pick), -draft_pick))) +
#   geom_line(aes(y=density_times_bp_M), linewidth=1) +
#   facet_wrap(~QB) +
#   xlab("apy cap pct") +
#   ylab("density") +
#   labs(title = "density of performance outcome Y") +
#   scale_color_discrete(name = "draft pick") +
#   theme(
#     axis.text.x = element_text(size = 10),
#     axis.text.y=element_blank(),
#     axis.ticks.y=element_blank()
#   ) 
# # plot_post_density_full_rd1
# ggsave("plots_byPos/plot_post_density_full_rd1_byQB.png", width=11, height=5)

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

######################################################
### Performance value V(x,QB) \propto E[G(Y)|x,QB] ###
######################################################

### the step success function
G_step_func <- function(q) { function(y) { as.numeric(y>q) } }
### the G curve success function
G_Scurve_func <- function(a, b) { function(y) { pbeta(y, a, b) } }
### S curve string description
betaCdfStr <- function(a,b) { paste0("g(y) = S(\U003B1=",a,", \U03B2=",b,")(y)") }

### posterior summary of beta shape parameters and bust probability
df_post_draws_shapeparams_QB = 
  df_post_draws_byQB %>%
  select(draw, draft_pick, QB, all_of(contains("shape")), all_of(contains("bust_prob"))) %>%
  left_join(compensation_1C) %>%
  select(-compensation_v1) %>%
  rename(cost = rookie_contract_cap_pct) %>%
  relocate(cost, .after = draft_pick)
df_post_draws_shapeparams_QB

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
  vals = numeric(nrow(df_post_draws_shapeparams_QB))
  for (j in 1:nrow(df_post_draws_shapeparams_QB)) {
    params_j = df_post_draws_shapeparams_QB[j,]
    print(paste0("j=",j,"/",nrow(df_post_draws_shapeparams_QB), 
                 ", pick=",params_j$draft_pick,", draw=",params_j$draw,", QB=",params_j$QB))
    
    integrand = G_times_density(
      bust_prob = params_j$bust_prob, 
      shape1 = params_j$shape1, 
      shape2 = params_j$shape2, 
      cost = params_j$cost,
      G_func=G_func, 
      surplus=surplus
    )
    
    safe_integrate <- function(integrand, lower = 0, upper = 1) {
      result <- tryCatch(
        integrate(integrand, lower, upper),
        error = function(e) {
          message("Error encountered, retrying with rel.tol = 1e-15")
          integrate(integrand, lower, upper, rel.tol = 1e-15)
        }
      )
      return(result)
    }
    
    integral = safe_integrate(integrand)
    
    vals[j] = integral$value
  }
  df_results = df_post_draws_shapeparams_QB
  df_results$v = vals
  df_results_1 = 
    df_results %>%
    ### group_by(draw,QB) %>%
    ##### value relative to first QB pick
    group_by(draw) %>%
    mutate(v1 = v/first(v)) %>%
    group_by(draft_pick,QB) %>%
    reframe(
      v_L = quantile(v, .025),
      v_M = mean(v),
      v_U = quantile(v, 0.975),
      v1_L = quantile(v1, .025),
      v1_M = mean(v1),
      v1_U = quantile(v1, 0.975),
    )
  df_results_1
  df_V_G = df_results_1 %>% mutate(desc = desc, type = type, surplus = surplus, q=q)
  df_V_G
}

### get V_G(x) for step function G(y) = 1{y>r}
OVERWRITE = FALSE
USE_SAVED_COMPUTATIONS = TRUE
pd_ = 1

filename_func_1 = "plots_byPos/df_V_G_step_1.csv"
fit_func_1 = !file.exists(filename_func_1) | OVERWRITE | !USE_SAVED_COMPUTATIONS
if (fit_func_1) {
  # q_ = 0.075
  q_ = q_grid[1]
  df_V_G_step_1 = get_df_V_G_QB(
    G_func=G_step_func(q=q_), desc=paste0("g(y) = 1{y>",formattable::percent(q_,pd_),"}"),q=q_, type="step"
  )
  write_csv(df_V_G_step_1, filename_func_1)
} else {
  df_V_G_step_1 = read_csv(filename_func_1)
}
df_V_G_step_1

filename_func_2 = "plots_byPos/df_V_G_step_2.csv"
fit_func_2 = !file.exists(filename_func_2) | OVERWRITE | !USE_SAVED_COMPUTATIONS
if (fit_func_2) {
  # q_ = 0.10
  q_ = q_grid[2]
  df_V_G_step_2 = get_df_V_G_QB(
    G_func=G_step_func(q=q_), desc=paste0("g(y) = 1{y>",formattable::percent(q_,pd_),"}"),q=q_, type="step"
  )
  write_csv(df_V_G_step_2, filename_func_2)
} else {
  df_V_G_step_2 = read_csv(filename_func_2)
}
df_V_G_step_2

filename_func_3 = "plots_byPos/df_V_G_step_3.csv"
fit_func_3 = !file.exists(filename_func_3) | OVERWRITE | !USE_SAVED_COMPUTATIONS
if (fit_func_3) {
  # q_ = 0.125
  q_ = q_grid[3]
  df_V_G_step_3 = get_df_V_G_QB(
    G_func=G_step_func(q=q_), desc=paste0("g(y) = 1{y>",formattable::percent(q_,pd_),"}"),q=q_, type="step"
  )
  write_csv(df_V_G_step_3, filename_func_3)
} else {
  df_V_G_step_3 = read_csv(filename_func_3)
}
df_V_G_step_3

filename_func_4 = "plots_byPos/df_V_G_step_4.csv"
fit_func_4 = !file.exists(filename_func_4) | OVERWRITE | !USE_SAVED_COMPUTATIONS
if (fit_func_4) {
  # q_ = 0.15
  q_ = q_grid[4]
  df_V_G_step_4 = get_df_V_G_QB(
    G_func=G_step_func(q=q_), desc=paste0("g(y) = 1{y>",formattable::percent(q_,pd_),"}"),q=q_, type="step"
  )
  write_csv(df_V_G_step_4, filename_func_4)
} else {
  df_V_G_step_4 = read_csv(filename_func_4)
}
df_V_G_step_4

filename_func_5 = "plots_byPos/df_V_G_step_5.csv"
fit_func_5 = !file.exists(filename_func_5) | OVERWRITE | !USE_SAVED_COMPUTATIONS
if (fit_func_5) {
  # q_ = 0.15
  q_ = q_grid[5]
  df_V_G_step_5 = get_df_V_G_QB(
    G_func=G_step_func(q=q_), desc=paste0("g(y) = 1{y>",formattable::percent(q_,pd_),"}"),q=q_, type="step"
  )
  write_csv(df_V_G_step_5, filename_func_5)
} else {
  df_V_G_step_5 = read_csv(filename_func_5)
}
df_V_G_step_5

filename_func_id = "plots_byPos/df_V_G_step_id.csv"
fit_func_id = !file.exists(filename_func_id) | OVERWRITE | !USE_SAVED_COMPUTATIONS
if (fit_func_id) {
  df_V_G_id = get_df_V_G_QB(G_func=function(y) { y }, desc=paste0("g(y) = y"), type="linear")
  write_csv(df_V_G_id, filename_func_id)
} else {
  df_V_G_id = read_csv(filename_func_id)
}
df_V_G_id

### visualize
plot_VG = 
  bind_rows(
    df_V_G_step_1,
    df_V_G_step_2,
    df_V_G_step_3,
    df_V_G_step_4,
    df_V_G_step_5,
    df_V_G_id %>% mutate(desc="Expected\nperformance\nvalue"),
  ) %>% 
  mutate(q1 = formattable::percent(q,pd_)) %>%
  mutate(ordering = ifelse(QB == "QB", 1, 2)) %>%
  # ggplot(aes(x=draft_pick)) +
  ggplot(aes(x = draft_pick, y = v1_M)) +
  facet_wrap(~reorder(QB,ordering)) +
  geom_ribbon(aes(ymin = v1_L, ymax=v1_U, fill = factor(q1)), 
              data = . %>% filter(!is.na(q)), alpha = 0.35) +
  geom_line(linewidth=2, aes(color=factor(q1)), data = . %>% filter(!is.na(q))) +
  geom_line(linewidth=2, aes(linetype=desc), data = . %>% filter(is.na(q)) ) +
  scale_linetype_manual(name="", values = c(
    "dotted", "solid"
  )) +
  ylab("Value relative to first QB pick") +
  xlab("Draft position") +
  theme(legend.key.width=unit(2.5,"cm")) +
  scale_color_brewer(
    name="Right tail probability\neliteness cutoff\n(percentage of cap)",
    palette = "Set1", na.translate=F
  ) +
  scale_fill_brewer(
    name="Right tail probability\neliteness cutoff\n(percentage of cap)",
    palette = "Set1", na.translate=F
  ) +
  theme(
    legend.text = element_text(size=16),
    legend.title = element_text(size=16),
  ) +
  
  # scale_y_continuous(limits=c(0,1)) +
  scale_x_continuous(breaks=seq(1,32*9,by=32*2))
# plot_VG
ggsave("plots_byPos/plot_G_valueCurves_byQB.png",width=11, height=4)

# ### get V_G(x) for 1 function G(y) = 1
# df_V_G_1f = get_df_V_G_QB(G_func=function(y) { 1 }, desc=paste0("g(y) = 1"))
# df_V_G_1f
# ### all values should be 1 (integrates to 1)
# mean(abs(df_V_G_1f$V_G))

# ### get V_G(x) for G curve function 
# get_df_V_G_Scurve <- function(a,b,surplus=FALSE) {
#   get_df_V_G_QB(
#     G_func=G_Scurve_func(a,b),
#     desc = betaCdfStr(a,b),
#     surplus = surplus,
#     type="s"
#   )
# }
# # df_V_G_Scurve_1 = get_df_V_G_Scurve(a=6, b=35)
# df_V_G_Scurve_1 = get_df_V_G_Scurve(a=6, b=55)
# df_V_G_Scurve_1
# # df_V_G_Scurve_2 = get_df_V_G_Scurve(a=5, b=60)
# df_V_G_Scurve_2 = get_df_V_G_Scurve(a=8, b=35)
# df_V_G_Scurve_2

#######################################################
### Surplus value V(x,QB) \propto E[G(Y-cost)|x,QB] ###
#######################################################

### get V_G(x) for step function G(y) = 1{y>r}
OVERWRITE = FALSE
USE_SAVED_COMPUTATIONS = TRUE
pd_ = 1

filename_func_1 = "plots_byPos/df_V_G_step_1_S.csv"
fit_func_1 = !file.exists(filename_func_1) | OVERWRITE | !USE_SAVED_COMPUTATIONS
if (fit_func_1) {
  # q_ = 0.075
  q_ = q_grid[1]
  df_V_G_step_1_S = get_df_V_G_QB(
    G_func=G_step_func(q=q_), surplus = T, desc=paste0("g(y) = 1{y>",formattable::percent(q_,pd_),"}"),q=q_, type="step"
  )
  write_csv(df_V_G_step_1_S, filename_func_1)
} else {
  df_V_G_step_1_S = read_csv(filename_func_1)
}
df_V_G_step_1_S

filename_func_2 = "plots_byPos/df_V_G_step_2_S.csv"
fit_func_2 = !file.exists(filename_func_2) | OVERWRITE | !USE_SAVED_COMPUTATIONS
if (fit_func_2) {
  # q_ = 0.10
  q_ = q_grid[2]
  df_V_G_step_2_S = get_df_V_G_QB(
    G_func=G_step_func(q=q_), surplus = T, desc=paste0("g(y) = 1{y>",formattable::percent(q_,pd_),"}"),q=q_, type="step"
  )
  write_csv(df_V_G_step_2_S, filename_func_2)
} else {
  df_V_G_step_2_S = read_csv(filename_func_2)
}
df_V_G_step_2_S

filename_func_3 = "plots_byPos/df_V_G_step_3_S.csv"
fit_func_3 = !file.exists(filename_func_3) | OVERWRITE | !USE_SAVED_COMPUTATIONS
if (fit_func_3) {
  # q_ = 0.125
  q_ = q_grid[3]
  df_V_G_step_3_S = get_df_V_G_QB(
    G_func=G_step_func(q=q_), surplus = T, desc=paste0("g(y) = 1{y>",formattable::percent(q_,pd_),"}"),q=q_, type="step"
  )
  write_csv(df_V_G_step_3_S, filename_func_3)
} else {
  df_V_G_step_3_S = read_csv(filename_func_3)
}
df_V_G_step_3_S

filename_func_4 = "plots_byPos/df_V_G_step_4_S.csv"
fit_func_4 = !file.exists(filename_func_4) | OVERWRITE | !USE_SAVED_COMPUTATIONS
if (fit_func_4) {
  # q_ = 0.15
  q_ = q_grid[4]
  df_V_G_step_4_S = get_df_V_G_QB(
    G_func=G_step_func(q=q_), surplus = T, desc=paste0("g(y) = 1{y>",formattable::percent(q_,pd_),"}"),q=q_, type="step"
  )
  write_csv(df_V_G_step_4_S, filename_func_4)
} else {
  df_V_G_step_4_S = read_csv(filename_func_4)
}
df_V_G_step_4_S

filename_func_5 = "plots_byPos/df_V_G_step_5_S.csv"
fit_func_5 = !file.exists(filename_func_5) | OVERWRITE | !USE_SAVED_COMPUTATIONS
if (fit_func_5) {
  # q_ = 0.15
  q_ = q_grid[5]
  df_V_G_step_5_S = get_df_V_G_QB(
    G_func=G_step_func(q=q_), surplus = T, desc=paste0("g(y) = 1{y>",formattable::percent(q_,pd_),"}"),q=q_, type="step"
  )
  write_csv(df_V_G_step_5_S, filename_func_5)
} else {
  df_V_G_step_5_S = read_csv(filename_func_5)
}
df_V_G_step_5_S

filename_func_id = "plots_byPos/df_V_G_step_id_S.csv"
fit_func_id = !file.exists(filename_func_id) | OVERWRITE | !USE_SAVED_COMPUTATIONS
if (fit_func_id) {
  df_V_G_id_S = get_df_V_G_QB(G_func=function(y) { y }, surplus = T, desc=paste0("g(y) = y"), type="linear")
  write_csv(df_V_G_id_S, filename_func_id)
} else {
  df_V_G_id_S = read_csv(filename_func_id)
}
df_V_G_id_S

### visualize
plot_VG_S = 
  bind_rows(
    df_V_G_step_1_S,
    df_V_G_step_2_S,
    df_V_G_step_3_S,
    df_V_G_step_4_S,
    df_V_G_step_5_S,
    df_V_G_id_S %>% mutate(desc="Expected\nsurplus\nvalue"),
  ) %>% 
  mutate(q1 = formattable::percent(q,pd_)) %>%
  mutate(ordering = ifelse(QB == "QB", 1, 2)) %>%
  # ggplot(aes(x=draft_pick)) +
  ggplot(aes(x = draft_pick, y = v1_M)) +
  facet_wrap(~reorder(QB,ordering)) +
  geom_hline(yintercept=1, linetype="dashed", color="gray60", linewidth=1) +
  geom_hline(yintercept=0, linetype="dashed", color="gray60", linewidth=1) +
  geom_ribbon(aes(ymin = v1_L, ymax=v1_U, fill = factor(q1)), 
              data = . %>% filter(!is.na(q)), alpha = 0.35) +
  geom_line(linewidth=2, aes(color=factor(q1)), data = . %>% filter(!is.na(q))) +
  geom_line(linewidth=2, aes(linetype=desc), data = . %>% filter(is.na(q)) ) +
  scale_linetype_manual(name="", values = c(
    "longdash", "solid"
  )) +
  ylab("Value relative to first QB pick") +
  xlab("Draft position") +
  theme(legend.key.width=unit(2.5,"cm")) +
  scale_color_brewer(
    name="Right tail probability\neliteness cutoff\n(percentage of cap)",
    palette = "Set1", na.translate=F
  ) +
  scale_fill_brewer(
    name="Right tail probability\neliteness cutoff\n(percentage of cap)",
    palette = "Set1", na.translate=F
  ) +
  theme(
    legend.text = element_text(size=16),
    legend.title = element_text(size=16),
  ) +
  scale_y_continuous(breaks=seq(0,2,by=0.5)) +
  scale_x_continuous(breaks=seq(1,32*9,by=32*2))
# plot_VG_S
ggsave("plots_byPos/plot_G_surplusValueCurves_byQB.png",width=11, height=4)

# plot_SVG_2A = 
#   df_plot_SV_G_2A %>%
#   mutate(q1 = percent(q,pd_)) %>%
#   ggplot(aes(x=draft_pick, y = V_G1, color=factor(q1))) +
#   facet_wrap(~QB) +
#   geom_hline(yintercept=1, linetype="dashed", color="gray60", linewidth=1) +
#   geom_hline(yintercept=0, linetype="dashed", color="gray60", linewidth=1) +
#   geom_line(linewidth=2, data = . %>% filter(!is.na(q))) +
#   geom_line(linewidth=2, color="black", aes(linetype=desc), data = . %>% filter(is.na(q)) ) 


##############################
### OLD Surplus value code ###
##############################

# ### posterior conditional surplus density
# df_post_summary_density_surplus = 
#   df_post_summary_density %>%
#   left_join(compensation_1C) %>%
#   rename(cost = rookie_contract_cap_pct) %>%
#   mutate(s = y - cost) %>%
#   relocate(s, .after = y) %>%
#   relocate(cost, .after = s) %>%
#   select(-compensation_v1) %>%
#   arrange(draft_pick,y)
# df_post_summary_density_surplus
# 
# ### plot posterior conditional density
# plot_post_surplus_density_full_rd1 = 
#   df_post_summary_density_surplus %>%
#   filter(draft_pick %in% c(seq(1,32,by=2))) %>%
#   ggplot(aes(x=s, color=fct_reorder(factor(draft_pick), -draft_pick))) +
#   geom_line(aes(y=density_times_bp_M), linewidth=1) +
#   facet_wrap(~QB) +
#   xlab("apy cap pct") +
#   ylab("density") +
#   labs(title = "density of surplus S") +
#   scale_color_discrete(name = "draft pick") +
#   theme(
#     axis.text.x = element_text(size = 10),
#     axis.text.y=element_blank(),
#     axis.ticks.y=element_blank()
#   ) 
# # plot_post_surplus_density_full_rd1
# ggsave("plots_byPos/plot_post_surplus_density_full_rd1_byQB.png", width=11, height=5)
# 
# ### plot posterior conditional density
# plot_post_surplus_density_full_rdsall = 
#   df_post_summary_density_surplus %>%
#   filter(draft_pick %in% c(seq(1,32*7,by=32/2))) %>%
#   ggplot(aes(x=s, color=fct_reorder(factor(draft_pick), -draft_pick))) +
#   geom_line(aes(y=density_times_bp_M), linewidth=1) +
#   facet_wrap(~QB) +
#   xlab("apy cap pct") +
#   ylab("density") +
#   labs(title = "density of surplus S") +
#   scale_color_discrete(name = "draft pick") +
#   theme(
#     axis.text.x = element_text(size = 10),
#     axis.text.y=element_blank(),
#     axis.ticks.y=element_blank()
#   ) 
# ggsave("plots_byPos/plot_post_surplus_density_full_rdsall_byQB.png", width=11, height=5)
# 
# # ### plot posterior conditional density
# # plot_post_surplus_density_rd1 = 
# #   df_post_summary_density_surplus %>%
# #   filter(draft_pick %in% c(seq(1,32,by=2))) %>%
# #   ggplot(aes(x=s, color=fct_reorder(factor(draft_pick), -draft_pick))) +
# #   geom_line(aes(y=density_M), linewidth=1) +
# #   facet_wrap(~QB) +
# #   xlab("apy cap pct") +
# #   ylab("density") +
# #   labs(title = "surplus density (given not a bust)") +
# #   scale_color_discrete(name = "draft pick") +
# #   theme(
# #     axis.text.x = element_text(size = 10),
# #     axis.text.y=element_blank(),
# #     axis.ticks.y=element_blank()
# #   ) 
# # # plot_post_surplus_density_rd1
# # ggsave("plots_byPos/plot_post_surplus_density_rd1_byQB.png", width=11, height=5)
# # 
# # ### plot posterior conditional density
# # plot_post_surplus_density_rdsall = 
# #   df_post_summary_density_surplus %>%
# #   filter(draft_pick %in% c(seq(1,32*7,by=32/2))) %>%
# #   ggplot(aes(x=s, color=fct_reorder(factor(draft_pick), -draft_pick))) +
# #   geom_line(aes(y=density_M), linewidth=1) +
# #   facet_wrap(~QB) +
# #   xlab("apy cap pct") +
# #   ylab("density") +
# #   labs(title = "surplus density (given not a bust)") +
# #   scale_color_discrete(name = "draft pick") +
# #   theme(
# #     axis.text.x = element_text(size = 10),
# #     axis.text.y=element_blank(),
# #     axis.ticks.y=element_blank()
# #   ) 
# # ggsave("plots_byPos/plot_post_surplus_density_rdsall_byQB.png", width=11, height=5)
# 
# ### get V_G(x) for 1 function G(y) = 1
# df_V_G_1fs = get_df_V_G_QB(G_func=function(y) { 1 }, desc=paste0("g(y) = 1"), surplus=TRUE)
# df_V_G_1fs
# ### all values should be 1 (integrates to 1)
# mean(abs(df_V_G_1fs$V_G))
# 
# ### get V_G(x) for identity function G(y) = y
# df_V_G_id_S = get_df_V_G_QB(G_func=function(y) { y }, desc=paste0("g(y) = y"), surplus=TRUE)
# df_V_G_id_S
# 
# ### get V_G(x) for step function G(y) = 1{y>r}
# pd_ = 1
# # q_ = 0.05
# q_ = 0.075
# df_V_G_step_0_S = get_df_V_G_QB(G_func=G_step_func(q=q_), desc=paste0("g(y) = 1{y>",q_,"}"), surplus=TRUE, q=q_)
# df_V_G_step_0_S
# # q_ = 0.0833333
# q_ = 0.10
# df_V_G_step_1_S = get_df_V_G_QB(G_func=G_step_func(q=q_), desc=paste0("g(y) = 1{y>",q_,"}"), surplus=TRUE, q=q_)
# df_V_G_step_1_S
# # q_ = 0.1166666667
# q_ = 0.125
# df_V_G_step_2_S = get_df_V_G_QB(G_func=G_step_func(q=q_), desc=paste0("g(y) = 1{y>",q_,"}"), surplus=TRUE, q=q_)
# df_V_G_step_2_S
# q_ = 0.15
# df_V_G_step_3_S = get_df_V_G_QB(G_func=G_step_func(q=q_), desc=paste0("g(y) = 1{y>",q_,"}"), surplus=TRUE, q=q_)
# df_V_G_step_3_S
# # q_ = 0.20
# # df_V_G_step_4_S = get_df_V_G_QB(G_func=G_step_func(q=q_), desc=paste0("g(y) = 1{y>",q_,"}"), surplus=TRUE, q=q_)
# # df_V_G_step_4_S
# 
# ### get V_G(x) for G curve function 
# # df_V_G_Scurve_1_S = get_df_V_G_Scurve(a=6, b=35, surplus=TRUE)
# df_V_G_Scurve_1_S = get_df_V_G_Scurve(a=6, b=55, surplus=TRUE)
# df_V_G_Scurve_1_S
# # df_V_G_Scurve_2_S = get_df_V_G_Scurve(a=5, b=60, surplus=TRUE)
# df_V_G_Scurve_2_S = get_df_V_G_Scurve(a=8, b=35, surplus=TRUE)
# df_V_G_Scurve_2_S
# 
# # ### visualize SV_G(x)
# # df_plot_SV_G = 
# #   bind_rows(
# #     df_V_G_step_1_S,
# #     df_V_G_step_2_S,
# #     df_V_G_step_3_S,
# #     df_V_G_Scurve_1_S,
# #     df_V_G_Scurve_2_S,
# #     # bind_rows(df_trade_market_weibull %>% mutate(QB="QB"), df_trade_market_weibull %>% mutate(QB="not QB")),
# #     # bind_rows(df_jj_1 %>% mutate(QB="QB"), df_jj_1 %>% mutate(QB="not QB")),
# #     bind_rows(df_jj_1 %>% mutate(QB="QB")),
# #     df_V_G_id_S
# #   ) #%>% select(-V_G) 
# # df_plot_SV_G
# # 
# # ###
# # plot_SVG = 
# #   df_plot_SV_G %>%
# #   ggplot(aes(x=draft_pick, y = V_G1, color=desc)) +
# #   # ggplot(aes(x=draft_pick, y = V_G, color=desc)) +
# #   facet_wrap(~QB) +
# #   geom_line(linewidth=2) +
# #   scale_color_brewer(name="E[g(S)|x,pos]/E[g(S)|x=1,qb]", palette = "Set2") +
# #   # scale_color_brewer(name=bquote(paste('sv'['g']*'(x) = E[G(Y-cost)|x]')), palette = "Set2") +
# #   xlab("draft pick") +
# #   ylab("surplus value relative to first QB pick") +
# #   # scale_y_continuous(limits=c(0,1)) +
# #   scale_x_continuous(breaks=seq(1,32*9,by=32*2))
# # # plot_SVG
# # ggsave("plots_byPos/plot_G_surplusValueCurves_byQB.png",width=15,height=5)
# 
#
# ### for CMSAC24 slides
# df_plot_SV_G_2A = 
#   bind_rows(
#     df_V_G_id_S %>% mutate(desc = "expected\nsurplus\nvalue"),
#     df_V_G_step_0_S,
#     df_V_G_step_1_S,
#     df_V_G_step_2_S,
#     df_V_G_step_3_S,
#   ) 
# df_plot_SV_G_2A
# plot_SVG_2A = 
#   df_plot_SV_G_2A %>%
#   mutate(q1 = percent(q,pd_)) %>%
#   ggplot(aes(x=draft_pick, y = V_G1, color=factor(q1))) +
#   facet_wrap(~QB) +
#   geom_hline(yintercept=1, linetype="dashed", color="gray60", linewidth=1) +
#   geom_hline(yintercept=0, linetype="dashed", color="gray60", linewidth=1) +
#   geom_line(linewidth=2, data = . %>% filter(!is.na(q))) +
#   geom_line(linewidth=2, color="black", aes(linetype=desc), data = . %>% filter(is.na(q)) ) +
#   scale_linetype_manual(name="", values = c(
#     "longdash", "solid"
#   )) +
#   scale_color_brewer(name="eliteness cutoff\n(Percentage of cap)", palette = "Set1", na.translate=F) +
#   ylab("value relative to first QB pick") +
#   xlab("draft pick") +
#   theme(legend.key.width=unit(2.5,"cm")) +
#   scale_x_continuous(breaks=seq(1,32*9,by=32*2))
# # plot_SVG_2A
# ggsave("plots_byPos/plot_G_surplusValueCurves_byQB.png",width=11,height=4)
# 
# 
# # df_plot_SV_G_2B =
# #   bind_rows(
# #     df_V_G_id_S %>% mutate(
# #       desc = "expected surplus value\nv(x,pos) ~ E(Y - cost(x)|x,pos)\n", a=1,
# #     ),
# #     df_V_G_step_025_S %>% mutate(
# #       desc = "v(x,pos) ~ P(Y - cost(x) > 0.025|x,pos)", a=2,
# #     ),
# #     df_V_G_step_05_S %>% mutate(
# #       desc = "v(x,pos) ~ P(Y - cost(x) > 0.05|x,pos)", a=3,
# #     ),
# #     df_V_G_step_1_S %>% mutate(
# #       desc = "v(x,pos) ~ P(Y - cost(x) > 0.10|x,pos)", a=4,
# #     ),
# #     df_V_G_step_2_S %>% mutate(
# #       desc = "v(x,pos) ~ P(Y - cost(x) > 0.15|x,pos)", a=5,
# #     ),
# #     # bind_rows(df_trade_market_weibull %>% mutate(QB="QB"), df_trade_market_weibull %>% mutate(QB="not QB")),
# #   ) 
# # df_plot_SV_G_2B
# # plot_SVG_2B = 
# #   df_plot_SV_G_2B %>%
# #   # ggplot(aes(x=draft_pick, y = V_G1, color=desc)) +
# #   ggplot(aes(x=draft_pick, y = V_G1, color=reorder(desc, a))) +
# #   facet_wrap(~QB) +
# #   geom_hline(yintercept=1, linetype="dashed", color="gray60", linewidth=1) +
# #   geom_hline(yintercept=0, linetype="dashed", color="gray60", linewidth=1) +
# #   geom_line(linewidth=2) +
# #   scale_color_brewer(
# #     name="",
# #     palette = "Set1"
# #   ) +
# #   xlab("draft pick") +
# #   ylab("surplus value relative to first QB pick") +
# #   scale_x_continuous(breaks=seq(1,32*9,by=32*2))
# # # plot_SVG_2B
# # ggsave("plots_byPos/plot_G_surplusValueCurves_byQB_2B.png",width=15,height=5)
# 

###############################
### visualize median(x,pos) ###
###############################

# df_summary_med_byQB = 
#   df_post_vals_byQB %>%
#   select(draft_pick, QB, all_of(starts_with("med")), all_of(starts_with("mu"))) %>%
#   left_join(compensation_1C) %>%
#   rename(compensation_M = rookie_contract_cap_pct) %>%
#   pivot_longer(-c("draft_pick","QB")) %>%
#   filter(name != "compensation_v1") %>%
#   mutate(
#     quantity = str_remove_all(name, "_L|_L1|_M|_M1|_U|_U1"),
#     letter = str_sub(name,-1,-1),
#   ) %>%
#   mutate(
#     quantity = ifelse(quantity == "med", "median", quantity),
#     quantity = ifelse(quantity == "mu", "mean", quantity),
#   ) %>%
#   select(-name) %>%
#   group_by(quantity, letter) %>%
#   mutate(value1 = value/first(value)) %>%
#   pivot_wider(names_from = c(letter), values_from=c(value, value1)) 
# # group_by(quantity, letter) %>%
# # mutate(value1 = value/first(value)) %>%
# # ungroup() %>%
# # pivot_wider(names_from = "quantity", values_from="value") 
# df_summary_med_byQB
# 
# df_summary_med_byQB %>%
#   # filter(letter=="M") %>%
#   ggplot(aes(x = draft_pick, y = value_M, color = quantity)) +
#   # geom_ribbon(aes(ymin = L, ymax = U, fill = quantity)) +
#   facet_wrap(~QB) +
#   geom_line(linewidth=2) +
#   xlab("draft pick") +
#   ylab("apy cap pct") +
#   scale_color_brewer(name = "", palette="Set1") +
#   scale_fill_brewer(name = "", palette="Set1") +
#   scale_x_continuous(breaks=seq(1,32*9,by=32*2)) 
# 
# df_summary_med_byQB %>%
#   # filter(letter=="M") %>%
#   ggplot(aes(x = draft_pick, y = value1_M, color = quantity)) +
#   # geom_ribbon(aes(ymin = L, ymax = U, fill = quantity)) +
#   facet_wrap(~QB) +
#   geom_line(linewidth=2) +
#   xlab("draft pick") +
#   ylab("apy cap pct") +
#   scale_color_brewer(name = "", palette="Set1") +
#   scale_fill_brewer(name = "", palette="Set1") +
#   scale_x_continuous(breaks=seq(1,32*9,by=32*2)) 
