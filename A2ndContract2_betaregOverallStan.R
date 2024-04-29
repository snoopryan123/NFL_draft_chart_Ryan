
###### STAN BAYSEIAN bust_spike + beta regression model of P(y|x)

##################
### load stuff ###
##################

### load stan stuff
library(rstan)
rstan_options(auto_write = TRUE)
cores = 1
NUM_ITS = 2500

### load header
source("A2ndContract1_Header.R")

############################
### train the Stan model ###
############################

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

##################################################################
### visualize mu(x), sd(x), bp(x) of our beta regression model ###
##################################################################

### posterior summary of mu and sd
df_post_summary_musd = 
  df_post_draws %>%
  select(draw, i, mu, sd, bust_prob, shape1, shape2) %>%
  pivot_longer(c("mu", "sd", "bust_prob", "shape1", "shape2"), names_to="quantity") %>%
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
df_overall_emp = full_join(df_overall_emp_musd_tail, df_overall_emp_bust_prob)
df_overall_emp

### plot conditional mean mu(x) and standard deviation sd(x) and bust_prob(x)
df_plot_musdbp = left_join(df_new, df_post_summary_musd) %>% left_join(df_overall_emp)
df_plot_musdbp

plot_empWithCondMean = 
  df_plot_musdbp %>%
  ggplot(aes(x = draft_pick)) +
  geom_point(aes(y=emp_mean_tail)) +
  geom_ribbon(aes(ymin = L_mu, ymax = U_mu), fill="gray60", alpha=0.6) +
  geom_line(aes(y = M_mu), linewidth=1) +
  xlab("draft pick") + ylab("apy cap pct") +
  labs(
    title = "conditional mean \U03BC(x) = E[Y|x]",
    subtitle="given not a bust"
  ) +
  scale_x_continuous(breaks=seq(1,32*9,by=32*2))
# plot_empWithCondMean

plot_empWithCondSd = 
  df_plot_musdbp %>%
  ggplot(aes(x = draft_pick)) +
  geom_point(aes(y=emp_sd_tail)) +
  geom_ribbon(aes(ymin = L_sd, ymax = U_sd), fill="gray60", alpha=0.6) +
  geom_line(aes(y = M_sd), linewidth=1) +
  xlab("draft pick") + ylab("apy cap pct") +
  labs(
    title = "conditional standard deviation sd(x)",
    subtitle="given not a bust"
  ) +
  scale_x_continuous(breaks=seq(1,32*9,by=32*2))
# plot_empWithCondSd

plot_empWithCondBp = 
  df_plot_musdbp %>%
  filter(draft_pick < 255) %>%
  ggplot(aes(x = draft_pick)) +
  geom_point(aes(y=emp_bust_prob)) +
  geom_ribbon(aes(ymin = L_bust_prob, ymax = U_bust_prob), fill="gray60", alpha=0.6) +
  geom_line(aes(y = M_bust_prob), linewidth=1) +
  xlab("draft pick") + ylab("probability") +
  labs(title = "conditional bust probability bp(x)") +
  scale_x_continuous(breaks=seq(1,32*9,by=32*2))
# plot_empWithCondBp

plot_empWithCondLines = plot_empWithCondMean + plot_empWithCondSd + plot_empWithCondBp
# plot_empWithCondLines
ggsave("plots_overall/plot_empWithCondLines.png", width=16, height=5)

################################################
### performance value & surplus value curves ###
################################################

### EV performance value curve
df_post_summary_perf_EV = 
  df_post_summary_musd %>%
  left_join(df_new) %>%
  select(i,draft_pick, all_of(contains("bust_prob")), all_of(contains("mu"))) %>%
  pivot_longer(-c(i,draft_pick)) %>%
  filter(!str_detect(name, "1")) %>%
  mutate(
    quantity = str_remove_all(name, "L_|L1_|M_|M1_|U_|U1_"),
    letter = str_sub(name,1,1),
  ) %>%
  select(-name) %>%
  pivot_wider(names_from = "quantity", values_from="value") %>%
  mutate(
    perf_EV = bust_cutoff/2 + (1-bust_prob)*mu
  ) %>%
  select(i, draft_pick ,letter, perf_EV) %>%
  group_by(letter) %>%
  mutate(perf_EV1 = perf_EV/first(perf_EV)) %>%
  ungroup() %>%
  pivot_wider(names_from = "letter", values_from = c("perf_EV", "perf_EV1")) %>%
  left_join(compensation_1C)
df_post_summary_perf_EV

### draft value curves using v(x) = mu(x)/mu(1)
df_post_summary_perf_EV %>%
  filter(draft_pick < 255) %>%
  ggplot(aes(x = draft_pick, y = perf_EV1_M)) +
  geom_ribbon(aes(ymin = perf_EV1_L, ymax = perf_EV1_U), fill="gray80") +
  geom_line(linewidth=1) +
  xlab("draft pick") +
  ylab("value relative to first pick") +
  labs(title = "posterior mean relative EV \U03BC(x)/\U03BC(x=1)") +
  scale_x_continuous(breaks=seq(1,32*9,by=32*2))

### surplus value curve
df_plot_surplus =
  df_post_summary_perf_EV %>%
  select(draft_pick, perf_EV_M, perf_EV_L, perf_EV_U, rookie_contract_cap_pct) %>%
  mutate(
    surplus_M = perf_EV_M - rookie_contract_cap_pct,
    surplus_L = perf_EV_L - rookie_contract_cap_pct,
    surplus_U = perf_EV_U - rookie_contract_cap_pct,
  ) %>%
  rename(compensation_M = rookie_contract_cap_pct)
df_plot_surplus
df_plot_surplus_1 =
  df_plot_surplus %>%
  pivot_longer(-draft_pick) %>%
  mutate(
    curve = str_remove_all(name, "_M|_U|_L"),
    letter = str_sub(name, nchar(name), nchar(name)),
    curve = ifelse(curve=="perf_EV", "performance", curve),
  ) %>%
  select(-name) %>%
  pivot_wider(names_from = "letter", values_from = "value") %>%
  group_by(curve) %>%
  mutate(
    M1 = M/first(M),
    L1 = L/first(L),
    U1 = U/first(U),
  )
df_plot_surplus_1
plot_surplus_condMean = 
  df_plot_surplus_1 %>%
  filter(draft_pick < 255) %>%
  ggplot(aes(x = draft_pick, color = curve, fill = curve)) +
  geom_ribbon(aes(ymin=L1, ymax=U1), alpha=0.5) +
  geom_line(aes(y=M1), linewidth=1) +
  xlab("draft pick") +
  scale_color_manual(
    name="", values=c("compensation"="firebrick", "performance"="dodgerblue2", "surplus"="forestgreen")
  ) +
  scale_fill_manual(
    name="", values=c("performance"="dodgerblue2", "surplus"="forestgreen")
  ) +
  ylab("value relative to first pick") +
  # labs(title = "posterior mean relative EV \U03BC(x)/\U03BC(x=1)") +
  scale_x_continuous(breaks=seq(1,32*9,by=32*2))
plot_surplus_condMean
ggsave("plots_overall/plot_surplus_condMean.png", width=8, height=5)

#########################################################
### visualize conditional density P(y | x, not bust)  ###
#########################################################

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
plot_post_density_rd1 = 
  df_post_summary_density %>%
  filter(draft_pick %in% c(seq(1,32,by=2))) %>%
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
# plot_post_density_rd1
ggsave("plots_overall/plot_post_density_rd1.png", width=7, height=5)

plot_post_density_rdsall = 
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
# plot_post_density_rdsall
ggsave("plots_overall/plot_post_density_rdsall.png", width=7, height=5)

### plot posterior conditional density
plot_post_density_rdsall_SE = 
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
# plot_post_density_rdsall_SE
ggsave("plots_overall/plot_post_density_rdsall_SE.png", width=7, height=5)

### plot posterior conditional density
plot_func_betareg_overall_density <- 
  function(ex_draft_picks, includeEmp=TRUE, includeErrorBars=TRUE, saveMe=F) {
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
  if (saveMe) {
    # browser()
    filepath = paste0("plots_overall/plot_density_byPick", 
                      "_", paste0(ex_draft_picks, collapse="_"),
                      if (includeEmp) "_emp" else "", 
                      if (includeErrorBars) "_SE" else "", ".png"
    )
    ggsave(filepath, p, width = 10, height=10) 
  } else {
    return(p)
  }
  # p
}

###
# plot_func_betareg_overall_density(c(seq(1,32*7,by=32/2)),F,F,F)
# plot_func_betareg_overall_density(c(seq(1,32*3,by=8), seq(32*3,32*5,by=16)),F,F,F)
# plot_func_betareg_overall_density(c(seq(1,32*2,by=32/8)),F,F,F)
###
plot_func_betareg_overall_density(c(seq(1,16,by=1)),T,F,T)
plot_func_betareg_overall_density(c(seq(17,32,by=1)),T,F,T)
plot_func_betareg_overall_density(c(seq(33,48,by=1)),T,F,T)
plot_func_betareg_overall_density(c(seq(49,64,by=1)),T,F,T)
###
plot_func_betareg_overall_density(c(seq(65,80,by=1)),T,F,T)
plot_func_betareg_overall_density(c(seq(81,112,by=2)),T,F,T)
plot_func_betareg_overall_density(c(seq(113,32*7,by=8)),T,F,T)
###

####################################################
### tail probability P(y>r|x) draft value curves ###
####################################################

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
df_post_summary_tail_prob = 
  left_join(
    df_post_summary_tail_prob_0,
    df_post_summary_perf_EV %>% 
      select(i,draft_pick,all_of(starts_with("perf_EV1")),compensation_v1)
  ) 
df_post_summary_tail_prob 

### draft value curves using v_q(x) = P(y>r|x)

### plot posterior tail probability
plot_tail_probs = 
  df_post_summary_tail_prob %>%
  filter(draft_pick < 255) %>%
  ggplot(aes(x = draft_pick, y = tail_prob_M, color=factor(q))) +
  geom_line(linewidth=1) +
  xlab("draft pick") +
  ylab("probability") +
  labs(title = "posterior mean tail probability P(y>r|x)") +
  scale_color_discrete(name="r") +
  scale_x_continuous(breaks=seq(1,32*9,by=32*2))
# plot_tail_probs

### plot posterior relative tail probability
plot_tail_probs_relative = 
  df_post_summary_tail_prob %>%
  filter(draft_pick < 255) %>%
  ggplot(aes(x = draft_pick, y = tail_prob_1_M, color=factor(q))) +
  geom_line(linewidth=1) +
  xlab("draft pick") +
  ylab("value relative to first pick") +
  labs(title = "posterior mean relative tail probability P(y>r|x)/P(y>r|x=1)") +
  # labs(title = "tail probability P(y>r|x)/P(y>r|x=1)") +
  scale_color_discrete(name="r") +
  scale_x_continuous(breaks=seq(1,32*9,by=32*2))
# plot_tail_probs_relative

ggsave("plots_overall/plot_tail_probs.png",
       plot_tail_probs + plot_tail_probs_relative,
       width=15, height=5)

### plot posterior relative tail probability with performance value curve
df_post_summary_tail_prob %>%
  filter(draft_pick < 255) %>%
  ggplot(aes(x = draft_pick, y = tail_prob_1_M, color=factor(q))) +
  geom_line(linewidth=1) +
  geom_line(aes(y = perf_EV1_M,), color="black", linetype="dashed", linewidth=1) +
  geom_line(aes(y = compensation_v1), color="black", linetype="dashed", linewidth=1) +
  xlab("draft pick") +
  ylab("value relative to first pick") +
  labs(title = "posterior mean relative tail probability P(y>r|x)/P(y>r|x=1)") +
  # labs(title = "tail probability P(y>r|x)/P(y>r|x=1)") +
  scale_color_discrete(name="r") +
  scale_x_continuous(breaks=seq(1,32*9,by=32*2))

# ### surplus value tail probability curve
# table(df_post_summary_tail_prob$q)
# # q_ = 0.10
# # q_ = 0.15
# q_ = 0.175
# df_plot_surplus_tail_prob = 
#   df_post_summary_tail_prob %>%
#   filter(q == q_) %>%
#   select(draft_pick, compensation_v1,
#          tail_prob_1_M, tail_prob_1_L, tail_prob_1_U) %>%
#   mutate(
#     surplus_1_M = tail_prob_1_M - compensation_v1,
#     surplus_1_L = tail_prob_1_L - compensation_v1,
#     surplus_1_U = tail_prob_1_U - compensation_v1,
#   ) %>%
#   rename(compensation_1_M  = compensation_v1)
# df_plot_surplus_tail_prob
# tail_prob_str = paste0("tail prob. P(y>r=", q_, "|x)")
# df_plot_surplus_tail_prob_1 = 
#   df_plot_surplus_tail_prob %>%
#   pivot_longer(-draft_pick) %>%
#   mutate(
#     curve = str_remove_all(name, "_v1|_1_M|_1_L|_1_U"),
#     letter = str_sub(name, nchar(name), nchar(name)),
#     curve = ifelse(curve=="tail_prob", tail_prob_str, curve),
#   ) %>%
#   select(-name) %>%
#   pivot_wider(names_from = "letter", values_from = "value")
# df_plot_surplus_tail_prob_1
# 
# color_vals = c("firebrick", "dodgerblue2", "forestgreen")
# color_vals = setNames(color_vals, c("compensation", tail_prob_str, "surplus"))
# fill_vals = c("dodgerblue2", "forestgreen")
# fill_vals = setNames(fill_vals, c(tail_prob_str, "surplus"))
# df_plot_surplus_tail_prob_1 %>%
#   ggplot(aes(x = draft_pick, color = curve, fill = curve)) +
#   geom_ribbon(aes(ymin=L, ymax=U), alpha=0.5) +
#   geom_line(aes(y=M), linewidth=1) +
#   xlab("draft pick") +
#   scale_color_manual(name="", values=color_vals) +
#   scale_fill_manual(name="", values=fill_vals) +
#   # scale_color_brewer(name="", palette = "Set1") +
#   ylab("value relative to first pick") +
#   # labs(title=paste0("q = ", q_)) +
#   # labs(title = "posterior mean relative EV \U03BC(x)/\U03BC(x=1)") +
#   scale_x_continuous(breaks=seq(1,32*9,by=32*2))

###########################
### G `GM Value` curves ###
###########################

### the step success function
G_step_func <- function(q) {
  function(y) { as.numeric(y>q) }
}

### plot the step success function
plot_G_step <- function(q) {
  tibble(x = seq(0,0.30,length.out=1000)) %>%
    ggplot(aes(x=x)) +
    xlab("y") + ylab("G") +
    labs(
      title="step GM value function",
      subtitle = paste0("G(y) = 1{y > ",q,"}")
    ) +
    theme(
      plot.subtitle = element_text(size=15),
    ) +
    stat_function(
      fun = G_step_func(q), 
      colour = "black", geom = "point"
    )
}
plot_G_step(q=0.10)

### the G curve success function
G_Scurve_func <- function(a, b) {
  function(y) { pbeta(y, a, b) }
}

### S curve string description
betaCdfStr <- function(a,b) { 
  # bquote(paste('G(y) = F'['Beta']*"(\U003B1=",.(a),", \U03B2=",.(b),")(y)"))
  # paste0("G(y) = betaCdf(\U003B1=",a,", \U03B2=",b,")(y)") 
  paste0("G(y) = S(\U003B1=",a,", \U03B2=",b,")(y)") 
}
# betaCdfStr <- function(a,b) {  }
  
plot_G_Scurve <- function(a,b) {
  plot_G = 
    tibble(x = seq(0,0.30,length.out=1000)) %>%
    ggplot(aes(x=x)) +
    xlab("y") + ylab("G") +
    labs(
      title="S curve GM value function",
      subtitle = betaCdfStr(a,b)
    ) +
    theme(
      plot.subtitle = element_text(size=15),
    ) +
    stat_function(
      fun = G_Scurve_func(a=a, b=b), 
      colour = "black", geom = "point"
    )
  print(paste0("G(y=0.01) = ", G_Scurve_func(a=a, b=b)(0.01)))
  print(paste0("G(y=0.10) = ", G_Scurve_func(a=a, b=b)(0.10)))
  print(paste0("G(y=0.25) = ", G_Scurve_func(a=a, b=b)(0.25)))
  plot_G
}
plot_G_Scurve(a=6,b=35)
plot_G_Scurve(a=5,b=60)

### examine quantiles of apy cap pct
# players_2C %>%
#   ggplot(aes(x=apy_cap_pct_2C)) +
#   geom_histogram(fill="black", bins=50) +
#   xlab("apy cap pct")
# 
# quantile(players_2C$apy_cap_pct_2C, 
#          c(0.5, 0.74, 0.9, 0.95, 0.98))

### plot the linear success function
plot_G_linear = 
  tibble(x = seq(0,0.30,length.out=1000)) %>%
  ggplot(aes(x=x)) +
  xlab("y") + ylab("G") +
  labs(
    title="linear GM value function",
    subtitle = paste0("G(y) = y")
  ) +
  theme(
    plot.subtitle = element_text(size=15),
  ) +
  stat_function(
    fun = function(y) y, 
    colour = "black", geom = "point"
  )
plot_G_linear

plot_G_curves = 
  plot_G_step(q=0.10) +
  plot_G_Scurve(a=5,b=60) +
  plot_G_Scurve(a=6,b=35) +
  plot_G_linear 
# plot_G_curves
ggsave("plots_overall/plot_G_curves.png", width=10, height=8)

############################################################
### G `success` function value curves V_G(x) = E[G(Y)|x] ###
############################################################

### posterior summary of beta shape parameters and bust probability
df_post_summary_shapeparams = 
  df_post_summary_musd %>%
  select(i, all_of(contains("shape")), all_of(contains("bust_prob"))) %>%
  left_join(df_new) %>% relocate(draft_pick, .after=i) %>%
  left_join(compensation_1C) %>%
  select(-compensation_v1) %>%
  rename(cost = rookie_contract_cap_pct) %>%
  relocate(cost, .after = draft_pick)
df_post_summary_shapeparams

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
 
  
  # if (surplus) {
  #   func = function(y) {
  #     density_y =  ifelse(
  #       y > bust_cutoff,
  #       (1-bust_prob)*dbeta(y,shape1,shape2),
  #       bust_prob/bust_cutoff
  #     )
  #     G_func(y-cost)*density_y
  #   }
  # } else {
  #   func = function(y) {
  #     density_y =  ifelse(
  #       y > bust_cutoff,
  #       (1-bust_prob)*dbeta(y,shape1,shape2),
  #       bust_prob/bust_cutoff
  #     )
  #     G_func(y)*density_y
  #   }
  # }
  # func
}

### get dataframe of V_G(x) = E[G(Y)|x] = ∫ G(y)•f(y|x) dy
### over each value of x
get_df_V_G <- function(
    G_func, desc="", surplus=FALSE
  ) {
  V_G <- function(x) {
    print(paste0("computing V_G(x) for draft pick x = ", x))
    dfx = df_post_summary_shapeparams %>% filter(draft_pick == x)
    dfx
    integrand_x = G_times_density(
      bust_prob=dfx$M_bust_prob, 
      shape1=dfx$M_shape1, shape2=dfx$M_shape2, 
      cost = dfx$cost,
      G_func=G_func,
      surplus=surplus
    )
    int_ = integrate(integrand_x, lower = 0, upper = 1)
    print(int_)
    int_$value
  }
  df_V_G = tibble(draft_pick = 1:256)
  V_G_values = sapply(df_V_G$draft_pick, V_G)
  df_V_G$V_G = V_G_values
  df_V_G = df_V_G %>% mutate(
    V_G1 = V_G/first(V_G),
    desc = desc,
    surplus = surplus,
  )
  df_V_G
}

### get V_G(x) for 1 function G(y) = 1
df_V_G_1f = get_df_V_G(
  G_func=function(y) { 1 },
  desc=paste0("G(y) = 1")
)
df_V_G_1f
### all values should be 1 (integrates to 1)
mean(abs(df_V_G_1f$V_G))
# max(abs(df_V_G_1f$V_G))
# hist(df_V_G_1f$V_G)

### get V_G(x) for identity function G(y) = y
df_V_G_id = get_df_V_G(
  G_func=function(y) { y }, 
  desc=paste0("G(y) = y")
)
df_V_G_id

### check that V_id(x) matches E(y|x)
df_plot_G_id_perfv = 
  bind_rows(
    df_V_G_id %>% mutate(desc = paste0("\nE[G(Y)|x], ", desc, "\n"))
    ,
    df_post_summary_perf_EV %>%
      select(draft_pick, perf_EV_M, perf_EV1_M) %>%
      rename(V_G = perf_EV_M, V_G1 = perf_EV1_M) %>%
      mutate(desc = "E[Y|x]")
  ) 
df_plot_G_id_perfv
df_plot_G_id_perfv %>%
  filter(draft_pick < 255) %>%
  ggplot(aes(x=draft_pick, y = V_G1, color=desc)) +
  geom_line(linewidth=1) +
  scale_color_brewer(name="", palette = "Set1") +
  ylab("value relative to first pick") +
  scale_y_continuous(limits=c(0,1)) +
  scale_x_continuous(breaks=seq(1,32*9,by=32*2))

### get V_G(x) for step function G(y) = 1{y>r}
q_ = 0.10
df_V_G_step = get_df_V_G(
  G_func=G_step_func(q=q_), 
  desc=paste0("G(y) = 1{y>",q_,"}")
  # desc=paste0("E[1{Y>",q_,"}|x]")
)
df_V_G_step

### check that V_G(x) matches P(y>r|x)
df_V_G_step %>%
  mutate(desc = paste0("E[G(Y)|x],\n", desc,"\n")) %>%
  bind_rows(
    df_post_summary_tail_prob %>%
      filter(q == q_) %>%
      select(draft_pick, tail_prob_M, tail_prob_1_M) %>%
      rename(V_G = tail_prob_M, V_G1 = tail_prob_1_M) %>%
      mutate(desc = paste0("P(Y>",q_,"|x)"))
  ) %>%
  ggplot(aes(x=draft_pick, y = V_G1, color=desc)) +
  geom_line(linewidth=1) +
  scale_color_brewer(name="", palette = "Set1") +
  ylab("value relative to first pick") +
  scale_y_continuous(limits=c(0,1)) +
  scale_x_continuous(breaks=seq(1,32*9,by=32*2))

### get V_G(x) for G curve function 
get_df_V_G_Scurve <- function(a,b,surplus=FALSE) {
  get_df_V_G(
    G_func=G_Scurve_func(a,b),
    # desc = paste0("E[G(Y)|x] ", ", ", betaCdfStr(a,b))
    desc = betaCdfStr(a,b),
    surplus = surplus
  )
}
df_V_G_Scurve_1 = get_df_V_G_Scurve(a=6, b=35)
df_V_G_Scurve_1
df_V_G_Scurve_2 = get_df_V_G_Scurve(a=5, b=60)
df_V_G_Scurve_2

### visualize V_G(x)
df_plot_V_G = 
  bind_rows(
    df_V_G_Scurve_1,
    df_V_G_Scurve_2,
    df_V_G_step,
    df_V_G_id
  ) %>%
  select(-V_G) 
df_plot_V_G

###
plot_VG = 
  df_plot_V_G %>%
  filter(draft_pick < 255) %>%
  ggplot(aes(x=draft_pick, y = V_G1, color=desc)) +
  geom_line(linewidth=1) +
  # scale_color_brewer(name="V(x) = E[G(Y)|x]", palette = "Set1") +
  scale_color_brewer(name=bquote(paste('V'['G']*'(x) = E[G(Y)|x]')), palette = "Set1") +
  ylab("value relative to first pick") +
  xlab("draft pick") +
  scale_y_continuous(limits=c(0,1)) +
  scale_x_continuous(breaks=seq(1,32*9,by=32*2))
# plot_VG
ggsave("plots_overall/plot_G_valueCurves.png",width=10,height=5)

### visualize V_G(x)
df_plot_V_G_A = 
  bind_rows(
    df_V_G_id
    ,
    df_V_G_Scurve_1
    ,
    # df_plot_V_G %>%
    #   # mutate(desc = paste0("V(x) = E[G(Y)|x]/E[G(Y)|x=1],\n", desc,"\n"))
    #   # mutate(desc = bquote(paste('V'['G']*'(x) = ,', desc, '\n')))
    #   # mutate(desc = paste0('V_G(x) = ,', desc, '\n'))
    #   # mutate(desc = paste0('V_G(x),', desc))
    #   # mutate(desc = latex2exp::TeX("$V_G(x)$"))
    #   mutate(desc = desc) 
    # ,
    df_post_summary_perf_EV %>% 
      select(draft_pick, compensation_v1) %>%
      rename(V_G1 = compensation_v1) %>%
      mutate(desc = "compensation")
    ,
    df_jj %>% rename(V_G1 = jj_v1) %>% mutate(desc = "Jimmy Johnson")
    ,
    df_trade_market_weibull %>%
      filter(str_detect(m, "with\n")) %>%
      rename(draft_pick = t, V_G1 = v_M) %>%
      select(draft_pick, V_G1) %>%
      mutate(desc = "weibull")
  )
df_plot_V_G_A

plot_VG1 = 
  df_plot_V_G_A %>%
  filter(draft_pick < 255) %>%
  ggplot(aes(x=draft_pick, y = V_G1, color=desc)) +
  geom_line(linewidth=1) +
  # scale_color_brewer(name="V(x) = E[G(Y)|x]", palette = "Set1") +
  scale_color_brewer(
    # name="", 
    name=bquote(paste('V'['G']*'(x) = E[G(Y)|x]/E[G(Y)|x=1]')),
    palette = "Set2"
  ) +
  # labs(subtitle = bquote(paste('V'['G']*'(x) = E[G(Y)|x]/E[G(Y)|x=1]'))) +
  ylab("value relative to first pick") +
  xlab("draft pick") +
  scale_y_continuous(limits=c(0,1)) +
  scale_x_continuous(breaks=seq(1,32*9,by=32*2))
# plot_VG1
ggsave("plots_overall/plot_G_valueCurves1.png",width=10,height=5)

###########################################
### Accounting for cost: surplus curves ###
###########################################

### compensation curve
compensation_1C

### conditional mean surplus curve
plot_surplus_condMean

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

### plot posterior conditional surplus density
plot_post_surplus_density_rd1 = 
  df_post_summary_density_surplus %>%
  filter(draft_pick %in% c(seq(1,32,by=2))) %>%
  ggplot(aes(x=s, color=factor(draft_pick))) +
  geom_line(aes(y=density_M), linewidth=1) +
  xlab("apy cap pct") +
  ylab("density") +
  labs(title = "surplus density (given not a bust)") +
  scale_color_discrete(name = "draft pick") +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank()
  ) 
# plot_post_surplus_density_rd1
ggsave("plots_overall/plot_post_surplus_density_rd1.png", width=7, height=5)

plot_post_surplus_density_rdsall = 
  df_post_summary_density_surplus %>%
  filter(draft_pick %in% c(seq(1,32*7,by=32/2))) %>%
  ggplot(aes(x=s, color=factor(draft_pick))) +
  geom_line(aes(y=density_M), linewidth=1) +
  xlab("apy cap pct") +
  ylab("density") +
  labs(title = "surplus density (given not a bust)") +
  scale_color_discrete(name = "draft pick") +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank()
  ) 
# plot_post_surplus_density_rdsall
ggsave("plots_overall/plot_post_surplus_density_rdsall.png", width=7, height=5)

### get V_G(x) for 1 function G(y) = 1
df_V_G_1fs = get_df_V_G(
  G_func=function(y) { 1 },
  desc=paste0("G(y) = 1"),
  surplus = TRUE
)
df_V_G_1fs
### all values should be 1 (integrates to 1)
mean(abs(df_V_G_1fs$V_G))

### get V_G(x) for various GM value functions G(y)
q_ = 0.10
df_V_G_step_S = get_df_V_G(
  G_func=G_step_func(q=q_), desc=paste0("G(y) = 1{y>",q_,"}"), surplus = TRUE
)
df_V_G_step_S
df_V_G_Scurve_1_S = get_df_V_G_Scurve(a=6, b=35, surplus = TRUE)
df_V_G_Scurve_1_S
df_V_G_Scurve_2_S = get_df_V_G_Scurve(a=5, b=60, surplus = TRUE)
df_V_G_Scurve_2_S
df_V_G_id_S = get_df_V_G(
  G_func=function(y) { y }, desc=paste0("G(y) = y"), surplus = TRUE
)
df_V_G_id_S

### visualize 
df_plot_V_G_S = 
  bind_rows(
    df_V_G_Scurve_1_S,
    df_V_G_Scurve_2_S,
    df_V_G_step_S,
    df_V_G_id_S
  ) %>%
  select(-V_G) 
df_plot_V_G_S

plot_V_G_S = 
  df_plot_V_G_S %>%
  filter(draft_pick < 255) %>%
  ggplot(aes(x=draft_pick, y = V_G1, color=desc)) +
  geom_line(linewidth=1) +
  scale_color_brewer(name=bquote(paste('SV'['G']*'(x) = E[G(Y-cost)|x]')), palette = "Set1") +
  ylab("surplus value relative to first pick") +
  xlab("draft pick") +
  scale_x_continuous(breaks=seq(1,32*9,by=32*2))
# plot_V_G_S
ggsave("plots_overall/plot_G_surplusValueCurves.png",width=10,height=5)

df_plot_V_G_S_A = 
  bind_rows(
    df_V_G_Scurve_1 %>%
      mutate(desc = paste0("V(G)(y), ", desc))
    ,
    df_V_G_Scurve_1_S %>% mutate(desc = paste0("SV(G)(y), ", desc))
    ,
    df_plot_V_G %>%
      filter(str_detect(desc, "= y")) %>%
      mutate(desc = paste0("V(G)(y), ", desc))
    ,
    df_plot_V_G_S %>%
      filter(str_detect(desc, "= y")) %>%
      mutate(desc = paste0("SV(G)(y), ", desc))
    ,
    # df_post_summary_perf_EV %>% 
    #   select(draft_pick, compensation_v1) %>%
    #   rename(V_G1 = compensation_v1) %>%
    #   mutate(desc = "compensation")
    # ,
    df_jj %>% rename(V_G1 = jj_v1) %>% mutate(desc = "Jimmy Johnson")
    ,
    # df_trade_market_weibull %>%
    #   filter(str_detect(m, "with\n")) %>%
    #   rename(draft_pick = t, V_G1 = v_M) %>%
    #   select(draft_pick, V_G1) %>%
    #   mutate(desc = "weibull")
  )
df_plot_V_G_S_A

plot_V_G_S_A = 
  df_plot_V_G_S_A %>%
  filter(draft_pick < 255) %>%
  ggplot(aes(x=draft_pick, y = V_G1, color=desc)) +
  geom_line(linewidth=1) +
  # scale_color_brewer(name="V(x) = E[G(Y)|x]", palette = "Set1") +
  scale_color_brewer(
    name="",
    # name=bquote(paste('V'['G']*'(x) = E[G(Y)|x]/E[G(Y)|x=1]')),
    palette = "Set2"
  ) +
  # labs(subtitle = bquote(paste('V'['G']*'(x) = E[G(Y)|x]/E[G(Y)|x=1]'))) +
  ylab("value relative to first pick") +
  xlab("draft pick") +
  # scale_y_continuous(limits=c(0,1)) +
  scale_x_continuous(breaks=seq(1,32*9,by=32*2))
# plot_V_G_S_A
ggsave("plots_overall/plot_G_surplusValueCurves1.png",width=12,height=5)

