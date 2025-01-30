
###### STAN BAYESIAN bust_spike + beta regression model of P(y|x)

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
all(vec_rhats1 < 1.1)
# hist(vec_rhats1)

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
    sd = sqrt(mu*(1-mu)/(1+phi)),
    med = ifelse(
      0.5 - bust_prob > 0, ### median_in_tail
      qbeta(0.5 - bust_prob, shape1, shape2), ### median assuming the bust spike is uniform
      0.5 * bust_cutoff / bust_prob ### median of the bust spike
    ),
  ) %>%
  left_join(df_new) %>%
  relocate(draft_pick, .after = i)
df_post_draws
sum(is.na(df_post_draws))

##################################################################
### visualize mu(x), sd(x), bp(x) of our beta regression model ###
##################################################################

### posterior summary of mu and sd
df_post_summary_musd = 
  df_post_draws %>%
  select(draw, i, mu, sd, bust_prob, shape1, shape2, med) %>%
  pivot_longer(c("mu", "sd", "bust_prob", "shape1", "shape2", "med"), names_to="quantity") %>%
  group_by(i,quantity) %>%
  summarise(
    value_L =  quantile(value, .025),
    value_M = mean(value),
    value_U = quantile(value, .975),
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
  geom_point(aes(y=emp_mean_tail), color="gray60") +
  geom_ribbon(aes(ymin = L_mu, ymax = U_mu), fill="darkslategray2", alpha=0.6) +
  geom_line(aes(y = M_mu), linewidth=1) +
  xlab("Draft position") +
  ylab("Percentage of cap") +
  labs(
    # title = "Estimated \U03BC(x)", subtitle="Given not a bust"
    title = "Estimated \U03BC(x) given not a bust"
  ) +
  scale_y_continuous(
    labels = percent_format(), 
    limits=c(0,.15),
  ) +
  scale_x_continuous(breaks=seq(1,32*9,by=32*2))
# plot_empWithCondMean

plot_empWithCondSd = 
  df_plot_musdbp %>%
  ggplot(aes(x = draft_pick)) +
  geom_point(aes(y=emp_sd_tail), color="gray60") +
  geom_ribbon(aes(ymin = L_sd, ymax = U_sd), fill="darkslategray2", alpha=0.6) +
  geom_line(aes(y = M_sd), linewidth=1) +
  xlab("Draft position") +
  ylab("Percentage of cap") +
  labs(
    # title = "Estimated sd(x)", subtitle="Given not a bust"
    title = "Estimated sd(x) given not a bust"
  ) +
  scale_y_continuous(
    labels = percent_format(), 
    limits=c(0,.15),
  ) +
  scale_x_continuous(breaks=seq(1,32*9,by=32*2))
# plot_empWithCondSd

plot_empWithCondBp = 
  df_plot_musdbp %>%
  filter(draft_pick < 255) %>%
  ggplot(aes(x = draft_pick)) +
  geom_point(aes(y=emp_bust_prob), color="gray60") +
  geom_ribbon(aes(ymin = L_bust_prob, ymax = U_bust_prob), fill="darkslategray2", alpha=0.6) +
  geom_line(aes(y = M_bust_prob), linewidth=1) +
  xlab("Draft position") +
  ylab("Probability") +
  labs(title = "Estimated bp(x)") +
  scale_y_continuous(limits=c(0,1)) +
  scale_x_continuous(breaks=seq(1,32*9,by=32*2))
# plot_empWithCondBp

plot_empWithCondLines = plot_empWithCondMean + plot_empWithCondSd + plot_empWithCondBp
# plot_empWithCondLines
ggsave(
  "plots_overall/plot_empWithCondLines.png", 
   width=15, height=4
)

######################################################
### visualize uniform bust density P(y | x, bust)  ###
######################################################
  
### empirical 
df_overall_emp_bustSpike =
  players_2C %>%
  filter(bust==1) %>%
  group_by(draft_pick) %>%
  summarise(
    emp_mean_tail = mean(apy_cap_pct_2C),
    emp_sd_tail = sd(apy_cap_pct_2C),
    .groups = "drop"
  ) 
df_overall_emp_bustSpike

plot_empBustSpikeDist =
  df_overall_emp_bustSpike %>%
  pivot_longer(-draft_pick) %>%
  mutate(
    quantity = str_remove_all(str_remove_all(name,"emp_"),"_tail"),
    quantity = paste0("Empirical conditional ", quantity, " given bust"),
  ) %>%
  ggplot(aes(x=draft_pick,y=value)) +
  facet_wrap(~quantity) +
  geom_point(color="gray60") +
  geom_smooth(se=F, method=lm, linewidth=2, color="black") +
  xlab("Draft position") +
  ylab("Percentage of cap") +
  scale_y_continuous(labels = percent_format()) +
  scale_x_continuous(breaks=seq(1,32*9,by=32*2))
# plot_empBustSpikeDist
ggsave("plots_overall/plot_empBustSpikeDist.png", width=10, height=4)

################################################
### performance value & surplus value curves ###
################################################

### performance & surplus value posterior draws
df_post_draws_perf_surplus = 
  df_post_draws %>%
  select(draw, i, draft_pick, mu, bust_prob) %>%
  mutate(perf = bust_cutoff/2 + (1 - bust_prob)*mu) %>%
  select(-c(mu, bust_prob, i)) %>%
  left_join(compensation_1C) %>%
  rename(cost = rookie_contract_cap_pct, cost1 = compensation_v1) %>%
  mutate(surplus = perf - cost) %>%
  group_by(draw) %>%
  mutate(
    perf1 = perf/first(perf),
    surplus1 = surplus/first(surplus),
  ) %>%
  ungroup() %>%
  relocate(perf1, .after = perf) %>%
  relocate(surplus1, .after = surplus)
df_post_draws_perf_surplus

df_post_summary_perf_surplus = 
  df_post_draws_perf_surplus %>%
  pivot_longer(-c(draw, draft_pick), names_to="quantity") %>%
  group_by(draft_pick,quantity) %>%
  reframe(
    value_L =  quantile(value, .025),
    value_M = mean(value),
    value_U = quantile(value, .975),
  ) %>%
  pivot_wider(names_from = "quantity", values_from = all_of(starts_with("value"))) 
names(df_post_summary_perf_surplus) = str_remove(names(df_post_summary_perf_surplus), "value_")
df_post_summary_perf_surplus

df_post_summary_perf_surplus_1 =
  df_post_summary_perf_surplus %>%
  pivot_longer(-draft_pick) %>%
  mutate(
    curve = str_remove_all(name, "M_|U_|L_"),
    letter = str_sub(name, 1, 1),
    curve = str_replace_all(curve, "perf", "performance"),
    rel = ifelse(endsWith(curve, "1"), "1", ""),
    curve = str_remove_all(curve, "1"),
  ) %>%
  select(-name) %>%
  pivot_wider(names_from = "letter", values_from = "value") %>%
  pivot_wider(names_from = "rel", values_from = c("L","M","U"), names_sep="") 
df_post_summary_perf_surplus_1

### draft value curves using v(x) = mu(x)/mu(1)
plot_post_mean_curve = 
  df_post_summary_perf_surplus %>%
  ggplot(aes(x = draft_pick, y = M_perf1)) +
  geom_ribbon(aes(ymin = L_perf1, ymax = U_perf1), fill="darkslategray2") +
  geom_line(linewidth=1) +
  xlab("draft pick") +
  ylab("value relative to first pick") +
  labs(title = "posterior mean relative EV \U03BC(x)/\U03BC(x=1)") +
  scale_x_continuous(breaks=seq(1,32*9,by=32*2))
# plot_post_mean_curve

plot_surplus_condMean = 
  df_post_summary_perf_surplus_1 %>%
  ggplot(aes(x = draft_pick, color = curve, fill = curve)) +
  geom_ribbon(aes(ymin=L1, ymax=U1), alpha=0.5) +
  geom_line(aes(y=M1), linewidth=1) +
  xlab("draft pick") +
  scale_color_manual(
    name="", values=c("cost"="firebrick", "performance"="dodgerblue2", "surplus"="forestgreen")
  ) +
  scale_fill_manual(
    name="", values=c("performance"="dodgerblue2", "surplus"="forestgreen")
  ) +
  ylab("value relative to first pick") +
  scale_x_continuous(breaks=seq(1,32*9,by=32*2))
# plot_surplus_condMean
# ggsave("plots_overall/plot_surplus_condMean.png", width=8, height=5)

#########################################################
### visualize conditional density P(y | x, not bust)  ###
#########################################################

### posterior summary of the density
y_grid = seq(0.005,0.25,length.out=100)
y_grid

get_density_df <- function(y) {
  print(paste0("computing density for y=",y))
  df_post_draws %>% 
    select(i, draw, shape1, shape2, bust_prob) %>% 
    mutate(
      y = y,
      density = dbeta(y, shape1, shape2),
      density_times_bp = density*(1-bust_prob)
    ) %>%
    group_by(i,y) %>%
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

# bind_rows(lapply(c(0.005, 0.01), get_density_df))
df_post_summary_density = bind_rows(lapply(y_grid, get_density_df)) %>% left_join(df_new)
df_post_summary_density

### plot posterior conditional density
plot_full_post_density_rdsall_SE = 
  df_post_summary_density %>%
  filter(draft_pick %in% c(seq(1,32*7,by=32/1))) %>%
  ggplot(aes(x=y, 
         color=fct_reorder(factor(draft_pick), -draft_pick),
         fill=fct_reorder(factor(draft_pick), -draft_pick))
  ) +
  geom_line(aes(y=density_times_bp_M), linewidth=1) +
  geom_ribbon(aes(ymin = density_times_bp_L, ymax=density_times_bp_U), alpha = 0.35) +
  xlab("Percentage of cap") +
  ylab("Density") +
  scale_color_brewer(name="Draft\nposition", palette = "Set1", direction=-1) +
  scale_fill_brewer(name="Draft\nposition", palette = "Set1", direction=-1) +
  scale_x_continuous(labels = percent_format()) +
  theme(
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank()
  ) 
# plot_full_post_density_rdsall_SE
ggsave("plots_overall/plot_post_density_full_rdsall_SE.png", width=7, height=4)

### plot posterior conditional density
plot_func_betareg_overall_density <- 
  function(ex_draft_picks, includeEmp=TRUE, includeErrorBars=TRUE, saveMe=F) {
  # browser()
  p = 
    bind_rows(
      df_post_summary_density,
      players_2C %>% filter(bust==0) %>% select(draft_pick, apy_cap_pct_2C)
    ) %>%
    filter(draft_pick %in% ex_draft_picks) %>%
    mutate(
      draft_pick_x = paste0("x = ", draft_pick),
      draft_pick_x = fct_reorder(draft_pick_x, draft_pick),
    ) %>%
    ggplot() +
    facet_wrap(~ draft_pick_x) +
    xlab("Percentage of cap") +
    ylab("Density") +
    scale_x_continuous(
      labels = percent_format(), 
      breaks = seq(0,1,by=0.10),
    ) +
    # labs(title = "density (given not a bust)") +
    theme(
      axis.text.x = element_text(size = 15),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()
    ) 
  if (includeEmp) {
    p = p + 
      geom_histogram(aes(x = apy_cap_pct_2C, y=after_stat(density)), fill="gray80") +
      geom_density(aes(x = apy_cap_pct_2C), linewidth=0.5, color="gray60") 
  }
  if (includeErrorBars) {
    p = p + geom_ribbon(aes(x = y, ymin = density_L, ymax=density_U), 
                        fill="darkslategray2", color="darkslategray2") 
  }
  p = p + geom_line(aes(x = y, y = density_M), linewidth=0.5)
  if (saveMe) {
    filepath = paste0("plots_overall/plot_density_byPick", 
                      "_", paste0(ex_draft_picks, collapse="_"),
                      if (includeEmp) "_emp" else "", 
                      if (includeErrorBars) "_SE" else "", ".png"
    )
    ggsave(filepath, p, width = 9, height=7) 
  } else {
    return(p)
  }
  # p
}

###
plot_func_betareg_overall_density(seq(1,64*2,by=32/4),T,T,T)
plot_func_betareg_overall_density(seq(1,64*2,by=32/4),F,T,T)
###
# plot_func_betareg_overall_density(c(seq(1,32*7,by=32/2)),F,F,F)
# plot_func_betareg_overall_density(c(seq(1,32*3,by=8), seq(32*3,32*5,by=16)),F,F,F)
# plot_func_betareg_overall_density(c(seq(1,32*2,by=32/8)),F,F,F)
# ###
# plot_func_betareg_overall_density(seq(1,64,by=32/8),T,F,T)
# plot_func_betareg_overall_density(seq(1,64*2,by=32/4),T,F,T)
# plot_func_betareg_overall_density(seq(1,64*4,by=32/2),T,F,T)
# ###
# plot_func_betareg_overall_density(seq(1,64,by=32/8),F,T,T)
# plot_func_betareg_overall_density(seq(1,64*2,by=32/4),F,T,T)
# plot_func_betareg_overall_density(seq(1,64*4,by=32/2),F,T,T)
# ###
# plot_func_betareg_overall_density(c(seq(1,16,by=1)),T,F,T)
# plot_func_betareg_overall_density(c(seq(17,32,by=1)),T,F,T)
# plot_func_betareg_overall_density(c(seq(33,48,by=1)),T,F,T)
# plot_func_betareg_overall_density(c(seq(49,64,by=1)),T,F,T)
# ###
# plot_func_betareg_overall_density(c(seq(65,80,by=1)),T,F,T)
# plot_func_betareg_overall_density(c(seq(81,112,by=2)),T,F,T)
# plot_func_betareg_overall_density(c(seq(113,32*7,by=8)),T,F,T)
# ###

#####################################################
### "Right tail probability" section of the paper ###
### tail probability P(y>r|x) draft value curves  ###
#####################################################



### posterior summary of tail probability
get_tail_prob_df <- function(q) {
  print(paste0("computing tail prob for q=",q))
  assertthat::assert_that(q > bust_cutoff)
  
  df_post_draws %>%
    mutate(
      q = q, 
      tail_prob_given_notbust = pbeta(q, shape1, shape2, lower.tail = F),
      tail_prob = (1-bust_prob)*tail_prob_given_notbust
    ) %>%
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

df_post_summary_tail_prob = bind_rows(lapply(q_grid, get_tail_prob_df)) %>% left_join(df_new)
df_post_summary_tail_prob

### draft value curves using v_q(x) = P(y>r|x)

### plot posterior tail probability
plot_tail_probs = 
  df_post_summary_tail_prob %>%
  mutate(q1 = formattable::percent(q,perc_digits)) %>%
  ggplot(aes(
    x = draft_pick, y = tail_prob_M,
  )) +
  geom_ribbon(aes(ymin = tail_prob_U, ymax=tail_prob_L,fill=factor(q1)), alpha = 0.35) +
  geom_line(linewidth=2, aes(color=factor(q1))) +
  xlab("Draft position") +
  ylab("Probability") +
  # scale_color_manual(name="Eliteness cutoff\n(percentage of cap)", values = my_palette_1) +
  # scale_fill_manual(name="Eliteness cutoff\n(percentage of cap)", values = my_palette_1) +
  scale_color_brewer(name="Right tail probability\neliteness cutoff\n(percentage of cap)", palette = "Set1") +
  scale_fill_brewer(name="Right tail probability\neliteness cutoff\n(percentage of cap)", palette = "Set1") +
  scale_x_continuous(breaks=seq(1,32*9,by=32*2))
# plot_tail_probs
ggsave("plots_overall/plot_tail_probs_raw_SE.png",
       plot_tail_probs, width=8, height=4)

df_post_summary_tail_prob_1 = 
  bind_rows(
    df_post_summary_tail_prob %>% 
      rename(v1 = tail_prob_1_M) %>% 
      select(draft_pick,q,v1, tail_prob_1_L, tail_prob_1_U) %>% 
      mutate(desc = "right_tail_prob"),
    df_plot_Massey_Thaler_0 %>% 
      rename(v1 = performance) %>% 
      select(draft_pick, v1) %>% 
      mutate(desc = "performance"),
    df_trade_market_weibull %>%
      mutate(desc = "market") %>% 
      rename(v1 = V_G1)
  )
df_post_summary_tail_prob_1

plot_tail_probs_labels = c(
  "right_tail_prob" = "Right\ntail\nprobability\n",
  "performance" = "Expected\nperformance\nvalue",
  "market" = "Fitted\ntrade\nmarket\n"
)

plot_tail_probs_relative_2_SE = 
  df_post_summary_tail_prob_1 %>%
  mutate(q1 = formattable::percent(q,perc_digits)) %>%
  ggplot(aes(x = draft_pick, y = v1)) +
  geom_ribbon(aes(ymin = tail_prob_1_L, ymax=tail_prob_1_U, fill = factor(q1)), 
              data = . %>% filter(!is.na(q)), alpha = 0.35) +
  geom_line(linewidth=2, aes(color=factor(q1)), data = . %>% filter(!is.na(q))) +
  geom_line(linewidth=2, aes(linetype=desc), data = . %>% filter(is.na(q)) ) +
  xlab("Draft position") +
  ylab("Value relative to first pick") +
  scale_linetype_manual(
    name="",
    labels = plot_tail_probs_labels,
    values=c(
      "right_tail_prob" = "solid",
      "performance" = "dotted",
      "surplus" = "longdash",
      "market" = "solid"
    ),
  ) +
  theme(legend.key.width=unit(2.5,"cm")) +
  scale_color_brewer(
    name="Right tail probability\neliteness cutoff\n(percentage of cap)",
    palette = "Set1"
  ) +
  scale_fill_brewer(
    name="Right tail probability\neliteness cutoff\n(percentage of cap)",
    palette = "Set1"
  ) +
  theme(
    legend.text = element_text(size=16),
    legend.title = element_text(size=16),
  ) +
  scale_x_continuous(breaks=seq(1,32*9,by=32*2))
# plot_tail_probs_relative_2_SE
ggsave("plots_overall/plot_tail_probs_relative_SE.png",
       plot_tail_probs_relative_2_SE, width=8, height=4)

temp = 
  players_2C %>%
  filter(position =="QB") %>%
  arrange(-apy_cap_pct_2C) 
temp

players_2C %>% filter(apy_cap_pct_2C >= 0.15) %>% mutate(n())
16/nrow(players_2C)

##############################
### Surplus density curves ###
##############################

### compensation curve
compensation_1C

### conditional mean surplus curve
# plot_surplus_condMean

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
plot_post_surplus_density_full_rd1 = 
  df_post_summary_density_surplus %>%
  filter(draft_pick %in% c(1,5,9,17,33)) %>%
  # filter(draft_pick %in% c(1,5,9,seq(17,33,by=8))) %>%
  # filter(draft_pick %in% c(seq(1,33,by=4))) %>%
  # filter(draft_pick %in% c(seq(1,32,by=8))) %>%
  ggplot(aes(x=s, 
             color=fct_reorder(factor(draft_pick), -draft_pick),
             # fill=fct_reorder(factor(draft_pick), -draft_pick)
             )
  ) +
  geom_vline(xintercept = 0, color = "gray70", linetype="dashed", linewidth=1) +
  # geom_ribbon(aes(ymin = density_times_bp_L, ymax=density_times_bp_U), alpha = 0.35) +
  geom_line(aes(y=density_times_bp_M), linewidth=1) +
  xlab("Percentage of cap") +
  ylab("Density") +
  scale_color_brewer(name="Draft\nposition", palette = "Set1", direction=-1) +
  # scale_color_discrete(name = "Draft\nposition") +
  scale_x_continuous(labels = percent_format()) +
  theme(
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank()
  ) 
# plot_post_surplus_density_full_rd1
ggsave("plots_overall/plot_post_surplus_density_full_rd1.png", width=7, height=4)

plot_post_surplus_density_full_rdsall = 
  df_post_summary_density_surplus %>%
  filter(draft_pick %in% c(seq(1,32*7,by=32/1))) %>%
  ggplot(aes(x=s, 
             color=fct_reorder(factor(draft_pick), -draft_pick),
             fill=fct_reorder(factor(draft_pick), -draft_pick)
  )
  ) +
  geom_vline(xintercept = 0, color = "gray70", linetype="dashed", linewidth=1) +
  geom_ribbon(aes(ymin = density_times_bp_L, ymax=density_times_bp_U), alpha = 0.35) +
  geom_line(aes(y=density_times_bp_M), linewidth=1) +
  xlab("Percentage of cap") +
  ylab("Density") +
  # scale_color_discrete(name = "Draft\nposition") +
  # scale_fill_discrete(name = "Draft\nposition") +
  scale_color_brewer(name="Draft\nposition", palette = "Set1", direction=-1) +
  scale_fill_brewer(name="Draft\nposition", palette = "Set1", direction=-1) +
  scale_x_continuous(labels = percent_format()) +
  theme(
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank()
  ) 
# plot_post_surplus_density_full_rdsall
ggsave("plots_overall/plot_post_surplus_density_full_rdsall.png", width=7, height=4)

######################
### Surplus curves ###
######################

### posterior summary of beta shape parameters and bust probability
df_post_draws_shapeparams = 
  df_post_draws %>%
  select(draw, i, all_of(contains("shape")), all_of(contains("bust_prob"))) %>%
  left_join(df_new) %>% relocate(draft_pick, .after=i) %>%
  left_join(compensation_1C) %>%
  select(-compensation_v1) %>%
  rename(cost = rookie_contract_cap_pct) %>%
  relocate(cost, .after = draft_pick)
df_post_draws_shapeparams

### the step success function
G_step_func <- function(q) {
  function(y) { as.numeric(y>q) }
}

### g(y)•f(y|x) OR G(y-cost)•f(y|x)
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

### get dataframe of V_G(x) = E[g(y)|x] = ∫ g(y)•f(y|x) dy
### over each value of x
get_df_V_G <- function(
    G_func, desc="", type="", surplus=FALSE, printme=TRUE, q=NA
) {
  vals = numeric(nrow(df_post_draws_shapeparams))
  for (j in 1:nrow(df_post_draws_shapeparams)) {
    params_j = df_post_draws_shapeparams[j,]
    print(paste0("j=",j,"/",nrow(df_post_draws_shapeparams), ", pick=",params_j$draft_pick,", draw=",params_j$draw))
    
    integrand = G_times_density(
      bust_prob = params_j$bust_prob, 
      shape1 = params_j$shape1, 
      shape2 = params_j$shape2, 
      cost = params_j$cost,
      G_func=G_func, 
      surplus=surplus
    )
    
    integral = integrate(integrand, lower = 0, upper = 1)
    vals[j] = integral$value
  }
  df_results = df_post_draws_shapeparams
  df_results$v = vals
  df_results_1 = 
    df_results %>%
    group_by(draw) %>%
    mutate(v1 = v/first(v)) %>%
    group_by(draft_pick) %>%
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

### SANITY CHECKING
{
  # ### SANITY CHECK 1
  # ### get V_G(x) for identity function
  # df_V_G_id_S = get_df_V_G(
  #   G_func=function(y) { y }, desc=paste0("g(y) = y"), surplus = TRUE
  # )
  # df_V_G_id_S
  # ### the two expected surplus value plots should match
  # df_plot_G_id_surplusv = 
  #   bind_rows(
  #     df_V_G_id_S %>% mutate(desc = paste0("\nE[g(S)|x],\n ", desc, "\n")),
  #     df_post_summary_perf_surplus %>%
  #       select(c("draft_pick", all_of(contains("surplus")))) %>%
  #       rename(
  #         v_L = L_surplus, 
  #         v_M = M_surplus,
  #         v_U = U_surplus,
  #         v1_L = L_surplus1, 
  #         v1_M = M_surplus1,
  #         v1_U = U_surplus1,
  #       ) %>%
  #       mutate(desc = "E[S|x]")
  #   ) 
  # df_plot_G_id_surplusv
  # df_plot_G_id_surplusv %>%
  #   ggplot(aes(x=draft_pick)) +
  #   geom_ribbon(aes(fill = desc, ymin = v1_L, ymax = v1_U)) +
  #   geom_line(aes(color = desc, y = v1_M), linewidth=1) +
  #   scale_color_brewer(name="", palette = "Set1") +
  #   ylab("value relative to first pick") +
  #   scale_x_continuous(breaks=seq(1,32*9,by=32*2))
  # df_plot_G_id_surplusv %>%
  #   ggplot(aes(x=draft_pick)) +
  #   geom_ribbon(aes(fill = desc, ymin = v_L, ymax = v_U)) +
  #   geom_line(aes(color = desc, y = v_M), linewidth=1) +
  #   scale_color_brewer(name="", palette = "Set1") +
  #   ylab("value relative to first pick") +
  #   scale_x_continuous(breaks=seq(1,32*9,by=32*2))
  
  # ### SANITY CHECK 2
  # ### get V_G(x) for 1 function g(y) = 1
  # df_V_G_1fs = get_df_V_G(
  #   G_func=function(y) { 1 },
  #   desc=paste0("g(y) = 1"),
  #   surplus = TRUE
  # )
  # df_V_G_1fs
  # ### all values should be 1 (integrates to 1)
  # mean(abs(df_V_G_1fs$v_M))
}

### get V_G(x) for various GM value functions g(y)
q_ = q_grid[1]
# q_ = 0.025
# q_ = 0.075
df_V_G_step_S1 = get_df_V_G(
  G_func=G_step_func(q=q_), desc=paste0("g(y) = 1{y>",q_,"}"), surplus = TRUE, q=q_
)
df_V_G_step_S1
q_ = q_grid[2]
# q_ = 0.05
# q_ = 0.10
df_V_G_step_S2 = get_df_V_G(
  G_func=G_step_func(q=q_), desc=paste0("g(y) = 1{y>",q_,"}"), surplus = TRUE, q=q_
)
df_V_G_step_S2
q_ = q_grid[3]
# q_ = 0.10
# q_ = 0.125
df_V_G_step_S3 = get_df_V_G(
  G_func=G_step_func(q=q_), desc=paste0("g(y) = 1{y>",q_,"}"), surplus = TRUE, q=q_
)
df_V_G_step_S3
q_ = q_grid[4]
# q_ = 0.15
df_V_G_step_S4 = get_df_V_G(
  G_func=G_step_func(q=q_), desc=paste0("g(y) = 1{y>",q_,"}"), surplus = TRUE, q=q_
)
df_V_G_step_S4

### visualize 
perc_digits_S = 1
df_plot_V_G_S_A_2A = 
  bind_rows(
    df_plot_Massey_Thaler_0 %>% 
      rename(v1_M = surplus) %>% 
      select(draft_pick, v1_M) %>% 
      mutate(desc = "Expected\nsurplus\nvalue", type = "", surplus = TRUE, q = NA),
    df_V_G_step_S1,
    df_V_G_step_S2,
    df_V_G_step_S3,
    df_V_G_step_S4,
  )
# df_plot_V_G_S_A_2A
plot_V_G_S_A_2A = 
  df_plot_V_G_S_A_2A %>%
  mutate(q1 = formattable::percent(q, perc_digits_S)) %>%
  # filter(draft_pick < 255) %>%
  ggplot(aes(x=draft_pick, y = v1_M)) +
  geom_hline(yintercept=1, linetype="dashed", color="gray70", linewidth=1) +
  geom_hline(yintercept=0, linetype="dashed", color="gray70", linewidth=1) +
  geom_ribbon(aes(ymin = v1_L, ymax=v1_U, fill=factor(q1)), data = . %>% filter(!is.na(q1)), alpha = 0.35) +
  geom_line(aes(color=factor(q1)), linewidth=2, data = . %>% filter(!is.na(q1))) +
  geom_line(linewidth=2, color="black", aes(linetype=desc), data = . %>% filter(is.na(q1)) ) +
  scale_linetype_manual(name="", values = c(
    "longdash", "solid"
  )) +
  ylab("Value relative to first pick") +
  xlab("Draft position") +
  theme(legend.key.width=unit(2.5,"cm")) +
  scale_color_brewer(
    name="Right tail probability\neliteness cutoff\n(percentage of cap)",
    palette = "Set1"
  ) +
  scale_fill_brewer(
    name="Right tail probability\neliteness cutoff\n(percentage of cap)",
    palette = "Set1"
  ) +
  theme(
    legend.text = element_text(size=16),
    legend.title = element_text(size=16),
  ) +
  # scale_color_brewer(name="Right tail probability\neliteness cutoff\n(percentage of cap)", palette = "Set1", na.translate=F) +
  # scale_fill_brewer(name="Right tail probability\neliteness cutoff\n(percentage of cap)", palette = "Set1", na.translate=F) +
  # theme(legend.key.width=unit(2.5,"cm")) +
  scale_x_continuous(breaks=seq(1,32*9,by=32*2))
# plot_V_G_S_A_2A
ggsave("plots_overall/plot_G_surplusValueCurves_2A.png",width=8,height=4)

################################################################################
#########################
### OLD surplus plots ###
#########################

  # geom_line(aes(y=density_times_bp_M), linewidth=1) +
  # xlab("Percentage of cap") +
  # ylab("Density") +
  # # scale_color_discrete(name = "Draft\nposition") +
  # # scale_fill_discrete(name = "Draft\nposition") +
  # scale_color_brewer(name="Draft\nposition", palette = "Set1", direction=-1) +
  # scale_fill_brewer(name="Draft\nposition", palette = "Set1", direction=-1) +

# df_plot_V_G_S_A = 
#   bind_rows(
#     df_V_G_Scurve_1 %>%
#       mutate(desc = paste0("E[g(Y)|x]/E[g(Y)|x=1],\n", desc, "\n"))
#     ,
#     df_V_G_Scurve_1_S %>% 
#       mutate(desc = paste0("E[g(S)|x]/E[g(S)|x=1],\n", desc, "\n"))
#     ,
#     df_V_G_id %>%
#       mutate(desc = paste0("E[g(Y)|x]/E[g(Y)|x=1],\n", desc, "\n"))
#     ,
#     df_V_G_id_S %>%
#       mutate(desc = paste0("E[g(S)|x]/E[g(S)|x=1],\n", desc, "\n"))
#     ,
#     df_V_G_step_S2 %>%
#       mutate(desc = paste0("E[g(S)|x]/E[g(S)|x=1],\n", desc, "\n"))
#     ,
#     df_V_G_step1 %>%
#       mutate(desc = paste0("E[g(Y)|x]/E[g(Y)|x=1],\n", desc, "\n"))
#     ,
#     # df_post_summary_perf_EV %>% 
#     #   select(draft_pick, compensation_v1) %>%
#     #   rename(V_G1 = compensation_v1) %>%
#     #   mutate(desc = "compensation")
#     # ,
#     df_jj %>% rename(V_G1 = jj_v1) %>% mutate(desc = "Jimmy Johnson")
#     ,
#     # df_trade_market_weibull
#   )
# df_plot_V_G_S_A
# 
# plot_V_G_S_A = 
#   df_plot_V_G_S_A %>%
#   # filter(draft_pick < 255) %>%
#   ggplot(aes(x=draft_pick, y = V_G1, color=desc)) +
#   geom_line(linewidth=2) +
#   # scale_color_brewer(name="V(x) = E[g(y)|x]", palette = "Set1") +
#   scale_color_brewer(
#     name="",
#     # name=bquote(paste('V'['G']*'(x) = E[g(y)|x]/E[g(y)|x=1]')),
#     # palette = "Set1"
#     palette = "Set2"
#   ) +
#   # labs(subtitle = bquote(paste('V'['G']*'(x) = E[g(y)|x]/E[g(y)|x=1]'))) +
#   ylab("value relative to first pick") +
#   xlab("draft pick") +
#   # scale_y_continuous(limits=c(0,1)) +
#   scale_x_continuous(breaks=seq(1,32*9,by=32*2))
# # plot_V_G_S_A
# ggsave("plots_overall/plot_G_surplusValueCurves.png",width=12,height=6)

# ### credible intervals
# df_V_G_Scurve_1_S_SE = get_df_V_G_Scurve(a=6, b=35, surplus = TRUE, SE=T)
# df_V_G_Scurve_1_S_SE
# q_ = 0.15
# df_V_G_step_S2_SE = get_df_V_G(
#   G_func=G_step_func(q=q_), desc=paste0("g(y) = 1{y>",q_,"}"), surplus = TRUE, SE=T
# )
# df_V_G_step_S2_SE
# 
# bind_rows(
#   df_V_G_Scurve_1_S_SE
#   ,
#   df_V_G_step_S2_SE
# ) %>%
#   ggplot(aes(x=draft_pick, y = V_G1, color=desc, fill=desc)) +
#   geom_line(linewidth=2) +
#   geom_ribbon(aes(ymin=V_G_L1, ymax=V_G_U1)) +
#   # scale_color_brewer(name="V(x) = E[g(y)|x]", palette = "Set1") +
#   scale_color_brewer(
#     name="",
#     # name=bquote(paste('V'['G']*'(x) = E[g(y)|x]/E[g(y)|x=1]')),
#     # palette = "Set1"
#     palette = "Set2"
#   ) +
#   scale_fill_brewer(
#     name="",
#     palette = "Set2"
#   ) +
#   # labs(subtitle = bquote(paste('V'['G']*'(x) = E[g(y)|x]/E[g(y)|x=1]'))) +
#   ylab("value relative to first pick") +
#   xlab("draft pick") +
#   # scale_y_continuous(limits=c(0,1)) +
#   scale_x_continuous(breaks=seq(1,32*9,by=32*2))

# df_plot_V_G_S_A_2B = 
#   bind_rows(
#     df_V_G_id_S %>% mutate(
#       # desc = "v(x) ~ E(y - cost(x)|x)"
#       desc = "expected surplus value\nv(x) ~ E(y - cost(x)|x)\n"
#     ),
#     df_V_G_step_S025 %>% mutate(
#       desc = "v(x) ~ P(y - cost(x) > 0.025|x)"
#     ),
#     df_V_G_step_S05 %>% mutate(
#       desc = "v(x) ~ P(y - cost(x) > 0.05|x)"
#     ),
#     df_V_G_step_S1 %>% mutate(
#       desc = "v(x) ~ P(y - cost(x) > 0.10|x)"
#     ),
#     df_V_G_step_S2 %>% mutate(
#       desc = "v(x) ~ P(y - cost(x) > 0.15|x)"
#     ),
#   )
# df_plot_V_G_S_A_2B
# plot_V_G_S_A_2B = 
#   df_plot_V_G_S_A_2B %>%
#   # filter(draft_pick < 255) %>%
#   ggplot(aes(x=draft_pick, y = V_G1, color=desc)) +
#   geom_hline(yintercept=1, linetype="dashed", color="gray60", linewidth=1) +
#   geom_hline(yintercept=0, linetype="dashed", color="gray60", linewidth=1) +
#   geom_line(linewidth=2) +
#   # scale_color_brewer(name="V(x) = E[g(y)|x]", palette = "Set1") +
#   scale_color_brewer(
#     name="",
#     # name=bquote(paste('V'['G']*'(x) = E[g(y)|x]/E[g(y)|x=1]')),
#     palette = "Set1"
#     # palette = "Set2"
#   ) +
#   # labs(subtitle = bquote(paste('V'['G']*'(x) = E[g(y)|x]/E[g(y)|x=1]'))) +
# ylab("surplus value relative to first pick") +
#   xlab("draft pick") +
#   # scale_y_continuous(limits=c(0,1)) +
#   scale_x_continuous(breaks=seq(1,32*9,by=32*2))
# # plot_V_G_S_A_2B
# # ggsave("plots_overall/plot_G_surplusValueCurves_2B.png",width=10,height=5)

###########################
### G `GM Value` curves ###
###########################

# ### the step success function
# G_step_func <- function(q) {
#   function(y) { as.numeric(y>q) }
# }
# 
# ### plot the step success function
# plot_G_step <- function(q) {
#   tibble(x = seq(0,0.30,length.out=1000)) %>%
#     ggplot(aes(x=x)) +
#     xlab("y") + ylab("g") +
#     labs(
#       title="step function",
#       # subtitle = paste0("g(y) = 1{y > ",q,"}")
#       subtitle = paste0("g(y) = 1{y > ",percent(q,0),"}")
#     ) +
#     theme(
#       plot.subtitle = element_text(size=15),
#     ) +
#     stat_function(
#       fun = G_step_func(q), 
#       colour = "black", geom = "point"
#     )
# }
# plot_G_step(q=0.15)
# ggsave("plots_overall/plot_G_curve_step.png", width=5, height=4)
# 
# ### the G curve success function
# G_Scurve_func <- function(a, b) {
#   function(y) { pbeta(y, a, b) }
# }
# 
# ### s curve string description
# betaCdfStr <- function(a,b) { 
#   # bquote(paste('g(y) = F'['Beta']*"(\U003B1=",.(a),", \U03B2=",.(b),")(y)"))
#   # paste0("g(y) = betaCdf(\U003B1=",a,", \U03B2=",b,")(y)") 
#   paste0("g(y) = s(\U003B1=",a,", \U03B2=",b,")(y)") 
# }
# # betaCdfStr <- function(a,b) {  }
# 
# plot_G_Scurve <- function(a,b,quartiles=F) {
#   max_y = 0.30
#   plot_df_G = 
#     tibble(y = seq(0,max_y,length.out=1000)) %>%
#     mutate(
#       G = G_Scurve_func(a=a, b=b)(y),
#       diffG = c(0,diff(G)),
#       diffy = c(0,diff(y)),
#       deriv = diffG/diffy,
#       derivCrosses1 = as.numeric(
#         deriv >= 1 & lag(deriv,default=0) <= 1 |
#           deriv <= 1 & lag(deriv,default=0) >= 1
#       ),
#       derivCrosses1 = ifelse(is.na(derivCrosses1),0,derivCrosses1),
#     )
#   plot_df_G
#   plot_G = 
#     plot_df_G %>%
#     ggplot(aes(x=y,y=G)) +
#     geom_point() +
#     xlab("y") + ylab("g") +
#     labs(
#       title="s curve",
#       subtitle = betaCdfStr(a,b)
#     ) +
#     theme(
#       plot.subtitle = element_text(size=15),
#     ) 
#   plot_G
#   if (quartiles) {
#     # quartiles_ = quantile( (plot_df_G)$y, c(0.25, 0.5, 0.75))
#     quartiles_ = (plot_df_G %>% filter(derivCrosses1==1))$y
#     plot_G = 
#       plot_G +
#       geom_vline(xintercept = quartiles_[1], color="gray60", linewidth=1, linetype="dashed") +
#       geom_vline(xintercept = quartiles_[2], color="gray60", linewidth=1, linetype="dashed") +
#       geom_vline(xintercept = quartiles_[3], color="gray60", linewidth=1, linetype="dashed")
#     plot_G
#     # quartiles_ = quantile( (plot_df_G)$y, c(0.25, 0.5, 0.75))
#     # plot_G = 
#     #   plot_G +
#     #   geom_vline(xintercept = quartiles_[1], color="gray60", linewidth=1, linetype="dashed") +
#     #   geom_vline(xintercept = quartiles_[2], color="gray60", linewidth=1, linetype="dashed") +
#     #   geom_vline(xintercept = quartiles_[3], color="gray60", linewidth=1, linetype="dashed")
#     # plot_G
#   }
#   plot_G
#   
#   # plot_G = 
#   #   tibble(x = seq(0,0.30,length.out=1000)) %>%
#   #   ggplot(aes(x=x)) +
#   #   xlab("y") + ylab("g") +
#   #   labs(
#   #     title="s curve outcome value function",
#   #     subtitle = betaCdfStr(a,b)
#   #   ) +
#   #   theme(
#   #     plot.subtitle = element_text(size=15),
#   #   ) +
#   #   stat_function(
#   #     fun = G_Scurve_func(a=a, b=b), 
#   #     colour = "black", geom = "point"
#   #   )
#   # if (quartiles) {
#   #   
#   # }
#   # plot_G
#   
#   # print(paste0("G(y=0.01) = ", G_Scurve_func(a=a, b=b)(0.01)))
#   # print(paste0("G(y=0.10) = ", G_Scurve_func(a=a, b=b)(0.10)))
#   # print(paste0("G(y=0.25) = ", G_Scurve_func(a=a, b=b)(0.25)))
#   return(plot_G)
# }
# # plot_G_Scurve(a=6,b=35,quartiles=T)
# plot_G_Scurve(a=6,b=55,quartiles=T)
# ggsave("plots_overall/plot_G_curve_S1.png", width=5, height=4)
# # plot_G_Scurve(a=5,b=60)
# plot_G_Scurve(a=8,b=35)
# ggsave("plots_overall/plot_G_curve_S2.png", width=5, height=4)
# 
# # plot_G_Scurve(a=6,b=35,quartiles=T)
# # plot_G_Scurve(a=12,b=70,quartiles=T)
# 
# 
# ### examine quantiles of apy cap pct
# # players_2C %>%
# #   ggplot(aes(x=apy_cap_pct_2C)) +
# #   geom_histogram(fill="black", bins=50) +
# #   xlab("apy cap pct")
# # 
# # quantile(players_2C$apy_cap_pct_2C, 
# #          c(0.5, 0.74, 0.9, 0.95, 0.98))
# 
# ### plot the linear success function
# plot_G_linear = 
#   tibble(x = seq(0,0.30,length.out=1000)) %>%
#   ggplot(aes(x=x)) +
#   xlab("y") + ylab("g") +
#   labs(
#     title="linear function",
#     subtitle = paste0("g(y) = y")
#   ) +
#   theme(
#     plot.subtitle = element_text(size=15),
#   ) +
#   stat_function(
#     fun = function(y) y, 
#     colour = "black", geom = "point"
#   )
# plot_G_linear
# ggsave("plots_overall/plot_G_curve_line.png", width=5, height=4)
# 
# ### plot the quadratic success function
# plot_G_quadratic = 
#   tibble(x = seq(0,0.30,length.out=1000)) %>%
#   ggplot(aes(x=x)) +
#   xlab("y") + ylab("g") +
#   labs(
#     title="polynomial outcome value function",
#     subtitle = paste0("g(y) = y^6")
#   ) +
#   theme(
#     plot.subtitle = element_text(size=15),
#   ) +
#   stat_function(
#     fun = function(y) y^6, 
#     colour = "black", geom = "point"
#   )
# plot_G_quadratic
# ggsave("plots_overall/plot_G_curve_polynomial.png", width=5, height=4)
# 
# # plot_G_curves = 
# #   plot_G_step(q=0.10) +
# #   plot_G_Scurve(a=5,b=60) +
# #   plot_G_Scurve(a=6,b=35) +
# #   plot_G_linear 
# # # plot_G_curves
# # ggsave("plots_overall/plot_G_curves.png", width=10, height=8)

#############################################################
### G `GM value` function value curves V_G(x) = E[g(y)|x] ###
#############################################################

# ### posterior summary of beta shape parameters and bust probability
# df_post_summary_shapeparams = 
#   df_post_summary_musd %>%
#   select(i, all_of(contains("shape")), all_of(contains("bust_prob"))) %>%
#   left_join(df_new) %>% relocate(draft_pick, .after=i) %>%
#   left_join(compensation_1C) %>%
#   select(-compensation_v1) %>%
#   rename(cost = rookie_contract_cap_pct) %>%
#   relocate(cost, .after = draft_pick)
# df_post_summary_shapeparams
# 
# ### get V_G(x) for 1 function g(y) = 1
# df_V_G_1f = get_df_V_G(
#   G_func=function(y) { 1 },
#   desc=paste0("g(y) = 1"),
#   type = ""
# )
# df_V_G_1f
# ### all values should be 1 (integrates to 1)
# mean(abs(df_V_G_1f$V_G))
# # max(abs(df_V_G_1f$V_G))
# # hist(df_V_G_1f$V_G)
# 
# ### get V_G(x) for identity function g(y) = y
# df_V_G_id = get_df_V_G(
#   G_func=function(y) { y }, 
#   desc=paste0("g(y) = y"),
#   type = "linear"
# )
# df_V_G_id
# 
# ### check that V_id(x) matches E(y|x)
# df_plot_G_id_perfv = 
#   bind_rows(
#     df_V_G_id %>% mutate(desc = paste0("\nE[g(y)|x], ", desc, "\n"))
#     ,
#     df_post_summary_perf_EV %>%
#       select(draft_pick, perf_EV_M, perf_EV1_M) %>%
#       rename(V_G = perf_EV_M, V_G1 = perf_EV1_M) %>%
#       mutate(desc = "E[Y|x]")
#   ) 
# df_plot_G_id_perfv
# df_plot_G_id_perfv %>%
#   filter(draft_pick < 255) %>%
#   ggplot(aes(x=draft_pick, y = V_G1, color=desc)) +
#   geom_line(linewidth=1) +
#   scale_color_brewer(name="", palette = "Set1") +
#   ylab("value relative to first pick") +
#   scale_y_continuous(limits=c(0,1)) +
#   scale_x_continuous(breaks=seq(1,32*9,by=32*2))
# 
# ### get V_G(x) for step function g(y) = 1{y>r}
# # q_ = 0.10
# q_ = 0.09
# # q_ = 0.075
# pd_ = 0
# df_V_G_step = get_df_V_G(
#   G_func=G_step_func(q=q_), 
#   desc=paste0("g(y) = 1{y>",percent(q_,pd_),"}"),
#   type="step"
#   # desc=paste0("E[1{Y>",q_,"}|x]")
# )
# df_V_G_step
# 
# q_ = 0.15
# pd_ = 0
# df_V_G_step1 = get_df_V_G(
#   G_func=G_step_func(q=q_), 
#   desc=paste0("g(y) = 1{y>",percent(q_,pd_),"}"),
#   type="step"
#   # desc=paste0("E[1{Y>",q_,"}|x]")
# )
# df_V_G_step1
# 
# # ### check that V_G(x) matches P(y>r|x)
# # df_V_G_step %>%
# #   mutate(desc = paste0("E[g(y)|x],\n", desc,"\n")) %>%
# #   bind_rows(
# #     df_post_summary_tail_prob %>%
# #       filter(q == q_) %>%
# #       select(draft_pick, tail_prob_M, tail_prob_1_M) %>%
# #       rename(V_G = tail_prob_M, V_G1 = tail_prob_1_M) %>%
# #       mutate(desc = paste0("P(Y>",q_,"|x)"))
# #   ) %>%
# #   ggplot(aes(x=draft_pick, y = V_G1, color=desc)) +
# #   geom_line(linewidth=1) +
# #   scale_color_brewer(name="", palette = "Set1") +
# #   ylab("value relative to first pick") +
# #   scale_y_continuous(limits=c(0,1)) +
# #   scale_x_continuous(breaks=seq(1,32*9,by=32*2))
# 
# ### get V_G(x) for G curve function 
# get_df_V_G_Scurve <- function(a,b,surplus=FALSE,printme=TRUE,SE=FALSE) {
#   get_df_V_G(
#     G_func=G_Scurve_func(a,b),
#     type = "s",
#     # desc = paste0("E[g(y)|x] ", ", ", betaCdfStr(a,b))
#     desc = betaCdfStr(a,b),
#     surplus = surplus,
#     printme=printme,
#     SE=SE
#   )
# }
# # df_V_G_Scurve_1 = get_df_V_G_Scurve(a=6, b=35)
# df_V_G_Scurve_1 = get_df_V_G_Scurve(a=8, b=35)
# df_V_G_Scurve_1
# # df_V_G_Scurve_2 = get_df_V_G_Scurve(a=5, b=60)
# df_V_G_Scurve_2 = get_df_V_G_Scurve(a=6, b=55)
# df_V_G_Scurve_2
# 
# df_V_G_Scurve_1_S = get_df_V_G_Scurve(a=6, b=35, surplus = TRUE)
# df_V_G_Scurve_1_S
# df_V_G_Scurve_2_S = get_df_V_G_Scurve(a=5, b=60, surplus = TRUE)
# df_V_G_Scurve_2_S
# 
# # ### get V_G(x) for quadratic function g(y) = y^2
# # df_V_G_poly = get_df_V_G(
# #   G_func=function(y) { y^6 }, 
# #   desc=paste0("g(y) = y^6")
# # )
# # df_V_G_poly
# 
# # ### median
# # df_V_median = 
# #   df_post_summary_med %>% 
# #   filter(curve=="median",rel) %>% 
# #   rename(V_G1=M) %>%
# #   select(draft_pick, V_G1) %>% 
# #   mutate(desc = "median", surplus=FALSE)
# 
# ### visualize V_G(x)
# df_plot_V_G = 
#   bind_rows(
#     df_V_G_Scurve_1 
#     ,
#     df_V_G_Scurve_2 
#     ,
#     df_V_G_step 
#     ,
#     df_V_G_step1 
#     # ,
#     # df_V_G_id 
#     ,
#     df_plot_Massey_Thaler_0 %>% rename(V_G1 = performance) %>% select(draft_pick, V_G1) %>% 
#       mutate(desc = "g(y) = y", type = "linear", surplus = FALSE, q = NA),
#   ) %>%
#   select(-V_G) 
# df_plot_V_G
# 
# ###
# plot_VG = 
#   df_plot_V_G %>%
#   # ggplot(aes(x=draft_pick, y = V_G1, color=desc, linetype=type)) +
#   # geom_line(linewidth=2) +
#   ggplot(aes(x=draft_pick, y = V_G1)) +
#   geom_line(linewidth=2, aes(color=desc), linetype="dashed", data = . %>% filter(type=="s")) +
#   geom_line(linewidth=2, aes(color=desc), linetype="dotted", data = . %>% filter(type=="linear")) +
#   geom_line(linewidth=2, aes(color=desc), linetype="solid", data = . %>% filter(type=="step")) +
#   # scale_color_brewer(name="", palette = "Set1") +
#   scale_color_manual(name="", values = c(brewer.pal(n = 4, name = 'Set1'), "black")) +
#   ylab("value relative to first pick") +
#   xlab("draft pick") +
#   theme(
#     legend.key.size = unit(2, 'lines'),
#     legend.key.width=unit(2.5,"cm"),
#   ) +
#   # scale_linetype_manual(name="", values = c(
#   #   "dotted",  "longdash" , "solid"
#   # )) +
#   guides(
#     linetype = "none",
#   ) +
#   scale_y_continuous(limits=c(0,1)) +
#   scale_x_continuous(breaks=seq(1,32*9,by=32*2))
# # plot_VG
# ggsave("plots_overall/plot_G_valueCurves.png",width=9,height=5)
# 
# # ### for CMSAC24 slides:  visualize V_G(x)
# # df_plot_V_G_2A = 
# #   bind_rows(
# #     df_V_G_step %>% 
# #       mutate(desc = paste0("E[g(Y)|x]/E[g(Y)|x=1],\n", desc, "\n"))
# #     ,
# #     df_V_G_step1 %>% 
# #       mutate(desc = paste0("E[g(Y)|x]/E[g(Y)|x=1],\n", desc, "\n"))
# #     ,
# #     df_V_G_id %>%
# #       mutate(desc = paste0("E[g(Y)|x]/E[g(Y)|x=1],\n", desc, "\n"))
# #     ,
# #     df_post_summary_perf_EV %>%
# #       select(draft_pick, compensation_v1) %>%
# #       rename(V_G1 = compensation_v1) %>%
# #       mutate(desc = "compensation\n"),
# #     df_jj %>% rename(V_G1 = jj_v1) %>% mutate(desc = "Jimmy Johnson\n")
# #     ,
# #     df_trade_market_weibull %>% 
# #       mutate(desc = paste0("\n", desc, "\n"))
# #   ) %>%
# #   select(-V_G) 
# # df_plot_V_G_2A
# # plot_VG_2A = 
# #   df_plot_V_G_2A %>%
# #   # filter(draft_pick < 255) %>%
# #   ggplot(aes(x=draft_pick, y = V_G1, color=desc)) +
# #   geom_line(linewidth=2) +
# #   # scale_color_brewer(name="V(x) = E[g(y)|x]", palette = "Set1") +
# #   scale_color_brewer(
# #     name="",
# #     # name=bquote(paste('v'['g']*'(x) = E[g(Y)|x]/E[g(Y)|x=1]')),
# #     # palette = "Set3"
# #     palette = "Set2"
# #     # palette = "Set1"
# #   ) +
# #   ylab("value relative to first pick") +
# #   xlab("draft pick") +
# #   scale_y_continuous(limits=c(0,1)) +
# #   scale_x_continuous(breaks=seq(1,32*9,by=32*2))
# # plot_VG_2A
# # # ggsave("plots_overall/plot_G_valueCurves_2A.png",width=12,height=7)

#################################
### median draft value curves ###
#################################

# ### median posterior summary
# df_post_summary_med = 
#   df_post_summary_musd %>%
#   left_join(df_new) %>%
#   select(i,draft_pick, all_of(contains("med"))) %>%
#   pivot_longer(-c(i,draft_pick)) %>%
#   filter(!str_detect(name, "1")) %>%
#   mutate(
#     quantity = str_remove_all(name, "L_|L1_|M_|M1_|U_|U1_"),
#     letter = str_sub(name,1,1),
#   ) %>%
#   select(-name) %>%
#   pivot_wider(names_from = "quantity", values_from="value") %>%
#   group_by(letter) %>%
#   mutate(med1 = med/first(med)) %>%
#   ungroup() %>%
#   pivot_wider(names_from = "letter", values_from = c("med", "med1")) 
# df_post_summary_med
# 
# df_post_summary_med_1 = 
#   left_join(
#     df_post_summary_med,
#     df_post_summary_perf_EV %>%
#       select(i,draft_pick,all_of(starts_with("perf_EV")),compensation_v1)
#   )
# df_post_summary_med_1
# 
# df_post_summary_med_A = 
#   df_post_summary_med_1 %>%
#   pivot_longer(-c(i,draft_pick,compensation_v1)) %>%
#   # filter(str_detect(name, "1")) %>%
#   mutate(
#     letter = str_sub(name, -1),
#     curve = ifelse(str_detect(name, "med"), "med", "EV"),
#     rel = str_detect(name,"1"),
#   ) %>%
#   select(-c(name)) %>%
#   pivot_wider(values_from = value, names_from = c("letter")) %>%
#   mutate(curve = case_when(
#     curve == "med" ~ "median",
#     curve == "EV" ~ "mean",
#   )) 
# df_post_summary_med_A
# 
# ### plot posterior median
# plot_med = 
#   df_post_summary_med_A %>%
#   bind_rows(
#     compensation_1C %>% rename(M = rookie_contract_cap_pct) %>% 
#       mutate(curve = "compensation", rel=F)
#   ) %>%
#   filter(draft_pick < 255) %>%
#   filter(!rel) %>%
#   ggplot(aes(x = draft_pick, y = M, color=curve)) +
#   geom_line(linewidth=2) +
#   xlab("draft pick") +
#   ylab("apy cap pct") +
#   scale_color_brewer(name = "", palette="Set1") +
#   scale_x_continuous(breaks=seq(1,32*9,by=32*2)) 
# plot_med
# 
# ### plot relative posterior median
# plot_med_relative = 
#   df_post_summary_med_A %>%
#   bind_rows(
#     compensation_1C %>% rename(M = compensation_v1) %>% 
#       mutate(curve = "compensation", rel=T)
#   ) %>%
#   filter(draft_pick < 255) %>%
#   filter(rel) %>%
#   ggplot(aes(x = draft_pick, y = M, color=curve)) +
#   geom_line(linewidth=2) +
#   xlab("draft pick") +
#   ylab("value relative to first pick") +
#   scale_color_brewer(name = "", palette="Set1") +
#   scale_x_continuous(breaks=seq(1,32*9,by=32*2))
# plot_med_relative
# 
# ggsave("plots_overall/plot_med_0.png",
#        plot_med + plot_med_relative,
#        width=17, height=5)


############################################
### surplus value tail probability curve ###
############################################

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

############################################################
### Best fitting G function to the observed trade market ###
############################################################

# eval_MAE_V1_G_Scurve <- function(a, b) {
#   print(paste0("eval_MAE_V1_G_Scurve(a=",a,",b=",b,")"))
#   df_V_G_Scurve_ab = get_df_V_G_Scurve(a, b, printme=F)
#   df_V_G_Scurve_ab
#   df_trades_Gab0 = 
#     left_join(
#       df_trades, 
#       df_V_G_Scurve_ab %>% select(draft_pick, V_G1),
#       by="draft_pick"
#     ) 
#   df_trades_Gab0
#   df_trades_Gab1 = 
#     df_trades_Gab0 %>%
#     group_by(trade_idx,team) %>%
#     summarise(
#       sum_v1 = sum(V_G1),
#       .groups = "drop"
#     ) %>%
#     pivot_wider(names_from = team, values_from = sum_v1, names_prefix = "sum_v1_") %>%
#     mutate(v1_u_minus_d = sum_v1_u - sum_v1_d) %>%
#     drop_na()
#   df_trades_Gab1
#   result = mean(abs(df_trades_Gab1$v1_u_minus_d)) ### MAE
#   print(result)
#   result
# }
# # eval_MAE_V1_G_Scurve(a=6, b=35)
# 
# grid_ab0 = as_tibble(expand.grid(a = 1:10, b = seq(20,100,by=2)))
# # grid_ab0 = as_tibble(expand.grid(a = seq(5,30,by=2), b = seq(5,30,by=2)))
# # grid_ab0 = as_tibble(expand.grid(a = seq(1,100,by=4), b = seq(1,20,by=4)))
# grid_ab0
# grid_ab = 
#   grid_ab0 %>%
#   rowwise() %>%
#   mutate(G_mae = eval_MAE_V1_G_Scurve(a,b)) %>%
#   ungroup()
# grid_ab

############################################################