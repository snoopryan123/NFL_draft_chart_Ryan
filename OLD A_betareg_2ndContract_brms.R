
###########################################################
### BRMS spike + beta regression model of P(y|x,QB) ###
### need shrinkage since so few QB observations
###########################################################

# library(brms)
# 
# SPIKE = 0.01
# df_train = 
#   players_2C %>%
#   filter(pos %in% c(
#     "QB", "RB/FB", "WR", "TE", "IOL", "OT",
#     "CB", "ED", "IDL", "LB", "S"
#   )) %>%
#   select(draft_pick, apy_cap_pct_2C, pos) %>%
#   mutate(
#     side = case_when(
#       pos %in% c("QB", "RB/FB", "WR", "TE", "IOL", "OT") ~ "offense",
#       TRUE ~ "defense"
#     ),
#     side1 = case_when(
#       pos == "QB" ~ "QB",
#       TRUE ~ side
#     )
#   ) %>%
#   mutate(bust = as.numeric(apy_cap_pct_2C <= SPIKE)) %>%
#   mutate(pos_num = as.numeric(factor(pos)))
# df_train
# 
# # priors <- c(set_prior("student_t(3, 0, 2.5)", class = "Intercept"),
# #             set_prior("normal(0, 1)", class = "b"))
# 
# # Bayesian Beta regression
# model_betareg_QB_tail_bayes <- 
#   brm(
#     bf(
#       apy_cap_pct_2C ~ (draft_pick | pos),
#       phi ~ draft_pick
#     ),
#     # bf(
#     #   apy_cap_pct_2C ~ (1 + splines::ns(draft_pick,5) | pos),
#     #   phi ~ draft_pick
#     # ),
#     # bf(
#     #   apy_cap_pct_2C ~ (1 + bs(draft_pick,df=3) | pos),
#     #   phi ~ draft_pick
#     # ),
#     data = df_train %>% filter(bust == 0),
#     family = Beta(),
#     # prior = priors,
#     # chains = 4, iter = 2000, warmup = 1000,
#     # cores = 4, seed = 1234, 
#     chains = 1, iter = 2000, warmup = 1000,
#     cores = 1, seed = 1234, 
#     # # Use the cmdstanr backend for Stan because it's faster and more modern than
#     # # the default rstan You need to install the cmdstanr package first
#     # # (https://mc-stan.org/cmdstanr/) and then run cmdstanr::install_cmdstan() to
#     # # install cmdstan on your computer.
#     backend = "cmdstanr",
#     file = "betareg_model_1.rds"  # Save this so it doesn't have to always rerun
#   )
# summary(model_betareg_QB_tail_bayes)
# 
# model_betareg_QB_tail_bayes
# 
# # table(df_train$pos)
# 
# df_plot_betareg_bayes = 
#   expand.grid(
#     draft_pick = 1:256, 
#     pos = sort(unique(df_train$pos))
#     # pos = c(
#     #   sort((df_train %>% filter(side=="offense") %>% distinct(pos))$pos),
#     #   sort((df_train %>% filter(side=="defense") %>% distinct(pos))$pos)
#     # )
#     # pos=c("QB", "RB/FB", "WR", "TE", "IOL", "OT")
#     # pos = unique((df_train %>% filter(side == "offense"))$pos)
#     # pos = unique((df_train %>% filter(side == "defense"))$pos)
#   ) %>%
#   as_tibble() %>%
#   left_join(df_train %>% distinct(pos, side, side1)) %>%
#   mutate(
#     # posterior_linpred(model_betareg_QB_tail_bayes, .)
#     mu = fitted(model_betareg_QB_tail_bayes, .)[,"Estimate"],
#     # mu = predict(model_betareg_QB_tail_bayes, ., type="response")[,"Estimate"],
#     # mu_L = predict(model_betareg_QB_tail_bayes, ., type="response")[,"Q2.5"],
#     # mu_U = predict(model_betareg_QB_tail_bayes, ., type="response")[,"Q97.5"],
#     # sd = predict(model_betareg_QB_tail_bayes, ., type="variance")[,"Estimate"],
#     # phi = predict(model_betareg_QB_tail_bayes, ., type="precision")[,"Estimate"],
#     # temp = predict(model_betareg_QB_tail_bayes, ., type="precision")
#   ) 
# df_plot_betareg_bayes
# 
# plot_betareg_bayes_off = 
#   df_plot_betareg_bayes %>%
#   filter(side == "offense") %>%
#   ggplot(aes(x = draft_pick, color=pos)) +
#   # geom_line(aes(y = mu_L), linetype="dashed") +
#   # geom_line(aes(y = mu_U), linetype="dashed") +
#   geom_line(aes(y = mu), linewidth=1) +
#   scale_x_continuous(breaks=seq(1,32*9,by=32*2)) +
#   ylab("")
# plot_betareg_bayes_off
# 
# plot_betareg_bayes_def = 
#   df_plot_betareg_bayes %>%
#   filter(side == "defense") %>%
#   ggplot(aes(x = draft_pick, color=pos)) +
#   # geom_line(aes(y = mu_L), linetype="dashed") +
#   # geom_line(aes(y = mu_U), linetype="dashed") +
#   geom_line(aes(y = mu), linewidth=1) +
#   scale_x_continuous(breaks=seq(1,32*9,by=32*2)) +
#   ylab("")
# plot_betareg_bayes_def
# 
# plot_betareg_bayes_off + plot_betareg_bayes_def


