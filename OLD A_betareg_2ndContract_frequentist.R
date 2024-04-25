
##################
### load stuff ###
##################

library(rvest)
library(gt)
library(patchwork)
library(tidyverse)
library(splines)
library(RColorBrewer)
library(betareg)

### plotting pre-sets
theme_set(theme_bw())
theme_update(
  text = element_text(size=20),
  plot.title = element_text(hjust = 0.5, size=20),
  axis.title = element_text(size=20),
  axis.text = element_text(size=20),
  legend.text = element_text(size=20),
  legend.title = element_text(size=20),
  panel.spacing = unit(2, "lines")
) 

#######################################
### Get actual second contract data ###
#######################################

# take the APY in terms of percent of the cap for each drafted player's second deal (i.e., first deal after rookie contract). 
# For players that did not receive a second contract,  we'll assign them a value of zero. 

### all contracts of all players 
players_all <- 
  nflreadr::load_contracts() %>%
  filter(between(draft_year, 2013, 2023)) %>%
  arrange(otc_id, year_signed) 

### each player's 2nd contract 
players_2C = 
  players_all %>%
  group_by(otc_id) %>% 
  dplyr::slice(1:2) %>%
  mutate(contracts = n(), row = 1 : n()) %>%
  select(player, otc_id, position, team, draft_year, year_signed, years, value, apy, apy_cap_pct, draft_overall, contracts, row) %>%
  filter(
    !is.na(draft_overall),        # was drafted
    # position != "QB",             # is not a QB
    contracts == 1 | row == 2     # either only got drafted (no 2nd contract) or 2nd contract
  ) %>%
  ungroup() %>%
  arrange(draft_year, draft_overall) %>%
  mutate(
    # fill in 0 for guys who didn't get 2nd contract
    apy_cap_pct = ifelse(contracts == 1, 0, apy_cap_pct),
    # make some buckets for positions
    pos = case_when(
      position %in% c("C", "RG", "LG") ~ "IOL",
      position %in% c("RB", "FB") ~ "RB/FB",
      position %in% c("LT", "RT") ~ "OT",
      TRUE ~ position
    ),
    posCat = case_when(
      # pos %in% c("IOL", "RB/FB", "OT", "TE", "WR", "QB") ~ "Offense", 
      pos %in% c("QB") ~ "Quarterback", 
      pos %in% c("IOL", "RB/FB", "OT", "TE", "WR") ~ "Offense", 
      pos %in% c("CB", "ED", "IDL", "LB", "S") ~ "Defense",
      TRUE ~ "Other"
    ),
    is_QB = as.numeric(pos == "QB"),
    QB = ifelse(pos == "QB", "QB", "not QB"),
    # # overall fit
    # fit = loess(apy_cap_pct ~ draft_overall)$fitted
  ) %>%
  select(-c(value, apy)) %>%
  select(-c(contracts, row)) %>%
  rename(draft_pick = draft_overall, apy_cap_pct_2C = apy_cap_pct) %>%
  mutate(apy_cap_pct_2C_squeezed = (apy_cap_pct_2C*(n()-1)+0.5)/n()) %>%
  # group_by(pos) %>%
  # # positional fit
  # mutate(pos_fit = 100 * loess(apy_cap_pct ~ draft_overall)$fitted) %>%
  ungroup()
players_2C

######################################################################
### ignoring position, the empirical conditional mean, sd, and density 
### of 2nd contract value y given draft pick x
######################################################################

df_overall_emp =
  players_2C %>%
  group_by(draft_pick) %>%
  summarise(
    emp_mean = mean(apy_cap_pct_2C),
    emp_sd = sd(apy_cap_pct_2C),
    .groups = "drop"
  ) %>%
  mutate(
    emp_sd = ifelse(is.na(emp_sd), mean(emp_sd, na.rm=T), emp_sd),
    loess_mean = loess(emp_mean ~ draft_pick)$fitted,
    loess_sd = loess(emp_sd ~ draft_pick)$fitted,
  ) 
df_overall_emp

plot_emp_mean = 
  df_overall_emp %>%
  ggplot(aes(x = draft_pick)) +
  geom_point(aes(y = emp_mean)) +
  geom_line(aes(y = loess_mean), linewidth=2, color="dodgerblue2") +
  scale_x_continuous(name = "draft pick", breaks=seq(1,32*9,by=32*2)) +
  labs(title = "empirical conditional mean") +
  ylab("apy cap pct") 
# plot_emp_mean

plot_emp_sd = 
  df_overall_emp %>%
  ggplot(aes(x = draft_pick)) +
  geom_point(aes(y = emp_sd)) +
  geom_line(aes(y = loess_sd), linewidth=2, color="firebrick") +
  scale_x_continuous(name = "draft pick", breaks=seq(1,32*9,by=32*2)) +
  labs(title = "empirical conditional s.d.") +
  ylab("apy cap pct")
# plot_emp_sd

plot_emp_mean_sd = 
  plot_emp_mean + plot_emp_sd +
  plot_annotation(
    title = 'The value of draft picks, ignoring position',
    # subtitle = 'Draft Curves from 2013-2023 Draft Classes',
    # caption = '@RyanBrill_',
    theme = theme(
      plot.title = element_text(size = 20, hjust = 0.5, face="bold"),
      # plot.subtitle = element_text(size = 14, hjust = 0.5)
    )
  )
plot_emp_mean_sd

# ex_draft_picks_A = c(seq(1,32*3,by=8), seq(32*3,32*5,by=16))
# ex_draft_picks_A = c(seq(1,32*7,by=32/2))
ex_draft_picks_A = c(seq(1,32*3,by=32/4))
players_2C %>%
  filter(draft_pick %in% ex_draft_picks_A) %>%
  ggplot(aes(x = apy_cap_pct_2C)) +
  facet_wrap(~ draft_pick) +
  # geom_histogram(aes(y=after_stat(density))) +
  # geom_density() +
  geom_histogram(aes(y=after_stat(density)), fill="gray80") +
  geom_density(linewidth=1, color="gray60")  +
  theme(axis.text.x = element_text(size = 12)) +
  xlab("apy cap pct") +
  labs(title = "empirical conditional density") +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

###########################################################
### ignoring position, beta regression model of P(y|x)
###########################################################

### beta regression model
# K = 4
K = 6
model_betareg_overall = betareg(
  # formula = apy_cap_pct_2C_squeezed ~ bs(draft_pick, df=K) | draft_pick,
  # formula = apy_cap_pct_2C_squeezed ~ bs(draft_pick, df=6) | bs(draft_pick, df=6),
  # formula = apy_cap_pct_2C_squeezed ~ bs(draft_pick, knots=c(65)) | draft_pick,
  # formula = apy_cap_pct_2C_squeezed ~ bs(draft_pick, knots=c(65)) | bs(draft_pick, knots=c(65)),
  # formula = apy_cap_pct_2C_squeezed ~ bs(draft_pick, knots=c(65,10)) | bs(draft_pick, knots=c(65,10)),
  # formula = apy_cap_pct_2C_squeezed ~ bs(draft_pick, knots=c(33,65)) | bs(draft_pick, knots=c(33,65)),
  # formula = apy_cap_pct_2C_squeezed ~ bs(draft_pick, knots=c(33,65)) | bs(draft_pick, knots=c(33,65)),
  # formula = apy_cap_pct_2C_squeezed ~ bs(draft_pick, knots=seq(32*2,32*6,by=32)) | bs(draft_pick, knots=seq(32*2,32*6,by=32)),
  # formula = apy_cap_pct_2C_squeezed ~ draft_pick | draft_pick,
  formula = apy_cap_pct_2C_squeezed ~ draft_pick + I(draft_pick^2) + I(draft_pick^3) | draft_pick,
  # formula = apy_cap_pct_2C_squeezed ~ draft_pick + I(draft_pick^2) + I(draft_pick^3) + I(draft_pick^4) | draft_pick,
  # data = players_2C,
  data = players_2C %>% filter(apy_cap_pct_2C > 0),
  link="logit", link.phi = "identity", type="BC" #"ML"
)
model_betareg_overall

plot_betareg_overall_mean = 
  df_overall_emp %>%
  mutate(mu = predict(model_betareg_overall,.,type="response")) %>%
  ggplot(aes(x = draft_pick)) +
  geom_point(aes(y = emp_mean)) +
  geom_line(aes(y = mu), linewidth=2, color="dodgerblue2") +
  scale_x_continuous(name = "draft pick", breaks=seq(1,32*9,by=32*2)) +
  labs(title = "beta regession conditional mean") +
  ylab("apy cap pct") 
# plot_betareg_overall_mean

plot_betareg_overall_sd = 
  df_overall_emp %>%
  mutate(sd = sqrt(predict(model_betareg_overall,.,type="variance"))) %>%
  ggplot(aes(x = draft_pick)) +
  geom_point(aes(y = emp_sd)) +
  geom_line(aes(y = sd), linewidth=2, color="firebrick") +
  scale_x_continuous(name = "draft pick", breaks=seq(1,32*9,by=32*2)) +
  labs(title = "beta regession conditional s.d.") +
  ylab("apy cap pct") 
# plot_betareg_overall_sd

plot_betareg_overall_mean_sd = 
  plot_betareg_overall_mean + plot_betareg_overall_sd +
  plot_annotation(
    title = 'The value of draft picks, ignoring position',
    # subtitle = 'Draft Curves from 2013-2023 Draft Classes',
    # caption = '@RyanBrill_',
    theme = theme(
      plot.title = element_text(size = 20, hjust = 0.5, face="bold"),
      # plot.subtitle = element_text(size = 14, hjust = 0.5)
    )
  )
plot_betareg_overall_mean_sd

### beta regression conditional density
df_betareg_overall_params = 
  tibble(draft_pick = 1:256) %>%
  mutate(
    mu = predict(model_betareg_overall,.,type="response"),
    phi = predict(model_betareg_overall,.,type="precision"),
    shape1 = mu*phi,
    shape2 = phi*(1-mu),
  )
df_betareg_overall_params

# y_grid = seq(0.001,0.25,length.out=100)
y_grid = seq(0.005,0.25,length.out=100)
# y_grid = seq(0.01,0.25,length.out=100)
y_grid
df_betareg_overall_density = 
  tibble(expand.grid(draft_pick = 1:256, y = y_grid)) %>%
  left_join(df_betareg_overall_params) %>%
  mutate(density = dbeta(y, shape1, shape2))
df_betareg_overall_density

# df_betareg_overall_density %>%
#   filter(draft_pick %in% ex_draft_picks_A) %>%
#   ggplot(aes(x=y,y=density)) +
#   facet_wrap(~ draft_pick) +
#   geom_line() +
#   theme(axis.text.x = element_text(size = 12)) +
#   xlab("apy cap pct") +
#   # labs(title = "empirical conditional density") +
#   theme(axis.text.y=element_blank(),
#         axis.ticks.y=element_blank())

# # ex_draft_picks_B = c(seq(1,32*2,by=32/8))
# ex_draft_picks_B = c(seq(1,32,by=32/16))
# plot_betareg_overall_density = 
#   bind_rows(
#     df_betareg_overall_density,
#     players_2C %>% select(draft_pick, apy_cap_pct_2C)
#   ) %>%
#   filter(draft_pick %in% ex_draft_picks_B) %>%
#   ggplot() +
#   facet_wrap(~ draft_pick) +
#   geom_line(aes(x = y, y = density)) +
#   geom_density(aes(x = apy_cap_pct_2C), color="dodgerblue2") +
#   theme(axis.text.x = element_text(size = 12)) +
#   xlab("apy cap pct") +
#   labs(title = "empirical conditional density") +
#   theme(axis.text.y=element_blank(),
#         axis.ticks.y=element_blank())
# plot_betareg_overall_density

ex_draft_picks_C = seq(1,32, by=1)
# ex_draft_picks_C = seq(1,32*7, by=32)
df_betareg_overall_density %>%
  filter(draft_pick %in% ex_draft_picks_C) %>%
  ggplot() +
  geom_line(aes(x = y, y = density, color=factor(draft_pick)), linewidth=1)

plot_func_betareg_overall_density <- function(ex_draft_picks=ex_draft_picks_B, includeEmp=TRUE) {
  p = 
    bind_rows(
      df_betareg_overall_density,
      players_2C %>% select(draft_pick, apy_cap_pct_2C)
    ) %>%
    filter(draft_pick %in% ex_draft_picks) %>%
    ggplot() +
    facet_wrap(~ draft_pick) +
    theme(axis.text.x = element_text(size = 12)) +
    xlab("apy cap pct") +
    # labs(title = "conditional density") +
    theme(axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) 
  if (includeEmp) {
    p = p + 
      geom_histogram(aes(x = apy_cap_pct_2C, y=after_stat(density)), fill="gray80") +
      geom_density(aes(x = apy_cap_pct_2C), linewidth=1, color="gray60") 
  }
  p = p + geom_line(aes(x = y, y = density), linewidth=1) 
 p
}

plot_func_betareg_overall_density(c(seq(1,16,by=1)), includeEmp=T)
plot_func_betareg_overall_density(c(seq(17,32,by=1)), includeEmp=T)
plot_func_betareg_overall_density(c(seq(33,48,by=1)), includeEmp=T)
plot_func_betareg_overall_density(c(seq(49,64,by=1)), includeEmp=T)
###
plot_func_betareg_overall_density(c(seq(65,80,by=1)), includeEmp=T)
plot_func_betareg_overall_density(c(seq(81,112,by=2)), includeEmp=T)
plot_func_betareg_overall_density(c(seq(113,32*7,by=8)), includeEmp=T)






##################################################################
### ignoring position, spike + beta regression model of P(y|x) ###
##################################################################

### empirical density
# ex_draft_picks_A = c(seq(1,32*3,by=8), seq(32*3,32*5,by=16))
# ex_draft_picks_A = c(seq(1,32*7,by=32/2))
# ex_draft_picks_A = c(seq(1,32*3,by=32/4))
ex_draft_picks_A = c(seq(1,32*2,by=32/8))
players_2C %>%
  filter(draft_pick %in% ex_draft_picks_A) %>%
  ggplot(aes(x = apy_cap_pct_2C)) +
  facet_wrap(~ draft_pick) +
  # geom_histogram(aes(y=after_stat(density))) +
  # geom_density() +
  geom_histogram(aes(y=after_stat(density)), fill="gray80") +
  geom_density(linewidth=1, color="gray60")  +
  theme(axis.text.x = element_text(size = 12)) +
  xlab("apy cap pct") +
  labs(title = "empirical conditional density") +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

### looks like a spike near 0 and a beta distributed right tail.

### spike + beta regression model
# SPIKE = 0
SPIKE = 0.01
model_betareg_overall_tail = betareg(
  # formula = apy_cap_pct_2C ~ bs(draft_pick, knots=c(17,48)) | bs(draft_pick, knots=c(17,48)),
  formula = apy_cap_pct_2C ~ bs(draft_pick, knots=c(17,65)) | bs(draft_pick, knots=c(17,65)),
  data = players_2C %>% filter(apy_cap_pct_2C > SPIKE),
  link="logit", link.phi = "identity", type="BC" #"ML"
)
model_spike_prob = glm(
  formula = apy_cap_pct_2C <= SPIKE ~ bs(draft_pick, df=5),
  data = players_2C, 
  family = "binomial"
)
model_betareg_overall_tail
model_spike_prob

df_overall_emp_tail =
  players_2C %>% 
  filter(apy_cap_pct_2C > SPIKE) %>%
  group_by(draft_pick) %>%
  summarise(
    emp_mean = mean(apy_cap_pct_2C),
    emp_sd = sd(apy_cap_pct_2C),
    .groups = "drop"
  )
df_overall_emp_tail

plot_betareg_overall_mean_tail = 
  df_overall_emp_tail %>%
  mutate(mu = predict(model_betareg_overall_tail,.,type="response")) %>%
  ggplot(aes(x = draft_pick, y = emp_mean)) +
  geom_point() +
  geom_line(aes(y = mu), linewidth=2, color="dodgerblue2") +
  # geom_smooth(se=F, linewidth=2, color="dodgerblue2") +
  scale_x_continuous(name = "draft pick", breaks=seq(1,32*9,by=32*2)) +
  labs(title = "conditional mean") +
  ylab("apy cap pct") 
# plot_betareg_overall_mean_tail

plot_betareg_overall_sd_tail = 
  df_overall_emp_tail %>%
  mutate(sd = sqrt(predict(model_betareg_overall_tail,.,type="variance"))) %>%
  # filter(draft_pick <= 32*7) %>%
  ggplot(aes(x = draft_pick, y = emp_sd)) +
  geom_point() +
  geom_line(aes(y = sd), linewidth=2, color="firebrick") +
  # geom_smooth(se=F, linewidth=2, color="firebrick") +
  scale_x_continuous(name = "draft pick", breaks=seq(1,32*9,by=32*2)) +
  labs(title = "conditional s.d.") +
  ylab("apy cap pct") 
# plot_betareg_overall_sd_tail

plot_betareg_overall_mean_sd_tail = 
  plot_betareg_overall_mean_tail + plot_betareg_overall_sd_tail +
  plot_annotation(
    title = 'The value of draft picks, ignoring position',
    # subtitle = 'Draft Curves from 2013-2023 Draft Classes',
    # caption = '@RyanBrill_',
    theme = theme(
      plot.title = element_text(size = 20, hjust = 0.5, face="bold"),
      # plot.subtitle = element_text(size = 14, hjust = 0.5)
    )
  )
plot_betareg_overall_mean_sd_tail

### beta regression conditional density
df_betareg_overall_params_tail = 
  tibble(draft_pick = 1:256) %>%
  mutate(
    mu = predict(model_betareg_overall_tail,.,type="response"),
    phi = predict(model_betareg_overall_tail,.,type="precision"),
    shape1 = mu*phi,
    shape2 = phi*(1-mu),
  )
df_betareg_overall_params_tail

# y_grid = seq(0.001,0.25,length.out=100)
y_grid = seq(0.005,0.25,length.out=100)
# y_grid = seq(0.01,0.25,length.out=100)
y_grid
df_betareg_overall_density_tail = 
  tibble(expand.grid(draft_pick = 1:256, y = y_grid)) %>%
  left_join(df_betareg_overall_params_tail) %>%
  mutate(density = dbeta(y, shape1, shape2))
df_betareg_overall_density_tail

# df_betareg_overall_density %>%
#   filter(draft_pick %in% ex_draft_picks_A) %>%
#   ggplot(aes(x=y,y=density)) +
#   facet_wrap(~ draft_pick) +
#   geom_line() +
#   theme(axis.text.x = element_text(size = 12)) +
#   xlab("apy cap pct") +
#   # labs(title = "empirical conditional density") +
#   theme(axis.text.y=element_blank(),
#         axis.ticks.y=element_blank())

# # ex_draft_picks_B = c(seq(1,32*2,by=32/8))
# ex_draft_picks_B = c(seq(1,32,by=32/16))
# plot_betareg_overall_density = 
#   bind_rows(
#     df_betareg_overall_density,
#     players_2C %>% select(draft_pick, apy_cap_pct_2C)
#   ) %>%
#   filter(draft_pick %in% ex_draft_picks_B) %>%
#   ggplot() +
#   facet_wrap(~ draft_pick) +
#   geom_line(aes(x = y, y = density)) +
#   geom_density(aes(x = apy_cap_pct_2C), color="dodgerblue2") +
#   theme(axis.text.x = element_text(size = 12)) +
#   xlab("apy cap pct") +
#   labs(title = "empirical conditional density") +
#   theme(axis.text.y=element_blank(),
#         axis.ticks.y=element_blank())
# plot_betareg_overall_density




plot_func_betareg_overall_density_tail <- function(ex_draft_picks=ex_draft_picks_B, includeEmp=TRUE) {
  p = 
    bind_rows(
      df_betareg_overall_density_tail,
      players_2C %>% select(draft_pick, apy_cap_pct_2C) %>% 
        filter(apy_cap_pct_2C > SPIKE) 
    ) %>%
    filter(draft_pick %in% ex_draft_picks) %>%
    ggplot() +
    facet_wrap(~ draft_pick) +
    theme(axis.text.x = element_text(size = 12)) +
    xlab("apy cap pct") +
    # labs(title = "conditional density") +
    theme(axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) 
  if (includeEmp) {
    p = p + 
      # geom_histogram(aes(x = apy_cap_pct_2C, y=after_stat(density)), fill="gray80") +
      geom_density(aes(x = apy_cap_pct_2C), linewidth=1, color="gray60") 
  }
  p = p + geom_line(aes(x = y, y = density), linewidth=1) 
  p
}

plot_func_betareg_overall_density_tail(c(seq(1,16,by=1)), includeEmp=T)
plot_func_betareg_overall_density_tail(c(seq(17,32,by=1)), includeEmp=T)
plot_func_betareg_overall_density_tail(c(seq(33,48,by=1)), includeEmp=T)
###
plot_func_betareg_overall_density_tail(c(seq(1,16,by=1)), includeEmp=F)
plot_func_betareg_overall_density_tail(c(seq(17,32,by=1)), includeEmp=F)
plot_func_betareg_overall_density_tail(c(seq(33,48,by=1)), includeEmp=F)
###
plot_func_betareg_overall_density_tail(c(seq(49,64,by=1)), includeEmp=T)
plot_func_betareg_overall_density_tail(c(seq(65,80,by=1)), includeEmp=T)
plot_func_betareg_overall_density_tail(c(seq(81,112,by=2)), includeEmp=T)
plot_func_betareg_overall_density_tail(c(seq(113,32*7,by=8)), includeEmp=T)

ex_draft_picks_C = seq(1,32, by=1)
df_betareg_overall_density_tail %>%
  filter(draft_pick %in% ex_draft_picks_C) %>%
  ggplot() +
  geom_line(aes(x = y, y = density, color=factor(draft_pick)), linewidth=1)

ex_draft_picks_D = sort(unique(c(seq(1,32*8, by=32/2), seq(1,16,by=4))))
df_betareg_overall_density_tail %>%
  filter(draft_pick %in% ex_draft_picks_D) %>%
  ggplot() +
  geom_line(aes(x = y, y = density, color=factor(draft_pick)), linewidth=1)

### SPIKE

df_overall_emp_spikeProb =
  players_2C %>% 
  group_by(draft_pick) %>%
  summarise(
    num_spike = sum(apy_cap_pct_2C <= SPIKE),
    num_tot = n(),
    emp_spike_prob = num_spike/num_tot,
    .groups = "drop"
  )
df_overall_emp_spikeProb

df_overall_spikeProb = 
  df_overall_emp_spikeProb %>%
  mutate(spike_prob = predict(model_spike_prob,.,type="response"))
df_overall_spikeProb

plot_spikeprob_overall = 
  df_overall_spikeProb %>%
  ggplot(aes(x = draft_pick, y = emp_spike_prob)) +
  geom_point() +
  geom_line(aes(y = spike_prob), linewidth=2, color="dodgerblue2") +
  # geom_smooth(se=F, linewidth=2, color="dodgerblue2") +
  scale_x_continuous(name = "draft pick", breaks=seq(1,32*9,by=32*2)) +
  labs(title = "spike probability") +
  ylab("prob") 
plot_spikeprob_overall

mean_spike_val = mean((players_2C %>% filter(apy_cap_pct_2C <= SPIKE))$apy_cap_pct_2C)
mean_spike_val

players_2C %>% 
  filter(apy_cap_pct_2C <= SPIKE) %>%
  group_by(draft_pick) %>%
  summarise(
    mean_spike_val = mean(apy_cap_pct_2C)
  ) %>%
  ggplot() +
  geom_hline(yintercept=mean_spike_val, linewidth=1)  +
  geom_point(aes(x = draft_pick, y = mean_spike_val)) +
  ylab("apy cap pct") +
  labs(title = "conditional mean given bust") +
  # ylab("2nd contract percentage of cap") +
  scale_x_continuous(name = "draft pick", breaks=seq(1,32*9,by=32*2)) 

### overall CDF
# y_grid = seq(0.005,0.25,length.out=100)
y_grid
df_overall_density = 
  tibble(expand.grid(draft_pick = 1:256, y = y_grid)) %>%
  filter(y > SPIKE) %>%
  left_join(df_betareg_overall_params_tail) %>%
  left_join(
    df_overall_spikeProb %>% select(draft_pick, spike_prob)
  ) %>%
  mutate(
    cdf_tail = pbeta(y, shape1, shape2),
    cdf = cdf_tail*(1-spike_prob) + spike_prob
  ) %>%
  select(draft_pick, y, cdf) 
df_overall_density

df_overall_density_A = 
  bind_rows(
    df_overall_density
    ,
    df_overall_spikeProb %>% 
      select(draft_pick, spike_prob) %>%
      mutate(y = mean_spike_val) %>%
      rename(cdf = spike_prob) %>%
      relocate(y, .before = cdf)
    ,
    df_overall_spikeProb %>%
      distinct(draft_pick) %>%
      mutate(cdf = 0) %>%
      mutate(y = 0)
  ) %>%
  arrange(draft_pick,y) %>%
  group_by(draft_pick) %>%
  mutate(density = c(NA, diff(cdf))) %>%
  ungroup()
df_overall_density_A

cdf_spike_betareg <- function(df) {
  ### df is a tibble of (draft_pick, y)
  df1a = 
    df %>%
    mutate(
      mu = predict(model_betareg_overall_tail,.,type="response"),
      phi = predict(model_betareg_overall_tail,.,type="precision"),
      shape1 = mu*phi,
      shape2 = phi*(1-mu),
      cdf_tail = pbeta(y, shape1, shape2),
      cdf_spike = as.numeric(y >= mean_spike_val),
      p_spike = predict(model_spike_prob,.,type="response"),
      in_spike = y <= SPIKE,
      # cdf = in_spike*(cdf_spike*p_spike) + (1-in_spike)*(p_spike + (1-p_spike)*cdf_tail)
      cdf = (1-in_spike)*(p_spike + (1-p_spike)*cdf_tail)
    ) 
  df1a
  
  df1b = df1a %>% select(draft_pick, y, cdf) %>% arrange(draft_pick,y)
  df1b
}

# mean_spike_val
y_grid_A = c(0, 0.001, mean_spike_val, seq(0.005,0.25,length.out=100))
y_grid_A
df_A = tibble(expand.grid(draft_pick = 1:256, y = y_grid_A))
df_A
cdf_spike_betareg(df_A)

df_v1_q = 
  cdf_spike_betareg(tibble(expand.grid(draft_pick = 1:256, y = c(0.06,0.07,0.1)))) %>%
  group_by(y) %>%
  mutate(p = 1 - cdf) %>%
  mutate(v1 = p/first(p)) 
df_v1_q
df_v1_q %>%
  ggplot(aes(x=draft_pick, y=v1, color=factor(y))) +
  geom_point()

##################################################
### spike + beta regression model of P(y|x,QB) ###
##################################################

### spike + beta regression model
SPIKE = 0.01
model_betareg_QB_tail = betareg(
  formula = apy_cap_pct_2C ~ 
    # I(is_QB)*bs(draft_pick, df=3) 
    I(is_QB)*bs(draft_pick, knots=c(129)) 
    | 
    # I(is_QB)*draft_pick
    draft_pick
  ,
  data = players_2C %>% filter(apy_cap_pct_2C > SPIKE),
  link="logit", link.phi = "identity", type="BC" #"ML"
)
model_betareg_QB_tail
model_spike_prob_QB = glm(
  # formula = apy_cap_pct_2C <= SPIKE ~ is_QB*bs(draft_pick, df=5),
  # formula = apy_cap_pct_2C <= SPIKE ~ is_QB*bs(draft_pick, df=3),
  formula = apy_cap_pct_2C <= SPIKE ~ I(is_QB)*draft_pick,
  data = players_2C, 
  family = "binomial"
)
model_spike_prob_QB

### visualize conditional mean and s.d.
df_QB_emp_tail =
  players_2C %>% 
  filter(apy_cap_pct_2C > SPIKE) %>%
  group_by(draft_pick, is_QB) %>%
  summarise(
    emp_mean = mean(apy_cap_pct_2C),
    emp_sd = sd(apy_cap_pct_2C),
    .groups = "drop"
  ) %>%
   mutate(QB = ifelse(is_QB, "QB","not QB"))
df_QB_emp_tail

plot_betareg_QB_mean_tail = 
  df_QB_emp_tail %>%
  mutate(mu = predict(model_betareg_QB_tail,.,type="response")) %>%
  ggplot(aes(x = draft_pick, y = emp_mean)) +
  facet_wrap(~QB) +
  geom_point() +
  geom_line(aes(y = mu), linewidth=2, color="dodgerblue2") +
  # geom_smooth(se=F, linewidth=2, color="dodgerblue2") +
  scale_x_continuous(name = "draft pick", breaks=seq(1,32*9,by=32*2)) +
  labs(title = "conditional mean") +
  ylab("apy cap pct") 
# plot_betareg_QB_mean_tail

plot_betareg_QB_sd_tail = 
  df_QB_emp_tail %>%
  mutate(sd = sqrt(predict(model_betareg_QB_tail,.,type="variance"))) %>%
  # filter(draft_pick <= 32*7) %>%
  ggplot(aes(x = draft_pick, y = emp_sd)) +
  facet_wrap(~QB) +
  geom_point() +
  geom_line(aes(y = sd), linewidth=2, color="firebrick") +
  # geom_smooth(se=F, linewidth=2, color="firebrick") +
  scale_x_continuous(name = "draft pick", breaks=seq(1,32*9,by=32*2)) +
  labs(title = "conditional s.d.") +
  ylab("apy cap pct") 
# plot_betareg_QB_sd_tail

plot_betareg_QB_mean_sd_tail = 
  plot_betareg_QB_mean_tail + plot_betareg_QB_sd_tail +
  plot_annotation(
    # title = 'The value of draft picks, ignoring position',
    # subtitle = 'Draft Curves from 2013-2023 Draft Classes',
    # caption = '@RyanBrill_',
    theme = theme(
      plot.title = element_text(size = 20, hjust = 0.5, face="bold"),
      # plot.subtitle = element_text(size = 14, hjust = 0.5)
    )
  )
plot_betareg_QB_mean_sd_tail

### SPIKE
df_QB_emp_spikeProb =
  players_2C %>% 
  group_by(draft_pick, is_QB, QB) %>%
  summarise(
    num_spike = sum(apy_cap_pct_2C <= SPIKE),
    num_tot = n(),
    emp_spike_prob = num_spike/num_tot,
    .groups = "drop"
  )
df_QB_emp_spikeProb

df_QB_spikeProb = 
  df_QB_emp_spikeProb %>%
  mutate(spike_prob = predict(model_spike_prob_QB,.,type="response"))
df_QB_spikeProb

plot_spikeprob_QB = 
  df_QB_spikeProb %>%
  ggplot(aes(x = draft_pick, y = emp_spike_prob)) +
  facet_wrap(~QB) +
  geom_point() +
  geom_line(aes(y = spike_prob), linewidth=2, color="dodgerblue2") +
  # geom_smooth(se=F, linewidth=2, color="dodgerblue2") +
  scale_x_continuous(name = "draft pick", breaks=seq(1,32*9,by=32*2)) +
  labs(title = "spike probability") +
  ylab("prob") 
plot_spikeprob_QB

# players_2C %>% filter(apy_cap_pct_2C <= SPIKE) %>% 
#   group_by(QB,is_QB) %>%
#   summarise(m = mean(apy_cap_pct_2C), .groups = "drop")
# mean_spike_val = 
# mean_spike_val

cdf_spike_betareg_QB <- function(df) {
  ### df is a tibble of (draft_pick, y, is_QB)
  df1a = 
    df %>%
    mutate(
      mu = predict(model_betareg_QB_tail,.,type="response"),
      phi = predict(model_betareg_QB_tail,.,type="precision"),
      shape1 = mu*phi,
      shape2 = phi*(1-mu),
      cdf_tail = pbeta(y, shape1, shape2),
      cdf_spike = as.numeric(y >= mean_spike_val),
      p_spike = predict(model_spike_prob_QB,.,type="response"),
      in_spike = y <= SPIKE,
      # cdf = in_spike*(cdf_spike*p_spike) + (1-in_spike)*(p_spike + (1-p_spike)*cdf_tail)
      cdf = (1-in_spike)*(p_spike + (1-p_spike)*cdf_tail)
    ) 
  df1a
  
  df1b = df1a %>% select(draft_pick, is_QB, y, cdf) %>% arrange(draft_pick,y) 
  df1b
}

y_grid_A = c(0, 0.001, mean_spike_val, seq(0.005,0.25,length.out=100))
y_grid_A
df_A = tibble(expand.grid(draft_pick = 1:256, y = y_grid_A, is_QB=c(0,1)))
df_A
cdf_spike_betareg_QB(df_A)

df_v1_q = 
  cdf_spike_betareg_QB(tibble(expand.grid(draft_pick = 1:256, y = c(0.04,0.06,0.08, .15, .2, .23), is_QB=c(0,1)))) %>%
  group_by(y,is_QB) %>%
  mutate(p = 1 - cdf) %>%
  mutate(v1 = p/first(p)) 
df_v1_q
df_v1_q %>%
  mutate(QB = ifelse(is_QB, "QB", "not QB")) %>%
  ggplot(aes(x=draft_pick, y=v1, color=factor(y))) +
  facet_wrap(~ QB) +
  geom_line()
  # geom_point()

###########################################################
### BAYSEIAN spike + beta regression model of P(y|x,QB) ###
### need shrinkage since so few QB observations
###########################################################

library(brms)

# Bayesian Beta regression
priors <- c(set_prior("student_t(3, 0, 2.5)", class = "Intercept"),
            set_prior("normal(0, 1)", class = "b"))
model_betareg_QB_tail_bayes <- 
  brm(
    bf(
      apy_cap_pct_2C ~ draft_pick,
      phi ~ draft_pick
    ),
    data = players_2C %>% filter(apy_cap_pct_2C > SPIKE),
    family = Beta(),
    prior = priors,
    # chains = 4, iter = 2000, warmup = 1000,
    # cores = 4, seed = 1234, 
    chains = 1, iter = 2000, warmup = 1000,
    cores = 1, seed = 1234, 
    # # Use the cmdstanr backend for Stan because it's faster and more modern than
    # # the default rstan You need to install the cmdstanr package first
    # # (https://mc-stan.org/cmdstanr/) and then run cmdstanr::install_cmdstan() to
    # # install cmdstan on your computer.
    # backend = "cmdstanr",
    # file = "model_beta_bayes"  # Save this so it doesn't have to always rerun
  )
summary(model_betareg_QB_tail_bayes)

model_betareg_QB_tail_bayes

tibble(draft_pick = 1:256) %>%
  mutate(
    mu_L = predict(model_betareg_QB_tail_bayes, .)[,"Q2.5"],
    mu = predict(model_betareg_QB_tail_bayes, .)[,"Estimate"],
    mu_U = predict(model_betareg_QB_tail_bayes, .)[,"Q97.5"],
  ) %>%
  ggplot(aes(x = draft_pick)) +
  geom_line(aes(y = mu_L)) +
  geom_line(aes(y = mu)) +
  geom_line(aes(y = mu_U))



### spike + beta regression model
SPIKE = 0.01





