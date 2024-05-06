
### load header
source("A2ndContract1_Header.R")

####################################
### Replicate Massey Thaler 2013 ###
####################################

players_2C %>%
  left_join(compensation_1C) %>%
  select(draft_pick, apy_cap_pct_2C, rookie_contract_cap_pct, compensation_v1)

EV_model = lm(data=players_2C, apy_cap_pct_2C~bs(draft_pick,df=5))
EV_model

df_plot_Massey_Thaler = 
  tibble(draft_pick = 1:256) %>%
  left_join(compensation_1C) %>%
  mutate(
    compensation0 = rookie_contract_cap_pct,
    performance0 = predict(EV_model, .),
    surplus0 = performance0 - rookie_contract_cap_pct,
    compensation = compensation0/first(compensation0),
    performance = performance0/first(performance0),
    surplus = surplus0/first(surplus0),
  ) %>%
  select(draft_pick,performance,compensation,surplus) %>%
  rename(
    `performance\n(predicted\nsecond contract\ncompensation)\n` = performance,
    `first contract\ncompensation\n` = compensation,
  ) 
df_plot_Massey_Thaler

plot_Massey_Thaler = 
  df_plot_Massey_Thaler %>%
  pivot_longer(-draft_pick) %>%
  ggplot(aes(x=draft_pick,y=value,color=name)) +
  geom_line(linewidth=2) +
  xlab("draft pick") +
  labs(title="Massey Thaler Curves") +
  scale_color_manual(name="", values=c("firebrick", "dodgerblue2", "forestgreen")) +
  ylab("value relative to first pick") +
  # labs(title = "posterior mean relative EV \U03BC(x)/\U03BC(x=1)") +
  scale_x_continuous(breaks=seq(1,32*9,by=32*2))
# plot_Massey_Thaler
ggsave("plots_ReplicateAndEDA/plot_Massey_Thaler_replication.png", width=9, height=5)

plot_Massey_Thaler_1 = 
  df_plot_Massey_Thaler_1 %>%
  left_join(df_jj %>% select(-value_jj) %>% rename(`Jimmy Johnson\n` = jj_v1)) %>%
  left_join(df_trade_market_weibull %>% rename(`Weibull`=V_G1) %>% select(-desc)) %>%
  pivot_longer(-draft_pick) %>%
  ggplot(aes(x=draft_pick,y=value,color=name)) +
  geom_line(linewidth=2) +
  xlab("draft pick") +
  labs(title="Massey Thaler Curves") +
  scale_color_manual(name="", values=c("firebrick", "violet", "dodgerblue2", "forestgreen",  "orange")) +
  ylab("value relative to first pick") +
  # labs(title = "posterior mean relative EV \U03BC(x)/\U03BC(x=1)") +
  scale_x_continuous(breaks=seq(1,32*9,by=32*2))
ggsave("plots_ReplicateAndEDA/plot_Massey_Thaler_replication_1.png", width=9, height=5)

#####################################
### Performance value by position ###
#####################################

df_byPos1 = 
  players_2C %>%
  filter(!pos %in% c("LS", "K", "P")) %>%
  mutate(side = ifelse(pos %in% c("CB","ED", "IDL", "LB", "S"), "Defense", "Offense")) %>%
  select(draft_pick,pos,side,apy_cap_pct_2C)
df_byPos1

ymax = 0.08

plot_pos_curve_Off = 
  df_byPos1 %>%
  filter(side=="Offense") %>%
  group_by(pos) %>%
  mutate(
    # fit = loess(apy_cap_pct_2C ~ draft_pick)$fitted
    fit = loess(apy_cap_pct_2C ~ draft_pick, degree=1)$fitted
  ) %>%
  ungroup() %>%
  ggplot(aes(x=draft_pick, y=fit, color=pos)) +
  geom_line(linewidth=2) +
  xlab("draft pick") +
  labs(title="Offense") +
  scale_color_brewer(name="", palette = "Set2") +
  ylab("apy cap pct") +
  ylim(c(0,ymax)) +
  scale_x_continuous(breaks=seq(1,32*9,by=32*2))
# plot_pos_curve_Off

plot_pos_curve_Def = 
  df_byPos1 %>%
  filter(side=="Defense") %>%
  group_by(pos) %>%
  mutate(
    # fit = loess(apy_cap_pct_2C ~ draft_pick)$fitted
    fit = loess(apy_cap_pct_2C ~ draft_pick, degree=1)$fitted
  ) %>%
  ungroup() %>%
  ggplot(aes(x=draft_pick, y=fit, color=pos)) +
  geom_line(linewidth=2) +
  xlab("draft pick") +
  labs(title="Defense") +
  scale_color_brewer(name="", palette = "Set2") +
  ylab("apy cap pct") +
  ylim(c(0,ymax)) +
  scale_x_continuous(breaks=seq(1,32*9,by=32*2))
# plot_pos_curve_Def

plot_pos = plot_pos_curve_Off + plot_pos_curve_Def
# plot_pos
ggsave("plots_ReplicateAndEDA/plot_pos_replication.png", width=15, height=5)


################################################
### EDA: empirical conditional mean and s.d. ###
################################################

df_overall_emp_musd_tail =
  players_2C %>%
  group_by(draft_pick) %>%
  summarise(
    emp_mean_tail = mean(apy_cap_pct_2C),
    emp_sd_tail = sd(apy_cap_pct_2C),
    .groups = "drop"
  ) 
df_overall_emp_musd_tail

plot_empWithCondMean = 
  df_overall_emp_musd_tail %>%
  ggplot(aes(x = draft_pick, y=emp_mean_tail)) +
  geom_point() +
  geom_smooth(se=F, linewidth=2, color="dodgerblue2") +
  xlab("draft pick") + ylab("apy cap pct") +
  labs(title = "empirical conditional mean") +
  scale_x_continuous(breaks=seq(1,32*9,by=32*2))
# plot_empWithCondMean

plot_empWithCondSd = 
  df_overall_emp_musd_tail %>%
  ggplot(aes(x = draft_pick, y=emp_sd_tail)) +
  geom_point() +
  geom_smooth(se=F, linewidth=2, color="dodgerblue2") +
  xlab("draft pick") + ylab("apy cap pct") +
  labs(title = "empirical conditional s.d.") +
  scale_x_continuous(breaks=seq(1,32*9,by=32*2))
# plot_empWithCondSd

plot_empWithCondLines = plot_empWithCondMean + plot_empWithCondSd
# plot_empWithCondLines
ggsave("plots_ReplicateAndEDA/plot_empMeanSd.png", width=12, height=5)

############################################
### EDA: empirical conditional densities ###
############################################

ex_draft_picks = seq(1,32*7,by=32/2)

plot_cond_density <- function(ex_draft_picks, saveMe=F) {
  p = 
    players_2C %>% 
    select(draft_pick, apy_cap_pct_2C) %>%
    filter(draft_pick %in% ex_draft_picks) %>%
    mutate(
      draft_pick_x = paste0("x = ", draft_pick),
      draft_pick_x = fct_reorder(draft_pick_x, draft_pick),
    ) %>%
    ggplot() +
    facet_wrap(~ draft_pick_x) +
    xlab("apy cap pct") +
    ylab("density") +
    # labs(title = "conditional density") +
    theme(
      axis.text.x = element_text(size = 10),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()
    )  + 
    geom_histogram(aes(x = apy_cap_pct_2C, y=after_stat(density)), fill="gray80") +
    geom_density(aes(x = apy_cap_pct_2C), linewidth=1, color="gray60") 
  if (saveMe) {
    # browser()
    filepath = paste0("plots_ReplicateAndEDA/plot_density_byPick", 
                      "_", paste0(ex_draft_picks, collapse="_"),
                      "_emp", ".png"
    )
    ggsave(filepath, p, width = 10, height=10) 
  } else {
    return(p)
  }
}

plot_cond_density(seq(1,64,by=32/8),T)
plot_cond_density(seq(1,64*2,by=32/4),T)
plot_cond_density(seq(1,64*4,by=32/2),T)

############################################
###  ###
############################################

players_2C

r = 0.15
sum(players_2C$apy_cap_pct_2C >= r)
mean(players_2C$apy_cap_pct_2C >= r)*100

head(players_2C %>% arrange(-apy_cap_pct_2C) %>% 
       select(player, apy_cap_pct_2C, draft_pick, draft_year), 20)




