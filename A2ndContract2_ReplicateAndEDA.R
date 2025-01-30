
### load header
source("A2ndContract1_Header.R")

####################################
### Replicate Massey Thaler 2013 ###
####################################

plot_Massey_Thaler_line_labels = c(
  "performance" = "Expected\nperformance\nvalue\n",
  "compensation" = "Cost",
  "surplus" = "Expected\nsurplus\nvalue\n", 
  "JJ" = "Jimmy\nJohnson\n", 
  "market" = "\nFitted\ntrade\nmarket"
)
plot_Massey_Thaler_2B = 
  df_plot_Massey_Thaler_0 %>%
  # left_join(df_jj %>% select(-value_jj) %>% rename(JJ = jj_v1)) %>%
  left_join(df_trade_market_weibull %>% rename(market=V_G1) %>% select(-desc)) %>%
  pivot_longer(-draft_pick) %>%
  mutate(
    ordering = case_when(
      str_detect(name, "cost") ~ 2,
      str_detect(name, "surplus") ~ 3,
      str_detect(name, "perf") ~ 1,
      str_detect(name, "trade") ~ 4,
      str_detect(name, "Jimmy") ~ 5,
      TRUE ~ 6,
    ),
  ) %>%
  ggplot(aes(x=draft_pick,y=value,color=fct_reorder(name,ordering),linetype=fct_reorder(name,ordering))) +
  # ggplot(aes(x=draft_pick,y=value,color=name)) +
  geom_hline(yintercept=1, linetype="dashed", color="gray60", linewidth=1) +
  geom_hline(yintercept=0, linetype="dashed", color="gray60", linewidth=1) +
  geom_line(linewidth=2) +
  scale_color_manual(
    name="", 
    labels = plot_Massey_Thaler_line_labels,
    values=c(
      "performance" = "black",
      "surplus" = "black", 
      "market" = "black",
      "compensation" = "gray60",
      "JJ" = "violet"
    ),
  ) +
  scale_linetype_manual(
    name="", 
    labels = plot_Massey_Thaler_line_labels,
    values=c(
      "performance" = "dotted",
      "compensation" = "solid",
      "surplus" = "longdash", 
      "JJ" = "solid", 
      "market" = "solid"
    ),
  ) +
  theme(legend.key.width=unit(2.5,"cm")) +
  xlab("Draft position") +
  ylab("Value relative to first pick") +
  scale_x_continuous(breaks=seq(1,32*9,by=32*2))
# plot_Massey_Thaler_2B
ggsave("plots_ReplicateAndEDA/plot_Massey_Thaler_replicate.png", width=8, height=4)

#####################################
### Performance value by position ###
#####################################

# df_byPos1 = 
#   players_2C %>%
#   filter(!pos %in% c("LS", "K", "P")) %>%
#   mutate(side = ifelse(pos %in% c("CB","ED", "IDL", "LB", "S"), "Defense", "Offense")) %>%
#   select(draft_pick,pos,side,apy_cap_pct_2C)
# df_byPos1
# 
# ymax = 0.08
# 
# plot_pos_curve_Off = 
#   df_byPos1 %>%
#   filter(side=="Offense") %>%
#   group_by(pos) %>%
#   mutate(
#     # fit = loess(apy_cap_pct_2C ~ draft_pick)$fitted
#     fit = loess(apy_cap_pct_2C ~ draft_pick, degree=1)$fitted
#   ) %>%
#   ungroup() %>%
#   ggplot(aes(x=draft_pick, y=fit, color=pos)) +
#   geom_line(linewidth=2) +
#   xlab("draft pick") +
#   labs(title="Offense") +
#   scale_color_brewer(name="", palette = "Set2") +
#   ylab("apy cap pct") +
#   ylim(c(0,ymax)) +
#   scale_x_continuous(breaks=seq(1,32*9,by=32*2))
# # plot_pos_curve_Off
# 
# plot_pos_curve_Def = 
#   df_byPos1 %>%
#   filter(side=="Defense") %>%
#   group_by(pos) %>%
#   mutate(
#     # fit = loess(apy_cap_pct_2C ~ draft_pick)$fitted
#     fit = loess(apy_cap_pct_2C ~ draft_pick, degree=1)$fitted
#   ) %>%
#   ungroup() %>%
#   ggplot(aes(x=draft_pick, y=fit, color=pos)) +
#   geom_line(linewidth=2) +
#   xlab("draft pick") +
#   labs(title="Defense") +
#   scale_color_brewer(name="", palette = "Set2") +
#   ylab("apy cap pct") +
#   ylim(c(0,ymax)) +
#   scale_x_continuous(breaks=seq(1,32*9,by=32*2))
# # plot_pos_curve_Def
# 
# plot_pos = plot_pos_curve_Off + plot_pos_curve_Def
# plot_pos
# ggsave("plots_ReplicateAndEDA/plot_pos_replication.png", width=15, height=5)


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
  geom_point(color="gray60") +
  # geom_smooth(se=F, linewidth=2, color="dodgerblue2") +
  geom_smooth(se=F, linewidth=2, color="black") +
  xlab("Draft position") +
  # ylab("apy cap pct") +
  ylab("Percentage of cap") +
  labs(title = "Empirical conditional mean") +
  scale_y_continuous(labels = percent_format()) +
  scale_x_continuous(breaks=seq(1,32*9,by=32*2))
# plot_empWithCondMean

plot_empWithCondSd = 
  df_overall_emp_musd_tail %>%
  ggplot(aes(x = draft_pick, y=emp_sd_tail)) +
  geom_point(color="gray60") +
  # geom_smooth(se=F, linewidth=2, color="dodgerblue2") +
  geom_smooth(se=F, linewidth=2, color="black") +
  xlab("Draft position") +
  # ylab("apy cap pct") +
  ylab("Percentage of cap") +
  labs(title = "Empirical conditional s.d.") +
  scale_y_continuous(labels = percent_format()) +
  scale_x_continuous(breaks=seq(1,32*9,by=32*2))
# plot_empWithCondSd

plot_empWithCondLines = plot_empWithCondMean + plot_empWithCondSd
# plot_empWithCondLines
ggsave("plots_ReplicateAndEDA/plot_empMeanSd.png", width=10, height=4)

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
    xlab("Percentage of cap") +
    ylab("Density") +
    scale_x_continuous(
      labels = percent_format(), 
      breaks = seq(0,1,by=0.05),
      limits = c(0, 0.12),
    ) +
    theme(
      # axis.text.x = element_text(size = 15),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
      panel.spacing = unit(1, "lines")
    )  + 
    geom_histogram(aes(x = apy_cap_pct_2C, y=after_stat(density)), fill="gray80") +
    geom_density(aes(x = apy_cap_pct_2C), linewidth=1, color="gray60") 
  if (saveMe) {
    # browser()
    filepath = paste0("plots_ReplicateAndEDA/plot_density_byPick", 
                      "_", paste0(ex_draft_picks, collapse="_"),
                      "_emp", ".png"
    )
    ggsave(filepath, p, width = 9, height=7) 
  } else {
    return(p)
  }
}

plot_cond_density(seq(1,64*2,by=32/4),T)
plot_cond_density(seq(1,64,by=32/8),T)
plot_cond_density(seq(1,64*4,by=32/2),T)

##################
### Eyeballin' ###
##################

players_2C

r = 0.15
sum(players_2C$apy_cap_pct_2C >= r)
mean(players_2C$apy_cap_pct_2C >= r)*100

head(players_2C %>% arrange(-apy_cap_pct_2C) %>% 
       select(player, apy_cap_pct_2C, draft_pick, draft_year), 20)




