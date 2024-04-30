
### load header
source("A2ndContract1_Header.R")

###########
### EDA ###
###########

### EDA: empirical conditional mean and s.d.
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
  geom_smooth(se=F, linewidth=1, color="dodgerblue2") +
  xlab("draft pick") + ylab("apy cap pct") +
  labs(title = "empirical conditional mean") +
  scale_x_continuous(breaks=seq(1,32*9,by=32*2))
# plot_empWithCondMean

plot_empWithCondSd = 
  df_overall_emp_musd_tail %>%
  ggplot(aes(x = draft_pick, y=emp_sd_tail)) +
  geom_point() +
  geom_smooth(se=F, linewidth=1, color="dodgerblue2") +
  xlab("draft pick") + ylab("apy cap pct") +
  labs(title = "empirical conditional s.d.") +
  scale_x_continuous(breaks=seq(1,32*9,by=32*2))
# plot_empWithCondSd

plot_empWithCondLines = plot_empWithCondMean + plot_empWithCondSd
# plot_empWithCondLines
ggsave("plots_EDA/plot_empMeanSd.png", width=12, height=5)

###########
### EDA ###
###########

### EDA: full dists
ex_draft_picks = seq(1,32*7,by=32/2)

plot_cond_density <- function(ex_draft_picks, saveMe=F) {
  p = 
    players_2C %>% 
    select(draft_pick, apy_cap_pct_2C) %>%
    filter(draft_pick %in% ex_draft_picks) %>%
    ggplot() +
    facet_wrap(~ draft_pick) +
    xlab("apy cap pct") +
    ylab("density") +
    labs(title = "conditional density") +
    theme(
      axis.text.x = element_text(size = 10),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()
    )  + 
    geom_histogram(aes(x = apy_cap_pct_2C, y=after_stat(density)), fill="gray80") +
    geom_density(aes(x = apy_cap_pct_2C), linewidth=1, color="gray60") 
  if (saveMe) {
    # browser()
    filepath = paste0("plots_EDA/plot_density_byPick", 
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




