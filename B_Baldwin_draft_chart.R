
##################
### load stuff ###
##################

library(rvest)
library(gt)
library(patchwork)
library(tidyverse)
library(splines)
library(RColorBrewer)

### plotting pre-sets
theme_set(theme_bw())
theme_update(
  text = element_text(size=20),
  plot.title = element_text(hjust = 0.5),
  axis.title = element_text(size=20),
  axis.text = element_text(size=20),
  legend.text = element_text(size=20),
  legend.title = element_text(size=20),
  panel.spacing = unit(2, "lines")
) 

#######################################################
### How expensive is each draft pick's rookie deal? ###
#######################################################

# The first thing we need is the actual cost of each draft pick's contract. 
# Some googling led me to [this page](https://www.spotrac.com/nfl/draft/). 

url <- "https://web.archive.org/web/20230324102438/https://www.spotrac.com/nfl/draft/"
page <- read_html(url) %>% html_table()

# count rounds
r <- 0

t <- map_df(1:length(page), ~{
  
  item <- page[[.x]]
  
  if (nrow(item) > 5) {
    
    r <<- r + 1
    
  item %>%
    janitor::clean_names() %>%
    dplyr::slice(-1) %>%
    # hack for getting 2nd to last column when needed
    dplyr::select(c(1, 7, (ncol(item) - 2))) %>%
    set_names(c("pick", "value", "value2")) %>%
    mutate(value = ifelse(value == "", value2, value)) %>%
    # lol dolphins forfeited their pick
    filter(value != "-") %>%
    mutate(
      pick = stringr::str_remove(pick, "\\s\\(Comp\\)"),
      pick = as.integer(pick),
      # for some reason as.numeric doesn't work so have to do this insane thing
      value = stringr::str_remove(value, "\\$"),
      v1 = stringr::str_extract(value, "[:digit:]*(?=\\,)") %>% as.integer(),
      v2 = stringr::str_extract(value, "(?<=\\,)[:digit:]*(?=\\,)") %>% as.integer(),
      v3 = stringr::str_extract(value, "(?<=\\,)[:digit:]*$") %>% as.integer(),
      # thousands per year avg
      val = (1000000 * v1 + 1000 * v2 + v3) / 4000
    ) %>%
    select(pick, value, val) %>%
      mutate(round = r)
  }
  
}) %>%
  # fix dolphins missing pick
  mutate(
    pick = ifelse(pick > 20, pick + 1, pick)
  )

# average cap over next 4 seasons (in thousands of $)
  cap_2023 <- 224800000
  cap_tot <- (cap_2023 + cap_2023 * 1.07 + cap_2023 * (1.07^2) + cap_2023 * (1.07^3)) / 4000
  
# get APY % of cap by draft position
  contracts <- t %>%
    mutate(
      # fill in the random missing picks
      row = (1 : n()) + 1,
      pick = ifelse(is.na(pick), row, pick),
      # calculate % of cap by dividing by average cap over next 4 seasons
      contract_cap_pct = val / cap_tot
    ) %>%
    select(draft_overall = pick, contract_cap_pct) 
  
# save so don't have to re-scrape if things break later
write_csv(contracts, "df_Baldwin_contracts.csv")
contracts <- read_csv("df_Baldwin_contracts.csv")

# First, we're assuming cap growth rate of 7% over the next 3 seasons (i.e., 2024, 2025, and 2026) before converting the total amount of the rookie deal into a percentage of the cap. 
# For example, the 2023 cap is about $225 million, while the average of the 2023 through 2026 caps is about $250 million using this procedure. 
# Taking the Bears' No. 1 pick for example, we have total value of about $41 million, which is about $10.2 million per season, or 10.2/250 = 4.1 percent of the cap. 
# If we assumed no growth in the salary cap rates, then we would divide 10.2 by 225 which is about 4.5 percent of the cap. 
# In other words, we'd be counting the high draft picks as too expensive.

##########################################
### How productive has each pick been? ###
##########################################

# take the APY in terms of percent of the cap for each drafted player's second deal 
# (i.e., first deal after rookie contract). 
# For players that did not receive a second contract, 
# we'll assign them a value of zero. 
# We then smooth these values to get a smooth relationship between pick number and on-field performance.

players <- 
  nflreadr::load_contracts() %>%
  filter(between(draft_year, 2011, 2019)) %>%
  arrange(otc_id, year_signed) %>%
  group_by(otc_id) %>% 
  dplyr::slice(1:2) %>%
  mutate(
    contracts = n(), row = 1 : n()
    ) %>%
  select(player, otc_id, position, team, draft_year, year_signed, years, value, apy, apy_cap_pct, draft_overall, contracts, row) %>%
  filter(
    !is.na(draft_overall),        # was drafted
    position != "QB",             # is not a QB
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
    side = ifelse(
      pos %in% c("IOL", "RB/FB", "OT", "TE", "WR"), "Offense", "Defense"
    ),
    # overall fit
    fit = loess(apy_cap_pct ~ draft_overall)$fitted
  ) %>%
  group_by(pos) %>%
  # positional fit
  mutate(pos_fit = 100 * loess(apy_cap_pct ~ draft_overall)$fitted) %>%
  ungroup()

# combine the data: all positions
picks <- players %>%
  group_by(draft_overall) %>%
  slice_head(n = 1)

data <- picks %>%
  full_join(contracts, by = "draft_overall") %>%
  ungroup() %>%
  mutate(
    contract_cap_pct = ifelse(
      draft_overall %in% c(21) & is.na(contract_cap_pct), (lead(contract_cap_pct) + lag(contract_cap_pct))/2, contract_cap_pct
    ),
    surplus = fit - contract_cap_pct
  ) %>%
  select(draft_overall, fit, surplus, contract_cap_pct) %>%
  pivot_longer(2:4) %>%
  # convert to % of cap
  mutate(value = value * 100) %>%
  filter(draft_overall <= 256)

# contract for picks 1-256
cleaned_contract <- data %>% 
  filter(name == "contract_cap_pct") %>%
  select(draft_overall, contract_cap_pct = value)

# combine the data: by position
picks_pos <- players %>%
  group_by(draft_overall, pos) %>%
  slice_head(n = 1) %>%
  left_join(cleaned_contract, by = "draft_overall") %>%
  ungroup() %>%
  # mutate(pos_surplus = pos_fit - contract_cap_pct) %>%
  mutate(pos_surplus = pos_fit) %>%
  select(draft_overall, pos, side, pos_surplus)

#############################
### Show the draft curves ###
#############################
    
data %>%
  filter(draft_overall <= 100) %>%
  ggplot(aes(draft_overall, value, color = name)) +
  geom_hline(yintercept = 0) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(
    title = "The value of (non-QB) draft picks",
    x = "Draft Pick",
    y = "Value in % of salary cap",
    color = ""
  ) +
  scale_x_continuous(breaks = c(1, 10, 32, 64, 100), expand = c(.01, .01)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5), expand = c(0, 0.05)) +
  ggthemes::theme_fivethirtyeight() +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text.x=element_text(hjust=0.5),
    axis.title.x = element_text(size=12, face="bold"),
    axis.title.y = element_text(size=12, face="bold"),
    legend.position=c(.8,.8),
    legend.direction = "vertical",
    legend.text = element_text(size = 20),
    legend.background = element_rect(fill = alpha("white", 0))
  ) +
  scale_color_manual(
    values=c("#d95f02", "#7570b3", "#1b9e77"), 
    labels = c("Contract cost", "On-field value", "Surplus value")
  )


# We have re-produced the now-familiar result that for teams NOT drafting a quarterback, 
# the surplus value of the very top picks is lower than later in the first round 
# and even throughout most of the second round. 
# This is because the dropoff in salary that teams have to pay to draft picks (orange line) 
# is steeper than the dropoff in on-field value (purple line). 
# This reflects the fact that teams are not very good at identifying the best players in the draft. 
# We have [a lot of evidence that this is the case](https://www.pff.com/news/draft-surplus-value-of-each-position-in-the-nfl-draft). 

# This pattern of later first round picks having higher surplus value than early first round picks 
# was originally referred to as the loser's curse 
# in the [seminal paper by Cade Massey and Richard Thaler]
# Massey and Thaler wrote their paper during the pre-2011 CBA when rookie contracts 
# were a lot more expensive; however, the pattern still holds up now. 

# In addition, we have replicated earlier findings by 
# [PFF's Timo Riske](https://www.pff.com/news/draft-surplus-value-of-each-position-in-the-nfl-draft) 
# using a similar approach that took advantage of player contract data and yet another study [by Riske](https://www.pff.com/news/nfl-revisiting-the-losers-curse-the-surplus-value-of-draft-picks) 
# that used PFF Wins Above Replacement (WAR).

##########################################
### Comparison with other draft curves ###
##########################################

# The three draft charts that I tend to see the most often are the [OTC chart]
# (https://overthecap.com/draft-trade-value-chart), 
# the [PFR chart](https://www.footballperspective.com/draft-value-chart/) made by Chase Stuart based on AV,
# and the original Jimmy Johnson chart. 
# Here's how they compare with the surplus and on-field values shown above. 

raw_otc <- read_html("https://overthecap.com/draft-trade-value-chart") %>%
  html_table() %>%
  pluck(1) %>%
  janitor::clean_names() 

# what peak coding performance looks like
otc <- bind_rows(
  raw_otc %>% select(pick, value),
  raw_otc %>% select(pick = pick_2, value = value_2),
  raw_otc %>% select(pick = pick_3, value = value_3),
  raw_otc %>% select(pick = pick_4, value = value_4)
) %>%
  select(draft_overall = pick, otc_value = value) %>%
  mutate(otc_value = otc_value / dplyr::first(otc_value))

raw_pfr <- read_html("https://www.footballperspective.com/draft-value-chart/") %>%
  html_table() %>%
  pluck(1) %>%
  janitor::clean_names() %>%
  select(draft_overall = pk, av_val) %>%
  mutate(av_val = av_val / dplyr::first(av_val))

raw_jj <- read_html("https://www.drafttek.com/NFL-Trade-Value-Chart.asp") %>%
  html_table() %>%
  pluck(2) %>%
  janitor::clean_names() %>%
  select(x2, x4, x6) %>%
  filter(!is.na(x6))

jj <- bind_rows(
  raw_jj %>% select(value = x2) %>% filter(!is.na(value)),
  raw_jj %>% select(value = x4) %>% filter(!is.na(value)),
  raw_jj %>% select(value = x6) %>% filter(!is.na(value))
) %>%
  mutate(draft_overall = 1 : n()) %>%
  mutate(jj_value = value / dplyr::first(value)) %>%
  select(-value)

df <- data %>%
  filter(name %in% c("surplus", "fit")) %>%
  group_by(draft_overall, name) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  pivot_wider(names_from = "name", values_from = "value") %>%
  mutate(
    osf_surplus = surplus / max(surplus),
    osf_fit = fit / dplyr::first(fit)
    ) %>%
  left_join(otc, by = "draft_overall") %>%
  left_join(raw_pfr, by = "draft_overall") %>%
  left_join(jj, by = "draft_overall") %>%
  pivot_longer(2:8) %>%
  arrange(draft_overall, name)

plot_df <- df %>%
  mutate(value = value * 100) %>%
  filter(draft_overall <= 100, !name %in% c("fit", "surplus"))

plot_df %>%
  ggplot(aes(draft_overall, value, color = name)) +
  geom_hline(yintercept = 0) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(
    title = "Draft chart comparison",
    x = "Draft Pick",
    y = "Pct diff in value vs to No. 1 pick",
    color = ""
  ) +
  scale_x_continuous(breaks = c(1, 10, 32, 64, 100), expand = c(.01, .01)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5), expand = c(.05, 0.55)) +
  ggthemes::theme_fivethirtyeight() +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text.x=element_text(hjust=0.5),
    axis.title.x = element_text(size=12, face="bold"),
    axis.title.y = element_text(size=12, face="bold"),
    legend.position=c(.6,.95),
    # legend.direction = "vertical",
    legend.text = element_text(size = 16),
    legend.background = element_rect(fill = alpha("white", 0))
  ) +
  scale_color_manual(
    values=c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e"), 
    labels = c("PFR", "JJ", "BB (on-field)", "BB (surplus)", "OTC")
    )


# Because our surplus value chart doesn't have the first pick as the most valuable pick, 
# unsurprisingly, it looks very different than the other charts. 
# Interestingly, the player value curve (i.e., on-field value of player without accounting for cost of rookie contract) 
# is still flatter than the other three curves. T
# his shouldn't come as a surprise given that we have excluded quarterbacks 
# and the other charts do not.

#FIXME # MONEY MAKER QUOTE:
# Unsurprisingly, draft charts tend to overvalue the top picks 
# in the case when these picks are not used on a quarterback. 
# This adds to the mountain of evidence discussed above that 
# NFL teams largely do not understand 
# the relative values of draft picks 
# and are dramatically overconfident in their ability to pick the right players 
# at the top of the draft. 
# The original Jimmy Johnson chart, for example, 
# comes nowhere close to reflecting the true value of picks.

###############################
### Draft Value by Position ###
###############################

# Inspired by [Kevin Cole's post here](https://unexpectedpoints.substack.com/p/what-analytical-draft-value-curves), 
# we can also compute position-specific curves. 
# Let's show on-field value rather than surplus value 
# because surplus value is computed using costs that are invariant to position. 
# That is, to go from one to the other, 
# we would subtract the pick-specific cost from each curve.

def <- picks_pos %>%
  filter(side == "Defense") %>%
  filter(draft_overall <= 100, !pos %in% c("K", "P", "LS")) %>%
  ggplot(aes(draft_overall, pos_surplus, color = pos)) +
  geom_hline(yintercept = 0) +
  geom_smooth(size = 2, se = FALSE) + 
  labs(
    title = "Defense",
    x = "Draft Pick",
    y = "Value in % of salary cap",
    color = ""
  ) +
  scale_x_continuous(breaks = c(1, 10, 32, 64, 100), expand = c(.01, .01)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10), expand = c(0, 0.05)) +
  ggthemes::theme_fivethirtyeight() +
  theme(
    plot.title = element_text(size = 18, hjust = 0.5),
    axis.text.x=element_text(hjust=0.5),
    axis.title.x = element_text(size=12, face="bold"),
    axis.title.y = element_text(size=12, face="bold"),
    legend.position=c(.8,.8),
    legend.direction = "vertical",
    legend.text = element_text(size = 24),
    legend.background = element_rect(fill = alpha("white", 0))
  ) +
  scale_color_manual(values=c(
    "#e41a1c", 
    "#377eb8", 
    "#4daf4a",
    "#984ea3",
    "#ff7f00"
  )
  )

off <- picks_pos %>%
  filter(side == "Offense") %>%
  filter(draft_overall <= 100, !pos %in% c("K", "P", "LS")) %>%
  ggplot(aes(draft_overall, pos_surplus, color = pos)) +
  geom_hline(yintercept = 0) +
  geom_smooth(size = 2, se = FALSE) + 
  labs(
    title = "Offense",
    x = "Draft Pick",
    y = "Value in % of salary cap",
    color = ""
  ) +
  scale_x_continuous(breaks = c(1, 10, 32, 64, 100), expand = c(.01, .01)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10), expand = c(0, 0.05)) +
  ggthemes::theme_fivethirtyeight() +
  theme(
    plot.title = element_text(size = 18, hjust = 0.5),
    axis.text.x=element_text(hjust=0.5),
    axis.title.x = element_text(size=12, face="bold"),
    axis.title.y = element_text(size=12, face="bold"),
    legend.position=c(.8,.8),
    legend.direction = "vertical",
    legend.text = element_text(size = 24),
    legend.background = element_rect(fill = alpha("white", 0))
  ) +
  scale_color_manual(values=c(
    "#e41a1c", 
    "#377eb8", 
    "#4daf4a",
    "#984ea3",
    "#ff7f00"
  )
  )

off + def &
  ggthemes::theme_fivethirtyeight() &
  theme(
    plot.title = element_text(size = 18, hjust = 0.5),
    axis.text.x=element_text(hjust=0.5),
    axis.title.x = element_text(size=12, face="bold"),
    axis.title.y = element_text(size=12, face="bold"),
    legend.position=c(.8,.8),
    legend.direction = "vertical",
    legend.text = element_text(size = 12),
    legend.background = element_rect(fill = alpha("white", 0))
  ) &
  scale_y_continuous(limits = c(0, 8), expand = c(0, 0.05), breaks = scales::pretty_breaks(n = 10)) &
  plot_annotation(
    title = 'The Value of (non-QB) Draft Picks',
    subtitle = 'Draft Curves from 2011-2019 Draft Classes',
    caption = '@benbbaldwin',
    theme = theme(
      plot.title = element_text(size = 18, hjust = 0.5, face="bold"),
      plot.subtitle = element_text(size = 14, hjust = 0.5)
      )
  )
  
# Looking at the results, we see that RB (offense) and Safety (defense) 
# are the low positions on the totem pole. 
# At the top, the highest returns have been for pass rushers (both edge and inside) 
# and offensive tackles.

