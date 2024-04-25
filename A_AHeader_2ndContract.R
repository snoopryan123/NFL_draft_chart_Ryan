
### spike / bust cutoff
bust_cutoff = 0.01

##################
### load stuff ###
##################

### packages
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

################################
### Get second contract data ###
################################

# SCRAPE_DATA = TRUE
SCRAPE_DATA = FALSE
filename_data_2ndContract = "data_2ndContract.csv"

if (SCRAPE_DATA | !file.exists(filename_data_2ndContract)) {
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
      bust = as.numeric(apy_cap_pct <= bust_cutoff),
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
  
  ### save data
  write_csv(players_2C, filename_data_2ndContract) 
} else {
  players_2C = read_csv(filename_data_2ndContract, show_col_types = F) 
}
print(players_2C)


