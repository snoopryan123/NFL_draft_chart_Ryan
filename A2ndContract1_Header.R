
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

# SCRAPE_2NDCONTRACT_DATA = TRUE
SCRAPE_2NDCONTRACT_DATA = FALSE
filename_data_2ndContract = "data_2ndContract.csv"

if (SCRAPE_2NDCONTRACT_DATA | !file.exists(filename_data_2ndContract)) {
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

#######################################################
### How expensive is each draft pick's rookie deal? ###
#######################################################

### from Ben Baldwin:

# SCRAPE_COMPENSATION_DATA = TRUE
SCRAPE_COMPENSATION_DATA = FALSE
filename_data_compensation = "data_1stContractCompensation.csv"

if (SCRAPE_COMPENSATION_DATA | !file.exists(filename_data_compensation)) {
  
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

  # First, we're assuming cap growth rate of 7% over the next 3 seasons (i.e., 2024, 2025, and 2026) 
  # before converting the total amount of the rookie deal into a percentage of the cap. 
  # For example, the 2023 cap is about $225 million, while the average of the 2023 through 2026 caps 
  # is about $250 million using this procedure. 
  # Taking the Bears' No. 1 pick for example, we have total value of about $41 million, 
  # which is about $10.2 million per season, or 10.2/250 = 4.1 percent of the cap. 
  # If we assumed no growth in the salary cap rates, then we would divide 10.2 by 225 
  # which is about 4.5 percent of the cap. 
  # In other words, we'd be counting the high draft picks as too expensive.

  # average cap over next 4 seasons (in thousands of $)
  cap_2023 <- 224800000
  cap_tot <- (cap_2023 + cap_2023 * 1.07 + cap_2023 * (1.07^2) + cap_2023 * (1.07^3)) / 4000
  
  # get APY % of cap by draft position
  compensation_1C_0 <- t %>%
    mutate(
      # calculate % of cap by dividing by average cap over next 4 seasons
      rookie_contract_cap_pct = val / cap_tot,
      ) %>%
    select(draft_pick = pick, rookie_contract_cap_pct) 
  compensation_1C_0
  
  # fill in the NA compensation values
  compensation_1C =
    tibble(draft_pick = 1:max(compensation_1C_0$draft_pick)) %>%
    left_join(compensation_1C_0) %>%
    mutate(
      rookie_contract_cap_pct = ifelse(
        is.na(rookie_contract_cap_pct) ,
        (lag(rookie_contract_cap_pct) + lead(rookie_contract_cap_pct))/2,
        rookie_contract_cap_pct
      ),
      # value relative to first pick
      compensation_v1 = rookie_contract_cap_pct/first(rookie_contract_cap_pct),
      # # smoothed value relative to first pick
      # compensation_v1s = loess(compensation_v1 ~ draft_pick)$fitted
    )
  compensation_1C
  
  ### save data
  write_csv(compensation_1C, filename_data_compensation) 
} else {
  compensation_1C = read_csv(filename_data_compensation, show_col_types = F) 
}
print(compensation_1C)





