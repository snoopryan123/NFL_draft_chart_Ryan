
# Blake Zilberman
# Tue, Sep 10, 10:44pm

library(nflreadr)
library(tidyverse)
library(splines)

### plotting pre-sets
theme_set(theme_bw())
theme_update(
  text = element_text(size=20),
  plot.title = element_text(hjust = 0.5, size=20),
  plot.subtitle = element_text(size=15),
  axis.title = element_text(size=20),
  axis.text = element_text(size=20),
  legend.text = element_text(size=20),
  legend.title = element_text(size=20),
  panel.spacing = unit(2, "lines")
)

draft_picks <- load_draft_picks() 
contracts <- load_contracts(file_type = getOption("nflreadr.prefer", default = "rds"))

combined_data = 
  left_join(
    contracts,
    draft_picks %>% mutate(player = pfr_player_name),
    by = c("player", "position")
  ) %>%
  relocate(pfr_player_name, .after=player) %>%
  filter(season>=1997)
combined_data

names(combined_data)


###combined_data <- merge(draft_picks, contracts, by = "player")

model1 <- lm(apy_cap_pct ~ pick, data = combined_data)

model2 <- lm(apy_cap_pct ~ pick + I(pick^2), data = combined_data)

model3 <- lm(apy_cap_pct ~ pick + I(pick^2) + I(pick^3), data = combined_data)

model4 <- lm(apy_cap_pct ~ bs(pick,degree=3,df=5), data = combined_data)

model5 <- lm(apy_cap_pct ~ pick, data = combined_data)

preds_df = 
  tibble(pick = 1:400) %>%
  mutate(
    pred1 = predict(model1, .),
    pred2 = predict(model2, .),
    pred3 = predict(model3, .),
    pred4 = predict(model4, .),
    pred5 = predict(model5, .),
  )

combined_data_1 = 
  left_join(combined_data, preds_df, by="pick")
combined_data_1

ggplot(combined_data_1, aes(x = pick, y = value)) +
  #geom_point() +  
  ### geom_smooth(method = "lm", se = FALSE) +  
  # geom_function(fun = function(x>), colour = "red")+
  geom_line(aes(y = pred1), color="dodgerblue2", size=1) +
  geom_line(aes(y = pred2), color="red", size=1) +
  geom_line(aes(y = pred3), color="green", size=1) +
  geom_line(aes(y = pred4), color="purple", size=1) +
  geom_line(aes(y = pred5), color="yellow", size=1) +
  scale_x_continuous(breaks=seq(1,9*32,32)) +
  labs(title = "Relationship Between Draft Pick and Contract Value",
       x = "Draft Pick",
       y = "APY Cap %") +
  theme_minimal()


modeli <- lm(apy_cap_pct ~ bs(pick,degree=3,df=5), data = combined_data)

combined_data %>%
  mutate(pred=predict(modeli,.))%>%
  ggplot(aes(x = pick, y = apy_cap_pct)) +
  geom_point(shape=21, size=2) +  
  geom_line(aes(y = pred), color="purple", size=1) +
  labs(title = "Relationship Between Draft Pick and Contract Value",
       x = "Draft Pick",
       y = "APY Cap %") +
  scale_x_continuous(breaks=seq(1,9*32,32)) +
  theme_minimal()

combined_data %>%
  mutate(pred=predict(modeli,.))%>%
  group_by(pick) %>%
  mutate(
    emp_sd = sd(apy_cap_pct),
  ) %>%
  ungroup() %>%
  ggplot(aes(x = pick, y = apy_cap_pct)) +
  geom_point(shape=21, size=2) +
  geom_line(aes(y = pred), color="purple", size=1.5) +
  geom_smooth(aes(y = emp_sd), formula = y ~ s(x, bs = "cs", k=1), se=F,color="orange", size=1.5) +
  labs(title = "Relationship Between Draft Pick and Contract Value",
       x = "Draft Pick",
       y = "APY Cap %") +
  scale_x_continuous(breaks=seq(1,9*32,32)) +
  theme_minimal()

#given the draft pick x, how do we estimate the probability density of the value of the picp
picks=c(1,5,10,17,25,37,50,67,75,100,125)

plot_density_1 = 
  combined_data %>%
  filter(pick %in% picks) %>%
  mutate(
    pick_str = paste0("draft pick = ", pick),
    pick_str = fct_reorder(pick_str, pick)
  ) %>%
  ggplot(aes(x=apy_cap_pct)) +
  facet_wrap(~pick_str, scales="free_y")+
  xlab("APY Cap %") +
  ylab("Density") +
  scale_x_continuous(breaks=seq(0,1,0.1)) +
  #geom_histogram()
  geom_density()
plot_density_1
ggsave("plot_density_1.png", plot_density_1, width=12, height=8)

#####################################################################

### ordinary linear regression
olr1=lm(data=combined_data,
        apy_cap_pct ~ draft_overall
)
olr1 

### spline regression
sr1=lm(data=combined_data,
       apy_cap_pct ~ bs(draft_overall,knots=65)
)
sr1 

### visualize the fit
combined_data %>% 
  mutate(
    pred=predict(olr1,.)
  ) %>%
  ggplot(aes(x=draft_overall)) +
  geom_line(aes(y=pred))

### visualize the fit w/ scatter plot
combined_data %>% 
  mutate(
    pred=predict(olr1,.),
    pred_spline = predict(sr1,.)
  ) %>%
  group_by(draft_overall) %>%
  summarize(
    average_apy_cap_pct = mean(apy_cap_pct),
    pred=unique(pred),
    pred_spline=unique(pred_spline)
  ) %>%
  ggplot(aes(x=draft_overall)) +
  geom_line(aes(y=pred),size=1) + 
  geom_line(aes(y=pred_spline),size=1,color="red")+
  geom_point(aes(y=average_apy_cap_pct))+
  xlab("Draft Pick") +
  ylab("APY Cap %")+
  scale_x_continuous(breaks=seq(1,9*32,32))

####### beta regression
n=nrow(combined_data)
combined_data = combined_data %>% mutate(y=(apy_cap_pct*(n-1) + 1/2) / n)
library(betareg)
con_density_model_1 = betareg(data=combined_data, y ~ draft_overall)
con_density_model_1


con_density_model_2 = betareg(data=combined_data, y ~ draft_overall | draft_overall)
con_density_model_2

con_density_model_3 = betareg(data=combined_data, y ~ bs(draft_overall, df=5) | draft_overall)
con_density_model_3

con_density_model_4 = betareg(data=combined_data, y ~ I(draft_overall==1) +
                                bs(draft_overall, df=5) | draft_overall)
con_density_model_4

con_density_model_5 = betareg(data=combined_data %>% filter(y>0.01),
                              y ~ bs(draft_overall, df=5) | draft_overall)
con_density_model_5

#####lets plot the models
combined_data %>%
  filter(pick %in% picks) %>%
  ggplot(aes(x=apy_cap_pct)) +
  facet_wrap(~pick,scales="free_y")+
  #geom_histogram()
  geom_density()

combined_data %>% 
  mutate(
    pred=predict(olr1,.),
    pred_spline = predict(sr1,.),
    pred_betareg1=predict(con_density_model_1,.,type="response"),
    pred_betareg2=predict(con_density_model_2,.,type="response"),
    pred_betareg3=predict(con_density_model_3,.,type="response")
  ) %>%
  group_by(draft_overall) %>%
  summarize(
    average_apy_cap_pct = mean(apy_cap_pct),
    pred=unique(pred),
    pred_spline=unique(pred_spline),
    pred_betareg1=unique(pred_betareg1),
    pred_betareg2=unique(pred_betareg2),
    pred_betareg3=unique(pred_betareg3)
  ) %>%
  ggplot(aes(x=draft_overall)) +
  ####geom_line(aes(y=pred)) + 
  geom_line(aes(y=pred_spline),color="blue")+
  ####geom_line(aes(y=pred_betareg1),color="purple")+
  ##### geom_line(aes(y=pred_betareg2),color="green")+
  geom_line(aes(y=pred_betareg3),color="magenta")+
  geom_point(aes(y=average_apy_cap_pct))



##### plot variance curve
combined_data %>% 
  mutate(
    pred_betareg3=sqrt(predict(con_density_model_3,.,type="variance"))
  ) %>%
  group_by(draft_overall) %>%
  summarize(
    sd_apy_cap_pct = sd(apy_cap_pct),
    pred_betareg3=unique(pred_betareg3)
  ) %>%
  ggplot(aes(x=draft_overall)) +
  geom_line(aes(y=pred_betareg3),size=1 ,color="magenta")+
  geom_point(aes(y=sd_apy_cap_pct))+
  scale_x_continuous(breaks=seq(1,9*32,32)) +
  xlab("Draft Pick") +
  ylab("APY Cap %") 





#### visualize probability of y|x
combined_data %>%
  filter(pick %in% picks) %>%
  ggplot(aes(x=apy_cap_pct)) +
  facet_wrap(~pick,scales="free_y")+
  #geom_histogram()
  geom_density()

####expand.grid(draft_overall = 1:256,y=seq(0,1,length.out=100)) %>%
df_beta_params = 
  tibble(draft_overall=1:256) %>%
  mutate(
    ###mu=predict(con_density_model_4,.,type="response"),
    ###phi=predict(con_density_model_4,.,type="precision"),
    mu=predict(con_density_model_3,.,type="response"),
    phi=predict(con_density_model_3,.,type="precision"),
    shape1 = phi*mu,    
    shape2 = phi*(1-mu),
    ### And we transformed the parameters mu, phi to shape1, shape2
    ### Because the dbeta function uses shape1, shape2
    ### And the beta regression that we ran earlier uses mu, phi
  )
df_beta_params

####y_grid=seq(0,1,length.out=100)
y_grid=seq(0.001,0.3,length.out=100)
y_grid
df_plot_betareg_density = 
  ### evaluate the conditional beta density P(y|x)at all combinations of x and y
  expand.grid(draft_overall = 1:256,y=y_grid) %>% 
  as_tibble() %>%
  arrange(draft_overall) %>%
  left_join(df_beta_params) %>%
  mutate(
    ### density of Beta dist with params mu, phi = dbeta
    p=dbeta(y,shape1,shape2)
    ### p = P(Y=y|x)
  )
df_plot_betareg_density
#####Given draft_overall = 1, there is an associated beta distribution P(y|x=1),
#####and this beta distribution has parameters shape1=1.13 and shape2=23.8,
#####and given these beta distribution parameters we want to calculate the density
#####(aka the probability) P(y=0|x=1).

### This is the fitted density (estimated)
### We want to plot this on top of the empirical density (ie the raw histograms) to see if it is a good fit
plot_density_2 = df_plot_betareg_density %>%
  filter(draft_overall %in% picks) %>%
  mutate(
    pick_str = paste0("draft pick = ", draft_overall),
    pick_str = fct_reorder(pick_str, draft_overall)
  ) %>%
  ggplot(aes(x=y,y=p)) +
  geom_line()+
  facet_wrap(~pick_str, scales="free_y")+
  xlab("APY Cap %") +
  ylab("Density")
ggsave("plot_density_2.png", plot_density_2, width=12, height=8)


combined_data %>%
  filter(pick %in% picks) %>%
  ggplot(aes(x=apy_cap_pct)) +
  facet_wrap(~pick,scales="free_y")+
  #geom_histogram()
  geom_density()

plot_densities_together = function(picks,ymax = 0.3) {
  df_plot_1 = combined_data %>%
    select(pick, apy_cap_pct) %>%
    rename(draft_overall=pick) %>%
    bind_rows(df_plot_betareg_density)
  df_plot_1 %>%
    mutate(
      pick_str = paste0("draft pick = ", draft_overall),
      pick_str = fct_reorder(pick_str, draft_overall)
    ) %>%
    filter(draft_overall %in% picks) %>%
    ggplot()+
    facet_wrap(~pick_str,scales="free_y")+
    geom_histogram(aes(x=apy_cap_pct),fill="pink", alpha=0.5)+
    geom_density(aes(x=apy_cap_pct),color="red")+
    geom_line(aes(x=y,y=p)) +
    xlim(c(0,ymax))+
    ylab("Density")+
    xlab("APY Cap %")
}

###plot_densities_together(picks=c(1,2,3,4,5,6,10,50,100,150))
plot_density_3 = plot_densities_together(picks=seq(1,16,1))
ggsave("plot_density_3.png", plot_density_3, width=12, height=8)
plot_density_4 = plot_densities_together(picks=seq(1,128,8))
ggsave("plot_density_4.png", plot_density_4, width=12, height=8)
plot_density_5 = plot_densities_together(picks=seq(129,256,8))
ggsave("plot_density_5.png", plot_density_5, width=12, height=8)

#### next time: have a lesson on plotting... make it look good


### expected value E[f(Y)|x]

cond_density = function(x,y){
  ### calculate the conditional density: p(y|x) using our beta regression
  df_beta_params
  
  temp = df_beta_params %>% filter(draft_overall==x)
  dbeta(y, temp$shape1, temp$shape2)
}

### sanity check
cond_density(x=5,y=0.3)
cond_density(x=5,y=0.1)
cond_density(x=2,y=0.3)

### various f functions
f_identity = function(y){
  y
}
f_nonlinear = function(y,q_great,q_bad, a){
  is_great = ifelse(y>=q_great,1,0)
  is_decent = ifelse(y<q_great & y>=q_bad,1,0)
  return(a*is_great+(1-a)*is_decent)
}
### sanity check
f_nonlinear(y=0.3, a = 0.1, q_great = 0.2, q_bad = 0.1)
f_nonlinear(y=0.1, a = 0.5, q_great = 0.1, q_bad = 0.05)
f_nonlinear(y=0.29, a = 0.01, q_great = 0.3, q_bad = 0.02)
### In line 307, we call the function f_nonlinear
### With arguments y = 0.3, a = 0.1, q_great = 0.2, and q_bad = 0.1

### sanity check
f_identity(y = 0.1)
f_identity(y = 0.2)
f_identity(y = 0.5)

### We’ve now coded P(y|x) and f(y)
### let’s now code v(x)
v_raw = function(x,f){
  Integrand = function(y) {
    f(y) * cond_density(x,y)
  }
  integral = integrate(Integrand,lower=0, upper=1)
  return(integral$value)
}
### sanity check 
v_raw(x=5,f=f_identity)




### compute v(x) for each draft pick x
dataframe_v = tibble(x = 1:256) %>%
  rowwise() %>%
  mutate(
    v_raw_identity = v_raw(x,f=f_identity),
    v_raw_nonlinear_15 = v_raw(x, f = function(y){
      f_nonlinear(y, a = 1, q_great=0.15, q_bad=0)
    }),
    v_raw_nonlinear_14 = v_raw(x, f = function(y){
      f_nonlinear(y, a = 1, q_great=0.14, q_bad=0)
    }),
    v_raw_nonlinear_13 = v_raw(x, f = function(y){
      f_nonlinear(y, a = 1, q_great=0.13, q_bad=0)
    }),
    v_raw_nonlinear_12 = v_raw(x, f = function(y){
      f_nonlinear(y, a = 1, q_great=0.12, q_bad=0)
    }),
    v_raw_nonlinear_11 = v_raw(x, f = function(y){
      f_nonlinear(y, a = 1, q_great=0.11, q_bad=0)
    }),
    v_raw_nonlinear_10 = v_raw(x, f = function(y){
      f_nonlinear(y, a = 1, q_great=0.10, q_bad=0)
    }),
    v_raw_nonlinear_9 = v_raw(x, f = function(y){
      f_nonlinear(y, a = 1, q_great=0.09, q_bad=0)
    }),
    v_raw_nonlinear_8 = v_raw(x, f = function(y){
      f_nonlinear(y, a = 1, q_great=0.08, q_bad=0)
    }),
    v_raw_nonlinear_7 = v_raw(x, f = function(y){
      f_nonlinear(y, a = 1, q_great=0.07, q_bad=0)
    }),
    v_raw_nonlinear_6 = v_raw(x, f = function(y){
      f_nonlinear(y, a = 1, q_great=0.06, q_bad=0)
    }),
    v_raw_nonlinear_5 = v_raw(x, f = function(y){
      f_nonlinear(y, a = 1, q_great=0.05, q_bad=0)
    }),
    v_raw_nonlinear_4 = v_raw(x, f = function(y){
      f_nonlinear(y, a = 1, q_great=0.04, q_bad=0)
    }),
    v_raw_nonlinear_3 = v_raw(x, f = function(y){
      f_nonlinear(y, a = 1, q_great=0.03, q_bad=0)
    }),
    # v_raw_nonlinear_2 = v_raw(x, f = function(y){
    #   f_nonlinear(y, a = 1, q_great=0.02, q_bad=0)
    # }),
    # v_raw_nonlinear_1 = v_raw(x, f = function(y){
    #   f_nonlinear(y, a = 1, q_great=0.01, q_bad=0)
    # })
    #  ### v_raw_nonlinear=v_raw(x,f=function(y){
    # ####f_nonlinear(y,a=9/10, q_great=0.15,q_bad=0.04)
    #  ### f_nonlinear(y,a=1, q_great=0.15,q_bad=0)
    #     
    #   })
    ### f_nonlinear(y=0.3, a = 0.1, q_great = 0.2, q_bad = 0.1)
  ) 

dataframe_v1 = dataframe_v %>%
  ungroup() %>%
  mutate(
    v_identity = v_raw_identity / v_raw_identity[1],
    v_nonlinear_15 = v_raw_nonlinear_15 / v_raw_nonlinear_15[1],
    v_nonlinear_14 = v_raw_nonlinear_14 / v_raw_nonlinear_14[1],
    v_nonlinear_13 = v_raw_nonlinear_13 / v_raw_nonlinear_13[1],
    v_nonlinear_12 = v_raw_nonlinear_12 / v_raw_nonlinear_12[1],
    v_nonlinear_11 = v_raw_nonlinear_11 / v_raw_nonlinear_11[1],
    v_nonlinear_10 = v_raw_nonlinear_10 / v_raw_nonlinear_10[1],
    v_nonlinear_9 = v_raw_nonlinear_9 / v_raw_nonlinear_9[1],
    v_nonlinear_8 = v_raw_nonlinear_8 / v_raw_nonlinear_8[1],
    v_nonlinear_7 = v_raw_nonlinear_7 / v_raw_nonlinear_7[1],
    v_nonlinear_6 = v_raw_nonlinear_6 / v_raw_nonlinear_6[1],
    v_nonlinear_5 = v_raw_nonlinear_5 / v_raw_nonlinear_5[1],
    v_nonlinear_4 = v_raw_nonlinear_4 / v_raw_nonlinear_4[1],
    v_nonlinear_3 = v_raw_nonlinear_3 / v_raw_nonlinear_3[1],
    #  v_nonlinear_2 = v_raw_nonlinear_2 / v_raw_nonlinear_2[1],
    # v_nonlinear_1 = v_raw_nonlinear_1 / v_raw_nonlinear_1[1]
  )
dataframe_v1

### make a legend for the plot 
dataframe_v2= dataframe_v1 %>% 
  select(!matches("raw")) %>%
  pivot_longer(-x) %>%
  mutate(
    digit = str_remove(name,"v_nonlinear_"),
    #digit1 = paste0("0.",digit),
    digit1= ifelse(
      #   as.numeric(digit) < 9,
      nchar(digit) == 1,
      paste0("0.0",digit),
      paste0("0.",digit)
    ),
    Legend_Name = ifelse(
      str_detect(name,"identity"),
      "v(y)=y",
      paste0("v(y) = 1{y>",digit1,"}")
    )
  )
dataframe_v2 %>%
  ggplot(aes(x=x,y=value,color=Legend_Name)) +
  geom_line(linewidth=1) +
  xlab("Draft pick number") +
  scale_x_continuous(breaks=seq(1,256,32)) +
  scale_color_discrete(name="Value Function") +
  ylab("Value relative to first pick")