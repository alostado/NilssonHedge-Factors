library(tibbletime)
library(tidyverse)
library(broom)
library(lubridate)
library(RColorBrewer)


load('CTAdb.RData')

window_length = 24

roll_ff5_mom <- rollify(
  .f = function(y,MKT,SMB,HML,RMW,CMA,MOM){
    lm(y ~ MKT+SMB+HML+RMW+CMA+MOM)},
  window = window_length, # 12 months = 1 year
  unlist = F)

factor_loadings = df %>% 
  filter(between(Date, ff5_mom_mindate, ff5_mom_maxdate)) %>%
  group_by(ID, Program, Manager, Type, Style, Strategy, Sector) %>%
  filter(sum(!is.na(Return)) >= window_length, sum(is.na(Return)) == 0) %>%
  mutate(
    model = 'FF5+MOM',
    fit = roll_ff5_mom(Return-RF, MKT, SMB, HML, RMW, CMA, MOM) # currently FF5+MOM
  ) %>% 
  filter(!is.na(fit)) %>%
  mutate(tidied = map(fit, tidy),
         glanced = map(fit, glance)) %>%
  select(-fit) %>% unnest(tidied) %>%
  select(-c(std.error, statistic, p.value)) %>%
  unnest(glanced) %>% 
  select(-c(adj.r.squared, sigma, statistic, p.value, df, 
            logLik, AIC, BIC, deviance, df.residual, nobs))

rm(list=setdiff(ls(),'factor_loadings'))

save.image('factor_loadings.RData')
