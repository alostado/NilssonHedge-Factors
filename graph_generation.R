library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(gt)
theme_set(theme_minimal())

load('factor_loadings.RData')

factor_loadings_type = factor_loadings %>%
  filter(Type != 'AssetAllocation', Type != 'LongOnly', 
         Type != 'Crypto', Type != 'Hedge Fund', 
         Type != 'NilssonHedge Index', !is.na(Type),
         !is.nan(r.squared)) %>%
  group_by(Date, Manager, Program, Type) %>%
  summarize(r.squared = r.squared[1],
            alpha = estimate[term == '(Intercept)'],
            MKT = estimate[term == 'MKT'],
            HML = estimate[term == 'HML'],
            SMB = estimate[term == 'SMB'],
            RMW = estimate[term == 'RMW'],
            CMA = estimate[term == 'CMA'],
            MOM = estimate[term == 'MOM']) %>%
  ungroup() %>%
  pivot_longer(c(r.squared, alpha, MKT, HML, SMB, RMW, CMA, MOM),
               names_to = 'term', values_to = 'estimate') %>%
  group_by(Date, term, Type) %>%
  summarize(mean = mean(estimate),
            median = median(estimate),
            lo50 = quantile(estimate, 0.25),
            hi50 = quantile(estimate, 0.75),
            lo70 = quantile(estimate, 0.15),
            hi70 = quantile(estimate, 0.85),
            lo90 = quantile(estimate, 0.05),
            hi90 = quantile(estimate, 0.95),
            pos_load = mean(estimate > 0),
            n = n(),
            model = 'FF5+MOM')

clrs_types <- brewer.pal(6, 'Dark2')
names(clrs_types) <- c('CTA', 'Equity LS', 'Event Driven', 
                       'Fixed Income','Market Neutral', 'RiskPremia')

lastdate_ff5 <- max(factor_loadings_type$Date)

png('graphs/rolling_factor_loadings_alphas_10year.png', 
    width = 8000, height = 4000, res=720)
print(factor_loadings_type %>%
        filter(term == 'alpha', 
               Date >= lastdate_ff5 %m-% months(120), n >= 10) %>%
        ggplot(aes(x=Date)) +
        geom_line(aes(y=0), linetype = 'dashed') +
        geom_line(aes(y=median, color=Type)) +
        geom_ribbon(aes(ymin=lo50, ymax=hi50, fill=Type), alpha = 0.2) +
        geom_ribbon(aes(ymin=lo70, ymax=hi70, fill=Type), alpha = 0.2) +
        geom_ribbon(aes(ymin=lo90, ymax=hi90, fill=Type), alpha = 0.2) +
        facet_wrap(~ Type, nrow = 4, ncol = 2, scales = 'free_y', strip.position = 'bottom') +
        scale_color_manual(values = clrs_types) +
        scale_fill_manual(values = clrs_types) +
        scale_y_continuous(labels = scales::label_percent(accuracy = 0.1)) +
        labs(y='Loading') +
        theme(strip.text.y = element_text(angle=0),
              legend.position = 'none',
              axis.title.x = element_blank()))
dev.off()


png('graphs/rolling_factor_loadings_alphas_36month.png', 
    width = 8000, height = 4000, res=720)
print(factor_loadings_type %>%
        filter(term == 'alpha', 
               Date >= lastdate_ff5 %m-% months(36),  n >= 10) %>%
        ggplot(aes(x=Date)) +
        geom_line(aes(y=0), linetype = 'dashed') +
        geom_line(aes(y=median, color=Type)) +
        geom_ribbon(aes(ymin=lo50, ymax=hi50, fill=Type), alpha = 0.2) +
        geom_ribbon(aes(ymin=lo70, ymax=hi70, fill=Type), alpha = 0.2) +
        geom_ribbon(aes(ymin=lo90, ymax=hi90, fill=Type), alpha = 0.2) +
        facet_wrap(~ Type, nrow = 4, ncol = 2, scales = 'free_y', strip.position = 'bottom') +
        scale_color_manual(values = clrs_types) +
        scale_fill_manual(values = clrs_types) +
        scale_y_continuous(labels = scales::label_percent(accuracy = 0.1)) +
        labs(y='Loading') +
        theme(strip.text.y = element_text(angle=0),
              legend.position = 'none',
              axis.title.x = element_blank()))
dev.off()

png('graphs/rolling_factor_loadings_alpha_positive_10year.png', 
    width = 8000, height = 4000, res=720)
print(factor_loadings_type %>%
        filter(term == 'alpha', 
               Date >= lastdate_ff5 %m-% months(120), n >= 10) %>%
        ggplot(aes(x=Date)) +
        geom_area(aes(y=pos_load, fill=Type), stat = 'identity') +
        geom_line(aes(y=0.5), linetype = 'dashed') +
        facet_wrap(~ Type, nrow = 4, ncol = 2, strip.position = 'bottom') +
        scale_fill_manual(values = clrs_types) +
        scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
        labs(y='% of Programs with Positive Alpha') +
        theme(strip.text.y = element_text(angle=0),
              legend.position = 'none',
              axis.title.x = element_blank()))
dev.off()


png('graphs/rolling_factor_loadings_alpha_positive_36month.png', 
    width = 8000, height = 4000, res=720)
print(factor_loadings_type %>%
        filter(term == 'alpha', 
               Date >= lastdate_ff5 %m-% months(36), n >= 10) %>%
        ggplot(aes(x=Date)) +
        geom_area(aes(y=pos_load, fill=Type), stat = 'identity') +
        geom_line(aes(y=0.5), linetype = 'dashed') +
        facet_wrap(~ Type, nrow = 4, ncol = 2, strip.position = 'bottom') +
        scale_fill_manual(values = clrs_types) +
        scale_y_continuous(labels = scales::label_percent(accuracy = 1),
                           limits = c(0,1)) +
        labs(y='% of Programs with Positive Alpha') +
        theme(strip.text.y = element_text(angle=0),
              legend.position = 'none',
              axis.title.x = element_blank()))
dev.off()


png('graphs/rolling_factor_loadings_rsquared_10y.png', 
    width = 8000, height = 4000, res=720)
print(factor_loadings_type %>%
        filter(term == 'r.squared', 
               Date >= lastdate_ff5 %m-% months(120), n >= 10) %>%
        ggplot(aes(x=Date)) +
        geom_line(aes(y=median, color=Type)) +
        geom_ribbon(aes(ymin=lo50, ymax=hi50, fill=Type), alpha = 0.2) +
        geom_ribbon(aes(ymin=lo70, ymax=hi70, fill=Type), alpha = 0.2) +
        geom_ribbon(aes(ymin=lo90, ymax=hi90, fill=Type), alpha = 0.2) +
        facet_wrap(~ Type, nrow = 4, ncol = 2, scales = 'free_y', strip.position = 'bottom') +
        scale_color_manual(values = clrs_types) +
        scale_fill_manual(values = clrs_types) +
        scale_y_continuous(limits = c(0,1)) +
        labs(y='R-squared') +
        theme(strip.text.y = element_text(angle=0),
              legend.position = 'none',
              axis.title.x = element_blank()))
dev.off()

names(clrs_types) <- c('MKT', 'SMB', 'HML', 'RMW', 'CMA', 'MOM')


png('graphs/factors_cumulative_return_10y.png', 
    width = 8000, height = 4000, res=720)
print(factor_loadings %>% ungroup() %>%
        select(c(Date, MKT, SMB, HML, RMW, CMA, MOM)) %>%
        pivot_longer(-Date, names_to = 'Factor', values_to = 'Return') %>%
        group_by(Date, Factor) %>%
        summarize(Return = Return[1]) %>%
        filter(Date >= lastdate_ff5 %m-% months(120)) %>%
        group_by(Factor) %>%
        mutate(CumRet = cumprod(1+Return) - 1) %>%
        ggplot(aes(x=Date)) +
        geom_line(aes(y=CumRet, color=Factor)) +
        facet_wrap(~ Factor, nrow = 3, ncol = 2, scales = 'free_y', strip.position = 'bottom') +
        scale_color_manual(values = clrs_types) +
        scale_fill_manual(values = clrs_types) +
        scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
        labs(y='Factor Cumulative Return') +
        theme(strip.text.y = element_text(angle=0),
              legend.position = 'none',
              axis.title.x = element_blank()))
dev.off()

# Make tables

factor_loadings$term[factor_loadings$term == '(Intercept)'] = 'alpha'


factor_loadings %>% ungroup() %>% 
  filter(Date == max(Date), term == 'alpha', Type != 'AssetAllocation', 
         Type != 'LongOnly', Type != 'Crypto', Type == 'CTA') %>% 
  select(Manager, Program, estimate) %>%
  slice_max(estimate, n=5) %>% gt() %>%
  fmt_percent(columns = vars(estimate), decimals = 1) %>%
  cols_width(vars(estimate) ~ px(70),
             vars(Manager, Program) ~ px(240)) %>%
  cols_align(align = 'right', columns = vars(estimate)) %>%
  cols_label(estimate = 'Alpha') %>%
  tab_options(table.font.names = 'Source Code Pro') %>%
  gtsave(filename = 'factor_loadings_CTA_top5_alpha.png', path = 'graphs/tables')


factor_loadings %>% ungroup() %>% 
  filter(Date == max(Date), term == 'alpha', Type != 'AssetAllocation', 
         Type != 'LongOnly', Type != 'Crypto', Type == 'CTA') %>% 
  select(Manager, Program, estimate) %>%
  slice_min(estimate, n=5) %>% gt() %>%
  fmt_percent(columns = vars(estimate), decimals = 1) %>%
  cols_width(vars(estimate) ~ px(70),
             vars(Manager, Program) ~ px(240)) %>%
  cols_align(align = 'right', columns = vars(estimate)) %>%
  cols_label(estimate = 'Alpha') %>%
  tab_options(table.font.names = 'Source Code Pro') %>%
  gtsave(filename = 'factor_loadings_CTA_bot5_alpha.png', path = 'graphs/tables')



factor_loadings %>% ungroup() %>% 
  filter(Date == max(Date), term == 'alpha', Type != 'AssetAllocation', 
         Type != 'LongOnly', Type != 'Crypto', Type == 'Equity LS') %>% 
  select(Manager, Program, estimate) %>%
  slice_max(estimate, n=3) %>% gt() %>%
  fmt_percent(columns = vars(estimate), decimals = 1) %>%
  cols_width(vars(estimate) ~ px(70),
             vars(Manager, Program) ~ px(240)) %>%
  cols_align(align = 'right', columns = vars(estimate)) %>%
  cols_label(estimate = 'Alpha') %>%
  tab_options(table.font.names = 'Source Code Pro') %>%
  gtsave(filename = 'factor_loadings_EquityLS_top5_alpha.png', path = 'graphs/tables')


factor_loadings %>% ungroup() %>% 
  filter(Date == max(Date), term == 'alpha', Type != 'AssetAllocation', 
         Type != 'LongOnly', Type != 'Crypto', Type == 'Equity LS') %>% 
  select(Manager, Program, estimate) %>%
  slice_min(estimate, n=3) %>% gt() %>%
  fmt_percent(columns = vars(estimate), decimals = 1) %>%
  cols_width(vars(estimate) ~ px(70),
             vars(Manager, Program) ~ px(240)) %>%
  cols_align(align = 'right', columns = vars(estimate)) %>%
  cols_label(estimate = 'Alpha') %>%
  tab_options(table.font.names = 'Source Code Pro') %>%
  gtsave(filename = 'factor_loadings_EquityLS_bot5_alpha.png', path = 'graphs/tables')


factor_loadings %>% ungroup() %>% 
  filter(Date == max(Date), term == 'alpha', Type != 'AssetAllocation', 
         Type != 'LongOnly', Type != 'Crypto', Type == 'Fixed Income') %>% 
  select(Manager, Program, estimate) %>%
  slice_max(estimate, n=3) %>% gt() %>%
  fmt_percent(columns = vars(estimate), decimals = 1) %>%
  cols_width(vars(estimate) ~ px(70),
             vars(Manager, Program) ~ px(240)) %>%
  cols_align(align = 'right', columns = vars(estimate)) %>%
  cols_label(estimate = 'Alpha') %>%
  tab_options(table.font.names = 'Source Code Pro') %>%
  gtsave(filename = 'factor_loadings_FixedIncome_top5_alpha.png', path = 'graphs/tables')


factor_loadings %>% ungroup() %>% 
  filter(Date == max(Date), term == 'alpha', Type != 'AssetAllocation', 
         Type != 'LongOnly', Type != 'Crypto', Type == 'Fixed Income') %>% 
  select(Manager, Program, estimate) %>%
  slice_min(estimate, n=3) %>% gt() %>%
  fmt_percent(columns = vars(estimate), decimals = 1) %>%
  cols_width(vars(estimate) ~ px(70),
             vars(Manager, Program) ~ px(240)) %>%
  cols_align(align = 'right', columns = vars(estimate)) %>%
  cols_label(estimate = 'Alpha') %>%
  tab_options(table.font.names = 'Source Code Pro') %>%
  gtsave(filename = 'factor_loadings_FixedIncome_bot5_alpha.png',
         path = 'graphs/tables')



factor_loadings %>% ungroup() %>% 
  filter(Date == max(Date), term == 'alpha', 
         Type %in% c('Event Driven', 'Hedge Fund', 
                     'Market Neutral', 'RiskPremia')) %>% 
  select(Type, Manager, Program, estimate) %>% 
  group_by(Type) %>%
  slice_max(estimate, n=1) %>% 
  ungroup() %>%
  gt() %>%
  fmt_percent(columns = vars(estimate), decimals = 1) %>%
  cols_width(vars(estimate) ~ px(70),
             vars(Manager, Program) ~ px(240)) %>%
  cols_align(align = 'right', columns = vars(estimate)) %>%
  cols_label(estimate = 'Alpha') %>%
  tab_options(table.font.names = 'Source Code Pro') %>%
  gtsave(filename = 'factor_loadings_rest_top5_alpha.png', path = 'graphs/tables')



factor_loadings %>% ungroup() %>% 
  filter(Date == max(Date), term == 'alpha', 
         Type %in% c('Event Driven', 'Hedge Fund', 
                     'Market Neutral', 'RiskPremia')) %>% 
  select(Type, Manager, Program, estimate) %>% 
  group_by(Type) %>%
  slice_min(estimate, n=1) %>% 
  ungroup() %>%
  gt() %>%
  fmt_percent(columns = vars(estimate), decimals = 1) %>%
  cols_width(vars(estimate) ~ px(70),
             vars(Manager, Program) ~ px(240)) %>%
  cols_align(align = 'right', columns = vars(estimate)) %>%
  cols_label(estimate = 'Alpha') %>%
  tab_options(table.font.names = 'Source Code Pro') %>%
  gtsave(filename = 'factor_loadings_rest_bot5_alpha.png', path = 'graphs/tables')


