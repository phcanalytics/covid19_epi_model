# ------------------------------------------------------------------------------
# Title: Bootstrapped uncertainty estimates for main GAM
# Author: Sarah McGough
# Purpose: Variance estimates from GAM objects with time series data can be
# artificially low, leading to tighter uncertainty estimates. Here we bootstrap
# estimates of uncertainty of the main GAM. This is run in parallel on 20 cores 
# and takes about 20 minutes. If run locally, this could take a multiple hours.
# ------------------------------------------------------------------------------

# Parametric GAM bootstrap- not residuals

# libraries 
library(tidyverse)
library(mgcv) # for gam 
library(dlnm)
library(foreach)
library(doParallel)
library(parallel)

# Read yaml config file --------------------------------------------------------
config <- read_yaml('./scripts/config.yaml')

# Read analysis dataframe
analysis_df <- read_csv('./data/05-analysis_df.csv') %>% 
  # starting at config$max_lag (60) days prior to time since first death in order to get lagged
  # values config$max_lag (60) days prior 
  filter(time_since_first_death >= (-(config$max_lag))) %>% 
  # code metro_state_county as factor
  mutate(metro_state_county = as.factor(metro_state_county))


# build crossbasis for gam model for 60 day lagged mobility of retail and rec
# called cbgam 
retail_basis <- crossbasis(
  analysis_df$retail_and_recreation_percent_change_from_baseline, 
  lag = config$max_lag,
  argvar = list(fun='cr', df=4), # using penalized spline 'cr'
  arglag = list(fun='cr', df=4),
  group = analysis_df$metro_state_county # makes sure I lag appropriately by metro/county
)


# penalize the crossbasis splines
retail_basisPen <- cbPen(retail_basis) 

# ADD NEW MOBILITY CATEGORIES: build cross-basis for each

workplace_basis <- crossbasis(
  analysis_df$workplaces_percent_change_from_baseline, 
  lag = config$max_lag,
  argvar = list(fun='cr', df=4), # using penalized spline 'cr'
  arglag = list(fun='cr', df=4),
  group = analysis_df$metro_state_county # makes sure I lag appropriately by metro/county
)


# penalize the crossbasis splines
workplacePen <- cbPen(workplace_basis) 

# Another category
parks_basis <- crossbasis(
  analysis_df$parks_percent_change_from_baseline, 
  lag = config$max_lag,
  argvar = list(fun='cr', df=4), # using penalized spline 'cr'
  arglag = list(fun='cr', df=4),
  group = analysis_df$metro_state_county # makes sure I lag appropriately by metro/county
)

# penalize the crossbasis splines
parksPen <- cbPen(parks_basis) 

# 1. Fit GAM --------------------------------------------------------------

dlnm.main.gam_multimob3 <- gam(
  daily_deaths ~
    retail_basis + # this is the lag matrix 
    #workplace_basis +
    parks_basis + 
    s(time_since_first_death) + 
    s(time_since_first_death, metro_state_county, bs=c("fs")) + # global smoother
    s(PC1, bs="cr", k=5) +
    s(PC2, bs="cr", k=5) +
    s(PC3, bs="cr", k=5) +
    s(PC4, bs="cr", k=5) +
    offset(log(population_v051)), 
  data=analysis_df,
  paraPen=list(retail_basis=retail_basisPen,
               # workplace_basis=workplacePen,
               parks_basis=parksPen),# this applies the penalty to the lagged matrix
  family="quasipoisson",
  method="REML"
)

summary(dlnm.main.gam_multimob3)

## We can use the knots of the cubic splines identifed above to apply the
## same spline transformation to new data so we don't run in to the issue
## of trying to identify new knots from a vector of a single value (e.g. -50)

# take out existing knots from existing crossbasis
ret_var_knots <- attributes(retail_basis)$argvar$knots
ret_lag_knots <- attributes(retail_basis)$arglag$knots

# repeat analysis df and make counterfactual
pred_df <- analysis_df

pred_df$retail_and_recreation_percent_change_from_baseline <- -50

# make countnerfactual basis where all mobility is set to -50 using known knots
counter_fact_basis <- crossbasis(
  pred_df$retail_and_recreation_percent_change_from_baseline,
  lag = config$max_lag,
  argvar = list(fun='cr', knots=ret_var_knots), # instead of dfs; use the knots argument
  arglag = list(fun='cr', knots=ret_lag_knots), # instead of dfs; use the knots argument
  group = pred_df$metro_state_county # makes sure I lag appropriately by metro/county
)


# new data for prediction
dim(counter_fact_basis[complete.cases(counter_fact_basis), ])

# Getting out the predicted daily deaths using the observed data
# pred using observed data
pred_deaths <- predict(
  dlnm.main.gam_multimob3, 
  type = 'response'
)

# pulling out the data the model fit; this was easier than only selecting certain
# columns from the analysis_df
new_mat <- dlnm.main.gam_multimob3$model

# replace observed retail mobility with counterfactual; mgcv treats the lagged
# matrcies we use as a single column/element, so we can use replace the 
# second column with the counterfactual matrix
new_mat[,2] <- counter_fact_basis[complete.cases(counter_fact_basis), ]

# make new predictions under counterfactual scenario
counterfact_50_deaths <- predict(
  dlnm.main.gam_multimob3, 
  newdata = new_mat, 
  type = 'response'
  )

## try plotting; need to start at timei since first death >= 0 to avoid missing
plot_df <- pred_df %>% filter(time_since_first_death >= 0)

plot_df$pred_deaths <- pred_deaths
plot_df$cf_50_deaths <- counterfact_50_deaths

# exampmle with NYC only
# subset to nyc just to try it out
nyc_1 <- analysis_df %>% 
  filter(
    metro_state_county == 'New York City, New York County, NY'
  )

nyc <- plot_df %>% 
  filter(
    metro_state_county == 'New York City, New York County, NY'
    )

nyc[, c('daily_deaths', 'cf_50_deaths')]

# mobility
ggplot() +
  geom_point(data = nyc_1, 
    aes(x=time_since_first_death, 
        y = retail_and_recreation_percent_change_from_baseline))

# observed deaths, predicted, and counterfactual if mobility was always -50
ggplot() +
  geom_point(data = nyc, aes(x=time_since_first_death, y = daily_deaths)) +
  geom_line(
    data = nyc, 
    aes(
      x=time_since_first_death, 
      y = cf_50_deaths, 
      color = 'Counterfactual -50')
    ) +
  geom_line(
    data = nyc, 
    aes(
      x=time_since_first_death, 
      y = pred_deaths, 
      color = 'Predicted')
  ) +
  scale_color_manual(
    values = c('Counterfactual -50' = 'blue',
               'Predicted' = 'red')
  ) +
  theme_classic()


# California
# using analysis_df since I set retail-rec mobility to -50 in the pred df
cali_1 <- analysis_df %>%
  filter(time_since_first_death >= 0) %>% 
  filter(state == 'California')

cali_2 <- plot_df %>% 
  filter(state == 'California')

# mobility
ggplot() +
  geom_point(
    data = cali_1,
    aes(
      x = time_since_first_death, 
      y = retail_and_recreation_percent_change_from_baseline,
      group = metro_state_county,
      color = metro_state_county)
    ) +
  facet_wrap(~metro_area) +
  theme_classic()
  
# observed over counterfactual deaths rate per million
ggplot() +
  # dots are observed
  geom_point(
    data = cali_2,
    aes(
      x = time_since_first_death,
      y = (daily_deaths/population_v051)*1000000,
      group = metro_state_county,
      color = metro_state_county),
    alpha = 0.2
  ) +
  # solid is if mobility were always -50
  geom_line(
    data = cali_2,
    aes(
      x = time_since_first_death,
      y = (cf_50_deaths/population_v051)*1000000,
      group = metro_state_county,
      color = metro_state_county),
    size = 1
  ) +
  # dashed is predicted without any changes to data
  geom_line(
    data = cali_2,
    aes(
      x = time_since_first_death,
      y = (pred_deaths/population_v051)*1000000,
      group = metro_state_county,
      color = metro_state_county),
    linetype = 'dashed'
  ) +
  facet_wrap(~metro_area) +
  theme_classic()



