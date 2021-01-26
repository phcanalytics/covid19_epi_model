# Presupposes that analysis_df is loaded in the environment after running scripts 01_ through 05_ 

# libraries 
library(tidyverse)
library(mgcv) # for gam 
library(dlnm)
library(wesanderson)
library(yaml)
library(cowplot)
library(ggplot2)

config <- read_yaml('./scripts/config.yaml')

# Helper function ---------------------------------------------------------

make_lag_slices <- function(crosspred){
  rr_fits <- data.frame(crosspred$matRRfit) %>%
    mutate(mobility = as.factor(rownames(.)))
  
  names(rr_fits) <- gsub("lag","",make.names(names(rr_fits)))
  # sorted mobility for plot labels
  sorted_mobility_retail <- paste(sort(as.integer(levels(rr_fits$mobility))))
  rr_fits$mobility <- factor(rr_fits$mobility, levels = sorted_mobility_retail)
  selowfits_retail <- data.frame(crosspred$matRRlow) %>% mutate(mobility=as.factor(rownames(.)))
  sehighfits_retail <- data.frame(crosspred$matRRhigh) %>% mutate(mobility=as.factor(rownames(.)))
  names(selowfits_retail) <- gsub("lag","",make.names(names(selowfits_retail)))
  names(sehighfits_retail) <- gsub("lag","",make.names(names(sehighfits_retail)))
  sorted_mobility_retail <- paste(sort(as.integer(levels(rr_fits$mobility))))
  selowfits_retail$mobility <- factor(selowfits_retail$mobility, levels = sorted_mobility_retail)
  sehighfits_retail$mobility <- factor(sehighfits_retail$mobility, levels = sorted_mobility_retail)
  
  # make lagged dataframe for plots
  lagged_estimates_df <- rr_fits %>%
    tidyr::gather(lag, RR, -mobility) %>%
    mutate(type="RR") %>%
    left_join(selowfits_retail %>% 
                tidyr::gather(lag, se_low, -mobility), by = c('mobility', 'lag')) %>%
    left_join(sehighfits_retail %>% 
                tidyr::gather(lag, se_high, -mobility), by = c('mobility', 'lag')) %>%
    mutate(lag=as.numeric(lag))
  
  return(lagged_estimates_df)
}

make_cumulative_slices <- function(crosspred){
  rr_fits <- data.frame(crosspred$cumRRfit) %>%
    mutate(mobility = as.factor(rownames(.)))
  
  names(rr_fits) <- gsub("lag","",make.names(names(rr_fits)))
  # sorted mobility for plot labels
  sorted_mobility_retail <- paste(sort(as.integer(levels(rr_fits$mobility))))
  rr_fits$mobility <- factor(rr_fits$mobility, levels = sorted_mobility_retail)
  selowfits_retail <- data.frame(crosspred$cumRRlow) %>% mutate(mobility=as.factor(rownames(.)))
  sehighfits_retail <- data.frame(crosspred$cumRRhigh) %>% mutate(mobility=as.factor(rownames(.)))
  names(selowfits_retail) <- gsub("lag","",make.names(names(selowfits_retail)))
  names(sehighfits_retail) <- gsub("lag","",make.names(names(sehighfits_retail)))
  sorted_mobility_retail <- paste(sort(as.integer(levels(rr_fits$mobility))))
  selowfits_retail$mobility <- factor(selowfits_retail$mobility, levels = sorted_mobility_retail)
  sehighfits_retail$mobility <- factor(sehighfits_retail$mobility, levels = sorted_mobility_retail)
  
  # make lagged dataframe for plots
  cumulative_estimates_df <- rr_fits %>%
    tidyr::gather(lag, RR, -mobility) %>%
    mutate(type="RR") %>%
    left_join(selowfits_retail %>% 
                tidyr::gather(lag, se_low, -mobility), by = c('mobility', 'lag')) %>%
    left_join(sehighfits_retail %>% 
                tidyr::gather(lag, se_high, -mobility), by = c('mobility', 'lag')) %>%
    mutate(lag=as.numeric(lag))
  
  return(cumulative_estimates_df)
}


# Analysis ----------------------------------------------------------------

# Clean up
analysis_df_sub <- analysis_df %>%
  filter(metro_area!="Los Angeles") %>%
  filter(date!="2020-09-21") %>% # Harris County data dump
  filter(date!="2020-11-13") %>% # abnormal data dump
  filter(date!="2020-11-26") %>%
  filter(date!="2020-12-21") %>% # abnormal
  filter(date!="2020-12-25") %>%
  mutate(metro_state_county=factor(metro_state_county)) 

# Plot retail mobility
p_mob <- analysis_df_sub %>% 
  left_join(data.frame(state=state.name,region=state.region), by="state") %>%
  ggplot(aes(date, retail_and_recreation_percent_change_from_baseline)) +
  geom_line(aes(col=region,group=metro_state_county), alpha=0.2) +
  ggtitle("Retail and recreational mobility") +
  xlab("Date") +
  ylab("Percent change from pre-pandemic") +
  theme_classic() +
  geom_hline(yintercept = -30, linetype="longdash") +
  geom_hline(yintercept = -10, linetype="longdash")
  
ggsave("results/mobility_Jan26.png",p_mob, device="png", width=10, height=6, units="in")  

# Create retail basis
retail_basis <- crossbasis(
  analysis_df_sub$retail_and_recreation_percent_change_from_baseline, 
  lag = config$max_lag,
  argvar = list(fun='cr', knots=c(quantile(analysis_df_sub$retail_and_recreation_percent_change_from_baseline, probs=seq(0,1,0.25)))), # df=4), #  # using penalized spline 'cr' ADD SPECIFIC KNOT VALUES
  arglag = list(fun='cr', df=4),
  group = analysis_df_sub$metro_state_county # makes sure I lag appropriately by metro/county
)

# penalize the crossbasis splines
retail_basisPen <- cbPen(retail_basis) 

# Run retail-case model
retail_ptm <- proc.time()
mod_cases_retail <- gam(
  daily_cases ~
    retail_basis + 
    s(time_since_first_case) + 
    s(time_since_first_case, metro_state_county, bs=c("fs")) + # global smoother
    s(PC1, bs="cr", k=5) +
    s(PC2, bs="cr", k=5) +
    s(PC3, bs="cr", k=5) +
    s(PC4, bs="cr", k=5) +
    offset(log(population_v051)), 
  data=analysis_df_sub,
  paraPen=list(retail_basis=retail_basisPen),# this applies the penalty to the lagged matrix
  family="quasipoisson",
  method="REML"
)
retail_time <- proc.time() - retail_ptm

# Extract effects
pred_retail_cases <- crosspred(
  retail_basis,
  mod_cases_retail,
  at=10:-50,
  by=1,
  bylag=1,
  ci.level = 0.95,
  cen= -20,
  cumul=TRUE
)

plot(pred_retail_cases)

df_cases_retail <- make_lag_slices(pred_retail_cases)
df_cumulative_cases_retail <- make_cumulative_slices(pred_retail_cases)

df_cumulative_cases_retail %>% filter(lag%in%seq(0,60,by=10)) %>% filter(mobility=="-30")
df_cumulative_cases_retail %>% filter(lag==33) %>% filter(mobility=="-30")
df_cumulative_cases_retail %>% filter(lag%in%seq(0,60,by=10)) %>% filter(mobility=="-10")

ggplot(
  # subset to certain mobilities
  data = rbind(df_cumulative_cases_retail %>% mutate(type="retail")) %>% 
    filter(
        mobility %in% c("-30", "-20", "-15", "-10" , "-5")
      ) %>%
    mutate(
      # order mobility levels
      mobility = factor(
        mobility, levels= c("-30", "-20", "-15", "-10" , "-5")
      )
    ),
  aes(x = lag, y = RR)
) +
  geom_line(
    aes(col=mobility)
  ) + 
  geom_ribbon(
    aes(ymin=se_low,ymax=se_high, fill=mobility),alpha=0.1
  )+
  scale_color_manual(
    values=wesanderson::wes_palette("Darjeeling1",n=12,type="continuous")
  ) +
  scale_fill_manual(
    values=wesanderson::wes_palette("Darjeeling1",n=12,type="continuous"),
    guide=FALSE
  ) +
  geom_hline(yintercept=1, color='black', linetype='dotted', alpha=0.5) +
  xlab('Days After') +
  ylab('Relative Daily Case Rate') +
  facet_wrap(
    ~ type, scales="free"
  ) +
  theme_classic() +
  #scale_x_continuous(expand=c(0,0), limits=c(0,60)) +
  #scale_y_continuous(expand=c(0,0), limits=c(0.6,1.25)) +
  theme(strip.background = element_blank()) +
  theme(legend.position = "bottom") +
  ggtitle("Cases")


# Repeat on parks ----------------------------------------------

# Create parks basis
parks_basis <- crossbasis(
  analysis_df_sub$parks_percent_change_from_baseline, 
  lag = config$max_lag,
  argvar = list(fun='cr', knots=c(quantile(analysis_df_sub$parks_percent_change_from_baseline, probs=seq(0,1,0.25)))), # df=4), #  # using penalized spline 'cr' ADD SPECIFIC KNOT VALUES
  arglag = list(fun='cr', df=4),
  group = analysis_df_sub$metro_state_county # makes sure I lag appropriately by metro/county
)

# penalize the crossbasis splines
parks_basisPen <- cbPen(parks_basis) 

# Run retail-case model

mod_cases_park <- gam(
  daily_cases ~
    parks_basis + 
    s(time_since_first_case) + 
    s(time_since_first_case, metro_state_county, bs=c("fs")) + # global smoother
    s(PC1, bs="cr", k=5) +
    s(PC2, bs="cr", k=5) +
    s(PC3, bs="cr", k=5) +
    s(PC4, bs="cr", k=5) +
    offset(log(population_v051)), 
  data=analysis_df_sub,
  paraPen=list(parks_basis=parks_basisPen),# this applies the penalty to the lagged matrix
  family="quasipoisson",
  method="REML"
)

pred_parks_cases <- crosspred(
  parks_basis,
  mod_cases_park,
  at=300:-50,
  by=1,
  bylag=1,
  ci.level = 0.95,
  cen= 0,
  cumul=TRUE
)

df_cumulative_cases_parks <- make_cumulative_slices(pred_parks_cases)

df_cumulative_cases_parks %>% filter(lag%in%seq(0,60,by=10)) %>% filter(mobility=="-20")
df_cumulative_cases_parks %>% filter(lag%in%seq(0,60,by=10)) %>% filter(mobility=="20")

rbind(df_cumulative_cases_retail %>% mutate(type="retail"),
      df_cumulative_cases_parks %>% mutate(type="parks")) %>%
  filter(
    case_when(
      type=="parks" ~ mobility %in% c("-20", "-10", "0", "5", "10", "30" ,"50"),
      type=="retail" ~ mobility %in% c("-30", "-20", "-15", "-10")
    )
  ) %>%
  ggplot(aes(lag, RR)) + geom_line(aes(col=mobility)) +
  geom_ribbon(aes(ymin=se_low,ymax=se_high, fill=mobility),alpha=0.1) +
  facet_wrap(~type, scales="free") +
  geom_hline(yintercept = 1, linetype="dashed") +
  theme_classic()


p_retail_cum <- df_cumulative_cases_retail %>% 
  filter(mobility %in% c("-30", "-20", "-15", "-10")) %>%
  mutate(mobility=recode(mobility, "-30"="-10%", "-20"="Average", "-15"="+5%", "-10"="+10%")) %>%
  ggplot(aes(lag, RR)) + geom_line(aes(col=mobility)) +
  geom_ribbon(aes(ymin=se_low,ymax=se_high, fill=mobility),alpha=0.1) +
  geom_hline(yintercept = 1, linetype="dashed") +
  theme_classic() +
  ggtitle("Cumulative effect of mobility on daily case rate") +
  xlab("Days since mobility reduction") +
  ylab("Relative daily case rate (RR)") 

p_retail_cum

ggsave("results/p_retail_cum.png", p_retail_cum, device="png", width=8, height=6, units="in")
