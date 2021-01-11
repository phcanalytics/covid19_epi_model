# Mobility and covid-19 in the US: a story in ggplot

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
  mutate(metro_state_county = as.factor(metro_state_county)) %>%
  # filter to after September
  filter(date>="2020-09-01") 

# Excluding LA metro area- this is an outlier that merits its own analysis
# Excluding Harris County
# Excluding Thanksgiving/Christmas holidays

analysis_df_sub <- analysis_df %>%
  filter(metro_area!="Los Angeles") %>%
  filter(county!="Harris County") %>%
  filter(date!="2020-11-26") %>%
  filter(date!="2020-12-25") %>%
  mutate(metro_state_county=factor(metro_state_county)) 


# The south and north central regions have closed down the least
p_mob <- ggplot() +
  geom_line(data=analysis_df_sub %>%
              filter(metro_area!="Los Angeles") %>%
         left_join(data.frame(state=state.name,region=state.region), by="state") %>%
         group_by(region, date) %>%
         summarise(daily_cases=sum(daily_cases),
                   avg_retail=mean(retail_and_recreation_percent_change_from_baseline)), 
       aes(date, avg_retail, col=region), alpha=0.6) + 
  geom_line(
    data=analysis_df_sub %>%
      left_join(data.frame(state=state.name,region=state.region), by="state"),
    aes(date, retail_and_recreation_percent_change_from_baseline, col=region, group=metro_state_county),
    alpha=0.1
  ) +
  xlab("Date") +
  ylab("Percent change from baseline") +
  theme_classic() +
  ggtitle("Retail and recreational mobility")

p_cases <- 
  ggplot() +
  geom_line(data=analysis_df_sub %>%
              filter(metro_area!="Los Angeles") %>%
              left_join(data.frame(state=state.name,region=state.region), by="state") %>%
              group_by(region, date) %>%
              summarise(daily_cases=sum(daily_cases),
                        avg_retail=mean(retail_and_recreation_percent_change_from_baseline)), 
            aes(date, daily_cases, col=region), alpha=0.6) + 
  geom_line(
    data=analysis_df_sub %>%
      left_join(data.frame(state=state.name,region=state.region), by="state"),
    aes(date, daily_cases, col=region, group=metro_state_county),
    alpha=0.1
  ) +
  xlab("Date") +
  ylab("Daily cases") +
  theme_classic() +
  ggtitle("Daily cases")

p_deaths <- 
  ggplot() +
  geom_line(data=analysis_df_sub %>%
              filter(metro_area!="Los Angeles") %>%
              left_join(data.frame(state=state.name,region=state.region), by="state") %>%
              group_by(region, date) %>%
              summarise(daily_deaths=sum(daily_deaths),
                        avg_retail=mean(retail_and_recreation_percent_change_from_baseline)), 
            aes(date, daily_deaths, col=region), alpha=0.6) + 
  geom_line(
    data=analysis_df_sub %>%
      left_join(data.frame(state=state.name,region=state.region), by="state"),
    aes(date, daily_deaths, col=region, group=metro_state_county),
    alpha=0.1
  ) +
  xlab("Date") +
  ylab("Daily deaths") +
  theme_classic() +
  ggtitle("Daily deaths")

p_parks <- 
  ggplot() +
  geom_line(data=analysis_df_sub %>%
              filter(metro_area!="Los Angeles") %>%
              left_join(data.frame(state=state.name,region=state.region), by="state") %>%
              group_by(region, date) %>%
              summarise(daily_cases=sum(daily_cases),
                        avg_parks=mean(parks_percent_change_from_baseline)), 
            aes(date, avg_parks, col=region), alpha=0.6) + 
  geom_line(
    data=analysis_df_sub %>%
      left_join(data.frame(state=state.name,region=state.region), by="state"),
    aes(date, parks_percent_change_from_baseline, col=region, group=metro_state_county),
    alpha=0.1
  ) +
  xlab("Date") +
  ylab("Percent change from baseline") +
  theme_classic() +
  ggtitle("Parks mobility")

png("results/new_mobility_trends.png", width=14, height=8, units="in", res=300)
plot_grid(p_mob, p_cases, p_parks, p_deaths, ncol=2)
dev.off()

#### Model the distributed lags of mobility across all counties, excluding LA, and excluding Thanksgiving and Christmas day specifically ####
# analysis_df_sub$retail_and_recreation_percent_change_from_baseline[analysis_df_sub$date=="2020-11-26"] <- 
#   tapply(analysis_df_sub$retail_and_recreation_percent_change_from_baseline[analysis_df_sub$date=="2020-11-27"],
#        analysis_df_sub$retail_and_recreation_percent_change_from_baseline[analysis_df_sub$date=="2020-11-25"],fun=mean)
# 
# analysis_df_sub$retail_and_recreation_percent_change_from_baseline[analysis_df_sub$date=="2020-12-25"] <- 
#   tapply(analysis_df_sub$retail_and_recreation_percent_change_from_baseline[analysis_df_sub$date=="2020-12-24"],
#          analysis_df_sub$retail_and_recreation_percent_change_from_baseline[analysis_df_sub$date=="2020-12-26"],fun=mean)

# Create the basis 
retail_basis <- crossbasis(
  analysis_df_sub$retail_and_recreation_percent_change_from_baseline, 
  lag = config$max_lag,
  argvar = list(fun='cr', knots=c(quantile(analysis_df_sub$retail_and_recreation_percent_change_from_baseline, probs=seq(0,1,0.25)))), # df=4), #  # using penalized spline 'cr' ADD SPECIFIC KNOT VALUES
  arglag = list(fun='cr', df=4),
  group = analysis_df_sub$metro_state_county # makes sure I lag appropriately by metro/county
)

# penalize the crossbasis splines
retail_basisPen <- cbPen(retail_basis) 

# Another category
parks_basis <- crossbasis(
  analysis_df_sub$parks_percent_change_from_baseline, 
  lag = config$max_lag,
  argvar = list(fun='cr', knots=c(as.vector(quantile(analysis_df_sub$parks_percent_change_from_baseline, prob=seq(0,1,0.1))))), # using penalized spline 'cr', use deciles for knots
  arglag = list(fun='cr', df=4),
  group = analysis_df_sub$metro_state_county # makes sure I lag appropriately by metro/county
)

# penalize the crossbasis splines
parksPen <- cbPen(parks_basis) 

# Fit the GAM
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
  data=analysis_df_sub,
  paraPen=list(retail_basis=retail_basisPen,
               # workplace_basis=workplacePen,
               parks_basis=parksPen),# this applies the penalty to the lagged matrix
  family="quasipoisson",
  method="REML"
)

dlnm_pred_retail <- crosspred(
  retail_basis,
  dlnm.main.gam_multimob3,
  at=10:-50,
  by=1,
  bylag=1,
  ci.level = 0.95,
  cen= -10 #,  #config$ref_lag,
 # cumul=TRUE
)

plot(dlnm_pred_retail)

retail_rr_fits <- data.frame(dlnm_pred_retail$matRRfit) %>%
  mutate(mobility = as.factor(rownames(.)))

names(retail_rr_fits) <- gsub("lag","",make.names(names(retail_rr_fits)))
# sorted mobility for plot labels
sorted_mobility_retail <- paste(sort(as.integer(levels(retail_rr_fits$mobility))))
retail_rr_fits$mobility <- factor(retail_rr_fits$mobility, levels = sorted_mobility_retail)
selowfits_retail <- data.frame(dlnm_pred_retail$matRRlow) %>% mutate(mobility=as.factor(rownames(.)))
sehighfits_retail <- data.frame(dlnm_pred_retail$matRRhigh) %>% mutate(mobility=as.factor(rownames(.)))
names(selowfits_retail) <- gsub("lag","",make.names(names(selowfits_retail)))
names(sehighfits_retail) <- gsub("lag","",make.names(names(sehighfits_retail)))
sorted_mobility_retail <- paste(sort(as.integer(levels(retail_rr_fits$mobility))))
selowfits_retail$mobility <- factor(selowfits_retail$mobility, levels = sorted_mobility_retail)
sehighfits_retail$mobility <- factor(sehighfits_retail$mobility, levels = sorted_mobility_retail)

# make lagged dataframe for plots
retail_lagged_estimates_df <- retail_rr_fits %>%
  tidyr::gather(lag, RR, -mobility) %>%
  mutate(type="RR") %>%
  left_join(selowfits_retail %>% 
              tidyr::gather(lag, se_low, -mobility), by = c('mobility', 'lag')) %>%
  left_join(sehighfits_retail %>% 
              tidyr::gather(lag, se_high, -mobility), by = c('mobility', 'lag')) %>%
  mutate(lag=as.numeric(lag))

## Do the same for parks
dlnm_pred_parks <- crosspred(
  parks_basis,
  dlnm.main.gam_multimob3,
  at=300:-50,
  by=1,
  bylag=1,
  ci.level = 0.95,
  cen= -10 #,  #config$ref_lag,
  # cumul=TRUE
)

parks_rr_fits <- data.frame(dlnm_pred_parks$matRRfit) %>%
  mutate(mobility = as.factor(rownames(.)))

names(parks_rr_fits) <- gsub("lag","",make.names(names(parks_rr_fits)))
# sorted mobility for plot labels
sorted_mobility_parks <- paste(sort(as.integer(levels(parks_rr_fits$mobility))))
parks_rr_fits$mobility <- factor(parks_rr_fits$mobility, levels = sorted_mobility_parks)
selowfits_parks <- data.frame(dlnm_pred_parks$matRRlow) %>% mutate(mobility=as.factor(rownames(.)))
sehighfits_parks <- data.frame(dlnm_pred_parks$matRRhigh) %>% mutate(mobility=as.factor(rownames(.)))
names(selowfits_parks) <- gsub("lag","",make.names(names(selowfits_parks)))
names(sehighfits_parks) <- gsub("lag","",make.names(names(sehighfits_parks)))
sorted_mobility_parks <- paste(sort(as.integer(levels(parks_rr_fits$mobility))))
selowfits_parks$mobility <- factor(selowfits_parks$mobility, levels = sorted_mobility_parks)
sehighfits_parks$mobility <- factor(sehighfits_parks$mobility, levels = sorted_mobility_parks)

parks_lagged_estimates_df <- parks_rr_fits %>%
  tidyr::gather(lag, RR, -mobility) %>%
  mutate(type="RR") %>%
  left_join(selowfits_parks %>% 
              tidyr::gather(lag, se_low, -mobility), by = c('mobility', 'lag')) %>%
  left_join(sehighfits_parks %>% 
              tidyr::gather(lag, se_high, -mobility), by = c('mobility', 'lag')) %>%
  mutate(lag=as.numeric(lag))


ggplot(
  # subset to certain mobilities
  data = rbind(retail_lagged_estimates_df %>% mutate(type="retail"),parks_lagged_estimates_df %>% mutate(type="parks")) %>% 
    filter(
      mobility %in% c("-80","-70","-60","-50", "-25","-10", "0", "5", '10', "50", "100", "200")
    ) %>%
    mutate(
      # order mobility levels
      mobility = factor(
        mobility, levels=c("-80","-70","-60","-50", "-25","-10", "0", "5", '10', "50", "100", "200")
      )
    ),
  aes(x = lag, y = RR)
) +
  geom_line(
    aes(col=type)
  ) + 
  geom_ribbon(
    aes(ymin=se_low,ymax=se_high, fill=type),alpha=0.1
  )+
  scale_color_manual(
    values=wesanderson::wes_palette("Darjeeling1",n=2,type="discrete"),
    guide=FALSE
  ) +
  scale_fill_manual(
    values=wesanderson::wes_palette("Darjeeling1",n=2,type="discrete"),
    guide=FALSE
  ) +
  geom_hline(yintercept=1, color='black', linetype='dotted', alpha=0.5) +
  xlab('Days After') +
  ylab('Relative Daily Death Rate') +
  facet_wrap(
    ~ mobility, scales="free",
    labeller = labeller(
      mobility = c(
        "-80" = "80% decrease in mobility",
        "-70" = "70% decrease in mobility",
        "-60" = "60% decrease in mobility",
        "-50" = "50% decrease in mobility",
        "-25" = "25% decrease in mobility",
        "-10" = "10% decrease in mobility",
        "0" = "0% change in mobility",
        "5" = "5% increase in mobility",
        "10" = "10% increase in mobility",
        "50" = "50% increase in mobility",
        "100" = "100% increase in mobility",
        "200" = "200% increase in mobility"
      )
    )
  ) +
  guides(
    alpha=guide_legend(title='Model'), 
    linetype=guide_legend(title='Model'),
    size=guide_legend(title='Model')
  ) +
  theme_classic() +
  scale_x_continuous(expand=c(0,0), limits=c(0,60)) +
  scale_y_continuous(expand=c(0,0), limits=c(0.6,1.25)) +
  theme(strip.background = element_blank()) 

# Facet by mobility TYPE instead of by mobility level

p_deaths <- ggplot(
  # subset to certain mobilities
  data = rbind(retail_lagged_estimates_df %>% mutate(type="retail"),parks_lagged_estimates_df %>% mutate(type="parks")) %>% 
    filter(
      mobility %in% c("-80","-70","-60","-50", "-25","-10", "0", "5", '10', "50", "100", "200")
    ) %>%
    mutate(
      # order mobility levels
      mobility = factor(
        mobility, levels=c("-80","-70","-60","-50", "-25","-10", "0", "5", '10', "50", "100", "200")
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
  ylab('Relative Daily Death Rate') +
  facet_wrap(
    ~ type, scales="free",
    labeller = labeller(
      type = c(
        "retail" = "Retail and recreational mobility",
        "parks" = "Parks mobility"
      )
    )
  ) +
  # guides(
  #   alpha=guide_legend(title='Model'), 
  #   linetype=guide_legend(title='Model'),
  #   size=guide_legend(title='Model')
  # ) +
  theme_classic() +
  scale_x_continuous(expand=c(0,0), limits=c(0,60)) +
  scale_y_continuous(expand=c(0,0), limits=c(0.6,1.25)) +
  theme(strip.background = element_blank()) +
  theme(legend.position = "bottom") +
  ggtitle("Deaths")


# Analysis on cases -------------------------------------------------------

dlnm.main.gam_multimob3_cases <- gam(
  daily_cases ~
    retail_basis + # this is the lag matrix 
    #workplace_basis +
    parks_basis + 
    s(time_since_first_case) + 
    s(time_since_first_case, metro_state_county, bs=c("fs")) + # global smoother
    s(PC1, bs="cr", k=5) +
    s(PC2, bs="cr", k=5) +
    s(PC3, bs="cr", k=5) +
    s(PC4, bs="cr", k=5) +
    offset(log(population_v051)), 
  data=analysis_df_sub,
  paraPen=list(retail_basis=retail_basisPen,
               # workplace_basis=workplacePen,
               parks_basis=parksPen),# this applies the penalty to the lagged matrix
  family="quasipoisson",
  method="REML"
)

dlnm_pred_retail_cases <- crosspred(
  retail_basis,
  dlnm.main.gam_multimob3_cases,
  at=10:-50,
  by=1,
  bylag=1,
  ci.level = 0.95,
  cen= -10 #,  #config$ref_lag,
  # cumul=TRUE
)

plot(dlnm_pred_retail_cases)

dlnm_pred_parks_cases <- crosspred(
  parks_basis,
  dlnm.main.gam_multimob3_cases,
  at=300:-50,
  by=1,
  bylag=1,
  ci.level = 0.95,
  cen= -10 #,  #config$ref_lag,
  # cumul=TRUE
)

plot(dlnm_pred_parks_cases)

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

df_cases_retail <- make_lag_slices(dlnm_pred_retail_cases)
df_cases_parks <- make_lag_slices(dlnm_pred_parks_cases)


p_cases <- ggplot(
  # subset to certain mobilities
  data = rbind(df_cases_retail %>% mutate(type="retail"),df_cases_parks %>% mutate(type="parks")) %>% 
    filter(
      mobility %in% c("-80","-70","-60","-50", "-25","-10", "0", "5", '10', "50", "100", "200")
    ) %>%
    mutate(
      # order mobility levels
      mobility = factor(
        mobility, levels=c("-80","-70","-60","-50", "-25","-10", "0", "5", '10', "50", "100", "200")
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
  ylab('Relative Daily Death Rate') +
  facet_wrap(
    ~ type, scales="free",
    labeller = labeller(
      type = c(
        "retail" = "Retail and recreational mobility",
        "parks" = "Parks mobility"
      )
    )
  ) +
  # guides(
  #   alpha=guide_legend(title='Model'), 
  #   linetype=guide_legend(title='Model'),
  #   size=guide_legend(title='Model')
  # ) +
  theme_classic() +
  scale_x_continuous(expand=c(0,0), limits=c(0,30)) +
  scale_y_continuous(expand=c(0,0), limits=c(0.6,1.25)) +
  theme(strip.background = element_blank()) +
  theme(legend.position = "bottom")+
  ggtitle("Cases")

ggsave("results/new_dlnm_cases.png", p_cases, device="png", width=8, height=6, units="in")
ggsave("results/new_dlnm_deaths.png", p_deaths, device="png", width=8, height=6, units="in")

