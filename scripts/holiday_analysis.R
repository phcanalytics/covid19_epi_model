# Mobility and covid-19 in the US: a story in ggplot

# libraries 
library(tidyverse)
library(mgcv) # for gam 
library(dlnm)
library(wesanderson)
library(yaml)
library(cowplot)
library(ggplot2)

# Setup --------------------------------------------------------

# Read config file
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
  # filter(county!="El Paso County") %>%
  filter(date!="2020-11-13") %>% # try
  filter(date!="2020-11-26") %>%
  filter(date!="2020-12-21") %>%
  #filter(date!="2020-08-06") %>% # abnormal day for NYC
  filter(date!="2020-12-25") %>%
  #filter(date!="2020-07-04") %>% # July 4th holiday
  mutate(metro_state_county=factor(metro_state_county)) 


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

# Exploratory plots -------------------------------------------------------



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
    aes(date, daily_cases/population_v051*100000, col=region, group=metro_state_county),
    alpha=0.1
  ) +
  xlab("Date") +
  ylab("Daily cases per 100,000 persons") +
  theme_classic() +
  ggtitle("Daily case rate")

p_deaths <- 
  ggplot() +
  # geom_line(data=analysis_df_sub %>%
  #             filter(metro_area!="Los Angeles") %>%
  #             left_join(data.frame(state=state.name,region=state.region), by="state") %>%
  #             group_by(region, date) %>%
  #             summarise(daily_deaths=sum(daily_deaths),
  #                       avg_retail=mean(retail_and_recreation_percent_change_from_baseline)),
  #           aes(date, daily_deaths, col=region), alpha=0.6) +
  geom_line(
    data=analysis_df_sub %>%
      left_join(data.frame(state=state.name,region=state.region), by="state"),
    aes(date, daily_deaths/population_v051*100000, col=region, group=metro_state_county),
    alpha=0.1
  ) +
  xlab("Date") +
  ylab("Daily deaths per 100,000 persons") +
  theme_classic() +
  ggtitle("Daily death rate")

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


# Deaths ------------------------------------------------------------------


#### Model the distributed lags of mobility across all counties, excluding LA, and excluding Thanksgiving and Christmas day specifically
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
ptm.deaths <- proc.time()

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

deaths_gam_time <- proc.time()-ptm.deaths

dlnm_pred_retail <- crosspred(
  retail_basis,
  dlnm.main.gam_multimob3,
  at=10:-50,
  by=1,
  bylag=1,
  ci.level =0.95,
  cen= -20 #,  #config$ref_lag,
  # cumul=TRUE
)

plot(dlnm_pred_retail)

## Do the same for parks
dlnm_pred_parks <- crosspred(
  parks_basis,
  dlnm.main.gam_multimob3,
  at=300:-50,
  by=1,
  bylag=1,
  ci.level = 0.95,
  cen= 0 #,  #config$ref_lag,
  # cumul=TRUE
)


df_deaths_retail <- make_lag_slices(dlnm_pred_retail)
df_deaths_parks <- make_lag_slices(dlnm_pred_parks)

# Facet by mobility TYPE instead of by mobility level

p_deaths <- ggplot(
  # subset to certain mobilities
  data = rbind(df_deaths_retail %>% mutate(type="retail"),df_deaths_parks %>% mutate(type="parks")) %>% 
    filter(
      mobility %in% c("-30", "-20", "-10", "-5", "0",  "50", "100")
      #mobility %in% c("-80","-70","-60","-50", "-25","-10", "0", "5", '10', "50", "100", "200")
    ) %>%
    mutate(
      # order mobility levels
      mobility = factor(
        mobility, levels=c("-30", "-20", "-10", "-5", "0",  "50", "100")
        #mobility, levels=c("-80","-70","-60","-50", "-25","-10", "0", "5", '10', "50", "100", "200") # c("-40", "-20", "-10", "0",  "50", "100")
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
  #scale_x_continuous(expand=c(0,0), limits=c(0,60)) +
  #scale_y_continuous(expand=c(0,0), limits=c(0.6,1.25)) +
  theme(strip.background = element_blank()) +
  theme(legend.position = "bottom") +
  ggtitle("Deaths")

ggsave("results/dlnm_slices_deaths.png", p_deaths, device="png", width=8, height=6, units="in")

# Make cumulative plots instead

dlnm_pred_retail_cumul_try <- crosspred(
  retail_basis,
  dlnm.main.gam_multimob3,
  at=10:-40,
  by=1,
  bylag=1,
  ci.level = 0.95,
  cen= -20,
  cumul=TRUE
)

data.frame(cumRR=dlnm_pred_retail_cumul_try$cumRRfit[1,]) %>%
  mutate(lag=as.numeric(substr(rownames(.),4,5))) %>%
  ggplot(aes(lag,cumRR)) + geom_point() + theme_bw() +
  geom_hline(yintercept=1.0, linetype="dashed") +
  ggtitle("Cumulative RR at -50 mobility, at multiple lags") +
  ylim(c(0.5,2))

data.frame(cumRR=dlnm_pred_retail_cumul_try$cumRRfit["-20",]) %>%
  mutate(lag=as.numeric(substr(rownames(.),4,5))) %>%
  ggplot(aes(lag,cumRR)) + geom_point() + theme_bw() +
  geom_hline(yintercept=1.0, linetype="dashed") +
  ggtitle("Cumulative RR at -20 mobility, at multiple lags")

dlnm_pred_parks_cumul <- crosspred(
  parks_basis,
  dlnm.main.gam_multimob3,
  at=300:-20,
  by=1,
  bylag=1,
  ci.level = 0.95,
  cen= 0,
  cumul=TRUE
)

df_cumulative_deaths_retail <- make_cumulative_slices(dlnm_pred_retail_cumul_try)
df_cumulative_parks_retail <- make_cumulative_slices(dlnm_pred_parks_cumul)

# Facet by mobility TYPE instead of by mobility level

p_cumul_deaths <- ggplot(
  # subset to certain mobilities
  data = rbind(df_cumulative_deaths_retail %>% mutate(type="retail"),df_cumulative_parks_retail %>% mutate(type="parks")) %>% 
    filter(
      mobility %in% c("-30", "-20", "-10", "-5", "0",  "50", "100")
      # c("-80","-70","-60","-50","-40", "-30", "-20" ,"-10", "0", "5", '10', "50", "100") #, "200")
    ) %>%
    mutate(
      # order mobility levels
      mobility = factor(
        mobility, levels=
          c("-30", "-20", "-10", "-5", "0",  "50", "100")
        #c("-80","-70","-60","-50","-40", "-30", "-20" ,"-10", "0", "5", '10', "50", "100") #c("-40", "-20", "-10", "0",  "50", "100")
      )
    ), # %>%
  # filter(case_when(type=="parks" ~ mobility%in%c("-10", "0", "50", "100"),
  #                  TRUE ~ TRUE)),
  aes(x = lag, y = RR)
) +
  geom_line(
    aes(col=mobility)
  ) + 
  geom_ribbon(
    aes(ymin=se_low,ymax=se_high, fill=mobility),alpha=0.1
  )+
  scale_color_manual(
    values=wesanderson::wes_palette("Darjeeling1",n=13,type="continuous")
  ) +
  scale_fill_manual(
    values=wesanderson::wes_palette("Darjeeling1",n=13,type="continuous"),
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
  #scale_x_continuous(expand=c(0,0), limits=c(0,60)) +
  #scale_y_continuous(expand=c(0,0), limits=c(0.6,1.25)) +
  theme(strip.background = element_blank()) +
  theme(legend.position = "bottom") +
  ggtitle("Deaths")

ggsave("results/dlnm_cumul_deaths.png", p_cumul_deaths, device="png", width=8, height=6, units="in")

p_cumul_deaths + scale_x_continuous(expand=c(0,0), limits=c(50,60)) +
  scale_y_continuous(expand=c(0,0), limits=c(0.6, 7))

# Cases -------------------------------------------------------

ptm <- proc.time()

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

case_gam_time <- proc.time()-ptm

dlnm_pred_retail_cases <- crosspred(
  retail_basis,
  dlnm.main.gam_multimob3_cases,
  at=10:-50,
  by=1,
  bylag=1,
  ci.level = 0.95,
  cen= -20 #,  #config$ref_lag,
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
  cen= 0 #,  #config$ref_lag,
  # cumul=TRUE
)

plot(dlnm_pred_parks_cases)

df_cases_retail <- make_lag_slices(dlnm_pred_retail_cases)
df_cases_parks <- make_lag_slices(dlnm_pred_parks_cases)


p_cases <- ggplot(
  # subset to certain mobilities
  data = rbind(df_cases_retail %>% mutate(type="retail"),df_cases_parks %>% mutate(type="parks")) %>% 
    filter(
      mobility %in% c("-40", "-20", "-10", "0",  "50", "100")
    ) %>%
    mutate(
      # order mobility levels
      mobility = factor(
        mobility, levels=c("-40", "-20", "-10", "0",  "50", "100")
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

ggsave("results/dlnm_slices_cases.png", p_cases, device="png", width=8, height=6, units="in")

# Cumulative effect on cases
dlnm_pred_parks_cases_cumul <- crosspred(
  parks_basis,
  dlnm.main.gam_multimob3_cases,
  at=300:-50,
  by=1,
  bylag=1,
  ci.level = 0.95,
  cen= 0, # mean(analysis_df_sub$parks_percent_change_from_baseline)
  cumul=TRUE
)

dlnm_pred_retail_cases_cumul <- crosspred(
  retail_basis,
  dlnm.main.gam_multimob3_cases,
  at=10:-50,
  by=1,
  bylag=1,
  ci.level = 0.95,
  cen= -20,  #config$ref_lag, # mean(analysis_df_sub$retail_and_recreation_percent_change_from_baseline)
  cumul=TRUE
)

data.frame(cumRR=dlnm_pred_retail_cases_cumul$cumRRfit[1,]) %>%
  mutate(lag=as.numeric(substr(rownames(.),4,5))) %>%
  ggplot(aes(lag,cumRR)) + geom_point() + theme_bw() +
  geom_hline(yintercept=1.0, linetype="dashed") +
  ggtitle("Cumulative RR at -50 mobility, at multiple lags, CASES")

data.frame(cumRR=dlnm_pred_retail_cases_cumul$cumRRfit["-20",]) %>%
  mutate(lag=as.numeric(substr(rownames(.),4,5))) %>%
  ggplot(aes(lag,cumRR)) + geom_point() + theme_bw() +
  geom_hline(yintercept=1.0, linetype="dashed") +
  ggtitle("Cumulative RR at -20 mobility, at multiple lags, CASES")


df_cumulative_cases_retail <- make_cumulative_slices(dlnm_pred_retail_cases_cumul)
df_cumulative_cases_parks <- make_cumulative_slices(dlnm_pred_parks_cases_cumul)

# Facet by mobility TYPE instead of by mobility level

p_cumul_cases <- ggplot(
  # subset to certain mobilities
  data = rbind(df_cumulative_cases_retail %>% mutate(type="retail"),df_cumulative_cases_parks %>% mutate(type="parks")) %>% 
    filter(
      mobility %in% c("-40", "-20", "-10", "0",  "50", "100")
    ) %>%
    mutate(
      # order mobility levels
      mobility = factor(
        mobility, levels=c("-40", "-20", "-10", "0",  "50", "100")
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
  #scale_x_continuous(expand=c(0,0), limits=c(0,60)) +
  #scale_y_continuous(expand=c(0,0), limits=c(0.6,1.25)) +
  theme(strip.background = element_blank()) +
  theme(legend.position = "bottom") +
  ggtitle("Cases")

ggsave("results/dlnm_cumul_cases.png", p_cumul_cases, device="png", width=8, height=6, units="in")
