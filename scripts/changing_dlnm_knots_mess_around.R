retail_basis_try <- crossbasis(
  analysis_df$retail_and_recreation_percent_change_from_baseline, 
  lag = config$max_lag,
  argvar = list(fun='cr', knots=c(-80,-60,-40,-20,0,20)), # using penalized spline 'cr'
  arglag = list(fun='cr', df=4),
  group = analysis_df$metro_state_county # makes sure I lag appropriately by metro/county
)

retail_basisPen_try <- cbPen(retail_basis_try) 

parks_basis_try <- crossbasis(
  analysis_df$parks_percent_change_from_baseline, 
  lag = config$max_lag,
  argvar = list(fun='cr', df=10), # using penalized spline 'cr'
  arglag = list(fun='cr', df=4),
  group = analysis_df$metro_state_county # makes sure I lag appropriately by metro/county
)

parks_basisPen_try <- cbPen(parks_basis_try) 

dlnm.main.gam_multimob4 <- gam(
  daily_deaths ~
    retail_basis_try + # this is the lag matrix 
    #workplace_basis +
    parks_basis_try + 
    s(time_since_first_death) + 
    s(time_since_first_death, metro_state_county, bs=c("fs")) + # global smoother
    s(PC1, bs="cr", k=5) +
    s(PC2, bs="cr", k=5) +
    s(PC3, bs="cr", k=5) +
    s(PC4, bs="cr", k=5) +
    offset(log(population_v051)), 
  data=analysis_df,
  paraPen=list(retail_basis_try=retail_basisPen_try,
               # workplace_basis=workplacePen,
               parks_basis_try=parks_basisPen_try),# this applies the penalty to the lagged matrix
  family="quasipoisson",
  method="REML"
)

dlnm_pred_retail_cumul_try <- crosspred(
  retail_basis_try,
  dlnm.main.gam_multimob4,
  at=10:-80,
  by=1,
  bylag=1,
  ci.level = 0.95,
  cen= config$ref_lag,
  cumul=TRUE
)

plot(dlnm_pred_retail_cumul_try, theta=400)

lag30 <- data.frame(cumRR=dlnm_pred_retail_cumul_try$cumRRfit[,"lag30"]) %>%
  mutate(mobility=as.numeric(rownames(.))) %>%
  ggplot(aes(mobility, cumRR)) + geom_point() + geom_line() +
  #ylim(c(0,1.3)) +
  geom_hline(yintercept=1.0, linetype="dashed") +
  ggtitle("Retail & recreation, 30-day cumulative effect, all counties") +
  theme_bw()

lag60 <- data.frame(cumRR=dlnm_pred_retail_cumul_try$cumRRfit[,"lag60"]) %>%
  mutate(mobility=as.numeric(rownames(.))) %>%
  ggplot(aes(mobility, cumRR)) + geom_point() + geom_line() +
  #ylim(c(0,1.3)) +
  geom_hline(yintercept=1.0, linetype="dashed") +
  ggtitle("Retail & recreation, 60-day cumulative effect, all counties") +
  theme_bw()

plot_grid(lag30, lag60, ncol=2)


# To do RR slice plots ----------------------------------------------------

data.frame(cumRR=dlnm_pred_retail_cumul$cumRRfit[1,]) %>%
  mutate(lag=as.numeric(substr(rownames(.),4,5))) %>%
  ggplot(aes(lag,cumRR)) + geom_point() + theme_bw() +
  geom_hline(yintercept=1.0, linetype="dashed") +
  ggtitle("Cumulative RR at -80 mobility, at multiple lags")

data.frame(cumRR=dlnm_pred_retail_cumul$cumRRfit["-10",]) %>%
  mutate(lag=as.numeric(substr(rownames(.),4,5))) %>%
  ggplot(aes(lag,cumRR)) + geom_point() + theme_bw() +
  ggtitle("Cumulative RR at -10 mobility, at multiple lags")

data.frame(cumRR=dlnm_pred_retail_cumul$cumRRfit["-60",]) %>%
  mutate(lag=as.numeric(substr(rownames(.),4,5))) %>%
  ggplot(aes(lag,cumRR)) + geom_point() + theme_bw() +
  geom_hline(yintercept=1.0, linetype="dashed") +
  ggtitle("Cumulative RR at -60 mobility, at multiple lags")

data.frame(cumRR=dlnm_pred_retail_cumul$cumRRfit["-50",]) %>%
  mutate(lag=as.numeric(substr(rownames(.),4,5))) %>%
  ggplot(aes(lag,cumRR)) + geom_point() + theme_bw() +
  geom_hline(yintercept=1.0, linetype="dashed") +
  ggtitle("Cumulative RR at -50 mobility, at multiple lags")

data.frame(cumRR=dlnm_pred_retail_cumul$cumRRfit["-20",]) %>%
  mutate(lag=as.numeric(substr(rownames(.),4,5))) %>%
  ggplot(aes(lag,cumRR)) + geom_point() + theme_bw() +
  geom_hline(yintercept=1.0, linetype="dashed") +
  ggtitle("Cumulative RR at -20 mobility, at multiple lags")

data.frame(cumRR=dlnm_pred_retail_cumul$cumRRfit["-30",]) %>%
  mutate(lag=as.numeric(substr(rownames(.),4,5))) %>%
  ggplot(aes(lag,cumRR)) + geom_point() + theme_bw() +
  geom_hline(yintercept=1.0, linetype="dashed") +
  ggtitle("Cumulative RR at -30 mobility, at multiple lags")

data.frame(cumRR=dlnm_pred_retail_cumul$cumRRfit["0",]) %>%
  mutate(lag=as.numeric(substr(rownames(.),4,5))) %>%
  ggplot(aes(lag,cumRR)) + geom_point() + theme_bw() +
  geom_hline(yintercept=1.0, linetype="dashed") +
  ggtitle("Cumulative RR at 0 mobility, at multiple lags")

data.frame(cumRR=dlnm_pred_retail_cumul$matRRfit["0",]) %>%
  mutate(lag=as.numeric(substr(rownames(.),4,5))) %>%
  ggplot(aes(lag,cumRR)) + geom_point() + theme_bw() +
  geom_hline(yintercept=1.0, linetype="dashed") +
  ggtitle("RR at 0 mobility, at multiple lags")


plot.crosspred(dlnm_pred_retail_cumul,
               theta=400)

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

## Plot
ggplot(
  # subset to certain mobilities
  data = rbind(retail_lagged_estimates_df %>% mutate(type="retail")) %>% 
    filter(
      mobility %in% c("-80","-70","-60","-50", "-25","-10", "0", "5", '10')
    ) %>%
    mutate(
      # order mobility levels
      mobility = factor(
        mobility, levels=c("-80","-70","-60","-50", "-25","-10", "0", "5", '10')
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
    values=wesanderson::wes_palette("Darjeeling1",n=1,type="discrete"),
    guide=FALSE
  ) +
  scale_fill_manual(
    values=wesanderson::wes_palette("Darjeeling1",n=1,type="discrete"),
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
        "10" = "10% increase in mobility"
      )
    )
  ) +
  guides(
    alpha=guide_legend(title='Model'), 
    linetype=guide_legend(title='Model'),
    size=guide_legend(title='Model')
  ) +
  theme_classic() +
  scale_x_continuous(expand=c(0,0), limits=c(0,30)) +
  scale_y_continuous(expand=c(0,0), limits=c(0.6,1.25)) +
  theme(strip.background = element_blank()) +
  theme(legend.position = "bottom") 
