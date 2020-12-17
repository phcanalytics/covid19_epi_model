# Extract DLNM slices

dlnm_pred_retail

# RR fits and SE
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

dlnm_pred_parks

# make lagged dataframe for plots
retail_lagged_estimates_df <- retail_rr_fits %>%
  tidyr::gather(lag, RR, -mobility) %>%
  mutate(type="RR") %>%
  left_join(selowfits_retail %>% 
              tidyr::gather(lag, se_low, -mobility), by = c('mobility', 'lag')) %>%
  left_join(sehighfits_retail %>% 
              tidyr::gather(lag, se_high, -mobility), by = c('mobility', 'lag')) %>%
  mutate(lag=as.numeric(lag))

#### Parks ####

# RR fits and SE
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


counterfact_df <- analysis_df # duplicates dataset
counterfact_df[, 'parks_percent_change_from_baseline'] = -50 # sets them all to -50
# new crossbasis AFTER you've already fit the model with the observed crossbasis
parks_basis_counfact <- crossbasis(
  counterfact_df$parks_percent_change_from_baseline, 
  lag = config$max_lag,
  argvar = list(fun='cr', df=4), # using penalized spline 'cr'
  arglag = list(fun='cr', df=4),
  group = counterfact_df$metro_state_county # makes sure I lag appropriately by metro/county
)
