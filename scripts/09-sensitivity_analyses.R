# ------------------------------------------------------------------------------
# Title: Sensivitiy generalized additive model for COVID-19 county association 
# Author: Sarah McGough and Ryan Gan
# Purpose: Sensitivity GAM models with dlnm lagged mobility and principle components
# ------------------------------------------------------------------------------

# Libraries ----
library(tidyverse)
library(mgcv) # for gam 
library(dlnm) # for distributed lag
library(datasets) # for state abbreviation
# libraries used in plots
library(ggrepel) # for cleaner plot labels
library(reshape2) # for plotting
library(gridExtra) # grid alignments
library(cowplot)

# Read in analysis dataframe ---------------------------------------------------

analysis_df <- read_csv('./data/05-analysis_df.csv') %>% 
  # starting at 30 days prior to time since first death in order to get lagged
  # values 30 days prior
  filter(time_since_first_death >= (-30)) %>% 
  # code metro_state_county as factor
  mutate(metro_state_county = as.factor(metro_state_county))


# descriptive plots here


# MAIN DLMN MODEL --------------------------------------------------------------

# Note to use the crossbasis function from the dlmn package to create the lagged
# values and create the crossbasis between basis for lagged mobility and 
# basis for mobility we kept 30 days prior to first death for mobility
# so that for each county of interest, there should be complete mobility lagged
# matrix even though there will be missing values in the matrix that will
# be subsequently exluded in the gam modeling process


# build crossbasis for gam model for 30 day lagged mobility of retail and rec
# called cbgam 
cbgam <- crossbasis(
  analysis_df$retail_and_recreation_percent_change_from_baseline, 
  lag = 30,
  argvar = list(fun='cr', df=4), # using penalized spline 'cr'
  arglag = list(fun='cr', df=4),
  group = analysis_df$metro_state_county # makes sure I lag appropriately by metro/county
)



# crossbasis summary
sink('./results/09-crossbasis_summary.txt')
print(summary(cbgam))

print('#######################################################################')
print('Dimensions of non-missing lagged matrix :')
print(dim(na.omit(cbgam)))

sink()

# unit check on number of obs expected
expect_n_obs <- analysis_df %>% filter(time_since_first_death >= 0) %>% nrow()
# check that number of rows line up with dimensions of matrix
if (expect_n_obs == dim(na.omit(cbgam))[1]) {
  print('Observations same length in crossbasis and dataframe')
} else {
  print('Observations different; check crossbasis object and dataframe')
}

# penalize the crossbasis splines
cbgamPen <- cbPen(cbgam) 

# fit gam with dlnm modeling first; this one uses the dlmn matrix; 
# other version of model treats them as fixed variables to allow for use of predict.gam
# this was done as the dlmn matrix messes up some functionality of predict
dlnm.main.gam <- gam(
  daily_deaths ~
    cbgam + # this is the lag matrix 
    s(time_since_first_death) + 
    s(time_since_first_death, metro_state_county, bs=c("fs")) + # global smoother
    s(PC1, bs="cr", k=5) +
    s(PC2, bs="cr", k=5) +
    s(PC3, bs="cr", k=5) +
    s(PC4, bs="cr", k=5) +
    offset(log(population_v051)), 
  data=analysis_df,
  paraPen=list(cbgam=cbgamPen),# this applies the penalty to the lagged matrix
  family="quasipoisson",
  method="REML"
)


# Results ----------------------------------------------------------------------

# Model general summaries and plots 
# check gam summary
summary(dlnm.main.gam)

# saving gam summary of dlmn matrix
sink("./results/09-dlnm_gam_summary.txt")
print(summary(dlnm.main.gam))
sink()

# save partial dependency plot of smooth terms 
# save plot
pdf(
  file = './results/09-dlmn_gam_partial_dependcy_plot.pdf'
  , width = 8
  , height = 6
)
plot(dlnm.main.gam, pages = 1, seWithMean = TRUE)
dev.off()

# Association between COVID mortality and PC1 plots ----------------------------
# (using partial plot output); 'save' plot object that contains partial dependencies 
# option for residuals to view fit
pdf(
  file = './results/09-dlmn_gam_partial_dependcy_plot_residuals.pdf'
  , width = 8
  , height = 6
)
save <- plot(dlnm.main.gam, pages=1, residuals=TRUE, seWithMean = TRUE)
dev.off()


# get state abbreviations
states <- data.frame(state.name, state.abb)
# limit to complete data used in model so that vector of predictions from plot
# 'save' object match dataframe
complete_data <- analysis_df %>% 
  filter(time_since_first_death >= 0)

# principle components 1 plot 
pc1_plot <- ggplot() +
  geom_point(
    # join in residuals with complete data
    data = cbind(complete_data, predicted=save[[3]]$p.resid) %>% 
           #mutate(county_state=paste(county,state,sep=", ")) %>%
           group_by(metro_state_county, metro_area, county, state) %>% 
           summarise(PC1=mean(PC1),predicted=mean(predicted)) %>%
           left_join(states, by=c("state"="state.name")) %>%
           mutate(county_state=paste(county,state.abb,sep=", ")),
    aes(x = PC1, y = exp(predicted), shape=metro_area),size=2) +
  # pc1 fit
  geom_line(
    data = data.frame(cbind(x=save[[3]]$x, y=exp(save[[3]]$fit))),
    aes(x = x , y = V2),col="darkblue") +
  # 95% CI around estimate
  geom_ribbon(
    data = data.frame(
      cbind(x = save[[3]]$x, y = exp(save[[3]]$fit), 
            lower95 = exp(save[[3]]$fit - 2*save[[3]]$se),
            upper95 = exp(save[[3]]$fit + 2*save[[3]]$se)
            )
      ),
    aes(x = x, ymin = V3, ymax = V4), fill='lightblue', alpha = 0.5) +
  # labels
  geom_text_repel(
    data = cbind(complete_data, predicted=save[[3]]$p.resid) %>% 
                 group_by(metro_state_county, metro_area, county, state) %>% 
                 summarise(PC1=mean(PC1),predicted=mean(predicted)) %>%
                 left_join(data.frame(state.name,state.abb), by=c("state"="state.name")) %>%
                 mutate(county_state=paste(county,state.abb,sep=", ")),
    aes(x = PC1, y = exp(predicted), label = county_state), 
    size=3.75, direction="both", force=40, segment.size=0.25, 
    segment.linetype="dashed"
    ) +
  theme_classic() +
  ylab("Relative Daily Mortality Rate (RR)") +
  xlab("Health and Wealth") +
  coord_cartesian(ylim=c(0,8)) +
  theme(axis.text = element_text(size=12), axis.title = element_text(size=14)) +
  theme(legend.text = element_text(size=12), legend.title=element_text(size=14))

# save plot
ggsave(
  filename = './results/09-dlmn_gam_pc1_plot.pdf',
  plot = pc1_plot,
  height = 8,
  width = 16,
  unit = 'in'
  )

# saving metro area comparsions of SF and NO
sink("./results/09-sanfran_neworleans_estimates.txt")

# Get the relative ranges for New Orleans vs SF
city_est <- cbind(complete_data, predicted=save[[3]]$p.resid) %>% 
  mutate(county_state=paste(county,state,sep=", ")) %>%
  group_by(metro_state_county, metro_area, county) %>% 
  summarise(PC1=mean(PC1),predicted=mean(predicted)) %>% 
  filter(metro_area%in%c("New Orleans","San Francisco")) %>% 
  group_by(metro_area) %>% summarise(min=min(exp(predicted)),max=max(exp(predicted)))  
print(city_est)

sink()

# Replication of Figure 2B. Visualize certain PC1 variables --------------------

pc1_levels <- data.frame(
  PC1=complete_data$PC1, metro_state_county=complete_data$metro_state_county
  ) %>% 
  distinct() %>%
  arrange(PC1) %>% 
  pull(metro_state_county)

pc1_levels_x <- data.frame(
  PC1=complete_data$PC1, metro_state_county=complete_data$metro_state_county
  ) %>% 
  arrange(PC1) %>%
  distinct()

p_smoking <- complete_data %>%
  select(
    metro_state_county, disease_diabetes_prevalence_v060, adult_smoking_v009, 
    education_some_college_v069, median_household_income_v063
  ) %>%
  rename("Diabetes prevalence"=disease_diabetes_prevalence_v060) %>%
  rename("Adult smoking prevalence"=adult_smoking_v009) %>%
  rename("College education prevalence"=education_some_college_v069) %>%
  rename("Median household income (USD)"=median_household_income_v063) %>%
  left_join(pc1_levels_x) %>%
  mutate(metro_state_county=factor(metro_state_county, 
                                   levels = rev(pc1_levels))) %>%
  melt(id=c("metro_state_county","PC1")) %>%
  filter(variable=="Adult smoking prevalence") %>%
  distinct() %>% 
  ggplot(aes(metro_state_county, value)) +
  geom_bar(stat="identity", aes(fill=PC1)) +
  scale_fill_gradient2(low = "dodgerblue", high="lightblue", midpoint=10) + 
  xlab("") +
  ylab("Prevalence (fraction of population)") +
  coord_flip() +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  scale_y_continuous(expand = c(0, 0)) +
  ggtitle("B. Adult smoking prevalence") +
  theme(legend.position="none") +
  theme(axis.text.y = element_blank()) +
  theme(panel.border = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")) 

# income plot
p_income <- complete_data %>%
  select(
    metro_state_county, disease_diabetes_prevalence_v060, adult_smoking_v009, 
    education_some_college_v069, median_household_income_v063) %>%
  # transform median household income to be by 10k units
  mutate(median_household_income_10k = median_household_income_v063/1000) %>% 
  rename("Diabetes prevalence"=disease_diabetes_prevalence_v060) %>%
  rename("Adult smoking prevalence"=adult_smoking_v009) %>%
  rename("College education prevalence"=education_some_college_v069) %>%
  rename("Median household income (USD 10k)"= median_household_income_10k) %>%
  left_join(pc1_levels_x) %>%
  mutate(metro_state_county=factor(metro_state_county, 
                                   levels = rev(pc1_levels))) %>%
  melt(id=c("metro_state_county","PC1")) %>%
  filter(variable=="Median household income (USD 10k)") %>%
  distinct() %>%  # one unique value per county; needs this or the plot is overscaled
  ggplot(aes(x=metro_state_county, y=value)) +
  geom_bar(stat="identity", aes(fill=PC1)) +
  scale_fill_gradient2(low = "dodgerblue", high="lightblue", midpoint=10)+ 
  xlab("") +
  ylab("Dollars (USD 10k)") +
  coord_flip() +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  scale_y_continuous(expand = c(0, 0)) +
  ggtitle("C. Median household income") +
  theme(legend.position="none") +
  theme(axis.text.y = element_blank()) +
  theme(panel.border = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")) 


# grid extra layout
fig2_layout <- rbind(
  c(1,1,1,2),
  c(1,1,1,3)
)

# png version
png("./results/09-pc1_pca_fig_2.png",width=16, height=9, units="in", res=300)
grid.arrange(pc1_plot, p_smoking, p_income, layout_matrix=fig2_layout)
dev.off()

# pdf version
pdf(
  file = './results/09-pc1_pca_fig_2.pdf',
  width = 16,
  height = 9,
  )
grid.arrange(pc1_plot, p_smoking, p_income, layout_matrix=fig2_layout)
dev.off()


# Lagged mobility crossbasis plot ----------------------------------------------
# pulling out lagged estimates using dlmn package

dlnm_pred <- crosspred(
  cbgam, # cross basis object 
  dlnm.main.gam, # gam model with dlmn matrix 
  at=10:-80, # discussion on range to model
  cen = 0, # cender at 0
  ci.level=0.95 # modeling at .95 CI
)


# save plot
pdf(
  file = './results/09-mobility_crossbasis.pdf'
  , width = 8
  , height = 8
)
plot(
  dlnm_pred, 
  xlab= "% Change Rec/Retail Mobility (centered 0 mobility)", 
  zlab= "Relative Daily Death Rate", theta=400, phi=30, lphi=20,
)
dev.off()

# Lagged Mobility Slice Plots --------------------------------------------------

# finer resolution of predictions over lagged dates for smoother looking plots
# only difference here is bylag of 0.2 per 1 day
dlnm_pred <- crosspred(
  cbgam, 
  dlnm.main.gam,
  at=10:-80,
  by=1,
  bylag=0.2,
  ci.level = 0.95,
  cen=0,
)


# Mobility slice plots at multiple mobility changes ----------------------------

# Build dataframe of mobility relative rate estimates for full model
# For full model; this is the RR for different lags at 0% mobility
rr_fits <- data.frame(dlnm_pred$matRRfit) %>% 
  mutate(mobility = as.factor(rownames(.)))

# strip lag off days
names(rr_fits) <- gsub("lag","",make.names(names(rr_fits)))
# sorted mobility for plot labels
sorted_mobility <- paste(sort(as.integer(levels(rr_fits$mobility))))
rr_fits$mobility <- factor(rr_fits$mobility, levels = sorted_mobility)
selowfits <- data.frame(dlnm_pred$matRRlow) %>% mutate(mobility=as.factor(rownames(.)))
sehighfits <- data.frame(dlnm_pred$matRRhigh) %>% mutate(mobility=as.factor(rownames(.)))
names(selowfits) <- gsub("lag","",make.names(names(selowfits)))
names(sehighfits) <- gsub("lag","",make.names(names(sehighfits)))
sorted_mobility <- paste(sort(as.integer(levels(rr_fits$mobility))))
selowfits$mobility <- factor(selowfits$mobility, levels = sorted_mobility)
sehighfits$mobility <- factor(sehighfits$mobility, levels = sorted_mobility)


 # make lagged dataframe for plots
lagged_estimates_df <- rr_fits %>%
  tidyr::gather(lag, RR, -mobility) %>%
  mutate(type="RR") %>%
  left_join(selowfits %>% 
              tidyr::gather(lag, se_low, -mobility), by = c('mobility', 'lag')) %>%
  left_join(sehighfits %>% 
              tidyr::gather(lag, se_high, -mobility), by = c('mobility', 'lag')) %>%
  mutate(lag=as.numeric(lag))

# slice lagged mobility plot for all counties 
lagged_plot <- ggplot(
  # subset mobility levels and convert to factor
  data = lagged_estimates_df %>% 
    filter(
      mobility %in% c("-80","-70","-60","-50", "-25","-10", "0", "5", '10')
      ) %>%
    mutate(
      mobility = factor(
        mobility, 
        levels=c("-80","-70","-60","-50", "-25","-10", "0", "5", '10')
        )
      ), 
  aes(x=lag, y=RR)
  ) +
  geom_line(aes(col=mobility)) + 
  scale_size_manual(values=c(1,0.65)) +
  geom_ribbon(aes(ymin=se_low,ymax=se_high, fill=mobility), alpha = 0.5) +
  scale_color_manual(values=wesanderson::wes_palette("Darjeeling1",n=9,type="continuous"),guide=FALSE) +
  scale_fill_manual(values=wesanderson::wes_palette("Darjeeling1",n=9,type="continuous"),guide=FALSE) +
  geom_hline(yintercept=1, color='black', linetype='dotted', alpha=0.5) +
  xlab('Days After') +
  ylab('Relative Daily Death Rate') +
  facet_wrap(
    ~ mobility, 
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
  theme_minimal()

# save lagged plot with all counties
ggsave(
  plot = lagged_plot,
  filename = './results/09-mobility_slices_all_counties.pdf',
  height = 8,
  width = 10,
  unit = 'in'
)

# SENSITIVITY ANALYSIS: EXCLUSION OF NEW YORK CITY -----------------------------
# dlmn model excluding nyc

nonyc_df <- read_csv('./data/05-sensitivity_analysis_nonyc_df.csv') %>% 
  # starting at 30 days prior to time since first death in order to get lagged
  # values 30 days prior
  filter(time_since_first_death >= (-30)) %>% 
  # code metro_state_county as factor
  mutate(metro_state_county = as.factor(metro_state_county))

# build crossbasis for gam model for 30 day lagged mobility of retail and rec
# called nonyc_cbgam 
nonyc_cbgam <- crossbasis(
  nonyc_df$retail_and_recreation_percent_change_from_baseline, 
  lag = 30,
  argvar = list(fun='cr', df=4), # using penalized spline 'cr'
  arglag = list(fun='cr', df=4),
  group = nonyc_df$metro_state_county # makes sure I lag appropriately by metro/county
)


# crossbasis summary
sink('./results/09-no_nyc_crossbasis_summary.txt')
print(summary(nonyc_cbgam))

print('#######################################################################')
print('Dimensions of non-missing lagged matrix :')
print(dim(na.omit(nonyc_cbgam)))

sink()

# unit check on number of obs expected
nonyc_expect_n_obs <- nonyc_df %>% filter(time_since_first_death >= 0) %>% nrow()
# check that number of rows line up with dimensions of matrix
if (nonyc_expect_n_obs == dim(na.omit(nonyc_cbgam))[1]) {
  print('Observations same length in crossbasis and dataframe')
} else {
  print('Observations different; check crossbasis object and dataframe')
}

# penalize the crossbasis splines
nonyc_cbgamPen <- cbPen(nonyc_cbgam) 

# fit gam with dlnm modeling first; this one uses the dlmn matrix; 
# other version of model treats them as fixed variables to allow for use of predict.gam
# this was done as the dlmn matrix messes up some functionality of predict
nonyc.dlnm.main.gam <- gam(
  daily_deaths ~
    nonyc_cbgam + # this is the lag matrix 
    s(time_since_first_death) + 
    s(time_since_first_death, metro_state_county, bs=c("fs")) + # global smoother
    s(PC1, bs="cr", k=5) +
    s(PC2, bs="cr", k=5) +
    s(PC3, bs="cr", k=5) +
    s(PC4, bs="cr", k=5) +
    offset(log(population_v051)), 
  data=nonyc_df,
  paraPen=list(nonyc_cbgam=nonyc_cbgamPen),# this applies the penalty to the lagged matrix
  family="quasipoisson",
  method="REML"
)


# Model general summaries and plots 
# check gam summary
summary(nonyc.dlnm.main.gam)

# saving gam summary of dlmn matrix
sink("./results/09-no_nyc_dlnm_gam_summary.txt")
print(summary(nonyc.dlnm.main.gam))
sink()

# save partial dependency plot of smooth terms 
# save plot
pdf(
  file = './results/09-no_nyc_dlmn_gam_partial_dependcy_plot.pdf'
  , width = 8
  , height = 6
)
plot(nonyc.dlnm.main.gam, pages = 1, seWithMean = TRUE)
dev.off()

# Association between COVID mortality and PC1 plots ----------------------------
# (using partial plot output); 'save' plot object that contains partial dependencies 
# option for residuals to view fit
pdf(
  file = './results/09-no_nyc_dlmn_gam_partial_dependcy_plot_residuals.pdf'
  , width = 8
  , height = 6
)
nonyc_save <- plot(nonyc.dlnm.main.gam, pages=1, residuals=TRUE, seWithMean = TRUE)
dev.off()

# limit to complete data used in model so that vector of predictions from plot
# 'save' object match dataframe
nonyc_complete_data <- nonyc_df %>% 
  filter(time_since_first_death >= 0)

# plot
nonyc_pc1_plot <- ggplot() +
  geom_point(
    # join in residuals with complete data
    data = cbind(nonyc_complete_data, predicted=nonyc_save[[3]]$p.resid) %>% 
      #mutate(county_state=paste(county,state,sep=", ")) %>%
      group_by(metro_state_county, metro_area, county, state) %>% 
      summarise(PC1=mean(PC1),predicted=mean(predicted)) %>%
      left_join(states, by=c("state"="state.name")) %>%
      mutate(county_state=paste(county,state.abb,sep=", ")),
    aes(x = PC1, y = exp(predicted), shape=metro_area),size=2) +
  # pc1 fit
  geom_line(
    data = data.frame(cbind(x=nonyc_save[[3]]$x, y=exp(nonyc_save[[3]]$fit))),
    aes(x = x , y = V2),col="red") +
  # 95% CI around estimate
  geom_ribbon(
    data = data.frame(
      cbind(x = nonyc_save[[3]]$x, y = exp(nonyc_save[[3]]$fit), 
            lower95 = exp(nonyc_save[[3]]$fit - 2*nonyc_save[[3]]$se),
            upper95 = exp(nonyc_save[[3]]$fit + 2*nonyc_save[[3]]$se)
      )
    ),
    aes(x = x, ymin = V3, ymax = V4), fill='red', alpha = 0.1) +
  # labels
  geom_text_repel(
    data = cbind(nonyc_complete_data, predicted=nonyc_save[[3]]$p.resid) %>% 
      group_by(metro_state_county, metro_area, county, state) %>% 
      summarise(PC1=mean(PC1),predicted=mean(predicted)) %>%
      left_join(data.frame(state.name,state.abb), by=c("state"="state.name")) %>%
      mutate(county_state=paste(county,state.abb,sep=", ")),
    aes(x = PC1, y = exp(predicted), label = county_state), 
    size=5, direction="both", force=40, segment.size=0.25, 
    segment.linetype="dashed"
  ) +
  theme_classic() +
  ylab("Relative Daily Mortality Rate (RR)") +
  xlab("Health and Wealth") +
  scale_shape_manual("Metropolitan area", values=c(15:18,8,6,3)) +
  geom_hline(yintercept=1, linetype="dashed", col="red") +
  coord_cartesian(ylim=c(0,8)) +
  theme(axis.text = element_text(size=12), axis.title = element_text(size=14)) +
  theme(legend.text = element_text(size=12), legend.title=element_text(size=14))

# save plot
ggsave(
  filename = './results/09-no_nyc_dlmn_gam_pc1_plot.pdf',
  plot = nonyc_pc1_plot,
  height = 10,
  width = 16,
  unit = 'in'
)

# Exluding NYC: Crossbasis 3d plot ---------------------------------------------

nonyc_dlnm_pred <- crosspred(
  nonyc_cbgam, # cross basis object 
  nonyc.dlnm.main.gam, # gam model with dlmn matrix 
  at=10:-80, # discussion on range to model
  cen = 0, # cender at 0
  ci.level=0.95 # modeling at .95 CI
)


# save plot
pdf(
  file = './results/09-no_nyc_mobility_crossbasis.pdf'
  , width = 8
  , height = 8
)
plot(
  nonyc_dlnm_pred , 
  xlab= "% Change Rec/Retail Mobility (centered 0 mobility)", 
  zlab= "Relative Daily Death Rate", theta=400, phi=30, lphi=20,
)
dev.off()


# This is the RR for different lags at 0% mobility, excluding NYC
# Different rows give you different mobility measures
# pull out df
nonyc_rr_fits <- data.frame(nonyc_dlnm_pred$matRRfit) %>% 
  mutate(mobility = as.factor(rownames(.)))
# strip lag off days
names(nonyc_rr_fits) <- gsub("lag","",make.names(names(nonyc_rr_fits)))
# sorted mobility for plot labels
sorted_mobility_nonyc <- paste(sort(as.integer(levels(nonyc_rr_fits$mobility))))
nonyc_rr_fits$mobility <- factor(nonyc_rr_fits$mobility, levels = sorted_mobility_nonyc)
selowfits_nonyc <- data.frame(nonyc_dlnm_pred$matRRlow) %>% mutate(mobility=as.factor(rownames(.)))
sehighfits_nonyc <- data.frame(nonyc_dlnm_pred$matRRhigh) %>% mutate(mobility=as.factor(rownames(.)))
names(selowfits_nonyc) <- gsub("lag","",make.names(names(selowfits_nonyc)))
names(sehighfits_nonyc) <- gsub("lag","",make.names(names(sehighfits_nonyc)))
sorted_mobility_nonyc <- paste(sort(as.integer(levels(nonyc_rr_fits$mobility))))
selowfits_nonyc$mobility <- factor(selowfits_nonyc$mobility, levels = sorted_mobility_nonyc)
sehighfits_nonyc$mobility <- factor(sehighfits_nonyc$mobility, levels = sorted_mobility_nonyc)

# make lagged dataframe for plots
nonyc_lagged_estimates_df <- nonyc_rr_fits %>%
  tidyr::gather(lag, RR, -mobility) %>%
  mutate(type="RR") %>%
  left_join(selowfits_nonyc %>% 
              tidyr::gather(lag, se_low, -mobility), by = c('mobility', 'lag')) %>%
  left_join(sehighfits_nonyc %>% 
              tidyr::gather(lag, se_high, -mobility), by = c('mobility', 'lag')) %>%
  mutate(lag=as.numeric(lag))

# Sensitivity figure excluding NYC: ----
lagged_mobility_nonyc <- ggplot(
  # subset to certain mobilities
  data = nonyc_lagged_estimates_df %>% 
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
    aes(col=mobility)
  ) + 
  geom_ribbon(
    aes(ymin=se_low,ymax=se_high, fill=mobility),alpha=0.3
  )+
  scale_color_manual(
    values=wesanderson::wes_palette("Darjeeling1",n=9,type="continuous"), 
    guide=FALSE
  ) +
  scale_fill_manual(
    values=wesanderson::wes_palette("Darjeeling1",n=9,type="continuous"),
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
  scale_y_continuous(expand=c(0,0), limits=c(0.7,1.5)) +
  theme(strip.background = element_blank()) +
  theme(legend.position = "bottom") 

# save plot; paper figure
ggsave(
  plot = lagged_mobility_nonyc,
  filename = './results/07-lagged_mobility_nonyc.pdf',
  height = 8,
  width = 10,
  unit = 'in'
)


# COMBINE both models
nonyc_lagged_estimates_df$model <- "Excluding NYC"
lagged_estimates_df$model <- "Main model"

# DLNM Slice plot with and without NYC -----------------------------------------
# combine the main model lagged results with the results exlcuding NYC
lagged_estimates_bothmods <- rbind(lagged_estimates_df,nonyc_lagged_estimates_df)

# mobility lagged plot overlaying main model with no_nyc model mobility results
sensivity_lagged_plot <- ggplot(
  # subset to certain mobilities
  data = lagged_estimates_bothmods %>% 
    filter(
      mobility %in% c("-80","-70","-60","-50", "-25","-10", "0", "5", '10')
      ) %>%
    mutate(
      # order mobility levels
      mobility = factor(
        mobility, levels=c("-80","-70","-60","-50", "-25","-10", "0", "5", '10')
        )
      ),
  aes(x = lag, y = RR, group=model)
  ) +
  geom_line(
    aes(col=mobility,linetype=model,size=model)
    ) + 
  scale_size_manual(values=c(0.65,1)) +
  geom_ribbon(
    aes(ymin=se_low,ymax=se_high, fill=mobility, alpha=model, linetype=model)
    )+
  scale_linetype_manual(values=c("dashed","solid"))+
  scale_alpha_manual(values=c(0.15,0.3)) +
  scale_color_manual(
    values=wesanderson::wes_palette("Darjeeling1",n=9,type="continuous"), 
    guide=FALSE
    ) +
  scale_fill_manual(
    values=wesanderson::wes_palette("Darjeeling1",n=9,type="continuous"),
    guide=FALSE
    ) +
  geom_hline(yintercept=1, color='black', linetype='dotted', alpha=0.5) +
  xlab('Days After') +
  ylab('Relative Daily Death Rate') +
  facet_wrap(
    ~ mobility, 
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
  theme_minimal() + 
  theme(legend.position = "bottom")


# save plot; paper figure
ggsave(
  plot = sensivity_lagged_plot,
  filename = './results/09-fig_3.pdf',
  height = 8,
  width = 10,
  unit = 'in'
)

# Specific estimates to report in paper
rr_estimates_paper_bothmods <- lagged_estimates_bothmods %>% 
  filter(mobility %in% c("-50", "10") & lag %in% c(15,30)) %>% 
  dplyr::select(-type) %>% 
  rename(
    relative_death_rate = RR, 
    lower_95 = se_low,
    upper_95 = se_high
  ) %>% 
  mutate_at(
    vars(relative_death_rate:upper_95),
    list(~round(as.numeric(.), 3))
  )

# save as csv of text estimates at different mobility days
print(rr_estimates_paper_bothmods)

write_csv(rr_estimates_paper_bothmods, './results/09-dlnm_effect_estimates_to_report.csv')


# SENSITIVITY ANALYSIS: TRUNCATED TIME -----------------------------------------
# This sensitivity analysis truncates all county time series to the max time
# of the county wiht the minimum amount of time.

# Find the county time length with the shortest amount of time
time_length <- analysis_df %>% 
  group_by(metro_state_county) %>%
  summarize(max_time = max(time_since_first_death)) %>% 
  ungroup() %>% 
  summarize(time_length = min(max_time)) %>% 
  pull()
# print time to truncate all counties to
print(time_length)  

# make fixed time series
fixed_ts_df <- analysis_df %>%
  # make sure it's ungrouped
  ungroup() %>% 
  filter(time_since_first_death <= time_length) %>%
  mutate(metro_state_county = as.factor(metro_state_county))

# build crossbasis for fixed time series
fix_ts_cbgam <- crossbasis(
  fixed_ts_df$retail_and_recreation_percent_change_from_baseline, 
  lag = 30,
  argvar = list(fun='cr', df=4), # using penalized spline ps; best performer in gasparini 2017
  arglag = list(fun='cr', df=4),
  group = fixed_ts_df$metro_state_county # makes sure I lag appropriately by metro/county
)

summary(fix_ts_cbgam)

# penalize the crossbasis splines
fix_ts_cbgamPen <- cbPen(fix_ts_cbgam) 

# unit check on number of obs expected
fix_ts_expect_n_obs <- fixed_ts_df %>% filter(time_since_first_death >= 0) %>% nrow()
# check that number of rows line up with dimensions of matrix
if (fix_ts_expect_n_obs == dim(na.omit(fix_ts_cbgam))[1]) {
  print('Observations same length in crossbasis and dataframe')
} else {
  print('Observations different; check crossbasis object and dataframe')
}

# fit gam with dlnm for fixed time series
fixts.gam <- gam(
  daily_deaths ~
    fix_ts_cbgam + # this is the lag matrix
    s(time_since_first_death) + 
    s(time_since_first_death, metro_state_county, bs=c("fs")) + # global smoother
    s(PC1, bs="cr", k=5) +
    s(PC2, bs="cr", k=5) +
    s(PC3, bs="cr", k=5) +
    s(PC4, bs="cr", k=5) +
    offset(log(population_v051)), 
  data=fixed_ts_df,
  paraPen=list(fix_ts_cbgam=fix_ts_cbgamPen ),# this applies the penalty to the lagged matrix
  family="quasipoisson",
  method="REML"
)

# summary of fixed time series gam
summary(fixts.gam)


# saving gam summary of dlmn matrix
sink("./results/09-fixed_timeseries_dlnm_gam_summary.txt")
print(summary(fixts.gam))
sink()

# save partial dependency plot of smooth terms 
pdf(
  file = './results/09-fixed_timeseries_dlmn_gam_partial_dependcy_plot.pdf',
  width = 8,
  height = 6
)
plot(fixts.gam, pages = 1, seWithMean = TRUE)
dev.off()

# adding 3d tensor plot
fixed_ts_pred <- crosspred(
  fix_ts_cbgam, # cross basis object 
  fixts.gam, # gam model with dlmn matrix 
  at=10:-80, # discussion on range to model
  cen = 0, # cender at 0
  ci.level=0.95 # modeling at .95 CI
)


# save plot
pdf(
  file = './results/09-fixed_timeseries_mobility_crossbasis.pdf'
  , width = 8
  , height = 8
)
plot(
  fixed_ts_pred, 
  xlab= "% Change Rec/Retail Mobility (centered 0 mobility)", 
  zlab= "Relative Daily Death Rate", theta=400, phi=30, lphi=20,
)
dev.off()

# SENSIVITY ANALYSIS: ALTERNATE LAG MODEL --------------------------------------
# alternate 'constrained' lag model approach where the median weekly value
# for mobility is used

weekly_median_lag <- analysis_df %>% 
  select(fips, date, retail_and_recreation_percent_change_from_baseline) %>% 
  mutate(wk = lubridate::week(date)) %>% 
  group_by(fips, wk) %>% 
  summarize(
    retail_rec_wk_lag0 = median(retail_and_recreation_percent_change_from_baseline)
  ) %>% 
  ungroup() %>% 
  group_by(fips) %>% 
  mutate(
    retail_rec_wk_lag1 = lag(retail_rec_wk_lag0, n = 1, order_by = wk),
    retail_rec_wk_lag2 = lag(retail_rec_wk_lag0, n = 2, order_by = wk),
    retail_rec_wk_lag3 = lag(retail_rec_wk_lag0, n = 3, order_by = wk),
    retail_rec_wk_lag4 = lag(retail_rec_wk_lag0, n = 4, order_by = wk)
  )


# add weekly median lags to all counties mobility
analysis_df_simplelag <- analysis_df %>% 
  ungroup() %>% 
  mutate(wk = lubridate::week(date),
         metro_state_county = as.factor(metro_state_county)) %>% 
  left_join(weekly_median_lag, by = c('fips', 'wk')) %>% 
  filter(time_since_first_death >= 0) 


# trying something
simplelag.gam <- gam(
  daily_deaths ~
    s(time_since_first_death) + 
    s(time_since_first_death, metro_state_county, bs=c("fs")) + # global smoother
    s(PC1, bs="cr", k=5) +
    s(PC2, bs="cr", k=5) +
    s(PC3, bs="cr", k=5) +
    s(PC4, bs="cr", k=5) +
    s(retail_rec_wk_lag0, bs="cr", k=5) +
    s(retail_rec_wk_lag1, bs="cr", k=5) +
    s(retail_rec_wk_lag2, bs="cr", k=5) +
    s(retail_rec_wk_lag3, bs="cr", k=5) +
    s(retail_rec_wk_lag4, bs="cr", k=5) +
    offset(log(population_v051)), 
  data=analysis_df_simplelag,
  family="quasipoisson",
  method="REML"
)


summary(simplelag.gam)

# print summary of weelkly median lag gam 
sink('./results/09-weekly_median_lag_gam_summary.txt')
print(summary(simplelag.gam))
sink()

# save partial dependency plot
pdf(
  file = './results/09-weekly_median_gam_partial_dependcy_plot.pdf',
  width = 8,
  height = 6
)
plot(simplelag.gam, pages = 1, seWithMean = TRUE)
dev.off()

