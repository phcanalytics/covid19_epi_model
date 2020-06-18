# ------------------------------------------------------------------------------
# Title: Results from boostrapped variance estimates
# Author: Sarah McGough and Ryan Gan
# Purpose: Uses list objects to again plot uncertainy estimates main GAM
# ------------------------------------------------------------------------------

# load tidyverse library
library(tidyverse)
# libraries used in plots
library(ggrepel) # for cleaner plot labels
library(reshape2) # for plotting
library(gridExtra) # grid alignments
library(cowplot)

# Read in data for results and plots -------------------------------------------
# load boostrapped list and main model list
load(file = './data/07-boot_results_list.RData')
load(file = './data/07-main_model_list.RData')

# read in analysis dataframe for pc1 results
analysis_df <- read_csv('./data/05-analysis_df.csv')

# check lenght of boot_results_list is less than 1000; print warning
if( length(boot_results_list) < 1000 ) {
  print(paste0('Warning: Bootstrapped confidence intervals may not be reliable ',
               'with fewer than 1000 iterations'))
}

## MANUSCRIPT RESULTS AND FIGURES ## -------------------------------------------
# PC1 bootstrapped estimates

# Bias correction function for 95% percentiles for principle components 1

# Find boot estimates that are under the original predicted/expected estimate
diff_list <- lapply(boot_results_list, function(x){
  # test if for each given value on the pc1 x predictor, it's less than estimated
  ifelse(x$pc1$plotme$ystar < main_model_list$pc1$plotme$ystar, 1, 0) 
}) 

# find probability for each given estimate that the boot estimate is under expected
prob <- apply(do.call('rbind', diff_list), 2, mean)

# extract all boot statistics from all bootstraps
boot_statistic <- do.call(
  'rbind',
  lapply(boot_results_list, function(x){
    # test if for each given value on the pc1 x predictor, it's greater than estimated
    x$pc1$plotme$ystar
  }) # end function
) 

# get bias corrected ci percentiles for each value of pc1
ci <- data.frame(
  do.call(
    'rbind',
    lapply(1:length(prob), function(x){
      quantile(
        # vector of expected y star
        boot_statistic[,x], 
        probs = c(
          pnorm((2*qnorm(prob[x])) - 1.96),
          pnorm((2*qnorm(prob[x])) + 1.96)
        ))} # end function for quantile bias correction
    )
  )
) 

# assign new names 
colnames(ci) <- c('lower_95', 'upper_95')
# add pc1 value to ci dataframe
ci$pc1 <- boot_results_list[[1]]$pc1$plotme$x

# pc original estimates
pc1_orig <- data.frame(main_model_list$pc1$plotme)

# county-specifc pc1s
county_pc1 <- main_model_list$county_pc1 %>% 
  ungroup() %>% 
  mutate(metro_area = factor(metro_area))

# limit analysis_df to complete_data to get certain pc estimates
# to time_since_first_death >= 0; note this is the same data modeled in gam
complete_data <- analysis_df %>% 
  filter(time_since_first_death >= 0)

# Figure 2B. Visualize certain PC1 variables -----------------------------------

pc1_plot <- ggplot() +
  geom_hline(yintercept=1, linetype="dashed", col="dodgerblue2") +
  geom_line(data = pc1_orig, aes(x=x, y=exp(ystar)), color = 'darkblue', size = 0.8) +
  geom_ribbon(
    data = ci, 
    aes(x=pc1, ymin = exp(lower_95), ymax = exp(upper_95)), 
    fill = 'lightblue', alpha = 0.4
  ) +
  geom_point(
    data = county_pc1, 
    aes(x = PC1, y = exp(predicted), shape=metro_area), size=2
  ) +
  scale_shape_manual("Metropolitan area", values=c(15:18,8,6,3)) +
  # labels
  geom_text_repel(
    data = county_pc1,
    aes(x = PC1, y = exp(predicted), label = county_state),
    size=3.5, direction="both", force=10, segment.size=0.25,
    segment.linetype="dashed"
  ) +
  ggtitle("A") +
  theme_classic() +
  ylab("Relative Daily Mortality Rate (RR)") +
  xlab("Health and Wealth") +
  theme(axis.text = element_text(size=12), axis.title = element_text(size=14)) +
  theme(legend.text = element_text(size=12), legend.title=element_text(size=14))

# set up principle components plots
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

# smoking plot
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

# png version of figure 2 for paper; 300 dpi for publication quality
png("./results/08-fig_2.png",width=12, height=7, units="in", res=300)
grid.arrange(pc1_plot, p_smoking, p_income, layout_matrix=fig2_layout)
dev.off()

# saving a pdf version as well
pdf("./results/08-fig_2.pdf",width=12, height=7)
grid.arrange(pc1_plot, p_smoking, p_income, layout_matrix=fig2_layout)
dev.off()

# Figure 2: all bootstrapped estimates version ---------------------------------
# purrr::map_dfr bind
pc1_df <- boot_results_list %>%
  map_dfr(~ .x$pc1$plotme)

# pc1 plot using all bootstraps
pc1_all_boots_plot <- ggplot() +
  geom_line(
    data = pc1_df, 
    aes(x=x, y = exp(ystar), group = iteration), 
    color = 'lightblue', alpha = 0.1, size = 0.1
  ) +
  geom_line(data = pc1_orig, aes(x=x, y=exp(ystar)), color = 'darkblue', size = 0.8) +
  geom_point(
    data = county_pc1, 
    aes(x = PC1, y = exp(predicted), shape=metro_area), size=2
  ) +
  scale_shape_manual("Metropolitan area", values=c(15:18,8,6,3)) +
  # labels
  geom_text_repel(
    data = county_pc1,
    aes(x = PC1, y = exp(predicted), label = county_state),
    size=3.5, direction="both", force=10, segment.size=0.25,
    segment.linetype="dashed"
  ) +
  ggtitle("A") +
  theme_classic() +
  ylab("Relative Daily Mortality Rate (RR)") +
  xlab("Health and Wealth") +
  theme(axis.text = element_text(size=12), axis.title = element_text(size=14)) +
  theme(legend.text = element_text(size=12), legend.title=element_text(size=14))

# all bootstrap i version
# png version of figure 2 for paper; 300 dpi for publication quality
png("./results/08-fig_2_all_boots.png",width=12, height=7, units="in", res=300)
grid.arrange(pc1_all_boots_plot, p_smoking, p_income, layout_matrix=fig2_layout)
dev.off()

# saving a pdf version as well
pdf("./results/08-fig_2_all_boots.pdf",width=12, height=7)
grid.arrange(pc1_all_boots_plot, p_smoking, p_income, layout_matrix=fig2_layout)
dev.off()



# Figure 3 DLNM percentile figures ---------------------------------------------

# NOTE: No percentile correction based on visual inspection of bootstrapped 

# DLNM results
dlnm_df <- boot_results_list %>%
  map_dfr(~ .x$dlnm) %>%
  filter(mobility %in% c(-80, -70, -60, -50, -25, -10, 0, 5, 10))

# dlnm original expected fit
dlnm_data_orig <- main_model_list$dlnm %>%
  mutate_all(as.numeric) %>% 
  filter(mobility %in% c(-80, -70, -60, -50, -25, -10, 0, 5, 10))

### NOTE: THIS IS THE FOLLOWING PROCESS TO CALCULATE NEW PERCENTILES GIVEN 
### MOBILITY AND LAGGED DAY
### APPROACH IS DIFFERENT GIVEN THE WAY WE STORED THE RESULTS
### COMMENTED OUT AS WE DECIDED TO GO WITH PERCENTILE METHOD

# Bias correction with tidyverse; first find each new probability for lower 
# and upper 95% CI for each mobility and lag value
# dlnm_prob <- dlnm_df %>% 
#   left_join(
#     dlnm_data_orig %>% 
#       select(mobility, lag_day, fit) %>% 
#       rename(fit_orig = fit), 
#     by = c('mobility', 'lag_day')) %>% 
#   mutate(low_indicator = ifelse(fit < fit_orig, 1, 0)) %>% 
#   # calculate percentile of boot estimates lower than original fit 
#   # by mobility and lag day 
#   group_by(mobility, lag_day) %>% 
#   summarize(prob = mean(low_indicator)) %>% 
#   # find new quantiles to draw from for each mobilitiy lag estimate
#   ungroup() %>% 
#   mutate(
#     lower_prob = pnorm((2*qnorm(prob)) - 1.96),
#     upper_prob = pnorm((2*qnorm(prob)) + 1.96)
#   )

# find Confidence intervals 
dlnm_bca_ci <- do.call(
  'rbind', 
  apply(
    expand.grid(unique(dlnm_df$mobility), unique(dlnm_df$lag_day)),
    1, # rowwise 
    function(x) {
      # subset specific estimates for mobility and lag day
      boot_est <- dlnm_df %>% 
        filter(mobility == x[1] & lag_day == x[2])
      
      # subset quantiles from dlnm prob; commented out for standard percentile method
      # probs_to_find <- dlnm_prob %>% 
      #   filter(mobility == x[1] & lag_day == x[2])
      
      # calculate quantile
      new_ci <- quantile(
        boot_est$fit, 
        #probs = c(probs_to_find$lower_prob, probs_to_find$upper_prob) # use this with above script for bca boot 
        probs = c(0.025, 0.975) # standard percentile boot
      )
      
      # return new cis
      return(
        data.frame(
          mobility = x[1], lag_day = x[2], lower_95 = new_ci[1], upper_95 = new_ci[2])
      )
    } # end function to find bias corrected estimates
  )) 

# join 95% percentile CIs to plot
dlnm_data_plot <- dlnm_data_orig %>% 
  left_join(dlnm_bca_ci, by = c('mobility', 'lag_day')) %>% 
  mutate(mobility = factor(
    mobility, 
    levels=c("-80","-70","-60","-50", "-25","-10", "0", "5", '10'))
  )

# dlnm plot for paper using percentile method
dlnm_plot <- ggplot() +
  geom_line(
    data=dlnm_data_plot, 
    aes(x=lag_day, y=rr, color = mobility), 
    size = 0.5
  ) +
  geom_ribbon(
    data = dlnm_data_plot,
    aes(x=lag_day, ymin = exp(lower_95), ymax = exp(upper_95), fill = mobility),
    alpha = 0.3
  ) + 
  facet_wrap(~mobility) +
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
  theme_classic() +
  scale_x_continuous(expand=c(0,0), limits=c(0,30)) +
  scale_y_continuous(expand=c(0,0), limits=c(0.7,1.5)) +
  theme(strip.background = element_blank(),
        panel.spacing = unit(1, "lines"),
        axis.text=element_text(size=8),
        axis.title=element_text(size=10))


# save plot pdf version
ggsave(
  plot = dlnm_plot,
  filename = './results/08-fig_3.pdf',
  height = 8,
  width = 10,
  unit = 'in'
)

# save png version for publication
ggsave(
  plot = dlnm_plot,
  filename = './results/08-fig_3.png',
  height = 8,
  width = 10,
  unit = 'in',
  dpi = 300
)


# DLNM estimates reported in text ----------------------------------------------
# Specific estimates to report in paper
rr_estimates_paper <- dlnm_data_plot %>% 
  filter(mobility %in% c("-80", "-50", "10") & lag_day %in% c(0,15,30)) %>% 
  select(mobility, lag_day, fit, lower_95, upper_95) %>% 
  # exp the lower bounds
  mutate_at(vars(fit, lower_95, upper_95), ~round(exp(.),2)) %>% 
  rename(rr = fit, perc_lower_95 = lower_95, perc_upper_95 = upper_95) %>% 
  arrange(mobility, lag_day)

# save as csv of text estimates at different mobility days
print(rr_estimates_paper)

write_csv(rr_estimates_paper, './results/08-dlnm_effect_estimates_to_report.csv')

# Fig 3: All boots version -----------------------------------------------------

dlnm_all_boots_plot <- ggplot() +
  geom_line(
    data = mutate(dlnm_df,
      mobility = factor( # make sure mobility is a factor wtih same levels
        mobility,
        levels=c("-80","-70","-60","-50", "-25","-10", "0", "5", '10')
        )
      ), # end mutate,
    aes(x=lag_day, y=rr, group = iteration, color = mobility),
    size = 0.05, alpha = 0.1
    ) +
  geom_line(
    data=dlnm_data_plot, 
    aes(x=lag_day, y=rr), 
    size = 0.8,  color = 'black', linetype = 'dashed'
  ) +
  facet_wrap(~mobility) +
  scale_color_manual(
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
  theme_classic() +
  scale_x_continuous(expand=c(0,0), limits=c(0,30)) +
  scale_y_continuous(expand=c(0,0), limits=c(0.7,1.5)) +
  theme(strip.background = element_blank(),
        panel.spacing = unit(1, "lines"),
        axis.text=element_text(size=8),
        axis.title=element_text(size=10))

# save plots as pdf and png
# save plot pdf version
ggsave(
  plot = dlnm_all_boots_plot,
  filename = './results/08-fig_3_all_boots.pdf',
  height = 8,
  width = 10,
  unit = 'in'
)

# save png version for publication
ggsave(
  plot = dlnm_all_boots_plot,
  filename = './results/08-fig_3_all_boots.png',
  height = 8,
  width = 10,
  unit = 'in',
  dpi = 300
)

# PLOTS OF MAIN ESTIMATE AND ALL BOOTSTRAPPED ITERATIONS -----------------------
# ADDITIONAL PLOTS FROM GAM MODEL 
# These are plots of each boot iteration and the original model fit.


# Global temporal trend of COVID19 deaths --------------------------------------
temp_df <- boot_results_list %>%
  map_dfr(~ .x$temporal$plotme)

# pc original
temp_orig <- data.frame(main_model_list$temporal$plotme)

temp_boot_plot <- ggplot() +
  geom_line(data = temp_df, aes(x=x, y=exp(ystar), group=iteration, color='Bootstrap'),
            alpha = 0.4, size = 0.1) +
  geom_line(data=temp_orig, aes(x=x, y=exp(ystar), color = 'Original'), size = 0.8) +
  geom_hline(aes(yintercept = 1), color = 'red', linetype = 'dashed') +
  scale_color_manual('', values = c('lightblue', 'darkblue')) +
  ylab('Relative Daily COVID19 Death Rate') +
  xlab('Time Since First Death') +
  theme_minimal() +
  theme(legend.position = 'bottom')


# save plot 
ggsave(
  filename = './results/08-temporal_all_boots.pdf',
  plot = temp_boot_plot,
  height = 8,
  width = 8,
  unit = 'in'
)


# PC2 --------------------------------------------------------------------------

# PC2 bootstrapped estimates
# purrr::map_dfr bind
pc2_df <- boot_results_list %>%
  map_dfr(~ .x$pc2$plotme)

# pc original
pc2_orig <- data.frame(main_model_list$pc2$plotme)

pc2_boot_plot <- ggplot() +
  geom_line(data = pc2_df, aes(x=x, y=exp(ystar), group=iteration, color='Bootstrap'),
            alpha = 0.4, size = 0.1) +
  geom_line(data=pc2_orig, aes(x=x, y=exp(ystar), color = 'Original'), size = 0.8) +
  geom_hline(aes(yintercept = 1), color = 'red', linetype = 'dashed') +
  scale_color_manual('', values = c('lightblue', 'darkblue')) +
  ylab('Relative Daily COVID19 Death Rate') +
  xlab('PC2') +
  theme_minimal() +
  theme(legend.position = 'bottom')


# save plot 
ggsave(
  filename = './results/08-pc2_all_boots.pdf',
  plot = pc2_boot_plot,
  height = 8,
  width = 8,
  unit = 'in'
)
sessionInfo()
pc2_boot_plot
# PC3 bootstrapped estimates ---------------------------------------------------
# purrr::map_dfr bind
pc3_df <- boot_results_list %>%
  map_dfr(~ .x$pc3$plotme)

pc3_orig <- data.frame(main_model_list$pc3$plotme)

pc3_boot_plot <- ggplot() +
  geom_line(data = pc3_df, aes(x=x, y=exp(ystar), group=iteration, color='Bootstrap'),
            alpha = 0.4, size = 0.1) +
  geom_line(data=pc3_orig, aes(x=x, y=exp(ystar), color = 'Original'), size = 0.8) +
  geom_hline(aes(yintercept = 1), color = 'red', linetype = 'dashed') +
  scale_color_manual('', values = c('lightblue', 'darkblue')) +
  ylab('Relative Daily COVID19 Death Rate') +
  xlab('PC3') +
  theme_minimal() +
  theme(legend.position = 'bottom')


# save plot 
ggsave(
  filename = './results/08-pc3_all_boots.pdf',
  plot = pc3_boot_plot,
  height = 8,
  width = 8,
  unit = 'in'
)


# PC4 bootstrapped estimates ---------------------------------------------------
# purrr::map_dfr bind
pc4_df <- boot_results_list %>%
  map_dfr(~ .x$pc4$plotme)

# pc original
pc4_orig <- data.frame(main_model_list$pc4$plotme)

pc4_boot_plot <- ggplot() +
  geom_line(data = pc4_df, aes(x=x, y=exp(ystar), group=iteration, color='Bootstrap'),
            alpha = 0.4, size = 0.1) +
  geom_line(data=pc4_orig, aes(x=x, y=exp(ystar), color = 'Original'), size = 0.8) +
  geom_hline(aes(yintercept = 1), color = 'red', linetype = 'dashed') +
  scale_color_manual('', values = c('lightblue', 'darkblue')) +
  ylab('Relative Daily COVID19 Death Rate') +
  xlab('PC4') +
  theme_minimal() +
  theme(legend.position = 'bottom')


# save plot 
ggsave(
  filename = './results/08-pc4_all_boots.pdf',
  plot = pc4_boot_plot,
  height = 8,
  width = 8,
  unit = 'in'
)


