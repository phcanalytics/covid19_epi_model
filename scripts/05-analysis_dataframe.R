# ------------------------------------------------------------------------------
# Title: Make smaller study area dataframe based on counties of interest
# Author: Sarah McGough and Ryan Gan
# Purpose: Builds dataframe for analysis in GAM models; this script runs after 
# PCA in 04
# ------------------------------------------------------------------------------

# library 
library(tidyverse)
library(yaml)
library(wesanderson)

# Read data --------------------------------------------------------------------
# load config file
config <- read_yaml('./scripts/config.yaml')

# read in pc
pc_df <- read_csv(
  file = './data/04-study_fips_pca.csv'
  ) %>%
  # using only some variables
  select(fips, metro_area, metro_state_county, PC1:PC4)
  
# read in master time series dataframe 
master_ts <- read_csv(
  file = './data/03-nyt_us_county_timeseries_daily.csv'
)

# read nyt time series and limit to study fips
analysis_df <- master_ts %>% 
  # filter/subset to study fips
  filter(fips %in% config$study_fips) %>% 
  left_join(pc_df, by = 'fips') %>% 
  # study window ends at date 2020-05-13; for reproducibility 
  filter(date <= config$study_window$end_date) %>% 
  # impute early portion of time series using imputeTS function
  arrange(fips,date) %>%
  group_by(fips) %>%
  mutate_at(
    vars(
      retail_and_recreation_percent_change_from_baseline,
      grocery_and_pharmacy_percent_change_from_baseline,
      parks_percent_change_from_baseline,
      transit_stations_percent_change_from_baseline,
      workplaces_percent_change_from_baseline,
      residential_percent_change_from_baseline
    ),
    list(
      ~ imputeTS::na_ma(., k = 5, weighting = "linear")
    )
  ) 

# save study df
write_csv(analysis_df, './data/05-analysis_df.csv')


# Saving time series plot by county ---------------------------------------

p_ts_county <- analysis_df %>%
  filter(time_since_first_death>=0) %>%
  ggplot(aes(time_since_first_death, daily_deaths_rollavg7)) +
  geom_line(aes(col=metro_area)) +
  scale_color_manual("Metro area", values=wes_palette("Darjeeling1",n=13,type="continuous")) +
  facet_wrap(~metro_state_county, scales="free") +
  theme_classic() +
  xlab("Days since first death") +
  ylab("Daily deaths (Rolling 7-day average)") +
  theme(strip.text = element_text(size=7))

# png version of figure 2 for paper; 300 dpi for publication quality
png("./results/05-time_series_county.png",width=15, height=8, units="in", res=300)
p_ts_county
dev.off()

# saving a pdf version as well
pdf("./results/05-time_series_county.pdf",width=15, height=8)
p_ts_county
dev.off()


# Saving correlation matrix of mobility data ------------------------------
# correlation matrix of variables
pdf(
  file = './results/05-mobility_corr_matrix.pdf'
  , width = 8
  , height = 8
)

corrplot(cor(analysis_df[,c("retail_and_recreation_percent_change_from_baseline",
                            "grocery_and_pharmacy_percent_change_from_baseline",
                            "parks_percent_change_from_baseline",
                            "transit_stations_percent_change_from_baseline",
                            "workplaces_percent_change_from_baseline"
                            )],use = "complete.obs"), type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, tl.cex=0.5)

dev.off()



# Storing summary information --------------------------------------------------
sink("./results/05-analysis_df_meta_info.txt")
print('#######################################################################')
# print out names of study areas
print('Study counties:')

print(unique(analysis_df$metro_state_county))

print('#######################################################################')
# print out study date window
print('Study date summary:')
print(summary(analysis_df$date))

# print dims of dataframe
print('#######################################################################')
print('n observations and columns: ')
print(dim(analysis_df))

sink()

# Exlusion of New York City sensivity dataframe --------------------------------
# read in pc
nonyc_pc_df <- read_csv(
  file = './data/04-study_fips_nonyc_pca.csv'
  ) %>%
  # using only some variables
  select(fips, metro_area, metro_state_county, PC1:PC4)

# read nyt time series and limit to study fips
nonyc_df <- master_ts %>%
  # filter/subset to study fips
  filter(fips %in% config$study_fips) %>% 
  right_join(nonyc_pc_df, by = 'fips') %>% 
  # study window ends at date 2020-05-13; for reproducibility 
  filter(date <= config$study_window$end_date) %>% 
  # impute early portion of time series using imputeTS function
  arrange(fips,date) %>%
  group_by(fips) %>%
  mutate_at(
    vars(
      retail_and_recreation_percent_change_from_baseline,
      grocery_and_pharmacy_percent_change_from_baseline,
      parks_percent_change_from_baseline,
      transit_stations_percent_change_from_baseline,
      workplaces_percent_change_from_baseline,
      residential_percent_change_from_baseline
    ),
    list(
      ~ imputeTS::na_ma(., k = 5, weighting = "linear")
    )
  ) 

# quick unit check; make sure new york is not present
if (!('New York' %in% unique(nonyc_df$state))) {
  'New York exlcuded from sensitivity dataframe'
} else {
  'Check, New York still in sensivity dataframe'
}

# write dataframe
write_csv(nonyc_df, './data/05-sensitivity_analysis_nonyc_df.csv')
