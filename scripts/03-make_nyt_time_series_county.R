# ------------------------------------------------------------------------------
# Title: NYTimes county-level timeseries data with RWJ and google mobility data.
# Author: Ryan Gan and Sarah McGough
# Notes:
# 1. Make one mapping table to map fips to county names
# ------------------------------------------------------------------------------

# library
library(tidyverse)
library(RcppRoll) # rolling averages
library(yaml)

# Read yaml config file --------------------------------------------------------
config <- read_yaml('./scripts/config.yaml')

# Read NY Times county time series ---------------------------------------------
# IMPORTANT NOTE: NYTimes has collapsed all 5 counties for NYC in to one variable
# We assign the fips code of 36061 (New York County; Manhattan) to serve 
# as the county for spatial related joins. Note that we also aggregate metrics
# such as mobility across the 5 NYC counties/boroughs.

nytimes <- read_csv('./data/source_data/nyt-us-counties.csv') %>% 
  # assign manhattan county fips to nyc
  mutate(
    fips = ifelse(county == 'New York City', '36061', fips),
    # add county tag to each county to join to mobility and RWJ data
    county = ifelse(
      state == 'Louisiana', 
      paste(county, 'Parish', sep = ' '), 
      paste(county, 'County', sep = ' ')
    )
  )

# find first case and death by state to be used later 
first_case <- nytimes %>%
  filter(cases > 0) %>%
  group_by(fips) %>%
  summarize(first_case = min(date))

# first death
first_death <- nytimes %>%
  filter(deaths > 0) %>%
  group_by(fips) %>%
  summarize(first_death = min(date))


# county_study_period
# will give warnings about row binds for coercing factor to character; this is ok
study_period <- nytimes %>% 
  group_by(state, county, fips) %>% 
  summarize(
    first_date = min(date),
    end_date = max(date)
  ) %>% 
  # find first date 30 days prior to first death
  mutate(
    start_date = first_date - 30) %>% 
  select(state, county, fips, start_date, end_date) %>% 
  rowwise() %>% 
  do(
    data.frame(
      state = as.character(.$state), 
      county = as.character(.$county),
      fips = as.character(.$fips),
      date = seq(from = .$start_date, to = .$end_date, by = '1 day')
    )
  ) %>% 
  ungroup()

# Read Google mobility ---------------------------------------------------------

# output states 
region_1_vec <- sort(unique(nytimes$state))

# output counties to subset for US; include the additional counties of NYC
region_2_vec <- c(
  unique(nytimes$county),
  'Queens County', 
  'Richmond County', 
  'Bronx County',
  'Kings County',
  'New York County'
)

# read county-level mobility
mobility <- read_csv(
  './data/source_data/google-mobility.csv', 
  # read in sub_region_2 and census_fips_code as character to avoid errors
  col_types = cols(sub_region_2 = 'c', census_fips_code = 'c')
  ) %>% 
  filter(sub_region_1 %in% region_1_vec & sub_region_2 %in% region_2_vec) %>% 
  mutate(
    date = as.Date(date, format = '%Y-%m-%d'),
    # rename sub_region_2 for NYC counties to NYC county
    sub_region_2 = ifelse(
      sub_region_1 == 'New York' &
        sub_region_2 %in% c('Queens County', 'Richmond County', 'Bronx County',
                            'Kings County', 'New York County'),
      'New York City County',
      sub_region_2)
  ) %>% 
  # aggregate nyc county
  group_by(sub_region_1, sub_region_2, date) %>% 
  summarize_at(
    vars(
      retail_and_recreation_percent_change_from_baseline:
        residential_percent_change_from_baseline
    ), 
    median # median value across nyc
  ) %>% 
  rename(
    state = sub_region_1,
    county = sub_region_2
  ) %>% 
  mutate(
    date = as.Date(date)
  )

# Read in CDC cancer variable defined by Anne-Marie ----------------------------
cdc_cancer <- read_csv(
  './data/source_data/cdc_cancer.csv'
  ) %>% 
  select(fips, cancer_age_adjusted_rate)

# Read in CMS comorbidities ----------------------------------------------------
cms_comorb <- read_csv(
  './data/source_data/cms_comorbidity.csv'
  ) %>% 
  # prevalence spelled wrong; fixing
  select(fips, copd_prevalance, hypertension_prevalence, ami_prevalence) %>%
  rename(copd_prevalence = copd_prevalance)

# Read RWJ ---------------------------------------------------------------------

rwj <- read_csv(
  './data/source_data/rwj-county_ranking-2020.csv',
  skip = 1,
  col_types = cols(.default = 'c')
  ) %>%
  # subset variables to those defined in rwj config list
  select(config$rwj$column_index) %>% 
  # convert variables to numeric
  mutate_at(
    vars(county_ranked:v057_rawvalue),
    as.numeric
    ) %>% 
  # rename with roche names
  rename_all(~config$rwj$roche_names) %>% 
  # join in cdc cancer and cms comorbidity
  left_join(cdc_cancer, by = 'fips') %>% 
  left_join(cms_comorb, by = 'fips')

# aggregate counties to join with NY Times data
nyc_rwj <- rwj %>% 
  filter(fips %in% c('36005','36047','36061', '36081','36085')) %>% 
  summarize_at(
    vars(premature_death_v001:severe_housing_cost_burden_v154,
         age_below_18_pct_v052:ami_prevalence),
    # median values except for population
    median
  ) %>% 
  # reassign new york county fips for joins
  mutate(
    fips = '36061',
    # take sum of population for new york city area
    population_v051 = (
      # population sum of nyc
      rwj %>% 
        filter(fips %in% c('36005','36047','36061', '36081','36085')) %>% 
        select(population_v051) %>% 
        pull() %>% 
        sum()
      )
    )

# append back to rwj without nyc counties
rwj_final <- rwj %>% 
  # remove some vars
  select(-state, -county, -statecode, -countycode, -year, -county_ranked) %>% 
  # exclude nyc counties
  filter(!(fips %in% c('36005','36047','36061', '36081','36085'))) %>% 
  # bind in nyc aggregates
  bind_rows(nyc_rwj)

# Principle Components Variables -----------------------------------------------
# Saving vector of variables to look at in the principle components analysis
principle_components_vars <- colnames(rwj_final)[
  # logic to exlcude column names we don't want in the pc
  which(
    !(colnames(rwj_final) %in% c('fips', 'population_v051'))
    ) # end which subset
  ]

if( length(principle_components_vars) == 35 ){
  '35 variables for PC analysis'
} else {
  'Variables do not match; check it out'
}
# overwrite pca variables; delete current values
config$pca_variables <- NULL
# append pc variables to config list and update config file
config$pca_variables$vars <- principle_components_vars

# rewrite config file with pca variables
write_yaml(config, './scripts/config.yaml')

# Read county population density -----------------------------------------------
pop_density <- read_csv('./data/source_data/county_pop_density.csv') %>% 
  select(-area)

# nyc population density
nyc_pop_density <- pop_density  %>% 
  filter(fips %in% c("36005","36047","36061","36081","36085")) %>% 
  summarize(
    population_v051 = sum(population_v051),
    land_area_km2 = sum(land_area_km2)
  ) %>% 
  # calculate population density
  mutate(
    # assign nyc counties the fips of 36061
    fips = "36061",
    pop_km2 = population_v051/land_area_km2
  ) 

# add nyc population density
pop_density <- pop_density %>%
  filter(
    !(fips %in% c("36005","36047","36061","36081","36085"))
  ) %>% 
  bind_rows(nyc_pop_density)

# Build Master Dataset ---------------------------------------------------------
# joins all above tables to a county-level time series

master_ts <- study_period %>% 
  left_join(nytimes, by = c('state', 'county', 'fips', 'date')) %>%
  group_by(fips) %>%
  mutate(
    week_day = lubridate::wday(date, label=T),
    daily_cases = cases - lag(cases, k=1, order_by = date),
    # fill in first date (missing) with cases, also some negative 
    daily_cases = ifelse(is.na(daily_cases), cases, daily_cases),
    # dates probably based on reporting bias on weekends, so I don't 
    # want negative values either, set them to 0
    daily_cases = ifelse(daily_cases < 0, 0, daily_cases),
    # rolling 7 day averages
    daily_cases_rollavg7 = roll_mean(daily_cases, n=7, align='right', fill=NA),
    # repeat same process for deaths
    daily_deaths = deaths - lag(deaths, k=1, order_by = date),
    daily_deaths = ifelse(is.na(daily_deaths), deaths, daily_deaths),
    daily_deaths = ifelse(daily_deaths < 0, 0, daily_deaths),
    daily_deaths_rollavg7 = roll_mean(daily_deaths, n=7, align='right', fill=NA)
  ) %>% 
  ungroup() %>%
  rename(cumulative_cases = cases, cumulative_deaths = deaths)  %>%     
  # join in first case and first death
  left_join(first_case, by = 'fips') %>%
  left_join(first_death, by = 'fips') %>%
  # create time since first death and case
  mutate(
    time_since_first_case = as.numeric(date - first_case), 
    time_since_first_death = as.numeric(date - first_death)
  ) %>% 
  # join in mobility 
  left_join(mobility, by = c('state', 'county', 'date')) %>% 
  # join in rwj
  left_join(rwj_final, by = 'fips') %>% 
  # join in population density; remove pop v051 as it's in rwj 
  left_join(select(pop_density, -population_v051), by = 'fips') %>% 
  arrange(state, county, date)

# file path
file_path <- './data/03-nyt_us_county_timeseries_daily.csv'
# writing time series csv 
write_csv(master_ts, path = file_path)

# messages
print('Done running script to make time series')
print(
  paste0('Saved time series for additional processing and analysis at ',
         file_path)
)

