# ------------------------------------------------------------------------------
# Title: Script to download data necessary for building the time sereis
# Author: Ryan Gan and Sarah McGough
# Purpose: To download county level data. Furture steps should automate download.
# 
# NOTES: Some files were easier to download manually and place in the repo 
# than to write a script to download and unpack; e.g. shapefile county boundries
# ------------------------------------------------------------------------------

# Static data (data that won't change day to day) 

static_files <- c('cdc_cancer.csv', 'cms_comorbidity.csv', 
                 'rwj-county_ranking-2020.csv', 'county_fips.csv', 
                 'county_pop_density.csv', 'cb_2018_us_county_5m') 

# check if all necessary static files are in repo
if (all(static_files %in% list.files('./data/source_data/'))){
  print('All static files necessary for analysis found in "./data/source_data/"')
} else {
  print('Not all data present in current repo, static data should be available on github repo')

  print('Downloading RWJ data')
  # Robert Wood Johnson County Data ----------------------------------------------
  download.file(
    # path to rwj county ranking data
    "https://www.countyhealthrankings.org/sites/default/files/media/document/analytic_data2020.csv",
    destfile = "./data/source_data/rwj-county_ranking-2020.csv"
  )

} # end else run to collect static data


# Time varying data (data that will change day to day or week to week) ---------

# NY Times COVID Data ----------------------------------------------------------
# download us counties file
download.file(
  # path to counties csv
  "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv",
  destfile = "./data/source_data/nyt-us-counties.csv", 
  method = "curl"
)

# Google Mobility Data ---------------------------------------------------------
download.file(
  # path to mobility data
  "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv?cachebust=911a386b6c9c230f",
  destfile = "./data/source_data/google-mobility.csv", 
  method = "curl"
)

print('Data collection complete')
