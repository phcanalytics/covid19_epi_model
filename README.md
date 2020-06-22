# COVID19 Population-Level Epidemiologic Models and Analyses

<b>Note: This README is updated, but not every day, so some things may be out of date</b>

This project focuses on population-level analyses of COVID19 time series. Our
spatial units of interest are at the US county-level.

This repository contains all scripts used to produce this analysis. Data are linked 
and/or provided for download in agreement with the data licenses pertaining to each source.

**License:** This project is licensed under the terms of the MIT license.

**Cite:** [McGough et al. 2020 pre-print](https://www.medrxiv.org/content/10.1101/2020.06.18.20134122v1). 

### Project Data Scientists

- Sarah McGough
- Ryan Gan

# Overall Aim
The overall aim of this project is to understand COVID19 mortality at a population-level. 
Herein, we developed a generalized additive model that models the general 
response of daily COVID19 deaths over time, within specific counties, 
county-level constructs of sociodemographic characteristics, and Google mobility.

The two specific aims are as follows:

### Specific Aim 1:
Understand and describe the relationship between COVID deaths, and if any
constructs of population-level characteristics increase the rate of COVID
deaths.

#### Approach:
Population-level time series evaluating constructs of sociodemographic factors
such as prevalence smoking, prevalence obesity, median household income, etc.
and their association with rate of daily COVID events, accounting for trends in
time.

### Specific Aim 2:
What is the impact of social distancing initiatives at the population-level on
COVID deaths?

#### Approach
In the same time-series model used above, we use a penalized distributed lag
approach to model the association between percent change in retail and recreation mobility 
using Google mobility data.

## Folder Structure:

`data`: Where created datasets are stored using data located in `source_data`.

`data/source_data/`: Contains scrapped data at different spatial and temporal resolutions.

- `nyt-us-counties`: New York Times Daily county time series of confirmed COVID cases and deaths. 
**CSV not provided** in accordance with licensing/redistributing restrictions,
but is downloaded when script is executed. Link to source [github](https://github.com/nytimes/covid-19-data).

- `rwj-county_ranking-2020`: Robert Wood Johnson (RWJ) sociodemographic variables.
Used with permission from RWJ. **CSV not provided** in accordance with licensing/redistributing restrictions,
but is downloaded when script is executed. Link to [website](https://www.countyhealthrankings.org/).

- `google-mobility`: Google mobility data. Used with permission. Note,
we cannot house raw Google mobility data in our repo (**CSV not provided**), 
but is downloaded when script is executed. Link to [source](https://www.google.com/covid19/mobility/).

- `cb_2018_us_county_5m`: Tiger/Line shapefile for US census used for various spatial
calculations and maps. Link to [US Census 2018 boundary file](https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_county_5m.zip)

- `county_pop_density.csv`: Created using RWJ county population and county areas
using TIGER shapefile.

- `county_fips.csv`: Mapping between county names and 5-digit character FIPS code.

- `cdc_cancer.csv`: CDC cancer incidence. Link to [CDC Cancer Statistics](https://gis.cdc.gov/Cancer/USCS/DataViz.html)

- `cms_comorbidity.csv`: CMS data on prevalence of acute myocardial infarction, 
hypertension, and chronic obstructive pulmonary disease. Link to 
[CMS Mapping Medicare Disparities](https://data.cms.gov/mapping-medicare-disparities)

`references`: Contains manuscripts and references used for the project.
- `methods` Folder for methods mostly around generalized additive models (GAMs)
and distributed lag models. Note we do not include these in the public repo to avoid copywrite
issues, but many of the references are publically available. 

- `data_dictionary` Contains data dictionary for Robert Wood Johnson (RWJ)
variables. We have also added our own key for names used in analysis and their
index in the original RWJ data dictionary; it can be used to map between the
two.

`results`: Contains results from scripts. Script of origin denoted by 2 digit
character (e.g. 04-result_figure came from 04 script).

`scripts`: Contains scripts for data collection, models, and results. Note, we
will add/modify 00-run.R script to make sure appropriate packages are installed.

- `00-run.R`: Runs all subsequent scripts. Can be executed from the top Level
of the directory using the following command in the command line:
`Rscript './scripts/00-run.R'`.

- `01-collect_data.R`: Collects all data necessary for analysis.

- `02-make_config_file.R`: Creates a config.yaml file used in subsequent scripts.

- `03-make_nyt_time_series_county.R`: Builds a master time series of all COVID19
cases and deaths, mobility, and RWJ variables for counties in the US.

- `04-county_principle_components_analysis.R`: Runs principle components analysis
for study counties, and sensitivity analysis that excludes New York City.

- `05-analysis_dataframe.R`: Joins in principle components to nyt time series
and limits only to counties of interest for analysis.

- `06-descriptive_figure.R`: Creates Figure 1 plot of study map and time series
of retail and recreation mobility and COVID19 deaths.

- `07-main_gam_boot.R`: GAM and corresponding bootstrap for primary results of principle component and lagged retail and recreation mobility. Saves large
R list objects to be used in subsequent script. Note, that the number of
iterations is based on the available compute cores. If at least 20 cores available it will run 10k iterations, else it will only run 100. User should
be aware that this process can take awhile.

- `08-boot_results.R`: Creates results and figures for manuscript. For PC1,
we used bias corrected percentiles of bootstraps, where DLNMs we used percentile
method without bias corrected as fits looked fine. Also includes versions of
plots showing all bootstrap iterations as another visualization of uncertainty.

- `09-sensitivity_analyses.R`: Runs sensitivity analyses excluding NYC, limiting
temporal trend, and trying another approach to model lagged relationship. Note
we use the mgcv bayesian posterior intervals for 95%CI as coverage is generally
comparable to boot methods. Note, this is not always the case for time series
models, hence our bootstrap approach above.

- `10-dlnm_twostage.R`: Sensitivity analysis using two-stage distributed lags.
Runs dlnms for each county/location separately and pools results in
meta-analysis. Note, that principle components cannot be included in these types
of models since there is no variation within a given county, so dlnm estimates
only.

- `config.yaml`: Configure file generated in `02`. Contains study end date,
study county fips, and RWJ variables to evaluate in principle components analysis.

- `results/00-session_info.txt`: Summary of time it takes to run from start to finish
and R session info (R version and packages used).

