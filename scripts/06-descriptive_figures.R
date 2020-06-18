# ------------------------------------------------------------------------------
# Title: Descriptive data visualizations and maps
# Author: Sarah McGough and Ryan Gan
# ------------------------------------------------------------------------------

# libraries 
library(tidyverse)
library(sf) # spatial package
library(cowplot) # for grid arrangement

# read in config file
config <- yaml::read_yaml('./scripts/config.yaml')

# read analysis_df
analysis_df <- read_csv('./data/05-analysis_df.csv')

# Figure 1A: Bivariate Choropleth ----------------------------------------------


# MAP SET UP -------------------------------------------------------------------

# Create bivariate color scheme
d <- expand.grid(x=1:100,y=1:100)

# limit to the first observation to have a single RWJ variable per county
first_obs <- analysis_df %>% 
  group_by(fips) %>% 
  slice(1)

# Now break into 3x3 groups for the bivariate plots
d <- expand.grid(x=1:3, y=1:3)

# Break variables of interest into quantiles
x.v <- quantile(first_obs$adult_obesity_v011, c(0.33,0.66,1))
y.v <- quantile(first_obs$median_household_income_v063, c(0.33,0.66,1))

# create appropriate labels for d
x_labs <- c(
  paste0('<', round(x.v[1]*100, 1)),
  paste0(round(x.v[1]*100, 1), '-', round(x.v[2]*100, 1)),
  paste0('>',round(x.v[2]*100, 1)) 
  )

y_labs <- c(
  paste0('<', substring(y.v[1], 1, 2), 'k'),
  paste0(substring(y.v[1], 1, 2), 'k', '-', substring(y.v[2], 1, 2), 'k'),
  paste0('>', substring(y.v[2], 1, 2), 'k')
  )

d <- merge(d,data.frame(x=1:3, xlabel = x_labs), by="x")
d <- merge(d,data.frame(y=1:3, ylabel = y_labs), by="y")

# Grab the two variables to plot
bivariates <- first_obs %>% 
  mutate(
    y = ifelse(
      median_household_income_v063 < y.v[1], 1,
      ifelse(median_household_income_v063 < y.v[2], 2, 3)
      ) ,
    x = ifelse(
      adult_obesity_v011 < x.v[1],1,
      ifelse(adult_obesity_v011 < x.v[2],2,3)
    )  
  )   %>%
  select(fips,county,state,adult_obesity_v011,median_household_income_v063,y,x) %>% 
  # make variable, map colors
  mutate(map_colors = atan(y/x), alpha_colors=x+y)


# Define color gradients low and high
low_pal <- '#FF0000'
high_pal <- '#046C9A'


# set min and max vals
min_val <- min(bivariates$map_colors, na.rm = T)
max_val <- max(bivariates$map_colors, na.rm = T)

# map legend ----
legend_plot <- ggplot(
  d, 
  aes(x,y, fill = atan(y/x), alpha=x+y, label= paste0(xlabel,"\n", ylabel))
  ) +
  geom_tile()+
  #geom_text(alpha=1, size = 3)+
  scale_fill_gradient(
    low = low_pal, 
    high = high_pal,
    na.value = 'white',
    limits = c(min_val, max_val)
  ) +
  scale_y_continuous(
    breaks = 1:3,
    labels = y_labs,
    position = 'right'
    ) +
  scale_x_continuous(
    breaks = 1:3,
    labels = x_labs
  ) +
  #theme_void() +
  theme(
    axis.ticks = element_blank(), # remove ticks
    axis.text = element_text(size = 7), # axis text size
    axis.title = element_text(size = 8),
    legend.position = "none",
    axis.title.y = element_text(angle = -90), 
    panel.background=element_blank(),
    plot.margin=margin(t=2,b=2,l=2, r=2)
    ) + 
  labs(
    x = "Adult Obesity Prevalence %",
    y = "Median\nHousehold Income"
    )


# spatial map ------------------------------------------------------------------
# read in US county shapefile
# subset to states
county_sf <- st_read(
  './data/source_data/cb_2018_us_county_5m/cb_2018_us_county_5m.shp'
  ) %>% 
  # limit to continental us
  filter(STATEFP %in% c(paste0('0', 1:9), as.character(10:56))) %>% 
  # exlcude hi and ak
  filter(!STATEFP %in% c('02', '15'))

# need to collapse nyc; assigning to 36061
ny <- county_sf %>% 
  filter(STATEFP == '36') %>%
  # filter out nyc counties
  filter(!(GEOID %in% c("36005","36047", "36081","36085")))


nyc <- county_sf %>% 
  filter(GEOID %in% c("36005","36047","36061","36081","36085")) %>% 
  st_union() 

# reassign geometry of manhattan the collapsed ny object
st_geometry(ny[ny$GEOID=='36061',]) <- st_geometry(nyc)

# county ses map
county_sf_ses <- county_sf %>% 
  # get rid of nyc counties except manhattan
  filter(!(GEOID %in% c("36005","36047","36081","36085"))) %>% 
  mutate(study_county = ifelse(GEOID %in% config$study_fips, 1, 0))

# reassign geometry of manhattan the collapsed ny object
st_geometry(county_sf_ses[county_sf_ses$GEOID=='36061',]) <- st_geometry(nyc)

# make a simple features to plot collapse counties and join in bivariate values
plot_sf <- county_sf_ses %>% 
  mutate(fips = case_when(study_county == 1 ~ GEOID)) %>% 
  group_by(fips) %>% 
  summarize(
    study_county = max(study_county)
    ) %>% 
  # join in metro area
  left_join(select(first_obs, fips, metro_area), by = 'fips') %>% 
  left_join(bivariates, by = c('fips' = 'fips'))


# us plot ----
us_plot <- ggplot() +
  geom_sf(
    data = plot_sf,
    aes(fill=map_colors, alpha = alpha_colors)
    ) + 
  coord_sf(datum=NA) + # removes lat/lon grid
  scale_fill_gradient(
    low = low_pal, 
    high = high_pal,
    na.value = 'white',
    limits = c(min_val, max_val)
  ) +
  theme_minimal() + 
  theme(legend.position="none")


# san francisco ----
sanfran_plot <- ggplot() +
  geom_sf(
    data = plot_sf %>% filter(metro_area == 'San Francisco'), 
    aes(fill=map_colors, alpha = alpha_colors)) +
  coord_sf(datum = NA) +
  scale_fill_gradient(
    low = low_pal, 
    high = high_pal,
    na.value = 'white',
    limits = c(min_val, max_val)
  ) +
  ggtitle('San Francisco Bay Area') +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    panel.border = element_rect(color = 'black', fill = NA, size = 0.5)
  )


# los angeles ----
losangeles_plot <- ggplot() +
  geom_sf(
    data = plot_sf %>% filter(metro_area == 'Los Angeles'), 
    aes(fill=map_colors, alpha = alpha_colors)) +
  coord_sf(datum = NA) +
  scale_fill_gradient(
    low = low_pal, 
    high = high_pal,
    na.value = 'white',
    limits = c(min_val, max_val)
  ) +
  ggtitle('Los Angeles') +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    panel.border = element_rect(color = 'black', fill = NA, size = 0.5)
  )


# seattle plot ----
seattle_plot <- ggplot() +
  geom_sf(
    data= plot_sf %>% filter(metro_area == 'Seattle'), 
    aes(fill=map_colors, alpha = alpha_colors)) +
  coord_sf(datum = NA) +
  scale_fill_gradient(
    low = low_pal, 
    high = high_pal,
    na.value = 'white',
    limits = c(min_val, max_val)
  ) +
  ggtitle('Seattle') +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    panel.border = element_rect(color = 'black', fill = NA, size = 0.5)
  )


# new york city ----
nyc_plot <- ggplot() +
  geom_sf(
    data= plot_sf %>% filter(metro_area == 'New York City'), 
    aes(fill=map_colors, alpha = alpha_colors)) +
  coord_sf(datum = NA) +
  scale_fill_gradient(
    low = low_pal, 
    high = high_pal,
    na.value = 'white',
    limits = c(min_val, max_val)
  ) +
  ggtitle('New York City') +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    panel.border = element_rect(color = 'black', fill = NA, size = 0.5)
  )

# chicago ----
chitown_plot <- ggplot() +
  geom_sf(
    data= plot_sf %>% filter(metro_area == 'Chicago'), 
    aes(fill=map_colors, alpha = alpha_colors)) +
  coord_sf(datum = NA) +
  scale_fill_gradient(
    low = low_pal, 
    high = high_pal,
    na.value = 'white',
    limits = c(min_val, max_val)
  ) +
  ggtitle('Chicago') +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    panel.border = element_rect(color = 'black', fill = NA, size = 0.5)
  )


# detroit ----
detroit_plot <- ggplot() +
  geom_sf(
    data= plot_sf %>% filter(metro_area == 'Detroit'), 
    aes(fill=map_colors, alpha = alpha_colors)) +
  coord_sf(datum = NA) +
  scale_fill_gradient(
    low = low_pal, 
    high = high_pal,
    na.value = 'white',
    limits = c(min_val, max_val)
  ) +
  ggtitle('Detroit') +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    panel.border = element_rect(color = 'black', fill = NA, size = 0.5)
  )


# new orleans ----
meworleans_plot <- ggplot() +
  geom_sf(
    data= plot_sf %>% filter(metro_area == 'New Orleans'), 
    aes(fill=map_colors, alpha = alpha_colors)) +
  coord_sf(datum = NA) +
  scale_fill_gradient(
    low = low_pal, 
    high = high_pal,
    na.value = 'white',
    limits = c(min_val, max_val)
  ) +
  ggtitle('New Orleans') +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    panel.border = element_rect(color = 'black', fill = NA, size = 0.5)
  )

# paper map --------------------------------------------------------------------

# map in paper; uses cowplot to arrange grids
paper_map <- ggdraw(xlim = c(0,1), ylim = c(0,1)) +
  draw_plot(us_plot, width = 1, height = 1, x = 0, y = 0) +
  draw_plot(seattle_plot, width = 0.25, height = 0.25, x = 0.05, y = 0.67) +
  draw_plot(sanfran_plot, width = 0.25, height = 0.25, x = 0.05, y = 0.43) +
  draw_plot(losangeles_plot, width = 0.25, height = 0.25, x = 0.02, y = 0.12) +
  draw_plot(chitown_plot, width = 0.25, height = 0.25, x = 0.43, y = 0.55) +
  draw_plot(detroit_plot, width = 0.25, height = 0.25, x = 0.57, y = 0.39) +
  draw_plot(meworleans_plot, width = 0.25, height = 0.25, x = 0.41, y = 0.28) +
  draw_plot(nyc_plot, width = 0.25, height = 0.25, x = 0.78, y = 0.35) +
  draw_plot(legend_plot, width = 0.25, height = 0.25, x = 0.75, y = 0) +
  draw_figure_label(label = 'A', size = 20, fontface = 'bold')


# Figure 1B: Mobility/Covid death time series ----------------------------------
mobility_covid_plot <- ggplot() +
  geom_hline(yintercept=100, linetype="dashed", color="grey40") +
  geom_hline(yintercept=50, linetype="dashed", color="grey40") +
  geom_hline(yintercept=0, linetype="dashed", color="grey40") +
  geom_hline(yintercept=-50, linetype="dashed", color="grey40") +
  geom_line(
    data = filter(analysis_df, date >= '2020-02-15'), 
    aes(x= date, y = retail_and_recreation_percent_change_from_baseline, 
        group=metro_state_county, color = 'Mobility: County'), 
    size = 0.5,
    alpha=0.7
  ) +
  geom_smooth(
    data = filter(analysis_df, date >= '2020-02-15'), 
    aes(x= date, y = retail_and_recreation_percent_change_from_baseline, 
        color = 'Mobility: LOESS Average'), se = F, size = 2
  ) +
  geom_line(
    data = filter(analysis_df, date >= '2020-02-15'), 
    aes(x= date, y = (daily_deaths_rollavg7/population_v051) * 100000 * 10,
        group=metro_state_county, color = 'COVID-19: County'),
    size = 0.5,
    alpha=0.7
  ) +
  geom_smooth(
    data = filter(analysis_df, date >= '2020-02-15'), 
    aes(x= date, y = (daily_deaths_rollavg7/population_v051) * 100000 * 10,
        color = 'COVID-19: LOESS Average'), se = F, size = 2
  ) +
  scale_color_manual(values=c("#FF8008","coral",'#00609d', '#00416A')) + 

  scale_y_continuous(
    name= 'Retail and Recreation % Change from Baseline',
    sec.axis = sec_axis(
      ~./10, 
      name = 'Daily COVID-19 Deaths Per 100,000 Persons\nRolling 7 Day Average')) +
  xlab('Date') +
  theme_classic() +
  theme(
    legend.position = 'bottom',
    legend.title = element_blank(),
    axis.title = element_text(size = 10)
  ) 


# add as cowplot type object
paper_mobility <- ggdraw() +
  draw_plot(mobility_covid_plot, width = 1, height = 0.9, x = 0, y = 0) +
  draw_figure_label(label = 'B', size = 20, fontface = 'bold')

# Final arrangement of Fig1 plots ----------------------------------------------
final_plot <- plot_grid(
  paper_map, 
  paper_mobility, 
  ncol = 1, 
  rel_widths = c(0.8, 0.8), 
  rel_heights = c(2, 2)
  )


# save final plot
ggsave(
  final_plot, 
  filename = './results/06-fig_1.pdf',
  height = 10,
  width = 8,
  unit = 'in'
  )

# save png version for publication
ggsave(
  final_plot, 
  filename = './results/06-fig_1.png',
  height = 10,
  width = 8,
  unit = 'in',
  dpi = 300
)

