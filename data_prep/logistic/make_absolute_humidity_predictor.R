library(plyr)
library(stringr)

setwd("~/Dropbox")
# import datelookup table
source('ILI_project/functions/datelookup.R')
source('ILI_project/functions/format_fips.R')

################
# weather data #
################

# raw bulb temp and relative humidity indexed by wban
#ncdc_raw <- read.csv('/Users/jj/flu_surveil_data/humidity/all_humidity_agg.csv', sep = ' ')
load('ILI_project/data/logistic/all_humidity_agg.Rda')

flu_weeks <- read.csv("ILI_project/data/logistic/flu_weeks.csv", header=F)

ncdc_raw = all_humidity_agg
# compute absolute humidity from raw bulb temp and relative humidity using method from:
# McDevitt et al., "Role of Absolute Humidity in the Inactivation of Inﬂuenza Viruses on Stainless Steel Surfaces at Elevated Temperatures,"
# APPLIED AND ENVIRONMENTAL MICROBIOLOGY, June 2010.
ncdc_raw$temp_kelvin <- 273.15 + (ncdc_raw$dry_bulb_temp - 32)/1.8
ncdc_raw$sat_vapor_pres <- exp(-5800/ncdc_raw$temp_kelvin + 1.391 - 0.04864*ncdc_raw$temp_kelvin + 4.176e-5*(ncdc_raw$temp_kelvin)^2 - 1.445e-8*(ncdc_raw$temp_kelvin)^3 + 6.456*log(ncdc_raw$temp_kelvin))
ncdc_raw$absolute_humidity <- 0.00217*ncdc_raw$sat_vapor_pres*ncdc_raw$relative_humidity/ncdc_raw$temp_kelvin

# convert dates to CDC week format
colnames(flu_weeks) = c('year_week','date_raw')
ncdc_raw$date_raw = ncdc_raw$date
ncdc_raw$date <- as.Date(as.character(ncdc_raw$date),format='%Y%m%d')
ncdc_raw <- merge(ncdc_raw, dateWeekTable, by = 'date')

# average weekly absolute humidity by wban
ncdc <- ncdc_raw[,c('state','wban','CDCdate','absolute_humidity')]
ncdc$date_num = as.numeric(as.character(ncdc$CDCdate))

#get the subset of the data that is in the 2008 - 2012 flu seasons
seasons = ncdc[ncdc$date_num >= 200840 & ncdc$date_num <= 201239, c('state','absolute_humidity') ]

hum_state_ave = aggregate(. ~ state, data = seasons, FUN = mean)

# merge in counties
load('ILI_project/data/logistic/counties.Rda')
load('ILI_project/data/regression/county_FIPS_xwalk.Rda')

counties_hum = merge(hum_state_ave, counties, by = 'state')
counties_hum_fips = merge(counties_hum, county_FIPS_xwalk, by = c('county','state'))

counties_hum_fips$FIPS_id = as.character(counties_hum_fips$FIPS_code)
counties_hum_fips = counties_hum_fips[,c('absolute_humidity','FIPS_code')]

save(counties_hum_fips, file = 'ILI_project/data/logistic/counties_hum_fips.Rda')
