# flu_data_prep.R takes in ILINet data and a crosswalk between county names 
# and FIPS ids and 
# (1) calculates the total number of ILI cases each provider saw
# (2) formats the county names to make them uniform
# (3) merges on FIPS code for each county
# (4) aggregates the ILI and denominator counts to the county level

setwd("~/Dropbox")
source('ILI_project/functions/format_fips.R')

library(reshape)
library(plyr)
library(stringr)

# ILI provider data
ili <- read.csv("ILI_project/data/regression/ProviderILI.txt")
# County to FIPS index
load('ILI_project/data/regression/county_FIPS_xwalk.Rda')

# sum over rows to make total ILI patient variable for each provider
ili$cases <- rowSums(ili[,c('Age_0_4','Age_5_24','Age_25_64',
                            'Age_25_49','Age_50_64','Age_65_and_older')], na.rm = TRUE)

# the following lines attempt to make the county names more uniform (e.g., some are misspelled,
# some say "Saint" instead of "St.", etc.)
ili = ili[order(ili$Phys_ID_Code, ili$datecode),]
ili$county_temp = tolower(str_trim((as.character(ili$County))))
ili$county_temp = gsub("\\b([a-z])([a-z]+)", "\\U\\1\\L\\2", ili$county_temp, perl=TRUE)
ili$county_temp = gsub("\\.","",ili$county_temp)
ili$county_temp = gsub("St ","St\\. ",ili$county_temp)
ili$county_temp = gsub("-"," ",ili$county_temp)
ili$county_temp[ili$County == ''] = 'New York'
ili$St = str_trim(ili$St)
ili$county_temp = str_trim(ili$county_temp)
ili$county_temp[ili$county_temp == 'Virginia Beach'] = 'Virginia Beach City'
ili$county_temp[ili$county_temp == 'Concoridia'] = 'Concordia'
ili$county_temp[ili$county_temp == 'Newport News'] = 'Newport News City'
ili$county_temp[ili$county_temp == 'Frederickburg City'] = 'Fredericksburg City'
ili$county_temp[ili$county_temp == 'Chesapeake'] = 'Chesapeake City'
ili$county_temp[ili$county_temp == 'Fairfax City'] = 'Fairfax'
ili$county_temp[ili$county_temp == 'Fairfiled'] = 'Fairfield'
ili$county_temp[ili$county_temp == 'Dekalb' & ili$St == 'AL'] = 'De Kalb'
ili$county_temp[ili$county_temp == 'Dekab' & ili$St == 'AL'] = 'De Kalb'
ili$county_temp[ili$county_temp == 'Lorian' & ili$St == 'OH'] = 'Lorain'
ili$county_temp[which(ili$county_temp == 'Cordova' & ili$St == 'AK')] = 'Valdez Cordova'
ili$county_temp[ili$county_temp == 'Miami Dade'] = 'Miami-Dade'
ili$county_temp[ili$county_temp == 'Cleveland' & ili$St == 'MS'] = 'Bolivar'
ili$county_temp[ili$county_temp == 'Dekalb' & ili$St == 'IL'] = 'De Kalb'
ili$county_temp[ili$county_temp == 'Dekalb' & ili$St == 'IN'] = 'De Kalb'
ili$county_temp[ili$county_temp == 'Dupage' & ili$St == 'IL'] = 'Du Page'
ili$county_temp[ili$county_temp == 'Lasalle' & ili$St == 'IN'] = 'La Salle'
ili$county_temp[ili$county_temp == 'Lasalle' & ili$St == 'IL'] = 'La Salle'
ili$county_temp[ili$county_temp == 'Desoto' & ili$St == 'MS'] = 'De Soto'
ili$county_temp[ili$county_temp == 'Desoto' & ili$St == 'FL'] = 'De Soto'
ili$county_temp[ili$county_temp == 'Ostego' & ili$St == 'NY'] = 'Otsego'
ili$county_temp[ili$county_temp == 'Dewitt' & ili$St == 'TX'] = 'De Witt'
ili$county_temp[ili$county_temp == 'Pleasant' & ili$St == 'WV'] = 'Pleasants'
ili$county_temp[ili$county_temp == 'Radford City' & ili$St == 'VA'] = 'Radford'
ili$county_temp[ili$county_temp == 'Salem City' & ili$St == 'VA'] = 'Salem'
ili$county_temp[ili$county_temp == 'Harrisonburg' & ili$St == 'VA'] = 'Harrisonburg City'
ili$county_temp[ili$county_temp == 'Harden' & ili$St == 'TX'] = 'Hardin'

# trim the county and state variables so they will merge with the FIPS data correctly
ili$county = str_trim(ili$county_temp)
ili$state = str_trim(ili$St)

# merge FIPS and ILI data
ili_cnty_with_FIPS = merge(ili, county_FIPS_xwalk, by = c('county','state'))
# format FIPS to have a leading 0 if only four digits only long
ili_cnty_with_FIPS$FIPS_id = format_fips(ili_cnty_with_FIPS$FIPS_code)

# extract the date (week number and year), FIPS id, totalpt (the denominator), and cases (the
# total number of ILI patients a provider treated)
ili_trim_cnty <- ili_cnty_with_FIPS[,c('datecode','FIPS_id','cases','totalpt')]
colnames(ili_trim_cnty) <- c('date','FIPS_id','cases','total_pt')
ili_trim_cnty$FIPS_num = as.numeric(ili_trim_cnty$FIPS_id)

## Data Aggregation
# sort the data by FIPS code (the numeric one) and the date
ili_trim_cnty = ili_trim_cnty[order(ili_trim_cnty$FIPS_num,ili_trim_cnty$date),]
# leave out the FIPS_num variable
ili_trim_cnty = ili_trim_cnty[,1:4]
# aggregate to county level
ili_cnty_agg <- aggregate(. ~ date + FIPS_id, data = ili_trim_cnty, FUN = sum)
# calculate %ILI_patients = cases/total_pt (replace the denominator for providers 
# who saw 0 ILI patients and 0 total patients with with 1's to avoid dividing by 0)
ili_cnty_agg$total_pt[ili_cnty_agg$cases == 0 & ili_cnty_agg$total_pt == 0] = 1
ili_cnty_agg$total = ili_cnty_agg$cases/ifelse(ili_cnty_agg$total_pt == 0, NA, ili_cnty_agg$total_pt)

#########################################
##### this is for logistic analysis ######
## make a dataframe that will be used in data_prep/logistic/make_age_reporting_predictors.R
ili_over_65 = ili_cnty_with_FIPS[,c('datecode','FIPS_id','cases','totalpt','Age_65_and_older')]
colnames(ili_over_65) <- c('date','FIPS_id','cases','total_pt','over_65')
save(ili_over_65, file = 'ILI_project/data/logistic/ili_over_65.Rda')

# calculate the number of providers per FIPS code
provider_FIPS_cnt = count(ili_cnty_with_FIPS[,c('FIPS_id','Phys_ID_Code')])
providers_per_FIPS = count(provider_FIPS_cnt[,'FIPS_id'])
colnames(providers_per_FIPS) = c('FIPS_id','n_providers')
save(providers_per_FIPS, file = 'ILI_project/data/logistic/providers_per_FIPS.Rda')

# calculate the average number of patients counties have per week
ave_denom_per_FIPS = aggregate(total_pt ~ FIPS_id, data = ili_cnty_agg, FUN = mean)
save(ave_denom_per_FIPS, file = 'ILI_project/data/logistic/ave_denom_per_FIPS.Rda')

## the logistic analysis portion is over
#########################################

# only keep date, FIPS code, and the %ILI_patients variables
ili_cnty_agg = ili_cnty_agg[,c('date','FIPS_id','total')]

# reshape ili data so each date has only one row
# make sure not all cells for a provider are NA
ili_wide_cnty <- reshape(ili_cnty_agg, v.names = 'total', idvar = 'date', timevar = 'FIPS_id', direction = 'wide')
ili_wide_cnty = ili_wide_cnty[order(ili_wide_cnty$date),]

# construct a dataframe with no missing values by replacing NA with 0
ili_wide_cnty_zeros_FIPS <- ili_wide_cnty
for (i in 1:nrow(ili_wide_cnty_zeros_FIPS)){
  ili_wide_cnty_zeros_FIPS[i,is.na(ili_wide_cnty_zeros_FIPS[i,])] = 0
}

# output the wide data
#save(ili_wide_cnty_zeros_FIPS, file = 'ILI_project/data/regression/ili_wide_cnty_zeros_FIPS.Rda')

# extract flu seasons from ilinet data
ili_wide_cnty_zeros_FIPS$week_num = as.numeric(substr(ili_wide_cnty_zeros_FIPS$date,5,8))
ili_flu_seasons = ili_wide_cnty_zeros_FIPS[ili_wide_cnty_zeros_FIPS$week_num >= 40 | ili_wide_cnty_zeros_FIPS$week_num <= 20, -1359]
ili_flu_seasons = ili_flu_seasons[order(ili_flu_seasons$date),]
save(ili_flu_seasons, file = 'ILI_project/data/regression/ili_flu_seasons.Rda')
