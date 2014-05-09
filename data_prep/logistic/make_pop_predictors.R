setwd("~/Dropbox")
library(plyr)
library(reshape)
library(stringr)
source('ILI_project/functions/format_fips.R')
# get list of counties
load('ILI_project/data/regression/county_FIPS_xwalk.Rda')

# get list of cities
mmwr <- read.csv("ILI_project/data/regression/mmwr.csv",stringsAsFactors = FALSE )
cities = count(mmwr[,c('city','state')])[,1:2]
save(cities, file = '~/flu_surveil_data/cities.Rda')
cities$city[cities$city == 'Boise' & cities$state == 'ID'] = 'Boise City'
cities$city[cities$city == 'Colo. Springs' & cities$state == 'CO'] = 'Colorado Springs'
cities$city[cities$city == 'Ft. Worth' & cities$state == 'TX'] = 'Fort Worth'
cities$city[cities$city == 'Honolulu' & cities$state == 'HI'] = 'Urban Honolulu'
cities$city[cities$city == 'New York City' & cities$state == 'NY'] = 'New York'

# load county information
county_pop <- read.csv("ILI_project/data/logistic/PEP_2012_PEPANNRES/PEP_2012_PEPANNRES.csv", colClasses=c(rep("character",3),  rep("numeric",5)))
city_pop <- read.csv("ILI_project/data/logistic/PEP_2011_PEPANRGCT.US23PR/PEP_2011_PEPANRGCT.US23PR.csv")

# load FIPS look up table
FIPS <- read.csv("ILI_project/data/logistic/FIPS_CountyName.csv", header=F, colClasses = c(rep("character",2)))
colnames(FIPS) = c('FIPS_code', 'county_fips')

# make a variable in county that is formatted the same way as the FIPS county variable
counties$fips_format = paste(counties$county, ' County, ', counties$state, sep = '')
counties$fips_format= gsub('City County',"city", counties$fips_format)
counties$fips_format= gsub('De Kalb',"DeKalb", counties$fips_format)
counties$fips_format= gsub('Dekalb',"DeKalb", counties$fips_format)
counties$fips_format= gsub('De Witt County, TX',"DeWitt County, TX", counties$fips_format)
counties$fips_format= gsub('De Soto',"DeSoto", counties$fips_format)

for (i in 1:nrow(counties)){
  counties$fips_format[i][counties$state[i] == 'LA'] = paste(counties$county[i], ' Parish, ', counties$state[i], sep = '')
}

counties$fips_format = toupper(counties$fips_format)

FIPS$county_form = as.character(FIPS$county_fips)
FIPS$county_form[FIPS$county_form == 'Do\xb1a Ana County, NM'] = 'Dona Ana County, NM'
FIPS$county_form = toupper(FIPS$county_form)

counties$fips_format = counties$fips_format
counties$fips_format[counties$fips_format == 'ANCHORAGE COUNTY, AK'] = "ANCHORAGE MUNICIPALITY, AK"
counties$fips_format[counties$fips_format == 'DADE COUNTY, FL'] = "MIAMI-DADE COUNTY, FL"
counties$fips_format[counties$fips_format == 'DISTRICT OF COLUMBIA COUNTY, DC'] = "DISTRICT OF COLUMBIA, DC"
counties$fips_format[counties$fips_format == 'DU PAGE COUNTY, IL'] = "DUPAGE COUNTY, IL"
counties$fips_format[counties$fips_format == 'FAIRBANKS NORTH STAR COUNTY, AK'] = "FAIRBANKS NORTH STAR BOROUGH, AK"
counties$fips_format[counties$fips_format == 'JAMES CITY, VA'] = "JAMES CITY COUNTY, VA"
counties$fips_format[counties$fips_format == 'JUNEAU COUNTY, AK'] = "JUNEAU CITY AND BOROUGH, AK"
counties$fips_format[counties$fips_format == 'KENAI PENINSULA COUNTY, AK'] = "KENAI PENINSULA BOROUGH, AK"
counties$fips_format[counties$fips_format == 'LA SALLE COUNTY, IL'] = "LASALLE COUNTY, IL"
counties$fips_format[counties$fips_format == 'RADFORD COUNTY, VA'] = "RADFORD CITY, VA"
counties$fips_format[counties$fips_format == 'SALEM COUNTY, VA'] = "SALEM CITY, VA"
counties$fips_format[counties$fips_format == 'VALDEZ CORDOVA COUNTY, AK'] = "VALDEZ-CORDOVA CENSUS AREA, AK"

county_FIPS = merge(counties, FIPS, by.x = "fips_format", by.y = "county_form")
county_FIPS$county_fips = as.character(county_FIPS$county_fips)
county_FIPS_xwalk = county_FIPS[,c('county','state','FIPS_code')]

# merge on population
m_county = merge(county_pop, county_FIPS, by.x = 'GEO.id2', by.y = 'FIPS_code', all.y = TRUE)
m_county = m_county[,c('GEO.id2','county','state','respop72010')]

# add populations for the two "counties" that do not have FIPS codes
unorg_ak_2010_pop = 19621 # from a report by Alaska Labor Statistics
virgin_islands_2010_pop = 106405 # from the census website
m_county = rbind(m_county, c('00998','Unorganized','AK', unorg_ak_2010_pop))
m_county = rbind(m_county, c('00999','Saint Croix Island','VI', virgin_islands_2010_pop))

m_county$county[m_county$county == "Dade"] = "Dade"
county_w_pop = unique(m_county[,c('GEO.id2','respop72010')])

save(county_w_pop, file = 'ILI_project/data/logistic/county_w_pop.Rda')

