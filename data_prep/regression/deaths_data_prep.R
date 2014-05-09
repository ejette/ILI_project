# deaths_data_prep takes in the influenza and mortality death data and
# (1) assigns an HHS region to each of the 122 cities
# (2) aggregates death data by date and HHS region
# (3) sums over regions to get national-level deaths

# input: mmwr.csv (Mortality and Morbidity Weekly report data from 122 cities)
# output: death data aggregated to the week at the regional and national level

setwd("~/Dropbox")

# load and format weekly report data
mmwr <- read.csv("ILI_project/data/regression/mmwr.csv")

# only keep observations in the right time window (based on the time window in the ILI data)
all_deaths <- mmwr[which(mmwr$mmrw_week_year >= 200840 & mmwr$mmrw_week_year <= 201239),c('city','state','mmrw_week_year','pneum_flu')]
all_deaths$state_full = state.name[match(all_deaths$state,state.abb)]

# states in each HHS region
region1 = c('Connecticut', 'Maine','Massachusetts',
            'New Hampshire','Rhode Island','Vermont')
region2 = c('New Jersey', 'New York')
region3 = c('Delaware','District of Columbia','Maryland',
            'Pennsylvania','Virginia','West Virginia')
region4 = c('Alabama', 'Florida', 'Georgia','Kentucky','Mississippi','North Carolina',
            'South Carolina','Tennessee')
region5 = c('Illinois', 'Indiana', 'Michigan', 'Minnesota', 'Ohio', 'Wisconsin')
region6 = c('Arkansas', 'Louisiana', 'New Mexico', 'Oklahoma', 'Texas')
region7 = c('Iowa', 'Kansas', 'Missouri', 'Nebraska')
region8 = c('Colorado', 'Montana', 'North Dakota', 'South Dakota', 'Utah', 'Wyoming')
region9 = c('Arizona', 'California', 'Hawaii', 'Nevada')
region10 = c('Alaska', 'Idaho', 'Oregon', 'Washington')

all_regions = list(region1, region2, region3, region4, region5, region6, region7, region8, region9, region10)
loop_len = length(all_regions)

# assign each observation to an HHS region
all_deaths$region = 0

for (i in 1:loop_len){
  all_deaths$region[all_deaths$state_full %in% all_regions[[i]]] = i
} 

# assign DC to region 3
all_deaths[all_deaths$region == 0, 'region'] = 3

# make a variable that contains week number
all_deaths$week_num = as.numeric(substr(all_deaths$mmrw_week_year,5,8))
# extract flu seasons
flu_season_deaths = all_deaths[all_deaths$week_num >= 40 | all_deaths$week_num <= 20, c('mmrw_week_year','pneum_flu','region')]

# aggregate up to the region
region_agg = aggregate(. ~ mmrw_week_year + region, data = flu_season_deaths, FUN = sum)

colnames(region_agg) <- c('date','region','deaths')
 
# make data wide so there is column for each region
deaths_flu_seasons_wide <- reshape(region_agg, v.names = 'deaths', idvar = 'date', timevar = 'region', direction = 'wide')
deaths_flu_seasons = deaths_flu_seasons_wide[order(deaths_flu_seasons_wide$date),]
# sum the deaths in all regions to get the national-level deaths
deaths_flu_seasons$deaths.national = rowSums(deaths_flu_seasons[,2:11])
deaths_flu_seasons = deaths_flu_seasons[,c(1,12,2:11)]

# output the regional death data
save(deaths_flu_seasons, file = 'ILI_project/data/regression/deaths_flu_seasons.Rda')

# save a dataframe of all the dates (this will be handy later when we need to plot things)
dates = data.frame(date = count(all_deaths[,'mmrw_week_year'])[1])
save(dates, file = "ILI_project/data/regression/dates.Rda")
