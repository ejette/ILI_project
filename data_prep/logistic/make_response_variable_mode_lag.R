library(plyr)
setwd("~/Dropbox")
source("ILI_project/functions/format_fips.R")

# load ILINet data
load("ILI_project/data/regression/ili_flu_seasons.Rda")
# load ranks data
load("ILI_project/data/regression_results/ranks_mode_lag.Rda")

# load all possible FIPS code and extract FIPS codes
load("ILI_project/data/regression/county_FIPS_xwalk.Rda")
FIPS = unique(format_fips(county_FIPS_xwalk$FIPS_code))

# find the counties that had non-zero observations
non_zero_counties = colnames(ili_flu_seasons[colSums(ili_flu_seasons > 0)])[-1]
non_zero_counties = gsub('total.','',non_zero_counties)

colnames(ranks_mode_lag)[1] = 'rank'
# find the counties that appeared in the top providers of the first optimization (this was the national level)
x = ranks_mode_lag[,2]
# leave out the lag tag
x = substr(x,1,5)
# assign a 1 if the county was chosen and a 0 if the county was not chosen as a provider
y = as.data.frame(as.numeric(FIPS %in% x))
colnames(y) = colnames(ranks_mode_lag[2])

# repeat for the rest of the regions
for (i in 3:ncol(ranks_mode_lag)){
  new_cols = c(colnames(y),colnames(ranks_mode_lag[i]))
  x = ranks_mode_lag[,i]
  x = substr(x,1,5)
  y = cbind(y,as.numeric(FIPS %in% x))
  colnames(y) = new_cols
}

# only use non-zero providers
y = cbind(FIPS, y)
y$non_zero_provider = 0
y$non_zero_provider[y$FIPS %in% non_zero_counties] = 1
y_lag_mode = y
colnames(y_lag_mode)[1] = 'FIPS_code'
save(y_lag_mode, file = 'ILI_project/data/logistic/y_lag_mode.Rda')
