setwd("~/Dropbox")
library(ggplot2)
load("ILI_project/data/regression/ili_flu_seasons.Rda")
load("ILI_project/data/regression/deaths_flu_seasons.Rda")

deaths = deaths_flu_seasons

# remove all columns that have a colsum of 0
ili_flu_seasons = ili_flu_seasons[,colSums(ili_flu_seasons) > 0]
# break data up into flu seasons so that we can lag it easily below
ili_flu_seasons1 = ili_flu_seasons[ili_flu_seasons$date >= 200840 & ili_flu_seasons$date <= 200921,]
ili_flu_seasons2 = ili_flu_seasons[ili_flu_seasons$date >= 200940 & ili_flu_seasons$date <= 201021,]
ili_flu_seasons3 = ili_flu_seasons[ili_flu_seasons$date >= 201040 & ili_flu_seasons$date <= 201121,]
ili_flu_seasons4 = ili_flu_seasons[ili_flu_seasons$date >= 201140 & ili_flu_seasons$date <= 201221,]

# initialize and empty list for the r^2 values for each provider
r2 = c()

# find the optimal lag for each provider and each region combination
for (k in 2:ncol(deaths)){
  print(colnames(deaths)[k])
  for (i in 2:ncol(ili_flu_seasons)){
    lag_df1 = data.frame(ili_flu_seasons1[,i])
    lag_df2 = data.frame(ili_flu_seasons2[,i])
    lag_df3 = data.frame(ili_flu_seasons3[,i])
    lag_df4 = data.frame(ili_flu_seasons4[,i])
    # for each provider, run the models with lags from 0 to 5 weeks
    for (j in 0:5){
      d1 = c(rep(NA,j), ili_flu_seasons1[1:(nrow(lag_df1)-j),i])
      d2 = c(rep(NA,j), ili_flu_seasons2[1:(nrow(lag_df2)-j),i])
      d3 = c(rep(NA,j), ili_flu_seasons3[1:(nrow(lag_df3)-j),i])
      d4 = c(rep(NA,j), ili_flu_seasons4[1:(nrow(lag_df4)-j),i])
      lagged = c(d1,d2,d3,d4)
      # bind the deaths for region k and the lagged provider variable
      model_df = data.frame(cbind(deaths[,k],lagged))
      colnames(model_df)[1] = 'deaths'
      # run the model with the lagged provider data
      fit = lm(deaths ~ ., data = model_df)
      # record r^2
      r2 = c(r2, summary(fit)$r.squared)
    }
    # find the maximum r^2 for that provider and region k
    max_r2 = max(r2)
    # find the lag that resulted in the maximum r^2 for that provider and region k
    max_r2_lag = which.max(r2)-1
    # record these two values
    # initialize a dataframe if this the first provider in the first region
    if (i == 2 & k == 2){
      opt_lag = data.frame(provider = colnames(ili_flu_seasons)[i], city = colnames(deaths)[k], max_r2 = max_r2, max_r2_lag = max_r2_lag, stringsAsFactors = FALSE)
    }
    # otherwise, bind the max_r2 and max_r2_lag to the existing dataframe
    else{
      opt_lag = rbind(opt_lag,c(colnames(ili_flu_seasons)[i], colnames(deaths)[k], max_r2, max_r2_lag))
    }
    r2 = c()
  }
}

# look at the distribution of optimal lags by region
# this will make it a little prettier
colnames(opt_lag)[2] = 'region'
opt_lag$region = gsub('deaths.','region',opt_lag$region)
opt_lag$region = gsub('regionnational','national',opt_lag$region)

# create a histogram per region of the optimal lags
m <- ggplot(opt_lag, aes(x=as.factor(max_r2_lag), fill=as.factor(max_r2_lag))) + geom_histogram(binwidth = 1)  
m + facet_grid(region ~ .) + labs(title = 'Optimal Lag Value Counts for all HHS Regions + National Timeseries',
                                  x = 'Lag',
                                  y = 'Count') + scale_fill_discrete(name="Lag in Weeks")

save(opt_lag, file = 'ILI_project/data/regression/opt_lag.Rda')