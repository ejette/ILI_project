# optimization_dynamic_lag.R builds models using forward selection incorporating a dynamic lag,
# meaning that the model picks the best provider/lag combination (i.e., providers can be chosen multiple 
# times with different lags in the same model)

setwd("~/Dropbox")
source("ILI_project/functions/make_positive_lag.R")
source("ILI_project/functions/var_select_cnty.R")

library(plyr)
library(reshape)
library(ggplot2)
library(zoo)
library(gridExtra)

# load dates data
load("ILI_project/data/regression/dates.Rda")

# load death timeseries
load("ILI_project/data/regression/deaths_flu_seasons.Rda")
deaths = deaths_flu_seasons
# load ILINet data
load("ILI_project/data/regression/ili_flu_seasons.Rda")

provider_data = ili_flu_seasons[,colSums(ili_flu_seasons) > 0]

# function lags the input data
make_lag_input = function(provider_data, lag_step){
  for (i in 2:(ncol(provider_data)-1)){
    provider_data[,i] = make_positive_lag(provider_data[,c(1,i)], lag = lag_step)
  }
  return(provider_data)
}

# list of all the column names
names = colnames(provider_data)
# create lags from 0 to 5 weeks and combine all the data
# rename the columns of each lagged dataframe to include the lag
providers_lag0 = provider_data
colnames(providers_lag0) = paste(names,'_0',sep="")
providers_lag1 = make_lag_input(provider_data, 1)
colnames(providers_lag1) = paste(names,'_1',sep="")
providers_lag2 = make_lag_input(provider_data, 2) 
colnames(providers_lag2) = paste(names,'_2',sep="")
providers_lag3 = make_lag_input(provider_data, 3) 
colnames(providers_lag3) = paste(names,'_3',sep="")
providers_lag4 = make_lag_input(provider_data, 4)
colnames(providers_lag4) = paste(names,'_4',sep="")
providers_lag5 = make_lag_input(provider_data, 5) 
colnames(providers_lag5) = paste(names,'_5',sep="")

all_lags = cbind(cbind(cbind(cbind(cbind(providers_lag0,providers_lag1[,-1]), providers_lag2[,-1]), providers_lag3[,-1]),
                       providers_lag4[,-1]), providers_lag5[,-1]) 

# size of optimal network
n_counties = 10
# ranks is a dataframe that keeps track of the counties chosen for the optimal network 
# and the order in which they were chosen
ranks_dynamic_lag = as.data.frame(1:n_counties)
colnames(ranks_dynamic_lag) = 'index'
# r2_values is a dataframe that keeps track of the r2 values associated with adding a provider in the
# ranks dataframe to the regression
r2_values = as.data.frame(1:n_counties)
colnames(r2_values) = 'index'
save(r2_values, file = 'ILI_project/data/regression_results/r2_values_dynamic_lag.Rda')

n = ncol(deaths)
for (i in 2:n){
  print(i)
  load('ILI_project/data/regression_results/r2_values_dynamic_lag.Rda')
  # build the model with n_providers for the region
  ranks_dynamic_lag = var_select_cnty(obj = deaths[,i], vars = all_lags[,2:ncol(all_lags)], goal = n_counties, r2_values = r2_values, ranks = ranks_dynamic_lag, type = 'dynamic')
  # name the last column in the ranks_dynamic_lag dataframe with the label in the opt_lag_data
  colnames(ranks_dynamic_lag)[length(colnames(ranks_dynamic_lag))] = colnames(deaths)[i]
  save(ranks_dynamic_lag,file = 'ILI_project/data/regression_results/ranks_dynamic_lag.Rda')
  # name the last column in the r2_values_dynamic_lag dataframe
  load('ILI_project/data/regression_results/r2_values_dynamic_lag.Rda')
  colnames(r2_values)[length(colnames(r2_values))] = colnames(deaths)[i]
  save(r2_values, file = 'ILI_project/data/regression_results/r2_values_dynamic_lag.Rda')
}

load('ILI_project/data/regression_results/ranks_dynamic_lag.Rda')
load('ILI_project/data/regression_results/r2_values_dynamic_lag.Rda')

pdf(file = 'ILI_project/visualizations/regression_residuals_dynamic_lag.pdf', width = 8.5, height = 11)
par(mfrow=c(3,2))
# initialize a dataframe to store the predicted deaths
predicted_deaths = data.frame(date = ili_flu_seasons[,'date'])

# run regressions with optimized sets of providers for each region and then make histogram/plots of residuals to decide which approach is best to use
for (i in 2:ncol(ranks_dynamic_lag)){
  # format region name
  region = colnames(ranks_dynamic_lag)[i]
  region_name = gsub('deaths.', 'HHS region ', region)
  region_name = gsub('HHS region national', 'national ', region_name)
  top_providers = paste('total.',ranks_dynamic_lag[,i],sep='')
  # extract the top provider variables
  X = all_lags[,c(top_providers)]
  # make regression input
  reg_opt = cbind(deaths[,region], X)
  colnames(reg_opt)[1] = 'deaths'
  # do the fit
  fit = lm(deaths ~., data = reg_opt)
  summary(fit)
  # plot the residuals
  hist(fit$residuals, main = paste("Histogram of residuals for ", region_name," optimization 
                                     using a lag of (using no seasonal indicator)", sep = ''), xlab = "residual")
  plot(fit$residuals, main = paste("Residuals for ", region_name," optimization 
     using a lag of (using no seasonal indicator)", sep = ''), xlab = "time in weeks", ylab = "residuals")
  # calculate the predicted deaths
  coefs = as.data.frame(t(coef(summary(fit))[,'Estimate']))
  deaths_pred = reg_opt
  # set the first column to be the intercept
  deaths_pred[,1] = rep(coefs[1,1],nrow(deaths_pred))
  for (i in 2:ncol(reg_opt)){
    # multiply each predictor by the coefficient from the regression
    deaths_pred[,i] = reg_opt[,i]*coefs[1,i]
  } 
  # sum the interecept and the products of the coefficents and the predictor variables to obtain the estimated deaths
  deaths_pred$x = rowSums(deaths_pred)
  predicted_deaths = cbind(predicted_deaths,deaths_pred$x)
}
dev.off()

# r^2 plots
r2_dynamic_lag = r2_values
# make the data long for ggplot
r2_long = melt(r2_dynamic_lag, id.vars = 'index')
# make the variable more readable
r2_long$variable =gsub("deaths.","",r2_long$variable)

# set the colors
region1_col = 'orange2'
region2_col = 'yellow3'
region3_col = 'turquoise3'
region4_col = 'blue'
region5_col = 'purple'
region6_col = 'deeppink1'
region7_col = 'red'
region8_col = 'darksalmon'
region9_col = 'darkorchid4'
region10_col = 'forestgreen'
national_col = 'black'

# plot the r^2 value vs. number of providers added
p = ggplot(data = r2_long, aes(x=index, y=value)) + geom_line(aes(colour=variable), size = 1.5) + scale_colour_discrete(name="Region") + labs(title = 'R^2 Value as Providers are Added \n to the Optimized Subset', x = 'Number of Providers in Model', y = 'R^2 value') + scale_x_continuous(breaks=c(seq(1,n_counties,by=1)))
p +   scale_color_manual(name="Region",values=c(region1_col, region2_col,
                                                region3_col, region4_col,
                                                region5_col, region6_col,
                                                region7_col, region8_col,
                                                region9_col, region10_col,
                                                national_col))
# predicted vs actual plots
# make the predicted and actual death comparison plots
names = c('National' ,paste('Region', seq(1,10)))
colnames(predicted_deaths) = c('date',names)

# in order to plot nicely, we need all the weeks that occurred in the four year window
all_dates = dates
colnames(all_dates) = 'date'
# merge predicted deaths with dates to get all dates
predicted_deaths_full = merge(all_dates, predicted_deaths, all.x = TRUE)
# merge deaths with dates to get all the dates
deaths_full =  merge(deaths,all_dates, all.y = TRUE)

# generate a sequence to be used as the x axis in the plot
start = 200800
start_yr = as.numeric(substr(start,1,4))
end = 201200
date <- as.Date(paste(start_yr, '-01-01' , sep = "")) + seq(from = 1, to = 1460, by = 7) - 1

# the following incredibly cumbersome code plots actual vs predicted time series (4 plots per page)
pdf(file = 'ILI_project/visualizations/actual_vs_predicted_dynamic_lag.pdf', onefile = TRUE, width = 8.5, height = 11)
i = 2
deaths_df = cbind(predicted_deaths_full[,i],deaths_full[,i])
counts_zoo = zoo(deaths_df,date)
colnames(counts_zoo) = c('predicted','actual')

p1 = autoplot(counts_zoo, facet = NULL) +  labs(title = colnames(predicted_deaths_full)[i], x = 'Year', y = 'Deaths') + theme(#legend.position = c(1.25,0.75),
  legend.background = element_rect(fill='#ffffffaa',colour='black'),panel.background = element_rect(fill = 'white',colour='black'), legend.key=element_rect(fill='white '), 
  axis.text = element_text(colour='black',size=10),axis.title = element_text(colour='black',size='15'),panel.grid.minor=element_blank(),panel.grid.major = element_blank()) + scale_y_continuous(expand=c(0.005,0.005))

deaths_df = cbind(predicted_deaths_full[,i+1],deaths_full[,i+1])
counts_zoo = zoo(deaths_df,date)
colnames(counts_zoo) = c('predicted','actual')

p2 = autoplot(counts_zoo, facet = NULL) +  labs(title = colnames(predicted_deaths_full)[i+1], x = 'Year', y = 'Deaths') + theme(#legend.position = c(1.25,0.75),
  legend.background = element_rect(fill='#ffffffaa',colour='black'), panel.background = element_rect(fill = 'white',colour='black'), legend.key=element_rect(fill='white '), 
  axis.text = element_text(colour='black',size=10),axis.title = element_text(colour='black',size='15'),panel.grid.minor=element_blank(), panel.grid.major = element_blank()) + scale_y_continuous(expand=c(0.005,0.005))

deaths_df = cbind(predicted_deaths_full[,i+2],deaths_full[,i+2])
counts_zoo = zoo(deaths_df,date)
colnames(counts_zoo) = c('predicted','actual')
p3 = autoplot(counts_zoo, facet = NULL) +  labs(title = colnames(predicted_deaths_full)[i+2], x = 'Year', y = 'Deaths') + theme(#legend.position = c(1.25,0.75),
  legend.background = element_rect(fill='#ffffffaa',colour='black'), panel.background = element_rect(fill = 'white',colour='black'), legend.key=element_rect(fill='white '), 
  axis.text = element_text(colour='black',size=10),axis.title = element_text(colour='black',size='15'),panel.grid.minor=element_blank(), panel.grid.major = element_blank()) + scale_y_continuous(expand=c(0.005,0.005))

deaths_df = cbind(predicted_deaths_full[,i+3],deaths_full[,i+3])
counts_zoo = zoo(deaths_df,date)
colnames(counts_zoo) = c('predicted','actual')

p4 = autoplot(counts_zoo, facet = NULL) +  labs(title = colnames(predicted_deaths_full)[i+3], x = 'Year', y = 'Deaths') + theme(#legend.position = c(1.25,0.75),
  legend.background = element_rect(fill='#ffffffaa',colour='black'), panel.background = element_rect(fill = 'white',colour='black'), legend.key=element_rect(fill='white '), 
  axis.text = element_text(colour='black',size=10),axis.title = element_text(colour='black',size='15'),panel.grid.minor=element_blank(), panel.grid.major = element_blank()) + scale_y_continuous(expand=c(0.005,0.005))

deaths_df = cbind(predicted_deaths_full[,i+4],deaths_full[,i+4])
counts_zoo = zoo(deaths_df,date)
colnames(counts_zoo) = c('predicted','actual')

p5 = autoplot(counts_zoo, facet = NULL) +  labs(title = colnames(predicted_deaths_full)[i+4], x = 'Year', y = 'Deaths') + theme(#legend.position = c(1.25,0.75),
  legend.background = element_rect(fill='#ffffffaa',colour='black'), panel.background = element_rect(fill = 'white',colour='black'), legend.key=element_rect(fill='white '), 
  axis.text = element_text(colour='black',size=10),axis.title = element_text(colour='black',size='15'),panel.grid.minor=element_blank(), panel.grid.major = element_blank()) + scale_y_continuous(expand=c(0.005,0.005))

deaths_df = cbind(predicted_deaths_full[,i+5],deaths_full[,i+5])
counts_zoo = zoo(deaths_df,date)
colnames(counts_zoo) = c('predicted','actual')

p6 = autoplot(counts_zoo, facet = NULL) +  labs(title = colnames(predicted_deaths_full)[i+5], x = 'Year', y = 'Deaths') + theme(#legend.position = c(1.25,0.75),
  legend.background = element_rect(fill='#ffffffaa',colour='black'), panel.background = element_rect(fill = 'white',colour='black'), legend.key=element_rect(fill='white '), 
  axis.text = element_text(colour='black',size=10),axis.title = element_text(colour='black',size='15'),panel.grid.minor=element_blank(), panel.grid.major = element_blank()) + scale_y_continuous(expand=c(0.005,0.005))

deaths_df = cbind(predicted_deaths_full[,i+6],deaths_full[,i+6])
counts_zoo = zoo(deaths_df,date)
colnames(counts_zoo) = c('predicted','actual')

p7 = autoplot(counts_zoo, facet = NULL) +  labs(title = colnames(predicted_deaths_full)[i+6], x = 'Year', y = 'Deaths') + theme(#legend.position = c(1.25,0.75),
  legend.background = element_rect(fill='#ffffffaa',colour='black'), panel.background = element_rect(fill = 'white',colour='black'), legend.key=element_rect(fill='white '), 
  axis.text = element_text(colour='black',size=10),axis.title = element_text(colour='black',size='15'),panel.grid.minor=element_blank(), panel.grid.major = element_blank()) + scale_y_continuous(expand=c(0.005,0.005))

deaths_df = cbind(predicted_deaths_full[,i+7],deaths_full[,i+7])
counts_zoo = zoo(deaths_df,date)
colnames(counts_zoo) = c('predicted','actual')

p8 = autoplot(counts_zoo, facet = NULL) +  labs(title = colnames(predicted_deaths_full)[i+7], x = 'Year', y = 'Deaths') + theme(#legend.position = c(1.25,0.75),
  legend.background = element_rect(fill='#ffffffaa',colour='black'), panel.background = element_rect(fill = 'white',colour='black'), legend.key=element_rect(fill='white '), 
  axis.text = element_text(colour='black',size=10),axis.title = element_text(colour='black',size='15'),panel.grid.minor=element_blank(), panel.grid.major = element_blank()) + scale_y_continuous(expand=c(0.005,0.005))

deaths_df = cbind(predicted_deaths_full[,i+8],deaths_full[,i+8])
counts_zoo = zoo(deaths_df,date)
colnames(counts_zoo) = c('predicted','actual')

p9 = autoplot(counts_zoo, facet = NULL) +  labs(title = colnames(predicted_deaths_full)[i+8], x = 'Year', y = 'Deaths') + theme(#legend.position = c(1.25,0.75),
  legend.background = element_rect(fill='#ffffffaa',colour='black'), panel.background = element_rect(fill = 'white',colour='black'), legend.key=element_rect(fill='white '), 
  axis.text = element_text(colour='black',size=10),axis.title = element_text(colour='black',size='15'),panel.grid.minor=element_blank(), panel.grid.major = element_blank()) + scale_y_continuous(expand=c(0.005,0.005))

deaths_df = cbind(predicted_deaths_full[,i+9],deaths_full[,i+9])
counts_zoo = zoo(deaths_df,date)
colnames(counts_zoo) = c('predicted','actual')

p10 = autoplot(counts_zoo, facet = NULL) +  labs(title = colnames(predicted_deaths_full)[i+9], x = 'Year', y = 'Deaths') + theme(#legend.position = c(1.25,0.75),
  legend.background = element_rect(fill='#ffffffaa',colour='black'), panel.background = element_rect(fill = 'white',colour='black'), legend.key=element_rect(fill='white '), 
  axis.text = element_text(colour='black',size=10),axis.title = element_text(colour='black',size='15'),panel.grid.minor=element_blank(), panel.grid.major = element_blank()) + scale_y_continuous(expand=c(0.005,0.005))

deaths_df = cbind(predicted_deaths_full[,i+10],deaths_full[,i+10])
counts_zoo = zoo(deaths_df,date)
colnames(counts_zoo) = c('predicted','actual')

p11 = autoplot(counts_zoo, facet = NULL) +  labs(title = colnames(predicted_deaths_full)[i+10], x = 'Year', y = 'Deaths') + theme(#legend.position = c(1.25,0.75),
  legend.background = element_rect(fill='#ffffffaa',colour='black'), panel.background = element_rect(fill = 'white',colour='black'), legend.key=element_rect(fill='white '), 
  axis.text = element_text(colour='black',size=10),axis.title = element_text(colour='black',size='15'),panel.grid.minor=element_blank(), panel.grid.major = element_blank()) + scale_y_continuous(expand=c(0.005,0.005))

grid.arrange(p1,p2,p3,p4,ncol=1,nrow=4)
grid.arrange(p5,p6,p7,p8,ncol=1,nrow=4)
grid.arrange(p9,p10,p11,ncol=1,nrow=3)
dev.off()
