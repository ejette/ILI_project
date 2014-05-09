# make_provider_features.R calculates the percentage of patients 65 and older in every provider county
# and the seasonal and non-seasonal reporting rates for every provider county
rm(list=ls())

setwd('~/Dropbox')

load('ILI_project/data/logistic/ili_over_65.Rda')

ili_trim_cnty = ili_over_65[order(ili_over_65$FIPS_id,ili_over_65$date),]
ili_cnty_agg <- aggregate(. ~ date + FIPS_id, data = ili_trim_cnty, FUN = sum)
ili_cnty_agg$total_pt[ili_cnty_agg$cases == 0 & ili_cnty_agg$total_pt == 0] = 1
ili_cnty_agg$total_cases_pct = ili_cnty_agg$cases/ifelse(ili_cnty_agg$total_pt == 0, NA, ili_cnty_agg$total_pt)
ili_cnty_agg$cases_over_65_pct = ili_cnty_agg$over_65/ifelse(ili_cnty_agg$total_pt == 0, NA, ili_cnty_agg$total_pt)

ili_cnty_agg$week_num = as.numeric(substr(ili_cnty_agg$date,5,9))

#over 65
num65<-by(ili_cnty_agg[,'over_65'],ili_cnty_agg[,'FIPS_id'],sum,na.rm=TRUE)

den<-by(ili_cnty_agg[,'total_pt'],ili_cnty_agg[,'FIPS_id'],sum,na.rm=TRUE)

p65<-matrix(num65)/matrix(den)
per65<-data.frame(c(names(den),'12086'),as.matrix(c(log(p65),log(p65[which(names(den)=='12086')]))))

colnames(per65)<-c('FIPS','perOver65Reported')
per65[which(is.finite(per65[,'perOver65Reported'])==FALSE),'perOver65Reported']<-0

save(per65, file='ILI_project/data/logistic/per65.Rda')

#reporting rate (full year)
nDates<-length(unique(ili_cnty_agg[,'date']))
reporting<-c()
for(i in per65[,'FIPS']){
	use.i<-which(as.character(ili_cnty_agg[,'FIPS_id'])==as.character(i))
	if(as.character(i)=='12086'){
		len.i<-length(use.i)/2
	}else{
		len.i<-length(use.i)
	}
	reporting<-c(reporting, len.i/nDates)
}

reportFullYear<-data.frame(per65[,'FIPS'], reporting)
colnames(reportFullYear)<-c('FIPS','reportingRateFullYear')

save(reportFullYear, file='ILI_project/data/logistic/reportingFullYear.Rda')

#reporting rate (flu season)
ili_cnty_agg_flu<-ili_cnty_agg[which(ili_cnty_agg[,'week_num']>39|ili_cnty_agg[,'week_num']<21),]

nDates<-length(unique(ili_cnty_agg_flu[,'date']))
reporting<-c()
for(i in per65[,'FIPS']){
	use.i<-which(as.character(ili_cnty_agg_flu[,'FIPS_id'])==as.character(i))
	if(as.character(i)=='12086'){
		len.i<-length(use.i)/2
	}else{
		len.i<-length(use.i)
	}
	reporting<-c(reporting, len.i/nDates)
}

reportFluSea<-data.frame(per65[,'FIPS'], reporting)
colnames(reportFluSea)<-c('FIPS','reportingRateFluSea')

save(reportFluSea, file='ILI_project/data/logistic/reportingRateFluSea.Rda')

#just for fun
plot(reportFullYear[,'reportingRateFullYear'], reportFluSea[,'reportingRateFluSea'],xlab='reporting rate full year',ylab='reporting rate flu season (weeks 40 - 20)',pch=16, col='#00000085')
abline(0,1,lty=3,lwd=3,col='#d6604d')