# which years have 53?  2003, 2008, 2014(?)
# starts week 1 of 2001, ends week 53 of 2014
a52 <- seq(1, 52, 1) #creates sequences with which to match the ILInet data
a53 <- seq(1, 53, 1)
a52[1:9] <- paste("0", as.character(a52)[1:9], sep="") #adds a 0 to the ones less than 10
a53[1:9] <- paste("0", as.character(a53)[1:9], sep="")
# note: assuming 53 weeks in 2014 CDC year (check if this is correct!)
years <- c(rep(2001, 52), rep(2002, 52), rep(2003, 53), rep(2004, 52), rep(2005, 52), #makes a sequence of Year labels of the right length
           rep(2006, 52), rep(2007, 52), rep(2008, 53), rep(2009, 52), rep(2010, 52),
           rep(2011, 52), rep(2012, 52), rep(2013, 52), rep(2014, 53))
weeks <- c(a52, a52, a53, a52, a52, a52, a52, a53, a52, a52, a52, a52, a52, a53) # makes a sequence of week labels of the same legnth
yearWeeks <- paste(years, weeks, sep="") #makes into CDC format

#hard code the first week beginning, and the first weekEnding
weekBeginning <- seq.Date(as.Date("2000-12-31"), as.Date("2014-12-28"), 7)
weekEnding <- seq.Date(as.Date("2001-01-06"), as.Date("2015-01-03"), 7)

#make a Lookup table for beginning and end date of each CDC week
dateLookUp <- data.frame(yearWeeks, weekBeginning, weekEnding)

#make a Lookup table that matches each date to its CDC week
dailyToWeek <- function(ymd){
	dateLookUp[which(dateLookUp$weekBeginning<=as.Date(ymd) & dateLookUp$weekEnding>=as.Date(ymd)), 1]	
}
date <- seq.Date(as.Date("2000-12-31"), as.Date("2015-01-03"), 1)
dateWeekTable <- data.frame(date, sapply(date, dailyToWeek))
colnames(dateWeekTable) <- c('date', 'CDCdate')


# test
#dateLookUp[match(as.Date("2001-09-30"), dateLookUp[, 2]),3]
#which(dateLookUp$weekBeginning<=as.Date("2010-10-02") & dateLookUp$weekEnding>=as.Date("2010-10-02"))