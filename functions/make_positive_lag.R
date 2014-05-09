make_positive_lag = function(ili_reg, lag){
  ili_reg1 = ili_reg[ili_reg$date >= 200840 & ili_reg$date <= 200921,]
  ili_reg2 = ili_reg[ili_reg$date >= 200940 & ili_reg$date <= 201021,]
  ili_reg3 = ili_reg[ili_reg$date >= 201040 & ili_reg$date <= 201121,]
  ili_reg4 = ili_reg[ili_reg$date >= 201140 & ili_reg$date <= 201221,]
  lag_df1 = data.frame(ili_reg1[,2])
  lag_df2 = data.frame(ili_reg2[,2])
  lag_df3 = data.frame(ili_reg3[,2])
  lag_df4 = data.frame(ili_reg4[,2])
  d1 = c(rep(NA,lag), ili_reg1[1:(nrow(lag_df1)-lag),2])
  d2 = c(rep(NA,lag), ili_reg2[1:(nrow(lag_df2)-lag),2])
  d3 = c(rep(NA,lag), ili_reg3[1:(nrow(lag_df3)-lag),2])
  d4 = c(rep(NA,lag), ili_reg4[1:(nrow(lag_df4)-lag),2])
  lagged = c(d1,d2,d3,d4)
  return(lagged)
}