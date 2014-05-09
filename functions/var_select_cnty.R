var_select_cnty <- function(obj,vars,goal=1, r2_values, ranks, type = 'mode'){
  # obj appears to be the goal timeseries
  # each provider appears to be a column in the pop matrix
  library('stringr')
  # convert dataframe of covariates into a matrix
	pop <- as.matrix(vars)
  # initialize a list of 
	selected <- numeric(0)
  # initialize a list of the R^2 values
  R2.selected<- c()
  # initialize a list of physician IDs, cities, and states
  state <- c()
  # if the desired number of covariates the user enters is greater than the number of columns,
  # then reset the goal to be the number of columns
	if(goal > ncol(pop)){
		goal <- ncol(pop)
		}
	for(k in 1:goal){
    # initializes the list of providers
		if(length(ncol(selected))==0){
      # initialize the list of r^2 values for the candidate covariates
			r2s <- c()
      # run a regression for each column in the pop matrix and record
      # the Rsquared value
			for(i in 1:ncol(pop)){
				m.i <- lm(obj ~ pop[,i])
				r2.i <- summary(m.i)$r.squared
				r2s <- c(r2s, r2.i)
				}
      # find the index of max Rsquared value
			maxR2 <- which(r2s == max(r2s))
      # check if there is a tie
			if(length(maxR2) > 1){
				cat('there was a tie','\n','\n')
        # if there was a tie for the max value then assign the maxR2 to the first one in the list
				maxR2 = maxR2[1]
			}
      #  add to the covariate matrix that has the column associated with the max Rsquared
			selected <- matrix(pop[,maxR2],ncol=1)
			colnames(selected)<-colnames(pop)[maxR2]
      #print(colnames(selected))
      R2.selected<-c(R2.selected,r2s[maxR2])
			#print(county.id)
      x = substr(colnames(pop)[maxR2],7,100)
			#x = gsub("\\."," ",x)
			#x = gsub("St ","St.",x)
			#x1 = substr(x,1,2)
			#x2 = substr(x,4,100)
			#st = paste(x1,x2,sep='-')
      st = x
      state = c(state, st)
      # take out the column of the provider who gave the max R^2
			pop <- pop[,-maxR2]
			}else{
				r2s<-c()
				for (j in 1:ncol(pop)){
				m.j <- lm(obj~cbind(selected,pop[,j]))
				r2.j <- summary(m.j)$r.squared
				r2s <- c(r2s,r2.j)
				}#end for i
			maxR2 <- which(r2s==max(r2s))
			if(length(maxR2)>1){
				cat('there was a tie','\n','\n')
				maxR2=maxR2[1]
			}
			selected <- cbind(selected,pop[,maxR2])
			colnames(selected)[k] <- colnames(pop)[maxR2]
			R2.selected <- c(R2.selected,r2s[maxR2])
			x = substr(colnames(pop)[maxR2],7,100)
      st = x
			state = c(state, st)
			pop<-pop[,-maxR2]
			}#end if/else
		}#end for k
  ranks = cbind(ranks, state)
  r2_values = cbind(r2_values,R2.selected)
  save(r2_values,file = paste('ILI_project/data/regression_results/r2_values_',type,'_lag.Rda', sep = ''))
	return(ranks)
}#end func