
#read in the file
bg.surv <- read.csv( file = "big-data-survey-2014-fall-interests.csv")
rownames( bg.surv ) <- bg.surv[,1]

#make this a numeric matrix
simData<- as.matrix(bg.surv[,8:21])

maxCor<- -1
maxCor.i <- integer()
#Loop through dataset
for(i in 1:nrow(simData))
{
	#if the row of data is not similar to my row of data 
	if(i != 6 ){
		#put it in a temp var and have them compare the correlation
		tempCor <- cor(simData[i,], simData[6,] )
		# if temp is greater than the max cor value
		# replace and have that the max cor value 
		if(tempCor > maxCor){
			maxCor <- tempCor
			maxCor.i<-i
		}
	}
}
#return the values in a list 
return(list(maxCor=maxCor,maxCor.i= maxCor.i))
print(list(maxCor=maxCor,maxCor.i= maxCor.i))
similarityData<-c(maxCor=maxCor,maxCor.i= maxCor.i)

#plot the histogram
hist(simData, xlim=c(0,10),ylim=c(0,145))


# get the max and min values
max_num<-which.max(similarityData)
min_num<-which.min(similarityData)

# print the the people I am most related to 
print(paste0("The person I am most related to is ",bg.surv[max_num,1]))
print(paste0("The person I am least related to is " ,bg.surv[min_num,1]))


