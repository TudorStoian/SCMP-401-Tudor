
#For testing purposes I will use the dataset used in the book "An Introduction to the Bootstrap " by Bradley Effron and Robert J. Tibshirani
install.packages("bootstrap") #if the R studio in question does not already have it installed.
library(bootstrap)
data("law")


#Here I test out how well randomly selecting a number works
test_prob=c()
k=10000
for(i in 1:k ){
test_prob[i]= sample(1:15, 1)
}
hist(test_prob)
#Note that the graph looks weird for the first observation, it's a problem with the settings of the histogram:
#For some reason the columns for 1 and 2 are displayed together, but there is no symmetrical feature for 14 and 15.

counter=0
for(i in 1:k){
  if(test_prob[i] ==1 || test_prob[i] ==2){
    counter= counter + 1
  }
}

print(counter)
#Seems like this works


# Here I'll try to compute the relevant statistic for a resampling, obviously how difficult this is depends on the statistic in question
#For now, I'll try to implement the mean and median for the resamples.
sample1=c()
for(i in 1:15){
  sample1[i]=sample( 1:15, 1)
}
mean(sample1)
median(sample1)

#Here I'll extend the algorithm to perform to generate B samples and generate the 
B= 1000 #This is the number of bootstrap replications 

matrix_of_statistics=c()
for(j in 1:B ){
  sample1=c() #We have an option here, we could commit to memory all the resamplings we obtained, but really we only care about the sample statistic
  for(i in 1:15){
    sample1[i]= law$GPA[ sample( 1:15, 1)] 
  }
  matrix_of_statistics[j]= mean(sample1)
}
hist(matrix_of_statistics)
summary(matrix_of_statistics)
#Question to ponder: is it worth sorting the data ahead of processing? At first it seems harmless, but since we resample, perhaps it has no effect


#I can create an empirical 95% confidence interval by ordering these statistics, and then choosing the relevant percentile
sorted_statistics= sort(matrix_of_statistics)

left_end= sorted_statistics[26]
right_end= sorted_statistics[976]

interval_statistics=c(left_end,right_end)
interval_statistics

hist(law$GPA)
t.test(law$GPA )
median(law$GPA)

#Next I compute the standard error using the bootstrap, as detailed in Effron and Tibshirani

boot_mean=bootstrap(law$GPA,1000,mean)
hist(boot_mean$thetastar)




#There are a number of complications with using the built-in 

#abcnon(law$GPA,mean, alpha = 0.05)

#Might be worth getting more data, and I know Stat2 has a lot of it available.
library(Stat2Data)
data("Cereal")
