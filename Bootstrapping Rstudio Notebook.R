
#For testing purposes I will use the dataset used in the book "An Introduction to the Bootstrap " by Bradley Effron and Robert J. Tibshirani
install.packages("bootstrap") #if the R studio in question does not already have it installed.
library(bootstrap)
data("law")

summary(law$GPA)
hist(law$GPA)


################# TESTING SAMPLING ROUTINES###########################
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
for(i in 1:15){ #I have chosen 15, since that is the sample size for the law dataset.
  sample1[i]=sample( 1:15, 1)
}
mean(sample1)
median(sample1)

################################################################### Tibshirani implementation


boot_mean=bootstrap(law$GPA,1000,median) 
str(boot_mean)  #Notice the type of object that comes out of bootstrap
hist(boot_mean$thetastar)





#abcnon(law$GPA,mean_function, alpha = 0.05)


theta <- function(p,x) {sum(p*x)/sum(p)}
results <- abcnon(law$GPA, theta)   #Format goes against convention in Statistics.
str(results)
results$limits
#I see now how to compare these, although it is very counter-intuitive:
#When looking at limits, I look at the 0.025 and 0.975 abc entries.
#This is very close to what I got empirically, although it seems shifted.
#I am still confused about what p is (the vector of proportions ,whatever that is)
#and why it is actually needed.
#The output also seems needlessly unintuitive , 


#After talking with Professor Hartlaub, we have decided that the vector of proportions indicates the number of times a given value appears
#in the original set of observations (think frequency , like a histogram). I was perplexed why we would care, but the Professor told me 
#that it's a quirk of the way they implemented, and them being careful about ties.


################################################################### My own implementation


#Here I'll extend the algorithm to perform to generate B samples and generate the relevant statistic
B= 1000 #This is the number of bootstrap replications 

matrix_of_statistics=c()
for(j in 1:B ){
  sample1=c() #We have an option here, we could commit to memory all the resamplings we obtained, but really we only care about the sample statistic
  for(i in 1:15){
    sample1[i]= law$GPA[ sample( 1:15, 1)] 
  }
  matrix_of_statistics[j]= mean(sample1) 
  #matrix_of_statistics[j]= IQR(sample1)  # THIS IS THE FUNCTION SPECIFIER
}
hist(matrix_of_statistics)
summary(matrix_of_statistics)
#Question to ponder: is it worth sorting the data ahead of processing? At first it seems harmless, but since we resample, perhaps it has no effect
#Answer: Yes, there is no point, since resampling eliminates any benefit from it.

sorted_statistics= sort(matrix_of_statistics)

#One other alternative is to take the mean and standard deviation of the observations, where we can use the normality assumption:
mean(sorted_statistics)
sd(sorted_statistics)

left_end= mean(sorted_statistics) - 2 * sd(sorted_statistics)
right_end= mean(sorted_statistics) + 2 * sd(sorted_statistics)

interval_statistics2=c(left_end,right_end)
interval_statistics2



#I can create an empirical 95% confidence interval by ordering these statistics, and then choosing the relevant percentile


left_end= sorted_statistics[25]
right_end= sorted_statistics[975]

interval_statistics=c(left_end,right_end)
interval_statistics



#Yet another version "flips" the offsets to the mean. E.g. if 2.5 percentile is 10 units below the mean , and 97.5 percentile is 6 units above the mean, 
#We create an interval that goes from 6 units below to 10 units above. It is not obvious why we do this, but to the extent that I understand this
#this has the effect of carrying through the effects of transformations. Might want to doublecheck this point.
left=mean(sorted_statistics) - ( sorted_statistics[976] - mean(sorted_statistics))
right= mean(sorted_statistics) + ( mean(sorted_statistics) - sorted_statistics[26])

interval_statistics3=c(left,right)
interval_statistics3

###################################### Looking at the source code

bootstrap:::abcnon

bootstrap:::bootstrap

####################################### Defining a function

tudor_bootstrap <- function( dataset, method = mean, B=1000, signficance_level= 0.95){
  n= length(dataset)
  
  matrix_of_statistics=c()
  for(j in 1:B ){
    sample1=c() #We have an option here, we could commit to memory all the resamplings we obtained, but really we only care about the sample statistic
    for(i in 1:n){
      sample1[i]= dataset[ sample( 1:n, 1)] 
    }
    matrix_of_statistics[j]= method(sample1) 
    #matrix_of_statistics[j]= IQR(sample1)  # THIS IS THE FUNCTION SPECIFIER
  }
  
  sorted_statistics= sort(matrix_of_statistics)
  return( c( sorted_statistics[B* (1 - signficance_level)/2],sorted_statistics[B* (signficance_level +(1 - signficance_level)/2)] ) )
}

tudor_bootstrap(dataset = law$GPA ,method = mean,B = 100,signficance_level = 0.95)

