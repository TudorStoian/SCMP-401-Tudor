
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

####################################### 1st sample bootstrap

tudor_bootstrap <- function( dataset, method = mean, B=1000, confidence_level= 0.95, interval_type= "percentile",show_histogram= TRUE){
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
  summary(sorted_statistics)
  print("The mean is ")
  print(mean(sorted_statistics))
  print("The standard error is")
  print(sd(sorted_statistics))
  if(show_histogram==TRUE){
    hist(sorted_statistics)}
  if(interval_type == "percentile"){
    return( c( sorted_statistics[B* (1 - confidence_level)/2],sorted_statistics[B* (confidence_level +(1 - confidence_level)/2)] ) )
  }
  if(interval_type ==  "normal"){
    
    left_end= mean(sorted_statistics) - qt(confidence_level,n-1) * sd(sorted_statistics)
    right_end= mean(sorted_statistics) + qt(confidence_level,n-1) * sd(sorted_statistics)
    
    return(c(left_end,right_end))
  }
  if(interval_type == "backwards"){
    left=mean(sorted_statistics) - ( sorted_statistics[B* (confidence_level +(1 - confidence_level)/2)] - mean(sorted_statistics))
    right= mean(sorted_statistics) + ( mean(sorted_statistics) - sorted_statistics[B* (1 - confidence_level)/2])
    return(c(left,right))
  }
}

library(Stat2Data)
data("Blood1")

tudor_bootstrap(dataset = Blood1$SystolicBP ,method = mean,B = 100,confidence_level = 0.98)
tudor_bootstrap(dataset = law$GPA ,method = mean,B = 100,confidence_level = 0.95,interval_type = "percentile")
tudor_bootstrap(dataset = law$GPA ,method = mean,B = 100,confidence_level = 0.95,interval_type = "normal")
tudor_bootstrap(dataset = law$GPA ,method = mean,B = 100,confidence_level = 0.95,interval_type = "backwards")


crazy_method = function(X){
  return(abs(mean(X)))
}

in_class_crazy_method = function(X){
  
}


tudor_bootstrap(dataset = law$GPA ,method = crazy_method,B = 100,confidence_level  = 0.95)

################################# 2 Sample version

tudor_bootstrap2 <- function( dataset1,dataset2 ,method = cor, B=1000, confidence_level= 0.95, interval_type= "percentile",show_histogram=TRUE,paired=TRUE){
  n= length(dataset1)
  n2=length(dataset2)
  matrix_of_statistics=c()
  for(j in 1:B ){
    sample1=c() #We have an option here, we could commit to memory all the resamplings we obtained, but really we only care about the sample statistic
    sample2=c()
    if(paired==TRUE){
      for(i in 1:n){
        roll=sample( 1:n, 1)
        sample1[i]= dataset1[ roll]
        sample2[i]= dataset2[roll]
      }
    }
    if(paired==FALSE){
      for(i in 1:n){
        roll=sample( 1:n, 1)
        sample1[i]= dataset1[ roll]
      }
      for(i in 1:n2){
        roll=sample( 1:n2, 1)
        sample2[i]= dataset2[roll]
      }
    }
    matrix_of_statistics[j]= method(sample1,sample2) 
    #matrix_of_statistics[j]= IQR(sample1)  # THIS IS THE FUNCTION SPECIFIER
  }
  
  sorted_statistics= sort(matrix_of_statistics)
  summary(sorted_statistics)
  print("The mean is ")
  print(mean(sorted_statistics))
  print("The standard error is")
  print(sd(sorted_statistics))
  if(show_histogram==TRUE){
    hist(sorted_statistics)}
  if(interval_type == "percentile"){
    return( c( sorted_statistics[B* (1 - confidence_level)/2],sorted_statistics[B* (confidence_level +(1 - confidence_level)/2)] ) )
  }
  if(interval_type ==  "normal"){
    
    left_end= mean(sorted_statistics) - qt(confidence_level,n-1) * sd(sorted_statistics)
    right_end= mean(sorted_statistics) + qt(confidence_level,n-1) * sd(sorted_statistics)
    
    return(c(left_end,right_end))
  }
  if(interval_type == "backwards"){
    left=mean(sorted_statistics) - ( sorted_statistics[B* (confidence_level +(1 - confidence_level)/2)] - mean(sorted_statistics))
    right= mean(sorted_statistics) + ( mean(sorted_statistics) - sorted_statistics[B* (1 - confidence_level)/2])
    return(c(left,right))
  }
}
cor(law$LSAT,law$GPA)

slope <- function(X,Y){
  a=lm(X~Y)$coefficients[2]
  return(a)
}


tudor_bootstrap2(dataset1 = law$LSAT,dataset2 = law$GPA,method = slope)


##################### K-sample version

tudor_bootstrapk= function( data_matrix,method, B=1000, confidence_level= 0.95, interval_type= "percentile",show_histogram=TRUE,paired=TRUE){
  number_of_samples= length(data_matrix[1,])
  length_matrix=c()
  for(i in 1:number_of_samples){
    length_matrix[i]= length(data_matrix[,i])
  }
  
  matrix_of_statistics=c()
  for(j in 1:B ){
    sample_matrix= matrix(ncol = number_of_samples,nrow = max(length_matrix))
    if(paired==TRUE){
      roll_matrix=  sample( 1:length_matrix[1], length_matrix[1], replace = TRUE) 
      for(k in 1:number_of_samples){
        for(i in 1:length_matrix[1]){
          sample_matrix[i,k]= data_matrix[ roll_matrix[i],k]
        }
      }
    }
    
    if(paired==FALSE){
      for(k in 1:number_of_samples){
        roll_matrix=  sample( 1:length_matrix[k], length_matrix[k], replace = TRUE)
        for(i in 1:length_matrix[k]){
          sample_matrix[i,k]= data_matrix[ roll_matrix[i],k]
        }
      }
    }
    matrix_of_statistics[j]= method(sample_matrix) 
  }
  
  sorted_statistics= sort(matrix_of_statistics)
  summary(sorted_statistics)
  print("The mean is ")
  print(mean(sorted_statistics))
  print("The standard error is")
  print(sd(sorted_statistics))
  if(show_histogram==TRUE){
    hist(sorted_statistics)}
  if(interval_type == "percentile"){
    return( c( sorted_statistics[B* (1 - confidence_level)/2],sorted_statistics[B* (confidence_level +(1 - confidence_level)/2)] ) )
  }
  if(interval_type ==  "normal"){
    
    left_end= mean(sorted_statistics) - qt(confidence_level,n-1) * sd(sorted_statistics)
    right_end= mean(sorted_statistics) + qt(confidence_level,n-1) * sd(sorted_statistics)
    
    return(c(left_end,right_end))
  }
  if(interval_type == "backwards"){
    left=mean(sorted_statistics) - ( sorted_statistics[B* (confidence_level +(1 - confidence_level)/2)] - mean(sorted_statistics))
    right= mean(sorted_statistics) + ( mean(sorted_statistics) - sorted_statistics[B* (1 - confidence_level)/2])
    return(c(left,right))
  }
}



intercept_k_compact <- function(datamatrix){
  k= length(datamatrix[1,]) 
  observations= length(datamatrix[,1])
  except_first= matrix(nrow = observations,ncol = k-1)
  for(i in 1:(k-1)){
  except_first[,i]=datamatrix[,i+1]   
  }
  
  a=lm(datamatrix[,1]~except_first)$coefficients[1]
  return(a)
}

library(Stat2Data)
data("MedGPA")

#Gonna try a baseline to see what I'm comparing against.

lm(MedGPA$BCPM~MedGPA$GPA+MedGPA$VR+MedGPA$PS)



testMed= matrix(nrow=55 ,ncol = 3)
testMed[,1]= MedGPA$GPA
testMed[,2]= MedGPA$VR
testMed[,3]= MedGPA$PS

lm(testMed[,1]~testMed[,2]+testMed[,3])



intercept_k_compact(testMed)

tudor_bootstrapk( data_matrix = testMed, method = intercept_k_compact)
