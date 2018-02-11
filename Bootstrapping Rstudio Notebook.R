
#For testing purposes I will use the dataset used in the book "An Introduction to the Bootstrap " by Bradley Effron and Robert J. Tibshirani
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
