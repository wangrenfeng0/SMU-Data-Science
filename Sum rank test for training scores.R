

# New vs Traditional  Go With NEW = 1

# Rank Sum Distribution


NewTrad = data.frame(Student = c("Bob","Sue", "Fred","Jim","Pam","Tim","Zac"))
NewTrad$Method = c("T","T","N","T","N","N","N")
NewTrad$Score = c(23,31,37,46,49,55,77)

simulations = 10000
RankSumHolder = numeric(simulations)
ranks = c(1,2,3,4,5,6,7)

for(i in 1:simulations)
{
  
  #shuffle labels
  relables  = sample(c("Trad","Trad","Trad", "New", "New", "New", "New"),7)
  # Code New = 1 and Trad = 0
  relablesNum = as.numeric(relables == "Trad") #'Trad' will be assigned to 1 and 'New' will be assigned to 0
  # The above code will make a vector like (1,0,1,1,0,0,1)
  # Mulitply this vector times the rank vector: (1,2,3,4,5,6,7) and 
  # you will be left with the ranks of the New. 
  # The next line of code sums those ranks up. ... thus you have the sum of the ranks. 
  RankSumHolder[i] = sum(relablesNum*ranks) #RankSumHolder[i] holds all 'Trad' labels ranks
  # This code simply shoots out the first 5 permutation and thus the first 5 sum of the ranks. 
  if(i < 6)
  {
    NewTrad2 = NewTrad
    NewTrad2$Method = relables
    print(NewTrad2)
    print(paste("Sum of Ranks: ",RankSumHolder[i]))
  }
  
}

# This code creates the histograms of the sum of the ranks of the many simulated experiments under Ho
hist(RankSumHolder, col = "blue", main = "Estimation of the Distribution of the Rank Sum Statistic", xlab = "Sum of the Ranks per Experiment", breaks = 40)
