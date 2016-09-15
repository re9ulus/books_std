# Tasks of part2 of ISLR.
# Boring tasks

install.packages("ISLR")

library("ISLR")
### Task 8

# a. Read data read.csv
College

# b. Look at the data with "fix" function 
fix(College)

# c.
# i. use summary()
summary(College)
# ii. use pairs() to produce scatterplots
pairs(College[,1:10])
# iii. use plot()
plot(College$Outstate, College$Private)
# iv. create Elite variable
Elite = rep("No", nrow(College))
Elite[College$Top10Perc > 50] = "Yes"
Elite=as.factor(Elite)
College=data.frame(College, Elite)
summary(College)
boxplot(College$Outstate, College$Elite)
# v. use hist
par(mfrow=c(1,2))
hist(College$Accept)
hist(College$PhD)

par(mfrow=c(1,1))
### Task 9
# Make sure thah missing values have been removed
any(is.na(Auto))
# a. Quantitive and qualitative predictors
summary(Auto)
# b. Range
colnames(Auto)
range(Auto$mpg)
range(Auto$weight) # etc
# c. Mean and Std
mean(Auto$mpg)
sd(Auto$mpg)
# e. Plots
plot(Auto$mpg, Auto$cylinders)


## Task 10
library(MASS)
Boston
?Boston
nrow(Boston)
ncol(Boston)
# b. plots
plot(Boston$crim, Boston$chas)
plot(Boston$dis, Boston$crim)
Boston$dis
# e. near river
sum(Boston$chas)
# f. median pupil-teacher among dataset
median(Boston$ptratio)
# ans so on.
