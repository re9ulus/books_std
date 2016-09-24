library(MASS)
library(ISLR)
library(car)

?Boston
fix(Boston)
names(Boston)

# Simple linear model
lm.fit <- lm(medv~lstat, data=Boston)

lm.fit
summary(lm.fit)

# Features of lm.fit
names(lm.fit)

# Coefficients
coef(lm.fit)

# Confident interval
confint(lm.fit)

# Confidence and prediction intervals for the predictions
predict(lm.fit, data.frame(lstat=(c(5, 10, 15))),
        interval="confidence")
predict(lm.fit, data.frame(lstat=(c(5,10,15))),
        interval="prediction")

?data.frame

# Plots
plot(Boston$lstat, Boston$medv, pch="+")
abline(lm.fit, lwd=2, col="red")

par(mfrow=c(2, 2))
plot(lm.fit)

par(mfrow=c(1, 1))
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))

plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))


### Multiple linear regression

lm.fit = lm(medv~lstat+age, data=Boston)
summary(lm.fit)

# Using all variables
lm.fit = lm(medv~., data=Boston)
summary(lm.fit)

summary(lm.fit)$r.sq
summary(lm.fit)$sigma # RSE
vif(lm.fit)

# Using all variables except age
lm.fit1 = lm(medv~.-age, data=Boston)
summary(lm.fit1)


### Interception term
# lstat * age == lstat + age + lstat*age (lower predictors included automatically)
summary(lm(medv ~ lstat * age, data = Boston))

### Non-linear tranformations of the predictors
# I() is used, cause ^ is special simbol in formula
lm.fit2 = lm(medv~lstat+I(lstat^2), data=Boston)
summary(lm.fit2)

lm.fit = lm(medv~lstat, data=Boston)
anova(lm.fit, lm.fit2)

par(mfrow=c(2, 2))
plot(lm.fit2)

# To produce polynoms of higher form better to use "poly" function
lm.fit5 = lm(medv~poly(lstat, 5), data=Boston)
summary(lm.fit5)


### Qualitative predictors
names(Carseats)
# R generate dummy varaible sutomatically
lm.fit = lm(Sales~.+Income:Advertising+Price:Age, data=Carseats)
summary(lm.fit)

# contrasts return the coding for dummy variables
contrasts(Carseats$ShelveLoc)
Carseats$ShelveLoc


### Writing functions

LOadLibraries <- function() {
  library(ISLR)
  library(MASS)
  print("The libraries have benn loaded.")
}
LOadLibraries()
