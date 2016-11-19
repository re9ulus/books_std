library(ISLR)

# Validation set approach
set.seed(1)
train = sample(392, 196)
lm.fit <- lm(mpg~horsepower, data=Auto, subset=train)

attach(Auto)
mean((mpg - predict(lm.fit, Auto))[-train]^2)

## Try polynomial models
lm.fit2 <- lm(mpg~poly(horsepower, 2), data=Auto, subset=train)
mean((mpg - predict(lm.fit2, Auto))[-train]^2)
lm.fit3 <- lm(mpg~poly(horsepower, 3), data=Auto, subset=train)
mean((mpg - predict(lm.fit3, Auto))[-train]^2)


# Leave-One-Out Cross-Validation
library(boot)
glm.fit <- glm(mpg~horsepower, data=Auto)
cv.err <- cv.glm(Auto, glm.fit)
cv.err$delta
coef(glm.fit)

cv.error <- rep(0, 5)
for (i in 1:5) {
  glm.fit <- glm(mpg~poly(horsepower, i), data=Auto)
  ## cv.glm performs LOOCV
  cv.error[i] <- cv.glm(Auto, glm.fit)$delta[1]
}
cv.error


# k-fold Cross-Validation
set.seed(17)
cv.error.10 <- rep(0, 10)
for(i in 1:10) {
  glm.fit <- glm(mpg~poly(horsepower,i), data=Auto)
  ## cl.glm with K param performs K-fold cross-val
  cv.error.10[i] <- cv.glm(Auto, glm.fit, K=10)$delta[1]
}
cv.error.10


# Bootstrap
alpha.fn <- function(data, index) {
  X <- data$X[index]
  Y <- data$Y[index]
  return((var(Y) - cov(X,Y)) / (var(X) + var(Y) - 2 * cov(X, Y)))
}

set.seed(1)
alpha.fn(Portfolio, sample(100, 100, replace = TRUE))

## perform bootstrap
boot(Portfolio, alpha.fn, R=1000)

# Estimating linear model with bootstrap
boot.fn <- function(data, index) {
  return(coef(lm(mpg~horsepower, data=data, subset=index)))
}
boot.fn(Auto, 1:392)

set.seed(1)
boot.fn(Auto, sample(392, 392, replace=TRUE))
boot.fn(Auto, sample(392, 392, replace=TRUE))

boot(Auto, boot.fn, 1000)
summary(lm(mpg~horsepower, data=Auto))$coef

boot.fn <- function(data, index) {
  coefficients(lm(mpg~horsepower+I(horsepower^2),data=data,
                  subset=index))
}
set.seed(1)
boot(Auto, boot.fn, 1000)

summary(lm(mpg~horsepower+I(horsepower^2), data=Auto))$coef
