library(ISLR)
names(Smarket)
summary(Smarket)
pairs(Smarket)

cor(Smarket[, -9])
attach(Smarket)
plot(Volume)

# Logistic regression
glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
               data=Smarket, family=binomial)
summary(glm.fit)

coef(glm.fit)
summary(glm.fit)$coef

glm.probs <- predict(glm.fit, type='response')
glm.probs[1:10]
contrasts(Direction)

glm.pred <- ifelse(glm.probs > 0.5, 'Up', 'Down')
table(glm.pred, Direction)
mean(glm.pred == Direction)

train <- (Year < 2005)
Smarket.2005 <- Smarket[!train, ]
dim(Smarket.2005)
Direction.2005 <- Direction[!train]

glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
               data=Smarket, family=binomial, subset=train)
glm.probs <- predict(glm.fit, Smarket.2005, type='response')

glm.pred <- ifelse(glm.probs > 0.5, 'Up', 'Down')
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)

glm.fit <- glm(Direction ~ Lag1 + Lag2, data=Smarket, family=binomial,
               subset=train)
glm.probs <- predict(glm.fit, Smarket.2005, type='response')
glm.pred <- ifelse(glm.probs > 0.5, 'Up', 'Down')
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)


# LDA
library(MASS)
lda.fit <- lda(Direction ~ Lag1 + Lag2, data=Smarket, subset=train)
lda.fit
plot(lda.fit)

lda.pred <- predict(lda.fit, Smarket.2005)
names(lda.pred)
lda.class <- lda.pred$class
table(lda.class, Direction.2005)
mean(lda.class == Direction.2005)


# Quadratic Disctiminant Analysis
qda.fit <- qda(Direction ~ Lag1 + Lag2, data=Smarket, subset=train)
qda.fit
qda.class <- predict(qda.fit, Smarket.2005)$class
table(qda.class, Direction.2005)
mean(qda.class == Direction.2005)


# K-NN
library(class)
train.X <- cbind(Lag1, Lag2)[train,]
test.X <- cbind(Lag1, Lag2)[!train,]
train.Direction <- Direction[train]

set.seed(1)
knn.pred <- knn(train.X, test.X, train.Direction, k=1)
table(knn.pred, Direction.2005)
mean(knn.pred == Direction.2005)

for (i in 1:5) {
  knn.pred <- knn(train.X, test.X, train.Direction, k=i)
  print(c(i, mean(knn.pred == Direction.2005)))
}


dim(Caravan)
attach(Caravan)
summary(Purchase)

standardized.X = scale(Caravan[, -86])
var(Caravan[,1])
var(standardized.X[,1])

test <- 1:1000
train.X <- standardized.X[-test,]
test.X <- standardized.X[test,]
train.Y <- Purchase[-test]
test.Y <- Purchase[test]
set.seed(1)
knn.pred <- knn(train.X, test.X, train.Y, k=5)
mean(test.Y != knn.pred)
mean(test.Y != 'No')
table(knn.pred, test.Y)
