##THE STOCK MARKET DATA

#loading packages
library(ISLR)

#examining stock market data
?Smarket
names(Smarket)
summary(Smarket)

#matrix containing pairwise correlations among the predictors in a data set
#must include [,-9] because the Direction variable is qualitative, can only use quantitative variables
cor(Smarket[,-9]) #appears to be little correlation between today's returns and previous days' returns (lags), only Year and Volume show substantial correlation

#plotting Volume
plot(Smarket$Volume)

##LOGISTIC REGRESSION

#fitting using the glm() function
#the glm() function fits generalized linear models, a class of models that includes logistic regression
#must pass the argument family=binomial to tell R to run a logistic regression
contrasts(Smarket$Direction) #R has created a dummy variable 1 for Up
glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket, family = binomial)
summary(glm.fit) #Lag1's negative coefficient suggests that if the market had a positive return yesterday, then it is less likely to go up today
coef(glm.fit)
summary(glm.fit)$coef

#predicting binary qualitative variable
#type="response" option tells R to output probabiliities of the form P(Y=1|X)
#P(Y=1|X) here is the probability that the market will go up, as 1=dummy variable for Up
#with no data set specified, R will use the training data used to generate the model
glm.probs <- predict(glm.fit, type = "response")
glm.probs[1:10]

#creating a vector of Up or Down class predictions based on predicted probability
glm.pred <- rep("Down", 1250)
glm.pred[glm.probs > .5] <- "Up"

#confusion matrix to determine how many observations were correctly or incorrectly classified
table(glm.pred, Smarket$Direction)
(507 + 145) / 1250
mean(glm.pred == Smarket$Direction)

#fitting model using part of data and predicting with held out data
train <- subset(Smarket, Year < 2005)
Smarket.2005 <- subset(Smarket, Year == 2005)
dim(Smarket.2005)

#fitting logistic regression model and predicting using held out data
glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = train, family = binomial)
glm.probs <- predict(glm.fit, Smarket.2005, type = "response")

#compute predictions for 2005 and compare to actual market movements in 2005
glm.pred <- rep("Down", 252)
glm.pred[glm.probs > .5] <- "Up"
table(glm.pred, Smarket.2005$Direction)
mean(glm.pred == Smarket.2005$Direction)
mean(glm.pred != Smarket.2005$Direction)

#simpler logistic regression removing non-significant predictors
glm.fit <- glm(Direction ~ Lag1 + Lag2, data = train, family = binomial)
glm.probs <- predict(glm.fit, Smarket.2005, type = "response")
glm.pred <- rep("Down", 252)
glm.pred[glm.probs > .5] <- "Up"
table(glm.pred, Smarket.2005$Direction)
mean(glm.pred == Smarket.2005$Direction)
106 / (106 + 76)

#predicting with specific values
#preicting Direction on a day when Lag1 and Lag2 equal 1.1 and 1.2, and a day when Lag1 and Lag2 equal 1.5 and -0.8
predict(glm.fit, newdata = data.frame(Lag1 = c(1.2, 1.5), Lag2 = c(1.1, -0.8)), type = "response")

##LINEAR DISCRIMINANT ANALYSIS

#fitting an LDA model using the lda() function and Smarket data
#the lda() function is identical to that of lm() and glm() except for the absence of the family option
library(MASS)
lda.fit <- lda(Direction ~ Lag1 + Lag2, data = train)

#Prior probabilities of groups - percent of training observations corresponding to variables (49.2% of observations correspond to days during which the market went down)
#Group means - average of each predictor within each class (there is a tendency for the previous 2 days' returns to be negative on days when the market increases)
#Coefficients of linear discrimnants - linear combination of variables that are used to form the LDA decision rule, these are the multipliers of the elements of X=x
lda.fit

#LDA prediction
#class - LDA's predictions about the movement of the market
#posterior - posterior probability that corresponding observation belongs to the kth class
#x - linear discriminants
lda.pred <- predict(lda.fit, Smarket.2005)
names(lda.pred)
lda.class <- lda.pred$class
table(lda.class, Smarket.2005$Direction)
mean(lda.class == Smarket.2005$Direction)

#applying 50% threshold to posterior probabilities and recreating the predictions contained in lda.pred$class
sum(lda.pred$posterior[,1] >= .5)
sum(lda.pred$posterior[,1] < .5)
lda.pred$posterior[1:20,1]
lda.class[1:20]

#changing posterior probability to at least 90%
sum(lda.pred$posterior[,1] > .9) #no days in 2005 meet this threshold

##QUADRATIC DISCRIMINANT ANALYSIS

#fitting a QDA model to the Smarket data using the qda() function
qda.fit <- qda(Direction ~ Lag1 + Lag2, data = train)
qda.fit #does not contain coefficients of the linear discriminants because QDA classifier involves a quadratic function fo predictors

#predicting using QDA
qda.class <- predict(qda.fit, Smarket.2005)$class
table(qda.class, Smarket.2005$Direction)
mean(qda.class == Smarket.2005$Direction) #QDA predictions are accurate almost 60% of the time

##K-NEAREST NEIGHBORS

#inputs for the knn() function (part of the class library):
#1. A Matrix containing the predictors associated with the training data
#2. A matris containing the predictors associated with the data for which we wish to make predictions
#3. A vector containing the class labels for the training observations
#4. A value for K, the number of nearest neighbors to be used by the classifier

#creating the matrices
library(class)
train.X <- cbind(train$Lag1, train$Lag2)
test.X <- cbind(Smarket.2005$Lag1, Smarket.2005$Lag2)
train.Direction <- train$Direction

#predicting with knn()
#seed set for reproducibility
set.seed(1)
knn.pred <- knn(train.X, test.X, train.Direction, k = 1)
table(knn.pred, Smarket.2005$Direction)
(83 + 43) / 252

#prediction using K = 3
knn.pred <- knn(train.X, test.X, train.Direction, k = 3)
table(knn.pred, Smarket.2005$Direction)
mean(knn.pred == Smarket.2005$Direction)

#applying the KNN approach to the Caravan dataset
?Caravan
dim(Caravan)
summary(Caravan$Purchase)
348 / 5822

#standardizing variables so that all are given a mean of zero and standard deviation of one - now they are on the same scale, and KNN won't favor larger scale variables
standardized.X <- scale(Caravan[,-86]) #excluding column 86 since it is the qualitative Purchase variable
var(Caravan[,1])
var(Caravan[,2])
var(standardized.X[,1])
var(standardized.X[,2])

#splitting into test and train sets
test <- 1:1000
train.X <- standardized.X[-test,]
test.X <- standardized.X[test,]
train.Y <- Caravan$Purchase[-test]
test.Y <- Caravan$Purchase[test]
set.seed(1)
knn.pred <- knn(train.X, test.X, train.Y, k = 1)
mean(test.Y != knn.pred) #the KNN error rate
mean(test.Y != "No") #the error rate always predicting "No" - KNN overall rate not of interest

#predicting among the customers that are already predicted to buy insturance
table(knn.pred, test.Y)
9 / (68 + 9)

#predictions using K = 3 and K = 5
knn.pred <- knn(train.X, test.X, train.Y, k = 3)
table(knn.pred, test.Y)
5 / 26
knn.pred <- knn(train.X, test.X, train.Y, k = 5)
table(knn.pred, test.Y)
4/ 15

#using a logistic regression model
#starting with a cut-off of 0.5 - no correct Yes predictions
#decreasing cut-off to 0.25 - performs much better
glm.fit <- glm(Purchase ~ ., data = Caravan[-test,], family = binomial)
glm.probs <- predict(glm.fit, Caravan[test,], type = "response")
glm.pred <- rep("No", 1000)
glm.pred[glm.probs > .5] <- "Yes"
table(glm.pred, test.Y)
glm.pred <- rep("No", 1000)
glm.pred[glm.probs > .25] <- "Yes"
table(glm.pred, test.Y)
11 / (22 + 11)
