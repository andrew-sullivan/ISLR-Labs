##SIMPLE LINEAR REGRESSION

#loading packages
library(MASS)
library(ISLR)

#using Boston housing data from MASS library
?Boston
names(Boston)

#fitting linear regression model with medv(median value of home) as response and lstat(lower status of population) as predictor
lm.fit <- lm(medv ~ lstat, data = Boston)
summary(lm.fit)
names(lm.fit)
coef(lm.fit)

#obtain confidence interval for the coefficient estimates
confint(lm.fit)

#predict confidence and prediction intervals for medv given values for lstat
predict(lm.fit, data.frame(lstat = c(5,10,15)), interval = "confidence")
predict(lm.fit, data.frame(lstat = c(5,10,15)), interval = "predict")

#scatterplot and regression plot of medv and lstat
plot(medv ~ lstat, data = Boston)
abline(lm.fit)
abline(lm.fit, lwd=3)
abline(lm.fit, lwd=3, col="red")
plot(medv ~ lstat, data = Boston, col="red")
plot(medv ~ lstat, data = Boston, pch=20)
plot(medv ~ lstat, data = Boston, pch = "+")
plot(1:20, 1:20, pch = 1:20)

#diagnostic plots
par(mfrow=c(2,2))
plot(lm.fit)

#restore graphic defaults
dev.off()

#residuals plotting
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))

#leverage statistics
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))

##MULTIPLE LINEAR REGRESSION

#fitting a multiple linear regression model using least squares
lm.fit <- lm(medv ~ lstat + age, data = Boston)
summary(lm.fit)

#multiple linear regression using all predictors
lm.fit <- lm(medv ~ ., data = Boston)
summary(lm.fit)

#accessing components of a summary object
?summary.lm
summary(lm.fit)$r.sq #R-squared
summary(lm.fit)$sigma #RSE

#computing variance inflation factors
library(car)
vif(lm.fit)

#regression using all of the variables but one
lm.fit1 <- lm(medv ~. - age, data = Boston)
summary(lm.fit1)
lm.fit1 <- update(lm.fit, ~. - age)

##INTERACTION TERMS

#lstat:age - interaction term
#lstat*age - interaction term plus individual terms
summary(lm(medv ~ lstat + age + lstat:age, data = Boston))
summary(lm(medv ~ lstat*age, data = Boston))

##NON-LINEAR TRANSFORMATIONS

#I(X^2) - X^2, must wrap with I()
lm.fit2 <- lm(medv ~ lstat + I(lstat^2), data = Boston)
summary(lm.fit2)

#use anova to quantify model vs model
#anova() function performs a hypothesis test comparing two models, with the null hypothesis that the two fit the data equally well, and the alternative that the full model is superior
#low p-value here indicates null hypothesis can be rejected, and model including polynomial is superior
lm.fit <- lm(medv ~ lstat, data = Boston)
anova(lm.fit, lm.fit2)
par(mfrow = c(2, 2))
plot(lm.fit2)

#regressing higher order polynomials
#use poly(predictor, order)
lm.fit5 <- lm(medv ~ poly(lstat, 5), data = Boston)
summary(lm.fit5)

#log transformation
summary(lm(medv ~ log(rm), data = Boston))

##QUALITATIVE PREDICTORS

#using Carseats data from ISLR library and creating model including qualitative predictors
?Carseats
names(Carseats)
lm.fit <- lm(Sales ~ . + Income:Advertising + Price:Age, data = Carseats)
summary(lm.fit)

#use contrasts() function to return the coding that R uses for the dummy variables
attach(Carseats)
contrasts(ShelveLoc)
?contrasts

##WRITING FUNCTIONS

#writing a simple function that reads in the ISLR and MASS libraries
LoadLibraries <- function(){
  library(ISLR)
  library(MASS)
  print("The libraries have been loaded.")
}
LoadLibraries
LoadLibraries()
