#############################################################################
# The University of Hong Kong
# ECON3284 - Introduction to Causal Inference and Statistical Learning
# Problem Set 1 - Question 2_3
# Objectives: to perform introductory linear-regression commands on textbook
# Leung Yat Long Mars (3035374046)
#############################################################################

setwd("C:\\Users\\marzl\\Documents\\#HKU\\ECON3284 Introduction to Causal Inference and Statistic Learning\\Problem Sets\\PS1")

##################
# 3.6.1 Libraries
##################

library(MASS) # MASS comes with R
library(ISLR) # needs to be additionally downloaded; type install.packages("ISLR") into the console

#################################
# 3.6.2 Simple Linear Regression
#################################

lm.fit = lm(medv~lstat,     # lm stands for linear model; outputs a list
            data = Boston)  # to specify the datatable; unnecessary if attached

attach(Boston)
lm.fit = lm(medv~lstat) # Equivalent to above

# Presenting regression results
print(lm.fit)   # Show coefficients 
summary(lm.fit) # Show regression table

# Attributes from lm()
names(lm.fit)   # to see what kind of attributes are stored within
coef(lm.fit)    # simply show coefficients of lm.fit; equivalent to lm.fit$coefficients
confint(lm.fit) # confidence interval

# Prediction given values of explanatory variable
predict(lm.fit,
        data.frame(lstat = c(5,10,15)), # Given some explicit values of the explanatory variables; if omitted, predict() uses the original observations
        interval = "confidence")        # Give confidence interval; if omitted, predict() only gives single-value predictions
  # Confidence interval takes uncertainty of predicted value into account

predict(lm.fit,
        data.frame(lstat = c(5,10,15)), 
        interval = "prediction")        # Give prediction interval
  # Prediction interval takes uncertainty of individual deviation from predicted value into account; and hence wider than C.I.

# Visualization
plot(lstat, medv, # Simple scatter plot, nothing new
     pch = "+")   # pch stands for plot character, aka point shape
abline(lm.fit,    # abline() add straight lines to a current plot; P.S. abline(c, m) plots y = mx + c
       lwd = 3,   # lwd stands for line width
       col = "red")

par(mfrow = c(2,2)) # split-screen for graphs
plot(lm.fit)        # directly pass lm into plot() produces 4 graphs, which would normally require use to click enter 3 times to show all consecutively:
                    # 1. residuals vs fitted; 2. normal Q-Q
                    # 3. scale-location;      4. residuals vs leverage

par(mfrow = c(1,1))

plot(predict(lm.fit), residuals(lm.fit))  # residuals() returns what you think it does
plot(predict(lm.fit), rstudent(lm.fit))   # rstudents() returns studentized residuals = residual / E(sd(residual))
plot(hatvalues(lm.fit))                   # hatvalues() returns leverage statistics = d(predicted y_i)/dy_i; positively related to to how "far way" an observation is

###################################
# 3.6.3 Mulitple Linear Regression
###################################

lm.fit = lm(medv~lstat+age)
summary(lm.fit)

lm.fit = lm(medv~., data = Boston)  # regress medv on ALL other variables
summary(lm.fit)

library(car)  # car stands for Companion to Applied Regression
vif(lm.fit)   # from the car library; compute variance inflation factors = 1/(1-R_j^2)

lm.fit1 = lm(medv~.-age, data = Boston)  # On ALL variabled but age
summary(lm.fit1)

lm.fit1 = update(lm.fit, ~.-age)  # Using update() to drop a variable

##########################
# 3.6.4 Interaction Terms
##########################

summary(lm(medv~lstat*age, data = Boston))  # a*b inlcudes a, b, and a times b; use : for multiplication

#####################################################
# 3.6.5 Non-linear Transformations of the Predictors
#####################################################

# Second Order Polynomial Fit
lm.fit2 = lm(medv~lstat + I(lstat ^2)) # use I() to wrap around the exponential expression
summary(lm.fit2)

lm.fit = lm(medv~lstat) # SLR 
anova(lm.fit, lm.fit2)  # H_0: the two models perform equally well vs H_1: the full model is superior

par(mfrow=c(2,2))
plot(lm.fit2)

# Multi-order Polynomial Fit
lm.fit5 = lm(medv~poly(lstat ,5)) # An order of 5
summary(lm.fit5)

###############################
# 3.6.6 Qualitative Predictors
###############################

library(ISLR)
fix(Carseats)   # in the ISLR library
names(Carseats) # Sheveloc is a qualitative predictor

lm.fit = lm(Sales~. + Income:Advertising + Price:Age, data = Carseats)
summary(lm.fit) # Note that R automatically generates 2 dummy variables for ShelveLoc

contrasts(Carseats$ShelveLoc) # Show how the dummies are constructed

##########################
# 3.6.7 Writing Functions
##########################

# A function defined to load both ISLR and MASS:
LoadLibraries = function(){
  library(ISLR)
  library(MASS)
  print("The Libraries have been loaded.")
}

LoadLibraries()
