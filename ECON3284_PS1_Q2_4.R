#############################################################################
# The University of Hong Kong
# ECON3284 - Introduction to Causal Inference and Statistical Learning
# Problem Set 1 - Question 2_3
# Objectives: Exercises 8 & 9(a,b,c,d) in textbook chapter 3.7
# Leung Yat Long Mars (3035374046)
#############################################################################

#############
# Exercise 8
#############

# Regressing mpg on horsepower
attach(Auto)
lm.fit_a = lm(mpg ~ horsepower)
summary(lm.fit_a)

# Predicted mpg with horsepower = 98
predict(lm.fit_a, 
        data.frame(horsepower = 98),
        interval = "confidence")
predict(lm.fit_a, 
        data.frame(horsepower = 98),
        interval = "prediction")

# Plot
plot(horsepower, mpg)
abline(lm.fit_a)

# Diagnostic plots 
par(mfrow = c(2,2))
plot(lm.fit_a)  # heteroskedasticity

#############
# Exercise 9
#############

# Scatterplot Matrix
pairs(Auto)

# Correlation Matrix
Auto_quant = Auto[,-which(colnames(Auto)=="name")]  # get rid of the name column
cor(Auto_quant)

# MLR
lm.fit_b = lm(mpg~., data = Auto_quant)
summary(lm.fit_b)

# Diagnostic plots 
par(mfrow = c(2,2))
plot(lm.fit_a)  # heteroskedasticity
