#######################################################################
# The University of Hong Kong
# ECON3284 - Introduction to Causal Inference and Statistical Learning
# Problem Set 1 - Question 2_1
# Objectives: to perform introductory commands on textbook
# Leung Yat Long Mars (3035374046)
#######################################################################

setwd("C:\\Users\\marzl\\Documents\\#HKU\\ECON3284 Introduction to Causal Inference and Statistic Learning\\Problem Sets\\PS1")

#######################
# 2.3.1 Basic Commands
#######################

# Assigning a vector to a variable by <-
x <- c(1,3,2,5) # c stands dor concatenate
x
# Equal sign does the same thing
x = c(1,6,2)
x

# Lengths of vectors
y = c(1,4,3)
length(x)
length(y)

# One-to-one summation of vectors
x + y

# Objects and removal of objects
ls()        # To return a list of all objects
rm(x,y)     # To remove the named objects; to remove ALL objects, use rm(list=ls())
ls()        # No objects exist => return character(0) 

# Help with a function
?matrix

# Matrix origination (by-col by default, i.e. from left to right)
x = matrix(data=c(1,2,3,4), nrow=2, ncol=2)
x = matrix(data=c(1,2,3,4), 2, 2) # Omitting the parameter names does the same thing

# By-row matrix origination, i.e. from up to down
matrix(c(1,2,3,4), 2, 2, byrow=TRUE)

# Square roots and exponentials
sqrt(x) # To return the square root of each eleent
x^2     # To return the square of each element

# Generating a vector of random normal variables
x = rnorm(50) # By default mean = 0 and sd = 1, i.e. standard normal

# Correlation coefficient between vectors
y = x + rnorm(50, mean = 50, sd = .1)
cor(x, y)

# Setting the randomization seed
set.seed(1303)        # Affecting the whole script; R will generate a new one automatically if left unset
set.seed(Sys.time())  # Using system time to set a new seed, effectively unsetting the seed

# Descriptive statistics of a vector
y = rnorm(100)
mean(y)       # Mean
var(y)        # Variance
sqrt(var(y))  # Standard deviation = sqrt(var)
sd(y)         # Standard deviation

#################
# 2.3.2 Graphics
#################

# Plotting
x = rnorm(100)
y = rnorm(100)
plot(x, y)      # Scatter plot by default; first x then y

# Labelling
plot(x, y, 
     xlab = "this is the x-axis",
     ylab = "this is the y-axis",
     main = "Plot of X vs Y")

# Exporting graphs (pdf() for saving as .pdf, jpeg() for .jpeg)
pdf("Figure.pdf")         # Start the graphic device driver
plot(x, y, col = "green") # Set the colour to green, just because
dev.off()                 # Shut down the device

# Creating a sequence of numbers
x = seq(1, 10)
x = 1:10                      # Do the same thing
x = seq(-pi, pi, length = 50) # Specify the first and last element, then equally space out the elements given the total number

# Contour plots (3D objects projected vertically onto a 2D plane, like a map) 
y = x
f = outer(x, y,                         # Outer product (of any specified function) of two vectors (1D x 1D -> 2D)
          function(x,y) cos(y)/(1+x^2)) # The specified function; anonymous function is used here (similar to Python's lamda function)
contour(x, y, f)
contour(x, y, f,
        nlevels = 45,                   # Specify the total number of contour lines
        add = T)                        # Add to a current plot, instead of creating a new plot
fa = (f-t(f))/2                         # t() transposes a matrix
contour(x, y, fa, nlevels = 15)

# Heatmaps
image(x, y, fa)

# Perspective plots
persp(x, y, fa)             # by default theta and phi equal 0
persp(x , y, fa,
      theta = 30)           # theta rotates the x-y plane
persp(x , y, fa,
      theta = 30, phi = 20) # phi rotates the z axis
persp(x , y, fa,
      theta = 30, phi = 40)
persp(x , y, fa,
      theta = 30, phi = 70)

######################
# 2.3.3 Indexing Data
######################

# Data indexing
A = matrix(1:16, 4, 4)
A
A[2,3] # Row 2, column 3

# Matrix slicing
A[c(1,3), c(2,4)] # Only rows 1 & 3, and columns 2 & 4 
A[1:3, 2:4]       # Rows 1 to 3, and columns 2 to 4
A[1:2, ]          # Select ALL columns
A[, 1:2]          # Select ALL rows
A[1,]             # Any single row or column outputs as a vector
A[-c(1,3),]       # Negative sign indicates the rows/columns to be excluded

# Dimension of an object
dim(A)            # Output numbers of rows and columns

#####################
# 2.3.4 Loading Data
#####################

# write.table() to export data

# Reading data
Auto = read.table("Auto.data")  # From working directory (Click "Session" tab to change that)
fix(Auto)                       # A pop-up window formatting the table into a spreadsheet

Auto = read.table("Auto.data",
                  header = T,       # Such that the variable names in the data will populate not the first row, but the header row
                  na.strings = "?") # If the reader sees a "?", treat that as NA (see row 33 as an example)

# Reading .csv
Auto = read.csv("Auto.csv", header = T, na.strings ="?")

# Omitting NA
dim(Auto) # Output numbers of rows and columns
Auto = na.omit(Auto)  # Remove rows with any NA value
dim(Auto)

# Variable names
names(Auto)

#####################################################
# 2.3.5 Additional Graphical and Numerical Summaries
#####################################################

# Simply plot(cylinders, mpg) does not work as R does not know to look in Auto

plot(Auto$cylinders, Auto$mpg)

# Attaching variable names from a datatable
attach(Auto)  # Now all the variable names in Auto are also in the R search path
plot(cylinders, mpg)

# Categorical variables and boxplots
cylinders = as.factor(cylinders)        # Converting quantitative variables into qualitative (or categorical variables)
plot(cylinders, mpg,                    # Now that x variable is qualitative, plot() produces boxplots by default
     col = "red",
     varwidth = T,                      # Make widths of boxes proportional to sample sizes
     horizontal = T,                    # Horizontal boxplot 
     xlab = "cylinders", ylab = "MPG") 

# Histograms
hist(mpg,
     col = 2,     # 2 corresponds to red
     breaks = 5)  # Numbers of bars

# Scatterplot Matrices
pairs(Auto)
pairs(~ mpg + displacement + horsepower + weight + acceleration , # Only a subset of the variables 
       Auto)

# Identifying a point on the plot
plot(horsepower, mpg)
identify(horsepower, mpg, name) # You can click on the graph to select a point, escape, then the specified variable (name in this case) will be printed near the point. The row index will be printed to the console

# Summary
summary(Auto)
summary(Auto$mpg)

#Saving command history
savehistory() # Use loadhistory() next time

# q() to quit