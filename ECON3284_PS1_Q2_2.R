#######################################################################
# The University of Hong Kong
# ECON3284 - Introduction to Causal Inference and Statistical Learning
# Problem Set 1 - Question 2_2
# Objectives: Exercises 8 - 10 in textbook chapter 2.4
# Leung Yat Long Mars (3035374046)
#######################################################################

setwd("C:\\Users\\marzl\\Documents\\#HKU\\ECON3284 Introduction to Causal Inference and Statistic Learning\\Problem Sets\\PS1") #set working directory

#############
# Exercise 8
#############

# Directly read from .csv
college = read.csv("College.csv") 
fix(college)

# Naming the rows by college name
rownames(college) = college[,1]
fix(college)

# Erase the orignial college name column
college = college[,-1]
fix(college)

# Give summary of the datatable
summary(college)

# Scatterplot matrix for the first 10 variables
pairs(college[,1:10])

# Side-by-side boxplots of Outstate vs Private
plot(college$Private, college$Outstate)

# Create a variable categorizing elite colleges
Elite = rep("No", nrow(college))      # rep() repeats "No" for nrow(college) times
Elite[college$Top10perc>50] = "Yes"   # if Top10perc
Elite = as.factor(Elite)              # make Elite cetagorical
college = data.frame(college, Elite)  # create a new dataframe from the old college and Elite
summary(college$Elite)                # summarize Elite
plot(college$Elite, college$Outstate) # side-by-side boxplots of Outstate vs Elite

# Create some histograms
par(mfrow = c(2,2)) # par(mfrow=c(m,n)) splits the output screen into sections for side-by-side graphs
hist(college$Enroll, breaks = 110, col = 1)
hist(college$Top10perc, breaks = 120, col = 2)
hist(college$S.F.Ratio, breaks = 130, col = 3)
hist(college$Expend, breaks = 140, col = 4)

#############
# Exercise 9
#############

Auto = read.csv("Auto.csv", header = T, na.strings ="?")  # read .csv
Auto = na.omit(Auto)                                      # omit rows with NA

fix(Auto) # cylinders, year, origin, and name are qualitative
qualitative_names = c("cylinders", "year", "origin", "name")
quantitative_names = setdiff(names(Auto), qualitative_names)

# A function reporting descriptive statistics
print_desc_stats = function(datatable) {
  for (column_name in names(datatable)) {                   # Loop through column names
    if (!(column_name %in% qualitative_names)){             # If the variable is not qualitative
      
      print(paste("Range for ", column_name, ":", sep=""),  # paste() joins strings together
            quote = F)                                      # Print the texts without quote
      print(range(datatable[,column_name]))                 # Print range for each column 
      
      print(paste("Mean for ", column_name, ":", sep=""),  
            quote = F)                                      
      print(mean(datatable[,column_name]))                  # Print range for each column 
      
      print(paste("Standard deviation for ", column_name, ":", sep=""),  
            quote = F)                                      
      print(sd(datatable[,column_name]))                    # Print range for each column 
    }
  }
}

print_desc_stats(Auto)  # report descriptive statistics

# Removing rows 10 to 85
Auto_shrinked = Auto[-10:-85,]
print_desc_stats(Auto_shrinked) # report descriptive statistics

# Scatterplot matrix
pairs(Auto)

# Looks like that all variables bar name are useful in predicting mpg from the scatterplots

##############
# Exercise 10
##############

library(MASS)
?Boston
fix(Boston)
dim(Boston)

# Scatterplot matrix
pairs(Boston)

# Findinng suburbs with extremem values
# Define extreme values as values beyond the median +/- k sd bands
BAND_WIDTH = 2  # k parameter

for (col_name in c("crim", "tax", "ptratio")){      # Looping through columns
  
  #message(col_name)
  col_median = median(Boston[,col_name])
  col_sd = sd(Boston[,col_name])
  upper_band = col_median + BAND_WIDTH * col_sd
  lower_band = col_median - BAND_WIDTH * col_sd
  
  for (row_index in 1:nrow(Boston)){                # Looping through rows
    
    if (Boston[row_index, col_name] > upper_band){
      message("Row ", row_index, ": ", "high ", col_name, " at ", Boston[row_index, col_name])
    }
    if (Boston[row_index, col_name] < lower_band){
      message("Row ", row_index, ": ", "low ", col_name, " at ", Boston[row_index, col_name])
    }
  }
}

# Number of suburbs set bound the Charles River
message("Number of suburbs set bound the Charles River = ", sum(Boston$chas))

# Median pupil-teacher ratio
message("Median pupil-teacher ratio = ", median(Boston$ptratio))

# Suburb with lowest median value of owner-occupied homes
message("Row index with lowest median value of owner-occupied homes = ", which.min(Boston$medv), ":")
message(Boston[which.min(Boston$medv),])

# Suburbs averaging more than 7 or 8 rooms per dwelling
for (room_number in c(7, 8)){
  message("Row numbers of suburbs averaging more than ", room_number, " rooms per dwelling:")
  print(which(Boston$rm > room_number))
}