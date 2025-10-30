#Lab 4 by Arav Kansal 

###1) Setting working directory ----

getwd() #where is your working directory?
setwd("C:\Users\aravk\OneDrive\Documents\PS394\R projects\R projects") #update it (if needed) to be where you saved the class data file

###2) Loading the data ----

data <- read.csv("PS394 Class Dataset F2025.csv") #load the data in r

View(data) #open the data window - see anything weird?
summary(data) #get a summary of the data - notice how everything is a character variable? This is because of those annoying first two rows carried in from Qualtrics

data <- data[-c(1, 2), ] #lets drop the top to rows of our data

View(data) #everything looks okay now!
summary(data) #but it's not - still reading as character variables because R decides that when you download the data

#so let's quickly jump out of R, open the data file in Excel, delete the teo extra rows (row 2 and 3 in excel - keep the variable names). Save the data with your intials at the end (i.e., "_XX")
data <- read.csv("PS394 Class Dataset F2025_AK.csv") #let's load it back in

View(data) #take a look and all looks good
summary(data) #and now the variables aren't all reading as character!!
data <- data[-c(1,2),] #removing the first two rows of the class data 

###3) Inspecting the data ----

data$ID <- seq_len(nrow(data)) #we are quickly going to compute an ID number for each row - 1 through 130
data$ID # if you run the variable date, you can see everyone now has a unique ID. you can also open the data and scroll right to the final column to see it in the dataset

View(data) #viewing ID variable

#To clean the data, we need to inspect it... Here is an extensive suite of functions that will allow you to look at any variable. In our example, we are looking at the GPA variable. 

hist(data$GPA) #histogram of GPA
mean(data$GPA, na.rm=TRUE) #mean of variable in dataset
median(data$GPA, na.rm=TRUE) #median of variable in dataset
range(data$GPA, na.rm=TRUE) #range of variable in dataset
var(data$GPA, na.rm=TRUE) #variance of variable in dataset
sd(data$GPA, na.rm=TRUE) #standard deviation of variable in dataset

#You can also get a birdseye view of the data using the describe() function
#This code uses psych, so that needs to be loaded before it will run

library(psych)
describe(data)

###4) Changing the data ----

# If you were to find any specific values you wanted to recode, you could use this code

data$GPA[data$ID == "70"] <-  8.9 #recode the GPA of person with ID 70 to 8.90
data$GPA[data$ID == "73"] <-  8.9 #recode the GPA of person with ID 85 to 8.90


# Practice - repeated steps for the variable MathGrade

# Data inspection for any inconsistencies

hist(data$MathGrade) #histogram of GPA
mean(data$MathGrade, na.rm=TRUE) #mean of variable in dataset
median(data$MathGrade, na.rm=TRUE) #median of variable in dataset
range(data$MathGrade, na.rm=TRUE) #range of variable in dataset
var(data$MathGrade, na.rm=TRUE) #variance of variable in dataset
sd(data$MathGrade, na.rm=TRUE) #standard deviation of variable in dataset

# Cleaning inconsistencies within MathGrade 

data$MathGrade[data$ID == "69"] <- 60 # Was written as 60& previously 
data$MathGrade[data$ID == "4"] <- 94 
data$MathGrade[data$ID == "12"] <- 75
data$MathGrade[data$ID == "26"] <- 98
data$MathGrade[data$ID == "31"] <- 85
data$MathGrade[data$ID == "37"] <- 75
data$MathGrade[data$ID == "46"] <- 92
data$MathGrade[data$ID == "60"] <- 75
data$MathGrade[data$ID == "65"] <- 90 
data$MathGrade[data$ID == "67"] <- 80
data$MathGrade[data$ID == "81"] <- 85
data$MathGrade[data$ID == "103"] <- 80
data$MathGrade[data$ID == "105"] <- 75
data$MathGrade[data$ID == "106"] <- 75
data$MathGrade[data$ID == "115"] <- 70
data$MathGrade[data$ID == "118"] <- 80
data$MathGrade[data$ID == "124"] <- 70
data$MathGrade[data$ID == "129"] <- 62

View(data)

# Getting final descriptives

library(psych)
describe(data)
