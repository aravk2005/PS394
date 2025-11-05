#Lab 7 by Arav Kansal

#use your code from previous lab to set your working directory and load the lab data

getwd()
setwd("C:\Users\aravk\OneDrive\Documents\PS394\R projects\R projects")

#read the data
data <- read.csv("PS394 Class Dataset F2025_AK.csv") #load the data in r
View(data)

library(psych)

###1) Compute a variable ----

##The self-compassion scale includes items SC_1 through SC_12. Items are rated 1 (almost never) through 5 (almost always). Items 1, 2, 3, 6, 8, 11 are reverse coded. The items and labels are below for reference. 
#SC_1: I’m disapproving and judgmental about my own flaws and inadequacies. (R)
#SC_2: When I’m feeling down I tend to obsess and fixate on everything that’s wrong. (R)
#SC_3: When I fail at something important to me I become consumed by feelings of inadequacy.(R)
#SC_4: When something upsets me I try to keep my emotions in balance.
#SC_5: When I feel inadequate in some way, I try to remind myself that feelings of inadequacy are shared by most people.
#SC_6: I’m intolerant and impatient towards those aspects of my personality I don't like. (R)
#SC_7: When I’m going through a very hard time, I give myself the caring and tenderness I need.
#SC_8: When I’m feeling down, I tend to feel like most other people are probably happier than I am. (R)
#SC_9: When something painful happens I try to take a balanced view of the situation.
#SC_10: I try to see my failings as part of the human condition.
#SC_11: When I fail at something that's important to me, I tend to feel alone in my failure. (R)
#SC_12: I try to be understanding and patient towards those aspects of my personality I don't like

##Step 1: Reverse code reversed items

data$SC_1r<- (6-data$SC_1) #create a new variable(column) ‘SC_1r’ with the reverse coded values of column ‘SC_1’. Not be subtract the items from 6 here - but this may differ depending on the scale. This value should be the highest rating on the scale plus 1 (i.e., 5 + 1 = 6)
data$SC_2r<- (6-data$SC_2) #create a new variable(column) ‘SC_2r’
data$SC_3r<- (6-data$SC_3) #create a new variable(column) ‘SC_3r’
data$SC_6r<- (6-data$SC_6) #create a new variable(column) ‘SC_6r’
data$SC_8r<- (6-data$SC_8) #create a new variable(column) ‘SC_8r’
data$SC_11r<- (6-data$SC_11) #create a new variable(column) ‘SC_11r’

##Step 2: Compute the mean
data$SC_mean<- rowMeans(cbind(data$SC_1r, data$SC_2r, data$SC_3r, data$SC_4, data$SC_5, data$SC_6r, data$SC_8r, data$SC_9, data$SC_10, data$SC_11r, data$SC_12)) #create a new variable (columm) SC_mean that is equal to the average of the scale items - be sure to use the reverse coded items for reverse items!!!

##Step 3: Check the reliability
items_SC <- cbind(data$SC_1r, data$SC_2r, data$SC_3r, data$SC_4, data$SC_5, data$SC_6r, data$SC_8r, data$SC_9, data$SC_10, data$SC_11r, data$SC_12) #create a data subset containing only the items used to compute your scale
alpha(items_SC) #get the alpha for the scale items


###2) Inspecting the variable ----

describe(data$SC_mean)

###3) Run a simple linear regression ----

#center your variables (if you want/need to) - 2 options
data$SC_meanc <- (data$SC_mean - mean(data$SC_mean, na.rm=TRUE)) #mean center your variable
data$zSC_mean <- ((data$SC_mean - mean(data$SC_mean, na.rm=TRUE))/sd(data$SC_mean, na.rm=TRUE)) #convert you variable to z scores

model1 <- lm(GPA ~ SC_mean, data = data)
summary(model1) #model with uncentered

model1c <- lm(GPA ~ SC_meanc, data = data)
summary(model1c) #model with mean centered

model1z <- lm(GPA ~ zSC_mean, data = data)
summary(model1z) #model with z scores


#Practice
data$SOC_2r<- (5-data$SOC_2) #create a new variable(column) ‘SOC_2'
data$SOC6_1r<- (5-data$SOC6_1) #create a new variable(column) ‘SOC6_1’
data$SOC11_1r<- (5-data$SOC11_1) #create a new variable(column) ‘SOC11_1’

data$SOC_mean <- rowMeans(cbind(data$SOC_1, data$SOC_2r, data$SOC_3r, data$SC_4, data$SC_5, data$SOC_6r, data$SOC_7, data$SOC10_1, data$SOC_11r))

items_SOC <- cbind(data$SOC_1, data$SOC_2r, data$SOC_3r, data$SC_4, data$SC_5, data$SOC_6r, data$SOC_7, data$SOC10_1, data$SOC_11r)

alpha(items_SOC)

describe(data$SOC_mean)

data$SOC_meanc <- (data$SOC_mean - mean(data$SC_mean, na.rm=TRUE)) #mean center your variable
data$zSOC_mean <- ((data$SOC_mean - mean(data$SC_mean, na.rm=TRUE))/sd(data$SC_mean, na.rm=TRUE)) 

model1 <- lm(GPA ~ SOC_mean, data = data)
summary(model1)

model1c <- lm(GPA ~ SOC_meanc, data = data)
summary(model1c) 

model1z <- lm(GPA ~ zSOC_mean, data = data)
summary(model1z) 