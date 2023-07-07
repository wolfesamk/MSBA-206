#Samuel Wolfe
#Homework Chapter 6 in R
#July 7th, 2023
#installed packages
#ggpubr
library(linprog)
library(lpSolve)
library(data.table)
library(leaps)
library(ISLR2)
library(fastDummies)
library(recipes)
library(lessR)

#Question 6.3
#Preparing the data
#import data
dfAirBase <- fread("Airfares.csv")
#verifying dataframe shape and columns
names(dfAirBase)
dim(dfAirBase)
#seperating FARE for readds
fares <- dfAirBase$FARE
#setting target variable
targetVar <- 'FARE'

#6.3.a.
#creating dataframe without categorical variables
dfAirInt <- dfAirBase%>% select(where(is.numeric))
airIntCol <- names(dfAirInt) #using later for 6.3.b.
cor_mtx <- cor(dfAirInt)
round(cor_mtx[, 10], 3)
#here we see that Distance has the greatest correlation with Fare.
#followed by Coupon.

#6.3.b.
#removing first four columns
dfAirAllCat <- dfAirBase[,-1:-4]
names(dfAirAllCat)
dim(dfAirAllCat)
#removing non cat columns
dfAirCat <- select(dfAirAllCat, -airIntCol)
airCatCol <- c(names(dfAirCat))
#readding FARE
dfAirCat$FARE <-fares
#producing pivot tables using lessR
meanVACATION <- pivot(data=dfAirCat, compute=mean, variable=FARE, by=c(VACATION))
meanSW <- pivot(data=dfAirCat, compute=mean, variable=FARE, by=c(SW))
meanGATE <- pivot(data=dfAirCat, compute=mean, variable=FARE, by=c(GATE))
meanSLOT <- pivot(data=dfAirCat, compute=mean, variable=FARE, by=c(SLOT))
#printing tables
meanVACATION
meanSW
meanGATE
meanSLOT
#The best categorical variable for predicting FARE seems to be SW. Followed by Vacation.

#6.3.c.i
#creating dummy variables
dfAirDummy <- dummy_cols(dfAirAllCat, remove_first_dummy = TRUE, remove_selected_columns = TRUE)
#splitting the data
#6.3.c.ii
air.step.full.fwd <-regsubsets(FARE ~., data = dfAirDummy, nvmax = 14, method = "forward")
summary(air.step.full.fwd)
air.step.full.fwd