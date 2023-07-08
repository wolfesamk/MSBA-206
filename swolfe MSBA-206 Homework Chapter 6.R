#Samuel Wolfe
#Homework Chapter 6 in R
#July 7th, 2023
#installed packages
#ggpubr
library(dplyr) #3
library(data.table) #1
library(curl) #2
library(fastDummies) #5
#library(ISLR2)
library(leaps) #6
library(lessR) #4
#library(linprog)
#library(lpSolve)
options(rstudio.help.showDataPreview = FALSE)
getOption("rstudio.help.showDataPreview")

#Question 6.3
#Preparing the data
#import data
dfAirBase <- fread("https://raw.githubusercontent.com/wolfesamk/MSBA-206/main/dmba/Airfares.csv")
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
set.seed(1)
airTrain.i <- sample(c(TRUE, FALSE),
                   nrow(dfAirDummy),
                   replace = TRUE)
airTest.i <-(!airTrain.i)
#6.3.c.ii
air.ii.step.best.fwd <-regsubsets(FARE ~., data = dfAirDummy[airTrain.i, ], nvmax = 14, method = "forward")
air.ii.step.test.mat <-model.matrix(FARE ~ ., data=dfAirDummy[airTest.i, ])
air.ii.val.errors <- rep(NA, 13)
for (i in 1:13) {
  coefi <- coef(air.ii.step.best.fwd, id = i)
  pred <- air.ii.step.test.mat[, names(coefi)] %*% coefi
  air.ii.val.errors[i] <- mean((dfAirDummy$FARE[airTest.i]-pred)^2)
}
air.ii.val.errors
summary(air.step.full.fwd)
air.step.full.fwd
