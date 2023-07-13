#Samuel Wolfe
#Homework Chapter 6 in R
#July 8th, 2023
#installed packages
#ggpubr
library(dplyr) #4
library(data.table) #1
library(curl) #2
library(fastDummies) #3
library(glmnet) #6
library(leaps) #5

#Question 6.3
#Preparing the data
#import data
dfAirBase <- fread("https://raw.githubusercontent.com/wolfesamk/MSBA-206/main/dmba/Airfares.csv")
#verifying dataframe shape and columns
names(dfAirBase)
dim(dfAirBase)
#creating dummy variables
dfAirAllCat <- dfAirBase[,-1:-4]
#adding dummies
dfAirDummy <- dummy_cols(dfAirAllCat, remove_first_dummy = TRUE, remove_selected_columns = TRUE)
#moving FARE to end
dfAirDummy <- dfAirDummy %>% select(-FARE,FARE)
#creating X and Y
airX <-model.matrix(FARE ~., data = dfAirDummy)[,-1]
airY <-dfAirDummy$FARE
#air.colnames(y) <- c("FARE")
#dfAir3 <-cbind(airY, airX)

#splitting the data
set.seed(1)
airTrain.i <- sample(1:nrow(airX),nrow(airX)*0.6)
airTest.i <- (-airTrain.i)
airTest.i.y <- airY[airTest.i]

#6.3 Best Subset Selection
air.i.best <-regsubsets(FARE ~., data = dfAirDummy[airTrain.i, ], nvmax = 13)
air.i.best.test.mat <-model.matrix(FARE ~ ., data=dfAirDummy[airTest.i, ])
air.i.val.errors <- rep(NA, 13)
for (i in 1:13) {
  coefi <- coef(air.i.best, id = i)
  pred <- air.i.best.test.mat[, names(coefi)] %*% coefi
  air.i.val.errors[i] <- mean((dfAirDummy$FARE[airTest.i]-pred)^2)
}
summary(air.i.best)
air.i.best.row <- which.min(air.i.val.errors)
air.i.best.row
air.i.val.errors[air.i.best.row]
coef(air.i.best, air.i.best.row)
alldummynames <- names(dfAirDummy[,-1:-2])
air.i.best.pred <- alldummynames[-12]
#these are the best predictors.
air.i.best.pred

#6.3 Forward Stepwise
air.ii.step.best.fwd <-regsubsets(FARE ~., data = dfAirDummy[airTrain.i, ], nvmax = 14, method = "forward")
air.ii.step.test.mat <-model.matrix(FARE ~ ., data=dfAirDummy[airTest.i, ])
air.ii.val.errors <- rep(NA, 13)
for (i in 1:13) {
  coefi <- coef(air.ii.step.best.fwd, id = i)
  pred <- air.ii.step.test.mat[, names(coefi)] %*% coefi
  air.ii.val.errors[i] <- mean((dfAirDummy$FARE[airTest.i]-pred)^2)
}
summary(air.ii.step.best.fwd)
air.ii.best.row <- which.min(air.ii.val.errors)
air.ii.best.row
air.ii.val.errors[air.ii.best.row]
coef(air.ii.step.best.fwd, air.ii.best.row)
alldummynames <- names(dfAirDummy[,-1:-2])
alldummynames <- alldummynames[-2]
air.ii.best.pred <- alldummynames[-11]
#these are the best predictors.
air.ii.best.pred

#6.3 Backward Stepwise
air.iii.step.best.bwd <-regsubsets(FARE ~., data = dfAirDummy[airTrain.i, ], nvmax = 14, method = "backward")
air.iii.step.test.mat <-model.matrix(FARE ~ ., data=dfAirDummy[airTest.i, ])
air.iii.val.errors <- rep(NA, 13)
for (i in 1:13) {
  coefi <- coef(air.iii.step.best.bwd, id = i)
  pred <- air.iii.step.test.mat[, names(coefi)] %*% coefi
  air.iii.val.errors[i] <- mean((dfAirDummy$FARE[airTest.i]-pred)^2)
}
summary(air.iii.step.best.bwd)
air.iii.best.row <- which.min(air.iii.val.errors)
air.iii.best.row
air.iii.val.errors[air.iii.best.row]
coef(air.iii.step.best.bwd, air.iii.best.row)
alldummynames <- names(dfAirDummy[,-1:-2])
alldummynames <- alldummynames[-2:-3]
air.iii.best.pred <- alldummynames[-8:-10]
#these are the best predictors.
air.iii.best.pred

#6.3 Ridge
#setting up ridge
grid <- 10^seq(10, -2, length = 100)
ridge.mod <- glmnet(airX, airY, alpha = 0, lambda = grid)
#finding best lambda
cv.out <- cv.glmnet(airX[airTrain.i, ], airY[airTrain.i], alpha = 0)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam
#running with best lambda
ridge.pred <- predict(ridge.mod, s = bestlam,
                      newx = airX[airTest.i, ])
mean((ridge.pred - airTest.i.y)^2)
out <- glmnet(airX, airY, alpha = 0)
predict(out, type = "coefficients", s = bestlam)[1:14, ]

#6.3 Lasso
lasso.mod <- glmnet(airX[airTrain.i, ], airY[airTrain.i], alpha = 1,lambda = grid)
plot(lasso.mod)
cv.out <- cv.glmnet(airX[airTrain.i, ], airY[airTrain.i], alpha = 1)
plot(cv.out)
bestlam <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod, s = bestlam,
                      newx = airX[airTest.i, ])
mean((lasso.pred - airTest.i.y)^2)
out <- glmnet(airX, airY, alpha = 1, lambda = grid)
lasso.coef <- predict(out, type = "coefficients",
                      s = bestlam)[1:14, ]
lasso.coef[lasso.coef != 0]

#6.3 True Stepwise
air.vi.step.best.bwd <-regsubsets(FARE ~., data = dfAirDummy[airTrain.i, ], nvmax = 14, method = "seqrep")
air.vi.step.test.mat <-model.matrix(FARE ~ ., data=dfAirDummy[airTest.i, ])
air.vi.val.errors <- rep(NA, 13)
for (i in 1:13) {
  coefi <- coef(air.vi.step.best.bwd, id = i)
  pred <- air.vi.step.test.mat[, names(coefi)] %*% coefi
  air.vi.val.errors[i] <- mean((dfAirDummy$FARE[airTest.i]-pred)^2)
}
summary(air.vi.step.best.bwd)
air.vi.best.row <- which.min(air.vi.val.errors)
air.vi.best.row
air.vi.val.errors[air.iii.best.row]
coef(air.vi.step.best.bwd, air.iii.best.row)
alldummynames <- names(dfAirDummy[,-1:-2])
alldummynames <- alldummynames[-2]
alldummynames <- alldummynames[-3:-4]
alldummynames <- alldummynames[-4]
air.vi.best.pred <- alldummynames[-8]
#these are the best predictors.
air.vi.best.pred
