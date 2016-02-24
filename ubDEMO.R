# DEMO for package "unbalanced"

# Install the last version from Github
# library(devtools)
# devtools::install_github("dalpozz/unbalanced")


set.seed(1234)
library(unbalanced)
data(ubIonosphere)
n <- ncol(ubIonosphere)
output <- ubIonosphere[ ,n]
input <- ubIonosphere[ ,-n]

#apply oversampling
data <- ubBalance(X=input, Y=output, type="ubOver", k=0)
#oversampled dataset
overData <- data.frame(data$X, Class=data$Y)
#check the frequency of the target variable after oversampling
summary(overData$Class)
## 0   1 
## 225 225 


#apply undersampling
data <- ubBalance(X=input, Y=output, type="ubUnder", perc=50, method="percPos")
#undersampled dataset
underData <- data.frame(data$X, Class=data$Y)
#check the frequency of the target variable after oversampling
summary(underData$Class)
## 0   1
## 126 126


set.seed(1234)
#keep half for training and half for testing
N <- nrow(ubIonosphere)
N.tr <- floor(0.5*N)
id.tr <- sample(1:N, N.tr)
id.ts <- setdiff(1:N, id.tr)
X.tr <- input[id.tr, ]
Y.tr <- output[id.tr]
X.ts <- input[id.ts, ]
Y.ts <- output[id.ts]
unbalTrain <- data.frame(X.tr, Class=Y.tr)
summary(unbalTrain$Class)
## 0   1
## 111 64

library(randomForest)
#use the original unbalanced training set to build a model
model1 <- randomForest(Class ~ ., unbalTrain)
#predict on the testing set
preds <- predict(model1, X.ts, type="class")
confusionMatrix1 <- table(prediction=preds, actual=Y.ts)
print(confusionMatrix1)
##            actual
## prediction 0   1
##          0 111 5
##          1 3   57

#rebalance the training set before building a model
balanced <- ubBalance(X=X.tr, Y=Y.tr, type="ubSMOTE", percOver=200, percUnder=150)
balTrain <- data.frame(balanced$X, Class=balanced$Y)
summary(balTrain$Class)
## 0 1
## 192 192

#use the balanced training set
model2 <- randomForest(Class ~ ., balTrain)
#predict on the testing set
preds <- predict(model2, X.ts, type="class")
confusionMatrix2 <- table(prediction=preds, actual=Y.ts)
print(confusionMatrix2)
##            actual
## prediction 0   1
##          0 103 1
##          1 11  61

#we can now correctly classify more minority class instances



set.seed(1234)
#configuration of the sampling method used in the race
ubConf <- list(percOver=250, percUnder=150, k=3, perc=50, method="percPos", w=NULL)

# Race with 10 trees in the Random Forest to speed up results
results <- ubRacing(Class ~., ubIonosphere, "randomForest", positive=1, 
                    metric="auc", ubConf=ubConf, ntree=10)

# Race using 4 cores and 500 trees (default number of trees in randomForest)
results <- ubRacing(Class ~., ubIonosphere, "randomForest", positive=1, 
                    metric="auc", ubConf=ubConf, ncore=4)

# Let's try with a different algorithm (see mlr package for supported packages)
library(e1071)
results <- ubRacing(Class ~., ubIonosphere, "svm", positive=1, ubConf=ubConf)
library(rpart)
results <- ubRacing(Class ~., ubIonosphere, "rpart", positive=1, ubConf=ubConf)




# Let's use a larger (and more unbalanced) dataset from:
# Dal Pozzolo, Caelen, Johnson, and Bontempi.
# "Calibrating probability with undersampling for unbalanced classification." 
# 2015 IEEE Symposium on Computational Intelligence and Data Mining. 

set.seed(1234)
# load the dataset
load(url("http://www.ulb.ac.be/di/map/adalpozz/data/creditcard.Rdata"))
#configuration of the sampling method used in the race
ubConf <- list(percOver=200, percUnder=200,
               k=2, perc=50, method="percPos", w=NULL)
# Race with 10 trees in the Random Forest to speed up results
results <- ubRacing(Class ~., creditcard, "randomForest", positive=1,
                    metric="auc", ubConf=ubConf, ntree=10, ncore=4)
##
## Racing for unbalanced methods selection in 10 fold CV
## Number of candidates...........................................9
## Max number of folds in the CV.................................10
## Max number of experiments....................................100
## Statistical test...................................Friedman test
##
## Markers:
## x No test is performed.
## - The test is performed and
## some candidates are discarded.
## = The test is performed but
## no candidate is discarded.
##
## +-+-----------+-----------+-----------+-----------+-----------+
## | |       Fold|      Alive|       Best|  Mean best| Exp so far|
## +-+-----------+-----------+-----------+-----------+-----------+
## |x|          1|          9|          4|     0.9541|          9|
## |=|          2|          9|          4|      0.954|         18|
## |-|          3|          2|          4|     0.9591|         27|
## |=|          4|          2|          4|      0.963|         29|
## |=|          5|          2|          4|     0.9651|         31|
## |-|          6|          1|          4|     0.9646|         33|
## +-+-----------+-----------+-----------+-----------+-----------+
## Selected candidate: ubSMOTE metric: auc mean value: 0.9646




