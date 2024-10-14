rm(list=ls())
library(ISLR2)
library(tree)

#################  ###################### Clasif Trees ###################### ######################

carseats <- Carseats
# new variable "High": Yes if the Sales variable exceeds 8, and No otherwise
High <- factor(ifelse(carseats$Sales <= 8, "No", "Yes"))

# merge
carseats <- data.frame(carseats, High)
head(carseats)
names(carseats)

tree.carseats <- tree(High ~ . - Sales, carseats) # classif tree without Sales
summary(tree.carseats) # bajo desvío indica buen fit to the training data

{
plot(tree.carseats)
text(tree.carseats, pretty = 0)
}
tree.carseats # el criterio de split


# estimacion del error
# split the observations into a training set and a test set, build the tree using the training set, and evaluate its performance on the test data

set.seed(2)
train <- sample(1:nrow(carseats), 200)
carseats.test <- carseats[-train, ]
head(carseats.test)
high.test <- High[-train]
head(high.test)

tree.carseats <- tree(High ~ . -Sales, carseats, subset = train)

tree.pred <- predict(tree.carseats, carseats.test, type = "class")

table(tree.pred, high.test)
(104 + 50) / 200 # (no-no + yes-yes) / total

set.seed(7)
cv.carseats <- cv.tree(tree.carseats, FUN = prune.misclass) # cross validation para determinar el óptimo de complejidad del tree
names(cv.carseats) # FUN = prune.misclass indica que el error rate guia el proceso de CV (en vez del default, que es el desvío)
head(cv.carseats)
cv.carseats

# el arbol con 9 nodos terminales resulta en solo 74 CV errors

par(mfrow = c(1, 2))
plot(cv.carseats$size , cv.carseats$dev, type = "b")
plot(cv.carseats$k, cv.carseats$dev, type = "b")

prune.carseats <- prune.misclass(tree.carseats , best = 9) # obtenemos el 9no nodo - si incrementamos el nodo baja accuracy
plot(prune.carseats)
text(prune.carseats , pretty = 0)

tree.pred <- predict(prune.carseats, carseats.test, type = "class")
table(tree.pred, high.test)
(97 + 58) / 200 # (no-no + yes-yes) / total
# 77.5% of the test observations are correctly classified, so more interpretable & improved accuracy.


#################  ###################### Regression Trees ###################### ######################

rm(list=ls())
medianhousevalue.Boston <- Boston
head(medianhousevalue.Boston)

set.seed(1)
train <- sample(1:nrow(medianhousevalue.Boston), nrow(medianhousevalue.Boston) / 2)
tree.boston <- tree(medv ~ ., medianhousevalue.Boston, subset = train)
summary(tree.boston)

plot(tree.boston)
text(tree.boston, pretty = 0) # larger values of rm (average number of rooms), or lower values of lstat (percentage of individuals with lower socioeconomic status), 
                              # correspond to more expensive houses. For example, the tree predicts
                               # a median house price of $45,400 for homes in census tracts in which rm >= 7.553

cv.boston <- cv.tree(tree.boston)
plot(cv.boston$size , cv.boston$dev, type = "b") # the most complex tree under consideration is selected by cross validation

pred <- predict(tree.boston , newdata = Boston[-train , ])
boston.test <- Boston[-train, "medv"]
plot(pred , boston.test)
abline (0, 1)
mean ((pred - boston.test)^2) # MSE single tree
# (MSE)^-1/2 = 5.941 => this model leads to test predictions that are (on average) within approximately $5,941 of the true median home value for the census tract.


#################  ###################### BAGGING AND RANDOM FOREST ###################### ######################

library(randomForest)

set.seed(1) # bagging m = p
bag.boston <- randomForest(medv ~ ., data = medianhousevalue.Boston, subset = train, mtry = 12, importance = TRUE)
bag.boston
# mtry => las 12 variables son tenidas en cuenta

pred.bag <- predict(bag.boston, newdata = medianhousevalue.Boston[-train, ])
plot(pred.bag, boston.test)
abline(0, 1)
mean((pred.bag - boston.test)^2) # MSE bagging 1

# podemos cambiar la cantidad de arboles con "ntree"

bag.boston <- randomForest(medv ~ ., data = medianhousevalue.Boston, subset = train, mtry = 12, ntree = 25)
pred.bag <- predict(bag.boston, newdata = medianhousevalue.Boston[-train, ])
mean((pred.bag - boston.test)^2) # MSE bagging 2

# random forest proceeds in exactly the same way, except that we use a smaller value of the mtry argument (by default p/3 variables reg trees and raíz de p clasif trees)

set.seed(1)
rf.boston <- randomForest(medv ~ ., data = medianhousevalue.Boston, subset = train, mtry = 6, importance = TRUE)

pred.rf <-  predict(rf.boston, newdata = medianhousevalue.Boston[-train, ])
mean((pred.rf - boston.test)^2) # MSE RANDOM FOREST

importance(rf.boston) # importancia de las variables
varImpPlot(rf.boston)

# In the case of regression trees, the node impurity is measured by the training RSS, and for classification trees by the deviance.




#################  ###################### BOOSTING (boosted regression trees) ###################### ######################

library(gbm)

set.seed (1)
boost.boston <- gbm(medv ~ ., data = medianhousevalue.Boston[train , ],
                      distribution = "gaussian", n.trees = 5000,
                      interaction.depth = 4)
summary(boost.boston) # lstat & rm variables mas importantes

plot(boost.boston , i = "rm") # partial dependence plots
plot(boost.boston , i = "lstat")

pred.boost <- predict(boost.boston ,
                      newdata = medianhousevalue.Boston[-train , ], n.trees = 5000)
mean((pred.boost - boston.test)^2)
summary(pred.boost) # MSE BOOSTING

# podemos utilizar otro valor del parámetro lambda λ (shrinkage)

boost.boston <- gbm(medv ~ ., data = medianhousevalue.Boston[train , ],
                    distribution = "gaussian", n.trees = 5000,
                    interaction.depth = 4, shrinkage = 0.2, verbose = F)
pred.boost <- predict(boost.boston ,
                        newdata = medianhousevalue.Boston[-train , ], n.trees = 5000)
mean((pred.boost - boston.test)^2) # MSE BOOSTING lambda = 0.2


#################  ###################### BART (bayesian aditive regression trees) ###################### ######################

library(BART)

# creamos matrices de los coef para test data y train data
head(medianhousevalue.Boston)
x <- medianhousevalue.Boston[, 1:12]
y <- medianhousevalue.Boston[, "medv"]
xtrain <- x[train, ]
ytrain <- y[train]

xtest <- x[-train, ]
ytest <- y[-train]
set.seed (1)
bartfit <- gbart(xtrain , ytrain , x.test = xtest)

yhat.bart <- bartfit$yhat.test.mean
mean (( ytest - yhat.bart)^2) # MSE BART

ord <- order(bartfit$varcount.mean , decreasing = T)
bartfit$varcount.mean[ord] # cantidad de veces que c/variable aparece en los árboles


#######################################################################################################################

################# ################# Random Forest con library(caret) ################# ################# 

# https://www.rebeccabarter.com/blog/2017-11-17-caret_tutorial

rm(list=ls())

library(caret)

library(ranger)
library(tidyverse)
library(e1071)
library(conflicted)
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")


# modelo simple

# Predicting the age of abalone from physical measurements
setwd("~/Code")
abalone_data <- read.table("abalone_data/abalone.data", sep = ",")
colnames(abalone_data) <- c("sex", "length", "diameter", "height", 
                            "whole.weight", "shucked.weight", 
                            "viscera.weight", "shell.weight", "age")
names(abalone_data)
# add a logical variable for "old" (age > 10)
abalone_data <- abalone_data %>%
  mutate(old = age > 10) %>%
  # remove the "age" variable
  select(-age)
head(abalone_data)

# split
set.seed(23489)
train_index <- sample(1:nrow(abalone_data), 0.9 * nrow(abalone_data))
a_train <- abalone_data[train_index, ]
a_test <- abalone_data[-train_index, ]
rm(abalone_data)
head(a_train)
dim(a_train)

rf_fit <- train(as.factor(old) ~ ., 
                data = a_train, 
                method = "ranger")
rf_fit

a_rf_pred <- predict(rf_fit, a_test) # predict the outcome on a test set
confusionMatrix(a_rf_pred, as.factor(a_test$old))


# modelo fancy
# pre-processing

# center, scale and perform a YeoJohnson transformation
# identify and remove variables with near zero variance
# perform pca

a_no_nzv_pca <- preProcess(select(a_train, - old), 
                                 method = c("center", "scale", "nzv", "pca"))
a_no_nzv_pca
a_no_nzv_pca$method
a_no_nzv_pca$rotation # componentes principales

# data spliting
# 10 different 80% subsamples
# train_index <- createDataPartition(iris$Species, p = 0.8, list = FALSE, times = 10)
# head(train_index) 

# k-fold cross validation 

a_grouped <- cbind(a_train[1:50, ], group = rep(1:10, each = 5))
head(a_grouped, 10) # 10 fold CV
folds <- groupKFold(a_grouped$group, k = 10)
folds

# resampling
set.seed(998)
training <- createDataPartition(a_train$old, p = .75, list = FALSE)
trainingset <- a_train[ training,]
testingset  <- a_train[-training,]

fit_control <- trainControl(## 10 foldCV 
  method = "cv", number = 10)

set.seed(825)
rf_fit <- train(as.factor(old) ~ ., 
                data = a_train, 
                method = "ranger",
                trControl = fit_control)
rf_fit

# usando folds

folds_fit_control <- trainControl(
  index = folds,
  method = "cv")

set.seed(825)
rf_fit <- train(as.factor(old) ~ ., 
                data = select(a_grouped, - group), 
                method = "ranger",
                trControl = folds_fit_control)
rf_fit





