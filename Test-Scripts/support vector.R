rm(list=ls())

################# ################# SV Classifier ################# ################# 

library(e1071)

# generating observations, two classes, checking whether the classes are linearly separable
set.seed (1)
x <- matrix(rnorm (20 * 2), ncol = 2)
y <- c(rep(-1, 10), rep(1, 10))
x[y == 1, ] <- x[y == 1, ] + 1
plot(x, col = (3 - y)) # no lo son

# para que svm() devuelva clasificacion => response as factor variable

dat <- data.frame(x = x, y = as.factor(y))
svmfit <- svm(y ~ ., data = dat , kernel = "linear",
                cost = 10, scale = FALSE)
plot(svmfit , dat) # la frontera de decision es lineal como elegimos, dos clases
# las cruces son los support vectors y las obs restantes los circulos
summary(svmfit)

svmfit <- svm(y ~ ., data = dat , kernel = "linear",
              cost = 0.1, scale = FALSE)
plot(svmfit , dat)
svmfit$index

set.seed (1)
tune.out <- tune(svm , y ~ ., data = dat , kernel = "linear",
                   ranges = list(cost = c(0.001 , 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)

bestmod <- tune.out$best.model
summary(bestmod)

xtest <- matrix(rnorm (20 * 2), ncol = 2)
ytest <- sample(c(-1, 1), 20, rep = TRUE)
xtest[ytest == 1, ] <- xtest[ytest == 1, ] + 1
testdat <- data.frame(x = xtest , y = as.factor(ytest))

ypred <- predict(bestmod, testdat)
table(predict = ypred, truth = testdat$y) # 17 clasificados correctamente

# supongamos ahora clases separables
x[y == 1, ] <- x[y == 1, ] + 0.5
plot(x, col = (y + 5) / 2, pch = 19)

dat <- data.frame(x = x, y = as.factor(y))
svmfit <- svm(y ~., data = dat , kernel = "linear",
                cost = 1e5)
summary(svmfit)
plot(svmfit , dat) # no errors, 3 SV



################# ################# SVM with non-linear kernel ################# ################# 

rm(list=ls())

set.seed (1)
x <- matrix(rnorm (200 * 2), ncol = 2)
x[1:100, ] <- x[1:100, ] + 2
x[101:150, ] <- x[101:150, ] - 2 
y <- c(rep(1, 150) , rep(2, 50))
dat <- data.frame(x = x, y = as.factor(y))
plot(x, col = y)

# split
train <- sample (200 , 100)
svmfit <- svm(y ~ ., data = dat[train , ], kernel = "radial",
                gamma = 1, cost = 1)
plot(svmfit , dat[train , ])
summary(svmfit)

# varios errores, aumentando el valor de cost podemos reducirlo
#pero generando una frontera de decision mas irregular, risk of overfitting the data

svmfit <- svm(y ~ ., data = dat[train , ], kernel = "radial",
              gamma = 1, cost = 1e5)
plot(svmfit , dat[train , ])

set.seed(1)
tune.out <- tune(svm, y ~ ., data = dat[train, ],
                 kernel = "radial",
                 ranges = list(
                   cost = c(0.1, 1, 10, 100, 1000),
                   gamma = c(0.5, 1,2,3,4)
                 ))
summary(tune.out)


pred2 <- table(
  true = dat[-train , "y"],
  pred = predict(
    tune.out$best.model , newdata = dat[-train , ]
  )
)


library(ROCR)
 rocplot <- function(pred2 , truth , ...) 
  {
  predob <- prediction(pred2 , truth)
  perf <- performance(predob , "tpr", "fpr")
  plot(perf , ...)
    }

 
# SVMs and support vector classifiers output class labels for each observation.
 #However, it is also possible to obtain fitted values for each observation,
 #which are the numerical scores used to obtain the class labels

 svmfit.opt <- svm(y ~., data = dat[train , ],
                   kernel = "radial", gamma = 2, cost = 1,
                   decision.values = T)
 fitted <- attributes(
   predict(svmfit.opt , dat[train , ], decision.values = TRUE)
 )$decision.values

 par(mfrow = c(1, 2))
 rocplot(-fitted, dat[train , "y"], main = "Training Data")
 
 svmfit.flex <- svm(y ~ ., data = dat[train , ],
                    kernel = "radial", gamma = 50, cost = 1,
                    decision.values = T)
 fitted <- attributes(
   predict(svmfit.flex , dat[train , ], decision.values = T)
 )$decision.values
 rocplot(-fitted , dat[train , "y"], add = T, col = "red")

 fitted <- attributes(
   predict(svmfit.opt , dat[-train , ], decision.values = T)
 )$decision.values
 rocplot(-fitted , dat[-train , "y"], main = "Test Data")
 fitted <- attributes(
   predict(svmfit.flex , dat[-train , ], decision.values = T)
 )$decision.values
 rocplot(-fitted , dat[-train , "y"], add = T, col = "red")


################# ################# SVM multiple class ################# ################# 

#If the response is a factor containing more than two levels, then the svm()
#function will perform multi-class classification using the one-versus-one approach

set.seed (1)
x <- rbind(x, matrix(rnorm (50 * 2), ncol = 2))
y <- c(y, rep(0, 50))
x[y == 0, 2] <- x[y == 0, 2] + 2
dat <- data.frame(x = x, y = as.factor(y))
par(mfrow = c(1, 1))
plot(x, col = (y + 1))

svmfit <- svm(y ~ ., data = dat , kernel = "radial",
              cost = 10, gamma = 1)
plot(svmfit , dat)












