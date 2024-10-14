rm(list=ls())

library(MASS)
library(ISLR2)
library(car)
library(e1071)
library(class)
library(boot)
library(glmnet)
library(pls)


###########################################################################################################################

table.medianhousevalue <- Boston
head(table.medianhousevalue)

?Boston

# Predict medv
# LINEAR REGRESSION - predict medv using 12 predictors such as rm (average number of rooms per house), age (average age of houses), and lstat (percent of households with low socioeconomic status)

lm.fit <- lm(medv ~ lstat, data = table.medianhousevalue)
attach(table.medianhousevalue)
lm.fit <- lm(medv ~ lstat)

summary(lm.fit)
names(lm.fit)
coef(lm.fit)

# Intervalo de confianza

confint(lm.fit)

predict(lm.fit, data.frame(lstat = (c(5, 10, 15))),
  interval = "confidence")
predict(lm.fit, data.frame(lstat = (c(5, 10, 15))),
        interval = "prediction")

# As expected, the confidence and prediction intervals are centered around the same point (a predicted value of 25.05 for medv when lstat equals 10), but the latter are substantially wider

plot(lstat, medv) # plot
abline(lm.fit) # least squares regression line

abline(lm.fit, lwd = 3, col = "red")
plot(lstat, medv, col = "green")

par(mfrow = c(2, 2))
plot(lm.fit)

#plot(predict(lm.fit), rstudent(lm.fit))
#plot(hatvalues(lm.fit))
#which.max(hatvalues(lm.fit))


###########################################################################################################################

# MULTIPLE REGRESSION

#lm.fit <- lm(medv ~ lstat + age, data = table.medianhousevalue)
lm.fit <- lm(medv ~ ., data = table.medianhousevalue)
summary(lm.fit)

summary(lm.fit)$r.sq # R2
summary(lm.fit)$sigma # RSE

vif(lm.fit) # variance inflation factors

lm.fit <- lm(medv ~ . - age, data = table.medianhousevalue) # excluye la edad, P-value alto
summary(lm.fit)

lm.fit2 <- lm(medv ~ lstat + I(lstat^2), data = table.medianhousevalue)
summary(lm.fit2)


anova(lm.fit, lm.fit2)
par(mfrow = c(2, 2))
plot(lm.fit2)

table.cardata <- Carseats
head(table.cardata)

# Predict Sales

lm.fit <- lm(Sales ~. + Income:Advertising + Price:Age, data = table.cardata)
summary(lm.fit)

contrasts(table.cardata$ShelveLoc) # dummy

###########################################################################################################################

# LOGISTIC REGRESSION
# S&P data - % returns stock index, 2001 to 2005, 5 previous trading days (lag#)

names(Smarket)
SYP <- Smarket
names(SYP)
dim(SYP)
summary(SYP)

cormatrizSYP <- cor(SYP[, -9]) # corr lag/today close to cero

plot(SYP$Volume) # volume increases

glm.fits <- glm( 
  Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Volume, data = SYP, family = binomial
)
summary(glm.fits) # smallest P-V Lag1 i.e. retornos positivos ayer, menos probable al alza hoy
# sin embargo en 0,15 el P-V es alto, no hay evidencia significativa de asociatividad Lag1/Direction

coef(glm.fits)
summary(glm.fits)$coef
summary(glm.fits)$coef[, 4]

# P(Y = 1|X) probabilidad de que el mercado sea bullish - type = "response"

glm.probs <- predict(glm.fits, type = "response")
glm.probs
contrasts(SYP$Direction) # dummy

glm.pred <- rep("Down", 1250)
glm.pred[glm.probs > .5] = "Up"

table(glm.pred, SYP$Direction) # goes up on 512 days, goes down on 146 days
mean(glm.pred == SYP$Direction) # fracción de los días para los cuales la predicción fue correcta / 1-mean = error!
# subestima el test error rate

# predecimos con remuestreo, out of sample, 2001-2004 a 2005

#train <- SYP[SYP$Year < 2005, ]
#SYP.2005 <- SYP[SYP$Year >=2005, ]
#SYP$Direction <- as.character(SYP$Direction)
#Direction.2005 <- SYP$Direction[!SYP$Year %in% train$Year]

train <- (SYP$Year < 2005)
SYP.2005 <- SYP[!train, ]
dim(SYP.2005)
Direction.2005 <- SYP$Direction[!train]

table(SYP$Direction)
table(Direction.2005)
#SYP$Direction <- as.numeric(factor(SYP$Direction, levels = c("Down", "Up"), labels = c(0, 1)))

glm.fits <- glm(
  Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, 
  data = SYP, family = binomial, subset = train
)

glm.probs <- predict(glm.fits, SYP.2005, type = "response") # trained and tested two separate sets: training before 2005, testing in 2005

glm.pred <- rep("Down", 252)
glm.pred[glm.probs > .5] <- "Up"
table(glm.pred, Direction.2005)

mean(glm.pred == Direction.2005)
mean(glm.pred != Direction.2005) # TEST ERROR peor que una adivinanza, obviamente

# P-Values underwhelming, podemos remover las variales que parecen no significativas para predecir Direction

glm.fits <- glm(Direction ~ Lag1 + Lag2, data = SYP, family = binomial, subset = train)
glm.probs <- predict(glm.fits, SYP.2005,
                     type = "response")
glm.pred <-  rep("Down", 252)
glm.pred[glm.probs > .5] <- "Up"

table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005) # up$up / (up$up + down$up)
106 / (106+76) # days when logistic reg predicts increase it has a 58% accuracy rate

# Suppose that we want to predict the returns associated a particular Direction
# on a day when Lag1 and Lag2 equal 1.2 and 1.1, respectively, and on a day when
# they equal 1.5 and −0.8. We do this using the predict() function.

predict(glm.fits, 
        newdata = data.frame(Lag1 = c(1.2, 1.5), Lag2 = c(1.1, -0.8)),
        type = "response"
)


#############################################################################################################################

# LINEAR DISCRIMINANT ANALYSIS (LDA or NDA) to find a linear combination of features that characterizes or separates two or more classes of objects or events

lda.fit <- lda(Direction ~ Lag1 + Lag2, data = SYP, subset = train)
lda.fit
plot(lda.fit)

# Prior probabilities of groups: de las observaciones, 49,2% == días bearish, 50,8% == días bullish

# Means == tendencia para los rendimientos de los dos días premios de ser negativos en días bullish (y viceversa)

# Coeficientes: -cxLag1 - cxLag2 grande => predice bullish / bajo => predice bearish

lda.pred <- predict(lda.fit, SYP.2005)
names(lda.pred) # Class up/down - posterior probabilidad

lda.class <- lda.pred$class
table(lda.class, Direction.2005)
mean(lda.class == Direction.2005)

sum(lda.pred$posterior[, 1] >= .5)
sum(lda.pred$posterior[, 1] < .5)

# Posterior probability corresponde a la probabilidad de que el mercado sea bearish

lda.pred$posterior[1:20, 1]
lda.class[1:20]

# suppose we wish to predict a market decrease only if we are very certain that the market will bearish on that day, prob at least 90%

sum(lda.pred$posterior[, 1] > .9) # cero


#############################################################################################################################

# QUADRATIC DISCRIMINANT ANALYSIS (QDA) proportion of data that belong to class, assumes each class ~ Gaussian page 189

qda.fit <- qda(Direction ~ Lag1 + Lag2, data = SYP, subset = train)
qda.fit
qda.class <- predict(qda.fit, SYP.2005)$class
table(qda.class, Direction.2005)
mean(qda.class == Direction.2005)

# QDA predict accurate nearly 60% of the time even without 2005's data


#############################################################################################################################

# NAIVE BAYES (p190)

nb.fit <- naiveBayes(Direction ~ Lag1 + Lag2, data = SYP, subset = train)
nb.fit

nb.class <- predict(nb.fit, SYP.2005)
table(nb.class, Direction.2005)
mean(nb.class == Direction.2005)

nb.preds <- predict(nb.fit, SYP.2005, type = "raw")
nb.preds[1:5, ]


#############################################################################################################################

# K-NEAREST NEIGHBORS

# Usa predicciones con 1 solo comando. Necesita 4 inputs: matriz training, matriz test o precictor, vector con el class de los predictores, y un valor de K

names(SYP)
train.X <- cbind(SYP$Lag1, SYP$Lag2)[train, ]
test.X <- cbind(SYP$Lag1, SYP$Lag2)[!train, ]
train.Direction <- SYP$Direction[train]

# random seed before we apply knn() because if several observations are tied as nearest neighbors, then R will randomly break the tie. Therefore, a seed must be set in order to ensure reproducibility of results.

set.seed(1)

knn.pred <- knn(train.X, test.X, train.Direction, k=1)
table(knn.pred, Direction.2005)
mean(knn.pred == Direction.2005)

# The results using K = 1 are not very good, since only 50% of the observations are correctly predicted.

knn.pred <- knn(train.X, test.X, train.Direction, k=3)
table(knn.pred, Direction.2005)
mean(knn.pred == Direction.2005)

## QDA provides the best results of the methods that we have examined so far.


caravan.demog <- Caravan
dim(caravan.demog)
names(caravan.demog)
# Purchase is response variable: si compran o no un seguro de caravana
summary(caravan.demog$Purchase)
348 / 5822 # Variables de mayor escala => mayor impacto en la distancia entre observaciones. Incluso si la intuición dice lo contrario.

# Es necesario ESTANDARIZAR

stand.X <- scale(caravan.demog[, -86]) # Excluye col86 = Purchase
var(caravan.demog[, 1])
var(stand.X[, 1])
mean(caravan.demog[, 1])
mean(stand.X[, 1])

# Ahora c/columna media = 0, var =1

# Splt: test set 1.000 obs

test <- 1:1000
train.X <- stand.X[-test, ]
test.X <- stand.X[test, ]

train.Y <- caravan.demog$Purchase[-test]
test.Y <- caravan.demog$Purchase[test]

set.seed(1)

knn.pred <-  knn(train.X, test.X, train.Y, k=1)

# Error 11,8% / 5,9% de los clientes compraron el seguro, por lo que podía bajarse 6% el error by always predicting "No" regardless of the values of the predictors!
# Si se intenta vender pólizas a un grupo aleatorio de clientes, el índice de éxito es sólo del 6
# Suponemos que les interesa vender sólo a aquellos que potencialmente comprarían, por lo que ese %% no interesa

mean(test.Y != knn.pred)
mean(test.Y != "No")
table(knn.pred, test.Y)

9 / ( 68+9 )

knn.pred <-  knn(train.X, test.X, train.Y, k=3)
5 / (19+5)

knn.pred <-  knn(train.X, test.X, train.Y, k=5)
4 / (11+4) # A medida que aumenta el valor de k, aumenta la tasa de éxito. Parece encontrar algun patrón en la data

# con k=5 sólo 15 clientes comprarían la póliza

# comparamos con una regresion logistica

glm.fits <- glm(Purchase ~ ., data = caravan.demog, family = binomial, subset = -test)
glm.probs <- predict(glm.fits, caravan.demog[test, ], type = "response")
glm.pred <- rep("No", 1000)
glm.pred[glm.probs > .5] <- "Yes"

# con cut en 0.5 solo 7 obs compran la póliza

table(glm.pred, test.Y)

glm.pred <- rep("No", 1000)
glm.pred[glm.probs > .25] <- "Yes"

11/(22+11)

# con cut en 0.25 33 clientes compran / x5 mejor que random guess



#############################################################################################################################

# POISSON REGRESSION

rm(list=ls())


bike.rentals <- Bikeshare
names(bike.rentals)
dim(bike.rentals)
head(bike.rentals)

{
linear.reg <- lm( 
  bikers ~ mnth + hr + workingday + temp + weathersit, 
  data = bike.rentals
  )
summary(linear.reg)
}

# los primeros niveles hr(0) y mnth (Jan) son baseline values (beta cero). 
# mnthFeb = 6,845 implica que ceteris paribus aprox +7 más bikers en febrero que en enero

contrasts(bike.rentals$hr) = contr.sum(24)
contrasts(bike.rentals$mnth) = contr.sum(12)

{
  linear.reg2 <- lm( 
    bikers ~ mnth + hr + workingday + temp + weathersit, 
    data = bike.rentals
  )
  summary(linear.reg2)
}

# mnth2 = -46 implica que ceteris paribus aprox -46 menos bikers en febrero respecto al PROMEDIO ANUAL

# es otra forma de plantearlo
# el coeficiente estimado se calcula para todo excepto el último valor de cada variable (hr y mnth)
# ese último coeficiente == suma negativa de los estimados para todos los otros niveles

sum((predict(linear.reg) - predict(linear.reg2))^2)

# es decir que los coeficientes de cada variable suman cero! intepretado como la dif respecto a las medias

coeficientes.mes <- c(coef(linear.reg2)[2:12],
                      -sum(coef(linear.reg2)[2:12])) 

plot(coeficientes.mes, xlab = "Month", ylab = "Coefficient", xaxt = "n", col = "red", pch = 19, type = "o")
axis(side = 1, at = 1:12, labels = c("J", "F", "M", "A",
                                     "M", "J", "J", "A", "S", "O", "N", "D"))

coeficientes.hr <- c(coef(linear.reg2)[13:35],
                      -sum(coef(linear.reg2)[13:35])) 
plot(coeficientes.hr, xlab = "Hour", ylab = "Coefficient", xaxt = "n", col = "blue", pch = 19, type = "o")


# Ahora probamos la Regresion POISSON

{
reg.poiss <- glm(
  bikers ~ mnth + hr + workingday + temp + weathersit, data = bike.rentals, family = poisson
  )
summary(reg.poiss)
}

coef.mes <- c(coef(reg.poiss)[2:12] ,
               -sum(coef(reg.poiss)[2:12]))

plot(coef.mes , xlab = "Month", ylab = "Coefficient",
       xaxt = "n", col = "green", pch = 19, type = "o")
axis(side = 1, at = 1:12, labels = c("J", "F", "M", "A", "M",
                                       "J", "J", "A", "S", "O", "N", "D"))

coef.hora <- c(coef(reg.poiss)[13:35] ,
                  -sum(coef(reg.poiss)[13:35]))
plot(coef.hora , xlab = "Hour", ylab = "Coefficient",
       col = "green", pch = 19, type = "o")


# tipe = "response" => exp(B0 + B1X1 + ... + BnXn) 

plot(predict(linear.reg2), predict(reg.poiss, type = "response"))
abline(0, 1, col = 2, lwd = 3)


# REGRESSION GAMMA

#{
#  reg.gamma <- glm(
#    bikers ~ mnth + hr + workingday + temp + weathersit, data = bike.rentals, family = Gamma
#  )
#  summary(gamma)
#}



#############################################################################################################################

# CROSS VALIDATION
# Validation set approach

rm(list=ls())

set.seed(1)
train <- sample(392, 196)
auto.data <- Auto

?sample

{
lm.fit <- lm(mpg ~ horsepower, data = auto.data, subset = train)
summary(lm.fit)
}

mean((auto.data$mpg - predict(lm.fit, auto.data))[-train]^2)

lm.fit2 <- lm(mpg ~ poly(horsepower, 3), data = auto.data, 
              subset = train)
mean((auto.data$mpg - predict(lm.fit2, auto.data))[-train]^2)

lm.fit3 <- lm(mpg ~ poly(horsepower, 2), data = auto.data, 
              subset = train)
mean((auto.data$mpg - predict(lm.fit3, auto.data))[-train]^2)


set.seed(2)
train <- sample(392, 196)

lm.fit <-  lm(mpg ~ horsepower, data = auto.data, 
              subset = train)
mean (( auto.data$mpg - predict(lm.fit , auto.data))[-train ]^2)

lm.fit2 <-  lm(mpg ~ poly(horsepower, 2), data = auto.data, 
              subset = train)
mean (( auto.data$mpg - predict(lm.fit2 , auto.data))[-train ]^2)

lm.fit3 <-  lm(mpg ~ poly(horsepower, 3), data = auto.data, 
              subset = train)
mean (( auto.data$mpg - predict(lm.fit3 , auto.data))[-train ]^2)

# un modelo que predice mpg mediante funcion cuadrática tiene mejor performance que uno con función lineal


# Leave-One-Out Cross Validation

glm.fit <- glm(mpg ~ horsepower, data = auto.data) # borrando el family equivale a la reg lineal
coef(glm.fit)
lm.fit <- lm(mpg ~ horsepower, data = auto.data)
coef(lm.fit)

cv.err <- cv.glm(auto.data , glm.fit)
cv.err$delta # delta vector contiene los resultados de la validación cruzada
cv.err$K
# Our cross-validation estimate for the test error is approximately 24.23

# loop, iteración, fits polynomial regressions for polynomials i = 1,...,10 y devuelve los cross validation error
cv.error <- rep(0, 10)
for (i in 1:10) {
  glm.fit <- glm(mpg ~ poly(horsepower , i), data = auto.data)
  cv.error[i] <- cv.glm(auto.data , glm.fit)$delta [1]
  }
cv.error


# K-Fold Cross Validation

set.seed (17)

# K = 10
cv.error.10 <- rep(0, 10)
for (i in 1:10) {
  glm.fit <- glm(mpg ~ poly(horsepower , i), data = auto.data)
  cv.error.10[i] <- cv.glm(Auto , glm.fit , K = 10)$delta [1]
  }
cv.error.10
# little evidence that using cubic or Higher-order polynomial terms leads to lower test error than simply using a quadratic fit
# k-fold CV, two numbers delta differ slightly. The first is the standard k-fold CV estimate. The second is a bias corrected version.

##################################################################################################################################################

# THE BOOSTRAP: Estimating the accuracy
# 2-step: crear una función del estadístico de interés => función boot() 

portfolio <- Portfolio # 100 pares de retornos (X,Y)

alpha.fn <- function(data, index) {
  X <- data$X[index]
  Y <- data$Y[index]
  (var(Y) - cov(X, Y)) / (var(X) + var(Y) - 2 * cov(X, Y))
}
# generamos una función que toma ese input + un vector que indica las observaciones de la estimación

alpha.fn(portfolio, 1:100) # estimación sobre el total

set.seed(7)
alpha.fn(portfolio, sample(100, 100, replace = T)) # muestreo con reemplazo

boot(portfolio, alpha.fn, R = 1000) # 1000 estimaciones de SE(alfa) = 0,089


# Estimating the accuracy lINEAR REGRESSION

boot.fn <- function(auto.data, index)
coef(lm(mpg ~ horsepower, data = auto.data, subset = index))
boot.fn(auto.data, 1:392) # estimaciones de beta cero intercept y beta uno slope

set.seed (1)
boot.fn(auto.data, sample (392 , 392, replace = T)) # same but muestra con reposición

boot(auto.data, boot.fn, 1000) # 1000 estimaciones de los errores para los beta

summary(lm(mpg ~ horsepower, data = auto.data))$coef # los errores estandard convencionales difieren del boostrap, ya que este último no asume que la variabilidad proviene sólo de los errores y linearidad


boot.fn <- function(auto.data, index)
  + coef(
    lm(mpg ~ horsepower + I(horsepower ^2),
       data = auto.data, subset = index)
  )
set.seed (1)
boot(auto.data, boot.fn, 1000)

summary(
  lm(mpg ~ horsepower + I(horsepower ^2), data = auto.data)
)$coef



##################################################################################################################################################
################# SUBSET SELECTION METHODS #####################

rm(list=ls())

# Best Subset Selection

baseball <- Hitters # records & salaries baseball players - predict Salary

names(baseball)
dim(baseball)
sum(is.na(baseball$Salary))
baseball <- na.omit(baseball)
dim(baseball)
sum(is.na(baseball$Salary))

library(leaps)
regfit.full <- regsubsets( Salary ~ ., baseball)
summary(regfit.full)

# "*" indica si una variable está incluida en el modelo => Hits & CRBI están en los dos mejores modelos

regfit.full <- regsubsets(Salary ~ ., data = baseball,
                          nvmax = 19)
reg.summary <- summary(regfit.full)
names(reg.summary)
reg.summary$rsq
reg.summary$adjr2

{
par(mfrow = c(2,2))
plot(reg.summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "R2ajustado", type = "l")

which.max(reg.summary$adjr2)
points (11, reg.summary$adjr2 [11] , col = "red", cex = 2,
          pch = 20) # red dot to indicate the model with the largest adjusted R2 statistic

plot(reg.summary$cp, xlab = "Number of Variables",
     ylab = "Cp", type = "l")
which.min(reg.summary$cp)
points (10, reg.summary$cp[10] , col = "blue", cex = 2,
          pch = 20)

which.min(reg.summary$bic)
plot(reg.summary$bic , xlab = "Number of Variables",
       ylab = "BIC", type = "l")
points (6, reg.summary$bic [6], col = "blue", cex = 2,
        pch = 20) # blue dots to indicate the model with the lowest BIC and Cp

coef(regfit.full , 6)
}

?plot.regsubsets()

#plot(regfit.full , scale = "r2")
#plot(regfit.full , scale = "adjr2")
#plot(regfit.full , scale = "Cp")
#plot(regfit.full , scale = "bic")



# Forward/Backward stepwise selection

regfit.fwd <- regsubsets(Salary ~ ., data = baseball,
                         nvmax = 19, method = "forward")
summary(regfit.fwd)

regfit.bwd <- regsubsets(Salary ~ ., data = baseball,
                         nvmax = 19, method = "backward")
summary(regfit.fwd)


coef(regfit.full , 7)
coef(regfit.fwd , 7)
coef(regfit.bwd , 7)


# Validation Set App & Cross Validation

{
set.seed (1)
# split training and test set
 train <- sample(c(TRUE , FALSE), nrow(baseball), 
                  replace = TRUE)
 
 test <- (!train)
 
 regfit.best <- regsubsets(Salary ~ .,
                           data = baseball[train , ], nvmax = 19)
 
}

head(train)
head(test)

test.mat <- model.matrix(Salary ~ ., data = baseball[test , ])

val.errors <- rep(NA, 19)
for (i in 1:19) {
   coefi <- coef(regfit.best , id = i)
   pred <- test.mat[, names(coefi)] %*% coefi
   val.errors[i] <- mean (( baseball$Salary[test] - pred)^2)
   # (for each size i, coefficients for the best model of that size) x (appropriate columns of the test model matrix to form the predictions and compute the test MSE)
   }
val.errors

which.min(val.errors)
coef(regfit.best, 7)

# Podemos crear una función que replique y resuma todos estos pasos

predict.regsubsets <- function(object , newdata , id, ...) {
 form <- as.formula(object$call [[2]])
 mat <- model.matrix(form , newdata)
 coefi <- coef(object , id = id)
 xvars <- names(coefi)
 mat[, xvars] %*% coefi
 }


# we must perform best subset selection within each of the k training sets

k <- 10
n <- nrow(baseball)
set.seed(1)

folds <- sample(rep(1:k, length = n))
 cv.errors <- matrix(NA, k, 19,
                     dimnames = list(NULL, paste(1:19)))

# In the jth fold, the elements of folds that equal j are in the test set, and the i=/=j remainder are in the training set

 for (j in 1:k) {
 best.fit <- regsubsets(Salary ~ .,
                            data = baseball[folds != j, ],
                            nvmax = 19)
 for (i in 1:19) {
     pred <- predict(best.fit , baseball[folds == j, ], id = i)
     cv.errors [j, i] <-
       mean (( baseball$Salary[folds == j] - pred)^2)
 }
 }
# will automatically use our predict.regsubsets() function when predict() because the best.fit object has class regsubsets
 
cv.errors # 10×19 matrix, of which the (j, i)th element corresponds to the test MSE for the jth cross-validation fold for the best i-variable model
  
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
par(mfrow = c(1, 1))
plot(mean.cv.errors , type = "b") # CV selects a 10-variable model
points (10, mean.cv.errors[10], col = "green", cex = 2, pch = 20)

reg.best <- regsubsets(Salary ~., data = baseball,
                       nvmax = 19)
coef(reg.best , 10)


################# RIDGE REGRESSION AND THE LASSO ######################
# Ridge regression: estimating the coefficients of multiple-regression models in scenarios where the independent variables are highly correlated
# LASSO: least absolute shrinkage and selection operator - selección de variables y regularización para mejorar la exactitud e interpretabilidad del modelo estadístico producido

rm(list=ls())

baseball <- Hitters

names(baseball)
dim(baseball)
sum(is.na(baseball$Salary))
baseball <- na.omit(baseball)

x <- model.matrix(Salary ~ ., baseball)[, -1] # it also transforms qualitative variables into dummy!
y <- baseball$Salary

?model.matrix()

# alpha = 1 => lasso model is fit
# alpha = 0 => ridge regression is fit

grid <- 10^seq(10, -2, length = 100) # grid rango de lambda = 10^10 to lambda = 10^−2, essentially covering the full range of scenarios 
ridge.mod <- glmnet(x, y, alpha = 0, lambda = grid) # by default, the glmnet() function standardizes the variables / to turn it off: standardize = FALSE

coef(ridge.mod)
dim(coef(ridge.mod))
# 20×100 => filas - coeficientes x valores de lambda

# Se espera que los coeficientes estimados sean bastante menores, en términos de ℓ2 norm distancia euclídea, cuando el valor de lambda es algo (y viceversa)

ridge.mod$lambda[50]
coef(ridge.mod)[, 50]
sqrt(sum(coef(ridge.mod)[-1, 50]^2))

ridge.mod$lambda[60]
coef(ridge.mod)[, 60]
sqrt(sum(coef(ridge.mod)[-1, 60]^2)) # mayor sd

predict(ridge.mod , s = 50, type = "coefficients")[1:20, ]

# random split test / training sets

set.seed (1)
 train <- sample (1: nrow(x), nrow(x) / 2)
 test <- (-train)
 y.test <- y[test]

 # we fit a ridge reg on training set, and evaluate MSE on the test set with lambda = 4
 
 ridge.mod <- glmnet(x[train , ], y[train], alpha = 0,
                     lambda = grid , thresh = 1e-12)
  ridge.pred <- predict(ridge.mod , s = 4, newx = x[test , ])
  mean (( ridge.pred - y.test)^2)

  mean (( mean(y[train ]) - y.test)^2) # if we had simply fit a model with just an intercept, we would have predicted each test observation using mean of the training observations

# same result with HUGE value of lambda
  
  ridge.pred <- predict(ridge.mod , s = 1e10 , newx = x[test , ])
  mean (( ridge.pred - y.test)^2)

# ridge devuelve menor test MSE que un modelo sólo con intercept
# tiene sentido ya que least squares es simplemente un ridge con lambda cero!
  
  ridge.pred <- predict(ridge.mod , s = 0, newx = x[test , ],
                        exact = T, x = x[train , ], y = y[train ])
  mean (( ridge.pred - y.test)^2)
  lm(y ~ x, subset = train)
  predict(ridge.mod , s = 0, exact = T, type = "coefficients",
            x = x[train, ], y = y[train])[1:20, ]

  
# In general, instead of arbitrarily choosing λ = 4, it would be better to use cross-validation to choose the tuning parameter lambda
  
  set.seed (1)
  cv.out <- cv.glmnet(x[train , ], y[train], alpha = 0)
  plot(cv.out)
  bestlam <- cv.out$lambda.min
  bestlam  # value of λ that results in the smallest crossvalidation error
  
 #  What is the test MSE?
  
  ridge.pred <- predict(ridge.mod , s = bestlam ,
                        newx = x[test , ])
  mean (( ridge.pred - y.test)^2)  # improvement over the test λ = 4
  
  out <- glmnet(x, y, alpha = 0)
  predict(out , type = "coefficients", s = bestlam)[1:20, ] # usando el lambda elegido por CV, en todo el dataset

  
  # LASSO
  
  lasso.mod <- glmnet(x[train , ], y[train], alpha = 1,
                      lambda = grid)
  plot(lasso.mod) 

  set.seed (1)
  cv.out <- cv.glmnet(x[train , ], y[train], alpha = 1)
  plot(cv.out)
  bestlam <- cv.out$lambda.min
  lasso.pred <- predict(lasso.mod , s = bestlam ,
                          newx = x[test , ])
  mean (( lasso.pred - y.test)^2)  #  mucho más bajo que el MSE inicial y least squares, y similar al ridge con el lambda elegido por CV
  
  out <- glmnet(x, y, alpha = 1, lambda = grid)
  lasso.coef <- predict(out , type = "coefficients",
                        s = bestlam)[1:20, ]
  lasso.coef
  lasso.coef[lasso.coef != 0]
# para algunas variables el coeficiente es cero i.e. LASSO con lambda elegido via CV genera un modelo con menos variables
  
  
  
  #################  ###################### PCR & PLS ###################### ######################
  
  ################# PCR: principal components regression ######################
  
  set.seed (2)
  pcr.fit <- pcr(Salary ~ ., data = baseball, scale = TRUE ,
                   validation = "CV")
  summary(pcr.fit) # RMSE
  (351.5)^2 # MSE
  
  validationplot(pcr.fit , val.type = "MSEP")
  
# the cross-validation error is roughly the same when only one component and when many => small number of components might suffice.  
  
  # % variance explained: la cantidad de información sobre los predictores que es capturada utilizando M (elegido) componentes principales
  
  # menor error => M = 6
  
  pcr.pred <- predict(pcr.fit , x[test , ], ncomp = 6)
  summary(pcr.pred)
  mean (( pcr.pred - y.test)^2)

  
  ################# PLS: Partial Least Squares ######################
  
  set.seed (1)
  pls.fit <- plsr(Salary ~ ., data = baseball , subset = train,
                    scale = TRUE , validation = "CV")
  summary(pls.fit)
  validationplot(pls.fit , val.type = "MSEP")
  
  # menor error => M = 1

  pls.pred <- predict(pls.fit , x[test , ], ncomp = 1)  
  summary(pls.pred)  
  mean((pls.pred - y.test)^2)  

  pls.fit <- plsr(Salary ~ ., data = baseball , scale = TRUE ,
                  ncomp = 1) # con todo el dataset
  summary(pls.fit)
  
  # PCR only attempts to maximize the amount of variance explained in the predictors, while PLS searches for directions that explain variance in both the predictors and the response.
  
  

  