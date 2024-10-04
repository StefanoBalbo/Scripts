rm(list=ls())

library(ISLR2)


###################### Polynomial Regression ######################

salarios <- Wage
head(salarios)
summary(salarios)
model <-lm(wage ~ poly(age, 4), data = salarios) # 4th degree polynomial
coef(summary(model)) # matrix: each column is a linorthogonal ear combination of the variables age, age^2, age^3 and age^4.

model.2 <-lm(wage ~ poly(age, 4, raw = T), data = salarios)
coef(summary(model.2)) # obtenemos los coef directamente ( raw = T)

# si bien una u otra forma afecta el resultado de los coeficientes, no incide sobre los fitted values

#model.2a <-  lm(wage ~ age + I(age^2) + I(age^3) + I(age^4), data = salarios)
#model.2b <-  lm(wage ~ cbind(age, age^2, age^3, age^4,), data = salarios)

agelims <-  range(salarios$age)
age.grid <- seq(from = agelims[1], to = agelims [2])
pred <- predict(model, newdata = list(age = age.grid), se = TRUE)
bands <- cbind(pred$model + 2 * pred$se.fit, pred$fit - 2 * pred$se.fit)

attach(Wage)
par(mfrow = c(1, 2), mar = c(4.5, 4.5, 1, 1), oma = c(0, 0, 4, 0))
plot(age, wage, xlim = agelims, cex = .5, col = "yellow")
title("Degree-4 Polynomial", outer = T)
lines(age.grid, pred$fit, lwd = 2, col = "red")
matlines(age.grid, bands, lwd = 1, col = "blue", lty = 3)

pred2 <-  predict(model.2, newdata = list(age = age.grid), se = TRUE)
max(abs(pred$fit - pred2$fit)) # cómo se mencionó, no hay diferencias en los fitted values

# ¿polinomios de qué grado usamos? podemos determinarlo vía test de hipotesis

mod.1 <- lm(wage ~ age , data = salarios)
mod.2 <- lm(wage ~ poly(age , 2), data = salarios)
mod.3 <- lm(wage ~ poly(age , 3), data = salarios)
mod.4 <- lm(wage ~ poly(age , 4), data = salarios)
mod.5 <- lm(wage ~ poly(age , 5), data = salarios)
anova(mod.1, mod.2, mod.3, mod.4, mod.5)

# el p value pr() = 0 y cercano a 0 cuando se compara mod1-mod2 y mod2-mod3
# mod3 vs mod4 nos da un p value de aproximadamente 5% i.e. polinomio a la 3 o 4 encajaría

#coef(summary(mod.5)) # otra forma
#(-11.9830341^2) # == F-stat es igual al calculado antes con ANOVA

# y también podríamos haber aplicado CROSS VALIDATION!



# predicción: probab de que un individuo gane más de USD 250.000 al año

fit <- glm(I(wage > 250) ~ poly(age, 4), data = salarios, family = binomial)
summary(fit) # usa I() para una respuesta binaria TRUE = 1 FALSE = 0

#pred <- predict(fit, newdata = list(age = age.grid), type = "response", se = T)
#summary(pred) # devuelve probabilidades negativas

pred <- predict(fit, newdata = list(age = age.grid), se = T)

# por default devuelve todo en terminos de X*Beta (ver fórmula del libro p315)
# hacemos una transformación para expresarlo en términos de la probabilidad

pfit <- exp(pred$fit) / (1+ exp(pred$fit))
bands.logit <- cbind(pred$fit + 2 * pred$se.fit, pred$fit - 2 * pred$se.fit)
bands <- exp(bands.logit) / (1 + exp(bands.logit))

plot(age , I(wage > 250), xlim = agelims , type = "n",
     ylim = c(0, .2))
points(jitter(age), I(( wage > 250) / 5), cex = .5, pch = "|",
         col = "darkgrey")
lines(age.grid, pfit , lwd = 2, col = "red")
matlines(age.grid , bands , lwd = 1, col = "green", lty = 3)

# used jitter() so that observations with the same age value do not cover each other up (rug plot)


# step function:
table(cut(age, 4))
fit <- lm(wage ~ cut(age, 4), data = salarios)
coef(summary(fit)) # variables dummy cortando el rango de edades en cuatro
# the intercept coefficient of $94,160 can be interpreted as the average salary for age < 33.5


###################### Splines ######################

# The main difference between polynomial and spline is that polynomial regression gives a single polynomial that models your entire data set. 
# Spline interpolation, however, yield a piecewise continuous function composed of many polynomials to model the data set.


rm(list=ls())
salarios <- Wage
agelims <-  range(salarios$age)
age.grid <- seq(from = agelims[1], to = agelims [2])

library(splines)

fit <- lm(wage ~ bs(age, knots = c(25, 45, 65)), data = salarios) # basis functions spline
pred <- predict(fit, newdata = list(age = age.grid), se = T)

plot(salarios$age, salarios$wage, col = "blue")
lines(age.grid, pred$fit, lwd = 2)
lines(age.grid, pred$fit + 2 * pred$se.fit, lty = "dashed")
lines(age.grid, pred$fit - 2 * pred$se.fit, lty = "dashed")

# spline with six basis functions. (Recall that a cubic spline with three knots has seven degrees of freedom; these degrees of freedom are used up by an intercept, plus six basis functions.)

dim(bs(salarios$age , knots = c(25, 40, 60)))
dim(bs(salarios$age , df = 6))
attr(bs(salarios$age , df = 6), "knots") # elige los knots en base a los 25/50/75 percentiles de la edad

fit2 <- lm(wage ~ ns(age, df = 4), data = salarios) # natural spline
pred2 <- predict(fit2, newdata = list(age = age.grid), se = T)
lines(age.grid, pred2$fit2, col = "red", lwd = 2)

plot(salarios$age, salarios$wage, xlim = agelims, cex = .5, col = "darkgrey")
title("Smooth Spline")
fit <- smooth.spline(salarios$age, salarios$wage, df = 16) # qué valor de lambda arroja 16 grados de libertad
fit2 <- smooth.spline(salarios$age, salarios$wage, cv = TRUE) # determinamos los grados de libertad vía cross validation

fit$df
fit2$df

lines(fit, col = "red", lwd = 2)
lines(fit2, col = "blue", lwd = 2)
legend("topright", legend = c("16 DF", "Cross Validation (6.8) DF"), col = c("red", "blue"), lty = 1, lwd = 2, cex = .8)


plot(salarios$age, salarios$wage, xlim = agelims, cex = .5, col = "darkgrey")
title("Local Regression") # local linear regression
fit <- loess(wage ~ age, span = .2, data = salarios)
fit2 <-  loess(wage ~ age, span = .5, data = salarios)

lines(age.grid, predict(fit, data.frame(age = age.grid)), col = "red", lwd = 2)
lines(age.grid, predict(fit2, data.frame(age = age.grid)), col = "blue", lwd = 2)

legend("topright", legend = c("Span = 0.2", "Span = 0.5"), col = c("red", "blue"), lty = 1, lwd = 2, cex = .8)


###################### GAM ######################
# natural spline functions of year and age, treating education as a qualitative predictor
# big linear regression model using an appropriate choice of basis functions

rm(list=ls())
salarios <- Wage

gam1 <- lm(wage ~ ns(year , 4) + ns(age , 5) + education ,
           data = salarios)

library(gam) # s() for smooth spline
# year 4 DF, age 5 DF, education dummy

gam.m3 <- gam(wage ~ s(year, 4) + s(age , 5) + education, data = salarios) # modelo con función spline del año
par(mfrow = c(1, 3))
plot(gam.m3, se = TRUE, col = "blue")

# desarrollamos dos modelo más para comparar

gam.m1 <- gam(wage ~ s(age , 5) + education , data = salarios) # modelo que excluye año
gam.m2 <- gam(wage ~ year + s(age , 5) + education , data = salarios) # modelo con función lineal del año

anova(gam.m1, gam.m2, gam.m3, test = "F")

# p-val = 0,0001447 => M2 con función lineal es mejor que excluirlo M1
# sin embargo no hay evidencia significativa de que la no lineal M3 sea necesaria (p-val = 0,3485)

summary(gam.m3)

# ANOVA parametric => los p-val reflejan que las 3 variables son estadísticam significativas
# ANOVA non-par => los p-val de year & age corresponden a una H0: relación lineal (vs no lineal H1)
# alto p-val p/year => lineal
# bajo p-val p/age => no lineal

pred <- predict(gam.m2, newdata = salarios)

gam.lo <- gam(
  wage ~ s(year , df = 4) + lo(age , span = 0.7) + education ,
  data = salarios
)
plot.Gam(gam.lo, se = TRUE , col = "green")

library(akima)
gam.lo.i <- gam(wage ~ lo(year , age , span = 0.5) + education ,
                data = salarios)
plot(gam.lo.i)

gam.lr <- gam(
  I(wage > 250) ~ year + s(age , df = 5) + education ,
  family = binomial , data = salarios
)
par(mfrow = c(1, 3))
plot(gam.lr, se = T, col = "green")

table(salarios$education , I(salarios$wage > 250)) # there are no high earners in the < HS - we can fit a logistic regression GAM using all but this category

gam.lr.s <- gam(
  I(wage > 250) ~ year + s(age , df = 5) + education ,
  family = binomial , data = salarios ,
  subset = (education != "1. < HS Grad")
)
plot(gam.lr.s, se = T, col = "green")








