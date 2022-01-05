# Ridge Regression
library(ISLR)
Hitters <- na.omit(Hitters)  # Remove missing values
X <- model.matrix(Salary ~ ., data = Hitters)[, -1]
y <- Hitters$Salary
library(glmnet)
grid <- 10^seq(10, -2, length = 100)
#
ridge.mod <- glmnet(X, y, alpha = 0, lambda = grid)
dim(coef(ridge.mod))
plot(ridge.mod, label = TRUE, xvar = "norm")
plot(ridge.mod, label = TRUE, xvar = "lambda")
plot(ridge.mod, label = TRUE, xvar = "dev")
#
print(ridge.mod)
# Parallel computing does not do much with this example.
#
library(doMC)
registerDoMC(cores = 2)
system.time(cv.glmnet(X, y))
system.time(cv.glmnet(X, y, parallel = TRUE))

#
#
cvfit <- cv.glmnet(X, y, nfolds = 10)
plot(cvfit)
cvfit$lambda.min
cvfit$lambda.1se
#
system.time(cv.glmnet(X, y, nfolds = 5))
system.time(cv.glmnet(X, y, nfolds = 5, parallel = TRUE))

