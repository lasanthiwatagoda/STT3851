housedata <- read.csv("https://lasanthi-asu.github.io/STT3851ClassRepo/Rmarkdown/Data/housedata.csv", 
                      colClasses = c(id = "character", date = "character", 
                                     yr_built = "character", zipcode = "factor", grade = "factor"))

housedata$date <- as.Date(housedata$date, "%Y%m%d")
housedata$waterfront <- factor(housedata$waterfront, labels = c("No", "Yes"))
housedata$condition <- factor(housedata$condition, labels = c("poor", "fair", "average", "good", "very good"))
housedata$yr_renovated <- ifelse(housedata$yr_renovated == 0, housedata$yr_built, housedata$yr_renovated)
housedata$yr_built <- as.Date(ISOdate(housedata$yr_built, 9, 1))  # Complete Year, Sept 1
housedata$yr_renovated <- as.Date(ISOdate(housedata$yr_renovated, 9, 1))  # Last renovated Year, Sept 1
housedata <- housedata[, -1]


# Test Data Set (only use this to send predictions)

#### Perform same steps with test set

housedataT <- read.csv("https://lasanthi-asu.github.io/STT3851ClassRepo/Rmarkdown/Data/housedataTEST.csv", 
                       colClasses = c(id = "character", date = "character", 
                                      yr_built = "character", zipcode = "factor", grade = "factor"))

housedataT$date <- as.Date(housedataT$date, "%Y%m%d")
housedataT$waterfront <- factor(housedataT$waterfront, labels = c("No", "Yes"))
housedataT$condition <- factor(housedataT$condition, labels = c("poor", "fair", "average", "good", "very good"))
housedataT$yr_renovated <- ifelse(housedataT$yr_renovated == 0, housedataT$yr_built, housedataT$yr_renovated)
housedataT$yr_built <- as.Date(ISOdate(housedataT$yr_built, 9, 1))  # Complete Year, Sept 1
housedataT$yr_renovated <- as.Date(ISOdate(housedataT$yr_renovated, 9, 1))  # Last renovated Year, Sept 1
#housedataT$grade <- factor(housedataT$grade, labels = c("1", "10", "11", "12", "13", "3", "4", "5", "6", "7", "8", "9"))
housedataT <- housedataT[, -1]



dim(housedata)
dim(housedataT)

x <- model.matrix (price~.,housedata )[,-1] # remove 1's
y <- housedata$price

library (glmnet)
grid <- 10^seq(10, -2, length = 100)
lasso_model <- glmnet(x, y, alpha = 1, lambda = grid)

train <- sample(1: nrow(x), nrow(x)/2)
test <- (-train)
y_test <- y[test]

cv_out_lasso <- cv.glmnet(x[train, ], y[train], alpha = 1, lambda = grid)

plot(cv_out_lasso)

bestlam <- cv_out_lasso$lambda.min
bestlam

predict(lasso_model, type ="coefficients", s = bestlam )
dim(predict(lasso_model, type ="coefficients", s = bestlam ))

lasso_pred <- predict (lasso_model ,s = bestlam, newx = x[test, ])
mean((lasso_pred - y_test)^2)

hT = model.matrix(~., housedataT)[,-1]

lasso_pred_send <- predict (lasso_model ,s = bestlam, newx = hT)
dim(lasso_pred_send)

dim(hT)
colnames(hT)
str(hT)
str(housedata)
