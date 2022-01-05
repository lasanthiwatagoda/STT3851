##### Rework with ggplot2 - note that it is very slow this way so changed
##### Ntrains from 1000 to 250

Ntrains <- 250    # Number of training sets to generate
n <- 40           # Number of observations to generate for each training set
dpt <- 5*9*5      # Number of x points to predict over
SD <- 0.5
#
xs <- sort(runif(n, 5, 9))
ys <- sin(xs) + rnorm(n, 0, SD)
DF <- data.frame(xs, ys)
library(ggplot2)
p1 <- ggplot(data = DF, aes(x = xs, y = ys)) + 
  geom_point(size = 1, color = "lightblue") + 
  geom_smooth(method = "lm", formula = y ~ 1, linetype = "dashed", se = FALSE) + 
  theme_bw() +
  lims(y = c(-2.5, 2.5)) + 
  labs(x = "X", y = "Y")
#
xs <- sort(runif(n, 5, 9))
ys <- sin(xs) + rnorm(n, 0, SD)
DF <- data.frame(xs, ys)
library(ggplot2)
p2 <- ggplot(data = DF, aes(x = xs, y = ys)) + 
  geom_point(size = 1, color = "lightblue") + 
  geom_smooth(method = "lm", formula = y ~ 1, linetype = "dashed", se = FALSE) + 
  theme_bw() +
  lims(y = c(-2.5, 2.5)) + 
  labs(x = "X", y = "Y")
#
xs <- sort(runif(n, 5, 9))
ys <- sin(xs) + rnorm(n, 0, SD)
DF <- data.frame(xs, ys)
library(ggplot2)
p3 <- ggplot(data = DF, aes(x = xs, y = ys)) + 
  geom_point(size = 1, color = "lightblue") + 
  geom_smooth(method = "lm", formula = y ~ 1, linetype = "dashed", se = FALSE) + 
  theme_bw() +
  lims(y = c(-2.5, 2.5)) + 
  labs(x = "X", y = "Y")
#
xs <- sort(runif(n, 5, 9))
ys <- sin(xs) + rnorm(n, 0, SD)
DF <- data.frame(xs, ys)
library(ggplot2)
p4 <- ggplot(data = DF, aes(x = xs, y = ys)) + 
  geom_point(size = 1, color = "lightblue") + 
  geom_smooth(method = "lm", formula = y ~ 1, linetype = "dashed", se = FALSE) + 
  theme_bw() +
  lims(y = c(-2.5, 2.5)) + 
  labs(x = "X", y = "Y")
#
gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2)

##########################################################################


#############################################################################
library(ggplot2)
p <- ggplot(data = data.frame(x = 5:9), aes(x = x)) + 
  stat_function(fun = sin, size = 0, n = 225) + 
  theme_bw() +
  lims(y = c(-2, 2)) + 
  labs(x = "X", y = "Y")
# curve(sin, 5, 9, ylim = c(-2, 2), ylab = "Y", xlab = "X", lwd = 3, type = "n")
yhn <- matrix(NA, Ntrains, dpt)
MSEtest <- numeric(Ntrains)
MSEtrain <- numeric(Ntrains)
for(i in 1:Ntrains){
  xs <- sort(runif(n, 5, 9))
  ys <- sin(xs) + rnorm(n, 0, SD)
  mod1 <- lm(ys ~ 1)
  ysn <- predict(mod1)
  NDF <- data.frame(xs, ysn)
  # lines(xs, nys, col = "pink", lty = "dashed")
  p <- p + geom_line(data = NDF, aes(x = xs, y = ysn), size = 0.1, color = "pink")
  nxs <- seq(5, 9, length = dpt)
  yhn[i, ] <- predict(mod1, newdata = data.frame(xs = nxs))
  yst <- sin(xs) + rnorm(n, 0, SD)
  MSEtest[i] <- mean((yst - ysn)^2)
  MSEtrain[i] <- mean((ys - ysn)^2)
}
p
yhnbar <- apply(yhn, 2, mean)
# curve(sin, 5, 9, ylim = c(-3, 3), ylab = "Y", xlab = "X", lwd = 3, add = TRUE)
p <- p + stat_function(fun = sin, size = 1, n = 225, color = "blue")
# lines(nxs, yhnbar, col = "red", lwd = 2)
NDF2 <- data.frame(nxs, yhnbar)
p + geom_line(data = NDF2, aes(x = nxs, y = yhnbar), size = 1, color = "red")
avgMSEtest <- mean(MSEtest)
avgMSEtrain <- mean(MSEtrain)
c(avgMSEtest, avgMSEtrain)


###############
Ntrains <- 250

library(ggplot2)
p <- ggplot(data = data.frame(x = 5:9), aes(x = x)) + 
   stat_function(fun = sin, size = 0, n = 225) + 
   theme_bw() +
   lims(y = c(-2, 2)) + 
   labs(x = "X", y = "Y")
# curve(sin, 5, 9, ylim = c(-2, 2), ylab = "Y", xlab = "X", lwd = 3, type = "n")
yhn <- matrix(NA, Ntrains, dpt)
MSEtest <- numeric(Ntrains)
MSEtrain <- numeric(Ntrains)
for(i in 1:Ntrains){
  xs <- sort(runif(n, 5, 9))
  ys <- sin(xs) + rnorm(n, 0, SD)
  mod1 <- lm(ys ~ xs)
  ysn <- predict(mod1)
  NDF <- data.frame(xs, ysn)
  # lines(xs, nys, col = "pink", lty = "dashed")
  p <- p + geom_line(data = NDF, aes(x = xs, y = ysn), size = 0.1, color = "pink")
  nxs <- seq(5, 9, length = dpt)
  yhn[i, ] <- predict(mod1, newdata = data.frame(xs = nxs))
  yst <- sin(xs) + rnorm(n, 0, SD)
  MSEtest[i] <- mean((yst - ysn)^2)
  MSEtrain[i] <- mean((ys - ysn)^2)
}
p
yhnbar <- apply(yhn, 2, mean)
# curve(sin, 5, 9, ylim = c(-3, 3), ylab = "Y", xlab = "X", lwd = 3, add = TRUE)
p <- p + stat_function(fun = sin, size = 1, n = 225, color = "blue")
# lines(nxs, yhnbar, col = "red", lwd = 2)
NDF2 <- data.frame(nxs, yhnbar)
p + geom_line(data = NDF2, aes(x = nxs, y = yhnbar), size = 1, color = "red")
avgMSEtest <- mean(MSEtest)
avgMSEtrain <- mean(MSEtrain)
c(avgMSEtest, avgMSEtrain)


###############
Ntrains <- 250

library(ggplot2)
p <- ggplot(data = data.frame(x = 5:9), aes(x = x)) + 
  stat_function(fun = sin, size = 0, n = 225) + 
  theme_bw() +
  lims(y = c(-2, 2)) + 
  labs(x = "X", y = "Y")
# curve(sin, 5, 9, ylim = c(-2, 2), ylab = "Y", xlab = "X", lwd = 3, type = "n")
yhn <- matrix(NA, Ntrains, dpt)
MSEtest <- numeric(Ntrains)
MSEtrain <- numeric(Ntrains)
for(i in 1:Ntrains){
  xs <- sort(runif(n, 5, 9))
  ys <- sin(xs) + rnorm(n, 0, SD)
  mod1 <- lm(ys ~ poly(xs, 2))
  ysn <- predict(mod1)
  NDF <- data.frame(xs, ysn)
  # lines(xs, nys, col = "pink", lty = "dashed")
  p <- p + geom_line(data = NDF, aes(x = xs, y = ysn), size = 0.1, color = "pink")
  nxs <- seq(5, 9, length = dpt)
  yhn[i, ] <- predict(mod1, newdata = data.frame(xs = nxs))
  yst <- sin(xs) + rnorm(n, 0, SD)
  MSEtest[i] <- mean((yst - ysn)^2)
  MSEtrain[i] <- mean((ys - ysn)^2)
}
p
yhnbar <- apply(yhn, 2, mean)
# curve(sin, 5, 9, ylim = c(-3, 3), ylab = "Y", xlab = "X", lwd = 3, add = TRUE)
p <- p + stat_function(fun = sin, size = 1, n = 225, color = "blue")
# lines(nxs, yhnbar, col = "red", lwd = 2)
NDF2 <- data.frame(nxs, yhnbar)
p + geom_line(data = NDF2, aes(x = nxs, y = yhnbar), size = 1, color = "red")
avgMSEtest <- mean(MSEtest)
avgMSEtrain <- mean(MSEtrain)
c(avgMSEtest, avgMSEtrain)


###############
Ntrains <- 250

library(ggplot2)
p <- ggplot(data = data.frame(x = 5:9), aes(x = x)) + 
  stat_function(fun = sin, size = 0, n = 225) + 
  theme_bw() +
  lims(y = c(-2, 2)) + 
  labs(x = "X", y = "Y")
# curve(sin, 5, 9, ylim = c(-2, 2), ylab = "Y", xlab = "X", lwd = 3, type = "n")
yhn <- matrix(NA, Ntrains, dpt)
MSEtest <- numeric(Ntrains)
MSEtrain <- numeric(Ntrains)
for(i in 1:Ntrains){
  xs <- sort(runif(n, 5, 9))
  ys <- sin(xs) + rnorm(n, 0, SD)
  mod1 <- lm(ys ~ poly(xs, 3))
  ysn <- predict(mod1)
  NDF <- data.frame(xs, ysn)
  # lines(xs, nys, col = "pink", lty = "dashed")
  p <- p + geom_line(data = NDF, aes(x = xs, y = ysn), size = 0.1, color = "pink")
  nxs <- seq(5, 9, length = dpt)
  yhn[i, ] <- predict(mod1, newdata = data.frame(xs = nxs))
  yst <- sin(xs) + rnorm(n, 0, SD)
  MSEtest[i] <- mean((yst - ysn)^2)
  MSEtrain[i] <- mean((ys - ysn)^2)
}
p
yhnbar <- apply(yhn, 2, mean)
# curve(sin, 5, 9, ylim = c(-3, 3), ylab = "Y", xlab = "X", lwd = 3, add = TRUE)
p <- p + stat_function(fun = sin, size = 1, n = 225, color = "blue")
# lines(nxs, yhnbar, col = "red", lwd = 2)
NDF2 <- data.frame(nxs, yhnbar)
p + geom_line(data = NDF2, aes(x = nxs, y = yhnbar), size = 1, color = "red")
avgMSEtest <- mean(MSEtest)
avgMSEtrain <- mean(MSEtrain)
c(avgMSEtest, avgMSEtrain)





###############
Ntrains <- 250

library(ggplot2)
p <- ggplot(data = data.frame(x = 5:9), aes(x = x)) + 
  stat_function(fun = sin, size = 0, n = 225) + 
  theme_bw() +
  lims(y = c(-2, 2)) + 
  labs(x = "X", y = "Y")
# curve(sin, 5, 9, ylim = c(-2, 2), ylab = "Y", xlab = "X", lwd = 3, type = "n")
yhn <- matrix(NA, Ntrains, dpt)
MSEtest <- numeric(Ntrains)
MSEtrain <- numeric(Ntrains)
for(i in 1:Ntrains){
  xs <- sort(runif(n, 5, 9))
  ys <- sin(xs) + rnorm(n, 0, SD)
  mod1 <- lm(ys ~ poly(xs, 5))
  ysn <- predict(mod1)
  NDF <- data.frame(xs, ysn)
  # lines(xs, nys, col = "pink", lty = "dashed")
  p <- p + geom_line(data = NDF, aes(x = xs, y = ysn), size = 0.1, color = "pink")
  nxs <- seq(5, 9, length = dpt)
  yhn[i, ] <- predict(mod1, newdata = data.frame(xs = nxs))
  yst <- sin(xs) + rnorm(n, 0, SD)
  MSEtest[i] <- mean((yst - ysn)^2)
  MSEtrain[i] <- mean((ys - ysn)^2)
}
p
yhnbar <- apply(yhn, 2, mean)
# curve(sin, 5, 9, ylim = c(-3, 3), ylab = "Y", xlab = "X", lwd = 3, add = TRUE)
p <- p + stat_function(fun = sin, size = 1, n = 225, color = "blue")
# lines(nxs, yhnbar, col = "red", lwd = 2)
NDF2 <- data.frame(nxs, yhnbar)
p + geom_line(data = NDF2, aes(x = nxs, y = yhnbar), size = 1, color = "red")
avgMSEtest <- mean(MSEtest)
avgMSEtrain <- mean(MSEtrain)
c(avgMSEtest, avgMSEtrain)



###############
Ntrains <- 250

library(ggplot2)
p <- ggplot(data = data.frame(x = 5:9), aes(x = x)) + 
  stat_function(fun = sin, size = 0, n = 225) + 
  theme_bw() +
  lims(y = c(-2, 2)) + 
  labs(x = "X", y = "Y")
# curve(sin, 5, 9, ylim = c(-2, 2), ylab = "Y", xlab = "X", lwd = 3, type = "n")
yhn <- matrix(NA, Ntrains, dpt)
MSEtest <- numeric(Ntrains)
MSEtrain <- numeric(Ntrains)
for(i in 1:Ntrains){
  xs <- sort(runif(n, 5, 9))
  ys <- sin(xs) + rnorm(n, 0, SD)
  mod1 <- lm(ys ~ poly(xs, 10))
  ysn <- predict(mod1)
  NDF <- data.frame(xs, ysn)
  # lines(xs, nys, col = "pink", lty = "dashed")
  p <- p + geom_line(data = NDF, aes(x = xs, y = ysn), size = 0.1, color = "pink")
  nxs <- seq(5, 9, length = dpt)
  yhn[i, ] <- predict(mod1, newdata = data.frame(xs = nxs))
  yst <- sin(xs) + rnorm(n, 0, SD)
  MSEtest[i] <- mean((yst - ysn)^2)
  MSEtrain[i] <- mean((ys - ysn)^2)
}
p
yhnbar <- apply(yhn, 2, mean)
# curve(sin, 5, 9, ylim = c(-3, 3), ylab = "Y", xlab = "X", lwd = 3, add = TRUE)
p <- p + stat_function(fun = sin, size = 1, n = 225, color = "blue")
# lines(nxs, yhnbar, col = "red", lwd = 2)
NDF2 <- data.frame(nxs, yhnbar)
p + geom_line(data = NDF2, aes(x = nxs, y = yhnbar), size = 1, color = "red")
avgMSEtest <- mean(MSEtest)
avgMSEtrain <- mean(MSEtrain)
c(avgMSEtest, avgMSEtrain)
