setwd("P:\\Spring 2020\\ML\\Classes\\Class 4")
regressionData <-read.csv("regression.csv", header = T, sep =",")
View(regressionData)
x = regressionData$x
y = regressionData$y


#Build a linear model
model <- lm(y~x, data= regressionData)
summary(model)

# visualize the model
plot(x,y,col=rgb(0.2,0.4,0.6,0.4), main="linear regression")
abline(model, col="red")

#Cost function
cost <- function(X, y, theta) {sum(X%*%theta - y)^2/(2*length(y))}

# Initialization

theta <- matrix(c(0,0), nrow=2)
num_iterations <- 2500
alpha <- 0.01

cost_history <- double(num_iterations)
theta_history <- list(num_iterations)
delta_history <- list(num_iterations)

##y = x0m0 +x1m1 where x0 always equals 1
X <- cbind(1, matrix(x))

X
theta
y

for (i in 1:num_iterations) {
  error <- (X %*% theta - y)
  delta <- t(X) %*% error/length(y)
  theta <- theta - alpha*delta
  cost_history[i] <- cost(X, y, theta)
  theta_history[[i]] <- theta
  delta_history[[i]] <- delta
}

print(theta)

plot(x, y, main="Gradient descent")
abline(coef=theta_history[[1]])
abline(coef=theta_history[[2]])
abline(coef=theta_history[[3]])
abline(coef=theta_history[[4]])
abline(coef=theta_history[[5]])

for (i in c(1,2,3,4,5, seq(6, num_iterations, by=10))) {
  abline(coef = theta_history[[i]], col=rgb(0.8,0,0,0.3))
}

##for (i in 1:num_iterations) {
#  abline(coef = theta_history[[i]], col=rgb(0.8,0,0,0.3))
#}

## print out first 100 theta
#for (i in 1:100) {
#  print(theta_history[[i]])
#}

## print out first 100 theta
#for (i in 1:2500) {
#  print(theta_history[[i]])
#}

#Build a linear model
model <- lm(y~x, data= regressionData)
summary(model)


#for (i in 1:500) {
#  print(delta_history[[i]])
#}

## delta should be close "enough" to zero
#for (i in 1:2500) {
#  print(delta_history[[i]])
#} 



## print out the slopes of m0 and m1
library(ggplot2)
ggplot(delta_history, aes(x=i, y=delta_history[[i]])) + geom_point()
View(delta_history)
View(theta_history)
df <- data.frame(matrix(unlist(delta_history), nrow=length(delta_history), byrow=T))
df$i <- seq.int(nrow(df))
View(df)
ggplot(df, aes(x=i, y=X1)) + geom_point() +ylim(0,2.5)
ggplot(df, aes(x=i, y=X2)) + geom_point() +ylim(-0.2,0)
