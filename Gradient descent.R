regressionData = read.csv("regression.csv", header =T, sep =",")
View(regressionData)

#Build a linear model
model = lm(y~x, data = regressionData)
summary(model)

#Visualize the model
attach(regressionData)
plot(x,y,col=rgb(0.2,0.4,0.6,0.4), main="linear regression")
abline(model, col="blue")

#Cost func
cost = function(X, y, theta) {
  sum(X%*%theta)
}

#Initialization

theta = matrix(c(0,0), nrow =2)
num_iterations = 2500
alpha = 0.01  # learning rate

cost_history = double(num_iterations)
theta_history = list(num_iterations)
delta_history = list(num_iterations)

##y = x0m0 + x1m1 where x0 always equals 1

X = cbind(1, matrix(x))

#Gradient descent calculations below
for (i in 1:num_iterations) {
  error = (X%*%theta - y)
  delta = t(X) %*% error/length(y) #t(x) is the transpose of the X matrix # delta is the slope
  theta = theta - alpha*delta
  cost_history[i] = cost(X, y, theta)
  theta_history[[i]] = theta
  delta_history[[i]] = delta
}

print(theta)

plot()




## print out first 100 theta
for(i in 1:100) {
  print(theta_history[[i]])
}

## print out first 2500 theta
for(i in 1:2500) {
  print(theta_history[[i]])
}

#Build a linear model
model = lm(y~x, data = regressionData)
summary(model)

for(i in 1:500) {
  print(delta_history[[i]])
}

##delta should be close "enough" to zero
for(i in 1:2500) {
  print(delta_history[[i]])
}

## print out the slopes m0 and m1
library(ggplot2)
ggplot(delta_history, aes(x=i, y=delta_history[[i]])) + geom_point()
View(delta_history)
df = data.frame(matrix(unlist(delta_history), nrow=length(delta_history), byrow='T'))
df$i = seq.int(nrow(df))
View(df)
ggplot(df, aes(x=i, y=X1)) + geom_point() + ylim(0,2.5)
ggplot(df, aes(x=i, y=X2)) + geom_point() + ylim(-0.2,0)

}

