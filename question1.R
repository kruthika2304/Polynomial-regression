set.seed(0)
cars1 <- read.csv("C:/Users/hp/Downloads/cars.csv")
rand = sample(1:nrow(cars1),350)
train = cars1[rand, ]
test = cars1[-rand, ]
summary(test)
summary(cars1)


m7 <- lm(MPG~Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)+ I(Weight^5) + I(Weight^6) 
         + I(Weight^6) + I(Weight^7), train)
m7

#PLOTTING THE MODEL OVER THE DATA
plot(train$Weight,train$MPG, pch=19, cex=0.5)
lines(sort(train$Weight), fitted(m7)[order(train$Weight)], col='brown', type='l',pch=20) 

#TRAIN AND TEST ACCURACY
sum(m7$residuals^2)
pred = predict(m7, newdata=test)
sum((pred-test$MPG)^2)

#Repeat this for different smaple sizes

set.seed(1)
cars1 <- read.csv("C:/Users/hp/Downloads/cars.csv")
rand1 = sample(1:nrow(cars1),20)
train = cars1[rand1, ]
test = cars1[-rand1, ]


m1 <- lm(mpg ~ wt, train)
m1

#PLOTTING THE MODEL OVER THE DATA
plot(train$wt,train$mpg, pch=19, cex=0.5)
lines(sort(train$wt), fitted(m1)[order(train$wt)], col='red', type='l') 

#TRAIN AND TEST ACCURACY
sum(m1$residuals^2)
pred = predict(m1, newdata=test)
sum((pred-test$mpg)^2)


set.seed(3)
cars1 <- read.csv("C:/Users/hp/Downloads/cars.csv")
rand3 = sample(1:nrow(cars1),20)
train = cars1[rand3, ]
test = cars1[-rand3, ]

set.seed(4)
cars1 <- read.csv("C:/Users/hp/Downloads/cars.csv")
rand4 = sample(1:nrow(cars1),20)
train = cars1[rand4, ]
test = cars1[-rand4, ]




