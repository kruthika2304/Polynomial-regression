set.seed(0)
cars1 <- read.csv("C:/Users/hp/Downloads/cars.csv")
rand = sample(1:nrow(cars1),20)
train = cars1[rand, ]
test = cars1[-rand, ]
m1 <- lm(MPG~Weight, train)
m1

m2 <- lm(MPG~Weight + I(Weight^2), train)
m2

m7 <- lm(MPG~Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)+ I(Weight^5) + I(Weight^6) 
         + I(Weight^6) + I(Weight^7), train)
m7

m8 <- lm(MPG~Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) +
           I(Weight^5) + I(Weight^6) + I(Weight^7) +
           I(Weight^8) , train)

m8


m9 <- lm(MPG ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) +
           I(Weight^5) + I(Weight^6) + I(Weight^7) +
           I(Weight^8) + I(Weight^9) , train)

m9

m10 <- lm(MPG ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) +
           I(Weight^5) + I(Weight^6) + I(Weight^7) +
           I(Weight^8) + I(Weight^9)+ I(Weight^10) , train)

m10

#jpeg('rplot.jpg', width = 350, height = 350)
plot(train$Weight,train$MPG, pch=19, cex=0.5)
lines(sort(train$Weight), fitted(m1)[order(train$Weight)], col='brown', type='l',pch=20) 
lines(sort(train$Weight), fitted(m2)[order(train$Weight)], col='blue', type='l',pch=20) 
lines(sort(train$Weight), fitted(m7)[order(train$Weight)], col='red', type='l',pch=20) 
lines(sort(train$Weight), fitted(m8)[order(train$Weight)], col='green', type='l',pch=20) 
lines(sort(train$Weight), fitted(m9)[order(train$Weight)], col='yellow', type='l',pch=20) 
lines(sort(train$Weight), fitted(m10)[order(train$Weight)], col='purple', type='l',pch=20) 
#dev.off()

#Setting seed to 1(sample 2)
set.seed(1)
cars1 <- read.csv("C:/Users/hp/Downloads/cars.csv")
rand = sample(1:nrow(cars1),20)
train = cars1[rand, ]
test = cars1[-rand, ]
m1 <- lm(MPG~Weight, train)
m1

m2 <- lm(MPG~Weight + I(Weight^2), train)
m2

m7 <- lm(MPG~Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)+ I(Weight^5) + I(Weight^6) 
         + I(Weight^6) + I(Weight^7), train)
m7

m8 <- lm(MPG~Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) +
           I(Weight^5) + I(Weight^6) + I(Weight^7) +
           I(Weight^8) , train)

m8


m9 <- lm(MPG ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) +
           I(Weight^5) + I(Weight^6) + I(Weight^7) +
           I(Weight^8) + I(Weight^9) , train)

m9

m10 <- lm(MPG ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) +
            I(Weight^5) + I(Weight^6) + I(Weight^7) +
            I(Weight^8) + I(Weight^9)+ I(Weight^10) , train)

m10

#jpeg('rplot.jpg', width = 350, height = 350)
plot(train$Weight,train$MPG, pch=19, cex=0.5)
lines(sort(train$Weight), fitted(m1)[order(train$Weight)], col='brown', type='l',pch=20) 
lines(sort(train$Weight), fitted(m2)[order(train$Weight)], col='blue', type='l',pch=20) 
lines(sort(train$Weight), fitted(m7)[order(train$Weight)], col='red', type='l',pch=20) 
lines(sort(train$Weight), fitted(m8)[order(train$Weight)], col='green', type='l',pch=20) 
lines(sort(train$Weight), fitted(m9)[order(train$Weight)], col='yellow', type='l',pch=20) 
lines(sort(train$Weight), fitted(m10)[order(train$Weight)], col='purple', type='l',pch=20) 
#dev.off()


#setting seed to 2--sample2

set.seed(2)
cars1 <- read.csv("C:/Users/hp/Downloads/cars.csv")
rand = sample(1:nrow(cars1),20)
train = cars1[rand, ]
test = cars1[-rand, ]
m1 <- lm(MPG~Weight, train)
m1

m2 <- lm(MPG~Weight + I(Weight^2), train)
m2

m7 <- lm(MPG~Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)+ I(Weight^5) + I(Weight^6) 
         + I(Weight^6) + I(Weight^7), train)
m7

m8 <- lm(MPG~Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) +
           I(Weight^5) + I(Weight^6) + I(Weight^7) +
           I(Weight^8) , train)

m8


m9 <- lm(MPG ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) +
           I(Weight^5) + I(Weight^6) + I(Weight^7) +
           I(Weight^8) + I(Weight^9) , train)

m9

m10 <- lm(MPG ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) +
            I(Weight^5) + I(Weight^6) + I(Weight^7) +
            I(Weight^8) + I(Weight^9)+ I(Weight^10) , train)

m10

#jpeg('rplot.jpg', width = 350, height = 350)
plot(train$Weight,train$MPG, pch=19, cex=0.5)
lines(sort(train$Weight), fitted(m1)[order(train$Weight)], col='brown', type='l',pch=20) 
lines(sort(train$Weight), fitted(m2)[order(train$Weight)], col='blue', type='l',pch=20) 
lines(sort(train$Weight), fitted(m7)[order(train$Weight)], col='red', type='l',pch=20) 
lines(sort(train$Weight), fitted(m8)[order(train$Weight)], col='green', type='l',pch=20) 
lines(sort(train$Weight), fitted(m9)[order(train$Weight)], col='yellow', type='l',pch=20) 
lines(sort(train$Weight), fitted(m10)[order(train$Weight)], col='purple', type='l',pch=20) 
#dev.off()


#Setting seed to 3--sample4


set.seed(3)
cars1 <- read.csv("C:/Users/hp/Downloads/cars.csv")
rand = sample(1:nrow(cars1),20)
train = cars1[rand, ]
test = cars1[-rand, ]
m1 <- lm(MPG~Weight, train)
m1

m2 <- lm(MPG~Weight + I(Weight^2), train)
m2

m7 <- lm(MPG~Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)+ I(Weight^5) + I(Weight^6) 
         + I(Weight^6) + I(Weight^7), train)
m7

m8 <- lm(MPG~Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) +
           I(Weight^5) + I(Weight^6) + I(Weight^7) +
           I(Weight^8) , train)

m8


m9 <- lm(MPG ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) +
           I(Weight^5) + I(Weight^6) + I(Weight^7) +
           I(Weight^8) + I(Weight^9) , train)

m9

m10 <- lm(MPG ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) +
            I(Weight^5) + I(Weight^6) + I(Weight^7) +
            I(Weight^8) + I(Weight^9)+ I(Weight^10) , train)

m10

#jpeg('rplot.jpg', width = 350, height = 350)
plot(train$Weight,train$MPG, pch=19, cex=0.5)
lines(sort(train$Weight), fitted(m1)[order(train$Weight)], col='brown', type='l',pch=20) 
lines(sort(train$Weight), fitted(m2)[order(train$Weight)], col='blue', type='l',pch=20) 
lines(sort(train$Weight), fitted(m7)[order(train$Weight)], col='red', type='l',pch=20) 
lines(sort(train$Weight), fitted(m8)[order(train$Weight)], col='green', type='l',pch=20) 
lines(sort(train$Weight), fitted(m9)[order(train$Weight)], col='yellow', type='l',pch=20) 
lines(sort(train$Weight), fitted(m10)[order(train$Weight)], col='purple', type='l',pch=20) 
#dev.off()

