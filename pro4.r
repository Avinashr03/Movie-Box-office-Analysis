movies <- read.csv("D:/one way.CSV")

#Linear Regression
x <- c(movies$Block)
y <- c(movies$Box_Office)
relation <- lm(y~x)
print(relation)
#print(summary(relation))

z <- c(movies$Budget)
y <- c(movies$Box_Office)
relation1 <- lm(z~y)
print(relation1)

#print(summary(relation1))

plot(movies$Block,movies$Box_Office)
model2=lm(Box_Office~Block,data = movies)
abline(model2, col="red")

plot(movies$Budget,movies$Box_Office)
model2=lm(Box_Office~Budget,data = movies)
abline(model2, col="red")