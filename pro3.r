
m.data <- read.csv("D:/one way.CSV")
x <- c(m.data$Block)
y <- c(m.data$Box_Office)
relation <- lm(y~x)
print(relation)
print(summary(relation))

c1 <- cor(m.data$Ratings,m.data$Budget)
c2 <- cor(m.data$Ratings,m.data$Box_Office)
cat("Correlation between Rating and Budget :\n",c1)
cat("\nCorrelation between Rating and Box Office :\n",c2)




