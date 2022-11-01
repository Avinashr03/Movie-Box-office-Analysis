

library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)
movie.data <- read.csv("D:/one way.CSV", header = TRUE, colClasses = c("factor", "factor", "numeric","factor","factor"))
summary(movie.data)
one.way <- aov(Box_Office ~ Ratings, data = movie.data)
summary(one.way)
two.way <- aov(Box_Office ~ Ratings + Budget, data = movie.data)
summary(two.way)
interaction <- aov(Box_Office ~ Ratings*Budget, data = movie.data)
summary(interaction)
blocking <- aov(Box_Office ~ Ratings+Budget + Block, data = movie.data)
summary(blocking)
library(AICcmodavg)
model.set <- list(one.way, two.way, interaction, blocking)
model.names <- c("one.way", "two.way", "interaction", "blocking")
aictab(model.set, modnames = model.names)
moviedata <- read.csv(file = "D:/one way.CSV",
                      header = TRUE, sep = ",")
print(moviedata)
#
# Visualize
boxplot(moviedata$Box_Office ~ moviedata$order, data = moviedata,
        col="light blue", ylab = "Box_Office", xlab = "order")
#
moviedata$order <- as.factor(moviedata$order)# coerce to a factor
moviedata$Budget <- as.factor(moviedata$Budget)
#
# Perform ANOVA
table <- aov(moviedata$Box_Office ~ moviedata$order, data = moviedata)
summary(table)   # F(2, 75) = 5.551, p = 0.002
#
# Perform Tukey Post-Hoc Test
TukeyHSD(table)                # Differences found between diets 1-3, and 2-3
#
# Visualize Means and Confidence Levels
plot(TukeyHSD(table))

table1 <- aov(moviedata$Box_Office ~ moviedata$order+moviedata$Budget, data = moviedata)
summary(table1)
TukeyHSD(table1)
#Non Parametric Test
kruskal <- kruskal.test(movie.data$Box_Office ~ movie.data$order)
print(kruskal)
chi1 <- chisq.test(moviedata$Box_Office)
chi2 <- chisq.test(moviedata$Ratings)
print(chi1)
print(chi2)
