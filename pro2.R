#install.packages(c("readxl", "ggplot2", "dplyr","ggpubr","tidyverse","broom","AICcmodavg"))


library(readxl)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)
Dataset <- read_excel("D:/Book1.xlsx",n_max=420)
print(Dataset)
summary(Dataset)
year <- Dataset$`Release Year`
budget <- Dataset$`Budget in Crores`
rating <- Dataset$Rating
collection <- Dataset$`Box Office Collection in crores`
plot(Dataset$`Release Year`,Dataset$`Box Office Collection in crores`,type='l')
#print(rating)
hist(rating)
mean(collection)
average1 <- with(Dataset,mean(collection[year>=2011 & year < 2012]) )
#print(average1)
average2 <- with(Dataset,mean(collection[year>=2012 & year < 2013]) )
#print(average2)
average3 <- with(Dataset,mean(collection[year>=2013 & year < 2014]) )
#print(average3)
average4 <- with(Dataset,mean(collection[year>=2014 & year < 2015]) )
#print(average4)
average5 <- with(Dataset,mean(collection[year>=2015 & year < 2016]) )
#print(average5)
average6 <- with(Dataset,mean(collection[year>=2016 & year < 2017]) )
#print(average6)
average7 <- with(Dataset,mean(collection[year>=2017 & year < 2018]) )
#2021print(average7)
Year_data <- c(2011,2012,2013,2014,2015,2016,2017)
Collection_data <- c(21.25893,27.70213,26.92857,24.4625,46.48529,34.16901,58.51515)
time_series <- ts(Collection_data)
plot.ts(time_series)
result <- data.frame(Year_data,Collection_data)
print(result)
f = Collection_data/length(Collection_data)
alpha = 2/(length(Collection_data)+1)
F_2017 = sum(Collection_data)/7
cat("Forecast of the year 2017 : ",F_2017)
Forecast = ((alpha)*58.51515)+((1-alpha)*F_2017)
cat("   Forecast for the year 2018 : ",Forecast)
x = c(2011,2012,2013,2014,2015,2016,2017)
y = c(21.25893,27.70213,26.92857,24.4625,46.48529,34.16901,58.51515)
dell_y=c(0)

dells_y=c(0,0)

len=0
for (i in lengths(y)){
  len=len+1
}
for (i in 2:len){
  temp=y[i]-y[i-1]
  
  dell_y=append(dell_y,temp)
}
for (i in 3:len){
  temp=dell_y[i]-dell_y[i-1]
  
  dells_y=append(dells_y,temp)
}

dell_y_by_y=dell_y/ y
print(y)
print(dell_y)
print(dells_y)
print(dell_y_by_y)
co_dell_y=mean(abs(mean(dell_y)-dell_y))
co_dells_y=mean(abs(mean(dells_y)-dells_y))
co_dell_y_by_y=mean(abs(mean(dell_y_by_y)-dell_y_by_y))
print(co_dell_y)
print(co_dells_y)
print(co_dell_y_by_y)
constant=min(c(co_dell_y,co_dells_y,co_dell_y_by_y))
print(constant)
med <- median(x)
cat("Med : ",med)
x = Year_data
print(x)
t=c()
Y=c()
z=readline(prompt="Enter the vaue of year to be calculated : ")
z=as.integer(z)
if(constant == co_dell_y_by_y){
  tsq=c()
  tY=c()
  for(i in 1:7){
    diff=x[i]-med
    t=append(t,diff)
    i=i+1
  }
  for(i in 1:7){
    ca1 = log(y[i])
    Y=append(Y,ca1)
    i=i+1
  }
  for(i in length(x)){
    ca2 = t*t
    tsq=append(tsq,ca2)
    i=i+1
  }
  for(i in length(x)){
    ca3 = t*Y
    tY=append(tY,ca3)
    i=i+1
  }
  result1 <- data.frame(x,t,y,Y,tsq,tY)
  print(result1)
  summary(result1)
  A=sum(Y)/length(x)
  b=sum(tY)/sum(tsq)
  print(b)
  a = exp(A)
  print(a)
  cat("THE EXPONENTIAL FORM OF THE EQUATION :",a,"*", "e ^",b,"(x-2014) ")
  c=a*exp(b*(z-2014))
  cat("  \n The trend value for the year ",z,"is:" ,c)
}