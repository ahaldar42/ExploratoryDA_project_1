#Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
#Using the base plotting system, make a plot showing the total PM2.5 emission 
#from all sources for each of the years 1999, 2002, 2005, and 2008.

#Preprocessing steps
rm(list=ls()) 
setwd("C:/Users/ahaldar/Desktop/Coursera")

#Check if the "Source_Classification_Code.rds" and "summarySCC_PM25.rds" are present
dir()

library(dplyr)

#Reading in data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Checking to see if there are any NA value
sum(is.na(NEI)) #0: so, no missing values

#Capitalize the column names
NEI <- rename(NEI, Year = year, Type = type) #New name = Old name

#Calculating Total Emissions for each year
aggregate.data<- aggregate(Emissions ~ Year, NEI, sum)

#Plot
par(mar = c(5, 5, 4, 2))
plot(aggregate.data$Year,aggregate.data$Emissions/1000,pch=19,cex=2,col="blue",
     main="Total Emissions, US 1999-2008",
     xlab="Year",
     ylab=expression("Emissions, PM" [2.5]*" (thousands of tons)"),
     xaxt="n")
axis(side=1, at=c("1999", "2002", "2005", "2008"))

#Export plot as png file
dev.copy(png, file="plot1.png", width=720, height=480)
dev.off()
