#Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") 
#from 1999 to 2008? Use the base plotting system to make a plot answering this question.

#Preprocessing steps
setwd("C:/Users/ahaldar/Desktop/Coursera")
library(dplyr)

#Reading in data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Checking to see if there are any NA value
sum(is.na(NEI)) #0: so, no missing values
NEI <- rename(NEI, Year = year, Type = type) #New name = Old name

#Subset Baltimore City, Maryland (fips == "24510")
#Only keeping relevant columns: "Emissions", "Year","Type"
Bal.Total.Emi <- subset(NEI, NEI$fips == "24510", c("Emissions", "Year","Type"))

#Calculating Total Emissions for each year
aggregate.data<- aggregate(Emissions ~ Year, Bal.Total.Emi, sum)

#Plot
par(mar = c(5, 5, 4, 2))
plot(aggregate.data$Year,aggregate.data$Emissions/1000,
     pch=18,cex=2,col="red",
     main="Emissions, Baltimore City 1999-2008",
     xlab="Year",
     ylab=expression("Emissions, PM" [2.5]*" (thousands of tons)"),
     xaxt="n")
axis(side=1, at=c("1999", "2002", "2005", "2008"))

#Export plot as png file
dev.copy(png, file="plot2.png", width=720, height=480)
dev.off()
