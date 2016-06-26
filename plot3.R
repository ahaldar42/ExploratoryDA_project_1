#Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
#which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City?
#Which have seen increases in emissions from 1999-2008?

library(dplyr)
library(ggplot2)
library(reshape2)

#Preprocessing steps
setwd("C:/Users/ahaldar/Desktop/Coursera")

#Reading in data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Checking to see if there are any NA value
sum(is.na(NEI)) #0: so, no missing values
NEI <- rename(NEI, Year = year, Type = type) #New name = Old name

#Subset Baltimore City, Maryland (fips == "24510")
#Only keeping relevant columns: "Emissions", "Year","Type"
Baltimore.NEI <- subset(NEI, NEI$fips == "24510", c("Emissions", "Year","Type"))

# Melt and reshape data summing emissions by Year then Type
Baltimore.Type <- melt(Baltimore.NEI, id=c("Year", "Type"), measure.vars=c("Emissions"))
Baltimore.Type <- dcast(Baltimore.Type, Year + Type ~ variable, sum)

#Plot
g <- ggplot(data=Baltimore.Type, aes(x=Year, y=Emissions, group=Type, color=Type)) 

ggp <-  g+
        geom_line(linetype = 2) +
        theme_bw()+
        geom_point(size=4, shape=16) +
        labs(x="Year", y=expression("Emissions, PM"[2.5]*" (tons)")) +
        labs(title=expression("Emissions by Source Type, Baltimore City 1999-2008"))
print(ggp)

#Export plot as png file
dev.copy(png, file="plot3.png", width=720, height=480)
dev.off()