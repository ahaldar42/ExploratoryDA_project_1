#How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?

library(dplyr)
library(ggplot2)
library(reshape2)

#Preprocessing steps
setwd("C:/Users/ahaldar/Desktop/Coursera")

#Reading in data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Checking to see if there are any NA value in NEI
sum(is.na(NEI)) #0: so, no missing values
NEI <- rename(NEI, Year = year, Type = type) #New name = Old name

#Exploring SCC to see where we can find motor vehicle sources information.
fdata_1<-factor(SCC$SCC.Level.One)
table(fdata_1)

fdata_2<-factor(SCC$SCC.Level.Two)
table(fdata_2)
#I found that in SCC Level.Two, there are the following specifications:
# Highway Vehicles - Diesel
# Highway Vehicles - Gasoline
# Off-highway Vehicle Gasoline, 2-Stroke
# Off-highway Vehicle Diesel
# Off-highway Vehicle Gasoline, 4-Stroke

#We assume that motor vehicle related SCC records are those 
#where SCC.Level.TWo contains the string 'vehicle'.

Vehicle.Logical <- grepl("vehicle", SCC$SCC.Level.Two, ignore.case=TRUE)
Vehicle.SCC <- SCC[Vehicle.Logical,]$SCC
Vehicle.NEI <- NEI[NEI$SCC %in% Vehicle.SCC,]

#Subset NEI
Baltimore.Veh.NEI <- Vehicle.NEI[Vehicle.NEI$fips==24510,]

# Summarize
Bal.Veh.Emission <- aggregate(Emissions ~ Year, Baltimore.Veh.NEI, sum)

#Plot
g <- ggplot(data=Bal.Veh.Emission, aes(x=Year, y=Emissions)) 
ggp <-  g+
        geom_line(linetype = 2) +
        theme_bw()+
        geom_point(size=4, shape=15, colour = "blue") +
        labs(x="Year", y=expression("Emissions,PM"[2.5]*" (tons)")) +
        labs(title=expression("Vehicle Emissions,Baltimore City 1999-2008"))
print(ggp)

#Export plot as png file
dev.copy(png, file="plot5.png", width=720, height=480)
dev.off()