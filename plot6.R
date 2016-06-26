#Compare emissions from motor vehicle sources in Baltimore City with emissions 
#from motor vehicle sources in Los Angeles County, California (fips == "06037"). 
#Which city has seen greater changes over time in motor vehicle emissions?

library(dplyr)
library(ggplot2)
library(reshape2)
library(cowplot)

#Preprocessing steps
setwd("C:/Users/ahaldar/Desktop/Coursera")

#Reading in data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Checking to see if there are any NA value in NEI
sum(is.na(NEI)) #0: so, no missing values
NEI <- rename(NEI, Year = year, Type = type) #New name = Old name


####Exploring SCC to see where we can find motor vehicle sources information.
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

fdata_3<-factor(SCC$SCC.Level.Three)
table(fdata_3)
#Motor Vehicles: SIC 371

#We assume that motor vehicle related SCC records are those 
#where SCC.Level.TWo contains the string 'vehicle'.
Vehicle.Logical <- grepl("vehicle", SCC$SCC.Level.Two, ignore.case=TRUE)
Vehicle.SCC <- SCC[Vehicle.Logical,]$SCC
Vehicle.NEI <- NEI[NEI$SCC %in% Vehicle.SCC,]


###Subset NEI

#Baltimore City (fips == "24510")
Baltimore.Veh.NEI <- Vehicle.NEI[Vehicle.NEI$fips==24510,]

#Los Angeles County, California (fips == "06037")
LA.Veh.NEI <- Vehicle.NEI[Vehicle.NEI$fips=="06037",]
Both.Veh.NEI <- rbind(Baltimore.Veh.NEI,LA.Veh.NEI)

#Rename "fips" to city
Both.Veh.NEI <- rename(Both.Veh.NEI, City=fips) #New name = Old name

# Summarize
Both.Veh.Emission <- melt(Both.Veh.NEI, id=c("Year", "City"), measure.vars=c("Emissions"))

#Reshape data summing emissions by city (i.e. City) then Year
Both.Veh.Emission <- dcast(Both.Veh.Emission, City + Year ~ variable, sum)

#Calculating change
Both.Veh.Emission[2:8,"Change"] <- diff(Both.Veh.Emission$Emissions)
Both.Veh.Emission[c(1,5),4] <- 0

#####Plot
#g1 <- ggplot(data=Both.Veh.Emission, aes(x=Year, y=Emissions,color=City))

g1 <- ggplot(data=Both.Veh.Emission,
            aes(x=Year, y=Emissions,
                color=factor(City,labels=c("Los Angeles", "Baltimore"))
            ))+
        labs(color = "City")

g2 <- ggplot(data=Both.Veh.Emission,
            aes(x=Year, y=Change,
                color=factor(City,labels=c("Los Angeles", "Baltimore"))
            ))+
        labs(color = "City")

ggp1 <-  g1+
        geom_line(linetype = 2) +
        theme_bw()+
        geom_point(size=4, shape=15) +
        labs(x="Year", y=expression("Emissions,PM"[2.5]*" (tons)")) +
        labs(title=expression("Vehicle Emissions, Baltimore City and LA: 1999-2008"))
ggp2 <-  g2+
        geom_line(linetype = 2) +
        theme_bw()+
        geom_point(size=4, shape=15) +
        labs(x="Year", y=expression("Change in Emissions,PM"[2.5]*" (tons)")) +
        labs(title=expression("Change in Vehicle Emissions, Baltimore City and LA: 1999-2008"))

plot_grid(ggp1, ggp2, labels=c("(1)", "(2)"),ncol = 1, nrow = 2)

#Export plot as png file
dev.copy(png, file="plot6.png", width=720, height=960)
dev.off()