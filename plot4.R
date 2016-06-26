#Across the United States, how have emissions from coal combustion-related sources changed 
#from 1999-2008?

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


###Choosing filtering condition to select emissions from coal combustion-related sources

#After looking into the data, I found that EI.Sector is the most relevant to find
#coal combustion data.There are the following three categories mentioning coal:
# Fuel Comb - Comm/Institutional - Coal #31
# Fuel Comb - Electric Generation - Coal 
# Fuel Comb - Industrial Boilers, ICEs - Coal

# fdata<-factor(SCC$EI.Sector)
# table(fdata)

####For coal combustion, I chose the "Fuel Comb - Comm/Institutional - Coal"####

## I didn't chose the Short.Name containing "coal" as the condition as there
##lots of cases where "coal" is present in string, however, it does not represent combustion
##Example: Short.Name = Coal Mining, Cleaning &Material Handling has nothing to do with coal combustion

#We assume that coal combustion related SCC records are those 
#where EI.Sector contains the string 'Fuel Comb - Comm/Institutional - Coal'
#Subset SCC, keeping on two columns: SCC and Short.Name and EI.Sector

df <- subset(SCC, select = c("SCC", "Short.Name","EI.Sector"))
Coal.Logical <- grepl("Fuel Comb - Comm/Institutional - Coal", 
                      df$EI.Sector, ignore.case=TRUE)          #Logical vector

#The next two lines are to check that the appropriate rows are selected
sum(Coal.Logical) #31
df2<-df[Coal.Logical,]

#Using Coal.Logical to select the NEI rows which correspond to coal combustion processes
Coal.SCC <- SCC[Coal.Logical,]$SCC                             #Selecting all the coal rows
Coal.NEI <- NEI[NEI$SCC %in% Coal.SCC,]                        #Selecting corr. NEI rows

# Summarize
US.Coal.Emission <- aggregate(Emissions ~ Year, Coal.NEI, sum)

#Plot
g <- ggplot(data=US.Coal.Emission, aes(x=Year, y=Emissions/1000)) 
ggp <-  g+
        geom_line(linetype = 2) +
        theme_bw()+
        geom_point(size=4, shape=16) +
        labs(x="Year", y=expression("Emissions, PM"[2.5]*" (thousands of tons)")) +
        labs(title=expression("Coal Emissions,US 1999-2008"))
print(ggp)

#Export plot as png file
dev.copy(png, file="plot4.png", width=720, height=480)
dev.off()