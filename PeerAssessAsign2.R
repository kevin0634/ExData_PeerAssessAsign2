## Read in data
setwd("/Users/xinzhang/Documents/dataScience/Exploratory_Analysis/PeerAssessAsign2")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## Question 1

## Calculate the total PM2.5 emission from all source for each year 
## of 1999, 2002, 2005, 2008

totalEm <- aggregate(Emissions ~ year, data = NEI, FUN = "sum", na.rm=TRUE)

## Plot
png("plot1.png",width=480,height=480)
with(totalEm, plot(year, Emissions, type="o", ylab = "PM2.5 Emissions (tons)"))
dev.off()

## Question 2

## subsetting data in Baltimore City and calculate the total PM2.5 emission from 
## all sources for each year

NEIBaltimore <- NEI[NEI$fips == "24510",]
totalEmBal <- aggregate(Emissions ~ year, data = NEIBaltimore, FUN = "sum", na.rm = TRUE)

## Plot
png("plot2.png",width=480,height=480)
with(totalEmBal, plot(year, Emissions, type="o", ylab = "PM2.5 Emissions (tons)"))
dev.off()

## Question 3

## subsetting data in Baltimore City and calculate the total PM2.5 emissions from
## different types for each year.

NEIBaltimore <- NEI[NEI$fips == "24510",]
totalEmBal <- aggregate(Emissions ~ type + year, data = NEIBaltimore, FUN = "sum", na.rm = TRUE)

## Plot
library(ggplot2)
png("plot3.png",width=480,height=480)
qplot(year, Emissions, data = totalEmBal, type = "o", facets = type ~ ., geom = c("point","line"))
dev.off()

## Question 4

## find in SCC coal combustion-related sources
## I use the Short.Name field in SCC dataset and grep "Comb" and "Coal"
## Then I chose the corresponding SCC in NEI dataset

SCCCoal <- SCC[grep("Comb.*Coal|Coal.*Comb", SCC$Short.Name),]
NEICoal <- NEI[which(NEI$SCC %in% SCCCoal$SCC),]
NEICoalEm <- aggregate(Emissions ~ year, data = NEICoal, FUN = "sum", na.rm=TRUE)

## Plot
png("plot4.png",width=480,height=480)
with(NEICoalEm, plot(year, Emissions, type="o"))
dev.off()

## Question 5

## find in SCC motor vehicle-related sources
## I use the EI.Sector field in SCC dataset and grep "Mobile"
## Then I chose the corresponding SCC in NEI dataset
## After that, I subset the NEI with baltimore fips code.

SCCMV <- SCC[grep("Mobile",SCC$EI.Sector),]
NEIMV <- NEI[which(NEI$SCC %in% SCCMV$SCC),]
NEIBalMV <- NEIMV[NEIMV$fips=="24510",]
NEIBalMVEm <- aggregate(Emissions ~ year, data = NEIBalMV, FUN= "sum", na.rm=TRUE)

## Plot

png("plot5.png",width=480,height=480)
with(NEIBalMVEm, plot(year, Emissions, type="o"))
dev.off()

## Question 6

## find in SCC motor vehicle-related sources
## I use the EI.Sector field in SCC dataset and grep "Mobile"
## Then I chose the corresponding SCC in NEI dataset
## After that, I subset the NEI with baltimore and Los Angeles fips code.

SCCMV <- SCC[grep("Mobile",SCC$EI.Sector),]
NEIMV <- NEI[which(NEI$SCC %in% SCCMV$SCC),]
NEIMVBL <- NEIMV[NEIMV$fips=="24510" | NEIMV$fips == "06037",]
NEIBLMVEm <- aggregate(Emissions ~ fips + year, data = NEIMVBL, FUN= "sum", na.rm=TRUE)
city <- data.frame(fips = c("06037","24510"), county = c("Los Angeles","Baltimore"))
NEIBLMVEmFinal <- merge(NEIBLMVEm, city, by = "fips", all.x = TRUE)
## Plot
library(ggplot2)
png("plot6.png",width=480,height=480)
qplot(year, Emissions, data = NEIBLMVEmFinal, geom = c("point","line"), facets = county~.)
dev.off()
dev.off()