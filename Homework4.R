setwd("C:/Users/CCSLAB1/Desktop/Exploratoy-Analysis")

#Read the data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Sampling the data for testing codes
sample <- NEI[sample(nrow(NEI),200),]

#1. Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
#Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.

#1-1. Sum emissions by year & change measures into kilotons
total <- tapply(NEI$Emissions, NEI$year, sum)
total <- total/1000
barplot(total)

#1-2. Save to png
png(filename="plot1.png")
barplot(total)
dev.off()


#2. Have total emissions from PM2.5 decreased in the Baltimore City, Maryland ( fips == "24510" ) from 1999 to 2008? 
#Use the base plotting system to make a plot answering this question.

#2-1. Subset Baltimore City data
bol_NEI<-NEI[NEI$fips=='24510',]
bol_total <- tapply(bol_NEI$Emissions, bol_NEI$year, sum)
barplot(bol_total)

#2-2. Save to png
png(filename="plot2.png")
barplot(bol_total)

#3. Of the four types of sources indicated by the type variable, 
#which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? 
#Which have seen increases in emissions from 1999–2008? 
#Use the ggplot2 plotting system to make a plot answer this question.

#3-1. 
type_total <- with(bol_NEI, aggregate(Emissions~year+type, FUN=sum))
par(mfrow=c(2,2))
with(type_total, tapply(Emissions, type, barplot))

#3-2. GGPLOT!
ggp <- ggplot(type_total, aes(factor(year), Emissions, fill=type)) +
  geom_bar(stat="identity") + #identity; 숫자를 그대로 내보냄, count=x의 각 factor의 갯수를 새줌
  facet_grid(.~type)
print(ggp)

png(filename='plot3.png')
print(ggp)

#4. Across the United States, how have emissions from coal combustion-related sources changed from 1999 to 2008?
#http://stat545.com/block022_regular-expression.html
#4-1. Find fuel comb at EI. sector
is.combustion.coal <- grepl("Fuel Comb.*Coal", SCC$EI.Sector)
coal.source <- SCC[is.combustion.coal,1]
source.matching.subset <- NEI[NEI$SCC %in% coal.source,]

#4-2. plot
library(ggplot2)
ggp <- ggplot(source.matching.subset, aes(factor(year), Emissions)) +
  geom_bar(stat="identity")
print(ggp)


#5 How have emissions from motor vehicle sources changed from 1999 to 2008 in Baltimore City?
is.motor <- grepl("", SCC$EI.Sector)
bol_motor_NEI<-NEI[NEI$fips=='24510' & ,]



#6Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (
fips == "06037"
). Which city has seen greater changes over time in motor vehicle emissions?