#reading the files
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#question 1(plot 1)
#aggregating the column Emission and year
totalEmission <- aggregate(Emissions ~ year, NEI, sum)
barplot((totalEmission$Emissions)/10^6,names.arg = totalEmission$year,xlab="Year",ylab="Emission in 10^6 tons",main="Total Emissions from US ")

#question 2(plot 2)
#making new dataset from NEI where fips="24510"
NEIBal<-subset(NEI,fips=="24510")
#aggregating the column Emission and year
totalEmission <- aggregate(Emissions ~ year, NEIBal, sum)
barplot((totalEmission$Emissions)/10^6,names.arg = totalEmission$year,xlab="Year",ylab="Emission in Maryland 10^6 tons",main="Total Emissions from US ")

#question 3(plot 3)
g<-ggplot(aes(x=year,y=Emissions,fill=type),data=NEIBal)
g+geom_bar(stat="identity")+facet_grid(.~type)+labs(x="Years",y="Total PM2.5 Emissions")+ labs(title=" Baltimore City")

#question 4(plot 4)
#setting up the names of SCC
names(SCC)<-gsub("\\.","", names(SCC))
#filtering SCC with pattern="comb"
SCCcomb<-grepl(pattern="comb",SCC$SCCLevelOne,ignore.case=TRUE)
#filtering SCC with pattern="coal"
SCCCoal<-grepl(pattern = "coal", SCC$SCCLevelFour, ignore.case = TRUE)
SCCCoalCombustion<-SCC[SCCcomb & SCCCoal,]$SCC
#aggregratinf in SCCCoalCombustion
NIECoalCombustionValues<-NEI[NEI$SCC %in% SCCCoalCombustion,]
NIECoalCombustionTotalEm<-aggregate(Emissions~year, NIECoalCombustionValues, sum)
#forming plot
g<-ggplot(aes(year, Emissions/10^5), data=NIECoalCombustionTotalEm)
g+geom_bar(stat="identity",fill="grey",width=0.75) +guides(fill=FALSE) + labs(x="year",y="Total PM2.5 Emission") + labs(title="Coal Combustion")


#question 5(plot 5)
#filtering SCC with pattern="vehicle"
SCCvehicle<-grepl(pattern = "vehicle", SCC$EISector, ignore.case = TRUE)
SCCvehicleSCC<- SCC[SCCvehicle,]$SCC
#aggregratinf in SCCvehicleSSC
NEIvehicleSSC <- NEI[NEI$SCC %in% SCCvehicleSCC, ]
NEIvehicleBaltimore <- subset(NEIvehicleSSC, fips == "24510")
NIEvehicleBaltimoreTotEm<-aggregate(Emissions~year, NEIvehicleBaltimore, sum)
#forming plot
g<-ggplot(aes(year, Emissions/10^5), data=NIEvehicleBaltimoreTotEm)
g+geom_bar(stat="identity",fill="grey",width=0.75) + guides(fill=FALSE) +labs(x="Year",y="Total PM2.5 Emissions")+labs(title="Motor Vehicle Source Emissions in Baltimore")


#question 6(plot 6)
NEIvehicleBalti<-subset(NEIvehicleSSC, fips == "24510")
NEIvehicleBalti$city <- "Baltimore City"
NEIvehiclela<-subset(NEIvehicleSSC, fips == "06037")
NEIvehiclela$city <- "Los Angeles County"
NEIBothCity <- rbind(NEIvehicleBalti, NEIvehiclela)
ggplot(NEIBothCity, aes(x=year, y=Emissions, fill=city)) + geom_bar(aes(fill=year),stat="identity") + facet_grid(.~city) + guides(fill=FALSE) + theme_bw() + labs(x="Year", y="Total PM2.5 Emissions") + labs(title="PM2.5 Motor Vehicle Source Emissions in Baltimore & LA")

