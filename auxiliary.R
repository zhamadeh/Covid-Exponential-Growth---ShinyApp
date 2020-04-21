#These are the required  packages
library(lubridate)
library(tidyverse)
library(gdata)
library(rsconnect)


##### This  is our data source and all the tidying we need to do #####

#This is the datasource that updates each day
covid <- read.csv('https://covid.ourworldindata.org/data/ecdc/full_data.csv',header=T,sep=",",fill=T)
#This one day from 2019 skews the weekly statistics so I remove it here
covid <- filter(covid,date != "2019-12-31") 
#Add week value starting from January 1 
covid<- mutate(covid,week=week(date))
#Create variable for each countries data
for (i in levels(covid$location)){
	name=i
	tmp <- filter(covid, location==i)
	assign(name,tmp)
}
full <- data.frame("Week"=c(),"new_cases"=c(),"new_deaths"=c(),"tot_cases"=c(),"tot_deaths"=c(),"location"=c())

#Aggregate weekly data for each country and then put them back together in the object: full
for (i in levels(covid$location)){
	tmp <- get(i)
	if (nrow(tmp)!=0){
		tot_cases <- aggregate(tmp$total_cases,by=list(tmp$week),data=tmp,mean)[2]
		tot_deaths<- aggregate(tmp$total_deaths,by=list(tmp$week),data=tmp,mean)[2]
		new_cases<- aggregate(tmp$new_cases,by=list(tmp$week),data=tmp,mean)
		new_deaths<- aggregate(tmp$new_deaths,by=list(tmp$week),data=tmp,mean)[2]
		df <- cbind(new_cases,new_deaths,tot_cases,tot_deaths)
		names(df)<- c("Week","new_cases","new_deaths","tot_cases","tot_deaths")
		df$location=i
		full=rbind(df,full)
	}
}
#These are our two tidy datasets: covid for daily data, full for weekly data
covid$Country <- covid$location
full$Country <- full$location