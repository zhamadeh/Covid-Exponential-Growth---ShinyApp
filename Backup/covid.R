#These are the required  packages
library(lubridate)
library(tidyverse)
library(gdata)
library(rsconnect)


#This is the datasource that updates each day
covid <- read.csv('https://covid.ourworldindata.org/data/ecdc/full_data.csv',header=T,sep=",",fill=T)

#This one day from 2019 skews the weekly statistics so I remove it here
covid <- filter(covid,date != "2019-12-31") 

#Add week value starting from January 1 
covid<- mutate(covid,week=week(date))



for (i in levels(covid$location)){
	#i=levels(covid$location)[1]
	name=i
	tmp <- filter(covid, location==i)
	#tmp <- aggregate(tmp$total_cases,by=list(tmp$week),data=tmp,mean)
	assign(name,tmp)
}


full <- data.frame("Week"=c(),"new_cases"=c(),"new_deaths"=c(),"tot_cases"=c(),"tot_deaths"=c(),"location"=c())
for (i in levels(covid$location)){
	#i=levels(covid$location)[5]
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
str(outliers)
outliers <- filter(full, location %in% c("China","Italy"))
can <- filter(covid, location %in% "Canada")
full$location <- as.factor(full$location)
outliers$location <- as.factor(outliers$location)
outliers$location <- droplevels(outliers$location)
str(full$location)
levels(outliers$location)
ggplot(full) + geom_line(aes(tot_cases,new_cases,alpha=0.1,group=location),show.legend = F) +
	scale_alpha(guide = 'none') +
	#geom_line(data=outliers,aes(tot_cases,new_cases,color=location),size=2) +
	geom_line(data=outliers,aes(tot_cases,new_cases, color=location),size=2) +
	theme_classic() +
	scale_x_log10(n.breaks=c(1,10000)) + 
	scale_y_log10() + 
	labs(legend = "Dose (mg)")+
	xlab("Total # of Cases") + ylab("# of New Cases/day") +
	ggtitle("Exponential Growth Rate of COVID-19 by Country")+
	theme(axis.text.x = element_text(size = 12),
		  axis.text.y = element_text(size = 12),  
		  axis.title.x = element_text(size = 14),
		  axis.title.y = element_text(size = 14))
	
	#theme(legend.position = "none") 
	#theme(legend.position = "bottom") +
	#theme(legend.title = element_blank()) + ggsave("/Users/zeidh/Desktop/Pictures & Videos/Screenshots/COVID_plot.png")

p + geom_line(aes(tot_cases,new_cases,alpha=0.1,color=location)) + 
	guide_legend(ncol=3)


rsconnect::setAccountInfo(name='zhamadeh',
						  token='3079071D07B0517DD361CC0792048575',
						  secret='stsc8d6t/yKC3bXnvFvjbjUDO6irRiyLmrzaFVS7')



	