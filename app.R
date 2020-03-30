#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(lubridate)
library(tidyverse)
library(gdata)

covid <- read.csv('https://covid.ourworldindata.org/data/ecdc/full_data.csv',header=T,sep=",",fill=T)
covid <- filter(covid,date != "2019-12-31") 
covid <- filter(covid,location!="World")
covid<- mutate(covid,week=week(date))
levs <- levels(covid$location) #<- droplevels(covid$location)
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

full$Country <- full$location



# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("COVID-19 Exponential Growth"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
      	checkboxGroupInput("checkGroup", label = h3("Country"), 
      					   choices = (levs),
      					   selected = c("Canada","China","Italy"))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("Plot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
	
   #can <- renderPrint({ input$checkGroup })
   
   #can <- reactive({filter(covid,location==input$checkGroup)})
   
   output$Plot <- renderPlot({
      can <- filter(full,Country %in% input$checkGroup)
      ggplot(full) + geom_line(aes(tot_cases,new_cases,group=Country,alpha=0.1)) +
      	#scale_linetype(guide="none")+
      			#axis.text.x = element_text(angle=90, hjust=1)) +
      	scale_alpha(guide = 'none') +
      	geom_line(data=can,aes(tot_cases,new_cases,color=Country),size=2) +
      	theme_classic() +
      	scale_x_log10() + 
      	scale_y_log10() +
      	#guide_legend(title.l)
      	xlab("Total # of Cases") + ylab("# of New Cases/day") +
      	ggtitle("Exponential Growth Rate of COVID-19 by Country") +
      	theme(axis.text.x = element_text(size = 17),
      		  axis.text.y = element_text(size = 17),  
      		  axis.title.x = element_text(size = 20),
      		  axis.title.y = element_text(size = 20),
      		  title = element_text(size=18))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

