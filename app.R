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
#covid <- filter(covid,location!="World")
covid<- mutate(covid,week=week(date))
levs <- levels(covid$location) 
#levels(covid$location) <-droplevels(covid$location)


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
covid$Country <- covid$location
full$Country <- full$location



# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel(paste0("Every Country's COVID-19 Exponential Growth as of ",Sys.Date())),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
      	radioButtons("average", label = h3("Average by:"),
      				 choices = list("Daily" ,"Weekly"), 
      				 selected = "Weekly"),
      	radioButtons("deaths", label = h3("Cases or Deaths"),
      				 choices = list("Cases","Deaths"), 
      				 selected = "Cases"),
      	radioButtons("background", label = h3("Background"),
      				 choices = list("Show Other Countries"="Show",
      				 			   "Dont Show Other Countries"="Dont"), 
      				 selected = "Show"),
      	checkboxGroupInput("checkGroup", label = h3("Country"), 
      					   choices = (levs),
      					   selected = c("Canada","World","China"))
      ),
      
      
      # Show a plot of the generated distribution
      mainPanel(
      	br(),br(),
      	print("Data surrounding the COVID-19 epidemic is so rapidly changing it makes it difficult to see the “flattening of the curve”. 
		Here, I take today’s most up to date global dataset of COVID-19 cases and deaths and plot out the number of new cases/day (growth rate) 
		against the total number of cases in a given day (averaged across the week) and visualize it on a logarithmic scale that helps to properly 
		contextualize the nature of exponential growth and how countries are mitigating spread."),
      	 hr(),
      	 plotOutput("Plot"),
         hr(),
         tableOutput("table"),
         hr(),
         print("Data acquired from https://ourworldindata.org/coronavirus-source-data")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
	
   #can <- renderPrint({ input$checkGroup })
   
   #can <- reactive({filter(covid,location==input$checkGroup)})
   
   output$Plot <- renderPlot({
   	  take <- input$deaths
      average <- input$average
      if (input$background == "Show"){
	      if (take == "Cases" & average == "Weekly"){
	          filterWeekly <- filter(full, Country %in% input$checkGroup)
	          
		      ggplot(full) + geom_line(aes(tot_cases,new_cases,group=Country,alpha=0.1)) +
		      	#scale_linetype(guide="none")+
		      			#axis.text.x = element_text(angle=90, hjust=1)) +
		      	scale_alpha(guide = 'none') +
		      	geom_line(data=filterWeekly,aes(tot_cases,new_cases,color=Country),size=2) +
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
	      }
	      else if (take == "Deaths" & average == "Weekly"){
	      	filterWeekly <- filter(full, Country %in% input$checkGroup)
	      	
	      	ggplot(full) + geom_line(aes(tot_deaths,new_deaths,group=Country,alpha=0.1)) +
	      		#scale_linetype(guide="none")+
	      		#axis.text.x = element_text(angle=90, hjust=1)) +
	      		scale_alpha(guide = 'none') +
	      		geom_line(data=filterWeekly,aes(tot_deaths,new_deaths,color=Country),size=2) +
	      		theme_classic() +
	      		scale_x_log10() + 
	      		scale_y_log10() +
	      		#guide_legend(title.l)
	      		xlab("Total # of Deaths") + ylab("# of New Deaths/day") +
	      		ggtitle("Exponential Growth Rate of COVID-19 by Country") +
	      		theme(axis.text.x = element_text(size = 17),
	      			  axis.text.y = element_text(size = 17),  
	      			  axis.title.x = element_text(size = 20),
	      			  axis.title.y = element_text(size = 20),
	      			  title = element_text(size=18))
	      }
	      else if (take == "Deaths" & average == "Daily"){
	      	filterDaily <- filter(covid,Country %in% input$checkGroup)
	      	
	      	ggplot(covid) + geom_line(aes(total_deaths,new_deaths,group=Country,alpha=0.1)) +
	      		#scale_linetype(guide="none")+
	      		#axis.text.x = element_text(angle=90, hjust=1)) +
	      		scale_alpha(guide = 'none') +
	      		geom_line(data=filterDaily,aes(total_deaths,new_deaths,color=Country),size=2) +
	      		theme_classic() +
	      		scale_x_log10() + 
	      		scale_y_log10() +
	      		#guide_legend(title.l)
	      		xlab("Total # of Deaths") + ylab("# of New Deaths/day") +
	      		ggtitle("Exponential Growth Rate of COVID-19 by Country") +
	      		theme(axis.text.x = element_text(size = 17),
	      			  axis.text.y = element_text(size = 17),  
	      			  axis.title.x = element_text(size = 20),
	      			  axis.title.y = element_text(size = 20),
	      			  title = element_text(size=18))
	      }
	      else if (take == "Cases" & average == "Daily"){
	      	filterDaily <- filter(covid,Country %in% input$checkGroup)
	      	ggplot(covid) + geom_line(aes(total_cases,new_cases,group=Country,alpha=0.1)) +
	      		#scale_linetype(guide="none")+
	      		#axis.text.x = element_text(angle=90, hjust=1)) +
	      		scale_alpha(guide = 'none') +
	      		geom_line(data=filterDaily,aes(total_cases,new_cases,color=Country),size=2) +
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
	      }
      	}
      	else if (input$background == "Dont"){
      		if (take == "Cases" & average == "Weekly"){
      			filterWeekly <- filter(full, Country %in% input$checkGroup)
      			
      			ggplot(filterWeekly) + geom_line(aes(tot_cases,new_cases,color=Country),size=2) +
      				theme_classic() +
      				scale_x_log10() + 
      				scale_y_log10() +
      				xlab("Total # of Cases") + ylab("# of New Cases/day") +
      				ggtitle("Exponential Growth Rate of COVID-19 by Country") +
      				theme(axis.text.x = element_text(size = 17),
      					  axis.text.y = element_text(size = 17),  
      					  axis.title.x = element_text(size = 20),
      					  axis.title.y = element_text(size = 20),
      					  title = element_text(size=18))
      		}
      		else if (take == "Deaths" & average == "Weekly"){
      			filterWeekly <- filter(full, Country %in% input$checkGroup)
      			
      			ggplot(filterWeekly) + geom_line(aes(tot_deaths,new_deaths,color=Country),size=2)+
      				#scale_linetype(guide="none")+
      				#axis.text.x = element_text(angle=90, hjust=1)) +
      				#scale_alpha(guide = 'none') +
      				#geom_line(data=filterWeekly,aes(tot_deaths,new_deaths,color=Country),size=2) +
      				theme_classic() +
      				scale_x_log10() + 
      				scale_y_log10() +
      				#guide_legend(title.l)
      				xlab("Total # of Deaths") + ylab("# of New Deaths/day") +
      				ggtitle("Exponential Growth Rate of COVID-19 by Country") +
      				theme(axis.text.x = element_text(size = 17),
      					  axis.text.y = element_text(size = 17),  
      					  axis.title.x = element_text(size = 20),
      					  axis.title.y = element_text(size = 20),
      					  title = element_text(size=18))
      		}
      		else if (take == "Deaths" & average == "Daily"){
      			filterDaily <- filter(covid,Country %in% input$checkGroup)
      			
      			ggplot(filterDaily) + geom_line(aes(total_deaths,new_deaths,color=Country),size=2) +
      				theme_classic() +
      				scale_x_log10() + 
      				scale_y_log10() +
      				#guide_legend(title.l)
      				xlab("Total # of Deaths") + ylab("# of New Deaths/day") +
      				ggtitle("Exponential Growth Rate of COVID-19 by Country") +
      				theme(axis.text.x = element_text(size = 17),
      					  axis.text.y = element_text(size = 17),  
      					  axis.title.x = element_text(size = 20),
      					  axis.title.y = element_text(size = 20),
      					  title = element_text(size=18))
      		}
      		else if (take == "Cases" & average == "Daily"){
      			filterDaily <- filter(covid,Country %in% input$checkGroup)
      			ggplot(filterDaily) + geom_line(aes(total_cases,new_cases,color=Country),size=2) +
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
      		}
      }
   })
   output$table <- renderTable({
   	take <- input$deaths
   	average <- input$average
   	if (average == "Daily"){
   		filterDaily <- filter(covid,Country %in% input$checkGroup)
   		can = select(filterDaily, -location)
   		colnames(can) <- c("Day of 2020", "New Cases","New Deaths","Total Cases","Total Deaths","Week","Country")
   		print(can)
   	}
   	else if (average == "Weekly"){
   		filterWeekly <- filter(full, Country %in% input$checkGroup)
   		can <- select(filterWeekly,-location)
   		colnames(can) <- c("Week of 2020", "New Cases","New Deaths","Total Cases","Total Deaths","Country")
   		print(can)
   	}
   })
}

# Run the application 
shinyApp(ui = ui, server = server)
#rsconnect::deployApp()

