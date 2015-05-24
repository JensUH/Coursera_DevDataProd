
library(ggplot2)
library(reshape2)
library(scales)
library(OIdata)
library(gridExtra)
library(RColorBrewer)

library(shiny)

options(scipen=999)

############################
#                          #
#  Code for the plot etc.  #
#                          #
############################
############################

# Setting uniform plotting theme for plotting over the time period
basic_time_theme <- theme(plot.title=element_text(size=18, vjust=1)) +
     theme(axis.title.x = element_text(size=12, vjust=0)) +
     theme(axis.text.y = element_text(size=10)) +
     theme(axis.title.y = element_text(size=12, vjust=1)) +
     theme(axis.text.x = element_text(angle=30, hjust=1, size=10))


# Creating a generic AdStock function
Adstock <<- function(media.input, decay=0){
     # Takes a vector of media input (spending) "media.input" and
     # a decay rate "decay". The function returns a vector of the
     # calculated AdStock for the period using the given decay rate.
     adstocked_advertising <- as.numeric(filter(x=1000*(1- exp(-0.005*media.input)), filter=decay, method="recursive"))
     return(adstocked_advertising)
}

# Creating data for the past 6 weeks
Past.data <- data.frame(Date=seq(Sys.Date() - 7*5, Sys.Date(), by="week"),
                        TV.Spend=c(500, 750, 1200, 0, 0, 370),
                        Radio.Spend=c(120, 245, 0, 0, 0, 0),
                        Dailies.Spend=c(45, 0, 0, 78, 54, 120),
                        Competitor.1.Spend=c(1200, 780, 450, 1100, 1348, 989),
                        Competitor.2.Spend=c(230, 0, 754, 345, 0, 654))

# Defining the model
the_model <<- function(TV, Radio, Dailies, Competitor1, Competitor2){
     return(3219 + 0.54*Adstock(TV, 0.7) + 0.61*Adstock(Radio, 0.6)
            + 0.34*Adstock(Dailies, 0.78) - 0.32*Adstock(Competitor1, 0.72)
            - 0.17*Adstock(Competitor2, 0.73))
}

# The function plotting the What If Analysis
what_if.plot <<- function(TV1, TV2, TV3, TV4, Ra1, Ra2, Ra3, Ra4, Da1, Da2, Da3, Da4){
     # This function takes the media spending for the next four weeks and provides
     # a "What If Analysis".
    
     # The media spending for the next four weeks
     Future.TV <- c(TV1, TV2, TV3, TV4)
     Future.Radio <- c(Ra1, Ra2, Ra3, Ra4)
     Future.Dailies <- c(Da1, Da2, Da3, Da4)
     
     # Other future variables
     Future.Competitor.1 <- c(0, 657, 1230, 876)
     Future.Competitor.2 <- c(447, 234, 79, 0)
     Future.Date <- seq(Past.data$Date[nrow(Past.data)]+1*7,
                        Past.data$Date[nrow(Past.data)] +4*7, by="week")
     
     # Combining past and future data
     Future.data <- data.frame(Date=c(Past.data$Date, Future.Date),
                               TV.Spend=c(Past.data$TV.Spend, Future.TV),
                               Radio.Spend=c(Past.data$Radio.Spend, Future.Radio),
                               Dailies.Spend=c(Past.data$Dailies.Spend, Future.Dailies),
                               Competitor.1.Spend=c(Past.data$Competitor.1.Spend,
                                                    Future.Competitor.1),
                               Competitor.2.Spend=c(Past.data$Competitor.2.Spend,
                                                    Future.Competitor.2))
     
     # Calculating the values to be plotted
     Total.sales <<- the_model(Future.data$TV.Spend, Future.data$Radio.Spend,
                               Future.data$Dailies.Spend, Future.data$Competitor.1.Spend,
                               Future.data$Competitor.2.Spend)
     Base.volume <- the_model(rep(0, nrow(Future.data)), rep(0, nrow(Future.data)),
                              rep(0, nrow(Future.data)), rep(0, nrow(Future.data)),
                              rep(0, nrow(Future.data)))
     TV.sales <- the_model(Future.data$TV.Spend, rep(0, nrow(Future.data)),
                           rep(0, nrow(Future.data)), rep(0, nrow(Future.data)),
                           rep(0, nrow(Future.data))) - Base.volume
     Radio.sales <- the_model(rep(0, nrow(Future.data)), Future.data$Radio.Spend,
                              rep(0, nrow(Future.data)), rep(0, nrow(Future.data)),
                              rep(0, nrow(Future.data))) - Base.volume
     Dailies.sales <- the_model(rep(0, nrow(Future.data)), rep(0, nrow(Future.data)),
                                Future.data$Dailies.Spend, rep(0, nrow(Future.data)),
                                rep(0, nrow(Future.data))) - Base.volume
     Competitor.1.sales <- the_model(rep(0, nrow(Future.data)), rep(0, nrow(Future.data)),
                                     rep(0, nrow(Future.data)), Future.data$Competitor.1.Spend,
                                     rep(0, nrow(Future.data))) - Base.volume
     Competitor.2.sales <- the_model(rep(0, nrow(Future.data)), rep(0, nrow(Future.data)),
                                     rep(0, nrow(Future.data)), rep(0, nrow(Future.data)), 
                                     Future.data$Competitor.2.Spend) - Base.volume
          
     Future.Sales.att.data <<- data.frame(Time=Future.data$Date, 
                                         Base.volume=Base.volume,
                                         TV.sales=TV.sales,
                                         Radio.sales=Radio.sales,
                                         Dailies.sales=Dailies.sales,
                                         Competitor.1.sales=Competitor.1.sales,
                                         Competitor.2.sales=Competitor.2.sales)
     
     # Creating the Sales attribution plot of the given time period as well as the future
     sales.data.long <- melt(Future.Sales.att.data, id.vars="Time",
                             variable.name="Media.type",
                             value.name="Volume")
     # Splitting into positive and negative contributions
     positive <- function(x) {max(x, 0)}
     negative <- function(x) {min(x, 0)}
     sales.data.long.pos <- sales.data.long
     sales.data.long.pos$Volume <- mapply(sales.data.long.pos$Volume, FUN=positive)
     sales.data.long.neg <- sales.data.long
     sales.data.long.neg$Volume <- mapply(sales.data.long.neg$Volume, FUN=negative)
     
     new_datebreaks <- seq(as.Date(Future.Sales.att.data$Time[1]),
                           as.Date(Future.Sales.att.data$Time[nrow(Future.Sales.att.data)]), by="week")
     
     # Calculating x values to the annotaed text and the vertical line
     forvlinex <<- as.numeric(Future.data$Date[nrow(Future.data)-4])
     forpast <- Future.data$Date[nrow(Future.data)-5]
     forfuture <- Future.data$Date[nrow(Future.data)-3]
     my_colors <- brewer.pal(6, "Accent")
     my_colors <- c(my_colors[1:5], "#000000", my_colors[6])
     #The plot
     finalePlot <- ggplot(data=Future.Sales.att.data, aes(x=Time)) +
          geom_area(data=sales.data.long.pos, aes(x=Time, y=Volume, fill= Media.type)) +
          geom_area(data=sales.data.long.neg, aes(x=Time, y=Volume, fill= Media.type)) +
          ggtitle("Sales attribution divided into different sales drivers") +
          geom_line(aes(x=Time, y=Total.sales, fill="Total sales"), size=1.2) +
          scale_x_date(breaks=new_datebreaks) + ylab("Volume (in 1000 EUR)") +
          theme(axis.text.x = element_text(angle=30, hjust=1)) +
          basic_time_theme +
          scale_fill_manual(values= my_colors) +
          geom_vline(aes(xintercept=forvlinex), colour="black", alpha=.8) +
          annotate("text", x=c(forpast, forfuture), y=c(300,300), size=8, alpha=.8, label=c("Past","Future"))
     return(finalePlot)
}

Total.Spend.Cal <<- function(TV1, TV2, TV3, TV4, Ra1, Ra2, Ra3, Ra4, Da1, Da2, Da3, Da4){
     # This function takes the media spending for the next four weeks and 
     # calculate the total spending in the next four weeks.
     return_value <- TV1+TV2+TV3+TV4+Ra1+Ra2+Ra3+Ra4+Da1+Da2+Da3+Da4
     return(return_value)
}
     
Total.Sales.Cal <<- function(TV1, TV2, TV3, TV4, Ra1, Ra2, Ra3, Ra4, Da1, Da2, Da3, Da4){
     # This function takes the media spending for the next four weeks and 
     # calculate the total predicted sales in the next four weeks.
     
     Future.TV <- c(TV1, TV2, TV3, TV4)
     Future.Radio <- c(Ra1, Ra2, Ra3, Ra4)
     Future.Dailies <- c(Da1, Da2, Da3, Da4)
     Future.Competitor.1 <- c(0, 657, 1230, 876)
     Future.Competitor.2 <- c(447, 234, 79, 0)
     Future.Date <- seq(Past.data$Date[nrow(Past.data)]+1*7,
                        Past.data$Date[nrow(Past.data)] +4*7, by="week")
     Future.data <- data.frame(Date=c(Past.data$Date, Future.Date),
                               TV.Spend=c(Past.data$TV.Spend, Future.TV),
                               Radio.Spend=c(Past.data$Radio.Spend, Future.Radio),
                               Dailies.Spend=c(Past.data$Dailies.Spend, Future.Dailies),
                               Competitor.1.Spend=c(Past.data$Competitor.1.Spend,
                                                    Future.Competitor.1),
                               Competitor.2.Spend=c(Past.data$Competitor.2.Spend,
                                                    Future.Competitor.2))
     sales.in.total <- the_model(Future.data$TV.Spend, Future.data$Radio.Spend,
                                 Future.data$Dailies.Spend, Future.data$Competitor.1.Spend,
                                 Future.data$Competitor.2.Spend)
     
     last.index <- nrow(Future.data)
     return_value <- sum(sales.in.total[(last.index-3):last.index])
     return(return_value)
}

ROI.Cal <<- function(TV1, TV2, TV3, TV4, Ra1, Ra2, Ra3, Ra4, Da1, Da2, Da3, Da4){
     # This function takes the media spending for the next four weeks and 
     # calculate the total return on investment in the next four weeks.
     
     # Calculating future total sales
     tot.sales <- Total.Sales.Cal(TV1, TV2, TV3, TV4, Ra1, Ra2, Ra3, Ra4, Da1, Da2, Da3, Da4)
     
     # Calculating future total spend
     tot.spend <- Total.Spend.Cal(TV1, TV2, TV3, TV4, Ra1, Ra2, Ra3, Ra4, Da1, Da2, Da3, Da4)
     
     #Calculating future sales without any media spending
     Future.TV <- c(0, 0, 0, 0)
     Future.Radio <- c(0, 0, 0, 0)
     Future.Dailies <- c(0, 0, 0, 0)
     Future.Competitor.1 <- c(0, 657, 1230, 876)
     Future.Competitor.2 <- c(447, 234, 79, 0)
     Future.Date <- seq(Past.data$Date[nrow(Past.data)]+1*7,
                        Past.data$Date[nrow(Past.data)] +4*7, by="week")
     Future.data <- data.frame(Date=c(Past.data$Date, Future.Date),
                               TV.Spend=c(Past.data$TV.Spend, Future.TV),
                               Radio.Spend=c(Past.data$Radio.Spend, Future.Radio),
                               Dailies.Spend=c(Past.data$Dailies.Spend, Future.Dailies),
                               Competitor.1.Spend=c(Past.data$Competitor.1.Spend,
                                                    Future.Competitor.1),
                               Competitor.2.Spend=c(Past.data$Competitor.2.Spend,
                                                    Future.Competitor.2))
     last.index <- nrow(Future.data)
     base.sales <- sum(the_model(Future.data$TV.Spend, Future.data$Radio.Spend,
                             Future.data$Dailies.Spend, Future.data$Competitor.1.Spend,
                             Future.data$Competitor.2.Spend)[(last.index-3):last.index])
     return_value <- ((tot.sales-base.sales)-tot.spend)/(tot.spend+0.0001)
     return(return_value)
}


######################
#                    #
#  The Shiny Server  #
#                    #
######################
######################



shinyServer(
     function(input, output) {
          output$FutureSalesPlot <- renderPlot({
               what_if.plot(input$TV1, input$TV2, input$TV3, input$TV4,
                       input$Ra1, input$Ra2, input$Ra3, input$Ra4,
                       input$Da1, input$Da2, input$Da3, input$Da4)
          })
          output$TotalSpend <- renderPrint({Total.Spend.Cal(input$TV1, input$TV2, input$TV3, input$TV4,
                                                            input$Ra1, input$Ra2, input$Ra3, input$Ra4,
                                                            input$Da1, input$Da2, input$Da3, input$Da4)})
          
          output$TotalSales <- renderPrint({Total.Sales.Cal(input$TV1, input$TV2, input$TV3, input$TV4,
                                                            input$Ra1, input$Ra2, input$Ra3, input$Ra4,
                                                            input$Da1, input$Da2, input$Da3, input$Da4)})
          
          output$ROI <- renderPrint({ROI.Cal(input$TV1, input$TV2, input$TV3, input$TV4,
                                             input$Ra1, input$Ra2, input$Ra3, input$Ra4,
                                             input$Da1, input$Da2, input$Da3, input$Da4)})
          
     }
)


