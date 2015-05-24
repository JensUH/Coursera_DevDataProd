
library(ggplot2)
library(reshape2)
library(scales)
library(OIdata)
library(gridExtra)
library(RColorBrewer)

library(shiny)

shinyUI(pageWithSidebar(
     headerPanel("What If Analysis - Predicted sales the following four weeks for company X"),
     sidebarPanel(
          h3('Set the media Spending the next four weeks'),
          p("(All numbers are in 1000 EUR)"),
          #br(),
          fluidRow(
               column(4,
                      sliderInput("TV1", "TV week 1", value = 0, min = 0, max = 2000, step = 100),
                      sliderInput("TV2", "TV week 2", value = 0, min = 0, max = 2000, step = 100),
                      sliderInput("TV3", "TV week 3", value = 0, min = 0, max = 2000, step = 100),
                      sliderInput("TV4", "TV week 4", value = 0, min = 0, max = 2000, step = 100)
               ),
               column(4,
                      sliderInput("Ra1", "Radio week 1", value = 0, min = 0, max = 2000, step = 100),
                      sliderInput("Ra2", "Radio week 2", value = 0, min = 0, max = 2000, step = 100),
                      sliderInput("Ra3", "Radio week 3", value = 0, min = 0, max = 2000, step = 100),
                      sliderInput("Ra4", "Radio week 4", value = 0, min = 0, max = 2000, step = 100)
               ),
               column(4,
                      sliderInput("Da1", "Dailies week 1", value = 0, min = 0, max = 2000, step = 100),
                      sliderInput("Da2", "Dailies week 2", value = 0, min = 0, max = 2000, step = 100),
                      sliderInput("Da3", "Dailies week 3", value = 0, min = 0, max = 2000, step = 100),
                      sliderInput("Da4", "Dailies week 4", value = 0, min = 0, max = 2000, step = 100)
               )
          ),
          h3("Prediction of future sales and expenditures"),
          h4("Total marketing spend the next 4 weeks"),
          verbatimTextOutput("TotalSpend"),
          h4("Total sales predicted the next 4 weeks"),
          verbatimTextOutput("TotalSales"),
          h4("Return on Investment"),
          verbatimTextOutput("ROI"),
          h3("Explanation"),
          p("Given sales and marketing data from (fictitious) Company X, a Marketing Mix Model
            is constructed to predict how future spending on marketing in different media
            will contribute to future sales. Moving the sliders and thereby setting the
            marketing budget for the next four weeks, the company can plan how best to
            spend their marketing budget.")
     ),
     mainPanel(
          plotOutput('FutureSalesPlot', height = 750)
     )
))

