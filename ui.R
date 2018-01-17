library(zoo)
library(shiny)
library(plotly)
library(DT)

shinyUI(fluidPage(
  
  list(tags$head(tags$style("body {background-color: grey; }"))), 
  
  # First Row of the Webpage for THE HEADING 
  fluidRow(
    column(width = 12, align="center",
           titlePanel(title=div("PROBABILITY MAP TECHNOLOGY for CRUDE"))
    ),
    column(width = 12, align="left",
           titlePanel(title=div(img(src="logo.png", align="right", width= "10%", height="10%"))), 
           actionButton("refresh", "Refresh"))
  ),
  
  # Second Row of the Webpage for DAILY BAR CHARTS AND DENSITY OF EACH BAR CLASS AT 9:00
  fluidRow(
    column(width=5, align="center", 
           titlePanel(" "),
           plotlyOutput("daily_price", width = "100%", height = "auto")
    ),
    column(width=7, align="center",  
           titlePanel(" "),
           plotOutput("bar_frequency", width = "100%")
    )),
  
  # Third Row of the Webpage for 15 MIN BAR AND RET TO CLOSE FOR EACH BAR CLASS
  fluidRow(
    column(width=5, align="center",
           titlePanel(" "),
           plotlyOutput("fifteenmin", width="100%", height = "auto")
    ),
    column(width=2, align="center", 
           titlePanel(" "),
           # radioButtons("radio", label = "Choose the Return Type", choices = list("Individual", "Composite"), selected="Individual"),

             selectInput("bar_class_type", "Select the bar class to be analyzed", 
                         choices = list("1", "2", "4", "5", "6", "8", "9", "10", "11", "12", "13", "14", "16", "17", "18", "20", 
                                      "21", "22", "23", "24", "25", "26","28", "29", "30", "32", "33", "34", "35","36","Composite", "Most Likely"), selected ="Most Likely"),

             sliderInput("binsize", "Select the bin width:", min=5, max=100, value = 20)
    ),
    column(width=5, align="center", 
           titlePanel(" "),
          plotOutput("expected_returns", width = "100%")
    )),
  
  # Fourth Row 
  fluidRow(
    column(width=12, align="center", 
           titlePanel(" "),
           plotOutput("projections", width = "100%")
    )),    
  
  # Fourth Row of the Webpage for PROJECTION HEADING 
  fluidRow(
    column( width=12, align="center",        
           titlePanel("PROJECTIONS TABLE")
    )),
  
  # Fifth Row of the Webpage for PROJECTION DATA
  fluidRow(
    column( width=12, align="left", 
            div(dataTableOutput("projections_table"),style="font-size:75%; width:30%")
    )), 
  
  # Sixth Row of the Webpage for DRAWDOWN HEADING 
  fluidRow(
    column( width=12, align="center",        
           titlePanel("DRAWDOWN STATISTICS FROM THE HIGH/LOW OF THE DAY")
    )),
  
  # Seventh Row of the Webpage for DRAWDOWN DATA
  fluidRow(
    column( width=12, align="center", 
           div(dataTableOutput("drawdown_table"),style="font-size:75%; width:30%")
    )),
  
  fluidRow(
    column( width=12, align="center",        
           titlePanel("LEGEND")
    )),
  
  
  fluidRow(
    column(align = "right", width=3, 
           titlePanel(title=div(img(src="1.png", align="right",width= "90%")))
    ),
    column(width=3,
           titlePanel(title=div(img(src="2.png", align="right",width= "90%")))
    ), 
    column(width=3,
           titlePanel(title=div(img(src="3.png", align="right",width= "90%")))
    ), 
    column(width=3,
           titlePanel(title=div(img(src="4.png", align="right",width= "90%")))
    ))
))
