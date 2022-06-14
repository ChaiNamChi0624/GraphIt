#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

cancerDataset <- read.csv("data.csv")

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  
  # Application title
  titlePanel("Breast Cancer Data Visualizer & Cancer Predictor"),
  
  # Sidebar with a slider input for number of bins

   
  tabsetPanel(
               tabPanel("Documentation",
                        includeMarkdown('documentation.Rmd')),
                        
                tabPanel("Attribute Description and Data Summary",
                         includeMarkdown('description.Rmd'),
                         
                         h3('Summary of Breast Cancer Dataset'),
                         tableOutput("summary"),
                         h3('Info on Breast Cancer Dataset'),
                         verbatimTextOutput("info")),
                
                tabPanel("EDA",
                         sidebarLayout(
                           sidebarPanel(
                             
                             selectInput("x", "x-axis", names(cancerDataset)),
                             selectInput("y", "y-axis", names(cancerDataset), names(cancerDataset[[2]])),
                             sliderInput('sampleSize','Sample Size', min=1, max=nrow(cancerDataset),
                                         value=min(50,nrow(cancerDataset)),
                                         step=20,
                                         round=0),
                             
                             selectInput("graph", "Types of Graph", c("Scatter Plot", "Histogram", "Bar Graph", "Box Plot")),
                             
                             width = 2
                           ),
                           mainPanel (
                             
                             plotlyOutput(outputId = "plot"),
                             
                             hr(),
                             plotOutput("corr", width = "100%", height = "720px")
                           )
                         ))
           )
                       

    
    # Show a plot of the generated distribution
    # mainPanel(
    #   # plotlyOutput(outputId = "plot"),
    #   # plotOutput("corr", width = "100%", height = "400px"),
    #   
    #   #plotlyOutput("scatter"),
    #   #plotlyOutput("histogram"),
    #   #plotlyOutput("bar"),
    #   #plotlyOutput("lollipop"),
    #   #plotlyOutput("boxplot"),
    #   #textOutput("summary"),
    #   
    # )
  )
)
