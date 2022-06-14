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
library(DT)
library(highcharter)

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
                         )),
               tabPanel("Model Traning",
                        tabsetPanel(
                          tabPanel("Data Preparation",
                                   h2("1. Data Preparation"),
                                   hr(),
                                   h3("Initial Selected Features"),
                                   verbatimTextOutput("initial_features"),
                                   br(),
                                   h3("Target Variable"),
                                   verbatimTextOutput("target"),
                                   br(),
                                   h3("Dimensionality Reduction with PCA"),
                                   p("Applying PCA as a preprocessing step helps to:"),
                                   tags$ul(tags$li("Reduce the number of dimensions in the dataset to speed up training."),
                                           tags$li("De-noise the data. PCA is computed by finding the components which explain the greatest amount of variance."),
                                           tags$li("Leads to better performance on the prediction model.")),
                                   
                                   div(plotOutput("plot_pca_cum_var"), align = "center"),
                                   tags$blockquote("Pick a number of dimensions to reduce down to which preserve sufficiently large portion of the variance (>= 95%), i.e., dimensions = 5"),
                                   br(),
                                   h3("After Applying PCA with Number of Dimensions = 5"),
                                   p("Number of input features turns from 10 to 5"),
                                   tabsetPanel(
                                     tabPanel("x_train", DT::dataTableOutput("x_train_pca")),
                                     tabPanel("x_test", DT::dataTableOutput("x_test_pca"))),
                                   hr(),
                                   br(), br()),
                          tabPanel("Model Selection",
                                   h2("2. Setting Up Multiple Baseline Models"),
                                   hr(),
                                   br(),
                                   h3("Training Multiple ML Models and Evaluating Performance"),
                                   fluidPage(
                                     fluidRow(
                                       column(6, 
                                              h4(HTML("<b><u>Logistic Regression</u></b>"), align = "center"),
                                              verbatimTextOutput("lr_acc"),
                                              plotOutput("lr_cm")),
                                       column(6, h4(HTML("<b><u>Naive Bayes</u></b>"), align = "center"),
                                              verbatimTextOutput("nb_acc"),
                                              plotOutput("nb_cm"))
                                     ),
                                     fluidRow(
                                       column(6, h4(HTML("<b><u>SVM</u></b>"), align = "center"),
                                              verbatimTextOutput("svm_acc"),
                                              plotOutput("svm_cm")),
                                       column(6, h4(HTML("<b><u>Random Forest</u></b>"), align = "center"),
                                              verbatimTextOutput("rf_acc"),
                                              plotOutput("rf_cm"))
                                     )
                                   ),
                                   br(),
                                   h3("Model Comparison"),
                                   p("Let's simplify the model comparison by just comparing the accuracy of each model"),
                                   div(plotOutput("model_comparison"), align = "center"),
                                   tags$blockquote("From the result above, we can say that SVM is the best model among all with accuracy = 0.965. SVM is selected to futher fine-tuning and improving."),
                                   hr(),
                                   br(), br()),
                          tabPanel("Model Fine-tuning",
                                   h2("3. Fine-tune Selected Model"),
                                   hr(),
                                   p("We are using SVM with RBF kernel,  two important hyper-parameters to set are:"),
                                   tags$ul(tags$li("cost:", textOutput("param_grid_cost")),
                                           tags$li("gamma:", textOutput("param_grid_gamma"))),
                                   p("Using grid-search, find the best combination of cost and gamma that produces the highest accuracy"),
                                   br(),
                                   fluidPage(
                                     fluidRow(
                                       column(6, 
                                              h3("Fine-tuning", align = "center"),
                                              highchartOutput("plot_grid_search")),
                                       column(6, h3("Model Evaluation", align = "center"),
                                              verbatimTextOutput("best_svm_acc"),
                                              plotOutput("best_svm_cm"))
                                     )
                                   ))
                        )),
               tabPanel("Model Prediction",
                        sidebarLayout(
                          sidebarPanel(
                            tabPanel(HTML("<center><h3><b>User Input</b></h3></center>"),
                                     uiOutput("user_input")
                                     # ,submitButton("Predict")
                                     
                                     )),
                          mainPanel(
                            h2("Prediction"),
                            p(textOutput('best_model_acc')),
                            h3("Diagnosed with Breast Cancer ?"),
                            verbatimTextOutput("predicted"),
                            verbatimTextOutput("b_or_m"),
                            htmlOutput("additional_info")
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
