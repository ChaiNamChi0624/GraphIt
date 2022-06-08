#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# packages to plot graph
library(ggplot2)
library(plotly)

# packages to design ggplot graph
library(ggthemes)
library(tidyverse)

# package to plot correlation graph
library(corrplot)
library(ggcorrplot)

# package to plot Rtsne
library(Rtsne)

# packages to rearrange the layout
library(gridExtra)
library(grid)
library(reshape2)

# import dataset
cancerDataset <- read_csv("data.csv")

# copy of dataset
sampleCancer <- cancerDataset

# get number of rows
rows <- sample(nrow(sampleCancer))

# sample the dataset with 'rows' number of rows
sampleCancer <- sampleCancer[rows, ]

# add index to the sample data set
sampleCancer$Index = as.numeric(row.names(sampleCancer))

# Output
shinyServer(function(input, output) {
  
  
  # manipulate data with live input
  dataSample <- reactive({
    #req(input$x)
    #sampleCancer[names(sampleCancer) %in% input$x, input$sampleSize]
    
    # sample the dataset with sample size input
    sampleCancer[sample(nrow(sampleCancer),input$sampleSize),]
  })
  
  # Function to plot graph based on selected graph from input
  output$plot <- renderPlotly({
    
    # Assign inputs of axes variables to local variables
    xInput <- input$x
    yInput <- input$y
    
    # Plot scatter plot if input of graph is scatter plot
    if(input$graph %in% "Scatter Plot"){
      
      # plot by using the chosen size of sample data, input for x-axes, y-axes and color based on Diagnosis
      p <- ggplot(data = dataSample(), aes_string(x = xInput , y = yInput, col = "Diagnosis")) + 
        # add points
        geom_point() +
        
        # add regression line
        geom_smooth() +
        
        # add title
        labs(title = paste(xInput, "and", yInput),
             x = xInput, y = yInput) +
        theme_gdocs()
      
      # plot histogram if input of graph is Histogram
    }else if(input$graph %in% "Histogram"){
      
      # cannot plot categorical data in histogram
      if(xInput %in% "Diagnosis"){
        stop("Cannot plot histogram for Diagnosis. Please choose other parameter.")
      }
      
      # add sample data and x-axes to ggplot
      p <- ggplot(dataSample(), aes_string(x = xInput)) +
        
        # add histogram
        geom_histogram(fill = "#69b3a2") +
        
        # add graph title
        ggtitle(paste("Histogram of ", xInput))
      
      # Plot bar graph if the input of graph is Bar Graph
    }else if(input$graph %in% "Bar Graph"){
      
      # add sample data, x-axes to ggplot
      # coloring based on groups of Diagnosis
      ggplot(dataSample(), aes_string(x = xInput, fill = "Diagnosis")) +
        
        # add bar
        geom_bar(stat = "count", position = "stack", show.legend = TRUE) +
        
        # add title to graph
        ggtitle(paste("Bar Graph of ", xInput))
      
      # plot boxplot if input is Box Plot
    }else if(input$graph %in% "Box Plot"){
      
      # transform dataset into value form
      cancerDataset1 <- melt(dataSample()[2:32], id = "Diagnosis")
      
      # add reformed dataset, x-axes from input, value from reformed dataset to ggplot
      # coloring based on groups of Diagnosis
      ggplot(cancerDataset1, aes(x = xInput, y = value, color = Diagnosis)) +
        #geom_jitter(aes(color = Diagnosis)) +
        
        # add bosplot
        geom_boxplot() +
        
        # add labels and title to graph
        labs(x = NULL, y = "Standardized Value", title = paste("Box Plot of", xInput))
    }
  })
  
  
  # plot correlationships
  output$corr <- renderPlot({
    
    # modify non-numeric data into numeric
    # 0: Malignant 1:Benign
    cancerDataset$Diagnosis <- factor(cancerDataset$Diagnosis, levels = c("M", "B"), labels = c(0, 1))
    cancerDataset$Diagnosis <- as.numeric(as.character(cancerDataset$Diagnosis))
    
    #options(repr.plot.width = 20, repr.plot.height = 20, repr.plot.res = 100)
    
    #cancerDataset <- cancerDataset %>% relocate(Diagnosis, .after = fractal_dimension_worst)
    
    # get correlation score for data in dataset
    r <- cor(cancerDataset)
    
    # round off decimal points to 2 dp
    round(r, 2)
    
    #ggcorrplot(r, 
    # method = "circle",
    #title = "Correlation between Variables",
    #colors = c("#6D9EC1", "white", "#E46726"),
    #outline.col = "white",
    #ggtheme = ggplot2::theme_light,
    #hc.order = TRUE,
    #lab = FALSE,
    #size = 5,
    #type = "lower")
    
    #ggcorr(cancerDataset[, 2:32], 
    #method = c("everything", "pearson"),
    #geom = "text",
    #nbreak = 5,
    #palette = "RdYlBu",
    #size = 5,
    #hjust = 1,
    #layout.exp = 1,
    #label = TRUE,
    #label_alpha = 0.5)
    
    # define the grid: 1 row 3 columns
    par(mfrow = c(1,3))
    
    # plot graph for columns 2 to 11
    p1 <- corrplot(cor(cancerDataset[, c(2:12)]),
                   method = "circle",
                   order = "hclust",
                   type = "lower",
                   diag = FALSE,
                   tl.col = "black",
                   addCoef.col = "pink",
                   number.cex = 0.9,
                   bg = "white",
                   title = "Correlation between Variables",
                   mar = c(0, 0, 5, 0)
    )
    
    # plot graph for columns 12 to 21
    p2 <- corrplot(cor(cancerDataset[, c(12:22, 2)]),
                   method = "circle",
                   order = "hclust",
                   type = "lower",
                   diag = FALSE,
                   tl.col = "black",
                   addCoef.col = "pink",
                   number.cex = 0.9,
                   bg = "white",
                   title = "Correlation between Variables",
                   mar = c(0, 0, 5, 0))
    
    # plot graph for columns 22 to 31
    p3 <- corrplot(cor(cancerDataset[, c(22:32, 2)]),
                   method = "circle",
                   order = "hclust",
                   type = "lower",
                   diag = FALSE,
                   tl.col = "black",
                   addCoef.col = "pink",
                   number.cex = 0.9,
                   bg = "white",
                   title = "Correlation between Variables",
                   mar = c(0, 0, 5, 0))
    
    
  })
  
  ---------------------------------------------------------------------
    ##### Backup Code #####
  ---------------------------------------------------------------------
    output$scatter <- renderPlotly({
      
      xInput <- input$x
      yInput <- input$y
      
      
      p <- ggplot(data = dataSample(), aes_string(x = xInput , y = yInput, col = "Diagnosis")) + 
        geom_point() +
        geom_smooth() +
        #geom_line(size = 0.5) +
        #geom_hline(aes(yintercept = as.numeric(x)), linetype = 'dashed', color = 'red')+
        #geom_vline(aes(xintercept = as.numeric(y)), linetype = 'dashed', color = 'red')+
        labs(title = paste(xInput, "and", yInput),
             x = xInput, y = yInput)+
        theme_gdocs()
      
      ggplotly(p)
      
      print(p)
      
    })
  
  output$histogram <- renderPlotly({
    
    #sampleCancer$Diagnosis <- as.numeric(as.character(sampleCancer$Diagnosis))
    
    #corrplot(cor(sampleCancer), order = "hclust")
    
    p <- ggplot(sampleCancer, aes_string(x = input$x)) +
      geom_histogram(fill = "#69b3a2")
    
    ggplotly(p)
    
    print(p)
    
  })
  
  output$bar <- renderPlotly({
    #p <- ggplot(sampleCancer, aes_string(x = input$x, y = "Diagnosis", col = "Diagnosis")) +
    #geom_bar(stat = "identity", width = 0.5, aes_string(fill = "Diagnosis")) +
    #coord_flip()
    
    ggplot(sampleCancer, aes_string(x = input$x, fill = "Diagnosis")) +
      geom_bar(stat = "count", position = "stack", show.legend = TRUE)
  })
  
  output$lollipop <- renderPlotly({
    
    ggplot(sampleCancer, aes_string(x = input$x, y = input$y, col = "Diagnosis")) +
      geom_point() +
      geom_segment(aes_string(x = input$x, xend = input$x, y =0, yend = input$y)) +
      coord_flip()
    
    
  })
  
  
  
  
  output$boxplot <- renderPlotly({
    #p <- ggplot(cancerDataset, aes_string(x = "Diagnosis", y = "Texture_Mean", color = "Texture_Mean")) +
    #geom_boxplot() +
    #geom_jitter() +
    #ggtitle("Diagnosis based on Texture_Mean")
    
    #ggplotly(p)
    #print(p)
    
    cancerDataset1 <- melt(cancerDataset[2:32], id = "Diagnosis")
    
    ggplot(cancerDataset1, aes(x = variable, y = value, color = Diagnosis)) +
      #geom_jitter(aes(color = Diagnosis)) +
      geom_boxplot()
    
    
    
    
    
  })
  
  #output$summary <- renderText(      summary(sampleCancer))
  
})
