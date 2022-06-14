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

# package to summarise dataset
library(skimr)

# package to plot Rtsne
library(Rtsne)

# packages to rearrange the layout
library(gridExtra)
library(grid)
library(reshape2)

library(gsubfn)
library(DT)
library(dplyr)
library(highcharter)
library(caret)
source("./utils.R")

# import dataset
cancerDataset <- read_csv("./data.csv")

# copy of dataset
sampleCancer <- cancerDataset

# get number of rows
rows <- sample(nrow(sampleCancer))

# sample the dataset with 'rows' number of rows
sampleCancer <- sampleCancer[rows, ]

# add index to the sample data set
sampleCancer$Index = as.numeric(row.names(sampleCancer))

# Data preparation
seed <- 38260
list[initial_df, x_train, y_train, x_test, y_test] <- load_data("./data.csv", seed)
list[pca, standardizer, x_train, x_test] <- apply_pca(x_train, x_test, seed)

# Train models
lr_classifier <- train_model(model_name = "logisticRegression", x_train = x_train, y_train = y_train, seed = seed)
nb_classifier <- train_model(model_name = "naiveBayes", x_train = x_train, y_train = y_train, seed = seed)
svm_classifier <- train_model(model_name = "SVM", x_train = x_train, y_train = y_train, seed = seed)
rf_classifier <- train_model(model_name = "randomForest", x_train = x_train, y_train = y_train, seed = seed)

# Model prediction
y_prob <- predict(lr_classifier, type = 'response', newdata = x_test)
y_pred <- as.factor(ifelse(y_prob >= 0.5, 1, 0))
lr_cm <- confusionMatrix(y_pred, y_test)

y_pred <- predict(nb_classifier, x_test)
nb_acc <- mean(y_pred == y_test)
nb_cm <- confusionMatrix(y_pred, y_test)

y_pred <- predict(svm_classifier, x_test)
svm_cm <- confusionMatrix(y_pred, y_test)

y_pred <- predict(rf_classifier, x_test)
rf_cm <- confusionMatrix(y_pred, y_test)

model_accuracies <- data.frame(model=c("logisticRegression", "naiveBayes", "SVM", "randomForest"),
                               accuracy=c(lr_cm$overall[["Accuracy"]], nb_cm$overall[["Accuracy"]], svm_cm$overall[["Accuracy"]], rf_cm$overall[["Accuracy"]]))

# Hyperparameter tuning
list[df_tuned, best_gamma, best_cost] <- tune_svm(x_train, y_train, x_test, y_test, seed)

# Retrain SVM model with found best params
best_svm_classifier <- svm(x_train, y_train, gamma=best_gamma, cost=best_cost)
best_y_pred <- predict(best_svm_classifier, x_test)
best_svm_cm <- confusionMatrix(best_y_pred, y_test)

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
      p <- ggplot(data = dataSample(), aes_string(x = xInput , y = yInput, col = "diagnosis")) + 
        # add points
        geom_point() +
        
        # add regression line
        geom_smooth() +
        
        # add title
        labs(title = paste("Scatter Plot of ", xInput, "and", yInput),
             x = xInput, y = yInput)
      
      # plot histogram if input of graph is Histogram
    }else if(input$graph %in% "Histogram"){
      
      # cannot plot categorical data in histogram
      if(xInput %in% "diagnosis"){
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
      ggplot(dataSample(), aes_string(x = xInput, fill = "diagnosis")) +
        
        # add bar
        geom_bar(stat = "count", position = "stack", show.legend = TRUE) +
        
        # add title to graph
        ggtitle(paste("Bar Graph of ", xInput))
      
      # plot boxplot if input is Box Plot
    }else if(input$graph %in% "Box Plot"){
      # cannot plot same variable
      if(xInput %in% "diagnosis"){
        stop("Please choose other parameter.")
      }
      
      # transform dataset into value form
      
       sample = subset(dataSample(), select = c('diagnosis', xInput))
       print(sample)
       cancerDataset1 <- melt(sample, id = "diagnosis")
       print(cancerDataset1)
      
      # add reformed dataset, x-axes from input, value from reformed dataset to ggplot
      # coloring based on groups of Diagnosis
      ggplot(cancerDataset1, aes(x = xInput, y = value, color = diagnosis)) +
        geom_jitter(aes(color = diagnosis)) +
        
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
    cancerDataset$diagnosis <- factor(cancerDataset$diagnosis, levels = c("M", "B"), labels = c(0, 1))
    cancerDataset$diagnosis <- as.numeric(as.character(cancerDataset$diagnosis))
    
    #options(repr.plot.width = 20, repr.plot.height = 20, repr.plot.res = 100)
    
    #cancerDataset <- cancerDataset %>% relocate(Diagnosis, .after = fractal_dimension_worst)
    
    # get correlation score for data in dataset
    r <- cor(cancerDataset)
    
    # round off decimal points to 2 dp
    round(r, 2)
    
    ggcorrplot(r,
    method = "square",
    title = "Correlation between Variables",
    colors = c("#6D9EC1", "white", "#E46726"),
    outline.col = "white",
    ggtheme = ggplot2::theme_light,
    hc.order = TRUE,
    lab = TRUE,
    lab_size = 5,
    type = "lower")
    


   })
  
  output$summary <- renderTable({
    summary_skim <- skim(cancerDataset)
    summary_skim <- summary_skim[-c(3:9, 17)]
    return(summary_skim)
  })
  
  output$initial_features <- renderPrint(
    str(initial_df[-1])
  )
  
  output$target <- renderPrint({ 
    temp_df <- initial_df["diagnosis"]
    temp_df$diagnosis <- as.numeric(as.character(initial_df$diagnosis)) 
    return(str(temp_df))
  })
  
  output$plot_pca_cum_var <- renderPlot({
    cum_proportion <- cumsum(pca$sdev^2 / sum(pca$sdev^2))
    explained_var_df <- data.frame(explained_var = cum_proportion, n_dim = seq_along(cum_proportion))
    
    p <- ggplot(explained_var_df, aes(x = n_dim, y = explained_var)) + 
      geom_line(size = 1, color = "blue") +
      labs(y = "Explained Variance", x = "Dimensions") +
      geom_segment(aes(x = 6, y = explained_var[5] - 0.05, xend = 5 + 0.1, yend = explained_var[5] - 0.01),
                   arrow = arrow(length = unit(0.5, "cm")), size = 0.8, color = "red") +
      annotate("text", x = 6, y = explained_var_df$explained_var[5] - 0.07, label = "dimension = 5", size = 5) +
      ggtitle("Explained variance vs the number of dimensions") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold")) 
    return(p)
  }, height = 400, width = 600)
  
  output$x_train_pca <- DT::renderDataTable(
    DT::datatable(x_train,
                  options = list(pageLength = 5)
    )
  )
  output$x_test_pca <- DT::renderDataTable(
    DT::datatable(x_test,
                  options = list(pageLength = 5)
    )
  )
  
  output$lr_acc <- renderPrint({
    return(c(lr_cm$overall["Accuracy"], lr_cm$byClass["Precision"], lr_cm$byClass["Recall"], lr_cm$byClass["F1"]))
  })
  output$nb_acc <- renderPrint({
    return(c(nb_cm$overall["Accuracy"], nb_cm$byClass["Precision"], nb_cm$byClass["Recall"], nb_cm$byClass["F1"]))
  })
  output$svm_acc <- renderPrint({
    return(c(svm_cm$overall["Accuracy"], svm_cm$byClass["Precision"], svm_cm$byClass["Recall"], svm_cm$byClass["F1"]))
  })
  output$rf_acc <- renderPrint({
    return(c(rf_cm$overall["Accuracy"], rf_cm$byClass["Precision"], rf_cm$byClass["Recall"], rf_cm$byClass["F1"]))
  })
  
  output$lr_cm <- renderPlot(plot_confusion_matrix(lr_cm))
  output$nb_cm <- renderPlot(plot_confusion_matrix(nb_cm))
  output$svm_cm <- renderPlot(plot_confusion_matrix(svm_cm))
  output$rf_cm <- renderPlot(plot_confusion_matrix(rf_cm))
  
  output$model_comparison <- renderPlot(plot_model_comparison(model_accuracies), height = 400, width = 500)
  
  output$param_grid_cost <- renderText(paste("[", paste(2^(0:5), collapse = ", "), "]"))
  output$param_grid_gamma <- renderText(paste("[", paste(seq(0, 0.1, 0.01), collapse = ", "), "]"))
  
  output$plot_grid_search <- renderHighchart({
    opt_p <- subset(df_tuned, cnt==max(cnt))[1,]
    sub <- paste("Index of optimal parameters is", opt_p$p, "<br>(cost =", best_cost, ", gamma =", best_gamma, "accuracy :", opt_p$cnt,")")
    
    hchart(df_tuned, 'line', hcaes(p, cnt), name = "Accuracy", tooltip = list(pointFormat = "Accuracy = {point.cnt} <br>Cost = {point.cost} <br>Gamma = {point.gamma}")) %>%
      hc_title(text = "Accuracy With Varying Parameters (SVM)") %>%
      hc_subtitle(text = sub) %>%
      hc_add_theme(hc_theme_google()) %>%
      hc_xAxis(title = list(text = "Index of Parameter Combinations")) %>%
      hc_yAxis(title = list(text = "Accuracy")) %>%
      hc_tooltip(crosshairs = TRUE)
  })
  
  output$best_svm_acc <- renderPrint({
    return(c(best_svm_cm$overall["Accuracy"], best_svm_cm$byClass["Precision"], svm_cm$byClass["Recall"], best_svm_cm$byClass["F1"]))
  })
  
  output$best_svm_cm <- renderPlot(plot_confusion_matrix(best_svm_cm))
  
  output$user_input <- renderUI({
    taglist <- list()
    
    for (i in 1:dim(initial_df)[2]) {
      if (i == 1) { next() }
      col <- colnames(initial_df)[i]
      label <- substr(col, 1, nchar(col) - 5)
      var_name <- paste0(label, "_input")
      
      min_val <- min(initial_df[[col]])
      max_val <- max(initial_df[[col]])
      
      taglist[[i]] <- sliderInput(var_name, label, min = min_val,
                                  max = max_val,
                                  value = min_val + (max_val - min_val) / 2)
      
      
    }
    
    return(taglist)
  })
  
  
  make_prediction <- reactive({
    x_input <- data.frame(radius_mean = as.numeric(input$radius_input), texture_mean = as.numeric(input$texture_input), 
                          perimeter_mean = as.numeric(input$perimeter_input), area_mean = as.numeric(input$area_input), 
                          smoothness_mean = as.numeric(input$smoothness_input), compactness_mean = as.numeric(input$compactness_input),
                          concavity_mean = as.numeric(input$concavity_input), concave_points_mean = as.numeric(input$concave_points_input), 
                          symmetry_mean = as.numeric(input$symmetry_input), fractal_dimension_mean = as.numeric(input$fractal_dimension_input))
    
    
    if (nrow(x_input) == 0) {
      return("Click 'Predict' to make prediction")
    } else {
      x_input_norm <- predict(standardizer, x_input[1,])
      x_input_pca <- predict(pca, x_input_norm)
      x_input_pca <- t(data.frame(x_input_pca[,1:5]))
      
      predicted <- predict(best_svm_classifier, x_input_pca)
      predicted <- as.numeric(as.character(predicted))
      return(predicted)
    }
  })
  
  output$predicted <- renderText({
    
    predicted <- make_prediction()
    
    return(paste0("Predicted output : \n", predicted))
  })
  
  output$b_or_m <- renderText({
    predicted <- make_prediction()
    if (predicted == 1) {
      return("Malignant breast cancer is diagnosed. \nPlease consult a doctor ASAP.")
    } else if (predicted == 0) {
      return("Benign breast cancer is diagnosed. \nDon't worry, you can take your time to consult a doctor.")
    } else {
      return("No information.")
    }
  })
  
  output$best_model_acc <- renderText({
    return(paste0("Current model's accuracy (SVM) : ", round(best_svm_cm$overall["Accuracy"], 3)))
  })
  
  output$additional_info <- renderUI({
    predicted <- make_prediction()
    if (predicted == 0) {
      return(HTML('<div><div class="title section"><h2>Non-cancerous Breast Conditions</h2></div>
        <div class="text parbase section">
          <p>Benign (non-cancerous) breast conditions are very common, and most women have them. In fact, most breast changes are benign. Unlike&nbsp;<a href="https://www.cancer.org/cancer/breast-cancer.html">breast cancers</a>, benign breast conditions are not life-threatening. But some are linked with a higher risk of getting breast cancer later on.</p>
            <p>Some benign breast changes may cause signs or symptoms (such as breast lumps, pain, or nipple discharge), while others might be found during a <a href="https://www.cancer.org/cancer/breast-cancer/screening-tests-and-early-detection/mammograms.html">mammogram</a>. In either case, sometimes they can be hard to tell apart from breast cancer, so other exams or tests might be needed to find out for sure.&nbsp;</p>
              
              </div>
              
              </div>'))
    } else if (predicted == 1) {
      return(HTML("
             <h2>Malignant Breast Conditions</h2>
             <p>If the tumor is borderline or malignant, a wider margin (area of normal tissue around the tumor) usually needs to be removed as well. This might be done with breast-conserving surgery (lumpectomy or partial mastectomy), in which part of the breast is removed. Or the entire breast might be removed with a mastectomy, especially if a margin of normal breast tissue can't be taken out with breast-conserving surgery. Radiation therapy might be given to the area after surgery, especially if it’s not clear that all of the tumor was removed.</p>
             <p>Malignant phyllodes tumors are different from the more common types of breast cancer. They are less likely to respond to some of the treatments commonly used for breast cancer, such as the hormone therapy or chemotherapy drugs normally used for breast cancer. Phyllodes tumors that have spread to other parts of the body are often treated more like sarcomas (soft-tissue cancers) than breast cancers.</p>
             <p>Phyllodes tumors can sometimes come back in the same place. Because of this, close follow-up with frequent breast exams and imaging tests are usually recommended after treatment.</p>"))
    } else {
      return(NULL)
    }
  })
  
})
