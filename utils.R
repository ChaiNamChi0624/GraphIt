library(caTools)
library(caret)
library(e1071)
library(randomForest)
library(ada)

load_data <- function(filepath, seed) {
  df <- read.csv(filepath)
  # Encode B into 0, M into 1
  df$diagnosis <- factor(df$diagnosis, level = c('B', 'M'), labels = c(0, 1))
  
  # drop column id 
  df <- subset(df, select = -c(1))
  
  # Shuffle rows
  set.seed(seed)
  df <- df[sample(1:nrow(df)), ]
  
  # Stratified split the dataset into train & test set
  set.seed(seed)
  split <- sample.split(df$diagnosis, SplitRatio = 0.7)
  train_df <- subset(df, split == TRUE)
  test_df <- subset(df, split == FALSE)
  x_train <- train_df[-1]
  y_train <- train_df[["diagnosis"]]
  x_test <- test_df[-1]
  y_test <- test_df[["diagnosis"]]
  
  return(list("df" = df, "x_train" = x_train, "y_train" = y_train, "x_test" = x_test, "y_test" = y_test))
}

apply_pca <- function(x_train, x_test, seed) {
  # Perform PCA to reduce the dimensions of data
  standardizer <- preProcess(x_train, method = c("center", "scale"))
  x_train <- predict(standardizer, x_train)
  x_test <- predict(standardizer, x_test)
  
  set.seed(seed)
  pca <- prcomp(x_train)
  
  x_train_pca <- predict(pca, x_train)
  x_train_pca <- x_train_pca[,1:5]
  x_test_pca <- predict(pca, x_test)
  x_test_pca <- x_test_pca[,1:5]
  
  return(list("pca" = pca, "standardizer" = standardizer, "x_train" = as.data.frame(x_train_pca), "x_test" = as.data.frame(x_test_pca)))
}

train_model <- function(model_name, x_train, y_train, seed) {
  set.seed(seed)
  if (model_name == "naiveBayes") {
    model <- naiveBayes(x_train, y_train, laplace=0)
    
  } else if (model_name == "randomForest") {
    model <- randomForest(x = x_train, y = y_train, ntree=500, proximity = TRUE, importance = TRUE)
    
  } else if (model_name == "logisticRegression") {
    lr_data <- data.frame(x_train)
    lr_data$diagnosis <- y_train
    model <- glm(formula = diagnosis ~., family = binomial(link = "logit"), data = lr_data)
    
  } else if (model_name == "SVM") {
    model <- svm(x_train, y_train)
    
  } else {
    stop("No model named", model_name)
  }
  return(model)
}

plot_confusion_matrix <- function(cm) {
  table <- data.frame(cm$table)
  
  # change 1 to malignant, 0 to benign
  table <- table %>%
    mutate(across(1:2, 
                  ~ case_when(
                    . == 0 ~ "benign",
                    . == 1 ~ "malignant"
                  )))
  
  plot_table <- table %>%
    mutate(goodbad = ifelse(table$Prediction == table$Reference, "good", "bad")) %>%
    group_by(Reference) %>%
    mutate(prop = Freq/sum(Freq))
  
  # fill alpha relative to sensitivity/specificity by proportional outcomes within reference groups (see dplyr code above as well as original confusion matrix for comparison)
  p <- ggplot(data = plot_table, mapping = aes(x = Reference, y = Prediction, fill = goodbad, alpha = prop)) +
    geom_tile() +
    geom_text(aes(label = Freq), vjust = .5, fontface  = "bold", alpha = 1) +
    scale_fill_manual(values = c(good = "green", bad = "red")) +
    theme_bw() +
    theme(legend.position = "none")
  return(p)
}

plot_model_comparison <- function(model_accuracies) {
  # Model comparison using accuracy
  p <- ggplot(data=model_accuracies, aes(x=model, y=accuracy, fill=model)) +
    geom_bar(stat="identity", color="black") +
    ggtitle("Model Comparison") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "none") +
    geom_text(aes(label=round(accuracy, digits=3), vjust=1.6), color="white",
              position = position_dodge(0.9), size=3.5) + 
    geom_hline(aes(yintercept = max(accuracy), colour = "red"), linetype = "dashed")
  return(p)
}

tune_svm <- function(x_train, y_train, x_test, y_test, seed) {
  param_grid <- expand.grid(cost=2^(0:5), gamma=seq(0, 0.1, 0.01))
  
  df_tuned <- data.frame(matrix(vector(), 0, 4,
                                dimnames=list(c(), c("p", "cnt", "cost", "gamma"))),
                         stringsAsFactors=FALSE)
  
  for (i in 1:nrow(param_grid)) {
    set.seed(seed)
    svm_temp = svm(x_train, y_train, gamma=param_grid$gamma[i], cost=param_grid$cost[i])
    y_pred_temp = predict(svm_temp, x_test)
    acc_temp <- mean(y_pred_temp == y_test)
    df_tuned[nrow(df_tuned) + 1,] = list("p" = i, "cnt" = acc_temp, "cost" = param_grid$cost[i], "gamma" = param_grid$gamma[i])
  }
  opt_p <- subset(df_tuned, cnt==max(cnt))[1,]
  return(list("df_tuned" = df_tuned, "best_gamma" = opt_p$gamma, "best_cost" = opt_p$cost))
}

