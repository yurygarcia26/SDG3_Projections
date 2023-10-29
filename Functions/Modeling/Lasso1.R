Lasso_function <- function(region_col, training_data, testing_data,...,hyper_tune=TRUE, cv=TRUE) {
  
  set.seed(123)
  results <- list()
  
  y_train <- training_data$Outcome
  X_train <- subset(training_data, select = -c(Year, Outcome))  # Exclude the "Outcome" column
  X_train <- as.matrix(X_train[, which(colnames(X_train)!=region_col)])

  y_test <- testing_data$Outcome
  X_test <- subset(testing_data, select = -c(Year, Outcome))  # Exclude the "Outcome" column
  X_test <- as.matrix(X_test[, which(colnames(X_test)!=region_col)])
  
  # Initialize variables to keep track of the best model and its performance
  best_accuracy  <- Inf
  best_threshold <- 0
  final_model    <- NULL
  
  # Find best hyper parameters for XGBoost model
  cat("Hyperparameter tunning for Lasso model ... \n")
  
  # Create a sequence of lambda values
  #lambda_seq <- 10^seq(3, -2, by = -0.1)
  start_s <- 0.0001
  end_s   <- 1
  num_s   <- 12
  
  lambda_seq <- 10^seq(log10(start_s), log10(end_s), length.out = num_s)
  # Create a training control object for cross-validation
  control <- trainControl(method = "repeatedcv", number = 5)

  if (hyper_tune) {
    tunegrid <- expand.grid(alpha = 1,lambda = lambda_seq)
  } else {
    tunegrid <- expand.grid(alpha = 1,lambda = 0.01)
  }
  
  metric <- "RMSE"
  
  # Tune the Lasso model
  if (cv){
    model_train <- train(
      x = as.matrix(X_train), y = y_train,
      method    = "glmnet",
      metric    = metric,
      tuneGrid  = tunegrid,
      trControl = control)
  }else{
    model_train <- train(
      x = as.matrix(X_train), y = y_train,
      method    = "glmnet",
      metric    = metric,
      tuneGrid  = tunegrid)
  }
  
  cat("Feature Selection for Lasso ... \n")
  
  # Extract feature importance scores
  importance_scores <- varImp(model_train, scale=FALSE)$importance
  importance_scores <- data.frame(features=row.names(importance_scores),
                                  importance=importance_scores$Overall)
  importance_scores         <- importance_scores %>% arrange(desc(importance))
  importance_scores_no_zero <-importance_scores[importance_scores$importance>0,]
  
  if (nrow(importance_scores_no_zero)<=2){
    cat("Lasso Done \n")
    
    # Make predictions on the testing data/training data
    fit         <- predict(model_train, newdata=X_train)
    predictions <- predict(model_train, newdata=X_test)
    
    testing_data$Prediction  <- predictions
    training_data$Fit        <- fit
    final_features           <- importance_scores_no_zero$features
    
    if(nrow(importance_scores_no_zero)>0){
      relevant_var<-importance_scores_no_zero$features
    }else{
      relevant_var = colnames(subset(training_data, select = -c(Year, Outcome)))
    }
    
    return(lst(
      model           = model_train,
      Fit             = training_data,
      Predictions     = testing_data,
      features        = relevant_var,
      best_threshold  = ncol(training_data)-3))
    
  }else{
    for (threshold in 3:(nrow(importance_scores_no_zero))) {
      
      # Select the top N important features
      selected_features <- importance_scores$features[1:threshold]
      
      # Subset the training and testing data with selected features
      X_train_selected <- subset(X_train, select = selected_features)
      X_test_selected  <- subset(X_test, select = selected_features)
      
      # Retrain the XGBoost model with selected features
      if (cv){
        model_train <- train(
          x = as.matrix(X_train_selected), y = y_train,
          method    = "glmnet",
          metric    = metric,
          tuneGrid  = tunegrid,
          trControl = control)
      }else{
        model_train <- train(
          x = as.matrix(X_train_selected), y = y_train,
          method    = "glmnet",
          metric    = metric,
          tuneGrid  = tunegrid)
      }
      
      # Make predictions on the testing data/training data
      fit <- predict(model_train, newdata=X_train_selected,s= model_train$bestTune$lambda)
      predictions <- predict(model_train, newdata=X_test_selected, s= model_train$bestTune$lambda)
      
      # Evaluate the model's performance for this threshold
      accuracy <- sqrt(mean((y_test - predictions)^2))
      
      # Check if this model has the best accuracy so far
      if(accuracy < best_accuracy) {
        training_data$Fit        <- fit
        testing_data$Prediction  <- predictions
        best_accuracy            <- accuracy
        best_threshold           <- threshold
        final_features           <- selected_features
        final_model              <- model_train
      }
    }
    
    cat("Lasso Done \n")
    
    return(lst(
      model           = final_model,
      Fit             = training_data,
      Predictions     = testing_data,
      features        = final_features,
      best_threshold  = best_threshold))
  }
}
