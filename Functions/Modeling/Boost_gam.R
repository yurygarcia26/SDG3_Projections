Boost_gam_function <- function(region_col, training_data, testing_data, ..., hyper_tune=TRUE, cv=TRUE) {
  
  set.seed(123)

  threshold_values <- 3:(ncol(training_data)-3)  #threshold to select features in rf
  results <- list()
  cont = 0
  
  y_train <- training_data$Outcome
  X_train <- subset(training_data, select = -c(Year, Outcome))  # Exclude the "Outcome" column
  X_train <- X_train[, which(colnames(X_train)!=region_col)]
  
  y_test <- testing_data$Outcome
  X_test <- subset(testing_data, select = -c(Year, Outcome))  # Exclude the "Outcome" column
  X_test <- X_test[, which(colnames(X_test)!=region_col)]

  # Remove columns with near zero variance for the model fit
  zero_var_train <- nearZeroVar(X_train, saveMetrics = TRUE)
  zero_var_train_list <- rownames(zero_var_train[zero_var_train$zeroVar == TRUE,])
  zero_var_test <- nearZeroVar(X_test, saveMetrics = TRUE)
  zero_var_test_list <- rownames(zero_var_test[zero_var_test$zeroVar == TRUE,])
  zero_var_list = c(zero_var_train_list, zero_var_test_list)
  if(length(zero_var_list) > 0){
    X_train <- data.frame(X_train[, -which(names(X_train) %in% zero_var_list)])
    X_test <- data.frame(X_test[, -which(names(X_test) %in% zero_var_list)])
  }

  # Initialize variables to keep track of the best model and its performance
  best_accuracy  <- Inf
  best_threshold <- 0
  final_model    <- NULL
  
  # Find best hyperparameters for Boosted GLM model
  cat("Hyperparameter tuning for Boost GAM... \n")
  
  control <- trainControl(
    method        = "repeatedcv",  # Use repeated cross-validation
    number        = 5,             # Number of folds in cross-validation
    repeats       = 3,             # Number of times to repeat cross-validation
    verboseIter   = FALSE,         # Do not print iteration details
    allowParallel = TRUE           # Allow parallel processing if available
  )
  
  if (hyper_tune) {
    tunegrid <- expand.grid(mstop = c(10, 50, 100), prune = c(0, 0.01, 0.1, 1))
  } else {
    tunegrid <- expand.grid(mstop = 50, prune=0)
  }
  
  metric <- "RMSE"
  ## Train a random forest model
  if (cv){
    model_train <- train(X_train, y_train,
                         method="gamboost", 
                         metric=metric,
                         tuneGrid=tunegrid,
                         trControl=control)
  } else {
    model_train <- train(X_train, y_train,
                         method="gamboost", 
                         metric=metric,
                         tuneGrid=tunegrid)
  }
  
  cat("Feature Selection for GAM Boost ... \n")
  
  # Extract feature importance scores
  predictions         <- predict(model_train,newdata=X_test)
  importance_scores   <- filterVarImp(X_test,predictions)
  importance_scores   <- data.frame(features=row.names(importance_scores),
                                  importance=importance_scores$Overall)
  importance_scores <- importance_scores%>%arrange(desc(importance))
  importance_scores_no_zero <- importance_scores[importance_scores$importance>0,]
  
  
  for (threshold in 3:(nrow(importance_scores_no_zero))) {
    
    # Select the top N important features
    selected_features <- importance_scores_no_zero$features[1:threshold]
    selected_features_importances <- importance_scores_no_zero$importance[1:threshold]
    
    # Subset the training and testing data with selected features
    X_train_selected <- subset(X_train, select = selected_features)
    X_test_selected  <- subset(X_test, select = selected_features)
    
    if (cv){
      model_train_selected <- train(X_train_selected, 
                                    y_train,
                                    method   ="gamboost", 
                                    metric   =metric,
                                    trControl=control,
                                    tuneGrid =tunegrid)
    } else {
      model_train_selected <- train(X_train_selected, 
                                    y_train,
                                    method  ="gamboost", 
                                    metric  =metric,
                                    tuneGrid=tunegrid)
    }
    
    # Make predictions on the testing data/training data
    fit         <- predict(model_train_selected, newdata=X_train_selected)
    predictions <- predict(model_train_selected, newdata=X_test_selected)
    
    # Evaluate the model's performance for this threshold
    accuracy <- sqrt(mean((y_test - predictions)^2))
    
    # Check if this model has the best accuracy so far
    if(accuracy < best_accuracy) {
      training_data$Fit        <- fit
      testing_data$Prediction  <- predictions
      best_accuracy            <- accuracy
      best_threshold           <- threshold
      final_features           <- selected_features
      final_features_weights   <- selected_features_importances
      final_model              <- model_train_selected
    }else{
      cont= cont+1
      if(cont>10){break}
    }
    
  }
  
  cat("Gam Boost Done \n")
  
  return(lst(
    model           = final_model,
    Fit             = training_data,
    Predictions     = testing_data,
    features        = final_features,
    importances     = final_features_weights,
    best_threshold  = best_threshold))
}


