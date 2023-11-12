Boost_glm_function <- function(region_col, training_data, testing_data, ..., hyper_tune=TRUE, cv=TRUE) {
  
  set.seed(123)
  threshold_values <- 3:(ncol(training_data)-3)  #threshold to select features in boosted glm
  results <- list()
  
  y_train <- training_data$Outcome
  X_train <- subset(training_data, select = -c(Year, Outcome))  # Exclude the "Outcome" column
  X_train <- X_train[, which(colnames(X_train)!=region_col)]
  
  y_test <- testing_data$Outcome
  X_test <- subset(testing_data, select = -c(Year, Outcome))  # Exclude the "Outcome" column
  X_test <- X_test[, which(colnames(X_test)!=region_col)]
  
  # Initialize variables to keep track of the best model and its performance
  best_accuracy  <- Inf
  best_threshold <- 0
  final_model    <- NULL
  
  # Find best hyperparameters for Boosted GLM model
  cat("Hyperparameter tuning for GAM spline... \n")
  
  control <- trainControl(
    method        = "repeatedcv",  # Use repeated cross-validation
    number        = 5,             # Number of folds in cross-validation
    repeats       = 3,             # Number of times to repeat cross-validation
    verboseIter   = FALSE,         # Do not print iteration details
    allowParallel = TRUE           # Allow parallel processing if available
  )
  
  if (hyper_tune) {
    tunegrid <- expand.grid(df = seq(2, 6, by = 1))
  } else {
    tunegrid <- expand.grid(df = 3)
  }
  
  metric <- "RMSE"
  
  ## Train a boosted GLM model
  if (cv) {
    model_train <- train(X_train, y_train,
                         method   = "gamSpline",
                         metric   = metric,
                         tuneGrid = tunegrid, 
                         trControl= control)
  }else {
    model_train <- train(X_train, y_train,
                         method  = "gamSpline", 
                         metric  = metric,
                         tuneGrid= tunegrid)
  }
  
  cat("Feature Selection for GAM Spline... \n")
  # Extract variable importance scores
  importance_scores <- varImp(model_train, scale = FALSE)$importance
  importance_scores <- data.frame(features=row.names(importance_scores),
                                  importance=importance_scores$Overall)
  importance_scores         <- importance_scores %>% arrange(desc(importance))
  importance_scores_no_zero <- importance_scores[importance_scores$importance > 0,]
  
  if (nrow(importance_scores_no_zero) <= 2) {
    cat("Lasso Done \n")
    
    # Make predictions on the testing data/training data
    fit         <- predict(model_train, newdata = X_train)
    predictions <- predict(model_train, newdata = X_test)
    
    testing_data$Prediction  <- predictions
    training_data$Fit        <- fit
    final_features           <- importance_scores_no_zero$features
    
    if (nrow(importance_scores_no_zero) > 0) {
      relevant_var <- importance_scores_no_zero$features
    } else {
      train_data_relev <- subset(training_data, select = -c(Year, Outcome))
      train_data_relev <- train_data_relev[, which(colnames(train_data_relev)!=region_col)]
      relevant_var = colnames(train_data_relev)
    }
    
    return(list(
      model           = model_train,
      Fit             = training_data,
      Predictions     = testing_data,
      features        = relevant_var,
      best_threshold  = length(relevant_var)))
      
  } else {
    for (threshold in 3:(nrow(importance_scores_no_zero))){
      
      # Select the top N important features
      selected_features <- importance_scores_no_zero$features[1:threshold]
      selected_features_importances <- importance_scores_no_zero$importance[1:threshold]
      
      # Subset the training and testing data with selected features
      X_train_selected <- subset(X_train, select = selected_features)
      X_test_selected  <- subset(X_test, select  = selected_features)
      
      if (cv) {
        model_train <- train(X_train_selected, y_train,
                             method   = "gamSpline",
                             metric   = metric,
                             tuneGrid = tunegrid, 
                             trControl= control)
      } else {
        model_train <- train(X_train_selected, y_train,
                             method  = "gamSpline", 
                             metric  = metric,
                             tuneGrid= tunegrid)
      }
      
      # Make predictions on the testing data/training data
      fit         <- predict(model_train, newdata = X_train_selected)
      predictions <- predict(model_train, newdata = X_test_selected)
      
      # Evaluate the model's performance for this threshold
      accuracy <- sqrt(mean((y_test - predictions)^2))
      print(accuracy)
      # Check if this model has the best accuracy so far
      if (accuracy < best_accuracy) {
        training_data$Fit        <- fit
        testing_data$Prediction  <- predictions
        best_accuracy            <- accuracy
        best_threshold           <- threshold
        final_features           <- selected_features
        final_features_weights   <- selected_features_importances
        final_model              <- model_train
      }
    }
    
    cat("Boosted GLM Done \n")
    
    return(list(
      model           = final_model,
      Fit             = training_data,
      Predictions     = testing_data,
      features        = final_features,
      importances     = final_features_weights,
      best_threshold  = best_threshold))
  }
}

