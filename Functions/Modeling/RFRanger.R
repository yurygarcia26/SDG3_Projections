rfRanger_function <- function(region_col, training_data, testing_data, ..., hyper_tune=TRUE, cv=TRUE) {
  
  set.seed(123)
  threshold_values <- 3:(ncol(training_data)-3)  #threshold to select features in rf
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

  
  # Create a ranger model
  rf_model <- ranger(y_train ~ ., 
                     data = X_train,
                     importance = "impurity",  # You can use "permutation" or other options based on your needs
                     num.trees = 5000)  # You may adjust the number of trees
  
  # Extract feature importance
  relevant_var <- as.data.frame(importance(rf_model))
  colnames(relevant_var)<-"Overall"
  relevant_var$Feature  <-rownames(relevant_var)
  
  # Sort features by importance
  sorted_features <-  relevant_var[order( relevant_var$Overall, decreasing = TRUE), ]
  
  for (threshold in 3:(nrow(sorted_features))) {
    
    # Select the top N important features
    selected_features <- sorted_features$Feature[1:threshold]
    
    # Subset the training and testing data with selected features
    X_train_selected <- subset(X_train, select = selected_features)
    X_test_selected  <- subset(X_test, select = selected_features)
    
    rf_model_selected <- ranger(y_train ~ ., 
                         data = X_train_selected,
                         importance = "impurity",  # You can use "permutation" or other options based on your needs
                         num.trees = 5000)  # You may adjust the number of trees
    
    # Make predictions on the testing data/training data
    fit         <- predict(rf_model_selected, data=X_train_selected)
    predictions <- predict(rf_model_selected, data=X_test_selected)

    # Evaluate the model's performance for this threshold
    accuracy <- sqrt(mean((y_test - predictions$predictions)^2))
  
    
    # Check if this model has the best accuracy so far
    if(accuracy < best_accuracy) {
      training_data$Fit        <- fit$predictions
      testing_data$Prediction  <- predictions$predictions
      best_accuracy            <- accuracy
      best_threshold           <- threshold
      final_features           <- selected_features
      final_model              <- rf_model_selected
    }
    
  }
  
  cat("Random Forest Done \n")
  
  return(lst(
    model           = final_model,
    Fit             = training_data,
    Predictions     = testing_data,
    features        = final_features,
    best_threshold  = best_threshold))
  
}
