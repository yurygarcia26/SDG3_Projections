XGBoost_function <- function(region_col, training_data, testing_data, ..., hyper_tune=TRUE, cv=TRUE){
  
  set.seed(123)
  threshold_values <- 3:(ncol(training_data)-3)  #threshold to select features in xgboost
  results <- list()
  cont <- 0

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
  cat("Hyperparameter tunning for xgboost model ... \n")

  control <- trainControl(method="repeatedcv", number=3, verboseIter=FALSE, allowParallel=TRUE)

  if (hyper_tune) {
    tunegrid <- expand.grid(  eta = c(0.01, 0.1, 0.3),
                              nrounds = c(10, 25, 50),
                              max_depth = c(3, 6, 9),
                              subsample = 1,
                              gamma     = 0,
                              colsample_bytree = 1,
                              min_child_weight = 1
                              )
  } else {
    tunegrid <- expand.grid(nrounds = 50, 
                            eta = 0.1, 
                            max_depth = 4,
                            subsample = 1, 
                            gamma = 0, 
                            colsample_bytree = 1, 
                            min_child_weight = 1)
  }

  
  metric <- "RMSE"
  ## Train a xgboost model
  if (cv){
    model_train <- train(X_train, y_train,
                    method   = "xgbTree", 
                    metric   = metric,
                    trControl= control, 
                    tuneGrid = tunegrid,
                    verbose  = 0, 
                    verbosity = 0)
  } else {
    model_train <- train(X_train, y_train,
                    method  = "xgbTree", 
                    metric  = metric,
                    tuneGrid= tunegrid,
                    verbose = 0, 
                    verbosity = 0)
  }

  cat("Feature Selection for XGBoost ... \n")
  
  # Extract feature importance scores
  importance_scores <- varImp(model_train, scale=FALSE)$importance
  importance_scores <- data.frame(features=row.names(importance_scores),
                                  importance=importance_scores$Overall)
  importance_scores <- importance_scores %>% arrange(desc(importance))
  importance_scores_no_zero <-importance_scores[importance_scores$importance>0,]
  
  for (threshold in 3:(nrow(importance_scores_no_zero))) {
  
    # Select the top N important features
    selected_features <- importance_scores_no_zero$features[1:threshold]
    selected_features_importances <- importance_scores_no_zero$importance[1:threshold]

    # Subset the training and testing data with selected features
    X_train_selected <- subset(X_train, select = selected_features)
    X_test_selected  <- subset(X_test, select = selected_features)
  
    # Retrain the XGBoost model with selected features
    if (cv){
      model_train_selected <- train(X_train_selected, y_train,
                    method = "xgbTree", 
                    metric=metric,
                    trControl=control, 
                    tuneGrid=tunegrid, 
                    verbose = 0, 
                    verbosity = 0,
                    tuneLength =5)
    } else {
      model_train_selected <- train(X_train_selected, y_train,
                    method  = "xgbTree", 
                    metric  = metric,
                    tuneGrid= tunegrid, 
                    verbose = 0, 
                    verbosity = 0,
                    tuneLength =5)
    }

    # Make predictions on the testing data/training data
    fit <- predict(model_train_selected, newdata=X_train_selected)
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
    }
  }

  cat("XGBoost Done \n")

  return(lst(
    model           = final_model,
    Fit             = training_data,
    Predictions     = testing_data,
    features        = final_features,
    importances     = final_features_weights,
    best_threshold  = best_threshold))
}
