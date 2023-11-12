lasso_feature_selection <- function(region_col, region_data){

  set.seed(123) 

  # X (predictors) and y (target variable)
  y <- as.vector(region_data$Outcome)
  X <- subset(region_data, select = -c(Year, Outcome))  # Exclude the "Outcome" column
  X <- as.matrix(X[, which(colnames(X)!=region_col)])

  # Create a sequence of lambda values (regularization parameter)
  lambda_seq <- 10^seq(3, -2, by = -0.1)
  
  # Fit Lasso Regression models with cross-validation
  lasso_model       <- cv.glmnet(X, y, alpha = 1, lambda = lambda_seq)
  selected_features <- predict(lasso_model, s = "lambda.min", type = "nonzero")
  
  # Subset the testing data to include only the selected features
  selected_column_indices <- selected_features$lambda.min
  var_names <- colnames(X[, selected_column_indices])
  
  # Identify relevant variables based on coefficients
  var_imps <- as.data.frame(as.matrix(coef(lasso_model, s = "lambda.min")))

  list_return = list()
  list_return$var_names <- var_names
  list_return$var_imps <- var_imps

  return(list_return)
}

Lasso_function <- function(region_col, training_data, testing_data, ..., hyper_tune=TRUE, cv=TRUE) {
  
  set.seed(123)
  
  # Perform Lasso Feature Selection
  cat("Feature Selection for Lasso ... \n")
  all_data_by_country = rbind(training_data, testing_data)
  relevant_Var_Info <- lasso_feature_selection(region_col, all_data_by_country)

  relevant_Var <- relevant_Var_Info$var_names
  relevant_Var_Imps <- relevant_Var_Info$var_imps
  
  if (length(relevant_Var) > 2){

    Relevant_data  <- all_data_by_country[,c(region_col, "Year", "Outcome", relevant_Var)]
    training_data_use <- Relevant_data %>% filter(Year%in%2000:2015)
    testing_data_use  <- Relevant_data %>% filter(Year%in%2016:2019)
    relevant_Var_Imps <- relevant_Var_Imps[relevant_Var, ]

  }else{
    training_data_use <- all_data_by_country %>% filter(Year%in%2000:2015)
    testing_data_use  <- all_data_by_country %>% filter(Year%in%2016:2019)

    data_relev <- subset(all_data_by_country, select = -c(Year, Outcome))
    data_relev <- data_relev[, which(colnames(data_relev)!=region_col)]
    relevant_var = colnames(data_relev)
    relevant_Var_Imps <- relevant_Var_Imps[relevant_Var, ]

  }
  
  cat("Hyperparameter tunning for Lasso model ...\n")
  training_data2 <- subset(training_data_use, select = -c(Year))
  training_data2 <- training_data2[, which(colnames(training_data2)!=region_col)]
  testing_data2  <- subset(testing_data_use, select = -c(Year))
  testing_data2 <- testing_data2[, which(colnames(testing_data2)!=region_col)]
  
  y_train <- training_data2$Outcome
  X_train <- as.matrix(subset(training_data2, select = -Outcome))
  
  y_test <- testing_data$Outcome
  X_test <- as.matrix(subset(testing_data2, select = -Outcome))
  
  # Initialize variables to keep track of the best model and its performance
  best_accuracy  <- Inf
  best_threshold <- length(relevant_Var)
  final_model    <- NULL
  best_s <- NULL
  
  # Create a log-spaced sequence of s values
  start_s <- 0.0001
  end_s   <- 1
  num_s   <- 12
  s_values <- 10^seq(log10(start_s), log10(end_s), length.out = num_s)
  
  # Iterate through the range of s values
  for (s in s_values) {
    
    # Fit the Lasso Regression model
    lasso_model <- glmnet(X_train, y_train, alpha = 1)
    
    # Make predictions on the testing data/training data
    fit <- as.vector(predict(lasso_model, s = s, newx = X_train))
    predictions <- as.vector(predict(lasso_model, s = s, newx = X_test))
    
    # Evaluate the model's performance for this threshold
    accuracy <- sqrt(mean((y_test - predictions)^2))
    
    # Check if this model has the best accuracy so far
    if (accuracy < best_accuracy) {
      training_data$Fit        <- fit
      testing_data$Prediction  <- predictions
      best_accuracy            <- accuracy
      final_model              <- lasso_model
      best_s <- s
    }
  }
  
  cat("Lasso Done \n")

  return(lst(
    model           = final_model,
    Fit             = training_data,
    Predictions     = testing_data,
    features        = relevant_Var,
    importances     = relevant_Var_Imps,
    best_threshold  = best_threshold,
    best_s          = best_s
  ))
}
