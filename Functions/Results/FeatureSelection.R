#### Deprecated Feature Selection with Lasso

feature_selection_function<-function(country_data){
    set.seed(42) 
    # X (predictors) and y (target variable)
    y <- as.vector(country_data$Outcome)
    X <- as.matrix(subset(country_data, select = -c(Country, Year, Outcome)))
    
    # Create a sequence of lambda values (regularization parameter)
    lambda_seq <- 10^seq(3, -2, by = -0.1)
    
    # Fit Lasso Regression models with cross-validation
    lasso_model       <- cv.glmnet(X, y, alpha = 1, lambda = lambda_seq)
    selected_features <- predict(lasso_model, s = "lambda.min", type = "nonzero")
    
    # Subset the testing data to include only the selected features
    selected_column_indices <- selected_features$lambda.min
    var_names               <- colnames(X[, selected_column_indices])
    
    # Identify relevant variables based on coefficients
    selected_variables <- coef(lasso_model, s = "lambda.min")
    return(var_names)
  }
  