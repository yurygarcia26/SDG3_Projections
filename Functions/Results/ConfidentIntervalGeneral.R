bounds_computation <- function(model, method, training_data, testing_data, selected_features, n_samples, multiplier, best_s){

  # Bootstrap Confidence Interval computation algorithm proposed in
  # Kumar, S.; and Srivastava, A. 2012. Bootstrap prediction
  # intervals in non-parametric regression with applications to
  # anomaly detection. In Proc. 18th ACM SIGKDD Conf. Knowl.
  # Discovery Data Mining
  # https://ntrs.nasa.gov/api/citations/20130014367/downloads/20130014367.pdf
  # See https://www.youtube.com/watch?v=c3gD_PwsCGM for an explanation

  fit <- 0
  predictions <- 0
  lower_bound <- 0
  upper_bound <- 0

  bootstrap_predictions <- matrix(0, nrow = n_samples, ncol = dim(testing_data)[1])

  tryCatch({
  for (i in 1:n_samples) {
    
    # Split the data into training and validation sets
    if(multiplier == 1){
      train_indices = c(1:nrow(training_data))
    }else{
      train_indices <- sample(1:nrow(training_data), size=multiplier * nrow(training_data))
    }

    X_train_split <- training_data[train_indices, selected_features]
    y_train_split <- training_data[train_indices, "Outcome"]
    
    # Train and make predictions for each method
    if (method == "Random Forest" | method == "Ranger") {
        
        rf_model <- randomForest(y_train_split$Outcome ~ ., 
                                data = X_train_split,
                                mtry = model$bestTune$mtry,ntree=1000)
        
        # Compute Bootstrapped predictions
        # 1. Fit over training data partition
        fit <- predict(rf_model, newdata = X_train_split)
        # 2. Compute errors
        errors <- fit - y_train_split$Outcome
        # 3. Sample dim(testing_data) errors with replacement
        sample_errors <- sample(errors, size=nrow(testing_data), replace=TRUE)
        # 4. Predict this sub-model over testing data
        predictions <- predict(rf_model, newdata = testing_data[, selected_features])
        # 5. Sum predictions and errors
        bootstrapped_predictions <- predictions + sample_errors
        # Same procedure for all other models
        
    } else if (method == "Lasso") {
        
        X_train_lasso= as.matrix(X_train_split)
        lasso_model <- glmnet(X_train_lasso,
                              y_train_split$Outcome,
                              alpha = 1,
                              lambda = model$bestTune$lambda)
        
        # Compute Bootstrapped predictions
        fit <- as.vector(predict(lasso_model, newx = X_train_lasso, s=best_s))
        errors <- fit - y_train_split$Outcome
        sample_errors <- sample(errors, size=nrow(testing_data), replace=TRUE)
        predictions <- as.vector(predict(lasso_model,
                                         newx = as.matrix(testing_data[, selected_features]),
                                         s=best_s))
        bootstrapped_predictions <- predictions + sample_errors
        
    } else if (method == "XGBoost") {
        
        output<- capture.output(xgb_model <- xgboost(data = as.matrix(X_train_split),
                                                    label            = y_train_split$Outcome,
                                                    nrounds          = model$bestTune$nrounds,
                                                    max_depth        = model$bestTune$max_depth,
                                                    eta              = model$bestTune$eta,
                                                    gamma            = model$bestTune$gamma,
                                                    colsample_bytree = model$bestTune$colsample_bytree,
                                                    min_child_weight = model$bestTune$min_child_weight,
                                                    subsample        = model$bestTune$subsample,
                                                    objective        = "reg:squarederror"))
        
        # Compute Bootstrapped predictions
        fit <- predict(xgb_model, newdata = as.matrix(X_train_split))
        errors <- fit - y_train_split$Outcome
        sample_errors <- sample(errors, size=nrow(testing_data), replace=TRUE)
        predictions <- predict(xgb_model, newdata = as.matrix(testing_data[, selected_features]))
        bootstrapped_predictions <- predictions + sample_errors
        
    } else if (method == "Boost_glm") {

        # This function doesn't work
        # boosted_glm_model <- boostGLM(y ~ ., data = X_train_split, family = gaussian(),
        #                             mstop = model$bestTune$mstop, prune = model$bestTune$prune)

        # Train with caret then.
        tunegrid <- expand.grid(mstop = model$bestTune$mstop, prune=model$bestTune$prune)
        boosted_glm_model <- train(X_train_split, y_train_split$Outcome,
        method="glmboost", metric='RMSE', tuneGrid=tunegrid)
        
        # Compute Bootstrapped predictions
        fit <- predict(boosted_glm_model, newdata = X_train_split)
        errors <- fit - y_train_split$Outcome
        sample_errors <- sample(errors, size=nrow(testing_data), replace=TRUE)
        predictions <- predict(boosted_glm_model, newdata = testing_data[, selected_features])
        bootstrapped_predictions <- predictions + sample_errors

    } else if (method == "Boost_gam") {

        # This function doesn't work
        # boosted_gam_model <- boostGAM(y ~ ., data = X_train_split, family = gaussian(),
        #                               mstop = model$bestTune$mstop, prune = model$bestTune$prune)

        # Train with caret then.
        tunegrid <- expand.grid(mstop = model$bestTune$mstop, prune=model$bestTune$prune)
        boosted_gam_model <- train(X_train_split, y_train_split$Outcome,
        method="gamboost", metric='RMSE', tuneGrid=tunegrid)

        # Compute Bootstrapped predictions
        fit           <- predict(boosted_gam_model, newdata = X_train_split)
        errors        <- fit - y_train_split$Outcome
        sample_errors <- sample(errors, size=nrow(testing_data), replace=TRUE)
        predictions   <- predict(boosted_gam_model, newdata = testing_data[, selected_features])
        bootstrapped_predictions <- predictions + sample_errors
      
    }
    bootstrap_predictions[i, ] <- bootstrapped_predictions
  }

  # Calculate the 95% prediction interval for prediction
  lower_bound <- apply(bootstrap_predictions, 2, function(x) quantile(x, 0.025))
  upper_bound <- apply(bootstrap_predictions, 2, function(x) quantile(x, 0.975))

  list_return = list(
    fit = fit,
    predictions = predictions,
    lower_bound = lower_bound,
    upper_bound = upper_bound
  )

  return(list_return)

  }, error = function(err){
    
    cat(paste("Error computing confidence intervals for ", region, " ", err, "\n"))
    cat("Will use std \n")
    uncertainty <- sd(model_result$Prediction)
    lower_bound <-model_result$Prediction - uncertainty
    upper_bound <-model_result$Prediction + uncertainty

    list_return = list(
      fit = fit,
      predictions = predictions,
      lower_bound = lower_bound,
      upper_bound = upper_bound
    )

    return(list_return)

  })
}


CI_function <- function(model_result, training_data, testing_data,
                        info_df, region, region_col, best_s, Outcome_name){

   # List all files in the folder
  folder_path <- file.path(getwd(), "Models")
  # Construct the file path
  file_name <- paste0("best_model_", region, "_", Outcome_name, ".rds")
  file_path <- file.path(folder_path, file_name)
  model     <- readRDS(file_path)

  # Set a seed for reproducibility
  set.seed(123)  
  data_region      <- info_df%>%filter(!!as.symbol(region_col)==region)
  selected_features <- data_region$Relevant_Var
  method            <- data_region$method[1]
  n_samples         <- 100

  bounds_results <- bounds_computation(model, method, training_data, testing_data, selected_features, 1000, 0.8, best_s)

  model_result$lower_bound<-bounds_results$lower_bound
  model_result$upper_bound<-bounds_results$upper_bound

  return(model_result)
}   