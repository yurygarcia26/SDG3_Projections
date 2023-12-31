CI_New_Projection<- function(training_data,testing_data,model_info,country,Model_object,prediction,best_s){
  #--------------------
  cat("--------------------------------------------\n")
  cat("Computing CI for the following variables:\n")
  cat("\ncountry:\n")
  print(country)
  #-------------------------------------------------------

  #This is the model after the machine learning process
  model  <- Model_object

  # Set a seed for reproducibility
  set.seed(123)  
  data_country      <- model_info%>%filter(Country==country)
  selected_features <- data_country$Relevant_Var
  method            <- data_country$method[1]
  n_samples         <- 200
 
  # Bootstrap Confidence Interval computation algorithm proposed in
  # Kumar, S.; and Srivastava, A. 2012. Bootstrap prediction
  # intervals in non-parametric regression with applications to
  # anomaly detection. In Proc. 18th ACM SIGKDD Conf. Knowl.
  # Discovery Data Mining
  # https://ntrs.nasa.gov/api/citations/20130014367/downloads/20130014367.pdf
  # See rfor an explanation

  bootstrap_predictions <- matrix(0, nrow = n_samples, ncol = dim(testing_data)[1])
  
  tryCatch({
    for (i in 1:n_samples) {
      #print(i)
      # Split the data into training and validation sets
      train_indices <- sample(1:nrow(training_data), size = 0.8 * nrow(training_data))
      X_train_split <- training_data[train_indices, selected_features]
      y_train_split <- training_data[train_indices, "Outcome"]
      
      # Train and make predictions for each method
      if (method == "Random Forest" | method == "Ranger") {
        
        rf_model <- randomForest(y_train_split$Outcome ~ ., 
                                 data = X_train_split,
                                 mtry = model$bestTune$mtry,
                                 ntree=1000)
        
        # Compute Bootstrapped predictions
        # 1. Fit over training data partition
        fit <- predict(rf_model, newdata = X_train_split)
        # 2. Compute errors
        errors <- fit - y_train_split$Outcome
        # 3. Sample dim(testing_data) errors with replacement
        sample_errors <- sample(errors, size=nrow(testing_data), replace=TRUE)
        # 4. Predict this sub-model over testing data
        predictions   <- predict(rf_model, newdata = testing_data[, selected_features])
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
        predictions   <- as.vector(predict(lasso_model,
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
        predictions   <- predict(xgb_model, newdata = as.matrix(testing_data[, selected_features]))
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
        predictions   <- predict(boosted_glm_model, newdata = testing_data[, selected_features])
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
        fit           <- suppressWarnings(predict(boosted_gam_model, newdata = X_train_split))
        errors        <- fit - y_train_split$Outcome
        sample_errors <- sample(errors, size=nrow(testing_data), replace=TRUE)
        predictions   <- suppressWarnings(predict(boosted_gam_model, newdata = testing_data[, selected_features]))
        bootstrapped_predictions <- predictions + sample_errors
        
      }
      bootstrap_predictions[i, ] <- bootstrapped_predictions
    }
    
    # Calculate the 95% prediction interval for prediction
    lower_bound <- apply(bootstrap_predictions, 2, function(x) quantile(x, 0.025))
    upper_bound <- apply(bootstrap_predictions, 2, function(x) quantile(x, 0.975))
    
    lower_bound<-  lower_bound
    upper_bound<-  upper_bound
    mean_boostrap  <- rowMeans( bootstrap_predictions, na.rm = TRUE)
    
    return(list(lower=lower_bound,uper=upper_bound,mean=mean_boostrap))
    
  }, error = function(err){
    
    cat(paste("Error computing confidence intervals for ", country, " ", err, "\n"))
    cat("Will use std \n")
    uncertainty <-sd(prediction)
    lower_bound <-prediction - uncertainty
    upper_bound <-prediction + uncertainty
    return(list(lower_bound,upper_bound))
    
  })
  
}  