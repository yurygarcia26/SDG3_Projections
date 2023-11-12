get_results <- function(region, region_col, Data_used, hyper_tune, cv, Outcome_name){

  start_time <- Sys.time()
  cat(paste(" ==== ", region, " ==== \n"))

  # Reshape the file, each column equal to one indicator
  Data_by_region       <- Data_used%>%filter(!!as.symbol(region_col)==region)
  Data_by_region_wider <- Data_by_region %>% pivot_wider(names_from = Indicator, values_from = Value)

  # Step 2: Define training and testing data
  training_data <- Data_by_region_wider%>%filter(Year%in%2000:2015)
  testing_data  <- Data_by_region_wider%>%filter(Year%in%2016:2019)

  best_s <- 1

  # Step 3: Estimate predictions with each model
  # Add more models as needed ...

  model_names <- c("Random Forest", "Lasso", "XGBoost", "Boost_glm", "Boost_gam")

  # Random Forest
  rf_results    <- random_forest_function(region_col, training_data, testing_data, hyper_tune=hyper_tune, cv=cv)

  # Lasso
  Lasso_results <- Lasso_function(region_col, training_data, testing_data, hyper_tune=hyper_tune, cv=cv)
  
  # XGBoost (put inside capture output to avoid long logs)
  cat("Tunning XGBoost \n")
  xgboost_output <- capture.output(xgboost_results <- XGBoost_function(region_col, training_data, testing_data, hyper_tune=hyper_tune, cv=cv))
  cat("XGBoost done \n")

  # Boost glm
  Boost_glm_results <- Boost_glm_function(region_col, training_data, testing_data, hyper_tune=hyper_tune, cv=cv)

  # Random forest ranger for time series
  # rfranger_results <- rfRanger_function(region_col, training_data, testing_data, hyper_tune=hyper_tune, cv=cv)
  
  # GAM Boot
  cat("Tunning GAM Boost \n")
  gam_Boost_results <- Boost_gam_function(region_col, training_data, testing_data, hyper_tune=hyper_tune, cv=cv)
  cat("GAM Boost done \n")

  model_results_all <- list(rf_results, Lasso_results, xgboost_results, Boost_glm_results, gam_Boost_results)

  # Step 4: Estimate the metrics
  metrics_models <- data.frame("metrics"=c("MAE", "RMED"))
  for (i in c(1:length(model_results_all))) {
    metrics_models[paste("model", i, sep="")] = Est_Metrics_function(model_results_all[[i]]$Predictions)
  }

  # Step 5: Identify the best model
  min_column        <- apply(metrics_models, 1, which.min)[1] - 1
  final_method_name <- model_names[min_column]
  cat(paste("Best model: ", final_method_name, " \n"))
  
  model_results_use <- model_results_all[[min_column]]
  model_results     <- model_results_use$Predictions

  model_selected = model_results_use$model
  
  if(final_method_name == 'Lasso'){
    best_s = model_results_use$best_s # In case of lasso model
  }

  saveRDS(model_selected, paste("./Models/best_model_", region, "_", Outcome_name, ".rds", sep=""))

  # Save information by region
  info_df_region <- data.frame(
                              Total_Variables    = length(unique(Data_by_region$Indicator)), # Total initial variables
                              Relevant_Var       = model_results_use$features, # Selected variables
                              Selected_feat      = length(model_results_use$features),
                              method             = final_method_name)

  info_df_region[region_col] <- rep(c(region),each=dim(info_df_region)[1])
  info_df_region$best_s_lasso <- rep(c(best_s),each=dim(info_df_region)[1])

  tryCatch({
    info_df_region$Selected_feat_importance <- model_results_use$importances
  }, error = function(err){
    cat(paste("Error putting selected_feat_importance ", err, " Using", final_method_name, " \n"))
  })

  # Step 6: Compute Confident Intervals
  cat("Confidence Intervals computing ... \n")
  model_result_CI = CI_function(model_results, training_data, testing_data, info_df_region, region, region_col, best_s, Outcome_name)
  # Corrected CI_function
 
  # Step 7: Save figure
  method_selected = info_df_region$method[nrow(info_df_region)]
  cat("Plot computing ... \n")
  figure_region <- plot_function(Data_by_region_wider, model_results_use$Fit, model_result_CI, 
                                              method_selected, add_fit=TRUE)
  
  end_time <- Sys.time()
  cat(paste(region_col, " ", region, " took ", end_time-start_time, " minutes \n", sep=""))
  cat(" =============== \n")

  return(list(figure_=figure_region,
  info_df_=info_df_region,
  metrics_=metrics_models,
  model_result_CI=model_result_CI))

}