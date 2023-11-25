get_results <- function(region, region_col,
Data_used, hyper_tune, cv, Outcome_name, ..., use_all=FALSE, add_fit=TRUE){

  start_time <- Sys.time()
  cat(paste(" ==== ", region, " ==== \n"))

  # Reshape the file, each column equal to one indicator
  Data_by_region       <- Data_used%>%filter(!!as.symbol(region_col)==region)
  Data_by_region_wider <- Data_by_region %>% pivot_wider(names_from = Indicator, values_from = Value)

  # Step 2: Define training and testing data

  if (use_all){
    # For projections to 2030
    training_data <- Data_by_region_wider%>%filter(Year%in%2000:2019)
    testing_data  <- Data_by_region_wider%>%filter(Year%in%2000:2019)
  } else {
    training_data <- Data_by_region_wider%>%filter(Year%in%2000:2015)
    testing_data  <- Data_by_region_wider%>%filter(Year%in%2016:2019)
  }

  best_s <- 1

  # Step 3: Estimate predictions with each model
  # Add more models as needed ...

  if(use_all){
    model_names <- c("Random Forest", "Lasso", "Boost_glm", "Boost_gam")
  }else{
    model_names <- c("Random Forest", "Lasso", "XGBoost", "Boost_glm", "Boost_gam")
  }

  # Random Forest
  rf_results    <- random_forest_function(region_col, training_data, testing_data, hyper_tune=hyper_tune, cv=cv)

  # Lasso
  Lasso_results <- Lasso_function(region_col, training_data, testing_data, hyper_tune=hyper_tune, cv=cv, use_all=use_all)
  
  # XGBoost (put inside capture output to avoid long logs)

  if(use_all == FALSE){
    cat("Tunning XGBoost \n")
    xgboost_output <- capture.output(xgboost_results <- XGBoost_function(region_col, training_data, testing_data, hyper_tune=hyper_tune, cv=cv))
    cat("XGBoost done \n")
  }

  # Boost glm
  Boost_glm_results <- Boost_glm_function(region_col, training_data, testing_data, hyper_tune=hyper_tune, cv=cv)

  # Random forest ranger for time series
  # rfranger_results <- rfRanger_function(region_col, training_data, testing_data, hyper_tune=hyper_tune, cv=cv)
  
  # GAM Boot
  cat("Tunning GAM Boost \n")
  gam_Boost_results <- Boost_gam_function(region_col, training_data, testing_data, hyper_tune=hyper_tune, cv=cv)
  cat("GAM Boost done \n")

  if(use_all){
      model_results_all <- list(rf_results, Lasso_results, Boost_glm_results, gam_Boost_results)
  } else{
      model_results_all <- list(rf_results, Lasso_results, xgboost_results, Boost_glm_results, gam_Boost_results)
  }

  # Step 4: Estimate the metrics
  metrics_models <- data.frame("metrics"=c("MAE", "RMSE"))
  for (i in c(1:length(model_results_all))) {
    metrics_models[paste("model", i, sep="")] = Est_Metrics_function(model_results_all[[i]]$Predictions)
  }

  # Step 5: Identify the best model
  min_column        <- apply(subset(metrics_models, select=-c(metrics)), 1, which.min)[[2]]
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
  info_df_region <- data.frame(Relevant_Var = model_results_use$features)  # Selected variables

  info_df_region$Total_Variables <- rep(c(length(unique(Data_by_region$Indicator))), each=dim(info_df_region)[1]) # Total initial variables
  info_df_region$Selected_feat <- rep(c(length(model_results_use$features)), each=dim(info_df_region)[1]) # Selected variables
  info_df_region$method <- rep(c(final_method_name), each=dim(info_df_region)[1])
  info_df_region[region_col] <- rep(c(region), each=dim(info_df_region)[1])
  info_df_region$best_s_lasso <- rep(c(best_s), each=dim(info_df_region)[1])

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
                                              method_selected, add_fit=add_fit)
  
  end_time <- Sys.time()
  cat(paste(region_col, " ", region, " took ", end_time-start_time, " minutes \n", sep=""))
  cat(" =============== \n")

  return(list(figure_=figure_region,
  info_df_=info_df_region,
  metrics_=metrics_models,
  model_result_CI=model_result_CI))

}