Forcast_indicator_scenario<-function(Outcome_name,Selected_Region,Selected_Country,perturbation_data){
  
  cat(paste("-----------------\n"))
  cat(paste("Computing new forcasting \n"))
  
  # Obtain a dataframe with aggregate data for all possible regions
  Aggregate_Data_Use_All <- as.data.frame(read.xlsx("./Data/Imputed_Data/Final_imputed_data_country_region.xlsx"))

  # 1. Read current Region + Model Results:
  
  model_info     <- as.data.frame(read.xlsx(paste("./Data/",
  Selected_Region,"_Results/Relevant_var_",
  Outcome_name,"_",Selected_Region,".xlsx",sep="")))
  if("Region" %in% colnames(model_info)){
    colnames(model_info)[which(names(model_info) == "Region")] <- "Country"
  }

  region_results <- model_info
  region_results <- region_results[region_results$Country == Selected_Country, ]
  method         <- region_results$method[1]
  best_s         <- region_results$best_s_lasso[1]
  relevant_var   <- region_results$Relevant_Var
  
  cat("Check 3.1 \n")

  folder_path = './Data/Forecasting/Models'
  file_name    <- paste0("best_model_", Selected_Country, "_", Outcome_name, ".rds")
  file_path    <- file.path(folder_path, file_name)
  Model_object <- readRDS(file_path)

  cat("Check 3.2 \n")
  
  # 2. Create Train Data:
  Aggregate_Data_Use_All$Indicator[
    which(Aggregate_Data_Use_All$Indicator== Outcome_name)] <- "Outcome"
  X_train <- Aggregate_Data_Use_All[Aggregate_Data_Use_All$Country == Selected_Country, ]
  X_train <- X_train[X_train$Indicator %in% c(relevant_var, "Outcome"), ]
  X_train <- X_train %>% pivot_wider(names_from = Indicator, values_from = Value)
  X_train_ <- subset(X_train, select = -c(Year, Country))

  cat("Check 3.3 \n")
  
  # 3. Create Test Data:
  covariate_projections_file <- paste("./Data/Forecasting/Projection_Covariables/Covariable_Projections_",
                                        Outcome_name,"_withDiff_", Selected_Region,".xlsx", sep="")

  projection_data <- as.data.frame(read.xlsx(covariate_projections_file,
                                             sheet = Selected_Country))
  
  cat("Check 3.4 \n")
    
  X_test = projection_data[projection_data$Measure == 'Point.Forecast',
                           c("Indicator", "Value", "Year") ]
  X_test <- X_test %>% pivot_wider(names_from = Indicator, values_from = Value)
  X_test_ <- subset(X_test, select = -c(Year))
  
  #change for perturbed variable
  n = nrow(X_test_)
  col_name = unique(perturbation_data$Indicator)
  X_test_[,col_name]<-tail(perturbation_data$Perturbation, n)
  
  cat("Check 3.5 \n")

  ##====================predictions=============================
  if (method == "Random Forest") {
    prediction = predict(Model_object, newdata = X_test_)
    
  } else if (method == "Lasso") {
    
    prediction <- as.vector(predict(Model_object, newx = as.matrix(X_test_), s = best_s))
    cat("Prediction")
    print(prediction)
    
  } else if (method == "XGBoost") {
    prediction <- predict(Model_object, newdata = as.matrix(X_test_))
    
    
  } else if (method == "Boost_glm") {
    prediction <- suppressWarnings(predict(Model_object, newdata = X_test_))
    
    
  } else if (method == "Boost_gam") {
    prediction   <- suppressWarnings(predict(Model_object, newdata = X_test_))
  }
  
  cat("Check 3.6 \n")

  #================================================================
  CI_results = CI_New_Projection(X_train,X_test,model_info,Selected_Country,Model_object,prediction,best_s)
  
  results=list(prediction=prediction,lower=CI_results$lower,
               uper=CI_results$uper,mean=CI_results$mean)
  

  return(results)
  
  }