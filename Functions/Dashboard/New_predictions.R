new_projection<-function(Outcome_name,country,perturbation,Variable,Model_object,model_info,best_s){
  
  
  #--------------------
  cat("--------------------------------------------")
  cat("new_projection:\n")
  cat("country:\n")
  print(country)
  
  cat("\nOutcome_name:\n")
  print(Outcome_name)
  
  cat("\nperturbation:\n")
  print(perturbation)
  
  cat("\nVariable:\n")
  print(Variable)
  #-------------------------------------------------------
  
  
  #Most relevant variables
  country_info      <- model_info%>%filter(Country==country)
  method            <- unique(country_info$method)
  selected_features <- country_info$Relevant_Var
  
  #Original Dataset
  Imputed_data<-as.data.frame(read.xlsx("./Data/Imputed_Data/Final_imputed_data_country_region.xlsx"))
  
  #Remove redundant-----------------------------------------------
  ind_redundant<- which(Imputed_data$Indicator%in%c("COV16","COV17","COV18","COV19","COV20","HEALTH16"))
  if(length(ind_redundant)!=0){
  Imputed_data <- Imputed_data[-ind_redundant,]
  }
  #----------------------------------------------------------------
  ##Original basedata
  Imputed_data$Indicator[which(Imputed_data$Indicator==Outcome_name)]<- "Outcome"
  
  if (Outcome_name == "CovIndex"){
    ind_suicf    <- which(Imputed_data$Indicator=="SUICF")
    ind_suicm    <- which(Imputed_data$Indicator=="SUICM")
    Imputed_data <-Imputed_data[-c(ind_suicf,ind_suicm),]
  }else{
    ind_CovIndex <-which(Imputed_data$Indicator=="CovIndex")
    ind_dma      <-which(Imputed_data$Country=="DMA"|Imputed_data$Country=="KNA")
    
    if (Outcome_name=="SUICF"){
      ind_suic <- which(Imputed_data$Indicator=="SUICF")
    }else{
      ind_suic <- which(Imputed_data$Indicator=="SUICM")
    }
    Imputed_data <- Imputed_data[-c(ind_suic,ind_CovIndex,ind_dma),]
  }
  
  # Reshape the file, each column equal to one indicator
  Data_by_country       <- Imputed_data%>%filter(Country==country)%>%filter(Indicator%in%c("Outcome",selected_features))
  Data_by_country_wider <- Data_by_country%>% pivot_wider(names_from = Indicator, values_from = Value)
  
  #change the column with the new variable
  Data_by_country_wider[Variable]<-perturbation
  
  # Step 2: Define training and testing data
  training_data <- Data_by_country_wider%>%filter(Year%in%2000:2015)
  testing_data  <- Data_by_country_wider%>%filter(Year%in%2016:2019)
  
  y_train <- training_data$Outcome
  X_train <- subset(training_data, select = -c(Country, Year, Outcome))  # Exclude the "Outcome" column
  
  y_test <- testing_data$Outcome
  X_test <- subset(testing_data, select = -c(Country, Year, Outcome))  # Exclude the "Outcome" column
  
  ##====================predictions=============================
  # Train and make predictions for each method
  if (method == "Random Forest") {
      prediction = predict(Model_object, newdata = X_test)
      
  } else if (method == "Lasso") {
    
    prediction <- as.vector(predict(Model_object, newx = as.matrix(X_test), s = best_s))
    

  } else if (method == "XGBoost") {
    prediction <- predict(Model_object, newdata = as.matrix(X_test))
   
    
  } else if (method == "Boost_glm") {
    prediction <- suppressWarnings(predict(Model_object, newdata = X_test))
   
    
  } else if (method == "Boost_gam") {
    prediction   <- suppressWarnings(predict(Model_object, newdata = X_test))
  }
  
  #================================================================
  CI_resutls=CI_New_Projection(training_data,testing_data,model_info,country,Model_object,prediction,best_s)
  
  results=list(prediction=prediction,lower=CI_resutls$lower,
               uper=CI_resutls$uper,mean=CI_resutls$mean)

  
  
  return(results)
}