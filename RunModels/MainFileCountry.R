library(openxlsx)
library(readxl)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(randomForest)
library(glmnet)
library(cowplot)
library(Boruta) #Feature selection
library(boot)
library(xgboost)
library(grid)
library(IRdisplay)
library(cowplot)
library(boot)
library(caret)
library(mboost)
library(ranger)
library(gam)
library(parallel)

parentDir <- getwd()  # Make sure to choose the root directory as working directory

# === Import Functions ==================== #

# Models
source(paste(parentDir,"/Functions/Modeling/RandomForest.R", sep=""))
source(paste(parentDir,"/Functions/Modeling/Lasso.R", sep=""))
source(paste(parentDir,"/Functions/Modeling/XGBoost.R", sep=""))
source(paste(parentDir,"/Functions/Modeling/Boost_glm.R", sep=""))
source(paste(parentDir,"/Functions/Modeling/Boost_gam.R", sep=""))
source(paste(parentDir,"/Functions/Modeling/RFRanger.R", sep=""))


# Metrics Estimation MAE and RMED
source(paste(parentDir,"/Functions/Results/Est_Metrics.R", sep=""))

# Confident Intervals
source(paste(parentDir,"/Functions/Results/ConfidentIntervalGeneral.R", sep=""))

# Plot Results
source(paste(parentDir,"/Functions/Results/PlotResultsGeneral.R", sep=""))

# Get All Results
source(paste(parentDir,"/Functions/Results/GetResultsGeneral.R", sep=""))

####################################################
### Perform model fitting and save results #########
####################################################

set.seed(123)

Outcome_names <- c("SUICF", "SUICM", "CovIndex")

for (Outcome_name in Outcome_names) {
  
  cat("######################################################### \n")
  cat("######################################################### \n")
  cat(paste(Outcome_name, "Analysis \n"))
  cat("######################################################### \n")
  cat("######################################################### \n")
  
  ### Read Data ============================================= ###
  
  Imputed_data<- as.data.frame(read.xlsx(paste(parentDir,
                                               "/Data/Imputed_data/Final_imputed_data_All.xlsx", sep="")))
  
  Imputed_data$Indicator[
    which(Imputed_data$Indicator== Outcome_name)] <- "Outcome"
  
  if (Outcome_name == "CovIndex"){
    ind_suicf <- which(Imputed_data$Indicator=="SUICF")
    ind_suicm <- which(Imputed_data$Indicator=="SUICM")
    Imputed_data <-Imputed_data[-c(ind_suicf,ind_suicm),]
  }else{
    
    ind_CovIndex <-which(Imputed_data$Indicator=="CovIndex")
    ind_dma <- which(Imputed_data$Country=="DMA"|Imputed_data$Country=="KNA")
    if (Outcome_name=="SUICF"){
      ind_suic <- which(Imputed_data$Indicator=="SUICM")
    }else{
      ind_suic <- which(Imputed_data$Indicator=="SUICF")}
    Imputed_data <- Imputed_data[-c(ind_suic,ind_dma,ind_CovIndex),]
  }
  
  ind_redundant<- which(Imputed_data$Indicator%in%c("COV16","COV17",
                                                    "COV18","COV19",
                                                    "COV20","HEALTH16"))
  Imputed_data <- Imputed_data[-ind_redundant,]
  
  ### Fit Models ============================================= ###
  
  # Data_used <- Imputed_data %>% filter(Country%in%c("BRA"))
  Data_used <- Imputed_data
  
  # Identify the total countries in the file
  list_of_countries <- unique(Data_used$Country)
  
  list_of_figures           <-list()  # Save the prediction for each country
  list_of_relevant_var      <-list()  # Save selected variables
  list_of_final_prediction  <-list()
  info_df <- data.frame()
  list_of_metrics_by_country <- data.frame()
  
  for(country in list_of_countries){
    
    tryCatch({
      
      # Decide if you want to add hyper parameter tuning or
      # cross validation to the model fit
      
      hyper_tune <- TRUE
      cv         <- TRUE
      
      results_country <- get_results(country, "Country", Data_used,
                                     hyper_tune, cv, Outcome_name,
                                     use_all=FALSE, add_fit=TRUE)
      
      #general info
      info_df <- rbind(info_df, results_country$info_df_)
      
      #figures
      list_of_figures[[country]] <- results_country$figure_[[1]]
      
      #save metrics  
      final_metrics = results_country$metrics_ 
      final_metrics$Country = country
      
      list_of_metrics_by_country<-rbind(list_of_metrics_by_country, final_metrics)
      
      #save predictions to plot later
      list_of_final_prediction[[country]]<-results_country$figure_[[2]]
      
    }, error = function(err){
      
      cat(paste("Error computing results for ", country, " ", err, "\n"))
      
    })
    
  }
  
  ### Write results ========================================== ###
  
  today <- Sys.Date()
  # Create a list of plots using your figures
  plot_list <- lapply(names(list_of_figures), function(obj_name) {
    figure  <- list_of_figures[[obj_name]]
    # Add the title equal to the object's name
    figure +
      theme_bw() +
      labs(x = "", y = "", subtitle = obj_name)
  })
  # Arrange the plots in a grid using plot_grid
  grid_plot <- plot_grid(plotlist = plot_list, nrow = 9 , ncol = 4)
  
  # Display the grid of figures
  figure <- grid_plot
  ggsave(paste(parentDir, "/Data/Country_Results/All_Predictions_", Outcome_name,
               "_Country.pdf", sep=""), figure, width = 15, height = 20) 
  result_list <- list(
    "Model_Info"=info_df,
    "Metrics"   =list_of_metrics_by_country)
  
  write.xlsx(result_list, paste(parentDir, "/Data/Country_Results/Relevant_var_", 
                                Outcome_name, "_Country.xlsx", sep=""))
  
  write.xlsx(list_of_final_prediction,
             paste(parentDir, "/Data/Country_Results/Final_predictions_",
                   Outcome_name, "_Country.xlsx", sep=""))
}


