library(openxlsx)
library(readxl)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(randomForest)
library(glmnet)
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

######################################
# Read Data and Compute Aggregate Data
######################################

region_cols <- c("Ubication", "SubRegion", "RegionAll")
population_variable <- "SOC5"
Imputed_data_0 <- as.data.frame(
  read.xlsx(paste(parentDir,
                  "/Data/Imputed_data/Final_imputed_data_All_with_Regions.xlsx",
                  sep="")))

Aggregate_Data_Use_All <- data.frame()

for(region_column in region_cols){
  
  cat(paste("Aggregating by ", region_column, " ... \n"))
  
  Imputed_data <- Imputed_data_0[, c("Country",
                                     "Indicator",
                                     "Year",
                                     "Value",
                                     region_column)]
  
  if(region_column != "Region"){
    Imputed_data$Region <- Imputed_data[, c(region_column)]
  }
  
  Imputed_data <- Imputed_data[, c("Country",
                                   "Indicator",
                                   "Year",
                                   "Value",
                                   "Region")]
  
  Aggregate_Data <- data.frame()
  region_values <- unique(Imputed_data$Region)
  
  for (region_use in region_values) {
    
    cat(paste("Processing ", region_use, " ... \n"))
    
    # Population Data
    pop_data <- Imputed_data[Imputed_data$Indicator == "SOC5",
                             c("Country", "Year", "Value", "Region")]
    
    pop_data <- pop_data[pop_data$Region == region_use,
                         c("Country", "Year", "Value")]
    colnames(pop_data) <- c("Country", "Year", "Population")
    total_pop <- pop_data %>% group_by(Year) %>%
      summarise(TotalPop=sum(Population), .groups = 'drop')
    pop_data <- merge(pop_data, total_pop, by.x=c("Year"),
                      by.y=c("Year"), all.x=TRUE)
    pop_data$PopWeight <- pop_data$Population/pop_data$TotalPop
    
    var_data <- Imputed_data[Imputed_data$Indicator != "SOC5", ]
    var_data <- var_data[var_data$Region == region_use, ]
    variables_region <- unique(var_data$Indicator)
    
    Aggregate_Data_Region <- data.frame()
    
    for (var in variables_region) {
      
      # cat(paste("Processing ", var, " for ", region_use, " ... \n"))
      
      indicator_data <- var_data[which(var_data$Indicator == var), ]
      indicator_data <- merge(indicator_data, pop_data, by.x=c("Country", "Year"),
                              by.y=c("Country", "Year"), all.x=TRUE)
      
      indicator_data$WeightedValueRaw <- indicator_data$Value * indicator_data$PopWeight
      indicator_data <- na.omit(indicator_data)
      
      Aggregate_Data_Region_Indicator <- indicator_data %>% group_by(Year, Region) %>%
        summarise(MedianValue=median(Value),
                  WeightedValue=sum(WeightedValueRaw),
                  MeanValue=mean(Value), .groups = 'drop')
      
      Aggregate_Data_Region_Indicator$Indicator <- var
      
      Aggregate_Data_Region <- rbind(Aggregate_Data_Region, Aggregate_Data_Region_Indicator)
      
    }
    
    Aggregate_Data <- rbind(Aggregate_Data, Aggregate_Data_Region)
    
  }
  
  write.xlsx(Aggregate_Data, paste(parentDir, "/Data/Imputed_Data/Aggregate_Data/Aggregate_Data_",
                                   region_column, ".xlsx", sep=""))
  
  Aggregate_Data_Use_All <- rbind(Aggregate_Data_Use_All, Aggregate_Data)
  
}


write.xlsx(Aggregate_Data_Use_All,
           paste(parentDir,
                 "/Data/Imputed_Data/Aggregate_Data/Aggregate_Data_ALL_REGIONS.xlsx",
                 sep=""))

Aggregate_Data_Use_All <- as.data.frame(
  read.xlsx(paste(parentDir,
                  "/Data/Imputed_Data/Aggregate_Data/Aggregate_Data_ALL_REGIONS.xlsx",
                  sep="")))

#####################################
# Compute best models per region ####
#####################################

value_col_to_use <- "WeightedValue" # MedianValue, MeanValue
indicators <- c("SUICF") #, "CovIndex", "SUICM")

for (Outcome_name in indicators) {
  
  cat(" ========================================= \n ")
  cat(paste("Model creation for ", Outcome_name, "\n"))
  cat(" ========================================= \n ")
  
  Aggregate_Data_Use <- Aggregate_Data_Use_All[,
                                               c("Year", "Region",
                                                 "Indicator", value_col_to_use)]
  colnames(Aggregate_Data_Use) <-c("Year", "Region",
                                   "Indicator", "Value")
  
  Aggregate_Data_Use$Indicator[
    which(Aggregate_Data_Use$Indicator== Outcome_name)] <- "Outcome"
  
  if (Outcome_name == "CovIndex"){
    ind_suicf <- which(Aggregate_Data_Use$Indicator=="SUICF")
    ind_suicm <- which(Aggregate_Data_Use$Indicator=="SUICM")
    Aggregate_Data_Use <-Aggregate_Data_Use[-c(ind_suicf,ind_suicm),]
  }else{
    ind_CovIndex <-which(Aggregate_Data_Use$Indicator=="CovIndex")
    ind_dma <- which(Aggregate_Data_Use$Country=="DMA"|Aggregate_Data_Use$Country=="KNA")
    if (Outcome_name=="SUICF"){
      ind_suic <- which(Aggregate_Data_Use$Indicator=="SUICM")
    }else{
      ind_suic <- which(Aggregate_Data_Use$Indicator=="SUICF")}
    Aggregate_Data_Use <- Aggregate_Data_Use[-c(ind_suic,ind_dma,ind_CovIndex),]
  }
  
  set.seed(123)
  Data_used <- Aggregate_Data_Use
  
  # Identify the total regions in the file
  list_of_regions <- unique(Data_used$Region)            
  list_of_figures           <-list()  # Save the prediction for each region
  list_of_relevant_var      <-list()  # Save selected variables
  list_of_final_prediction  <-list()
  
  info_df <- data.frame()
  list_of_metrics_by_region <- data.frame()
  
  for(region in list_of_regions){
    
    tryCatch({
      
      # Decide if you want to add hyper parameter tuning or cross validation
      # to the model fit
      hyper_tune <- TRUE
      cv         <- TRUE
      
      results_region <- get_results(region, "Region", Data_used,
                                    hyper_tune, cv, Outcome_name,
                                    use_all=FALSE, add_fit=TRUE)
      
      # general info
      info_df <- rbind(info_df, results_region$info_df_)
      
      #figures
      list_of_figures[[region]] <- results_region$figure_[[1]]
      
      #save metrics  
      final_metrics = results_region$metrics_ 
      final_metrics$region = region
      
      list_of_metrics_by_region<-rbind(list_of_metrics_by_region,
                                       final_metrics)
      
      #save predictions to plot later
      list_of_final_prediction[[region]]<-results_region$figure_[[2]]
      
    }, error = function(err){
      cat(paste("Error computing results for", region, err, "\n"))
    })
    
  }
  
  # Create a list of plots using your figures
  plot_list <- lapply(names(list_of_figures), function(obj_name) {
    figure  <- list_of_figures[[obj_name]]
    # Add the title equal to the object's name
    figure +
      theme_bw() +
      labs(x = "", y = "", subtitle = obj_name)
  })
  
  ncol_ = 3
  nrow_ = 4
  
  # Arrange the plots in a grid using plot_grid
  grid_plot <- plot_grid(plotlist=plot_list, nrow=nrow_ , ncol=ncol_)
  
  # Display the grid of figures
  figure <- grid_plot
  ggsave(paste(parentDir, "/Data/Region_Results/All_Predictions_",
               Outcome_name, "_Region.pdf", sep=""),
         figure, width = 15, height = 20) 
  result_list <- list(
    "Model_Info"= info_df,
    "Metrics"   = list_of_metrics_by_region)
  
  write.xlsx(result_list, paste(parentDir, "/Data/Region_Results/Relevant_var_",
                                Outcome_name,
                                "_Region.xlsx", sep=""))
  
  write.xlsx(list_of_final_prediction,
             paste(parentDir, "/Data/Region_Results/Final_predictions_",
                   Outcome_name,
                   "_Region.xlsx", sep=""))
}


