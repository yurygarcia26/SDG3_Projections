library(openxlsx)
library(readxl)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(randomForest)
library(glmnet)
library(cowplot)
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
library(tidyr)
library(vars)
library(ggfortify)
library(forecast)
library(data.table)

source("././Functions/Results/ConfidentIntervalGeneral.R")

###################################################################
## Obtain Forecast for Outcome variable for 2020-2030 #############
# Returns dataframe with re-trained model fit and prediction ######
# As well as a plot with time series from 2000 to 2030 indicating #
# the prediction, fit and real series values. #####################
###################################################################


# Obtain a dataframe with aggregate data for all possible regions
get_data_all_regions <- function(value_col_to_use){
  Aggregate_Data_Use_All <- as.data.frame(
    read.xlsx("././Imputed_Data/Aggregate_Data_ALL_REGIONS.xlsx"))
  Aggregate_Data_Use_All <- Aggregate_Data_Use_All[, c("Year", "Region", "Indicator",
                                                       value_col_to_use)]
  colnames(Aggregate_Data_Use_All) <- c("Year", "Region", "Indicator", "Value")
  return (Aggregate_Data_Use_All)
}

Forecast_Function <- function(Outcome_name, region, diff=FALSE){
  
  # 1. Read current Region + Model Results:
  file_region_res <- paste('././Figures/Region_Results/Relevant_var_',
                           Outcome_name, '_Region.xlsx', sep='')
  region_results <- as.data.frame(read.xlsx(file_region_res))
  region_results <- region_results[region_results$Region == region, ]
  method <- region_results$method[1]
  best_s <- region_results$best_s_lasso[1]
  relevant_var <- region_results$Relevant_Var
  
  folder_path = '././Models'
  file_name <- paste0("best_model_", region, "_", Outcome_name, ".rds")
  file_path <- file.path(folder_path, file_name)
  model     <- readRDS(file_path)
  
  # 2. Create Train Data:
  
  Aggregate_Data_Use_All <- get_data_all_regions("WeightedValue")
  # MedianValue, MeanValue
  Aggregate_Data_Use_All$Indicator[
    which(Aggregate_Data_Use_All$Indicator== Outcome_name)] <- "Outcome"
  X_train <- Aggregate_Data_Use_All[Aggregate_Data_Use_All$Region == region, ]
  X_train <- X_train[X_train$Indicator %in% c(relevant_var, "Outcome"), ]
  X_train <- X_train %>% pivot_wider(names_from = Indicator, values_from = Value)
  X_train_ <- subset(X_train, select = -c(Year, Region))
  
  # 3. Create Test Data:
  
  if(diff == TRUE){
    covariate_projections_file <- paste("././Imputed_Data/Covariable_Projections",
                                        Outcome_name,
                                        "withDiff_Region.xlsx", sep="_")
  }else{
    covariate_projections_file <- paste("././Imputed_Data/Covariable_Projections",
                                        Outcome_name,
                                        "Region.xlsx", sep="_")
  }
  
  
  projection_data <- as.data.frame(read.xlsx(covariate_projections_file,
                                             sheet = region))
  X_test = projection_data[projection_data$Measure == 'Point.Forecast',
                           c("Indicator", "Value", "Year") ]
  X_test <- X_test %>% pivot_wider(names_from = Indicator, values_from = Value)
  X_test_ <- subset(X_test, select = -c(Year))
  
  
  # Re train model and Predict
  
  training_data <- X_train
  testing_data <- X_test
  selected_features <- relevant_var
  pred_results <- bounds_computation(model, method, 
                                     training_data, testing_data, 
                                     selected_features, 1, 1, best_s)
  fit = pred_results$fit
  predictions = pred_results$predictions
  
  # Compute Confidence Intervals for Predictions
  
  CI_results <- bounds_computation(model, method, 
                                   training_data, testing_data, 
                                   selected_features, 500, 0.8, best_s)
  
  # Save predictions
  real_df <- data.frame(
    "Year" = X_train$Year,
    "Value" = X_train$Outcome
  )
  predictions_df <- data.frame(
    "Year"= X_test$Year,
    "Prediction" = predictions
  )
  fit_df <- data.frame(
    "Year" = X_train$Year,
    "Fit" = fit
  )
  ci_df <- data.frame(
    "Year" = X_test$Year,
    "Lower" = CI_results$lower_bound,
    "Upper" = CI_results$upper_bound
  )
  results_df = merge(x=real_df, y=fit_df, by="Year", all.x=TRUE)
  results_df = merge(x=results_df, y=predictions_df, by="Year", all.y=TRUE, all.x = TRUE)
  results_df = merge(x=results_df, y=ci_df, by="Year", all.y=TRUE, all.x = TRUE)
  
  # Create plot
  fig <- ggplot(results_df, aes(x = Year, group=1)) +
    geom_line(aes(y  = Value), color = "blue", size = 0.75, 
              linetype = "solid") +
    geom_point(aes(y = Value), color = "blue", size = 2)+
    geom_line(aes(y  =Fit), color = "gray", size = 0.5, 
              linetype = "solid") +
    geom_point(aes(y = Fit), color = "gray", size = 2)+
    geom_line(aes(y    = Prediction),  color = "red", size = 0.5, 
              linetype = "dashed")+
    geom_point(aes(y = Prediction), color = "red", size = 2)+
    geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "gray", alpha = 0.3) +
    labs(x = "Year", y = "Value", title = paste("Model: ", method, ". ", 
                                                "Forecast for 2020-2030 for ",
                                                region, " (", Outcome_name, ").",
                                                " Diff = ", diff,
                                                sep="")) +
    theme_bw() + theme(axis.text.x = element_text(angle = 280))
  
  forecast_results <- list(
    forecast_df = results_df,
    forecast_figure = fig
  )
  
  return(forecast_results)
  
}

######### ==== Main ==== #########

Outcome_name <- "CovIndex"
region <- "Americas"
forecast_results <- Forecast_Function(Outcome_name, region, diff=TRUE)
forecast_results$forecast_df
forecast_results$forecast_figure
