#### Create projections of Co-variables for period 2020-2030 ###################
#### Each outcome name uses a different set of covariables for each region #####
#### Final data is a Covariable_Projections_<<Outcome name>>_Region.xlsx file ##
#### with a sheet for the projection of the relevant vars for each region ######

library(openxlsx)
library(tidyr)
library(vars)
library(ggfortify)
library(forecast)
library(data.table)


get_data_all_regions <- function(value_col_to_use){
  Aggregate_Data_Use_All <- as.data.frame(
    read.xlsx("././Imputed_Data/Aggregate_Data_ALL_REGIONS.xlsx"))
  Aggregate_Data_Use_All <- Aggregate_Data_Use_All[, c("Year", "Region", "Indicator",
                                                       value_col_to_use)]
  colnames(Aggregate_Data_Use_All) <- c("Year", "Region", "Indicator", "Value")
  ind_suicf <- which(Aggregate_Data_Use_All$Indicator=="SUICF")
  ind_suicm <- which(Aggregate_Data_Use_All$Indicator=="SUICM")
  ind_CovIndex <-which(Aggregate_Data_Use_All$Indicator=="CovIndex")
  Aggregate_Data_Use_All <-Aggregate_Data_Use_All[-c(ind_suicf,ind_suicm, ind_CovIndex),]
  return (Aggregate_Data_Use_All)
}
Aggregate_Data_Use_All <- get_data_all_regions("WeightedValue")
# MedianValue, MeanValue

Covariable_Projection_Function <- function(Outcome_name, region){
  
  cat(paste("=> Using Region ", region, " <= \n"))
  file_region_res <- paste('././Figures/Region_Results/Relevant_var_',
                           Outcome_name, '_Region.xlsx', sep='')
  actual_data <- Aggregate_Data_Use_All[Aggregate_Data_Use_All$Region == region, ]
  
  data <- actual_data %>% pivot_wider(names_from = Indicator, values_from = Value)
  Years <- data$Year
  data <- subset(data, select = -c(Year, Region))
  
  # Find Co-variables from best model for Region:
  
  region_results <- as.data.frame(read.xlsx(file_region_res))
  region_results <- region_results[region_results$Region == region, ]
  covariables_region <- region_results$Relevant_Var
  
  data <- subset(data, select = covariables_region)
  rownames(data) <- Years
  
  tryCatch({
    
    # Estimate a VAR model with one lag
    ts.matrix <- ts(data, start=2000, end=2019, frequency = 1)
    var_model <- VAR(ts.matrix, p=1)
    fcst.tmp <- forecast(var_model)
    serial.test(var_model, lags.pt=10, type="PT.asymptotic")
    
    projection <- forecast(var_model, h=11)
    
    # Plot of projection
    projection_plot <- projection %>% autoplot() + 
      xlab("Year") + 
      ggtitle(paste("Projections of relevant variables for ",
                    Outcome_name, "at Region ",
                    region, "for 2020-2030.", sep=""))
    
    covars <- names(projection$forecast)
    projection_df <- data.frame()
    
    for(covar in covars){
      dat_ <- data.frame(projection$forecast[[covar]])
      dat_$Year <- rownames(dat_)
      dat_ <- dat_ %>%
        pivot_longer(
          cols = c("Point.Forecast", 
                   "Lo.80", 
                   "Hi.80", 
                   "Lo.95", 
                   "Hi.95"), 
          names_to = "Measure",
          values_to = "Value"
        )
      dat_$Indicator <- covar
      dat_$Region <- region
      projection_df <- rbind(projection_df, dat_)
    }
    
    return(list(projection_df = projection_df, projection_plot = projection_plot))
    
  }, error = function(err){
    
    cat(paste("ERROR computing projection for", region, "at", Outcome_name, ":", err, "! \n"))
    return(list(error=err))
    
  })
  
}

######### ==== Main ==== #########

Outcome_names <- c("SUICF", "SUICM", "CovIndex")
list_of_regions <- unique(Aggregate_Data_Use_All$Region)
for (Outcome_name in Outcome_names) {
  
  cat(paste("====== Projections for ", Outcome_name, "======= \n"))
  
  projection_df_list <- list()
  plot_list <- list()
  
  for (region in list_of_regions) {
    
    projection_results <- Covariable_Projection_Function(Outcome_name, region)
    projection_df_list[[region]] <- projection_results$projection_df
    plot_list[[region]] <- projection_results$projection_plot

  }
  write.xlsx(projection_df_list, paste("././Imputed_Data/Covariable_Projections",
                                  Outcome_name,
                                  "Region.xlsx", sep="_"))
}


# Testing:
Covariable_Projection_Function("SUICF", "Americas")

# ==== Covariate Projections Done ==== #


