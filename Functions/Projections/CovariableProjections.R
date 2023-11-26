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
library(MTS)
library(gridExtra)
library(ggplot2)
library(cowplot)

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

Covariable_Projection_Function <- function(Outcome_name, region, diff=TRUE){
  
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
  
  data <- data.frame(subset(data, select = covariables_region))
  rownames(data) <- Years
  
  tryCatch({
    
    # Estimate a VAR model with one lag
    ts.matrix <- ts(data, start=2000, end=2019, frequency = 1)

    if(diff==TRUE){
      # Obtain day difference in time series
      ts.matrix <- ts(diffM(ts.matrix), start=2000, end=2019, frequency = 1)
    }

    var_model <- vars::VAR(ts.matrix, p=1)
    cat("Forecast ===> \n")
    projection <- forecast(var_model, h=11)
    # serial.test(var_model, lags.pt=10, type="PT.asymptotic")
    cat("Forecast done \n")
    covars <- names(projection$forecast)

    # Plot of projection
    # Create it manually

    plots_list <- list()
    for (covar in covariables_region) {
      
      if(diff==TRUE){
        # Come back to non differentiated values
        last_value_real <- tail(data[covar], n=1)[1,1]
        dat_ <- cumsum(data.frame(projection$forecast[[covar]])) + last_value_real
      } else{
        dat_ <- data.frame(projection$forecast[[covar]])
      }

      dat_$Year<- rownames(dat_)
      dat_['Variable'] <- NA
      x0 <- data.frame(
        Year = rownames(data)
      )
      x0['Variable'] <- data[covar]
      x0["Point.Forecast"] <- NA
      x0["Lo.80"] <- NA
      x0["Hi.80"] <- NA
      x0["Lo.95"] <- NA
      x0["Hi.95"] <- NA
      x0 <- x0[, colnames(dat_)]
      dat_plot <- rbind(x0, dat_)
      
      # Create plot
      fig <- ggplot(dat_plot, aes(x = Year, group=1)) +
        geom_line(aes(y  = Variable), color = "blue", size = 0.75, 
                  linetype = "solid") +
        geom_point(aes(y = Variable), color = "blue", size = 2)+
        geom_line(aes(y    = Point.Forecast),  color = "red", size = 0.5, 
                  linetype = "dashed")+
        geom_point(aes(y = Point.Forecast), color = "red", size = 2)+
        geom_ribbon(aes(ymin = Lo.80, ymax = Hi.80), fill = "gray", alpha = 0.3) +
        geom_ribbon(aes(ymin = Lo.95, ymax = Hi.95), fill = "gray", alpha = 0.3) +
        labs(x = "Year", y = "Value") +
        theme_bw() + theme(axis.text.x = element_text(angle = 280)) + ggtitle(covar)
      plots_list[[covar]] <- fig
    }

    nrow_ = floor(sqrt(length(plots_list))) + 1
    ncol_ = floor(sqrt(length(plots_list)))
    projection_plot <- plot_grid(plotlist=plots_list, nrow=nrow_ , ncol=ncol_)

    # Projection Info:
    projection_df <- data.frame()
    
    for(covar in covars){
      
      if(diff==TRUE){
        # Come back to non differentiated values
        last_value_real <- tail(data[covar], n=1)[1,1]
        dat_ <- cumsum(data.frame(projection$forecast[[covar]])) + last_value_real
      } else{
        dat_ <- data.frame(projection$forecast[[covar]])
      }

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
    
    return(list(projection_df = projection_df,
                projection_plot = projection_plot,
                plots_list = plots_list))
    
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
  
  projection_df_list2 <- list()
  plot_list2 <- list()
  
  for (region in list_of_regions) {
    
    projection_results <- Covariable_Projection_Function(Outcome_name,
                                                         region,
                                                         diff = FALSE)
    projection_df_list[[region]] <- projection_results$projection_df
    plot_list[[region]] <- projection_results$projection_plot
    

    projection_results2 <- Covariable_Projection_Function(Outcome_name,
                                                         region,
                                                         diff = TRUE)
    projection_df_list2[[region]] <- projection_results2$projection_df
    plot_list2[[region]] <- projection_results2$projection_plot

  }
  
  write.xlsx(projection_df_list, paste("././Imputed_Data/Covariable_Projections",
                                  Outcome_name,
                                  "Region.xlsx", sep="_"))
  
  write.xlsx(projection_df_list2, paste("././Imputed_Data/Covariable_Projections",
                                       Outcome_name,
                                       "withDiff_Region.xlsx", sep="_"))
}

# Testing:
res <- Covariable_Projection_Function("CovIndex", "Americas", diff = FALSE)
# Testing:
res2 <- Covariable_Projection_Function("CovIndex", "Americas", diff = TRUE)

res$plots_list$ECON3
res2$plots_list$ECON3

# ==== Covariate Projections Done ==== #
