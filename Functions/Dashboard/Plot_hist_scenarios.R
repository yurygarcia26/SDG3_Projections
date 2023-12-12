plot_hist_scenario<-function(list_of_results,country){
  
  #--------------------
  cat("--------------------------------------------")
  cat("plot_hist_scenario:\n")
  cat("country:\n")
  print(country)
  #-------------------------------------------------------
  
  
  Base_results          <-list_of_results$Base
  perturbation1_results <-list_of_results$perturbation1
  perturbation2_results <-list_of_results$perturbation2
  
  means_perturbation <- data.frame(
    "Base"      = Base_results$mean,
    "Scenario1" = perturbation1_results$mean,
    "Scenario2" = perturbation2_results$mean
  )
  
  mean_base          = mean(Base_results$prediction)
  mean_perturbation1 = mean(perturbation1_results$prediction)
  mean_perturbation2 = mean(perturbation2_results$prediction)
  
  # Assuming your data frame is named df
  df_long <- pivot_longer(means_perturbation, cols = c(Base, Scenario1, Scenario2), names_to = "Variable", values_to = "Value")
  
  plot_hist_means <- ggplot(df_long, aes(x = Value, fill = Variable)) +
    geom_histogram(binwidth = 0.01, position = "identity", alpha = 0.3) +
    geom_vline(xintercept = mean_base, color = "red", size = 1, linetype = "dashed") +
    geom_vline(xintercept = mean_perturbation1, color = "darkgreen", size = 1, linetype = "dashed") +
    geom_vline(xintercept = mean_perturbation2, color = "orange", size = 1, linetype = "dashed") +
    scale_fill_manual(values = c("Base" = "red", "Scenario1" = "darkgreen", "Scenario2" = "orange")) +
    labs(x = "Values", y = "Frequency") +
    facet_wrap(~Variable, scales = "free") +
    theme_bw() +
    theme(legend.position = "top", strip.background = element_rect(fill = "white"), strip.text = element_text(face = "bold")) +
    scale_y_continuous(expand = c(0, 0))
  
  
  
  #===========================|DELTA PLOT|============================
  
  DELTA_df <- data.frame(
    "Scenarios"    = c("scenario1", "scenario2"),
    "Country"      = c(country,country),
    "DELTAK"       = c(mean_perturbation1-mean_base,
                       mean_perturbation2-mean_base),
    "Delta_kappa1" = c(100*(mean_perturbation1-mean_base)/mean_base,                  
                       100*(mean_perturbation2-mean_base)/mean_base)
    
  )
  
  # Plot the bar chart
  # Create a new column for fill color based on the sign of DELTAK
 DELTA_df$color <- ifelse(DELTA_df$DELTAK >= 0, "Scenario 1","Scenario 2")
  
  # Plot the bar chart
  plot_delta <- ggplot(DELTA_df, aes(x = Country, y = DELTAK, fill = color)) +
    scale_fill_manual(values = c("Scenario 1" ="darkgreen" , "Scenario 2" ="orange")) +
    geom_col() +
    labs(title = "",
         x = "",
         y = "DK",
         fill = "")+
    theme(legend.position="")+
    theme_bw()
  
  
  #PLOT % DELTA ==========================================================
  # Create a new column for fill color based on the sign of DELTAK
  DELTA_df$color1 <- ifelse(DELTA_df$Delta_kappa1 >= 0, "Scenario 1", "Scenario 2")
  
  # Plot the bar chart
  Histo_delta <- ggplot(DELTA_df, aes(x = Scenarios, y = abs(Delta_kappa1), fill = color1)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("Scenario 1" ="darkgreen" , "Scenario 2" ="orange")) +
    geom_text(aes(label = paste(round(Delta_kappa1, digits = 2), "%"), y = abs(Delta_kappa1)), position = position_dodge(width = 0.9),  
              vjust = 0.4, 
              size  = 5)+ 
    labs(title = "",
         x = "",
         y = "",
         fill = "")+
    theme(legend.position="top")+
    theme_bw()
  
  
 return(list(plot_hist_means=plot_hist_means,plot_delta=plot_delta, plot_per_delta=Histo_delta))
}