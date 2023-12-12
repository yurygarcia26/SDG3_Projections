Plot_New_Projection<-function(country,Outcome,list_of_results,features,Selected_Region){

  #--------------------
  cat("Plot_New_Projection:\n")
  cat("country:\n")
  print(country)
  
  cat("\nOutcome:\n")
  print(Outcome)
  
  cat("\nSelected_Region:\n")
  print(Selected_Region)
  #-------------------------------------------------------
  
  
  #upload the final projections
  Data_by_country_predicted  <- as.data.frame(read.xlsx(paste("./Data/",Selected_Region,"_Results/Final_predictions_",Outcome,"_",Selected_Region,".xlsx",sep=""),sheet=country))
  
  #20% of data for tests
  test_years <- round(0.2*length(Data_by_country_predicted$Year))
  n = seq(length(Data_by_country_predicted$Year)-(test_years-1),length(Data_by_country_predicted$Year),1)
  
  
  Perturbation1 <- list_of_results$perturbation1
  Perturbation2 <- list_of_results$perturbation2
  
  Perturbation_df <- data.frame(
                      "Year"= Data_by_country_predicted$Year[n],
                      "perturbation1" = Perturbation1$prediction,
                      "lower_pert1"   = Perturbation1$lower,
                      "upper_pert1"   = Perturbation1$uper,
                      "perturbation2" = Perturbation2$prediction,
                      "lower_pert2"   = Perturbation2$lower,
                      "upper_pert2"   = Perturbation2$uper
                      
  )
  
  Data_perturbed <- merge(Data_by_country_predicted,Perturbation_df,by="Year",all=T)
  
  # Create the plot
  fig <- ggplot(Data_perturbed, aes(x = Year)) +
    geom_line(aes(y    = Outcome,color="Original"),  color = "blue", size = 0.5, 
              linetype = "solid")+
    geom_point(aes(y = Outcome), color = "blue", size = 2)+
    geom_line(aes(y  = Prediction,color="Prediction"), color = "red", size = 0.5, 
              linetype = "dashed") +
    geom_point(aes(y = Prediction), color = "red", size = 2)+
    geom_line(aes(y  = Fit,color="Fit"), color = "gray", size = 0.5, 
              linetype = "dashed")+
    scale_x_continuous(breaks = seq(2000, 2020, 2))+
    geom_line(aes(y    = perturbation2,color="%Decrease"),  color = "orange", size = 0.5, 
              linetype = "solid")+
    geom_point(aes(y = perturbation2), color = "orange", size = 2)+
    geom_line(aes(y    = perturbation1,color="%Increase"),  color = "darkgreen", size = 0.5, 
              linetype = "solid")+
    geom_point(aes(y = perturbation1), color = "darkgreen", size = 2)

    fig <- fig + geom_point(aes(y = Fit), color = "gray", size = 2)
  
    fig <- fig +
    theme_bw()+
    geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "gray", alpha = 0.3) +
    labs(x = "Year", y = "Value")+
    theme(legend.position = "top")+
    scale_color_manual(values=c("Original"="blue", "Prediction"="red", "Fit"="gray", "%Decrease"="orange", "%Increase"="darkgreen")) 
      
      
  
  return(fig) 
  
}