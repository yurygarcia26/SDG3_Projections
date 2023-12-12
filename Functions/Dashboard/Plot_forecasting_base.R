Plot_forecasting_base<-function(Selected_Region2,Selected_Country2,Outcome_name2,scenario,Diff){
  
  path<-paste("./Data/Forecasting/",Selected_Region2,"_Results/","Forecasts_",Outcome_name2,"_",Selected_Region2,".xlsx",sep="")
  forecasting_data     <- as.data.frame(read.xlsx(path,sheet=Selected_Country2))
  forecasting_data$Fit <- as.numeric(as.character(forecasting_data$Fit))


  if(Diff==FALSE){
  fig2<-ggplot(forecasting_data, aes(x = Year)) +
    geom_line(aes(y = Value, group = 1, color = "Original"), linetype = "solid") +
    geom_point(aes(y = Value, color = "Original"), size = 2) +
    geom_line(aes(y = Prediction, group = 1, color = "Projection"), linetype = "dashed") +
    geom_point(aes(y = Prediction, color = "Projection"), size = 2) +
    geom_line(aes(y = Fit, group = 1, color = "Model Fit"), linetype = "dashed") +
    geom_point(aes(y = Fit, color = "Model Fit"), size = 2) +
    geom_ribbon(aes(ymin = Lower, ymax = Upper,group = 1), fill = "red", alpha = 0.1) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90))+
    labs(x = "", y = Outcome_name2, title =  Selected_Country2 ) +
      scale_color_manual(
      name = NULL,
      values = c("Original" = "blue", "Projection" = "red", "Model Fit" = "gray"),
      labels = c("Original", "Projection", "Model Fit")) +
  guides(fill = guide_legend(title = NULL))  # Set legend title for fill to NULL
  
  return(fig2)
  }else{
    
    
    forecasting_data_perturbed <- forecasting_data
    forecasting_data_perturbed$Prediction_perturbed<-NA
    forecasting_data_perturbed$Lower_perturbed<-NA
    forecasting_data_perturbed$Upper_perturbed<-NA
    
    # Replace the last positions
    last_positions <- (length(forecasting_data_perturbed$Prediction_perturbed) - (length(scenario$prediction)-1)):length(forecasting_data_perturbed$Prediction_perturbed)
    forecasting_data_perturbed$Prediction_perturbed[last_positions] <- scenario$prediction
    forecasting_data_perturbed$Lower_perturbed[last_positions] <- scenario$lower
    forecasting_data_perturbed$Upper_perturbed[last_positions] <- scenario$uper
    
    #compute the reduction or increase
    mean_scenario<-round(mean(scenario$prediction),digits=2)
    mean_base    <-round(mean(forecasting_data_perturbed$Prediction[last_positions]),digits=2)
    Difference   <-round(mean_scenario-mean_base,digits = 2)
    DeltaK       = round(100*(mean_scenario-mean_base)/mean_base,digits=2)
    
    y_max <- round(max(forecasting_data_perturbed$Value,na.rm=TRUE))+2
    
    #Plot resutls
    fig3<-ggplot(forecasting_data_perturbed, aes(x = Year)) +
      geom_line(aes(y = Value, group = 1, color = "Original"), linetype = "solid") +
      geom_point(aes(y = Value, color = "Original"), size = 1) +
      geom_line(aes(y = Prediction, group = 1, color = "Projection (base)"), linetype = "dashed") +
      geom_point(aes(y = Prediction, color = "Projection (base)"), size = 1) +
      geom_line(aes(y = Prediction_perturbed, group = 1), linetype = "dashed") +
      geom_point(aes(y = Prediction_perturbed, color = "Projection (scenario)"), size = 1) +
      theme_bw() +
      geom_ribbon(aes(ymin = Lower, ymax = Upper,group = 1), fill = "red", alpha = 0.1) +
      geom_ribbon(aes(ymin = Lower_perturbed, ymax = Upper_perturbed,group = 1), fill = "orange", alpha = 0.1) +
      theme(axis.text.x = element_text(angle = 90)) +
      labs(x = "", y = Outcome_name2, title =  Selected_Country2 )+
      scale_color_manual(
        name = NULL,
        values = c("Original" = "blue", "Projection (base)" = "red", "Projection (scenario)" = "orange"),
        labels = c("Original", "Projection", "Projection perturbed")) +
      guides(fill = guide_legend(title = NULL))+
      geom_text(aes(x = forecasting_data_perturbed$Year[8], y = y_max, label = paste("\nK_base=",mean_base,"\nK_scenario=",mean_scenario,"\nDeltaK=",Difference,"\n% DeltaK=",DeltaK)), 
                vjust = -1, hjust = 0, color = "black", size = 4) 
    
     #geom_rect(aes(xmin = forecasting_data_perturbed$Year[6-4], xmax = forecasting_data_perturbed$Year[6+7], ymin = y_max-1, ymax = y_max+0.6), 
     #fill = "lightgray",color = "black", alpha = 0.3,size = 0.1)

    
    return(fig3)
    
    
  }
}