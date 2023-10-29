plot_function <- function(
      Data_by_region_wider,
      model_fit,
      model_results,
      method,
      add_fit){
  
    Data_by_region_predicted <- Data_by_region_wider
    # Create columns for predictions and uncertainty
    Data_by_region_predicted$Prediction  <- NA
    Data_by_region_predicted$Lower <- NA
    Data_by_region_predicted$Upper <- NA
    Data_by_region_predicted$Fit  <- NA

    # Fill the columns with predictions and uncertainty for the testing period
    Data_by_region_predicted$Prediction[Data_by_region_predicted$Year >= min(model_results$Year)] <- model_results$Prediction
    Data_by_region_predicted$Lower[Data_by_region_predicted$Year >= min(model_results$Year)] <- model_results$lower_bound
    Data_by_region_predicted$Upper[Data_by_region_predicted$Year >= min(model_results$Year)] <- model_results$upper_bound
    Data_by_region_predicted$Fit[Data_by_region_predicted$Year <= max(model_fit$Year)] <- model_fit$Fit
    Data_by_region_predicted$Year<-as.numeric(Data_by_region_predicted$Year)
    Data_by_region_predicted<-Data_by_region_predicted[order(Data_by_region_predicted$Year),]

    # Create the plot
    fig <- ggplot(Data_by_region_predicted, aes(x = Year)) +
      geom_line(aes(y    = Outcome),  color = "blue", size = 0.5, 
                linetype = "solid")+
      geom_point(aes(y = Outcome), color = "blue", size = 2)+
      geom_line(aes(y  = Prediction), color = "red", size = 0.5, 
                linetype = "dashed") +
      geom_point(aes(y = Prediction), color = "red", size = 2)+
      geom_line(aes(y  = Fit), color = "gray", size = 0.5, 
                linetype = "dashed")

    if (add_fit == TRUE){
      fig <- fig + geom_point(aes(y = Fit), color = "gray", size = 2)
    }
    fig <- fig +
      geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "gray", alpha = 0.3) +
      labs(x = "Year", y = "Value", title = paste("Model:", method))+
      theme_bw()

    return(list(fig,final_predictions_plot=Data_by_region_predicted))
  }