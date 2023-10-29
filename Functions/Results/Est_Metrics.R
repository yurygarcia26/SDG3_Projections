Est_Metrics_function<-function(Data){
  
  true_values      <- Data$Outcome
  predicted_values <- Data$Prediction
  
  # Calculate RMSE (Root Mean Squared Error)
  rmse <- sqrt(mean((true_values - predicted_values)^2))
  
  # Calculate MAE (Mean Absolute Error)
  mae <- mean(abs(true_values - predicted_values))
  
  # Calculate R-squared (Coefficient of Determination)
  #ss_total <- sum((true_values - mean(true_values))^2)
  #ss_residual <- sum((true_values - predicted_values)^2)
  #r_squared <- 1 - (ss_residual / ss_total)
  metrics<-c(rmse,mae)
  return(metrics)
}