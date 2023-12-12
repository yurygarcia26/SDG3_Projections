Projection_info<-function(country, Outcome, Selected_Region){
  
  #upload the final projections
  Data_by_country_predicted  <- as.data.frame(read.xlsx(paste("./Data/",
  Selected_Region,"_Results/Final_predictions_",Outcome,"_",Selected_Region,".xlsx",sep=""),sheet=country))
  
  #List with the most relevant variables for each country
  models_info <- as.data.frame(read.xlsx(paste("./Data/",
  Selected_Region,"_Results/Relevant_var_", Outcome,"_", Selected_Region,".xlsx",sep="")))

  if("Region" %in% colnames(models_info)){
    colnames(models_info)[which(names(models_info) == "Region")] <- "Country"
  }
  
  #list with all initial variables
  all_var_list <- as.data.frame(read.xlsx("./Data/Imputed_Data/List_of_final_variables.xlsx"),sheet="All")

  ##--------------------------------------------------------
  #FILTER COUNTRY INFO
  ##--------------------------------------------------------
  
  country_features         <-models_info%>%filter(Country==country)%>%dplyr::select(Relevant_Var)
  Table_features           <-all_var_list%>%filter(VAR%in%country_features$Relevant_Var)
  colnames(Table_features) <-c("Feature","Name")
  Table_features           <-Table_features[order(Table_features$Feature),]
  
  
  ##----------------FIGURE---------------------------------
  method   <- unique(models_info$method[models_info$Country==country])
  add_fit  <- TRUE
  
  # Create the plot
  fig <- ggplot(Data_by_country_predicted, aes(x = Year)) +
    geom_line(aes(y    = Outcome),  color = "blue", size = 0.5, 
              linetype = "solid")+
    geom_point(aes(y = Outcome), color = "blue", size = 2)+
    geom_line(aes(y  = Prediction), color = "red", size = 0.5, 
              linetype = "dashed") +
    geom_point(aes(y = Prediction), color = "red", size = 2)+
    geom_line(aes(y  = Fit), color = "gray", size = 0.5, 
              linetype = "dashed")+
    scale_x_continuous(breaks = seq(2000, 2020, 2))
    
    if (add_fit == TRUE){
      fig <- fig + geom_point(aes(y = Fit), color = "gray", size=2)
    }
    fig <- fig +
      geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "gray", alpha = 0.3) +
      labs(x = "Year", y = "Value", title=country,subtitle = paste("Model:", method))+
      theme_bw()
    
  return(list(Table=Table_features,Figure=fig)) 

}