Plot_projected_covariables<-function(outcome_name,Selected_Country,Selected_Region,variable,per){
  
  data_projections <- as.data.frame(read.xlsx(paste("./Data/Forecasting/Projection_Covariables/Covariable_Projections_",outcome_name,"_withDiff_Region.xlsx",sep=""),sheet=Selected_Country))
  actual_data      <- as.data.frame(read.xlsx(paste("./Data/Imputed_Data/Final_imputed_data_country_region.xlsx"))) 
  data             <- actual_data%>%pivot_wider(names_from = Indicator, values_from = Value)
  
  colnames(data_projections)[colnames(data_projections) == "Region"]<- "Country"
  
  #create df------------------------------------------------------------------
  crate_df <- function(covar){
 
    dat_ = data_projections%>%filter(Indicator==covar)%>%pivot_wider(names_from = Measure, values_from = Value) 
    data_country<-data%>%filter(Country==Selected_Country)
    dat_$Country  <-NULL
    dat_$Variable <-NA
    
    x0 <- data.frame(
      Year = data_country$Year
    )
    x0['Variable']       <- data_country[covar]
    x0["Point.Forecast"] <- NA
    x0["Indicator"]<-unique(dat_$Indicator)
    x0["Lo.80"] <- NA
    x0["Hi.80"] <- NA
    x0["Lo.95"] <- NA
    x0["Hi.95"] <- NA
    x0 <- x0[, colnames(dat_)]
    
    dat_plot <- rbind(x0, dat_)
    return(dat_plot)
  }
  
  
  #---create figure to return---------------------------------------------------
  if(variable==0){
    list_of_covariables<-data_projections%>%filter(Country==Selected_Country)%>%dplyr::select(Indicator)%>%distinct()
    plots_list<-list()
    
    for(covar in list_of_covariables$Indicator){
      dat_plot<-crate_df(covar)
      
      # Create plot
      fig <- ggplot(dat_plot, aes(x = Year, group=1)) +
        geom_line(aes(y  = Variable,text=Indicator), color = "blue", size = 0.75, 
                  linetype = "solid") +
        geom_point(aes(y = Variable,text=Indicator), color = "blue", size = 2)+
        geom_line(aes(y    = Point.Forecast),  color = "red", size = 0.5, 
                  linetype = "dashed")+
        geom_point(aes(y = Point.Forecast), color = "red", size = 2)+
        geom_ribbon(aes(ymin = Lo.80, ymax = Hi.80), fill = "gray", alpha = 0.3) +
        geom_ribbon(aes(ymin = Lo.95, ymax = Hi.95), fill = "gray", alpha = 0.3) +
        theme_bw() + theme(axis.text.x = element_text(angle = 90))
      
        plots_list[[covar]] <- fig
    }
    return(plots_list)
    
  }else{
    
    dat_plot      <-crate_df(variable)
    original_Data <-dat_plot%>%dplyr::select(Year,Indicator,Point.Forecast,Variable)
   

    original_Data$Perturbation <- original_Data$Point.Forecast 
    
    #increase/decrease x percentage per year until 2030
    original_Data$Perturbation <-original_Data$Perturbation + original_Data$Perturbation*per/100
    original_Data$Perturbation[is.na(original_Data$Perturbation)] <-original_Data$Variable[!is.na(original_Data$Variable)]

    original_Data$Year <- as.numeric(original_Data$Year)
    
    plot_indicator<- ggplot(original_Data)+
      geom_line(aes( x=Year, y=Perturbation,group="Perturbation", color="Scenario"))+
      geom_point(aes(x=Year, y=Perturbation), color="orange")+
      geom_line(aes( x=Year, y=Variable,group="Variable",color="Original"))+
      geom_point(aes(x=Year, y=Variable),color="darkblue")+
      geom_line(aes( x=Year, y=Point.Forecast,group="Point.Forecast",color="Projection"))+
      geom_point(aes(x=Year, y=Point.Forecast),color="red")+
      theme_bw()+
      theme(legend.position = "top")+
      labs(x="", y="Covariable value",title=variable)+
      scale_color_manual(
        name = "",
        values = c( "Projection"="red","Original" = "darkblue","Scenario" = "orange"),
        labels = c( "Projection","Original", "Scenario"))+
      theme(axis.text.x = element_text(angle = 90))+
      scale_x_continuous(breaks = unique(original_Data$Year))
    
    original_Data$Point.Forecast[is.na(original_Data$Point.Forecast)]<-original_Data$Variable[!is.na(original_Data$Variable)]
    
    
    return(list(plot_indicator,original_Data))
  }
  
  
}