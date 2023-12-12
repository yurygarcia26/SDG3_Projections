Plot_variable<-function(country,Indicator_name,per,n_test){
 
  #----------------------------------------------------------------
  original_data   <- as.data.frame(read.xlsx("./Data/Imputed_Data/Final_imputed_data_country_region.xlsx"))
  filter_indicator<- original_data%>%filter(Country==country)%>%filter(Indicator==Indicator_name)
  filter_indicator$Perturbation1 <- filter_indicator$Value
  filter_indicator$Perturbation2 <- filter_indicator$Value

    
  #increase x percentage per year
  filter_indicator$Perturbation1[nrow(filter_indicator) - (n_test-1):0] <- filter_indicator$Value[nrow(filter_indicator) - (n_test-1):0]+filter_indicator$Value[nrow(filter_indicator) - (n_test-1):0]*per/100
  #decrease x percentage per year
  filter_indicator$Perturbation2[nrow(filter_indicator) - (n_test-1):0] <- filter_indicator$Value[nrow(filter_indicator) - (n_test-1):0]-filter_indicator$Value[nrow(filter_indicator) - (n_test-1):0]*per/100
  #filter_indicator$Perturbation2[filter_indicator$Perturbation2[nrow(filter_indicator) - (n_test-1):0]< 0] <- 0
  
  filter_indicator$Year <- as.numeric(filter_indicator$Year)
  
  plot_indicator<- ggplot(filter_indicator)+
    geom_line(aes(x=Year,  y=Perturbation1,group="Perturbation1",color="Scenario1"))+
    geom_point(aes(x=Year, y=Perturbation1,group="Perturbation1"),color="darkgreen")+
    geom_line(aes(x=Year,  y=Perturbation2,group="Perturbation2",color="Scenario2"))+
    geom_point(aes(x=Year, y=Perturbation2,group="Perturbation2"),color="Orange")+
    geom_line(aes(x=Year,  y=Value,group="Value",color="Original"))+
    geom_point(aes(x=Year, y=Value),color="darkblue")+
    theme_bw()+
    theme(legend.position = "top")+
    scale_x_continuous(breaks = seq(2000, 2020, 2))+
    labs(x="", y="Indicator value",title=Indicator_name)+
    scale_color_manual(name = "", values = c("Scenario1" = "darkgreen", "Scenario2" = "orange", "Original" = "darkblue"),
                       labels = c("Scenario 1", "Scenario 2", "Original"))
  
  return(list(plot_indicator,filter_indicator))
}

