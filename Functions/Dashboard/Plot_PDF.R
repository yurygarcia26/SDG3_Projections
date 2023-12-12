plot_pdp_function<-function(Model_object,country_features,Table_features){
  
  cat("--------------------------------------------")
  cat("plot_pdp_function:\n")
  cat("country_features:\n")
  print(country_features)

  
  
  #=================function to create each plot=========#
  plot_PDP_function<-function(Data,feature_name,feature_def,name){
    wrapped_subtitle <- str_wrap(feature_def, width = 27)  # Adjust the width as needed
    
    figure<-ggplot(Data)+
      geom_line(aes(y=yhat,x=feature, text=paste(Name,"\n",Definition)))+
      geom_point(aes(y=yhat,x=feature,text=paste(Name,"\n",Definition)),size=0.5)+
      labs(x=name)+
      theme_bw()+
      theme(plot.subtitle = element_text(size = 3, vjust = 1))

    
    return(figure)
  }
  
  #============list of figures====================#
  #create a list with the pdp for each feature
  list_of_figures    <-list()

  for(name in country_features$Relevant_Var){
    feature_name <- name
    feature_def  <- Table_features$Name[Table_features$Feature==feature_name] 
    
    object <- pdp::partial(Model_object , pred.var = feature_name)
    object$Name <- colnames(object)[1]
    colnames(object)[1]    <- "feature"
    object$Definition      <- feature_def

    list_of_figures[[name]]<- plot_PDP_function(object,feature_name,feature_def,name)

  }
  # Combine all plots into one using patchwork
  #final_plots <- wrap_plots(list_of_figures, ncol = 4)

  return(list_of_figures)
}