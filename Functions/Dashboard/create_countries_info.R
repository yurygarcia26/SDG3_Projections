create_countries_info<-function(Selected_Region,Outcome_name){
  
  #--------------------
  cat("--------------------------------------------")
  cat("create_countries_info:\n")
  cat("Selected_Region:\n")
  print(Selected_Region)
  
  cat("\nOutcome_name:\n")
  print(Outcome_name)
  #-------------------------------------------------------
  
  countries_list <- as.data.frame(read.xlsx("./Data/Imputed_Data/List_of_final_variables.xlsx",sheet="Countries")) 
  regions_list   <- as.data.frame(read.xlsx("./Data/Imputed_Data/List_of_final_variables.xlsx",sheet="Regions")) 
  places_info    <- as.data.frame(read.xlsx(paste( "./Data/",Selected_Region,
  "_Results/Relevant_var_", Outcome_name,"_", Selected_Region,".xlsx",sep="")))

  if(Selected_Region=="Country"){
    countries_info <- merge(countries_list,places_info,by="Country")
  }else{
    # Rename Region Column to Country
    colnames(places_info)[which(names(places_info) == "Region")] <- "Country"
    countries_info <- merge(regions_list,places_info,by="Country")
  }

  list_of_countries <- places_info%>%dplyr::select(Country)%>%distinct()
  countries_name <- countries_info$Name

  return(list(countries_info=countries_info,countries_name=countries_name,list_of_countries=list_of_countries))
}