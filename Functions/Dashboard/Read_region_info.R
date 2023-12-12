create_countries_info<-function(Selected_Region,Outcome_name){

  countries_list <- as.data.frame(read.xlsx("./Data/Imputed_Data/List_of_final_variables.xlsx",sheet="Countries")) 
  regions_list   <- as.data.frame(read.xlsx("./Data/Imputed_Data/List_of_final_variables.xlsx",sheet="Regions")) 
  places_info    <- as.data.frame(read.xlsx(paste( "./Data/",Selected_Region,"_Results/Relevant_var_",Outcome_name,"_",Selected_Region,".xlsx",sep=""))) 

  list_of_countries <- places_info%>%dplyr::select(Country)%>%distinct()

  if(Selected_Region=="Country"){
    countries_info <- merge(countries_list,places_info,by="Country")
  }else{
    countries_info <- merge(regions_list,places_info,by="Country")
  }
  
  countries_name <- countries_info$Name
  return(list(countries_info=countries_info,countries_name=countries_name))
}