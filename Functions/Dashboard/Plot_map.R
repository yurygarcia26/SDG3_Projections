Plot_map<-function(var,model_info,Diff){

  if (Diff==TRUE){
  df <- model_info%>%filter(Relevant_Var==var)%>%dplyr::select(code=Country)

  sPDF <- joinCountryData2Map(df,
                            joinCode = "ISO3",
                            nameJoinColumn = "code")  
#select out your countries
colourPalette   <- rep("#FF7F24",nrow(df))
sPDFmyCountries <- sPDF[sPDF$code %in% df$code,]

#use the bbox to define xlim & ylim

#png("./CovIndex_5_Clusters/Map_Health_Cov_5_Clust.png",width = 800, height = 600)
map <- mapCountryData(sPDF, nameColumnToPlot="code",
                      colourPalette=colourPalette,
                      numCats = number_of_cluster,
                      catMethod="categorical",
                      #xlim=bbox(sPDFmyCountries)[1,],
                      #ylim=bbox(sPDFmyCountries)[2,],
                      mapTitle="",
                      addLegend=FALSE,
                      border="black")
  }else{
    # Create an empty world map
    worldMap <- getMap()
    
    # Plot the empty world map without color, color bar, and title
    map<-plot(worldMap, col = "white", 
         border = "black", 
         bg = "transparent", 
         main = "")

}

return(map)

#dev.off()
}
