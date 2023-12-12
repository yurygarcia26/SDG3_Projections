Plot_VarImp_Function<-function(best_model,Model_object,model_info,country){
  
  if(best_model=="Lasso"|best_model=="Boost_glm"|best_model=="Boost_gam"){
    
    importance_scores <-model_info%>%filter(Country==country)%>%dplyr::select(features=Relevant_Var,importance=Selected_feat_importance)
    
    
  }else{
    #List with the most relevant variables for each country
    importance_scores <- varImp(Model_object, scale=FALSE)$importance
    importance_scores <- data.frame(features=row.names(importance_scores),
                                  importance=importance_scores$Overall)
  }
  importance_scores<-importance_scores[order(-importance_scores$importance),]
  importance_scores$features<-factor(importance_scores$features, levels = importance_scores$features)
  
# Plot Variable Importance using ggplot2
Figure<-ggplot( importance_scores, aes(x = features, y = importance) )+
  geom_bar(stat = "identity", fill = "tan2", color = "black") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme_bw()+
  labs(x="Features",y="Importance")

return(Figure)
}