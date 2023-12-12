library(tidyverse)
library(readxl)
library(openxlsx)
library(dplyr)
library(flextable)
library(DT)
library(downloadthis)
library(ggplot2)
library(gridExtra)
library(car)
library(stats)   

##Upload data---------------------------------------
Data               <- as.data.frame(read.xlsx("./Imputed_data/1.Imputed_data_suicide_female.xlsx"))
list_of_indicators <- as.data.frame(read.xlsx("./Data/List_of_final_vairables.xlsx",sheet="SuicideRate"))
Name_data = "female"
Outcome   = "SUICF"

##Coverage Health--------------------------------------------
#Data               <- as.data.frame(read.xlsx("./ResultsMissingData/FilledData_CovHealth.xlsx"))
#list_of_indicators <- as.data.frame(read.xlsx("./Data/List_of_final_vairables.xlsx",sheet="Health_Coverage"))
#Data_Name = "CovHealth"
#Outcome   = "CovIndex"


#--------------------------------------------------
# Update column names by removing periods and separating words
colnames(list_of_indicators) <- gsub("\\.", " ", colnames(list_of_indicators))


##-------------------------------------------------------------

##Find correlation
Data_lon <- Data%>%pivot_longer(!c("Country","Indicator"),names_to = "Year", values_to = "Values")
Data_wid <- Data_lon%>%pivot_wider(names_from = Indicator, values_from = Values)

Data_wid<-Data
# Calculate the correlation matrix
correlation_matrix <- cor(Data_wid[ , -c(1,2)], use = "pairwise.complete.obs")

# Find correlations with SUICF
cor_with_suicf           <- as.data.frame(correlation_matrix[ , Outcome])
colnames(cor_with_suicf) <-'Corr_Suicide'
cor_with_suicf$Indicator <- rownames(cor_with_suicf)
write.xlsx(cor_with_suicf,paste("./ResultsCorrelation/Corr_suic_",Name_data,"2.xlsx",sep=""))

# Create a data frame for the heatmap
heatmap_data <- as.data.frame(as.table(correlation_matrix))
colnames(heatmap_data) <- c("Row", "Column", "Correlation")


# Create the heatmap using ggplot2
heatmap_plot <- function(data,PlotName){
  p<-ggplot(data, aes(x = Column, y = Row, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "orange", midpoint = 0, na.value = "grey50") +
  labs(title = "Correlation Heatmap") +
  theme_minimal() +
  theme(axis.text.x   = element_text(angle = 90, hjust = 1)) +
  geom_text(aes(label = round(Correlation, 1)), color = "black", size = 2)+
  labs(x="", y="")

# Save the plot with a specific size
ggsave(paste("./ResultsCorrelation/Full_Correlation_",PlotName,"_",Name_data,"2.pdf",sep=""), plot = p, width = 10, height = 10)
}
#Save corr matrix
heatmap_plot(heatmap_data,"FullCorr")


