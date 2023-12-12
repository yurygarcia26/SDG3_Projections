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
library(imputeTS)
library(miceRanger)


##---Upload data-----------------------------------------------
Data      <- as.data.frame(read.xlsx("./Data/3.Suicide_rate_values_male_final.xlsx"))
#Data     <- as.data.frame(read.xlsx("./Data/3.Health_Coverage_values_final.xlsx"))
list_of_indicators <- as.data.frame(read.xlsx("./Data/List_of_final_vairables.xlsx",sheet="SuicideRate"))

# Update column names by removing periods and separating words
colnames(list_of_indicators) <- gsub("\\.", " ", colnames(list_of_indicators))

Name_data = "Male"
##-----------------------------------------------------------
#We want to identify which indicators has the 80% or 
#more available data per country.

#1. Calculate the percentage of available data (non-missing values) for each indicator.
#2. Identify indicators that meet the criteria (80% or more available data).
#3. For indicators that meet the criteria, fill the missing values.
#4. Remove rows(indicators) that don't meet the criteria.

##-----------------------------------------------------------
#Reshape the file
reshaped_data1 <- Data%>%pivot_longer(!c(Country,Year), names_to = "Indicator", values_to ="Value")
reshaped_data2 <- reshaped_data1%>%pivot_wider(names_from=Year, values_from = Value)
reshaped_data2 <- reshaped_data2%>%select("Country", "Indicator", as.character(2000:2019))


#Calculate the percentage of available data in each column
percent_available <-rowMeans(!is.na(reshaped_data2[,3:ncol(reshaped_data2)]))

# Define the threshold (80%) for deciding whether to keep a indicator
threshold <- 0.8

# Identify and filter indicators that satisfy the threshold
Indicators_satisfying_threshold <- which(percent_available >= threshold)
filtered_data                   <- reshaped_data2[Indicators_satisfying_threshold,]

#Remove DMA and KNA, we do not have suicide information

country_indices  = which(filtered_data$Country=="DMA"|filtered_data$Country=="KNA")
if (length(country_indices)!=0){
filtered_data2      = filtered_data[-country_indices,]
}else(filtered_data2=filtered_data)

write.xlsx(filtered_data2,paste('./ResultsMissingData/FilledData_',Name_data,".xlsx", sep=""))

##-----------------------------------------------------------

# Identify and filter indicators to remove (save a file to have the register)
removed_indicators <- which(percent_available<threshold)
removed_data       <- reshaped_data2[removed_indicators,] 
write.xlsx(removed_data,paste('./ResultsMissingData/RemovedData_',Name_data,".xlsx", sep=""))

##------------------------------------------------------------
# Plot indicators with missing data
# Identify and filter with missing data

Indices_missing_values    <-which(percent_available<1 & percent_available>=threshold)
Data_with_missing_values  <-reshaped_data2[Indices_missing_values,]
Data_with_missing_values2 <-Data_with_missing_values%>%pivot_longer(!c("Country","Indicator"), names_to = "Year", values_to="Value")

plot_missing_values<-function(Data){
  ggplot(Data, aes(x = Year, y = Value, color = Country, group = Country)) +
  geom_line() +
  labs(title = "Indicator Values Over Years",
       x     = "Year",
       y     = "Value",
       color = "Country") +
  theme_minimal() +
  facet_wrap(~ Indicator, ncol = 1)
}

p<- plot_missing_values(Data_with_missing_values2)
# Save the plot with a specific size
#ggsave(paste("./ResultsMissingData/missing_data_",Name_data,".pdf",sep=""), plot = p, width = 10, height = 15)
###-----------------------------------------------------------
##Filling missing data using Kalma Filter
###-----------------------------------------------------------

#Identify variables with missing data
columns_with_missing        <- unique(Data_with_missing_values$Indicator)

#filter information for a particular indicator
countries_with_missing_data <- unique(Data_with_missing_values$Country)

#filter data for indicator with missing data in a particular country
Data_for_particular_country <- filtered_data2%>%filter(Country==countries_with_missing_data[1])%>%
                               pivot_longer(!c(Country,Indicator), names_to = "Year", values_to = "Value" )%>%
                               pivot_wider(names_from = Indicator, values_from = Value)

ampDat<- Data_for_particular_country[,3:ncol(Data_for_particular_country)]


# Create the imputation model using miceRanger
mrModelOutput <- miceRanger(ampDat, valueSelector = "value", verbose = FALSE)

##----------------------
##Diagnostic Plotting
##----------------------

#Distribution of Imputed Values
#We can take a look at the imputed distributions compared to the original distribution for each variable:
#The red line is the density of the original, nonmissing data. The smaller, black lines are the density of the imputed values in each of the datasets. If these don’t match up, it’s not a problem, however it may tell you that your data 
#was not Missing Completely at Random (MCAR).
plotDistributions(mrModelOutput,vars='allNumeric')
