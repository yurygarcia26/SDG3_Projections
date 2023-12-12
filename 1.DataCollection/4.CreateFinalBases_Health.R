library(tidyverse)
library(ggplot2)
library(readxl)
library(knitr)
library(kableExtra)
library(DT)
library(openxlsx)
library(dplyr)


###_---------------------------------------------------------
#upload files
country_Codes_ch          = as.data.frame(read_excel('G:/Mi unidad/2023_ODS_OPS/Data/CountryCodes_Americas.xlsx',sheet='Health'))
health_data               = as.data.frame(read.csv('G:/Mi unidad/2023_ODS_OPS/OutputFiles/2.Values_Health_Coverage_Indicators_WHO_gdhx_healthdata.csv'))
List_of_indicators_health = as.data.frame(read_excel('G:/Mi unidad/2023_ODS_OPS/OutputFiles/1.List_of_Selected_Health_Coverage_Indicators.xlsx',sheet='2_Selected_Health_Coverage'))


#filter columns we are interested in and merge the data set with
#the file with the codes for each variable
ch_data          = health_data%>%filter(Country%in%country_Codes_ch$Code)%>%select('IndicatorCode', 'Year','Country','SEX', 'Value')
variables_code   = List_of_indicators_health%>%select('IndicatorCode','VAR')
Health_data_file = merge(ch_data,variables_code,  by='IndicatorCode')


#Some variables have RURAL, URBAN, TOTAL, FMLE and MLE information
#we are going to separate all of them in different indicators
Health_data_file_RUR = Health_data_file%>%filter(SEX == "RUR")%>%
  mutate(VAR = paste(VAR, "RUR", sep = "_"))

Health_data_file_URB = Health_data_file%>%filter(SEX == "URB")%>%
  mutate(VAR = paste(VAR, "URB", sep = "_"))

Health_data_file_TOT = Health_data_file%>%filter(SEX == "TOTL")%>%
  mutate(VAR = paste(VAR, "TOTL", sep = "_"))

Health_data_file_FMLE= Health_data_file%>%filter(SEX == "FMLE")%>%
  mutate(VAR = paste(VAR, "FMLE", sep = "_"))

Health_data_file_MLE = Health_data_file%>%filter(SEX == "MLE")%>%
  mutate(VAR = paste(VAR, "MLE", sep = "_"))

# Filter rows where 'SEX' is not 'RUR,URB,TOT,FMLE,MLE'
non_modified_rows <- Health_data_file%>%filter(SEX!="URB"&SEX!="RUR"&SEX!='FMLE'&SEX!="MLE"&SEX!="TOTL")

#combine all the files
Health_data_file_modified =rbind(Health_data_file_RUR,Health_data_file_URB,Health_data_file_TOT,Health_data_file_FMLE,Health_data_file_MLE,non_modified_rows )


Health_data_file1= Health_data_file_modified%>%select(Year,Country,Value,VAR)
Health_data_file2= Health_data_file1%>%pivot_wider(names_from = VAR, values_from = Value)



# Write the data frame to the Excel file
write.xlsx(Health_data_file2, 'G:/Mi unidad/2023_ODS_OPS/OutputFiles/3.Health_Coverage_values_final.xlsx', na = "")
