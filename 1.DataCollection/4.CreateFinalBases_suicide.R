library(tidyverse)
library(ggplot2)
library(readxl)
library(knitr)
library(kableExtra)
library(DT)
library(openxlsx)


years = 2000:2019
Suicide_data=as.data.frame(read_excel('G:/Mi unidad/2023_ODS_OPS/OutputFiles/2.Values_Suicide_rate_Indicators_PAHO.xlsx'))
Extra_data         = as.data.frame(read_excel('G:/Mi unidad/2023_ODS_OPS/OutputFiles/2.Indicators_from_ghdx_healthdata.xlsx'))
List_of_indicators = as.data.frame(read_excel('G:/Mi unidad/2023_ODS_OPS/OutputFiles/1.List_of_Selected_Suicide_Indicators.xlsx',sheet='3.Suicide_Selected_Indicators'))
country_Codes      = as.data.frame(read_excel('G:/Mi unidad/2023_ODS_OPS/Data/CountryCodes_Americas.xlsx',sheet='Suicide'))

#Select specific columns and create a single file with a code for each variable. We removed the indicators names.

paho_data      = Suicide_data%>%select('IndicatorCode', 'Year','Country', 'Value')
variables_code = List_of_indicators%>%select('IndicatorCode','VAR','SEX')

Data_file       = merge(variables_code,paho_data,  by='IndicatorCode',all=TRUE)
Extra_data_code = merge(variables_code,Extra_data, by='IndicatorCode')
#Filter data related with female suicide rate and male suicide rate
female_suicide_rate_data = Data_file%>%filter(SEX =='BOTH'|SEX=='FMLE')%>%filter(Year%in%years)%>%filter(Country%in%country_Codes$Code)%>%select(VAR,Year,Country,Value)
Extra_data_famale        = Extra_data_code%>%select(VAR,Year,Country,Value)
female_final_data        = rbind(female_suicide_rate_data,Extra_data_famale)

##---
male_suicide_rate_data = Data_file%>%filter(SEX =='BOTH'|SEX=='MLE')%>%filter(Year%in%years)%>%filter(Country%in%country_Codes$Code)%>%select(VAR,Year,Country,Value)
Extra_data_male        = Extra_data_code%>%select(VAR,Year,Country,Value)
male_final_data        = rbind(male_suicide_rate_data,Extra_data_male)

##-------------
suicide_rate_data = Data_file%>%filter(SEX !='MLE'&SEX=='FMLE')%>%filter(Year%in%years)%>%filter(Country%in%country_Codes$Code)%>%select(VAR,Year,SEX,Country,Value)
Extra_data_both = Extra_data_code%>%
  select(VAR,Year,SEX=SEX.y,Country,Value)
suicide_final_data = rbind(suicide_rate_data,Extra_data_both)


##-----------------------------------------
female_data= female_final_data%>%pivot_wider(names_from = VAR, values_from = Value)
write.xlsx(female_data,'G:/Mi unidad/2023_ODS_OPS/OutputFiles/3.Suicide_rate_values_famale.xlsx')

male_data= male_final_data%>%pivot_wider(names_from = VAR, values_from = Value)

write.xlsx(male_data,'G:/Mi unidad/2023_ODS_OPS/OutputFiles/3.Suicide_rate_values_male.xlsx')

suicide_data= suicide_final_data%>%pivot_wider(names_from = VAR, values_from = Value)
write.xlsx(suicide_data,'G:/Mi unidad/2023_ODS_OPS/OutputFiles/3.Suicide_rate_values.xlsx')

