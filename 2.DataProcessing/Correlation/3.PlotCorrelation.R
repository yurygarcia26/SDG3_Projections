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
Data               <- as.data.frame(read.xlsx("./ResultsMissingData/FilledData_Fem.xlsx"))
list_of_indicators <- as.data.frame(read.xlsx("./Data/List_of_final_vairables.xlsx",sheet="SuicideRate"))

# Update column names by removing periods and separating words
colnames(list_of_indicators) <- gsub("\\.", " ", colnames(list_of_indicators))

Data_Name = "Fem"
##-------------------------------------------------------------

##Find correlation
Data_lon <- Data%>%pivot_longer(!c("Country","Indicator"),names_to = "Year", values_to = "Values")
Data_wid <- Data_lon%>%pivot_wider(names_from = Indicator, values_from = Values)






################################################################
##Plot a figure with each panel equal to SUICF vrs the other variables
only_indicators = as.data.frame(Data_wid[,-c(1,2)])

# Custom function to create scatter plots with 'SUICF' on x-axis and line of best fit
create_scatter_plot <- function(y_col, data) {
  ggplot(data = data, aes(x = .data[['SUICF']], y = .data[[y_col]])) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add line of best fit
    labs(x = 'SUICF', y = y_col) +
    theme_minimal()  # Customize the theme as needed
}

# Extract column names
all_columns       <- names(only_indicators)
indicator_columns <- setdiff(all_columns, "SUICF")

# Create a list of scatter plots
scatter_plots <- vector("list", length = length(indicator_columns))

index <- 1
for (i in indicator_columns) {
  scatter_plots[[index]] <- create_scatter_plot(i, only_indicators)
  index <- index + 1
}

# Arrange scatter plots in a grid using grid.arrange from gridExtra
arranged_plots <- grid.arrange(grobs = scatter_plots, ncol = 5)
ggsave(paste("./ResultsCorrelation/Line_plots_",Name_data,".pdf",sep=""), plot = arranged_plots, width = 10, height = 10)

###################################################################