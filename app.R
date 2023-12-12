library(shiny)
library(shinydashboard)
library(ggplot2)
library(readxl)
library(openxlsx)
library(tidyverse)
library(kableExtra)
library(pdp)
library(DT)
library(cowplot)
library(MASS)
library(plotly)
library(patchwork)
library(caret)
library(glmnet)
library(randomForest)
library(sf)
library(rworldmap)
library(leaflet)
library(shinycssloaders)
library(data.table)


List_of_indicators<- as.data.frame(read.xlsx("./Data/Imputed_Data/List_of_final_variables.xlsx",sheet="Outputs"))
n_test=4
IndxNames <- List_of_indicators$NAME

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .navbar {
        background-color: #022851;
      }
      .navbar-default .navbar-nav > li > a,
      .navbar-default .navbar-brand {
        color: white;
      }
    "))
  ),
  navbarPage("SDG3: Scenarios based-sensitivity analysis",
             
             #Page 1------------------------------------------------------------
             tabPanel("Main",
                      # fluid --------------------------------------------------
                      fluidRow(
                        tags$div(
                          style = "background-color: #FFE4B5; padding: 10px;",
                          tags$h4("IMPORTANT!: This project is currently in development and is not suitable for decision-making at this stage. The results require thorough analysis and evaluation by peer reviewers before being considered conclusive.")
                        )
                      ),#end_fluid
                      # fluid --------------------------------------------------
                      fluidRow(
                        tags$h3("Overview", style = "font-weight: bold;"),
                        tags$h4("This dashboard introduces a mathematical framework to analyze significant covariates influencing a range of Sustainable Development Goals (SDGs). Employing various machine learning techniques, optimal models are identified based on their ability to fit the time series of indicators concerning specific covariables. The initial panels present detailed information on the model fit results, while the subsequent set focuses on the relevance of different covariables in the model. Additionally, users can perturb covariables to explore their impact on the trajectory of SDGs.")
                        
                      ),#end_fluid
                      # fluid --------------------------------------------------
                      fluidRow(
                        tags$h3("Model Projection", style = "font-weight: bold;"),
                        h4("Below are the forecast results generated through machine learning models, incorporating key variables in the adjustment process."),
                        h4("The left panel showcases the model's best fit (gray), the original data (blue), and the projection (red). The top box provides insights into the most effective model, including data adjustments and the initial number of covariates considered. Additionally, the table on the left reveals the final set of covariates employed in the model after a variable selection process."),
                        status      = "primary",
                        solidHeader = TRUE,
                        
                        column(6,
                               selectInput(inputId   = "IndexName", 
                                           label     = "Choose Indicator",
                                           width     = "80%",
                                           choices   = IndxNames,
                                           selected  = IndxNames[1]),
                               verbatimTextOutput(outputId = "messageOutput")),
                        
                        column(3,
                               selectInput("RegionName",
                                           label    = "Select Region/Country", 
                                           choices  = list("Country",
                                                           "Region"),
                                           selected = "Region"),
                               actionButton(inputId = "checkButton", 
                                            label = "Check Results")),
                        column(3,
                               selectInput(inputId  = "CountryName",
                                           label    = "Select Subregion/Country",
                                           choices  = NULL,
                                           selected = "Americas"),
                        ),
                        
                      ),#end_fluid
                      # fluid --------------------------------------------------
                      fluidRow(
                        
                        column(6,
                               plotlyOutput("ModelFit", height = 400, width = "95%")
                               
                        ),
                        column(6,
                               h4("Most relevant variables"),
                               tableOutput('Table_var'),
                               tags$style(
                                 HTML("
                                    #Table_var {
                                          width: 100%;  /* Adjust the width as needed */
                                          max-height: 300px; /* Set the maximum height */
                                          overflow-y: auto;  /* Add a scrollbar if the content is too long */
                                          font-size: 12px;
                              }"))
                               
                        )# closeColumn 
                        
                      ),#end fluid
                      # fluid --------------------------------------------------
                      fluidRow(
                        status      = "primary",
                        solidHeader = TRUE
                        
                      ),#end_fluid
                      # fluid --------------------------------------------------
                      fluidRow(
                        column(6, 
                               tags$h3("Partial Dependence Plots (PDPs)", style = "font-weight: bold;"),
                               tags$div(
                                 h4("Partial Dependence Plots (PDPs) provide insights into how the predicted outcome changes as the chosen feature varies while keeping other features constant."),
                                 p("For more information, you can visit the ",
                                   a("Interpretable Machine Learning book", href = "https://christophm.github.io/interpretable-ml-book/pdp.html")
                                 ),
                                 h4("When the model is Lasso, Boost glm or Boost gam, PDP figures are not displayed, as these plots are typically more useful and informative for non-linear models",
                                    style = "background-color: lightblue; padding: 10px; color: black;"
                                 ),
                                 div(plotlyOutput("PDPsPlot", height = 500, width = "90%"), align = "left")
                               )),
                        
                        
                        column(6,
                               tags$h3("Variable Importance Plot (VIP)",style = "font-weight: bold;"),
                               h4("This figure represents the importance of each variable in the model's prediction and helps identify the key features contributing to the model's performance. A higher VIP score indicates a more influential variable, suggesting that changes in that variable have a substantial effect on the model's output. On the other hand, lower VIP scores signify less influential variables."),
                               div(plotlyOutput("VarImp", height = 500, width = "90%"), align = "left"))
                      ),#end_fluid
                      # fluid --------------------------------------------------
                      fluidRow(
                        tags$h3("Projected scenarios under plausible perturbations", style = "font-weight: bold;"),
                        h4("For the scenarios you have to:"),
                        h4("1. Select the variable that you want to modify."),
                        h4("2. Choose a percentage. The time series of the selected variable will be modified in the test period, with each year being increased or decreased by this percentage."),
                        h4("3. Utilizing the trained model, the forecasted trajectories for the test period are generated, while keeping the remaining variables unchanged."),
                        h4("4. The Base scenario is the projection with the original variables (without perturbations), and the hypothetical scenario corresponds to either of the two perturbations (increase or decrease)."),
                        h4("The analysis will present results based on whether the selected indicator's value decreases or increases by the specified percentage"),
                        h4(),
                        h3("Scenarios"),
                        h4("Scenario 1: The variable increase X% per year"),
                        h4("Scenario 2: The variable decrease X% per year"),
                        
                        status      = "primary",
                        solidHeader = TRUE,
                        column(6,
                               selectInput(inputId  = "perturbedVar",
                                           label    = "Variable",
                                           width    = "90%",
                                           choices  = NULL),
                               
                        ),#end_column
                        
                        column(3,
                               selectInput(inputId  = "percentage",
                                           label    = "% Perturbation",
                                           choices  = NULL),
                               
                               actionButton(inputId = "scenarios", label = "Compute Scenarios"),
                               verbatimTextOutput(outputId = "messageMissPer"),
                               tags$style(type="text/css", "#messageMissPer {width: 300px;}"),
                               tags$div(
                                 style = "background-color: #FFE4B5; padding: 10px;width: 500px;",
                                 tags$h4("IMPORTANT!: The completion of this process may require several minutes."))
                               
                               
                               
                               
                        ),#end_column
                      ),#end_fluid
                      # fluid --------------------------------------------------
                      fluidRow(
                        
                        column(6,
                               h4("The figure corresponds to the perturbed variable. Users can observe the trend over time and the updated trajectory when each year is increased or decreased by the selected percentage."),
                               div(plotlyOutput("PlotVar", height = 400, width = "95%"), align = "left")
                        ),
                        column(6,
                               tags$h4(" ", style = "font-weight: bold;"),
                               tabBox(id = "tabset0", height = 500, width="100%", 
                                      
                                      tabPanel(width=6,"% Delta K",
                                               tags$h4("Difference in Means(%):\nHypothetical Scenario Minus Base Scenario"),
                                               h4("The % Delta K corresponds to the percentage increase or decrease (indicated by the sign -) of the mean of the hypothetical scenario with respect to the mean of the base scenario."),
                                               tags$h4(HTML("&Delta;&kappa; = (K<sub>scenario</sub> - K<sub>base</sub>) / K<sub>base</sub> &times; 100%"), style = "font-weight: bold;"),
                                               div(shinycssloaders::withSpinner(plotlyOutput("plot_per_delta",width="70%",height = "300px")),align="center")),
                                      
                                      
                                      tabPanel(width=6,"Delta k",
                                               tags$h4("Difference in Means:\nHypothetical Scenario Minus Base Scenario"),
                                               h4("K represents the average of the projected years."),
                                               h4("Delta K is the difference between the mean of the hypothetical scenario and the mean of the base scenario."),
                                               tags$h4(HTML("&Delta;K = K<sub>scenario</sub> - K<sub>base</sub>"), style = "font-weight: bold;"),
                                               
                                               div(
                                                 shinycssloaders::withSpinner(plotlyOutput("plot_delta",  width="70%",height = "300px")),align="center"
                                               )),
                                      
                                      
                                      tabPanel(width=6,"Projections",
                                               tags$h4("Projections"),
                                               div(plotlyOutput("NewProjections",width="90%",height = "300px"),align="center")),
                                      
                               )),#end_col
                        
                        
                      ),#end_fluid
                      # fluid --------------------------------------------------
                      fluidRow(
                        h3("Histograms",style = "font-weight: bold;"),
                        h4("The red line represents the mean value of the base scenario, the green line corresponds to the scenario where the selected variable is increased by the chosen percentage (Scenario 1), and the orange line corresponds to the scenario where the selected variable is decreased by the chosen percentage (Scenario 2). The histogram illustrates the uncertainty associated with each value."),
                        div(shinycssloaders::withSpinner(plotlyOutput("plot_hist_means", height = 300, width = "90%")), align = "left")
                      ),#end_fluid
                      # fluid --------------------------------------------------
                      fluidRow(
                        h3("Map",style = "font-weight: bold;"),
                        h4("The map display all countries and islands in the Americas and the Caribbean that share a common covariable, chosen as a significant factor in the modeling process."),
                        plotOutput("map",width = "90%", height = "600px")
                      ),#end_fluid

             ), # end page1-----------------------------------------------------
           
             tabPanel("Projections",
                      # fluid --------------------------------------------------
                      fluidRow(
                        tags$h3("Projections and scenarios 2030", style = "font-weight: bold;"),
                        h4("Utilizing the models trained up until 2019, we employ an autoregressive model to project the most influential covariables forward in time. Subsequently, we leverage these projections to forecast the indicator using the machine learning models"),
                        status      = "primary",
                        solidHeader = TRUE,
                        
                        column(6,
                               selectInput(inputId   = "IndexName2", 
                                           label     = "Choose Indicator",
                                           width     = "80%",
                                           choices   = IndxNames,
                                           selected  = IndxNames[1])),
                        column(3,
                               selectInput("RegionName2",
                                           label    = "Select Region/Country", 
                                           choices  = list(                                                       "Region"),
                                           selected = "Region"),
                               
                               actionButton(inputId = "checkButton2", label = "Check Results")),
                        
                        column(3,
                               selectInput(inputId  = "CountryName2",
                                           label    = "Select Subregion/Country",
                                           choices  = NULL,
                                           selected = "Americas"))
                        
                        
                      ),#end fluid
                      # fluid --------------------------------------------------
                      fluidRow(
                        
                        tabBox(id = "tabset1", height = 500, width="100%", 
                               
                               tabPanel(width = "80%", "Indicator Forecasting",
                                        
                                        div(
                                          shinycssloaders::withSpinner(plotlyOutput("projectionIndicator", width = "70%", height = "400px")),align = "center"),

                                        ## -------------------------------------
                                        fluidRow(
                                          tags$h3("Scenarios 2030", style = "font-weight: bold;"),
                                          
                                          column(6,
                                                 selectInput(inputId  = "perturbVar2",
                                                             label    = "Variable",
                                                             width    = "90%",
                                                             choices  = NULL),),#end_column
                                          
                                          column(3,
                                                 selectInput(inputId  = "percent2",
                                                             label    = "% Perturbation",
                                                             choices  = NULL),
                                                 
                                                 actionButton(inputId = "scenarios2", label = "Compute projection"),
                                                 tags$div(
                                                   style = "background-color: #FFE4B5; padding: 10px;width: 500px;",
                                                   tags$h4("IMPORTANT!: The completion of this process may require several minutes."))
                                                 
                                          )#end_column
                                        ),#end fluid
                                        
                                        ## -------------------------------------
                                        fluidRow(
                                          column(6,
                                                 h4("Figure 2: Covariable time series",style = "font-weight: bold;"),
                                                 h4("The figure corresponds to the perturbed variable. Users can observe the trend over time (blue) and the updated trajectory when each year is increased or decreased by the selected percentage (Orange)."),
                                                 div(plotlyOutput("PlotVar2", height = 400, width = "95%"), align = "left")
                                          ),
                                          
                                          column(6,
                                                 h4("Figure 3: Predicted indicator.",style = "font-weight: bold;"),
                                                 h4("This figure depicts the prediction of the indicator when a covariate is perturbed."),
                                                 tags$h5(HTML("K<sub>base</sub>: Average value of the base projection"), style = "font-weight: bold;"),
                                                 tags$h5(HTML("K<sub>scenario</sub>: Average value of the scenario projection "), style = "font-weight: bold;"),
                                                 tags$h5(HTML("Delta K = (K<sub>scenario</sub> - K<sub>base</sub>)"), style = "font-weight: bold;"),
                                                 tags$h5(HTML("% Delta K = (K<sub>scenario</sub> - K<sub>base</sub>) / K<sub>base</sub> &times; 100%"), style = "font-weight: bold;"),
                                                 h4("% Delta K represents the percentage change (indicated by the sign) of the scenario in comparison to the base."),
                                                 div(
                                                   shinycssloaders::withSpinner(plotlyOutput("forecasting", height = 400, width = "95%")), align = "left")
                                          ),
                                          
                                          
                                        )
                                        
                               ),#endtab1
                               
                               tabPanel(width="80%","Covariables Forecasting",
                                        div(
                                          shinycssloaders::withSpinner(plotlyOutput("projectedCovar",width="90%",height = "1500px")),align="center"),
                               ))#endtab2
                        
                      )#end fluid
             ),# end page2 -----------------------------------------------------
             
             tabPanel("List of variables",
                      # fluid --------------------------------------------------
                      fluidRow(
                        h3("List of Variables"),
                        h4("All indicators potentially associated with suicide rates and health coverage were chosen from WHO and PAHO. Subsequently, those indicators with missing data were excluded either due to unavailability during the relevant years or because they were not applicable to most countries. The code was assigned for programming purposes."),
                        h4("Sources"),
                        HTML('<h4><a href="https://www.who.int/data/gho/data/indicators/indicators-index" target="_blank">WHO List of indicators</a></h4>'),
                        HTML('<h4><a href="https://opendata.paho.org/en/core-indicators" target="_blank">PAHO core indicators</a></h4>'),
                        HTML('<h4><a href="https://ghdx.healthdata.org/record/global-burden-disease-study-2019-gbd-2019-covariates-1980-2019" target="_blank">Global Health Data Exchange</a></h4>')
                      ), #end fluid
                      # fluid --------------------------------------------------
                      fluidRow(
                        div(
                          tableOutput('ListVariables'),
                          tags$style(
                            HTML("#ListVariables {
                                 margin: 0 auto; /* Center the table horizontally */
                                 width: 100%;    /* Adjust the width as needed */
                                 max-height: 500px; /* Set the maximum height */
                                 overflow-y: auto;  /* Add a scrollbar if the content is too long */
                                 font-size: 12px;
                              }")
                          )
                        )
                      )#end fluid
                      
             )# end page3 ------------------------------------------------------
  ) # end navbarPAge
) #end fluidPage


#======= SERVER=================================

server <- function(input, output, session) {
  
  set.seed(123) 
  # === Import Functions ==================== #
  
  #source("Functions/Read_data.R")            
  #create testing and training data
  
  source("./Functions/Dashboard/Projection_info.R")       
  #Return the figure with projections and the table with the most relevant
  # variables
  
  source("./Functions/Dashboard/Plot_PDF.R")              
  #Create the Partial Dependence Plots (PDPs)
  
  source("./Functions/Dashboard/get_best_model_file.R")
  
  source("./Functions/Dashboard/Plot_variable.R")         
  #Plot the independent trajectory of a specific variable in SA
  
  source("./Functions/Dashboard/New_predictions.R")       
  #Compute prediction with perturbed data
  
  source("./Functions/Dashboard/CI_New_Projection.R")     
  #Upload the function to compute the CI
  
  source("./Functions/Dashboard/Plot_New_Projections.R")  
  #plot projections
  
  source("./Functions/Dashboard/Plot_hist_scenarios.R")   
  #Plot the mean of the different scenarios
  
  source("./Functions/Dashboard/Plot_VarImp_Function.R")
  source("./Functions/Dashboard/Plot_map.R")
  source("./Functions/Dashboard/create_countries_info.R")
  
  source("./Functions/Dashboard/Plot_projected_covariables.R")
  source("./Functions/Dashboard/Plot_forecasting_base.R")
  source("./Functions/Dashboard/Forcast_indicator_scenario.R")
 
  List_of_indicators<- as.data.frame(read.xlsx(
    "./Data/Imputed_Data/List_of_final_variables.xlsx",sheet="Outputs"))
  n_test=4
  buttonClicked        <- reactiveVal(FALSE)
  anotherButtonClicked <- reactiveVal(FALSE)
  
  
  #list of countries-----------------------------------------
  observeEvent(input$RegionName, {
    Selected_Indicator <- as.character(input$IndexName)
    Selected_Region    <- as.character(input$RegionName)
    Outcome_name       <- List_of_indicators$CODE[
      List_of_indicators$NAME==Selected_Indicator]

    results<- create_countries_info(Selected_Region,Outcome_name )
    countries_info    <-results$countries_info
    countries_name    <-results$countries_name
    list_of_countries <-results$list_of_countries
    updateSelectInput(session, 'CountryName', choices = countries_name)
   })

  
  #==========Check if the info for the country is available ==================
  observeEvent(input$checkButton, {
      
      buttonClicked(TRUE)
  
      #clean variables:
      output$messageOutput <- renderText({NULL})
      output$ModelFit      <- renderPlot({NULL})
      output$Table_var     <- renderTable({NULL})
      output$VarImp        <- renderPlot({NULL})
      output$PDPsPlot      <- renderPlot({NULL})
      
      #-----------------------------------------
      Selected_Indicator   <- as.character(input$IndexName)
      Selected_Region      <- as.character(input$RegionName)
      Selected_Country     <- as.character(input$CountryName)
      Outcome_name         <- List_of_indicators$CODE[
        List_of_indicators$NAME==Selected_Indicator]
      

      results<- create_countries_info(Selected_Region, Outcome_name)
      countries_info    <-results$countries_info
      countries_name    <-results$countries_name
      list_of_countries <-results$list_of_countries
      updateSelectInput(session, 'CountryName',
                        choices = countries_name,selected=Selected_Country)
      
      #----------------------------------------------------------------
      model_info <- as.data.frame(read.xlsx(paste(
        "./Data/",Selected_Region, "_Results/Relevant_var_",
        Outcome_name,"_",Selected_Region, ".xlsx", sep="")))
      
      if("Region" %in% colnames(model_info)){
        colnames(model_info)[which(names(model_info) == "Region")] <- "Country"
      }
      
      print(colnames(model_info))
      
      model_info_countries <- as.data.frame(read.xlsx(paste(
        "./Data/Country_Results/Relevant_var_", Outcome_name,
        "_Country.xlsx", sep="")))
      
      country <- unique(
        countries_info$Country[countries_info$Name==Selected_Country])

      if (country %in% list_of_countries$Country) {
        
        print("Yes!")
        
        best_model            <- unique(model_info$method[
          model_info$Country==country])
        
        features              <- model_info %>% filter(Country==country) %>% 
          dplyr::select(Relevant_Var)
        
        Model_object          <- get_best_model_file(country, Outcome_name)
        
        print("Best model obtained !")
        
        results_projections   <- Projection_info(
          country, Outcome_name, Selected_Region)
        
        print("Results projections obtained !")
        
        Num_Initial_variables <- unique(
          model_info %>% filter(Country==country) %>% 
            dplyr::select(Total_Variables))$Total_Variables
        
        output$messageOutput <- renderText(paste(
          "Best model:", best_model, 
          "\nAn initial set of ", Num_Initial_variables,
          " covariables is employed for model fitting" ))
        
        #plot modelFit
        output$ModelFit  <- renderPlotly({
          Figure      <- results_projections$Figure
          plotly_plot <- ggplotly(Figure)
        })
        
        #Render Table 
        output$Table_var=renderTable({
          Table<-results_projections$Table
          Table
        })
        
        #Render VarImp
        output$VarImp <-renderPlotly({
          figure<-Plot_VarImp_Function(best_model,Model_object,
                                       model_info,country)
          figure<-ggplotly(figure)
          figure %>%
            layout(xaxis = list(tickangle = -90))  
        })
        
        # Render PDPs
        if(best_model=="Lasso"| best_model=="Boost_glm"| best_model=="Boost_gam"){
          
          output$PDPsPlot <- renderPlotly({NULL})
          
        }else{
          
          if(length(features$Relevant_Var)<4){
            n= length(features$Relevant_Var)
          }else{n=4}
          
          output$PDPsPlot <- renderPlotly({
            Table           <- results_projections$Table
            list_of_figures <- plot_pdp_function(Model_object,features,Table)
            list_of_figures <- lapply(list_of_figures, ggplotly)
            final_plots     <- plotly::subplot(list_of_figures, nrows = n)
            return(final_plots) 
          })
        }
        
        output$map <- renderPlot({
          Mapa     <- invisible(Plot_map(variable,
                                         model_info_countries,Diff=TRUE))
        })
        
        #--------------------------------------------------------------------
        #SENSITIVY ANALYSIS
        #--------------------------------------------------------------------
        cat("-------------------------------------------\n")
        cat("Starting sensitivity analysis\n")
        
        covariables_list <- results_projections$Table$Name
        per_values       <- seq(0,10,0.5)
        updateSelectInput(session,'perturbedVar',
                          choices = covariables_list,selected = covariables_list[1])
        updateSelectInput(session,'percentage',
                          choices = per_values, selected= per_values[1])
        
        #Plot indicator--------------------------------------
        output$PlotVar <-renderPlotly({
          var_name <- as.character(input$perturbedVar)
          per      <- as.numeric(input$percentage)
          variable <- results_projections$Table$Feature[
            results_projections$Table$Name==var_name]
          
          if(length(variable)==0){
            var_name= covariables_list[1] 
            variable <- results_projections$Table$Feature[
              results_projections$Table$Name==var_name]
          }
          
          #Plot covariable------------------------------------------------
          restuls_perturbation <- Plot_variable(country, variable, per, n_test)
          Plot <- ggplotly(restuls_perturbation[[1]])
          Plot%>%layout(legend = list(orientation = "h", x = 0.15, y = 1.15))
        })
        
      }else{
        output$messageOutput <- renderText(
          "There is no available information for this country.")}
      buttonClicked(TRUE)
    },priority = 1)#end_check_bottom
  
  
  #-------------------------------------------------------------------
  #CLEAN FIGURES
  observeEvent(buttonClicked(),{ 
    
  #clean plot delta
    output$plot_delta <- renderPlotly({
     
      plot_ly() %>% 
        layout(
          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
        )
    })
    
    #clean plot % delta
    output$plot_per_delta <- renderPlotly({
      plot_ly() %>% 
        layout(
          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
        )
    })
    
    #clean plot projections
    output$NewProjections <- renderPlotly({
      plot_ly() %>% 
        layout(
          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
        )
    })
    
    #clean hist means
    output$plot_hist_means <- renderPlotly({
      plot_ly() %>% 
        layout(
          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
        )
    })
    
    output$map <- renderPlot({
      variable <- NULL
      model_info_countries <- NULL
      Mapa     <- invisible(Plot_map(variable,model_info_countries,Diff=FALSE))
    })
    
    buttonClicked(FALSE)
  })
  
  #clean map
 
  # SCENARTIOS =========================================
  observeEvent(input$scenarios,{
    cat("-----------------------------\n")
    cat("Starting scenarios\n")
    
    # Clear previous outputs  
    if(input$percentage==0) {
      output$messageMissPer <- renderText(paste("Please select a percentage."))
      
    }else{
      
      output$plot_hist_means<-renderPlotly({NULL})
      output$map            <-renderPlotly({NULL})
      output$plot_delta     <-renderPlotly({NULL})
      output$plot_per_delta <-renderPlotly({NULL})
      output$NewProjections <-renderPlotly({NULL})
      
      #start----------------------------------------------------------
      Selected_Indicator   <- as.character(input$IndexName)
      Selected_Region      <- as.character(input$RegionName)
      Selected_Country     <- as.character(input$CountryName)
      per                  <- as.numeric(input$percentage)
      var_name             <- as.character(input$perturbedVar)
      Outcome_name         <- List_of_indicators$CODE[List_of_indicators$NAME==Selected_Indicator]
      
      model_info           <- as.data.frame(read.xlsx(paste(
        "./Data/", Selected_Region, "_Results/Relevant_var_",
        Outcome_name,"_", Selected_Region, ".xlsx", sep="")))
      if("Region" %in% colnames(model_info)){
        colnames(model_info)[which(names(model_info) == "Region")] <- "Country"
      }
      
      model_info_countries <- as.data.frame(read.xlsx(paste(
        "./Data/Country_Results/Relevant_var_", Outcome_name,
        "_Country.xlsx",sep="")))
      
      results         <- create_countries_info(Selected_Region,Outcome_name)
      countries_info  <- results$countries_info
      country         <- unique(countries_info$Country[
        countries_info$Name==Selected_Country])
      
      best_model      <- unique(model_info$method[model_info$Country==country])
      features        <- model_info %>% filter(Country==country) %>% 
        dplyr::select(Relevant_Var)
      Model_object    <- get_best_model_file(country,Outcome_name)
      
      results_projections <- Projection_info(country,
                                             Outcome_name,
                                             Selected_Region)
      
      covariables_list <- results_projections$Table$Name
      variable         <- results_projections$Table$Feature[
        results_projections$Table$Name==var_name]
      
      if(length(variable)==0){
        var_name = covariables_list[1] 
        variable <- results_projections$Table$Feature[
          results_projections$Table$Name==var_name]
      }
      
   
      #Plot covariable------------------------------------------------
      restuls_perturbation <-Plot_variable(country,variable,per,n_test)
      
      #Compute projections to each perturbation
      perturbation_data  <- restuls_perturbation[[2]]
      data_perturbation1 <- perturbation_data$Perturbation1
      data_perturbation2 <- perturbation_data$Perturbation2
      data_original      <- perturbation_data$Value
      best_s_list        <- model_info%>%
        filter(Country==country)%>%
        dplyr::select(best_s_lasso)
      best_s             <- unique(best_s_list$best_s_lasso)
      
      #base case
      Base_results          <- new_projection(Outcome_name,country,
                                              data_original,
                                              variable,
                                              Model_object,
                                              model_info,best_s)
      #increase
      perturbation1_results <- new_projection(Outcome_name,country,
                                              data_perturbation1,variable,
                                              Model_object,model_info,best_s)
      #decrease
      perturbation2_results <- new_projection(Outcome_name,country,
                                              data_perturbation2,variable,
                                              Model_object,model_info,best_s)
      #all resulsts
      list_of_results_scenarios <- list(Base=Base_results,
                                        perturbation1=perturbation1_results,
                                        perturbation2=perturbation2_results)
      
      lists_plots_scenarios     <- plot_hist_scenario(
        list_of_results_scenarios,country)  
      
      #PLOT HISTOGRAMS-------------------------------------
      output$plot_hist_means <-renderPlotly({
        ggplotly(lists_plots_scenarios$plot_hist_means) 
      })#end_plot
      
      
      #PLOT DELTA ----------------------------------------
      output$plot_delta <-renderPlotly({
          ggplotly(lists_plots_scenarios$plot_delta)
      })#end_plot
      
      #PLOT %DELTA ----------------------------------------
      output$plot_per_delta <-renderPlotly({
        ggplotly(lists_plots_scenarios$plot_per_delta)   
        
      })#end_plot
      
      
      
      #PLOT new projections--------------------------------
      output$NewProjections <-renderPlotly({
        fig<-ggplotly(Plot_New_Projection(country,Outcome_name,
                                          list_of_results_scenarios,
                                          features,Selected_Region),
                      tooltip = "text")
        fig%>%layout(legend = list(orientation = "h", x = 0.15, y = 1.15))
        return(fig)
      })#end_plot
      
      #incluir Mapa
      output$map <- renderPlot({
        Mapa     <- invisible(Plot_map(variable,
                                       model_info_countries,Diff=TRUE))
      })
      
    }#end_if
    
 
    
  },priority = 2)#end_
  
  #-----------------------------------------------------
  #Page 2
  output$ListVariables=renderTable({
      List_of_variables<-as.data.frame(read.xlsx(
        "./Data/Imputed_Data/List_of_final_variables.xlsx",sheet="All"))
      colnames(List_of_variables)<-c("Code","Name")
      List_of_variables
  })
  
  #-----------------------------------------------------------------------------
  # Page 3  FORCASTING INDICATOR AND VARIABLES
  #-----------------------------------------------------------------------------
  
  #update selected variables
  observeEvent(input$RegionName2, {
    Selected_Indicator2 <- as.character(input$IndexName2)
    Selected_Region2    <- as.character(input$RegionName2)
    Outcome_name2       <- List_of_indicators$CODE[List_of_indicators$NAME==Selected_Indicator2]
    
    results2           <-create_countries_info(Selected_Region2,Outcome_name2)
    countries_info2    <-results2$countries_info
    countries_name2    <-results2$countries_name
    list_of_countries2 <-results2$list_of_countries
    updateSelectInput(session, 'CountryName2', choices = countries_name2)
  })
 

  #==========Check if the info for the country is available ======================================
 
  observeEvent(input$checkButton2, {
    
    ##clean variables:
    anotherButtonClicked(TRUE)
    #-----------------------------------------
    Selected_Indicator2  <- as.character(input$IndexName2)
    Selected_Region2     <- as.character(input$RegionName2)
    Selected_Country2    <- as.character(input$CountryName2)
    Outcome_name2        <- List_of_indicators$CODE[List_of_indicators$NAME==Selected_Indicator2]

    results2           <- create_countries_info(Selected_Region2,Outcome_name2)
    countries_info2    <- results2$countries_info
    countries_name2    <- unique(results2$countries_name)
    list_of_countries2 <- results2$list_of_countries
    
    updateSelectInput(session, 'CountryName2', choices = countries_name2,selected=Selected_Country2)
    
    #---------------------------------------------------------------------------
    
    #plot indicator projection
    output$projectionIndicator <- renderPlotly({
      plot_projection <- Plot_forecasting_base(Selected_Region2, Selected_Country2, Outcome_name2,scenario = 0,Diff=FALSE)
      ggplotly(plot_projection,tooltip = "text")
      })
    
    #plot covariables  
    output$projectedCovar <- renderPlotly({
        list_fig_covar    <- Plot_projected_covariables(Outcome_name2,Selected_Country2,Selected_Region2,variable=0,per=0)
        n_rows            <- ceiling(length(list_fig_covar) / 2)
        list_fig_covar    <- lapply(list_fig_covar, ggplotly)
        final_plots       <- plotly::subplot(list_fig_covar, nrows = n_rows)
        final_plots
    })
        
    #Update variables for scenarios 20230  
    results_projections2 <- Projection_info(Selected_Country2,Outcome_name2,Selected_Region2)
    covariables_list2    <- results_projections2$Table$Name
    per_values2          <- seq(-10,10,0.5)
    updateSelectInput(session,'perturbVar2',choices = covariables_list2, selected = covariables_list2[1])
    updateSelectInput(session,'percent2',   choices = per_values2, selected= 0)
    
    #Plot indicator--------------------------------------
    output$PlotVar2 <-renderPlotly({
      var_name2 <- as.character(input$perturbVar2)
      per2      <- as.numeric(input$percent2)
      variable2 <- results_projections2$Table$Feature[results_projections2$Table$Name==var_name2]
      
      if(length(variable2)==0){
        var_name2= covariables_list2[1] 
        variable2 <- results_projections2$Table$Feature[results_projections2$Table$Name==var_name2]
      }
      
      #Plot covariable------------------------------------------------
      restuls_perturbation2 <- Plot_projected_covariables(Outcome_name2,Selected_Country2,Selected_Region2,variable2,per2)
      Plot <- ggplotly(restuls_perturbation2[[1]])
      Plot%>%layout(legend = list(orientation = "h", x = 0.15, y = 1.15))
    }) 
 
    
  })
  
  # CLEAN FIGURES FORESCASTING
  observeEvent(anotherButtonClicked(), { 
    
    # Clean another set of plots
    output$forecasting <- renderPlotly({
      plot_ly() %>% 
        layout(
          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
        )
    })
    anotherButtonClicked(FALSE)
  })
  
  #-----------------------------------------------------------------------------
  
  # SCENARIOS 2: This scenarios are created with the projections until 2030
  observeEvent(input$scenarios2,{

    cat("-----------------------------\n")
    cat("Starting scenarios 2030\n")
    
    #clean figures
    output$forecasting <- renderPlotly({NULL})

    #update variables
    Selected_Indicator2  <- as.character(input$IndexName2)
    Selected_Region2     <- as.character(input$RegionName2)
    Selected_Country2    <- as.character(input$CountryName2)
    per2                 <- as.numeric(input$percent2)
    var_name2            <- as.character(input$perturbVar2)
    Outcome_name2        <- List_of_indicators$CODE[
      List_of_indicators$NAME==Selected_Indicator2]
    
    results_projections2 <- Projection_info(
      Selected_Country2,Outcome_name2,Selected_Region2)
    covariables_list2    <- results_projections2$Table$Name
    variable2            <- results_projections2$Table$Feature[
      results_projections2$Table$Name==var_name2]
    
    cat("Check 1 \n")
    
    if(length(variable2)==0){
      var_name2= covariables_list2[1] 
      variable2 <- results_projections2$Table$Feature[
        results_projections2$Table$Name==var_name2]
    }
    
    cat("Check 2 \n")
    
    #Compute projections to each perturbation
    restuls_perturbation2 <- Plot_projected_covariables(
      Outcome_name2,Selected_Country2,Selected_Region2,variable2,per2)
    perturbation_data2    <- restuls_perturbation2[[2]]
    #compute new projections
    
    cat("Check 3 \n")
    
    perturbed_projection <- Forcast_indicator_scenario(
      Outcome_name2, Selected_Region2,Selected_Country2,perturbation_data2)

    cat("Check 4 \n")
    
    #PLOT new projections--------------------------------
    output$forecasting <-renderPlotly({
      plot_forcasting<-ggplotly(Plot_forecasting_base(
        Selected_Region2,Selected_Country2,Outcome_name2,
        perturbed_projection,Diff=TRUE),tooltip = "text")
      plot_forcasting%>%layout(legend = list(
        orientation = "h", x = 0.15, y = 1.15))
    })#end_plot
   
    cat("Check 5 \n")
    
  })
  
  #-----------------------------------------------------------------------------
} #end server

shinyApp(ui, server)

#Selected_Region="Country"
#country="HTI"
#var="COV4"
#Variable=var
#per=6
#Outcome_name="SUICM"