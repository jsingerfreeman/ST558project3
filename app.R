# Jose Singer-Freeman
# NC State | ST 558 | Final Project 


library(shiny)
library(shinydashboard)
library(tidyverse)
library(knitr)
library(plotly)
library(kableExtra)
library(ggiraph)
library(ggiraphExtra)
library(rgl)
library(caret)
library(tree)
library(randomForest)
library(gbm)
library(DT)

#torun original by Kyle Garrison:  runGitHub("ST-558-Final-Project", "kylebeard56", ref = "master")


# Data source is given here:
# https://www.kaggle.com/datasets/andrewmvd/heart-failure-clinical-data


# ============================================================
# Import dataset
# ============================================================
heartData <- read.csv("heart_failure_clinical_records_dataset.csv")

# ============================================================
# UI 
# ============================================================
ui <- dashboardPage(
    
    # Set the header for the dashboard
    dashboardHeader(title = "Heart Failure Prediction"),
    
    # Set up  tab items
    dashboardSidebar(
        sidebarMenu(
            menuItem("About", tabName = "about", icon = icon("home")),
            menuItem("Data Exploration", tabName = "exploration", icon = icon("chart-bar")),
            menuItem("Modeling", tabName = "modeling", icon = icon("brain")),
            menuItem("Data", tabName = "data", icon = icon("filter"))
          ) #end dashboardSidebar
    ),  #end dashboardPage
    
    # Body content
    dashboardBody(
        
        tabItems(
            
          # (2) Data Viewing - Allows the user to scroll through the data or subset data of interest
             tabItem(tabName = "data",
                 fluidRow(
                     box(h3("Filter by column"),
                         checkboxGroupInput("columns", 
                                     "Select columns to display:",
                                     choices = names(heartData),
                                     selected=names(heartData))
                                 ), #end box 1
          
                 box(h3("Filter by row"),
                      numericInput("row_number_from", "From Row Number:", min = 1, max = nrow(heartData), value = 1),
                     numericInput("row_number_to", "To Row Number:", min = 1, max = nrow(heartData), value = nrow(heartData)),
                     br()
                     
                 ), #end box 2
                
                  box(h3("Download the table below"),
                     # Download button
                     downloadButton("download_btn", "Download CSV"),
                     br()
                 ), #end box 3
          
          
          DT::dataTableOutput("table1", width = "100%")
          
             ) #end fluidRow
             
              
          
        ), #end of data tabItem 
        
        tabItem(tabName = "modeling",
                
                tabBox(
                  
                  title = "Modeling Page",
                  # The id lets us use input$tabset1 on the server to find the current tab
                  id = "tabset1", width=12,
                  
                  tabPanel("Modeling Info", "First tab content"),
                  
                  tabPanel("Model Fitting", 
                           box(h3("Random Forest Parameter"),
                               sliderInput("num_trees", "Select the number of trees:", min = 1, max = 10, value = 4, step=1),
                               br()
                               ), #end of box
                           box(h3("Traning/Testing Split"),
                               sliderInput("split", "Choose the proportion of data for training:", min =0.5 , max = 1, value = 0.7, step=0.1),
                               br()),
                          # Box to display the model summary
                          box(title = "Model Summary", width = 8,
                           verbatimTextOutput("rf_model_summary")
                               
                           )), #end of box and model fitting tabPanel
                  tabPanel("Prediction", "Tab content 3")
                
                  ) #end of tabBox
        
        ) #end of modeling tabItem 
        
    ) #end tabItems
) #end dashboardBody
) #end dashboardPage

# ============================================================
# SERVER 
# ============================================================
server <- function(input, output) {
    
    # --- (2) Data ---
    
    # Data table to display all data or subset
  
    filtered_data <- reactive({
    subset_data <- heartData%>%
      select(all_of(input$columns))%>%
      slice(input$row_number_from:input$row_number_to)
    subset_data
    }) #end reactive
  
    output$table1 <- DT::renderDT({
        datatable(filtered_data(),options=list(scrollX = TRUE))
  
    } ) #end output$table1

    

# Create a downloadable CSV file
    output$download_btn <- downloadHandler(
        filename = function() {
         if (is.null(input$columns) || identical(input$columns, names(heartData))) {
          # If no columns were deselected or all columns are selected
         return("data.csv")
       } else {
      # If columns were deselected, include "_subset" in the filename
        return("data_subset.csv")
     }
    },
         content = function(file) {
           if (is.null(input$columns) || identical(input$columns, names(heartData))) {
             # If no columns were deselected or all columns are selected
              write.csv(heartData, file)
            } else {
            # If columns were deselected, write the subsetted data to the file
            write.csv(filtered_data(), file)
    }
  }
)
    
    # --- (3) Modeling
    
    # Create an index vector for stratified sampling
    set.seed(101)
    index <- createDataPartition(heartData$DEATH_EVENT, p = 0.8, list = FALSE)
    
    # Split the data into 80% and 20% samples
    modeling_data <- heartData[index, ]
    test_data <- heartData[-index, ]
    
    modeling_data$DEATH_EVENT<-factor(modeling_data$DEATH_EVENT, levels=c(0,1), labels=c("ND", "D"))
    test_data$DEATH_EVENT<-factor(test_data$DEATH_EVENT, levels=c(0,1), labels=c("ND", "D"))
    
    
    #random forest
    rf_fit <- reactive({
      num_trees <- as.integer(input$num_trees)
      
      #control function for cv
      mycontrol<-trainControl(method = "cv", 
                              number = 10, 
                              summaryFunction = twoClassSummary,
                              savePredictions = 'all', 
                              classProbs = TRUE
                            )
      
      #model formula 
      Formula<-as.formula("DEATH_EVENT~.")
      
      #set seed for reproducibility
      set.seed(101)
      
      # Fit the random forest model
      
      rfFit <- train(form=Formula,
                     data=modeling_data,  #CHANGE THIS XXX
                     method="rf",
                     trControl=mycontrol,
                     ntree=num_trees)
      
      return(summary(rfFit$finalModel))
    })  #end reactive

    output$rf_model_summary <- renderPrint({
      rf_fit()
    }) #end output$rf_model_summary
} #end server

# Run the application 
shinyApp(ui = ui, server = server)
