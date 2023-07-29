# Jose Singer-Freeman
# Final Project ST 558 


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
                     
                 )), #end box 2 and fulidRow
                 fluidRow(
                  box(h3("Download the table below"),
                     # Download button
                     downloadButton("download_btn", "Download CSV"),
                     br()
                 ), #end box 3
          
          
          DT::dataTableOutput("table1", width = "100%")
          
             ) #end fluidRow
             
              
          
        ), #end of data tabItem 
        
        # (3) Modeling  -  allows the user to choose parameter for data splitting and for  the 3 models
        
        tabItem(tabName = "modeling",
               
                # Tab Box containing tab panels
                tabBox(
                  
                  title = "Modeling Page",
                  # The following id lets us use input$tabset1 on the server to find the current tabBox
                  id = "tabset1", width=12,
                  
                 
                  tabPanel("Modeling Info", "First tab content"),
                  
                  tabPanel("Model Fitting", 
                           fluidRow(
                           column( width=6,  
                           #Box to ask for  RF parameter
                           box(h3("Random Forest Parameter"),
                               sliderInput("mtry", "Select number of variables randomly sampled as candidates at each split:", min = 1, max = 10, value = 4, step=1),
                               br()
                               ), #end of box 
                           
                           
                           #Box to get number of trees parameter for bagged trees model 
                           box(h3("Bagged Trees Parameter"),
                               numericInput("ntrees", "Select number of trees:", min = 100, max =1000, value=500, step=100),
                               br()
                           ), #end of box 
                           
                           #Box to ask for training/test split parameter
                            box(h3("Split training/testing"), 
                             sliderInput("split", "Choose the proportion of data for training:", min =0.5 , max = 0.9, value = 0.7, step=0.1),
                            )), #end box and column
                          
                           column(width=4,
                           
                           box(h3("Predictor Choice and Traning/Testing Split"),
                               
                               checkboxGroupInput("predictors", "Select Predictors for models:", choices = setdiff(colnames(heartData), "DEATH_EVENT"),
                                           selected=setdiff(colnames(heartData), "DEATH_EVENT")),
                                                              br()
                               ))), #end of box and column and fluidrow
                           
                          
                           
                           #Box for button to run the models
                          fluidRow(
                            column(width=2,
                          box(actionButton("run_models", "Run Models"),
                               br()
                               )),#end of box and column
                           ),  #end of fluidrow
                           
                           column(width=4,
                          fluidRow(
                          # Box to display the RF model accuracy for training and testing
                          box(title="Random Forest Accuracy", 
                                      textOutput("rf_training_accuracy"),
                                      textOutput("rf_testing_accuracy")
                                  )),#end of box and row
                          fluidRow(        
                          # Box to display the RF model summary
                          box(title = "Random Forest Model Variable Importance", 
                          verbatimTextOutput("rf_model_summary"),
                          br()
                          ))), #end of box, row and column
                          
                          
                          
                          column(width=4,
                                 fluidRow(
                          # Box to display the logistic regression model accuracy for training and testing
                          box(title="Logistic Regression Accuracy", 
                          textOutput("lr_training_accuracy"),
                          textOutput("lr_testing_accuracy")
                          )),
                          
                          # Box to display the logistic regression model summary
                          fluidRow(
                          box(title = "Logistic Regression Model Summary", 
                              verbatimTextOutput("lr_model_summary")     
                           ))), #end of box and column
                          
                          
                          column(width=4,
                          
                          # Box to display the bagged trees model accuracy for training and testing
                          fluidRow(
                          box(title="Bagged Trees  Accuracy", 
                              textOutput("bt_training_accuracy"),
                              textOutput("bt_testing_accuracy"))),
                          fluidRow(    
                          box(title = "Bagged Trees Model Variable Importance", 
                              verbatimTextOutput("bt_model_summary")
                              ))     
                          
                          
                          ) #end of column 
                          ), #end of model fitting tabPanel and fluidrow
                  
                  
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

    # Reactive expression to store the selected predictors and subset the data
     data_subset <- reactive({
       selected_predictors <- input$predictors
       # Subset the data to selected predictors for both models
       data_subset <- heartData[, c("DEATH_EVENT", selected_predictors)]
       return(data_subset)
     })
    
    
    # Reactive expression to split data into training and testing sets
     data_splits <- reactive({
      data_sub <- data_subset()
      # Set the seed for reproducibility
      set.seed(123)
      # Create data partition for 80% training and 20% testing
      index <- createDataPartition(data_sub$DEATH_EVENT, p = input$split, list = FALSE)
      training_data <- data_sub[index, ]
      testing_data <- data_sub[-index, ]
      
      training_data$DEATH_EVENT<-factor(training_data$DEATH_EVENT, levels=c(0,1), labels=c("ND", "D"))
      testing_data$DEATH_EVENT<-factor(testing_data$DEATH_EVENT, levels=c(0,1), labels=c("ND", "D"))
      return(list(training = training_data, testing = testing_data))
    })
    
 
    
    
    ## Function to fit random forest model and return variable importance plot
      
      rf_model_fit <- eventReactive(input$run_models,{
      mtry1 <- as.integer(input$mtry)
      
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
                     data=data_splits()$training,  
                     method="rf",
                     trControl=mycontrol,
                     ntree=mtry1,
                     na.action = na.omit)
      
      return(list(summary=varImp(rfFit$finalModel), model=rfFit))
    })  #end reactive
     
     
     ## Function to fit Logistic Regression model and return summary
     
     lr_model_fit <- eventReactive(input$run_models,{
       
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
       
       # Fit the logistic regression model
       
       logFit <- train(form=Formula,
                      data=data_splits()$training,  
                      method="glm",
                      trControl=mycontrol,
                      family="binomial",
                      na.action = na.omit)
       
       return(list(summary=summary(logFit),model=logFit))
     })  #end reactive
      
     
     ## Function to fit Bagged Trees model and return summary
     
     bt_model_fit <- eventReactive(input$run_models,{
       #get parameter
       ntrees <- as.integer(input$ntrees)
       
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
       
       # Fit the bagged tree model
       
       btFit <- train(form=Formula,
                       data=data_splits()$training,  
                       method="treebag",
                       trControl=mycontrol,
                       ntrees=ntrees,
                       na.action = na.omit)
       
       return(list(summary=varImp(btFit$finalModel),model=btFit))
     })  #end reactive
     
     
     # Calculate and render the accuracy of the Random Forest  training set
     output$rf_training_accuracy <- renderText({
       predictions <- predict(rf_model_fit()$model, newdata = data_splits()$training)
       accuracy <- confusionMatrix(predictions, data_splits()$training$DEATH_EVENT)$overall["Accuracy"]
       paste("Training Data Accuracy = ", round(accuracy, 3))
     }) 
     
     
     # Calculate and render the accuracy of the Random Forest  testing set
     output$rf_testing_accuracy <- renderText({
       predictions <- predict(rf_model_fit()$model, newdata = data_splits()$testing)
       accuracy <- confusionMatrix(predictions, data_splits()$testing$DEATH_EVENT)$overall["Accuracy"]
       paste("Testing Data Accuracy = ", round(accuracy, 3))
     })
     
     
     # Calculate and render the accuracy of the logistic regression training set
     output$lr_training_accuracy <- renderText({
       predictions <- predict(lr_model_fit()$model, newdata = data_splits()$training)
       accuracy <- confusionMatrix(predictions, data_splits()$training$DEATH_EVENT)$overall["Accuracy"]
       paste("Training Data Accuracy = ", round(accuracy, 3))
     }) 
     
     
     # Calculate and render the accuracy of the logistic regression testing set
     output$lr_testing_accuracy <- renderText({
       predictions <- predict(lr_model_fit()$model, newdata = data_splits()$testing)
       accuracy <- confusionMatrix(predictions, data_splits()$testing$DEATH_EVENT)$overall["Accuracy"]
       paste("Testing Data Accuracy = ", round(accuracy, 3))
     })
     
     # Calculate and render the accuracy of the Bagged Trees  training set
     output$bt_training_accuracy <- renderText({
       predictions <- predict(bt_model_fit()$model, newdata = data_splits()$training)
       accuracy <- confusionMatrix(predictions, data_splits()$training$DEATH_EVENT)$overall["Accuracy"]
       paste("Training Data Accuracy = ", round(accuracy, 3))
     }) 
     
     
     # Calculate and render the accuracy of the  Bagged Trees  testing set
     output$bt_testing_accuracy <- renderText({
       predictions <- predict(bt_model_fit()$model, newdata = data_splits()$testing)
       accuracy <- confusionMatrix(predictions, data_splits()$testing$DEATH_EVENT)$overall["Accuracy"]
       paste("Testing Data Accuracy = ", round(accuracy, 3))
     })
     
     
    # Render the random forest model summary as output
    output$rf_model_summary <- renderPrint({
      rf_model_fit()$summary
    }) #end render rf model
    
    # Render the logistic regression model summary as output
    output$lr_model_summary <- renderPrint({
      lr_model_fit()$summary
    })
    
    
    # Render the Bagged Trees model summary as output
    output$bt_model_summary <- renderPrint({
      bt_model_fit()$summary
    }) #end render rf model
    
} #end server

# Run the application 
shinyApp(ui = ui, server = server)
