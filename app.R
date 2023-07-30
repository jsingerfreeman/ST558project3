# Jose Singer-Freeman
# Final Project ST 558 


library(shiny)
library(shinydashboard)
library(tidyverse)
library(knitr)
library(tidyr)
library(ggplot2)
library(caret)
library(DT)
library(shinyWidgets)


#torun original by Kyle Garrison:  runGitHub("ST-558-Final-Project", "kylebeard56", ref = "master")


# Data source is given here:
# https://www.kaggle.com/datasets/andrewmvd/heart-failure-clinical-data
#citation to article for source of data
# https://bmcmedinformdecismak.biomedcentral.com/articles/10.1186/s12911-020-1023-5

# ============================================================
# Import dataset
# ============================================================
heartData <- read.csv("heart_failure_clinical_records_dataset.csv")
heartData<-heartData%>%select(-c(anaemia, smoking, diabetes, high_blood_pressure, sex, time))  #remove predictors that the article said were not important

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
          # (1) About - Description of the app
          tabItem(tabName = "about",
                  titlePanel("About"),
                  br(),
                  h4(HTML("<span style='color:#00205B'><b>Purpose of this App</b>")),
                  
                  HTML("<ul>
                       <li>explore the Heart Failure data set </li>
                       <li>use three machine learning models to predict the occurence of a death </li>
                  </ul>"),
                  br(),
                  h4(HTML("<span style='color:#00205B'><b>The Data</b>")),
                  HTML("<p> The  dataset in this app consists of the medical records of 299 heart failure patients collected at two hospitals in Pakistan in 2015.</p>", 
                       "<p> The  dataset contains 9 features, mostly clinical data.</p>",
                       "<p>The  response variable., death event, indicates whether a patient died or survived before the end of a follow-up period. </p>", 
                       "<p>The data may be found at  <a href='https://www.kaggle.com/datasets/andrewmvd/heart-failure-clinical-data' >Heart Failure Prediction Competition</a>.</p>",  
                       "<p>Tthis <a href='https://bmcmedinformdecismak.biomedcentral.com/articles/10.1186/s12911-020-1023-5' >article</a> provides
                       valuable additional backgound.</p>",
                         ),
                  br(),
                  h4(HTML("<span style='color:#00205B'><b>Content of the Tabs in this App</b>")),
                  HTML("<p> In adition to this 'About' tab, you can find  the follwing 3 tabs (on the left side of the app)</p>
                       <ul>
                       <li><b>Data Exploration:</b>  You can plot  the features and obtain certain numeric summaries.  You can do so for the entire data or a sample. </li>
                       <li><b>Modeling:</b> There are 3 subtabs
                          <ul>
                          <li><b>Modeling Info</b> - explains the 3 machine learning techniques used in Model Fitting</li>
                          <li><b>Model Fitting</b> - Implements the 3 modelling techniches </li>
                          <li><b>Prediction</b> -  You can enter your own values for the features and obtain the prediction of the random forest model</li>
                          </ul>
                       <li><b>Data:</b>   You can filter the data by column and rows and export a csv file of the resulsts</li>
                       ")
                  
          ), #end tabItem
          
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
                  
                 
                  tabPanel("Modeling Info", 
                      h4(HTML("<p>The following are explanations of the three modeling approaches used in the 'Model Fitting' and 'Prediction' tabs above.</p>" )), 
                        box(   
                           h4(HTML("<span style='color:#00205B'><b>Logistic Regression</b>")),
                           h5(HTML("<b>Explanation:</b>")),
                           HTML("<p>
             Logistic Regression models the relationship between a binary response variable and predictor variables by estimating 
             the probabilities of the response variables' outcomes. The model uses the logistic function to 
             transform predictors into probabilities between 0 and 1. </p><p> The logistic function is shown below:
             </p>"),
                           withMathJax(),
                           helpText('$$p(X)=\\frac{e^{\\beta_X}}{1-e^{\\\\beta_1X}}$$'), 
                           HTML("<p> where X is the design matrix and beta a vector of coefficients. 
                           </p>"),
                           
                           h5(HTML("<b>Benefits:</b>")),
                           
                           HTML("<ul>
              <li>Interpretability: Logistic regression is interpretable (although odds ratios can confuse some.</li>
              <li>Fast Training: The model efficiently handles large datasets.</li>
              <li>Inference: allows for confidence estimates.</li>
              </ul>"),
                           h5(HTML("<b>Drawbacks</b>")),
                           HTML("<ul>
              <li>Linearity: Doesn't do well  with nonlinearity.</li>
              <li>Sensitive to Outliers: Affected by outliers, leading to biased predictions.</li>
            </ul>")
                           ), #end box,      
                 box(          
                           h4(HTML("<span style='color:#00205B'><b>Bagged Trees</b>")),
                           h5(HTML("<b>Explanation:</b>")),
                           HTML("<p>
             Bagged Trees is an ensemble learning method for classification. 
             It trains multiple decision tree models on random subsets of the entire data (bootstrapping) and 
             combines predictions through voting or averaging (aggregation).
             </p>"),
                           h5(HTML("<b>Benefits:</b>")),
                           HTML("<ul>
              <li>Reduced Overfitting: Bagging reduces variance by combining models.</li>
              <li>Robustness: Less sensitive to noisy data and outliers.</li>
              <li>Linearity: Captures nonlinear relationships.</li>
            </ul>"),
                           h5(HTML("<b>Drawbacks</b>")),
                           HTML("<ul>
              <li>Interpretability: Ensemble models lack interpretability.</li>
              <li>Computation: Training multiple trees can be resource-intensive.</li>
              <li>Lack of Diversity: Performance may not improve significantly if trees are highly correlated.</li>
            </ul>")
                 ), #end box, 
                  box(         
                           h4(HTML("<span style='color:#00205B'><b>Random Forest</b>")),
                           h5(HTML("<b>Explanation:</b>")),
                           HTML("<p>
             Random Forest is an extension of bagged trees with added randomness during the tree-building process.
             </p>"),
                           h5(HTML("<b>Benefits:</b>")),
                           HTML("<ul>
              <li>Reduced Overfitting: Random Forest reduces overfitting compared to bagged trees due to random feature selection</li>
              <li>Robustness: Less sensitive to noisy data and outliers.</li>
              <li>Linearity: Captures nonlinear relationships.</li>
            </ul>"),
                           h5(HTML("<b>Drawbacks</b>")),
                           HTML("<ul>
              <li>Interpretability: Same issue as above, can be harder to interpret than a single tree.</li>
              <li>Computation : Training multiple trees with random feature selection can be time-consuming.</li>
              </ul>")
                  ) #end box
                  ),
                           
                           
                           
                           
                           
                           
                  
                  tabPanel("Model Fitting", 
                           h4(HTML("<p>Running the models below can take a bit.  Please be patient after pressing the 'Run Models' button.</p>" )), 
                           fluidRow(
                           column( width=6,  
                           #Box to ask for  RF parameter
                           box(h3("Random Forest Parameter"),     #XXXXXXXXX
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
                           
                           box(h3("Predictor Choice"),
                               
                               checkboxGroupInput("predictors", "Select Predictors for models:", choices = setdiff(colnames(heartData), "DEATH_EVENT"),
                                           selected=setdiff(colnames(heartData), "DEATH_EVENT")),
                                                              br()
                               ))), #end of box and column and fluidrow
                           
                           #Set the green color for the button in HTML
                           tags$head(
                             tags$style(
                               HTML(".btn-green {
                                 background-color: green;
                                color: white;
                                border-color: green;}")
                             )
                           ),
                           
                           #Box for button to run the models
                          fluidRow(
                            column(width=2,
                          box(actionButton("run_models", "Run Models", class="btn-green"),
                               br()
                               )),#end of box and column
                           ),  #end of fluidrow
                           
                           column(width=4,
                          fluidRow(
                          # display the RF model accuracy for training and testing
                          h4(HTML("Random Forest Accuracy")), 
                                      textOutput("rf_training_accuracy"),
                                      textOutput("rf_testing_accuracy")
                                  ),#end of box and row
                          fluidRow(        
                          #  display the RF model summary
                            h4(HTML("Random Forest Variable Importance")) ,
                          verbatimTextOutput("rf_model_summary"),
                          br()
                          )), #end of box, row and column
                          
                          
                          
                          column(width=4,
                                 fluidRow(
                          #  display the logistic regression model accuracy for training and testing
                          h4(HTML("Logistic Regression Accuracy")),  
                          textOutput("lr_training_accuracy"),
                          textOutput("lr_testing_accuracy")
                          ),
                          
                          #  display the logistic regression model summary
                          fluidRow(
                            h4(HTML("Logistic Regression Summary")),  
                              verbatimTextOutput("lr_model_summary")     
                           )), #end of box and column
                          
                          
                          column(width=4,
                          
                          # display the bagged trees model accuracy for training and testing
                          fluidRow(
                            h4(HTML("Bagged Trees Accuracy")), 
                              textOutput("bt_training_accuracy"),
                              textOutput("bt_testing_accuracy")),
                          
                          fluidRow(    
                            h4(HTML("Bagged Trees Variable Importance")), 
                              verbatimTextOutput("bt_model_summary")
                              )     
                          
                          
                          ) #end of column 
                          ), #end of model fitting tabPanel and fluidrow
                  
                  
                  tabPanel("Prediction", 
                           #Set the green color for the button in HTML
                           tags$head(
                             tags$style(
                               HTML(".btn-green {
                                 background-color: green;
                                color: white;
                                border-color: green;}")
                             )
                           ),
                           
                           titlePanel("Prediction of Random Forest Model"),
                           sidebarLayout( 
                             sidebarPanel(h2("Choose predictors"),
                                          h4(HTML("<p><span style='color: red;'>Please run models in the 'Model Fitting' tab before using this tab.</span> </p><p> Enter zero if you want to omit a predictor.</p>")),
                                          
                                            
                                          numericInput("age", "age", label=paste("age: (40-100)"), value = 60),
                                          numericInput("phosphok", "creatinine phosphokinase", label=paste("creatinine phosphokinase: (23-7,900)"), value = 580),
                                          numericInput("platelets", "platelets:", value = 260000, label=paste("platelets: (25,000-900,000)")),
                                          numericInput("serumCr", "serum creatinine:", value = 1.3, label=paste("serum creatinine: (0.5-10)")),
                                          numericInput("serumSo", "serum sodium:", value = 137,label=paste("serum sodium: (100-150)")),
                                          numericInput("eject", "ejection fraction:", value = 38, label=paste("ejection fraction: (14-90)")),
                                          actionButton("predictButton", "Predict", class="btn-green")
                             ),
                             mainPanel(verbatimTextOutput("predicted_result")
                             )
                             
                           )
                  )
                 
                  ) #end of tabBox
              
        ), #end of modeling tabItem 
        
         #  4 Data Exploration
         tabItem(tabName = "exploration",
                 titlePanel("Data Exploration"),
                 sidebarLayout(  
                   
                   sidebarPanel(
                     selectInput("variable", "Choose Variable:", 
                                 choices=setdiff(colnames(heartData), "DEATH_EVENT"), selected="age"),
                     numericInput("num_rows", "Number of Rows to Sample from the dataset (randomly) (max 299):", value = 100,
                                  min = 1, max = nrow(heartData)),
                     radioButtons("plot_type", "Choose Plot Type:",
                                  choices = c("Single Histogram","Boxplot by Death Event"), 
                                  selected = "Single Histogram")
                     ), #end sidebar panel 
                   
                   mainPanel(
                     plotOutput("box_plot"),
                     h3(textOutput("summary_header")),  # Use textOutput for displaying the  variable name chosen by user
                     tableOutput("summary_table")
                     )
                 ) #end sidebar

         ) #end of tabItem
        
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
      
        return(list(summary=varImp(rfFit), model=rfFit))
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
       
       return(list(summary=varImp(btFit),model=btFit))
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
    
    
    
    ##   4    Prediction
    
    predict_result <- eventReactive(input$predictButton, {
      new_data <- data.frame(
        age = input$age,
        creatinine_phosphokinase = input$phosphok,
        platelets = input$platelets,
        serum_creatinine = input$serumCr,
        serum_sodium=input$serumSo,
        ejection_fraction=input$eject
      )
      
      prediction <- predict(lr_model_fit()$model, newdata = new_data, type = "prob")
      # Return the prediction
       return(prediction$D)
    })
    
    
    output$predicted_result <- renderPrint({
      prediction <- predict_result()
      paste("Predicted probability of death:", round(prediction,3))
    })

   
  ##  5    -- Data Exploration
    #Create data for sample of number of rows 
    filtered_data2 <- reactive({
      if (input$num_rows >= nrow(heartData)) {
        return(heartData)
      } else {
        set.seed(123)  # Set a seed for reproducibility
        sample_rows <- sample(1:nrow(heartData), size = input$num_rows, replace = FALSE)
        return(heartData[sample_rows, ])
      }
    })
    
    
    
    output$box_plot <- renderPlot(
      
      if (input$plot_type == "Single Histogram") {
        ggplot(data = filtered_data2(), aes(x = !!sym(input$variable))) +
          geom_histogram(fill = "steelblue", color = "white", bins = 30) +
          labs(title = paste("Histogram:", input$variable),
               x = input$variable,
               y = "Frequency")
        
      }  else {ggplot(data = filtered_data2(), aes(x = DEATH_EVENT, y = !!sym(input$variable), fill = factor(DEATH_EVENT))) +
          geom_boxplot() +
          labs(title = paste("Box Plot:", input$variable, "by Death Event"),
               x = "Death Event",
               y = input$variable) +
          scale_fill_discrete(name = "Death Event")
      }
    ) #end render Plot
   


    
    output$summary_header <- renderText({
      paste("Summary Table:", input$variable)  # Display the selected variable name
    })
    
    output$summary_table <- renderTable({
      if (input$plot_type == "Single Histogram"){
        summary_stats <- filtered_data2() %>%
          summarise(
            Min = min(!!sym(input$variable)),
            `1st Quartile` = quantile(!!sym(input$variable), 0.25),
            Median = median(!!sym(input$variable)),
            Mean = mean(!!sym(input$variable)),
            `3rd Quartile` = quantile(!!sym(input$variable), 0.75),
            Max = max(!!sym(input$variable))
          ) #end summarize
        colnames(summary_stats)[-1] <- paste(input$variable, colnames(summary_stats)[-1], sep = " - ")
        } else {
        
      summary_stats <- filtered_data2() %>%
        group_by(DEATH_EVENT) %>%
        summarise(
          Min = min(!!sym(input$variable)),
          `1st Quartile` = quantile(!!sym(input$variable), 0.25),
          Median = median(!!sym(input$variable)),
          Mean = mean(!!sym(input$variable)),
          `3rd Quartile` = quantile(!!sym(input$variable), 0.75),
          Max = max(!!sym(input$variable))
        )
      colnames(summary_stats)[-1] <- paste(input$variable, colnames(summary_stats)[-1], sep = " - ")
        }
      summary_stats
    }, rownames = FALSE) 
    
} #end server

# Run the application 
shinyApp(ui = ui, server = server)
