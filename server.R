###
# author: Ipsita Datta
# date: 11/25/2021
# purpose: Create the back-end  for the app exploring HeartDisease
# Data of Cleveland
###

# Load in packages used in the analysis.
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(DT)
library(readr)
library(tidyverse)
library(plotly)
library(rattle)

#include data 
source("source.R")

shinyServer(function(input, output, session) {
 
    getData<-reactive({
      ###
      # Create a data table output that the user can filter.
      ###
      selectedSex <- input$rb
      selectedcondition <- input$rb2
      selectedCols <- input$checkboxGroup
      
      heartDiseaseData%>%
        filter(sex %in% selectedSex,
               condition %in% selectedcondition)%>%
        select(selectedCols)
    })   
    
    output$HDdata <- renderDataTable({
    
     # Filter the data set based on user input.
      newData<-getData()
    
  })
    
    #download filtered data subset
    output$downloadData <- downloadHandler(
      
      filename = function() {
        paste("Heartdata.csv")
      },
      content = function(file) {
        
        write.csv(getData(),file)
         
      }
    )

  
  # plots for data exploration tab
  output$plot1<- renderPlot({
    xvalue<-input$predictor
    if (input$plotType == "Barchart"){
       ggplot(data = heartDiseaseData,aes(x=pull(heartDiseaseData,xvalue)))+
       geom_bar(aes(fill=pull(heartDiseaseData,xvalue))) +labs(x="")+theme(legend.title = element_blank())+
       ggtitle("Bar Chart based on predictors")
       }
    else if ( input$plotType == "Histogram"){
       g<- ggplot(heartDiseaseData,aes(age, fill=pull(heartDiseaseData,xvalue))) +
        geom_histogram(aes(y=..density..),breaks=seq(0, 80, by=1), color="grey18") +
        ylab("Density / Count") + theme(legend.title = element_blank())+ labs(x="Age")+
        ggtitle("Heart disease  spread out across age based on predictors")
        if (input$density){
          g<-g+geom_density(alpha=.2, fill="black")  
        } 
      return(g)
        
    }
  
  else if(input$plotType == "Boxplot"){
    ggplot(heartDiseaseData,aes(x=pull(heartDiseaseData,xvalue), y=age, fill=pull(heartDiseaseData,xvalue))) + 
      geom_boxplot() + theme(legend.title = element_blank())+labs(x="")
   }
  })#end of plot
  
  #Data Exploration back-end
  
  output$dTable<-renderDataTable({
    xvalue<-input$predictor
    if (input$summary == "Frequency"){
    tab<-table(pull(heartDiseaseData,xvalue))
    t<-data.frame(tab)
    names(t)[2]<-paste0("Frequency ")
    return(t)
    }
    else if (input$summary == "Proportions"){
      tab<-table(pull(heartDiseaseData,xvalue))
      t<-data.frame(prop.table(tab))
      t[2]<-round(t[2],4)
      names(t)[1]<-paste0("Variable name:",xvalue)
      names(t)[2]<-paste0("Proportion ")
      return(t)
    }
  })# end of dTtable
  
  output$info<-renderText(
    text<-paste0("Overall Summary"," of HeartDisease Data")
  )
  output$allsummary<-renderPrint(
    summary(heartDiseaseData)
  )
  
  # Model page back-end
  ## math expression for model info tab
  
  output$logReg <- renderUI({
    
    withMathJax(
      helpText(
        "$$\\ln(\\frac{p_i}{1-p_i}) = \\beta_0 + \\Sigma^k_{j=1}\\beta_jx_{ij}$$"
      )
    )
  })
  
 # model info tab
  output$minCpInput <- renderUI({
    
    ###
    # Create the input box for the min Cp for the tree.
    ###
    
    numericInput(
      inputId = "minCp",
      label = "Min.(select: 0.01 or 0.002 etc )", 
      min = 0, 
      max = 1000, 
      value = 0.01
    )
  })
  output$maxCpInput <- renderUI({
    
    ###
    # Create the input box for the max number of Cp in the tree.
    ###
    
    # Find the user's min Cp.
    minCp <- input$minCp
    
    # Start at 21.
    value <- 10
    
    # If the minCp is greater than 21, move it up.
    if (minCp > value){
      value <- minCp
    }
    
    numericInput(
      inputId = "maxCp",
      label = "Max.", 
      min = minCp, 
      max = 20, 
      value = value)
  })
  
  observeEvent(input$trainStart, {
    
    ###
    # train the performance of the three models.
    ###
    
    # Create a Progress object
    progress <- Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error.
    on.exit(progress$close())
    # Set the message to the user while cross-validation is running.
    progress$set(message = "Performing Cross-Validation", value = 0)
    
    # Get the variables to use for each model.
     Vars <- unlist(input$checkGroup)
    
    # Get the random seed, proportion of training, and k-folds params.
    randSeed <- input$randSeed
    propTraining <- input$propTraining
    numFolds <- input$numFolds
    
    # Get the Cps to try.
    minCp <- input$minCp
    maxCp <- input$maxCp
    numCps <- input$numCps
    Cps <- seq(minCp, maxCp, length.out=numCps)
    
    # Get the random forest mtrys.
    randForMtry <- as.numeric(input$randForMtry)
    
    # Set the random seed.
    set.seed(randSeed)
    # Get the traning indexes.
    trainInd <- sample(
      seq_len(nrow(datamodel)), 
      size=floor(nrow(datamodel)*propTraining)
    )
    
    # Split into training and testing sets.
    train <- datamodel[trainInd, ]
    test <- datamodel[-trainInd, ]
   
    
    # Suppress any warning in the fitting process.
    suppressWarnings(library(caret))
    
    # Set the repeated CV params.
    TrControl <- trainControl(
      method="cv",
      number=numFolds
    )
    
    # Increment the progress bar, and update the detail text.
    progress$inc(0.2, detail = "Logistic Regression")
    
    # Evaluate the logistic regression through CV.
    logRegModel = train(
      condition ~ ., 
      data=train[,c(c("condition"),Vars)],
      method = "glm",
      family = "binomial",
      metric="Accuracy",
      trControl=TrControl
    )
    
    # Increment the progress bar, and update the detail text.
    progress$inc(0.4, detail = "Classification Tree")
    
    # Let caret choose the best tree through CV.
    treeModel = train(
      condition ~ ., 
      data=train[,c(c("condition"),Vars)],
      method="rpart", 
      metric="Accuracy",
      tuneGrid=expand.grid(cp = Cps),
      trControl=TrControl
    )
    
    # Increment the progress bar, and update the detail text.
    progress$inc(0.6, detail = "Random Forest")
    
    # Let caret choose the best random forest through CV.
    rfModel = train(
      condition ~ ., 
      data=train[,c(c("condition"),Vars)],
      method="rf", 
      metric= "Accuracy",
      tuneGrid=expand.grid(mtry = randForMtry),
      trControl=TrControl
    )
    
    # Increment the progress bar, and update the detail text.
    progress$inc(0.8, detail = "Evaluating train Set Performance")

    # Get train set predictions.
    logRegPreds <- predict(logRegModel, train, type="raw")
    treePreds <- predict(treeModel, train, type="raw")
    randForPreds <- predict(rfModel, train, type="raw")

    # Get test set predictions.
    logRegPredstest <- predict(logRegModel, test, type="raw")
    treePredstest <- predict(treeModel, test, type="raw")
    randForPredstest <- predict(rfModel, test, type="raw")
    
    
    output$randinfo<-renderText(
      text<-paste0("Confusion matrix results of test set for RandomForest model ")
    )
    output$randmodelresults<-renderPrint(
      confusionMatrix(data = test$condition, 
                      reference=randForPredstest)
    )
    
    # Create the findMode function.
    findMode <- function(x) {
      uniqueX <- unique(x)
      uniqueX[which.max(tabulate(match(x, uniqueX)))]
    }
    # 
    # Find the no-info rate for train.
     noInfoRate <- mean(findMode(train$condition) == train$condition)
     
     # Find the no-info rate test.
     noInfoRatetest <- mean(findMode(test$condition) == test$condition)
     
    # Find the train set performances.
    accVec <- c(
      noInfoRate,
      mean(logRegPreds == train$condition, na.rm=TRUE),
      mean(treePreds == train$condition, na.rm=TRUE),
      mean(randForPreds == train$condition, na.rm=TRUE)
    )
    
    # Find the test set performances.
    accVectest <- c(
      noInfoRatetest,
      mean(logRegPredstest == test$condition, na.rm=TRUE),
      mean(treePredstest == test$condition, na.rm=TRUE),
      mean(randForPredstest == test$condition, na.rm=TRUE)
    )

    # Convert to a matrix and percentages.
    accMatrix <- t(as.matrix(accVec)) * 100
    accMatrixtest <- t(as.matrix(accVectest)) * 100
    
    # Add informative column names.
    colnames(accMatrix) <- c(
      "No Information Rate",
      "Logistic Regression",
      paste0("Tree (Cp = ", treeModel$bestTune$cp, ")"),
      paste0("Random Forest (mtry = ", rfModel$bestTune$mtry, ")")
    )
    # Add informative column names.
    colnames(accMatrixtest) <- c(
      "No Information Rate",
      "Logistic Regression",
      paste0("Tree (Cp = ", treeModel$bestTune$cp, ")"),
      paste0("Random Forest (mtry = ", rfModel$bestTune$mtry, ")")
    )
    # Convert the matrix to a dataframe.
    accTable <- as.data.frame(accMatrix) %>%
      mutate_all(
        round, digits = 3
      ) %>%
      mutate_all(
        paste0, sep="%"
      )
    # Convert the matrix to a dataframe.
    accTabletest <- as.data.frame(accMatrixtest) %>%
      mutate_all(
        round, digits = 3
      ) %>%
      mutate_all(
        paste0, sep="%"
      )

   # Create the output for the accuracy table.
   output$accTableOutput <- renderDataTable({
     datatable(accTable)
   })
   output$accTableOutputTest <- renderDataTable({
     datatable(accTabletest)
   })
  
  # Create an output for the logistic regression model rounding to 3 decimals.
  output$logRegSummary <- renderDataTable({
    round(as.data.frame(summary(logRegModel)$coef), 3)
  })
  
 
  # Create a nice tree diagram.
  output$treeSummary <- renderPlot({
    fancyRpartPlot(treeModel$finalModel)
  })
  

 # Create an output for the feature importance plot for random forest model.
  output$rfVarImpPlot <- renderPlot({
    ggplot(varImp(rfModel, type=2)) +
      geom_col(fill="purple") +
      ggtitle("Most Important Features by Decrease in Gini Impurity")
  })
  

  # Save the fitted models in a folder.
  saveRDS(logRegModel, "./Fitted Models/logRegModel.rds")
  saveRDS(treeModel, "./Fitted Models/treeModel.rds")
  saveRDS(rfModel, "./Fitted Models/rfModel.rds")

})

  
  output$logRegPredInputs <- renderUI({
    
    ###
    # Create a UI that lets the user input values for the logistic regression 
    # and get a prediction.
    ###
    
    # Get the variables to use for each model.
    Vars <- input$checkGroup
    
    # Loop through the variables and create numeric input boxes for them. Use
    # the median of the variable for the default value.
    tags$ul(tagList(
      lapply(Vars, function(variable) {
        numericInput(
          inputId = paste0(variable, "Value"),
          label = paste0("Input ", variable, " Value"),
          value = median(pull(datamodel[, variable]), na.rm=TRUE),
          step = 0.1
        )
      })
    ))
  })
  
  output$treePredInputs <- renderUI({
    
    ###
    # Create a UI that lets the user input values for the tree model and get a 
    # prediction.
    ###
    
    # Get the variables to use for each model.
    Vars <- input$checkGroup
    
    # Loop through the variables and create numeric input boxes for them. Use
    # the median of the variable for the default value.
    tags$ul(tagList(
      lapply(Vars, function(variable) {
        numericInput(
          inputId = paste0(variable, "Value"),
          label = paste0("Input ", variable, " Value"),
          value = median(pull(datamodel[, variable]), na.rm=TRUE),
          step = 0.1
        )
      })
    ))
  })
  
  output$randForPredInputs <- renderUI({
    
    ###
    # Create a UI that lets the user input values for the random forest model  
    # and get a prediction.
    ###
    
    # Get the variables to use for each model.
    Vars <- input$checkGroup
    
    # Loop through the variables and create numeric input boxes for them. Use
    # the median of the variable for the default value.
    tags$ul(tagList(
      lapply(Vars, function(variable) {
        numericInput(
          inputId = paste0(variable, "Value"),
          label = paste0("Input ", variable, " Value"),
          value = median(pull(datamodel[, variable]), na.rm=TRUE),
          step = 0.1
        )
      })
    ))
  })
  
  observeEvent(input$predStart, {
    
    ###
    # Return predictions when the user wants them.
    ###
    
    # Retrieve the model to use for prediction.
    modelType <- input$modelType
    
    # Load the appropriate model based on user input.
    if (modelType == "logReg"){
      
      # Get the names of the user inputs for the logistic regression model.
      varsOfInterest <- unlist(lapply(input$checkGroup, paste0, sep="Value"))
      # Load in the logistic regression model.
      myModel <- readRDS("./Fitted Models/logRegModel.rds")
      
    } else if (modelType == "tree"){
      
      # Get the names of the user inputs for the tree model.
      varsOfInterest <- unlist(lapply(input$checkGroup, paste0, sep="Value"))
      # Load in the tree model.
      myModel <- readRDS("./Fitted Models/treeModel.rds")
      
    } else {
      
      # Get the names of the user inputs for the random forest model.
      varsOfInterest <- unlist(lapply(input$checkGroup, paste0, sep="Value"))
      # Load in the random forest model.
      myModel <- readRDS("./Fitted Models/rfModel.rds")
      
    }
    
    # Loop through the user inputs adding them to a vector because you cannot
    # access the variables by simply passing the vector of list elements to 
    # input.
    inputCopy <- c()
    for (variable in varsOfInterest){
      inputCopy <- c(inputCopy, input[[variable]])
    }
    
    # Create a 1-row matrix of the user inputs.
    inputCopy <- t(matrix(inputCopy))
    # Strip "Value" from the variable names to match the column names in
    # datamodel
    colnames(inputCopy) <- str_remove_all(varsOfInterest, pattern="Value")

    # Create a data.frame from the user inputs.
     userInputs <- as.data.frame(inputCopy)

    # Get class and probability predictions for the user inputs.
    probPred <- predict(myModel, userInputs, type="prob")
    # Combine them into a single matrix and round probabilities to 5 decimals.
    preds <-  round(probPred, 5)
   
    # Add informative column names.
    colnames(preds) <- c(
      "Predicted Prob. of HeartDisease",
      "Predicted Prob. of No HeartDisease"
    )

    # Convert the preds matrix to a data.frame.
    preds <- as.data.frame(preds)

    # Return the predictions.
    output$preds <- renderDataTable({
      preds
    })
})


  # Return the output.
  return(output)

  })#end of shiny and function session