###
# author: Ipsita Datta
# date: 11/25/2021
# purpose: Create the front-end UI for the app exploring HeartDisease
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

# Get the location and name of the image for the About tab.
imageName <- c("heart.png")

shinyUI(navbarPage(
  
  title = "Project3 HeartDiseases Data of Clevland",
  # Add a theme.
  theme = shinytheme("flatly"),
  # create tab for 3 different subtab pages
    tabsetPanel(
     tabPanel(
      title="About",icon = icon("question"),
      mainPanel(
        # include image related to data
        img(
          src = imageName, 
          height = '260px', 
          width = '360px'
        ),
        h3("App Description"),
        "This app is about heart Disease data from Cleveland.",
        "It demonstrates an exploratory data analysis of the popular
         Heart Disease  database.Heart disease prediction is carried out
         using 3 different models such as logistic regression,classification Trees
         and Random Forest",
        h3("Data Description"),
        "The data collected from",
        a(href = "https://www.kaggle.com/cherngs/heart-disease-cleveland-uci/version/1",
          "Heart Disease  database"),
        "This data set is for cleveland and it uses only 14 for the analysis. You can
        see all the attributes details explanation in the above link.",
        h3("Tab Explanation"),
        tags$ul(
          tags$li(
            "Data Tab:  Scroll through the data set.",
            " User can Subset this data set (by rows and columns) and ",
            " Download the data "
          ),
          tags$li(
            "Data Exploration: Create numerical and graphical summaries",
            " Change the type of plot and type of summary reported",
            "Change the variables and filter the rows to change the data in the plots/summaries"
            
          ),
          tags$li(
            "Modeling: It gives information, fitting and prediction on  three supervised learning models. "
            
          )
        )
        
)#close main panel
),#close app description tab
    tabPanel(
      title = "Data Exploration",icon = icon("cogs"),
      sidebarPanel(
        selectInput("predictor", h5(strong("Choose Predictor")), c( "condition","cp","sex","exang")),
        selectInput("plotType",h5(strong("Type of Diagram")),c("Barchart","Histogram","Boxplot")),
        conditionalPanel(condition = "input.plotType == 'Histogram' ",
                         checkboxInput("density",h6("Add a desity curve to the plot"),value = FALSE)
                         ),
        selectInput("summary",h5(strong("Type of Summary Report")),c("Frequency","Proportions"))
      ),
      mainPanel(
        
          plotOutput("plot1"),
          dataTableOutput("dTable"),
          textOutput("info"),
          verbatimTextOutput("allsummary")
        ) # close of main panel
      ), #closes the data exploration tab
    
    tabPanel(
      title="Data",icon = icon("database"),
        sidebarPanel(
          
          checkboxGroupInput("checkboxGroup", label = h4("Let the user select the variables to subset data: "), 
                             choices = colnames(select_all(heartDiseaseData)) , inline = F,
                             selected = colnames(select_all(heartDiseaseData))),
          radioButtons("rb",h5(strong("Select the Sex")),c("Female","Male")),
          radioButtons("rb2",h5(strong("Select the Condition")),c("No Heart Disease","Heart Disease")),
          
        downloadButton("downloadData", "Download",icon = shiny::icon("download"))
      ),
      mainPanel(
        dataTableOutput("HDdata")
      )
    ) ,#closes the data tab
navbarMenu(
  
  # Add a title.
  title="Modeling Page",icon = icon("hornbill"),
  
  tabPanel(
    title = "Modeling Info",
    mainPanel(fluidPage(
      
      br(),
      # Give an overview of logistic regression.
      h4(strong("Logistic Regression")),
      "Logistic regression is the appropriate regression analysis to conduct",
      "when the dependent variable is dichotomous (binary). ...",
      "It is used to describe data and to explain the ",
      "relationship between one dependent binary variable and one or more nominal,",
      "ordinal, interval or ratio-level independent variables.",
      uiOutput("logReg"),
      h5(strong("Benefits")),
      "Logistic regression is easier to implement, interpret, and very efficient to train.",
      "It makes no assumptions about distributions of classes in feature space.",
      "It can easily extend to multiple classes(multinomial regression) and a natural ",
      "probabilistic view of class predictions.Good accuracy for many simple data sets",
      "and it performs well when the dataset is linearly separable.",
      
      h5(strong("Drawbacks")),
      "The major limitation of Logistic Regression is the assumption of linearity between",
      "the dependent variable and the independent variables.It can only be used to predict",
      "discrete functions. Hence, the dependent variable of Logistic Regression is bound to",
      "the discrete number set.Logistic Regression requires average or no multicollinearity",
      "between independent variables.",
      
      h4(strong("Classification Trees")),
      "A classification tree, or simply tree, is an algorithm ",
      "which recursively splits a feature space to create  ",
      "regions where observations are classified by the ",
      "most dominant class in the region.",
      br(),
      "Trees are also prone to overfitting (high variance).",
      "They do have the benefit of being highly interpretable. ",
      br(),
      h5(strong("Benefits")),
      "The Classification  methodology is one of the ",
      "oldest and most fundamental algorithms. It is used to predict outcomes",
      "based on certain predictor variables. They are excellent for data mining ",
      "tasks because they require very little data pre-processing.",
      br(),
      h5(strong("Drawbacks")),
      "The disadvantage of classification tree often involves higher time to train ",
      "the model. Decision tree training is relatively expensive as the complexity ",
      "and time has taken are more. The Decision Tree algorithm is inadequate for ",
      "applying regression and predicting continuous values.",
      
      
      h4(strong("Random Forests")),
      "A random forest model is a supervised learning algorithm which extend the",
      "idea of bagging method  to solve regression or classification problems.",
      "It creates multiple trees from bootstrap samples like as bagging method ",
      "and then use a random subset of predictors for each bootstrap sample/tree fit.", 
      br(),
      " By aggregating the predictions of the ",
      "independent trees, we reduce the variance of the ",
      "predictions. Random forests are typically good out-of-the",
      "-box predictive models, but unfortunately lose the ",
      "interpretability that stand-alone trees have.",
      br(),
      h5(strong("Benefits")),
      "It is robust to outliers. It works well with non-linear data. Lower risk",
      "of overfitting. It runs efficiently on a large dataset.",
      br(),
      h5(strong("Drawbacks")),
      "Random forests are found to be biased while dealing with categorical ",
      "variables.It slow training method. It is not suitable for linear methods",
      "with a lot of sparse features."
    ))
  ),
  # Add a tab for fitting the models.
  tabPanel(
    # Add a title for the sub tab.
    title = "Model Fitting",
    # Allow the user to set a random seed between -500 and 500.
    sidebarPanel(
      h3("Train-Test Split"),
      numericInput(
        inputId = "randSeed",
        label = "Set Random Seed",
        value = 1,
        min = -500,
        max = 500,
        step = 1
      ),
      # Allow the user to select the proportion of data to use for
      # a training set.
      numericInput(
        inputId = "propTraining",
        label = "Proportion of Data to use for Train Set",
        value = 0.8,
        min = 0.3,
        max = 0.8,
        step = 0.05
      ),
      
      # Create a section for the cross-validation parameters.
      h3("Cross-Validation"),
        div(
        numericInput(
          inputId = "numFolds",
          label = "Number of Folds",
          value = 3,
          min = 3,
          max = 5,
          step = 1
        )
      ),
      
      # Let the user set which variables to use.
      checkboxGroupInput("checkGroup", label = h4("Let the user set which variables to use for models: "), 
                         choices = colnames(select(datamodel, -condition)) , inline = F,
                         selected = colnames(select(datamodel, -condition))),
      # Add inputs to take in the tree parameters.
      h4(tags$b("Complexity Parameter:")),
      div(
        uiOutput("minCpInput"),  
        style="display:inline-block"
      ),
      br(),
      div(
        uiOutput("maxCpInput"),  
        style="display:inline-block"
      ),
      div(
        numericInput(
          inputId = "numCps",
          label = "no. of Values", 
          min = 1, 
          max = 5, 
          value = 3,
          step = 1
        ),  
        style="display:inline-block"
      ),
      
      # Let the user select the number of variables to consider
      # at each split.
      selectizeInput(
        inputId = "randForMtry", 
        label = "Select up to 5 values for mtry:", 
        choices = 1:length(colnames(datamodel)[1:13]),
        multiple = TRUE,
        selected = c(2, 6, 8),
        options = list(maxItems = 5)
      ),
      # Add a button for fitting models.
      actionButton(
        inputId = "trainStart",
        label = "Fit Models"
      )
    ),
    # Create the main panel to hold model performances and 
    # summaries.
    mainPanel(
      # Show the Train-set accuracy.
      h3("Train Set Accuracies upto 3 decimal places"),
      dataTableOutput("accTableOutput"),
      br(),
      h3("Test Set Accuracies upto 3 decimal places"),
      dataTableOutput("accTableOutputTest"),
      br(),
      # Show the coefficients of the Logistic Regression Model .
      h3("Summary of Logistic Regression Model for Train set"),
      dataTableOutput("logRegSummary"),
      br(),
      
      # Show the final tree diagram.
      h3("Classification Tree Diagram "),
      plotOutput("treeSummary"),
      br(),
      
      h3("Random Forest Feature Importances for Train set"),
      plotOutput("rfVarImpPlot"),
      
      textOutput("randinfo"),
      verbatimTextOutput("randmodelresults")
      
    )
  ),
  
  # Create the prediction tab.
  tabPanel(
    # Add a title.
    title = "Prediction",
    sidebarPanel(
      # Add buttons to select which model to use.
      radioButtons(
        inputId = "modelType",
        label = "Choose a Model",
        inline = TRUE,
        choiceNames = c("Logistic Regression","Classification Tree","Random Forest"),
        choiceValues = c("logReg", "tree", "randFor"),
        selected = "logReg"
      ),
      # Add a button for fitting models.
      actionButton(
        inputId = "predStart",
        label = "Predict"
      ),
      # Depending on which model to use for prediction, change the
      # variables shown to match the fitted models.
      conditionalPanel(
        condition = "input.modelType == 'logReg'",
        uiOutput("logRegPredInputs")
      ),
      conditionalPanel(
        condition = "input.modelType == 'tree'",
        uiOutput("treePredInputs")
      ),
      conditionalPanel(
        condition = "input.modelType == 'randFor'",
        uiOutput("randForPredInputs")
      )
    ),
    # Create the main panel to show predictions.
    mainPanel(
      h3("Predicted Heart disease of a person with selected Inputs"),
      dataTableOutput("preds")
    )
  )
)
)
)
)
