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
        "This app is about heart Disease data from Cleveland",
        "It demonstrates an exploratory data analysis of the popular
         Heart Disease  database.Heart disease prediction is carried out
         using 3 different models such as linear regression,classification Trees
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
            "Data Tab:  Scroll through the data set",
            " Subset this data set (by rows and columns)",
            " Download the data "
          ),
          tags$li(
            "Data Exploration: Create numerical and graphical summaries",
            " Change the type of plot and type of summary reported",
            "Change the variables and filter the rows to change the data in the plots/summaries"
            
          ),
          tags$li(
            "Modeling: Gives information, fitting and prediction on  three supervised learning models, "
            
          )
        )
        
)#close main panel
),#close app description tab
    tabPanel(
      title = "Data Exploration",icon = icon("cogs"),
      sidebarPanel(
        selectInput("predictor", h5(strong("Choose Predictor")), c( "condition","cp","sex","exang")),
        selectInput("plotType",h5(strong("Type of Diagram")),c("Barchart","Histogram","Boxplot")),
        selectInput("summary",h5(strong("Type of Summary Report")),c("Frequency","Proportions"))
        
      ),
      mainPanel(
        
          plotOutput("plot1"),
          dataTableOutput("dTable")
        ) # close of main panel
      ), #closes the data exploration tab
    
    tabPanel(
      title="Data",icon = icon("database"),
        sidebarPanel(
        
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
      # Give an overview of the modeling approaches
      br(),
      # Give an overview of logistic regression.
      h4("Logistic Regression"),
      "Logistic regression is the appropriate regression analysis to conduct",
      "when the dependent variable is dichotomous (binary). ...",
      "Logistic regression is used to describe data and to explain the ",
      "relationship between one dependent binary variable and one or more nominal,",
      "ordinal, interval or ratio-level independent variables.",
      uiOutput("logReg"),
      h5("Benefits"),
      "Logistic regression is easier to implement, interpret, and very efficient to train.",
      "It makes no assumptions about distributions of classes in feature space.",
      "It can easily extend to multiple classes(multinomial regression) and a natural ",
      "probabilistic view of class predictions.Good accuracy for many simple data sets",
      "and it performs well when the dataset is linearly separable.",
      
      h5("Drawbacks"),
      "The major limitation of Logistic Regression is the assumption of linearity between",
      "the dependent variable and the independent variables.It can only be used to predict",
      "discrete functions. Hence, the dependent variable of Logistic Regression is bound to",
      "the discrete number set.Logistic Regression requires average or no multicollinearity",
      "between independent variables.",
      
      h4("Classification Trees"),
      "A classification tree, or simply tree, is an algorithm ",
      "which recursively splits a feature space to create  ",
      "regions where observations are classified by the ",
      "most dominant class in the region. Probabilities are the ",
      "relative frequency of each class in a terminal node.",
      br(),
      "Each split is made to reduce the training error as much  ",
      "possible for that split; not for future splits. This  ",
      "makes classification trees greedy algorithms. The best ",
      " split at time ", strong("t"), 
      " may not be the best split for the final ",
      "fit. Trees are also prone to overfitting (high variance).",
      "They do have the benefit of being highly interpretable. ",
      br(),
      h5("Benefits"),
      "The Classification  methodology is one of the ",
      "oldest and most fundamental algorithms. It is used to predict outcomes",
      "based on certain predictor variables. They are excellent for data mining ",
      "tasks because they require very little data pre-processing.",
      br(),
      h5("Drawbacks"),
      "The disadvantage of classification tree often involves higher time to train ",
      "the model. Decision tree training is relatively expensive as the complexity ",
      "and time has taken are more. The Decision Tree algorithm is inadequate for ",
      "applying regression and predicting continuous values.",
      
      
      h4("Random Forests"),
      "Random forests create bootstrap samples of the training ",
      "data and grow classification or regression trees on each ",
      "sample. At each split, the trees are restricted to a ",
      "subset of the features. For classification, the trees ",
      "take a majority vote on the class of the new data. For ",
      "regression, their predictions are averaged.",
      br(),
      "Only considering a subset of features at each split ",
      "prevents a handful of features from dominating the early ",
      "splits in each tree and makes each tree more independent ",
      "(hopefully). By aggregating the predictions of the ",
      "independent trees, we reduce the variance of the ",
      "predictions. Random forests are typically good out-of-the",
      "-box predictive models, but unfortunately lose the ",
      "interpretability that stand-alone trees have.",
      br(),
      h5("Benefits"),
      "It is robust to outliers. It works well with non-linear data. Lower risk",
      "of overfitting. It runs efficiently on a large dataset.",
      br(),
      h5("Drawbacks"),
      "Random forests are found to be biased while dealing with categorical ",
      "variables.It slow training method. It is not suitable for linear methods",
      "with a lot of sparse features."
    ))
  ),
  # Add a tab for fitting the models.
  tabPanel(
    # Add a title for the sub tab.
    title = "Model Fitting",
    # Allow the user to set a random seed between -1000 and 1000.
    sidebarPanel(
      h3("Train-Test Split"),
      numericInput(
        inputId = "randSeed",
        label = "Set Random Seed",
        value = 1,
        min = -1000,
        max = 1000,
        step = 1
      ),
      # Allow the user to select the proportion of data to use for
      # a testing set.
      numericInput(
        inputId = "propTesting",
        label = "Proportion of Data to use for Test Set",
        value = 0.2,
        min = 0.1,
        max = 0.5,
        step = 0.05
      ),
      
      # Create a section for the cross-validation parameters.
      h3("Cross-Validation"),
      # Set the number of folds.
      div(
        numericInput(
          inputId = "numFolds",
          label = "Number of Folds",
          value = 3,
          min = 3,
          max = 5,
          step = 1
        )#,
        #style="display:inline-block"
      ),
      
      # Let the user set which variables to use.
      checkboxGroupInput("checkGroup", label = h4("Let the user set which variables to use for models: "), 
                         choices = colnames(select(datamodel, -condition)) , inline = F,
                         selected = colnames(select(datamodel, -condition))),
      # Add side-by-side inputs to take in the tree parameters.
      h4(tags$b("Complexity Parameter:")),
      div(
        uiOutput("minCpInput"),  
        style="display:inline-block"
      ),
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
      # Show the test-set accuracy.
      h3("Test Set Accuracies to 3 decimal places"),
      dataTableOutput("accTableOutput"),
      br(),
      # Show the coefficients of the Logistic Regression Model.
      h3("Summary of Linear Regression Model"),
      dataTableOutput("logRegSummary"),
      tableOutput('logRegconfusionMatrix'),
      verbatimTextOutput("logRegclassError"),
      br(),
      # Show the final tree diagram.
      h3("Classification Tree Diagram"),
      plotOutput("treeSummary"),
      tableOutput('treeconfusionMatrix'),
      verbatimTextOutput("treeclassError"),
      br(),
      h3("Random Forest Feature Importances"),
      plotOutput("rfVarImpPlot"),
      verbatimTextOutput("rfVarclassError")
    )
  ),
  
  # Create the prediction tab.
  tabPanel(
    # Add a title.
    title = "Prediction",
    # Create a sidebar for the user to play with inputs.
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
