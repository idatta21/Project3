library(shiny)
library(readr)
library(tidyverse)


HDdata <- read_delim("cleveland_HeartDiseases_Data.data", delim = ",", col_names = FALSE)
colnames(HDdata) <- c("age", "sex", "cp", "trestbps", "chol", "fbs", "restecg", "thalach", "exang", "oldpeak", "slope","ca","thal","num")

shinyUI(navbarPage(
  
  title = "Project3 HeartDiseases Data of Clevland",
    tabsetPanel(
     tabPanel(
      title="About",
      mainPanel(
        h3("App Description"),
        "This app is about heart Disease data from Clevland", 
        h3("Data Description"),
        "The data collected from",
        a(href = "https://archive.ics.uci.edu/ml/datasets/Heart+Disease",
          "Heart Disease Data Set"),
        h3("Tab Explanation"),
        " describe  the purpose of each tab (page) of the app"
        # need to include image too  
      ) # close main panel
    ),# close app description tab
    tabPanel(
      title = "Data Exploration",
      sidebarPanel(
        selectInput("pred",h5(strong("Variables to Plot")),c("Age","Sex","chol")),
        selectInput("plotType",h5(strong("Type of Diagram")),c("Barchart","Histogram","Boxplot")),
        selectInput("summary",h5(strong("Type of Summary")),c("FivePoint Summary","Mean","IQR")),
      ),
      mainPanel(
        conditionalPanel(
          condition = "input.plotType == 'Barchart'",
          plotOutput("barchart")
        ),
        conditionalPanel(
          condition = "input.plotType == 'Histogram'",
          plotOutput("histogram")
        ),
        conditionalPanel(
          condition = "input.plotType == 'Boxplot'",
          plotOutput("boxplot")
        ),
        conditionalPanel(
          condition = "input.summary == 'FivePoint Summary'",
          dataTableOutput("fivepoint")
        ),
        conditionalPanel(
          condition = "input.summary == 'Mean'",
          dataTableOutput("mean")
        ),
        conditionalPanel(
          condition = "input.summary == 'IQR'",
          dataTableOutput("fivepoint")
        )
        ) # close of main panel
      ), #closes the data exploration tab
    tabPanel(
      title="Modeling Page",
       sidebarPanel(
         selectInput("super",h5(strong("Choose Supervise Learning Model")),
                     c("Linear regression model","Classification
                       tree","Random Forest Model")),
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            title = "Modeling Info",
            #mainPanel(
             #verbatimTextOutput("info") 
            #)
          ),
          tabPanel(
            title = "Model Fitting",
            #mainPanel(
             # verbatimTextOutput("fit") 
            #)
          ),
          tabPanel(
            title = "Prediction",
            #mainPanel(
             # verbatimTextOutput("pred") 
            #)
          )
        #dataTableOutput("supertab")
      )
    )
),# close modeling page tab
    tabPanel(
      title="Data",
        sidebarPanel(
         selectInput(
          inputId = "columns",
          label = "Data Visualization",
          choices = colnames(HDdata),
          selected = colnames(HDdata),
          multiple = TRUE
        ),
        downloadButton("downloadData", "Download")
      ),
      mainPanel(
        dataTableOutput("HDdata")
      )
    ) #closes the data tab
  ) #tabsetpanel 
)) #shinyUI and navbar
