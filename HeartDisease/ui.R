library(shiny)
library(readr)
library(readxl)
library(dplyr)
library(DT)
library(caret)
library(tidyverse)
library(plotly)

heartDiseaseData <- read_csv("heart_cleveland.csv", col_names = TRUE)

#Factoring the features
#condition
heartDiseaseData$condition<-as.factor(heartDiseaseData$condition)
levels(heartDiseaseData$condition)[levels(heartDiseaseData$condition)==0] <- "No Heart Disease"
levels(heartDiseaseData$condition)[levels(heartDiseaseData$condition)==1] <- "Heart Disease"
#Chest Pain
heartDiseaseData$cp<-as.factor(heartDiseaseData$cp)
levels(heartDiseaseData$cp)[levels(heartDiseaseData$cp)==0] <- "Chest Pain Type 0"
levels(heartDiseaseData$cp)[levels(heartDiseaseData$cp)==1] <- "Chest Pain Type 1"
levels(heartDiseaseData$cp)[levels(heartDiseaseData$cp)==2] <- "Chest Pain Type 2"
levels(heartDiseaseData$cp)[levels(heartDiseaseData$cp)==3] <- "Chest Pain Type 3"
#Sex
heartDiseaseData$sex<-as.factor(heartDiseaseData$sex)
levels(heartDiseaseData$sex)[levels(heartDiseaseData$sex)==0] <- "Male"
levels(heartDiseaseData$sex)[levels(heartDiseaseData$sex)==1] <- "Female"
#Thalassemia
heartDiseaseData$thal<-as.factor(heartDiseaseData$thal)
levels(heartDiseaseData$thal)[levels(heartDiseaseData$thal)==0] <- "No Thalassemia"
levels(heartDiseaseData$thal)[levels(heartDiseaseData$thal)==1] <- "Normal Thalassemia"
levels(heartDiseaseData$thal)[levels(heartDiseaseData$thal)==2] <- "Fixed Defect Thalassemia"
levels(heartDiseaseData$thal)[levels(heartDiseaseData$thal)==3] <- "Reversible Defect Thalassemia"
#Fasting Blood Sugar
heartDiseaseData$fbs<-as.factor(heartDiseaseData$fbs)
levels(heartDiseaseData$fbs)[levels(heartDiseaseData$fbs)==0] <- "Fasting Blood Sugar <= 120"
levels(heartDiseaseData$fbs)[levels(heartDiseaseData$fbs)==1] <- "Fasting Blood Sugar > 120"
#Exercise Induced Angina
heartDiseaseData$exang<-as.factor(heartDiseaseData$exang)
levels(heartDiseaseData$exang)[levels(heartDiseaseData$exang)==1] <- "Exercise Induced Angina"
levels(heartDiseaseData$exang)[levels(heartDiseaseData$exang)==0] <- "No Exercise Induced Angina"
#ECG
heartDiseaseData$restecg<-as.factor(heartDiseaseData$restecg)
levels(heartDiseaseData$restecg)[levels(heartDiseaseData$restecg)==0] <- "Rest ECG 0"
levels(heartDiseaseData$restecg)[levels(heartDiseaseData$restecg)==1] <- "Rest ECG 1"
levels(heartDiseaseData$restecg)[levels(heartDiseaseData$restecg)==2] <- "Rest ECG 2"
#ST Slope
heartDiseaseData$slope<-as.factor(heartDiseaseData$slope)
levels(heartDiseaseData$slope)[levels(heartDiseaseData$slope)==0] <- "Peak Excercise ST Slope 0"
levels(heartDiseaseData$slope)[levels(heartDiseaseData$slope)==1] <- "Peak Excercise ST Slope 1"
levels(heartDiseaseData$slope)[levels(heartDiseaseData$slope)==2] <- "Peak Excercise ST Slope 2"

shinyUI(navbarPage(
  
  title = "Project3 HeartDiseases Data of Clevland",
    tabsetPanel(
     tabPanel(
      title="About",
      mainPanel(
        h3("App Description"),
        "This app is about heart Disease data from Clevland",
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
            " Subset this data set (rows and columns)",
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
        

#need to include image too
)#close main panel
),#close app description tab
    tabPanel(
      title = "Data Exploration",
      sidebarPanel(
        selectInput("predictor", h5(strong("Choose Predictor")), c( "condition","cp","sex","exang")),
        selectInput("plotType",h5(strong("Type of Diagram")),c("Barchart","Histogram","Boxplot")),
        selectInput("summary",h5(strong("Type of Summary")),c("FivePoint Summary","Mean","IQR"))
      ),
      mainPanel(
        
          plotOutput("plot1")
        ) # close of main panel
      ), #closes the data exploration tab
    tabPanel(
      title="Modeling Page",
       sidebarPanel(
         selectInput("super",h5(strong("Choose Supervise Learning Model")),
                     c("Linear regression model","Classification
                       tree","Random Forest Model"))
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            title = "Modeling Info",
            mainPanel(
             verbatimTextOutput("info")
            )
          ),
          tabPanel(
            title = "Model Fitting",
             mainPanel(
              verbatimTextOutput("fit")
            )
          ),
          tabPanel(
            title = "Prediction",
            mainPanel(
             verbatimTextOutput("pred")
            )
          )
        
      )# close of subtab
    )# close of sub main paneltab
),# close modeling page tab
    tabPanel(
      title="Data",
        sidebarPanel(
        
        downloadButton("downloadData", "Download",icon = shiny::icon("download"))
      ),
      mainPanel(
        dataTableOutput("HDdata")
      )
    ) #closes the data tab
  ) #tabsetpanel
)) #shinyUI and navbar
