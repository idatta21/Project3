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


shinyServer(function(input, output, session) {
  
  
  output$plot1<- renderPlot({
    xvalue<-input$predictor
    if (input$plotType == "Barchart"){
       ggplot(data = heartDiseaseData,aes(x=pull(heartDiseaseData,xvalue)))+
       geom_bar(aes(fill=pull(heartDiseaseData,xvalue))) +labs(x="")+theme(legend.title = element_blank())+
       ggtitle("Bar Chart based on predictors")
       }
    else if ( input$plotType == "Histogram"){
      ggplot(heartDiseaseData,aes(age, fill=pull(heartDiseaseData,xvalue))) + 
        geom_histogram(aes(y=..density..),breaks=seq(0, 80, by=1), color="grey18") +
        geom_density(alpha=.2, fill="black")+   labs(x="Age") + 
        ylab("Density / Count") + theme(legend.title = element_blank())+
        ggtitle("Heart disease  spread out across age based on predictors")
    }
  
  else if(input$plotType == "Boxplot"){
    ggplot(heartDiseaseData,aes(x=pull(heartDiseaseData,xvalue), y=age, fill=pull(heartDiseaseData,xvalue))) + 
      geom_boxplot() + theme(legend.title = element_blank())+labs(x="")
   }
  })#end of plot
  })#end of shiny and function session