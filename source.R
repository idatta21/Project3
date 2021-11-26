#Read in raw data
heartDiseaseData <- read_csv("heart_cleveland.csv", col_names = TRUE)
datamodel<-heartDiseaseData
datamodel$condition <- as.factor(datamodel$condition)
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
