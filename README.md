## Project3
### Overview
It is a well known fact that Heart Diseases are currently the leading cause of death across the globe. The development of a computational system that can predict the presence of heart diseases in patients will significantly reduce the mortality rates and substantially reduce the costs of healthcare. Machine learning is used across many spheres around the world. Especially it is gaining more popularity in the healthcare industry. Machine learning can play an essential role in predicting presence or absence of a critical disease, for an instance, Heart disease, etc. If such information is predicted well in advance, can provide important insights to the doctors, who can then carry out the treatments of the patients accordingly and efficiently. The ShinyApp  demonstrates an exploratory data analysis of the popular Heart Disease UCI database. In addition to that, heart disease prediction is carried out using different approaches such as logistic regression, Classification Tree and Random Forest Models.

### The following packages are used for this app:

[`shiny`]: app framework
[`shinyWidgets`]: additional functionality for Shiny
[`shinythemes`]: prettying up the app
[`DT`]: additional functionality for Shiny
[`readr`]: reading in data
[`tidyverse`]: data manipulation and visualization
[`plotly`]: additional visualization functionality
[`imager`]: working with images
[`caret`]: machine learning
[`rattle`]: visualizing the tree model

### To install them all, run this code chunk:

````````
install.packages("shiny")
install.packages("shinyWidgets")
install.packages("shinythemes")
install.packages("DT")
install.packages("readr")
install.packages("tidyverse")
install.packages("plotly")
install.packages("imager")
install.packages("caret")
install.packages("rattle")
````````
### The  code that you can copy and paste into RStudio to run this app.
```
shiny::runGitHub("idatta21/Project3", ref="main")
```