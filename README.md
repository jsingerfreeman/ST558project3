# Final Project - Statistics 558 - NCSU
#### By: Jose Singer-Freeman
#### Due date:  July 31, 2023


### Background
<p>This app allows the user to explore a dataset of medical records of 299 heart failure patients.  The user may  predict the probability of a death event occuring using three types of machine learning supervised classification models.  </p>
<p>The dataset was downloaded from <a href="https://www.kaggle.com/datasets/andrewmvd/heart-failure-clinical-data" target="_blank">Heart Failure Clinical Data on Kaggle</a>.</p>
                      

###  List of Packages Needed for the App
shiny <br>
shinydashboard <br>
tidyverse <br>
knitr <br>
tidyr <br>
ggplot2 <br>
caret <br>
DT <br>
shinyWidgets 


 ###  Code to Install the Packages (if not already installed)
 ```
packages<-c("shiny", "shinydashboard", "tidyverse","knitr","tidyr","ggplot2","caret","DT","shinyWidgets") # List of packages to be installed
 
 install_if_missing <- function(package_name) {#Function to check and install packages
    if (!requireNamespace(package_name, quietly = TRUE)) {
     install.packages(package_name)
   }
 }
 for (package in packages) {# Loop through the list and install missing packages
   install_if_missing(package)
 }
 ```
### Code to Run the  App
 You can run this app in RStudio with the following code :
```
 shiny::runGitHub("ST558project3", "jsingerfreeman")
```
