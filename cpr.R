## ----setup, include=FALSE--------------------------------------------------------------
knitr::opts_chunk$set(cache = T, warning = F,error = F,message = F, comment = "#")


## ----  echo=FALSE----------------------------------------------------------------------
# Global Ops, Packages, Libraries ####
## Set global options ####
options(repos="https://cran.rstudio.com")
options(timeout=10000, digits=10, pillar.sigfigs=100)
## Install packages ####
list.of.packages <- c("caret", "class", "dplyr", "ggplot2", "ggthemes", "kableExtra", "knitr", "RColorBrewer","rpart", "scales", "tidyr","tidyverse", "tinytex")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
rm(list.of.packages,new.packages)

## ----libraries-------------------------------------------------------------------------
# Load libraries 
library(tidyverse)
library(ggthemes)
library(caret)
library(MASS) #for polr() function 
library(class) #for k-nearest neighbour
library(rpart) # for decision tree
library(knitr) #A General-Purpose Package for Dynamic Report Generation in R
library(kableExtra)
library(tinytex)
library(latexpdf)
# set global options
options(timeout=10000, digits=3)


## ----Downloading file------------------------------------------------------------------
# Download and import dataset 
urlfile<-"https://raw.githubusercontent.com/nevillio/Harvardx-capstone2/main/cpr_dataset.csv"
cpr<-read_csv(urlfile,col_types = cols())

## ----echo=FALSE------------------------------------------------------------------------
rm(urlfile)


## --------------------------------------------------------------------------------------
class(cpr) # type of dataset
dim(cpr) # no. of rows and columns in the dataset


## --------------------------------------------------------------------------------------
str(cpr) # structure of the dataset 
head(cpr) # first few rows of the dataset


## ----data transformation---------------------------------------------------------------
#Remove whitespaces from column names
names(cpr) <- str_replace_all(names(cpr)," ","_")

#Change name and values of 'Religion = Islam' Column
cpr <- rename(cpr, Religion = 'Religion_=_Islam',n_children = Number_of_Children)

#view the modified column names
names(cpr) 

#Renaming categories of required columns
cpr <- cpr %>% 
# Renaming categories in Contraceptive_Method_Used
    mutate(Contraceptive_Method_Used = 
               factor(
                   case_when(
                       Contraceptive_Method_Used == 1 ~ "None",
                       Contraceptive_Method_Used == 2 ~ "Long Term",
                       Contraceptive_Method_Used == 3 ~ "Short Term",
                   ),
                   levels = c("None","Short Term","Long Term") 
               ),
# Renaming categories in Religion
           Religion = ifelse(Religion == 1, "Muslim", "Non_muslim")
)

# Swapping values
## Creating a swapping function
swap <- function(x) {
    x = x-1
    x = ifelse(x==-1,-x,x)
    x
}

## Swapping 0's and 1's in required columns
cpr$Media_Exposure <- swap(cpr$Media_Exposure)
cpr$Currently_working <- swap(cpr$Currently_working)

# Converting categorical variables to factors, these variables have at most 4 unique values
cpr[sapply(cpr, function(x) n_distinct(x)<5)] <- 
        lapply(cpr[sapply(cpr,function(x)n_distinct(x)<5)],as.factor)

#Result of all Transformations
str(cpr)


## ----echo=FALSE------------------------------------------------------------------------
rm(swap)


## ----Splitting Data--------------------------------------------------------------------
# Split the dataset into training and test sets 
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y=cpr$Contraceptive_Method_Used, times = 1, p = 0.3, list = FALSE)
cpr_train <- cpr[-test_index,]
cpr_test <- cpr[test_index,]
rm(test_index,cpr)
nrow(cpr_train) #check number of observations of train set
nrow(cpr_test) #check number of observations of test set


## ----summary---------------------------------------------------------------------------
sapply(cpr_train, n_distinct) #No.of unique values in each variable
summary(cpr_train) #Summary of distribution of each variable

## ---- train Control--------------------------------------------------------------------
# Set control parameters - run algorithms using 10-fold cross validation repeated 3 times
trControl <- trainControl(method = "repeatedcv",
                          number = 10,
                          repeats=3,
                          savePredictions="final")


## ---- Ordinal Logistic regression------------------------------------------------------
# Ordinal Logistic Regression Model 
set.seed(4,sample.kind = "Rounding")
fit_olr<-train(Contraceptive_Method_Used~.,
              data=cpr_train,
              method="polr",
              trControl=trControl)

# Predict on training set
train_olr<-predict(fit_olr,cpr_train)

# Accuracy
accuracy <- mean(train_olr == cpr_train$Contraceptive_Method_Used)
accuracy

# Variable Importance
varImp(fit_olr)


## ---- result_table---------------------------------------------------------------------
# Create a table of results to compare models' performance 
results_table <- tibble(Method = "Ordinal Logistic Regression",
                        Accuracy = accuracy)
kable(results_table) %>% kable_styling()


## ----knn-------------------------------------------------------------------------------
set.seed(7,sample.kind = "Rounding")
#train the model
fit_knn<-train(Contraceptive_Method_Used~.,
               data=cpr_train,
               method="knn",
               trControl=trControl,
               tuneGrid = data.frame(k = seq(3,30,2)))

#predict the result
train_knn<-predict(fit_knn,cpr_train)

# Accuracy
accuracy <- mean(train_knn == cpr_train$Contraceptive_Method_Used)
accuracy

# Optimum value of k
best_k <- fit_knn$bestTune$k
best_k

# Add Results
results_table <- bind_rows(results_table,
                           tibble(Method = "knn",
                                  Accuracy = accuracy))


## ----Decision Tree---------------------------------------------------------------------
set.seed(14,sample.kind = "Rounding")
#train the model
fit_rpart<-train(Contraceptive_Method_Used~.,
                 data=cpr_train,
                 method="rpart",
                 trControl=trControl,
                 tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)),
)
# Predict on training set
train_rpart<-predict(fit_rpart,cpr_train)

# Accuracy
accuracy <- mean(train_rpart == cpr_train$Contraceptive_Method_Used)
accuracy

# Optimum value of cp
best_cp <- fit_rpart$bestTune$cp
best_cp

# plot the decision tree model
plot(fit_rpart$finalModel,margin = .1)
text(fit_rpart$finalModel)

# Check the variable importance
varImp(fit_rpart)

# Add Results
results_table <- bind_rows(results_table,
                           tibble(Method = "Decision Tree",
                                  Accuracy = accuracy))



## ----test validation-------------------------------------------------------------------
set.seed(14,sample.kind = "Rounding")
fit_final<-train(Contraceptive_Method_Used~.,
                 data=cpr_train,
                 method = "rpart",
                 tuneGrid = data.frame(cp = best_cp)
                 )

# Predict on test set
test_rpart<-predict(fit_final,cpr_test)

# Accuracy
accuracy <- mean(test_rpart == cpr_test$Contraceptive_Method_Used)
accuracy
    
# Add Results
results_table <- bind_rows(results_table,
                           tibble(Method = "Final Validation",
                                  Accuracy = accuracy))


## ---- result---------------------------------------------------------------------------
knitr::kable(results_table, caption="RMSE Results") %>% 
  kable_styling(bootstrap_options = c("striped", "hover","condensed"))

#Investigate the final model
## Check the variable importance
varImp(fit_final)

## plot the final model
plot(fit_final$finalModel,margin = .1)
text(fit_final$finalModel)

