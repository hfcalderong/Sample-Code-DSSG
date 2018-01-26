#####################
#PREPARATION
#####################

# Packages
setwd("~/Desktop")
if(!require("dplyr")){install.packages("dplyr")} #Verifies whether the package is installed, if not then it installs it
library("dplyr", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library") #Loads the package
library("e1071")

data_new<- read.csv("~/Desktop/New Example data file.csv")

#Data pre-processing
data_svm = data_new 
data_svm = data.frame(scale(data_svm[,-c(1:2)],scale = T, center = T) ) 
data_svm[,ncol(data_svm)+1] = rep_len(1:12,nrow(data_svm)) 
colnames(data_svm) = c(colnames(data_svm[,-ncol(data_svm)]),"#month") 

testing = data_svm[c(193:240),]

training = data_svm[-c(193:240),]
training = na.omit(training) 

# Model Estimation - Radial Kernel
tune_svm = tune(svm, Dependent~., data = training, kernel = "radial",ranges = list(cost = 1:20, gamma = 2^c(-6:2),epsilon = 10^c(-3:-1))) #It fits several models and estimates by CV the MSE. It chooses the best model.
summary_training(tune_svm)

svm_fit = svm(Dependent~., data = training, kernel = "radial",gamma=tune_svm$best.parameters$gamma,cost = tune_svm$best.parameters$cost, epsilon= tune_svm$best.parameters$epsilon) 
y_predict = predict(svm_fit, newdata = testing[,-1]) 

#####################
#GRADIENT ESTIMATION
#####################
#The gradient of the model trained above will be estimated separately for each point provided.
#The derivatives are estimated using numerical methods given that we do not have an analytical expression for them
#The method used hereby is the fourth-order method for the first derivative, namely
#       df(x)/dx = 1/12/h*[-f(x+2h)+8f(x+h)-8f(x-h)+f(x-2h)]
# Where h is the size of the step to estimate the derivative 

gradient = function(point){
  y = point
  x_aux = as.data.frame(matrix(data=0*1:(4*length(y)*length(y)),nrow = length(y)*4, ncol = length(y)))
  colnames(x_aux) = colnames(y) 
  for (i in 1:nrow(x_aux)){ 
    x_aux[i,1:ncol(x_aux)]= y 
  }
  coefh = c(2,1,-1,-2) 
  for (i in 1:ncol(x_aux)){
    h = abs(0.05*y[i]) 
    for (j in 1:4){
      x_aux[(i-1)*4+j,i] = x_aux[(i-1)*4+j,i]+coefh[j]*h 
    }
  }
  x_aux$t_id = point[,1] 
  y_predict = predict(svm_fit, newdata = x_aux) 
  f_aux = as.data.frame(matrix(data=y_predict,nrow = ncol(y), ncol = 4,byrow = T)) 
  rownames(f_aux) = colnames(y) 
  colnames(f_aux) = c("2h","h","-h","-2h") 
  h = abs(0.05*y) 
  elasticity = 1/12/h*(-f_aux$`2h`+8*f_aux$h-8*f_aux$`-h`+f_aux$`-2h`) 
  return (elasticity) 
}

#####################
#SUMMARY TABLES
#####################

#Summary Table Version 1: Just for forecast
#Estimating elasticity for forecast
y = testing[,-1]
elasticity_matrix = as.data.frame(matrix(0,nrow = nrow(y),ncol = (ncol(y)))) 
colnames(elasticity_matrix) = colnames(y) 

for (i in 1:nrow(y)){
  elasticity_matrix[i,]=gradient(y[i,])
}
elasticity_matrix 
 
#summary_testing Table
ny = nrow(testing)/12 #Number of years in the summary_testing, i.e. number of rows
summary_testing = matrix(0*1:(ny*(4*ncol(testing[,-1])+5)), nrow = ny, ncol = 4*ncol(testing[,-1])+5)
colnames(summary_testing) = c("Rows","Year","y","Annual change dependent","Percentual change dependent (%)",paste("Annual Change",names(testing[,-1])),paste("Elasticity",names(testing[,-1])),paste("Contribution",names(testing[,-1])),paste("Percentual Contribution",names(testing[,-1]),"%"))
summary_testing = as.data.frame(summary_testing)

for (i in 1:ny){
    summary_testing[i,1] = paste("Row",1+12*(i-1),"to",12*i,sep = " ")
    summary_testing[i,2] = paste("Year",i,sep = " ")
    summary_testing[i,3] = y_predict[1+12*(i-1)] #Value of dependent at time t
    summary_testing[i,4] = y_predict[12*i]-y_predict[1+12*(i-1)] #Difference of dependent at time t+12 and at time t
    summary_testing[i,5] = summary_testing[i,4]/summary_testing[i,3]*100 #Percentual change of dependent
    summary_testing[i,6:12] = as.numeric(testing[12*i,-1]-testing[1+12*(i-1),-1]) #Difference of independents at time t+12 and at time t
    summary_testing[i,13:19] = as.numeric(elasticity_matrix[1+12*(i-1),]) #Elasticities of independents at time t
    summary_testing[i,20:26] = summary_testing[i,6:12]*summary_testing[i,13:19] #Total differentials = difference * elasticities
    summary_testing[i,27:33] = summary_testing[i,20:26]/summary_testing[i,3]*100 #Change as percent of dependent
}
write.csv2(x = summary_testing[,c(1:2,5,27:33)],file = "summary_testing.csv")  

#summary Table Version 2: Just for training points
#Estimating elasticity for trainig points
y = data_svm[,-1]
elasticity_matrix = as.data.frame(matrix(0,nrow = nrow(y),ncol = (ncol(y)))) 
colnames(elasticity_matrix) = colnames(y) 

for (i in 1:nrow(y)){
  elasticity_matrix[i,]=gradient(y[i,])
}
elasticity_matrix 

#summary_training Table Training
ny = nrow(training)/12 #Number of years in the summary_training, i.e. number of rows
summary_training = matrix(0*1:(ny*(4*ncol(testing[,-1])+5)), nrow = ny, ncol = 4*ncol(testing[,-1])+5)
colnames(summary_training) = c("Rows","Year","y","Annual change dependent","Percentual change dependent (%)",paste("Annual Change",names(testing[,-1])),paste("Elasticity",names(testing[,-1])),paste("Contribution",names(testing[,-1])),paste("Percentual Contribution",names(testing[,-1]),"%"))
summary_training = as.data.frame(summary_training)

for (i in 1:ny){
  summary_training[i,1] = paste("Row",1+12*(i-1),"to",12*i,sep = " ")
  summary_training[i,2] = paste("Year",i,sep = " ")
  summary_training[i,3] = training[1+12*(i-1),1] #Value of dependent at time t
  summary_training[i,4] = training[12*i,1]-training[1+12*(i-1),1] #Difference of dependent at time t+12 and at time t
  summary_training[i,5] = summary_training[i,4]/summary_training[i,3]*100 #Percentual change of dependent
  summary_training[i,6:12] = as.numeric(training[12*i,-1]-training[1+12*(i-1),-1]) #Difference of independents at time t+12 and at time t
  summary_training[i,13:19] = as.numeric(elasticity_matrix[1+12*(i-1),]) #Elasticities of independents at time t
  summary_training[i,20:26] = summary_training[i,6:12]*summary_training[i,13:19] #Total differentials = difference * elasticities
  summary_training[i,27:33] = summary_training[i,20:26]/summary_training[i,3]*100 #Change as percent of dependent
}
write.csv2(x = summary_training[,c(1:2,5,27:33)],file = "summary_training.csv")  

#summary Table Version 3: For all points
final_summary = rbind(summary_testing, summary_training)
write.csv2(x = final_summary[,c(1:2,5,27:33)],file = "final_summary.csv")  
