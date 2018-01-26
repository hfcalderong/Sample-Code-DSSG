# THIS CODE ESTIMATES A SVM MODEL.

#Data pre-processing
data_svm = data_t2 #It creates a data frame, data_svm, where we're gonna store all predictors for the model with time as a predictor.

data_svm = na.omit(data_svm) #Ignores cases with NA.
data_svm = scale(data_svm,scale = T, center = T) #It centers and scale the data so that all variables are given the same weight.
data_svm = data.frame(data_svm) #It converts data_svm in a data frame.

data_svm[,ncol(data_svm)+1] = 1:nrow(data_svm) #It creates an integer variable that says how many periods away is the current observation from the origin (first observation).  
data_svm[,ncol(data_svm)+1] = rep_len(1:12,nrow(data_svm)) #It creates a variables that serves as a pivot for the seasonality of data. 

colnames(data_svm) = c(colnames(data_t2),"t_id","#month") #Gives names to the columns, it names the new variables as t_id and #month correspondingly.

# Training and validation datasets
n = nrow(data_svm) #It says how many observations there are in the data set. 
s = n*0.7 #It estimates the turning point for the trainning and testing set. 
xtrain_svm = data_svm[1:s,-1] #It saves the predictors (independent variables) for the trainning set. The first variable is the dependent variable that is why I use -1. 
ytrain_svm = data_svm[1:s,1] #It saves the predictors (independent variables) for the trainning set. The first variable is the dependent variable that is why I use 1. 
val_svm = data_svm[s:n,] #The testing set is created here.

id_var_svm = which(diag(var(xtrain_svm))>=1e-10) #Sometimes within the training sample there are variables with nearly-zero variance. These variables are ommitted.
xtrain_svm = xtrain_svm[,id_var_svm] #It keeps all variables that were not excluded in the line above.

xeval_svm = val_svm[,(id_var_svm+1)] #It saves the predictor for the testing set. The same predictors that we conserved in the trainning set, otherwise it wuold return an error.
yeval_svm = val_svm[,1] #It saves the response variable for the testing set. 

train_svm = cbind(ytrain_svm,xtrain_svm) #It merges the training set
colnames(train_svm)[1] = "y" # Simplifies the name of the dependent variable in the training set
colnames(data_svm)[1] = "y" # Simplifies the name of the dependent variable in the original data set

# Model Estimation
if(!require("e1071")){install.packages("e1071")}
library("e1071", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")

# Radial Kernel
tune_svm = tune(svm, y~., data = data_svm, kernel = "radial",ranges = list(cost = 1:20, gamma = 2^c(-6:2),epsilon = 10^c(-3:-1))) #It fits several models and estimates by CV the MSE. It chooses the best model.
summary(tune_svm) #Provides summary information of the models estimated above.

svm_fit = svm(y~., data = train_svm, kernel = "radial",gamma=0.015625,cost = 2, epsilon= 0.01) #Estimates single model with given parameters.
y_predict = predict(svm_fit, newdata = xeval_svm) # Predicts the dependent variable with the model above.
plot(y_predict, yeval_svm)  # Plots the fitted values and the actual values
min = min(min(y_predict), min(yeval_svm)) # Store the minimum value of the actual and the fitted response
max = max(max(y_predict), max(yeval_svm)) # Store the maximum value of the actual and the fitted response
lines(c(min,max),c(min,max)) # It adds the line, x=y.

# Polynomial Kernel
tune_svm = tune(svm, y~., data = data_svm, kernel = "polynomial",ranges = list(cost = c(0.1,0.5,1,2), degree = 2, epsilon = 10^c(-3:-1))) #It fits several models and estimates by CV the MSE. It chooses the best model.
summary(tune_svm) #Provides summary information of the models estimated above.

svm_fit = svm(y~., data = train_svm, kernel = "polynomial",degree=2,cost = 1,epsilon = 0.001) #Estimates single model with given parameters.
y_predict = predict(svm_fit, newdata = xeval_svm) # Predicts the dependent variable with the model above.

plot(y_predict, yeval_svm) # Plots the fitted values and the actual values
min = min(min(y_predict), min(yeval_svm)) # Store the minimum value of the actual and the fitted response
max = max(max(y_predict), max(yeval_svm)) # Store the maximum value of the actual and the fitted response
lines(c(min,max),c(min,max)) # It adds the line, x=y.
