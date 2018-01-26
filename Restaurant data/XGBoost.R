# THIS CODE ESTIMATES A XGBOOST MODEL WITH AND WITHOUT TIME AS PREDICTORS.

#Data pre-processing
data_xg = data_t2 #It creates a data frame, data_xg, where we're gonna use store all predictors for the model with time as a predictor.

data_xg = na.omit(data_xg) #Ignores cases with NA.
data_xg = scale(data_xg,scale = T, center = T) #It centers and scale the data so that all variables are given the same weight.
data_xg = data.frame(data_xg) #It converts data_xg in a data frame.

data_xg[,ncol(data_xg)+1] = 1:nrow(data_xg) #It creates an integer variable that says how many periods away is the current observation from the origin (first observation).  
data_xg[,ncol(data_xg)+1] = rep_len(1:12,nrow(data_xg)) #It creates a variables that serves as a pivot for the seasonality of data. 

colnames(data_xg) = c(colnames(data_t2),"t_id","#month") #Gives names to the columns, it names the new variables as t_id and #month correspondingly.

# Training and validation datasets
n = nrow(data_xg) #It says how many observations there are in the data set. 
s = n*0.7 #It estimates the turning point for the trainning and testing set. 
xtrain_xg = data_xg[1:s,-1] #It saves the predictors (independent variables) for the trainning set. The first variable is the dependent variable that is why I use -1. 
ytrain_xg = data_xg[1:s,1] #It saves the predictors (independent variables) for the trainning set. The first variable is the dependent variable that is why I use 1. 
val_xg = data_xg[s:n,] #The testing set is created here.

id_var_xg = which(diag(var(xtrain_xg))>=1e-10) #Sometimes within the training sample there are variables with nearly-zero variance. These variables are ommitted.
xtrain_xg = xtrain_xg[,id_var_xg] #It keeps all variables that were not excluded in the line above.

xeval_xg = val_xg[,(id_var_xg+1)] #It saves the predictor for the testing set. The same predictors that we conserved in the trainning set, otherwise it wuold return an error.
yeval_xg = val_xg[,1] #It saves the response variable for the testing set. 

train_xg = cbind(ytrain_xg, xtrain_xg) #It merges the training set
colnames(train_xg)[1]="y" # Simplifies the name of the dependent variable in the training set


# Model Estimation
library("xgboost", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")

xg_fit = xgboost( data = as.matrix(xtrain_xg), label = ytrain_xg, objective = "reg:linear", eta = 0.1, max.depth = 6, nrounds = 50) #Estimates single model with given parameters.

y_predict = predict(xg_fit, newdata = as.matrix(xeval_xg)) # Predicts the dependent variable with the model above.
plot(y_predict, yeval_xg)  # Plots the fitted values and the actual values
min = min(min(y_predict), min(yeval_xg)) # Store the minimum value of the actual and the fitted response
max = max(max(y_predict), max(yeval_xg)) # Store the maximum value of the actual and the fitted response
lines(c(min,max),c(min,max)) # It adds the line, x=y.




