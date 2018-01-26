# It works on the results of the previous code (Script.R)
library("car", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("np", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("FWDselect", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")

################################
# KERNEL REGRESSION WITHOUT TIME
################################
# Training and validation datasets
data_k = na.omit(data_vif) 
data_k = scale(data_k,scale = T, center = T) 
data_k = data.frame(data_k) 

n = nrow(data_k)
s = sample(1:n,n*0.7, replace = F)
xtrain_nt = data_k[s,-1]
ytrain_nt = data_k[s,1]
val_nt = data_k[-s,]

# Model Estimation
# The kernels used for the estimation are: Gaussian, Epanechnikov, Uniform.
reg_type = c("lc", "ll")
ck_type = c("epanechnikov","gaussian","uniform")
ck_order = c(2,4)

model_in = data.frame(matrix(vector(), 0, 5,dimnames=list(c(), c("Model","R2", "MAE", "MAPE","MSE"))),stringsAsFactors=F)
model_out = data.frame(matrix(vector(), 0, 5,dimnames=list(c(), c("Model","R2", "MAE", "MAPE","MSE"))),stringsAsFactors=F)
predicted_tn = data.frame(matrix(vector(), nrow(xeval_nt), 12))

id_var_nt = which(!diag(var(xtrain_nt))<=1e-10)
xtrain_nt = xtrain_nt[,id_var_nt]
eigen(var(xtrain_nt))

aux = colnames(val_nt)%in%colnames(xtrain_nt)
xeval_nt = val_nt[,aux]
yeval_nt = val_nt[,1]

z = 0
for (i in 1:2){
  for (j in 1:3){
    for (k in 1:2){
      current_row = nrow(model_in)+1
      z = z+1
      # In-sample accuracy
      bw.all = npregbw( xdat = xtrain_nt,ydat = ytrain_nt ,regtype = reg_type[i], bwmethod = "cv.aic",ckertype = ck_type[j],ckerorder = ck_order[k])
          #It estimates the optimal bandwith by crossvalidation.
      model.np = npreg(bws = bw.all,exdat = xeval_nt, eydat = yeval_nt,txdat = xtrain_nt, tydat = ytrain_nt)
          #It estimates the regression model and predicts the value for the testing set.
      
      y_est_in = predict(model.np)
      residuals = y_est_in - ytrain_nt
      model_in[current_row,1] = paste(reg_type[i],ck_type[j],ck_order[k],sep = "-")
      model_in[current_row,2] = cov(y_est_in ,ytrain_nt)/sqrt(var(y_est_in)*var(ytrain_nt))
      model_in[current_row,3] = mean(abs(residuals))
      model_in[current_row,4] = mean(abs(residuals)/abs(ytrain_nt))
      model_in[current_row,5] = var(residuals)
      
      print(z)
      # Out-of-sample accuracy
      y_est = fitted(model.np)
      predicted_tn[,z] = y_est
      residuals = y_est - yeval_nt
      model_out[current_row,1] = paste(reg_type[i],ck_type[j],ck_order[k],sep = "-")
      model_out[current_row,2] = cov(y_est ,yeval_nt)/sqrt(var(y_est)*var(yeval_nt))
      model_out[current_row,3] = mean(abs(residuals))
      model_out[current_row,4] = mean(abs(residuals)/abs(yeval_nt))
      model_out[current_row,5] = var(residuals)
      
      par(mfrow=c(1,2))
      plot(y_est_in, ytrain_nt)
      lines(x = c(min(y_est_in),max(y_est_in)) , y = c(min(ytrain_nt),max(ytrain_nt)))
      plot(y_est, yeval_nt)
      lines(x = c(min(y_est),max(y_est)) , y = c(min(yeval_nt),max(yeval_nt)))
    }
  }
}

model_out = model_out[order(model_out$MAPE,decreasing = F),]
model_in = model_in[order(model_in$MAPE,decreasing = F),]

# Significance test
bw.all = npregbw( xdat = xtrain_nt,ydat = ytrain_nt ,regtype = "ll", bwmethod = "cv.aic",ckertype = "uniform") #It estimates the optimal bandwith by crossvalidation.
model.np = npreg(bws = bw.all,exdat = xeval_nt, eydat = yeval_nt,txdat = xtrain_nt, tydat = ytrain_nt) #It estimates the regression model and predicts the value for the testing set.

sigtest = npsigtest(model.np)
index = which(sigtest$P>=0.05)

bw.all = npregbw( xdat = xtrain_nt[,-index],ydat = ytrain_nt ,regtype = "ll", bwmethod = "cv.aic",ckertype = "uniform") #It estimates the optimal bandwith by crossvalidation.
model.np = npreg(bws = bw.all,exdat = xeval_nt[,-index], eydat = yeval_nt,txdat = xtrain_nt[,-index], tydat = ytrain_nt) #It estimates the regression model and predicts the value for the testing set.
summary(model.np)


################################
# KERNEL REGRESSION WITH TIME
################################

data_t = data_k 
data_t[,ncol(data_k)+1] = 1:nrow(data_k) #It creates an integer variable that says how many periods away is the current observation from the origin (first observation).  
data_t[,ncol(data_t)+1] = rep_len(1:12,nrow(data_t)) #It creates a variables that serves as a pivot for the seasonality of data. 
colnames(data_t) = c(colnames(data_k),"t_id","#month")

# Training and validation datasets
n = nrow(data_t) 
s = n*0.7 
xtrain_t = data_t[1:s,-1] 
ytrain_t = data_t[1:s,1] 
val_t = data_t[s:n,]

# Model Estimation
# The kernels used for the estimation are: Gaussian, Epanechnikov, Uniform.
reg_type = c("lc", "ll")
ck_type = c("epanechnikov","gaussian","uniform")
ck_order = c(2,4)

model_in_t = data.frame(matrix(vector(), 0, 5,dimnames=list(c(), c("Model","R2", "MAE", "MAPE","MSE"))),stringsAsFactors=F)
model_out_t = data.frame(matrix(vector(), 0, 5,dimnames=list(c(), c("Model","R2", "MAE", "MAPE","MSE"))),stringsAsFactors=F)
predicted_t = data.frame(matrix(vector(), nrow(val_t), 12))

id_var_t = which(!diag(var(xtrain_t))<=1e-10)
xtrain_t = xtrain_t[,id_var_t]
eigen(var(xtrain_t)) #It shows the eigen structure of the variance-covariance matrix of xtrain_t. It gives an idea of multicollinearity.

xeval_t = val_t[,(id_var_t+1)]
yeval_t = val_t[,1]

curret_row  = 0
z = 0
for (i in 1:2){ #Length of reg_type 
  for (j in 1:3){ #Length of ck_type
    for (k in 1:2){ #Length of ck_order
      
      curret_row = nrow(model_in_t)+1
      z = z+1 
      prin(z)
      
      # IN-SAMPLE ACCURACY
      bw.all = npregbw( xdat = xtrain_t,ydat = ytrain_t ,regtype = reg_type[i], bwmethod = "cv.aic",ckertype = ck_type[j],ckerorder = ck_order[k])
                  #It estimates the optimal bandwith by crossvalidation.
      model.np = npreg(bws = bw.all,exdat = xeval_t, eydat = yeval_t,txdat = xtrain_t, tydat = ytrain_t)
                  #It estimates the regression model and predicts the value for the testing set.
      y_est_in = predict(model.np) 
      residuals = y_est_in - ytrain_t 
      model_in_t[curret_row,1] = paste(reg_type[i],ck_type[j],ck_order[k],sep = "-")
      model_in_t[curret_row,2] = cov(y_est_in ,ytrain_t)/sqrt(var(y_est_in)*var(ytrain_t)) 
      model_in_t[curret_row,3] = mean(abs(residuals)) 
      model_in_t[curret_row,4] = mean(abs(residuals)/abs(ytrain_t)) 
      model_in_t[curret_row,5] = var(residuals)
      
      # Out-of-sample accuracy
      y_est = fitted(model.np) 
      predicted_t[,z] = y_est 
      residuals = y_est - yeval_t
      model_out_t[curret_row,1] = paste(reg_type[i],ck_type[j],ck_order[k],sep = "-") 
      model_out_t[curret_row,2] = cov(y_est ,yeval_t)/sqrt(var(y_est)*var(yeval_t)) 
      model_out_t[curret_row,3] = mean(abs(residuals)) 
      model_out_t[curret_row,4] = mean(abs(residuals)/abs(yeval_t)) 
      model_out_t[curret_row,5] = var(residuals) 
    }
  }
}

model_out_t = model_out_t[order(model_out_t$MAPE,decreasing = F),] #It sorts the model_out_t matrix in increasing order of the MAPE
model_in_t = model_in_t[order(model_in_t$MAPE,decreasing = F),] #It sorts the model_in_t matrix in increasing order of the MAPE

# Significance test
bw.all = npregbw( xdat = xtrain_t,ydat = ytrain_t ,regtype = "ll", bwmethod = "cv.aic",ckertype = "uniform") #It estimates the optimal bandwith by crossvalidation.
model.np = npreg(bws = bw.all,exdat = xeval_t, eydat = yeval_t,txdat = xtrain_t, tydat = ytrain_t) #It estimates the regression model and predicts the value for the testing set.

sigtest = npsigtest(model.np)
index = which(sigtest$P>=0.05)

bw.all = npregbw( xdat = xtrain_t[,-index],ydat = ytrain_t ,regtype = "ll", bwmethod = "cv.aic",ckertype = "uniform") #It estimates the optimal bandwith by crossvalidation.
model.np = npreg(bws = bw.all,exdat = xeval_t[,-index], eydat = yeval_t,txdat = xtrain_t[,-index], tydat = ytrain_t) #It estimates the regression model and predicts the value for the testing set.
summary(model.np)

