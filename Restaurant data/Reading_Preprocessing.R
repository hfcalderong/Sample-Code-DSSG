"PREPARATION"
"Set working directory"
setwd("~/Desktop/Nitesh")

"Reading data"
data <- read.csv("~/Desktop/Nitesh/DATA.csv")

data_backup = data
data_backup[which(data_backup[,1]=="#N/A"),1]=NA
data_backup$Month = as.Date(data_backup$Month,format = "%m/%d/%Y")

c = order(data_backup$Month, decreasing = T)
data_backup = data_backup[c,]

c = 0*1:ncol(data_backup)
for ( i in 2:ncol(data_backup)){
  data_backup[which(data_backup[,i]=="#N/A"),i]=NA  
  data_backup[,i] = as.numeric(as.character(data_backup[,i]))
  c[i] = min(data_backup[which(!is.na(data_backup[,i])),i])
}
min_v = c[which.min(c[-1])+1]

#Removing near-zero variance variables and variables with over 25% of NAs.
data_t3 = data_backup[,-c(1:2)] #It creates an auxiliary data frame to delete the variables with the criteria mentioned above.
teta = 1e-10 #It's the threshold for the variance. All variables whose variance is less than or equal to this value will be discarded.
n = length(which(diag(var(data_t3, use = "pairwise.complete.obs"))<=teta)) #It says how many variables will be discarded, if any.
if (!n==0){ #It enters if there's at least 1 variable to discard.
  data_t3 = data_t3[,-which(diag(var(data_t3, use = "pairwise.complete.obs"))<=teta)] #It measures the variance for all variables and deletes those whose variance is less than teta.  
}

c = 0 #c saves the value of NAs for each variable. Starts in zero. 
for (i in 1:ncol(data_t3)){ #It goes through all variables that were left after removing near-zero variance variables
  c[i] = length(which(is.na(data_t3[,i]))) #It counts how many NAs there are in the column for variable i 
}
data_t3 = data_t3[,which(c/nrow(data_t3)<=0.25)] #It keeps the variables that have less or equal than 25% of NAs. 

length(which(c/nrow(data_t3)>0.25)) #It tells how many variables are left with over 25% of NAs. This value is supposed to be zero.
length(which(diag(var(data_t3, use = "pairwise.complete.obs"))<=teta)) #It tells how many variables are left with near-zero variance (Var <= 1e-10). This value is supposed to be zero.

data_backup = cbind(data_backup[,c(1:2)],data_t3)

"TASK 1"
"Variable transformation"
data_etl = data_backup[,-which(colnames(data_backup)=="V1")]
data_etl = data_etl[,-which(colnames(data_etl)=="Month")]
original_v = ncol(data_etl)

for (i in 1:original_v){
  aux = original_v+ 9*(i-1)
  "Exp, log, inv"
  col.name = colnames(data_etl)
  data_etl[,aux+1]=exp(data_etl[,i])
  data_etl[,aux+2]=1/(data_etl[,i])
  data_etl[,aux+3]=log(data_etl[,i]-min_v+1)
  colnames(data_etl) = c(col.name,paste(col.name[i],c("exp","log","inv"),sep = "_"))
  
  "lags"
  col.name = colnames(data_etl)
  for (j in 1:6){
    aux_lag = embed(data_etl[,i],j+1)
    data_etl[1:nrow(aux_lag),aux+j+3]=aux_lag[,1]
  }
  colnames(data_etl) = c(col.name,paste(col.name[i],"lag",c(1:6),sep = "_"))
}
head(data_etl)
tail(data_etl)

"Correlation of lags with V1"
name_max_lag = 0*1:original_v
pos_max_lag = 0*1:original_v
id_max_lag = 0*1:original_v

name_max = 0*1:original_v
pos_max = 0*1:original_v
id_max = 0*1:original_v

col.name = colnames(data_etl)
cor_matrix = cor(cbind(data_backup$V1,data_etl), use="pairwise.complete.obs")
cor_val = cor_matrix[,1]

for (i in 1:original_v){
  position = grep(col.name[i],colnames(cor_matrix))
  cor_aux = cor_val[position]
  
  "correlation of lags"
  id_max_lag[i] = which.max(abs(cor_aux[5:10]))+4
  pos_max_lag[i] = position[id_max_lag[i]]
  name_max_lag[i] = colnames(cor_matrix)[pos_max_lag[i]]
  
  "correlation of remaining variables"
  y = c(1:4,id_max_lag[i])
  x = abs(cor_aux[y])
  id_max[i] = which.max(x)
  pos_max[i] = position[y[id_max[i]]]
  name_max[i] = colnames(cor_matrix)[pos_max[i]]  
}
name_max_lag
name_max

"TASK 2"
library("Hmisc", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")
data_t2 = cbind(data_backup$V1,data_etl[,pos_max-1]) #It puts in the same data frame the dependent variable and the transformed variables with higher correlation (absolute value).
cor_t2 = rcorr(as.matrix(data_t2)) #It estimates the correlation matrix with missing values. It also provides the p-value and the number of observations used in the calculation.

summary = data.frame(matrix(vector(), 0, 4,dimnames=list(c(), c("Variable 1","Variable 2", "Correlation", "p-value"))),stringsAsFactors=F)
            #The line above creates an empty data frame called Summary where we're gonna store the information of the variables whose correlation is higher than a threshold.
            #It has four columns: Variable 1, Variable 2, the valur of the correlation, and its p-value.
name_t2 = colnames(data_t2) #It stores the names of the variables of data_t2
abs_cor = abs(cor_t2$r) #It stores the absolute value of all correlations.
current = 0 #Current is a variable that will help us build the summary matrix. It says how many rows Summary has.
            #Given that we created an empty data frame, it starts at 0.


          #Given that the correlation matrix is symmetric, and the diagonal is 1, we're gonna read all values found in the upper triangle. 
for (i in 1:(ncol(data_t2)-1)){ #We sweep all rows but the last, that is why it goes until ncol(data_t2)-1
  for (j in (i+1):ncol(data_t2)){ #We sweep all columns above the diagonal, that is why is goes from (i+1) to ncol(data_t2)
    if (abs_cor[i,j]>=0.1){ #It compares the correlation in position i,j againts the threshold. If it is higher, then it stores the information. 
      current = nrow(summary) #It says how many rows there exists in Summary. 
      summary[current+1,1] = name_t2[i] #It saves in the first column of summary the name of the variable in row i.
      summary[current+1,2] = name_t2[j] #It saves in the second column of summary the name of the variable in column j.
      summary[current+1,3] = cor_t2$r[i,j] #It saves in the third column of summary the correlation between the variable in column j and the variable in row i.
      summary[current+1,4] = cor_t2$P[i,j] #It saves the p-value of the correlation above.
    }
  }
}
summary = summary[order(summary$Correlation,decreasing = T),] #It sorts the summary matrix in decreasing value of the correlations found. 
summary #It prints the summary matrix.

"TASK 3"
"VIF method"
library("car", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")
fit = lm(`data_backup$V1`~.,data = data_t2)
vif = vif(fit)
data_vif = data_t2[,c(1,which(sqrt(vif)<=2)+1)]

"Conclusion: All variables should be retained given that all VIFs are lower than 2" 

"Eigen method"
eigen = eigen(cor_t2$r[-1,-1])
max(eigen$values)/min(eigen$values)
length(which(eigen$values<=0.01))

"Conclusion: All variables should be retained given that the cor matrix has a low condition number" 

"The eigen method tells you whether there is a multicollinearity problem and how many variables
are likely to be compromised, but it does not say which variables. The number of variables that may
cause the multicollinearity is equal to the number of eigen values that are 0"