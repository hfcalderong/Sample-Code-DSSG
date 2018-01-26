library("data.table", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")
library("tidyr", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")
library("readxl", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")

#reads data
raw_data = read_excel("~/Downloads/Example File.xlsx",sheet = "Raw data")
coefficients = read_excel("~/Downloads/Example File.xlsx",sheet = "Coefficients")
coefficients = coefficients[1,] #It is assumed that the coefficients are found on the first row as in the example
coefficients = coefficients[,!is.na(coefficients)] #It deletes all columns with NA values.

#Data creation
id = with(data = raw_data, order(State,Year,Month)) #Sorts data by date and state. Helpful in case the data has not previously been sorted.
raw_data = raw_data[id,] #Rearrange the dataframe with the oder found above

former = raw_data[!raw_data$Year == max(raw_data$Year),] #It stores the value of variables at n-1
latest = raw_data[!raw_data$Year == min(raw_data$Year),] #It stores the value of variables at n

ratio = latest[,-c(1:3)]/former[,-c(1:3)] #Calculates the ratio of (n-1)/n
ratio = cbind(latest[,c(1:3)],ratio)
ratiop = ratio[,-c(1:3)] #Creates dataframe to store the value of ratio to power

for (i in 1:ncol(ratiop)){
  ratiop[,i] = ratio[,i+3]^as.numeric(coefficients[i]) #Calculates and stores the values of ratio to power
}
ratiop = cbind(ratio[,c(1:3)],ratiop) #Joins the state, year and month variables to the ratiop data frame

gmean <- function(x, na.rm = FALSE){ 
  if(na.rm) x <- x[!is.na(x)] 
  n <- length(x) 
  return(prod(x)^(1/n))
} 

#TABLE 1
cities = unique(ratiop$State)
years = unique(ratiop$Year)
variables = colnames(ratiop)
aux1 =  matrix(tapply(X = ratiop[,4], INDEX = list(ratiop$State, ratio$Year),FUN = gmean), nrow = 2, byrow = F) #Initializes an auxiliary variable to calculate the first table
aux1 = as.data.frame(aux1) #Changes the format of the variable from matrix to data frame
colnames(aux1) = years #Gives names to the columns
aux1$States = cities #Stores the states
aux1$Var = variables[4] 
for (i in 2:(ncol(ratiop)-3)){ #It starts at 2 because the first variable was used above.
  c = matrix(tapply(X = ratiop[,3+i], INDEX = list(ratiop$State, ratio$Year),FUN = gmean), nrow = 2, byrow = F) #Calculates and stores the geometric mean of ratio to power per state and year
  c = as.data.frame(c) #Changes the format of the variable from matrix to data frame
  colnames(c) = years #Gives names to the columns
  c$States = cities #Stores the states
  c$Var = variables[3+i] 
  aux1 = rbind(c,aux1) #Merge the results for the variable being evaluated to the previous results
}
aux1[order(aux1$States,decreasing = F),] #Organizes rows according to state in AZ order.
m = gather(aux1, year,ratiop,1:length(years)) #Changes the distribution of the data 
aux1 = spread(data = m, key = Var, value = ratiop) #Changes the distribution of the data to match the example
aux1$Fitted = apply(aux1[,-c(1:2)], 1, prod) #Calculates the fitted column as the product of all regressors
table1 = (aux1[,-c(1,2)] -1)*100 #Calculates table 1


#TABLE 2
cities = unique(ratio$State)
years = unique(ratio$Year)
variables = colnames(ratio)
aux2 =  matrix(tapply(X = ratio[,4], INDEX = list(ratio$State, ratio$Year),FUN = gmean), nrow = 2, byrow = F) #Initializes an auxiliary variable to calculate the first table
aux2 = as.data.frame(aux2) #Changes the format of the variable from matrix to data frame
colnames(aux2) = years #Gives names to the columns
aux2$States = cities #Stores the states
aux2$Var = variables[4] 
for (i in 2:(ncol(ratio)-3)){ #It starts at 2 because the first variable was used above.
  c = matrix(tapply(X = ratio[,3+i], INDEX = list(ratio$State, ratio$Year),FUN = gmean), nrow = 2, byrow = F) #Calculates and stores the geometric mean of ratio to power per state and year
  c = as.data.frame(c) #Changes the format of the variable from matrix to data frame
  colnames(c) = years #Gives names to the columns
  c$States = cities #Stores the states
  c$Var = variables[3+i] 
  aux2 = rbind(c,aux2) #Merge the results for the variable being evaluated to the previous results
}
aux2[order(aux2$States,decreasing = F),] #Organizes rows according to state in AZ order.
m = gather(aux2, year,ratio,1:length(years)) #Changes the distribution of the data 
aux2 = spread(data = m, key = Var, value = ratio) #Changes the distribution of the data to match the example
table2 = (aux2[,-c(1,2)] -1)*100 #Calculates table 2