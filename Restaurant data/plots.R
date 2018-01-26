# CODE FOR THE PLOTS
c = unique(c(summary$Variable.1,summary$Variable.2)) #It identifies all variables listed in the summary matrix. 
                                                     #It takes unique values, because a variable can appear in column 1 or column 2. 

"Univariate time series"
setwd("~/Desktop/Nitesh/Univariate") #It chooses the folder to save the univariate plots.
for (i in 1:length(c)){ #It goes through all unique variable. length(c) says how many there are.
  j = which(colnames(data_t2)%in%c[i]) #j stores the column within data_t2 of the variable i in c.
  jpeg(paste(c[i], "time series",collapse =" ")) #It says we want to create a jpeg image named "(VariableName) time series".
  plot(data_backup$Month,data_backup[,j],xlab = "Time",ylab = colnames(data_backup)[j],type="l") #It plots Month against variable in column j as a line.
  dev.off() #Creates and stores image.
}

"Matrix plot"
setwd("~/Desktop/Nitesh") #The matrix plot is saved in the main space of the folder Nitesh
aux = colnames(data_t2)%in%c #It saves the position of ALL variables listed in either column of summary matrix, 
                             #equivalently, it sabes the position of all variables in c. 
jpeg("Matrix") #It says we want to create a jpeg image named "matrix".
plot(data_t2[,which(aux)]) #It creates the plot matrix. aux is a binary variable, which(aux) tells me which variable are TRUE 
dev.off() #Creates and stores image.

"Bivariate plot"
setwd("~/Desktop/Nitesh/Bivariate") #It chooses the folder to save the bivariate plots.
for (i in 1:nrow(summary)){ #It goes through all rows of summary matrix. nrow(summary) says how many there are.
  #Summary stores in column 1 (Variable.1) and column 2 (Variable.2) the pair of variables whose correlation is higher than 0.5
  j = which(colnames(data_t2)%in%summary$Variable.1[i]) #j stores the column within data_t2 of Variable.1 in row i.
  k = which(colnames(data_t2)%in%summary$Variable.2[i]) #k stores the column within data_t2 of Variable.2 in row i.
  jpeg(paste(summary$Variable.1[i], "against",summary$Variable.2[i],collapse =" ")) #It says we want to create a jpeg image named "(Variable.1) against (Variable.2)".
  plot(data_t2[,j],data_t2[,k], ylab = colnames(data_t2)[k], xlab = colnames(data_t2)[j]) #Plots the data and gives names to the axis
  dev.off() #Creates and stores image.
}

setwd("~/Desktop/Nitesh") #It returns to the parent folder, Nitesh. 




