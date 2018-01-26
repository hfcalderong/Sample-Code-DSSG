######################
# PREPARATION
######################
library(arules)
poblacion2 <- read.csv("~/Documents/MinJusticia/Proyectos/Ricardo/2016/Data/20160209.csv", sep=";")

#Intramural
db=poblacion2[poblacion2[,1]%in%unique(poblacion2[,1])[which(table(poblacion2[,1])==1)],]
sum(table(db[,3],db[,5]))
db = db[which(db[,3]=="Intramuros"),]
db = db[which(db[,5]=="CONDENADO"),]

#House arrest
c = unique(poblacion2[,3])
db = poblacion2[which((poblacion2[,3]==c[2])|(poblacion2[,3]==c[4])),]
db = db[which(db[,5]=="CONDENADO"),]
db=db[db[,1]%in%unique(db[,1])[which(table(db[,1])==1)],]
sum(table(db[,3]))

#Offenses under study
modalidades = unique(db[,7])
length(modalidades)

c = grep("FALSO",modalidades) #Search for offenses under study
c
modalidades[c]

db[which(db[,7]==modalidades[27]),7]=modalidades[4]
db[which(db[,7]==modalidades[31]),7]=modalidades[4]
db[which(db[,7]==modalidades[30]),7]=modalidades[12]


mod_ind_2 = c(44,41,63,57,74,14) #Indexes of offenses under study. Category 1.
mod_ind = c(1,3,2,4,20,26,11,10,29,12,45,50,19,6,13,18,32,43,28,38,16) #Indexes of offenses under study. Category 2.
modalidades[mod_ind]

db_21 = db[db[,7]%in%modalidades[mod_ind],]
db_dom = db[db[,7]%in%modalidades[mod_ind_2],]

#Frequency table. Offense. 
c = table(db_dom[,7])
sum(c[order(c,decreasing = T)][1:6])

#Frequency table. Year of exit. 
db$ANO_SALIDA = floor(db[,2]+(1-0)*db[,6])
f_db = db[db[,1]%in%unique(db[,1])[which(db$ANO_SALIDA>=2016)],]
barplot(table(db_21[,10])[1:40])

summary(db_21[,10])

##########################
# MARKET BASKET ANALYSIS
##########################
#Selection of population with only one charge
db = poblacion2[which(poblacion2[,3]=="Intramuros"),]
db = db[which(db[,5]=="CONDENADO"),]
db=db[db[,1]%in%unique(db[,1])[which(table(db[,1])>1)],]
table(table(db[,1]))

barplot(table(table(db[,1])))
length(unique(db[,1]))

#Analysis of associations
subdb = data.frame(TID = db[,1], modalidad= as.factor(db[,7]))
subdb_tran = as(split(subdb[,2],subdb[,1]),"transactions")
inspect(subdb_tran[1:10])
rules = apriori(subdb_tran,parameter = list(target="rules",minlen=2,confidence=0.05,supp=0.02))
summary(rules)
inspect(rules)

#Itemset Analysis
itemsets = unique(generatingItemsets(rules))
itemsets.df = as(itemsets,"data.frame")
itemsets.df[with(itemsets.df, order(-support,items)),]
dim(itemsets.df)
sum(itemsets.df[,2])

#Add scale of severity
tran_matrix = as(subdb_tran,"ngCMatrix")
tran_matrix = t(tran_matrix)
length(which(tran_matrix[1,]==TRUE))

mod_names = colnames(tran_matrix)
ind_tran = c(119,184,199,185,10,13,89,90,11,17,39,92,178,179,20,123,146,198,221,116,129)
gravedad = c(10,9.2,6,6,5,5,4.8,4.5,4.5,4,3.98,3.6,3.38,3.33,3.25,3.2,3.08,3,2.2,1.8,1.23)
mod_names[ind_tran]

tipo_delito = data.frame(in_21 = rowSums(tran_matrix[,ind_tran]), out_21= rowSums(tran_matrix[,-ind_tran]))

table(tipo_delito[,1],tipo_delito[,2])
tran_matrix_21 = tran_matrix[,ind_tran]
dim(tran_matrix_21)

table(rowSums(tran_matrix[,ind_tran]))
par(las=2)
par(mar=c(5,18,4,2))
barplot(colSums(tran_matrix_21),horiz = T,main = "Frecuencia de modalidades ordenadas por gravedad",names.arg = mod_names[ind_tran],cex.names=0.5)

#Just analyze the crime of highest severity
grave_matrix = data.frame(HOMICIDIO = seq(0,dim(tran_matrix_21)[1]-1)*0)
grave_matrix[which(tran_matrix_21[,1]==TRUE),1]=1

zeros = seq(0,dim(tran_matrix_21)[1]-1)*0
for (i in 2:21){
  grave_matrix[,i] = zeros
  grave_matrix[which(tran_matrix_21[,i]==TRUE),i]=1
}
colnames(grave_matrix)=mod_names[ind_tran]
str(grave_matrix)

for (i in 1:21){
  grave_matrix[which(grave_matrix[,i]==1),-i]=0
}

par(las=2)
par(mar=c(5,18,4,2))
barplot(colSums(grave_matrix),xlim = c(0,12000),horiz = T,main = "Frecuencia de modalidades ordenadas por gravedad",names.arg = mod_names[ind_tran],cex.names=0.5)
colSums(tran_matrix_21)