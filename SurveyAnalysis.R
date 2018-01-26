#############################
# DESCRIPTION
#############################
#The data set includes the response of 1750 people to 81 questions in a survey applied by the Ministry of Justice
#about the perception of punitive attitudes among citizens and policy makers. The questions were organized in the following
#blocks
# 1. General Perception of Justice
# 2. Perception of Institutions
# 3. Perception of Courts
# 4. Perception of Length of Sentences
# 5. Perception of Alternative Sentences
# 6. Demographic Profile

#############################
# PREPARATION
#############################

setwd("~/Documents/MinJusticia/Proyectos/Ever/R")
pack = c("plyr","dplyr","fpc","FactoMineR","factoextra","Rtsne","klaR","cluster","devtools",
         "likert","rpart","ggplot2","doBy","vcd","corrplot","homals")
lapply(pack, require, character.only = T)
preguntas <- read_excel("~/Documents/MinJusticia/Proyectos/Ever/R/new_data.xlsx", sheet = "Mapping")
datos <- read_excel("~/Documents/MinJusticia/Proyectos/Ever/R/new_data.xlsx", sheet = "Data", na = "NA")
datos = as.data.frame(datos)
datos[datos[,61]=="Ninguno",61]="Ninguna"
datos[datos[,63]=="Ninguno",63]="Ninguna"

for (i in 1:ncol(datos)){
  datos[,i]=factor(datos[,i])
}
datos = datos[-which(is.na(datos[,65:79])),]
datos = datos %>% filter(!edad_cat == "NA")

#Organizes the levels of factors in a likert scale
for (i in 1:24){ levels(datos[,i]) = c("Nada","Algo","Bastante","Totalmente") }
for (i in 25:32){ levels(datos[,i]) = c("Muy negativo","Negativo","Aceptable","Positivo","Muy positivo") }
for (i in 33:41){ levels(datos[,i]) = c("Nunca","Algunas veces","A menudo","Siempre") }
for (i in 42:52){ levels(datos[,i]) = c("Ninguna","Menos de 25","Entre 26 y 50",
                                        "Entre 51 y 75","Mas de 76","Todas") }
for (i in 53:63){ levels(datos[,i]) = c("Ninguna","Trabajo comunitario","Multas","Prision temporal",
                                        "Prision perpetua","Pena de muerte") }
for (i in 64:64){ levels(datos[,i]) = c("3 meses","6 meses","12 meses","24 meses","36 meses","Indefinidamente") }
for (i in 65:65){ levels(datos[,i]) = c("No Se","No","SI") }

#Stores the blocks of questions
grupos_preguntas = data.frame("PInicio"=c(1,25,33,42,53), 
                              "PFinal"= c(24,32,41,52,63),
                              "Categoria"=c("PercepcionGeneralJusticia", "PercepcionInstituciones","PercepcionTribunales",
                                            "PercepcionCondenas","PercepcionPenasAlternativas"))
datos = datos[,-43]

#############################
# DEMOGRAPHIC GROUPS
#############################
demo = datos[,c(74:78)]
demo = as.data.frame(demo)
colnames(demo) = c("Sexo","Estado_Civil","Estrato","Academico","Edad")
demo$Estrato = as.factor(demo$Estrato)

for (i in 1:ncol(demo)){
  demo[,i]=factor(demo[,i])
}

#Multiple Correspondence Analysis
mca = MCA(demo[,-6], ncp = ncol(demo[,-6]))
coord = get_mca_ind(mca)
coord = as.data.frame(coord$coord)

#Hierarchical Clustering
p = hclust(dist(coord))
plot(p,labels = rep("",nrow(coord)))
rect.hclust(p, k = 8)

#Calinski-Harabasz index for estimating the number of clusters
k = matrix(0,nrow = 20,ncol = 2, dimnames = list(c(),c("cluster","CH")))
k[,1]=1:nrow(k)
k = as.data.frame(k)
for (i in 2:nrow(k)){
  m = kmeans(coord, i) #K-means
  k[i,2] = calinhara(coord, as.numeric(m$cluster))
}
plot(k)
which.max(k[,2])
m = kmeans(coord, 8,iter.max = 500) #8 clusters of respondents were identified according to their demographic profile
m$size
demo$cluster = factor(m$cluster)

#t-SNE plot to confirm separability of clusters
tsne = Rtsne(dist(coord), max_iter = 500)
aux = data.frame("X"=tsne$Y[,1],"Y"=tsne$Y[,2],"cluster" = factor(demo$cluster))
ggplot(aux, aes(x = X, y = Y, color = cluster))+geom_point()

datos$cluster = m$cluster
demo = as.data.frame(demo)
c = aggregate(x = demo[,-6], list(demo$cluster),table)
t(c)

#K-medoids
gower_dist = daisy(x = demo[,-6], metric = "gower")
demo = as.data.frame(demo)
demo = demo[-which(demo$Academico == "NA"),]

sil_width <- c(NA)
for(i in 2:20){
  print(i)
  pam_fit <- pam(gower_dist, diss = TRUE,k = i)
  sil_width[i] <- pam_fit$silinfo$avg.width
}
plot(1:20, sil_width, xlab = "Number of clusters", ylab = "Silhouette Width")
lines(1:20, sil_width)
which.max(sil_width)

pam_fit <- pam(gower_dist, diss = TRUE,k = 14)
demo$cluster = pam_fit$clustering

#############################
# VARIABLE CLUSTERING
#############################
#The clustering of questions is made within each question block
aux = datos[,53:62]
mca = MCA(aux, ncp = ncol(aux)) #Multiple Correspondence Analysis
coord = get_mca_ind(mca)
coord = as.data.frame(coord$coord)
p = hclust(dist(t(coord)))
plot(p)
rect.hclust(p, k = 7)

kmodes(data = na.omit(aux),modes = 1,iter.max = 500)


#############################
# INDEPENDENCE TESTS
#############################
#Independence test for age
c = 0*1:65
aux = datos[which(demo$Edad!="66-75"),]
for (i in 1:65){
  m = chisq.test(table(aux[,i], aux$edad_cat)[,-c(1,7)])
  c[i] = m$p.value
}
length(which(c<=0.05))
barplot(c)

#Independence test for sex
c = 0*1:65
aux = datos[which(demo$Edad!="66-75"),]
for (i in 1:65){
  m = chisq.test(table(aux[,i], aux$`Sexo Biologico`))
  c[i] = m$p.value
}
length(which(c<=0.05))
barplot(c)

#Independence test for level of education
c = 0*1:65
aux = datos[which(demo$Edad!="66-75"),]
for (i in 1:65){
  m = chisq.test(table(aux[,i], aux$`Nivel Academico`))
  c[i] = m$p.value
}
length(which(c<=0.05))
barplot(c)

#Independence test for socioeconomic status
c = 0*1:65
aux = datos[which(demo$Edad!="66-75"),]
for (i in 1:65){
  m = chisq.test(table(aux[,i], aux$`Estrato socioeconomico del lugar donde usted vive`))
  c[i] = m$p.value
}
length(which(c<=0.05))
barplot(c)

#Independence test for geographic region
c = 0*1:65
aux = datos[which(demo$Edad!="66-75"),]
for (i in 1:65){
  m = chisq.test(table(aux[,i], aux$Region))
  c[i] = m$p.value
}
length(which(c<=0.05))
barplot(c)

#Independence test for civil status
c = 0*1:65
aux = datos[which(demo$Edad!="66-75"),]
for (i in 1:65){
  m = chisq.test(table(aux[,i], aux$`Estado civil`))
  c[i] = m$p.value
}
length(which(c<=0.05))
barplot(c)

#####################################
# MEASURE OF ASSOCIATION - CRAMER'S V
#####################################
#Across the entire survey
names(datos)
aux = datos[,c(1:42,44:65,67,73:78)]
cramerv <- matrix(0*1:ncol(aux)*ncol(aux),nrow = ncol(aux), ncol = ncol(aux))
for(i in 1:ncol(aux)){
  for (j in 1:ncol(aux)){
    c = assocstats(table(aux[,i],aux[,j]))
    cramerv[i,j] = c$cramer  
  }
}
corrplot(cramerv-diag(diag(cramerv)), method = "color")

#Within questions blocks
for (k in 1:nrow(grupos_preguntas)){
  print(k)
  aux = datos[,grupos_preguntas[k,1]:grupos_preguntas[k,2]]
  po = as.numeric(substr(x = colnames(aux)[1],start = 2,stop = 5))-1
  for (j in 1:ncol(aux)){ colnames(aux)[j] = preguntas[po+j,2] }
  cramerv <- matrix(0*1:ncol(aux)*ncol(aux),nrow = ncol(aux), ncol = ncol(aux))
  for(i in 1:ncol(aux)){
    for (j in 1:ncol(aux)){
      c = assocstats(table(aux[,i],aux[,j]))
      cramerv[i,j] = c$cramer  
    }
  }
  jpeg(paste(grupos_preguntas[k,3],".jpg",sep = ""))
  print(corrplot(cramerv-diag(diag(cramerv)), method = "color"))
  dev.off()
}

#Within demographic variables
aux = datos[,74:78]
cramerv <- matrix(0*1:ncol(aux)*ncol(aux),nrow = ncol(aux), ncol = ncol(aux))
for(i in 1:ncol(aux)){
  for (j in 1:ncol(aux)){
    c = assocstats(table(aux[,i],aux[,j]))
    cramerv[i,j] = c$cramer  
  }
}
jpeg("demographics.jpg")
print(corrplot(cramerv-diag(diag(cramerv)), method = "color"))
dev.off()

#Association between each question block against demographic profile
for (k in 1:nrow(grupos_preguntas)){
  print(k)
  aux = datos[,grupos_preguntas[k,1]:grupos_preguntas[k,2]]
  po = as.numeric(substr(x = colnames(aux)[1],start = 2,stop = 5))-1
  for (j in 1:ncol(aux)){ colnames(aux)[j] = preguntas[po+j,2] }
  cramerv <- matrix(0*1:ncol(aux)*ncol(demo[,-6]),nrow = ncol(aux), ncol = ncol(demo[,-6]))
  for(i in 1:ncol(aux)){
    for (j in 1:ncol(demo[,-6])){
      c = assocstats(table(aux[,i],demo[,j]))
      cramerv[i,j] = c$cramer  
    }
  }
  jpeg(paste(grupos_preguntas[k,3],".jpg",sep = ""))
  print(corrplot(cramerv, method = "color"))
  dev.off()
}

#############################
# VISUALIZATION
#############################
setwd("~/Desktop")

###### HEAT PLOTS
for (i in 1:nrow(grupos_preguntas)){
  print(i)
  aux = datos[,grupos_preguntas[i,1]:grupos_preguntas[i,2]]
  po = as.numeric(substr(x = colnames(aux)[1],start = 2,stop = 5))-1
  for (j in 1:ncol(aux)){ colnames(aux)[j] = preguntas[po+j,2] }
  name = paste("Heat",grupos_preguntas[i,3],".jpg",sep = "")
  jpeg(name)
  m = likert(aux)
  print(plot(m,type = "heat"))
  dev.off()
}

###### BAR PLOTS
for(i in 1:78){
  print(i)
  c = as.data.frame(table(datos[,i]))
  m = ggplot(data = c, aes(x = Var1, y = Freq))+geom_bar(stat = 'identity') +geom_text(aes(label = Freq),color = "white",vjust = 1)+labs(x = "",y="Frecuencia")
  jpeg(paste("P",i,".jpg",sep = ""))
  print(m)
  dev.off()  
}

head(datos[,77])
i = 68
c = as.data.frame(table(datos[,i]))
c  =c[order(c[,2],decreasing = T),]
c$Var1 = factor(c$Var1,levels = c$Var1[order(c$Freq)])
m = ggplot(data = c, aes(x = Var1, y = Freq))+geom_bar(stat = 'identity') +geom_text(aes(label = Freq),hjust = -0.1)+labs(x = "",y="Frecuencia")+coord_flip()

jpeg(paste("P",i,".jpg",sep = ""))
print(m)
dev.off()  

###### LIKERT
for (i in 1:nrow(grupos_preguntas)){
  print(i)
  aux = datos[,grupos_preguntas[i,1]:grupos_preguntas[i,2]]
  po = as.numeric(substr(x = colnames(aux)[1],start = 2,stop = 5))-1
  for (j in 1:ncol(aux)){ colnames(aux)[j] = preguntas[po+j,2] }
  name = paste(grupos_preguntas[i,3],"NotCentered",".jpg",sep = "")
  jpeg(name)
  m = likert(aux)
  print(plot(m,centered = F))
  dev.off()
}

###### LIKERT - grouped by demographic variables
#Sex
for (i in 1:65){
  print(i)
  m = likert(datos[,i,drop = F],grouping = datos$`Sexo Biologico`)
  name = paste("P",i,"NotCentered",".jpg",sep = "")
  jpeg(name)
  print(plot(m,centered = F)) 
  dev.off()
  
  name = paste("P",i,"Centered",".jpg",sep = "")
  jpeg(name)
  print(plot(m)) 
  dev.off()
}

#Region
for (i in 1:65){
  print(i)
  m = likert(datos[,i,drop = F],grouping = datos$Region)
  name = paste("P",i,"NotCentered",".jpg",sep = "")
  jpeg(name)
  print(plot(m,centered = F)) 
  dev.off()
  
  name = paste("P",i,"Centered",".jpg",sep = "")
  jpeg(name)
  print(plot(m)) 
  dev.off()
}

#Age
for (i in 1:65){
  print(i)
  m = likert(datos[,i,drop = F],grouping = datos$edad_cat)
  name = paste("P",i,"NotCentered",".jpg",sep = "")
  jpeg(name)
  print(plot(m,centered = F)) 
  dev.off()
  
  name = paste("P",i,"Centered",".jpg",sep = "")
  jpeg(name)
  print(plot(m)) 
  dev.off()
}

#Civil Status
for (i in 1:65){
  print(i)
  m = likert(datos[,i,drop = F],grouping = datos$`Estado civil`)
  name = paste("P",i,"NotCentered",".jpg",sep = "")
  jpeg(name)
  print(plot(m,centered = F)) 
  dev.off()
  
  name = paste("P",i,"Centered",".jpg",sep = "")
  jpeg(name)
  print(plot(m)) 
  dev.off()
}

#Socioeconomic status
for (i in 1:65){
  print(i)
  m = likert(datos[,i,drop = F],grouping = datos$`Estrato socioeconomico del lugar donde usted vive`)
  name = paste("P",i,"NotCentered",".jpg",sep = "")
  jpeg(name)
  print(plot(m,centered = F)) 
  dev.off()
  
  name = paste("P",i,"Centered",".jpg",sep = "")
  jpeg(name)
  print(plot(m)) 
  dev.off()
}

#Academic level
for (i in 1:65){
  print(i)
  m = likert(datos[,i,drop = F],grouping = datos$`Nivel Academico`)
  name = paste("P",i,"NotCentered",".jpg",sep = "")
  jpeg(name)
  print(plot(m,centered = F)) 
  dev.off()
  
  name = paste("P",i,"Centered",".jpg",sep = "")
  jpeg(name)
  print(plot(m)) 
  dev.off()
}

#Demographic Cluster
datos_aux = datos[-c(1:6),]
datos_aux$cluster = pam_fit$clustering
for (i in 1:65){
  print(i)
  m = likert(datos_aux[,i,drop = F],grouping = datos_aux$cluster)
  name = paste("P",i,"Centered",".jpg",sep = "")
  jpeg(name)
  print(plot(m)) 
  dev.off()
}


#############################
# PERCEPTION OF INSTITUTIONS
#############################
aux = datos[,c(25:32)]
aux = na.omit(aux)
names(aux)

mca = MCA(aux, ncp = ncol(aux))
coord = get_mca_ind(mca)
coord = as.data.frame(coord$coord)
p = hclust(dist(t(coord)))
plot(p)
rect.hclust(p, k = 7)

#K-modes
c = kmodes(data = aux[1:8],modes = 4,iter.max = 500,weighted = F)
c$modes
c$size
c$cluster

#K-medoids
gower_dist = daisy(x = datos[,1:65], metric = "gower")

sil_width <- c(NA)
for(i in 2:20){
  print(i)
  pam_fit <- pam(gower_dist, diss = TRUE,k = i)
  sil_width[i] <- pam_fit$silinfo$avg.width
}
plot(1:20, sil_width, xlab = "Number of clusters", ylab = "Silhouette Width")
lines(1:20, sil_width)
which.max(sil_width)

pam_fit <- pam(gower_dist, diss = TRUE,k = 10)
pam_results <- aux %>% mutate(cluster = pam_fit$clustering)
table(pam_fit$clustering)
aux[pam_fit$id.med,]

tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
plot(tsne_obj$Y,col = pam_fit$clustering)

#Likert Plot
names(aux)
m = likert(aux[,1:8],grouping = aux$`Sexo Biologico`)
plot(m,type = "density")
plot(m)
plot(m,centered = F)

#Decision Tree
z = 4
aux = datos[,c(z,73:77)]
for (i in 1:ncol(aux)){
  aux = aux[aux[,i] %in% names(table(aux[,i]))[table(aux[,i])>=15],]
}

dec_tree = aux
for (i in 1:ncol(dec_tree)){
  dec_tree[,i]=factor(dec_tree[,i])
}
m = train(P4~.,data = dec_tree, method = "rf")
m
table(datos[,z])/sum(table(datos[,z]))


summary(dec_tree)
head(dec_tree)
m = rpart(P1~.,data = dec_tree,method = "class")
m
m = randomForest(P1~.,data = dec_tree)
summary(m)
plot(m)


#HOMALS
names(dec_tree)
aux = datos[,c(3,73:77)]
for (i in 1:ncol(aux)){
  aux = aux[aux[,i] %in% names(table(aux[,i]))[table(aux[,i])>=15],]
}

m = homals(aux[,1:19],sets = list(c(1:8),c(9:19)))
head(m$cat.centroids)
unlist(m$cat.centroids)

p = m$cat.centroids[[1]]
rownames(p) = paste(names(m$cat.centroids)[i],".",rownames(p))
for (i in 2:ncol(aux)){
  k = m$cat.centroids[[i]]
  rownames(k) = paste(names(m$cat.centroids)[i],".",rownames(k))
  p = rbind(p,k)
}

plot(p[,1],p[,2])
text(p[,1],p[,2],labels=rownames(p),cex = 0.5)

table(datos[,53])

plot_ly(x = p[,1], y = p[,2], z = p[,3])

plot_ly(x = m$objscores[,1], y = m$objscores[,2], z = m$objscores[,3])
plot3dstatic(m, plot.type = "loadplot")
plot(m, plot.type = "spanplot", plot.dim = c(1, 3), var.subset = 1)
plot(m, plot.type = "labplot", plot.dim = c(1, 3), var.subset = 1)
plot(m, plot.type = "laodplot", plot.dim = c(2, 3), var.subset = 1)

