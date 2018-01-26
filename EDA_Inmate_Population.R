"Preparar datos y variables"
PPL_2016 <- read.csv("~/Documents/MinJusticia/Proyectos/Adriana/2016/Cifras y Conceptos/Data/20160209.csv", sep=";")
"Ajustar formato del archivo que subi"
MJ_PPL_2016 = data.frame(INTERNO = as.factor(x = PPL_2016[,1]))
MJ_PPL_2016$MODALIDAD = as.factor(x = PPL_2016[,2])
MJ_PPL_2016$FECHA_INGRESO = as.Date(PPL_2016$FECHA_INGRESO,"%d/%m/%Y")
MJ_PPL_2016$ESTADO  = as.factor(x = PPL_2016[,4])
MJ_PPL_2016$FECHA_SALIDA = as.Date(PPL_2016$FECHA_SALIDA,"%d/%m/%Y")
MJ_PPL_2016$SITUACION_JURIDICA  = as.factor(x = PPL_2016[,6])
MJ_PPL_2016$FECHA_NACIMIENTO = as.Date(PPL_2016$FECHA_NACIMIENTO,"%d/%m/%Y")
MJ_PPL_2016$GENERO = as.factor(x = PPL_2016[,8])
MJ_PPL_2016$MESES_CONDENA = PPL_2016$MESES_DE_CONDENA
MJ_PPL_2016$REINCIDENCIAS = PPL_2016$REINCIDENCIAS
MJ_PPL_2016$ESTABLECIMIENTO = PPL_2016$ESTABLECIMIENTO
MJ_PPL_2016$REGIONAL = PPL_2016$REGIONAL

"Quedarme solo con los datos de los internos, sin modalidad"
internos = MJ_PPL_2016[-which(MJ_PPL_2016$GENERO == unique(MJ_PPL_2016$GENERO)[1]),-2]
internos$EDAD = floor(as.numeric(as.Date("2016-08-01")-internos$FECHA_NACIMIENTO)/365)
internos$EDAD_CAPTURA = floor(as.numeric((internos$FECHA_INGRESO-internos$FECHA_NACIMIENTO)/365))
internos$FECHA_SALIDA_ESPERADA = internos$FECHA_INGRESO + 30*internos$MESES_CONDENA
internos$MESES_RECLUSION = as.numeric(as.Date("2016-08-01")-internos$FECHA_INGRESO)/30
internos$CUMPLIMIENTO = internos$MESES_RECLUSION/internos$MESES_CONDENA
internos$RANGO_ETARIO = cut(internos$EDAD,breaks = c(17,25,30,40,50,100),labels = c("18-25 años","26-30 años","31-40 años","41-50 años","50+ años"))

"elimino los que tienen mal dato de edad"
internos = internos[which(internos$EDAD>=18),]
internos = internos[which(!is.na(internos$EDAD)),]

"TOMAR FOTO"
table(table(internos[,1]))
"Incluye solo los que aparecen 1 vez"
c = levels(internos$INTERNO)[which(table(internos$INTERNO)==1)]
foto = internos[internos$INTERNO%in%c,] 
table(table(foto$INTERNO))

c = levels(internos$INTERNO)[which(table(internos$INTERNO)>1)]
internos[internos$INTERNO == c[1],]
ultima_entrada = function (x){
  df = internos[internos$INTERNO == x,]
  return (df[which.max(df$FECHA_INGRESO),])
}

foto_nrows = dim(foto)[1]

for (i in 1:length(c)){
  foto[foto_nrows+i,] = ultima_entrada(c[i])  
  print(i)
}

#Construccion tabla crimen
internos_crimen = array(0,c(length(unique(foto$INTERNO)),length(unique(PPL_2016$MODALIDAD))))
internos_crimen = as.data.frame(internos_crimen)
delitos = unique(PPL_2016$MODALIDAD)
colnames(internos_crimen) = delitos
internos_crimen$INTERNO = foto$INTERNO

for (i in 1:length(delitos)){
  print (i)
  x = match(unique(MJ_PPL_2016$INTERNO[which(MJ_PPL_2016$MODALIDAD == delitos[i])]),unique(foto$INTERNO))
  internos_crimen[x[which(!is.na(x))],i] = 1 
}
head(internos_crimen)

#Union foto y tabla de crimen
foto[,(dim(foto)[2]+1):(dim(foto)[2]+dim(internos_crimen)[2])] = internos_crimen[,-dim(internos_crimen)[2]]
foto =foto[,-dim(foto)[2]]
foto_backup = foto

#Seleccion de modalidades
c = grep("CATOR",colnames(foto))
c
colnames(foto)

c = colSums(foto[,18:dim(foto)[2]])
c[order(c,decreasing = T)][1:10]

ind_mod = c(17,16,21,24,23,18,31,22,42,78,19,26,92,20)
sub_foto = foto[,c(1:17,ind_mod+2)]
colSums(sub_foto[,18:30])

#ANALISIS DESCRIPTIVO
table(rowSums(sub_foto[,18:30]))

"Tabla cruzada de cuántos delitos están dentro y cuántos por fuera"
in_out = data.frame(in_14 = rowSums(foto[,ind_mod+2]))
in_out$out_14 = rowSums(foto[,-c(1:17,ind_mod+2)])
table(in_out[,1],in_out[,2])

"me quedo con los internos que cometieron algun delito de los 14"
internos_14 = sub_foto[-which(rowSums(sub_foto[,18:30])==0),]

"analisis de genero, situacion juridica, modalidad, rango etario"

"univariado"
table(internos_14$GENERO)
table(internos_14$SITUACION_JURIDICA)
table(internos_14$RANGO_ETARIO)

"multivariado"
table(internos_14$GENERO, internos_14$SITUACION_JURIDICA)
table(internos_14$GENERO, internos_14$RANGO_ETARIO)
table(internos_14$SITUACION_JURIDICA, internos_14$RANGO_ETARIO)

colnames(internos_14)
index_var = 17
levels_var = levels(internos_14[,index_var])
table_var = array(0,c(length(ind_mod),length(levels_var)))
rownames(table_var) = colnames(internos_14[,18:31])
colnames(table_var) = levels(internos_14[,index_var])
for (i in 1:14){
  for (j in 1:length(levels_var)){
    table_var[i,j] = length(which((internos_14[,17+i]==1)&(internos_14[,index_var]==levels_var[j]))) 
  }
}
table_var

"calcular numero de cargos"
internos_14$CARGOS = as.factor(rowSums(internos_14[,18:31]))
barplot(table(internos_14$CARGOS))

table(internos_14$CARGOS, internos_14$SITUACION_JURIDICA)
table(internos_14$CARGOS, internos_14$RANGO_ETARIO)
table(internos_14$CARGOS, internos_14$GENERO)

"promedio condena"
internos_graph = internos_14[which((internos_14$SITUACION_JURIDICA == "Condenado")&(internos_14$MESES_CONDENA<=750)),]
boxplot(internos_graph$MESES_CONDENA~internos_graph$RANGO_ETARIO)
dim(internos_graph)[1]/dim(internos_14[which(internos_14$SITUACION_JURIDICA == "Condenado"),])[1]

cdplot(internos_14$RANGO_ETARIO[which(internos_14$SITUACION_JURIDICA == "Condenado")]~internos_14$MESES_CONDENA[which(internos_14$SITUACION_JURIDICA == "Condenado")])
par(new=F)
plot(ecdf(internos_graph$MESES_CONDENA[internos_graph$GENERO == "Femenino"]))
lines(ecdf(internos_graph$MESES_CONDENA[internos_graph$GENERO == "Masculino"]))

par(new=F)
plot(density(internos_graph$MESES_CONDENA[internos_graph$GENERO == "Femenino"]),main = "",ylim = c(0,0.020),col = "red",xlab = "Meses de condena")
lines(density(internos_graph$MESES_CONDENA[internos_graph$GENERO == "Masculino"]),main = "",col="blue")

ks.test(internos_graph$MESES_CONDENA[internos_graph$GENERO == "Femenino"],internos_graph$MESES_CONDENA[internos_graph$GENERO == "Masculino"])
ks.test(internos_graph$MESES_RECLUSION[internos_graph$GENERO == "Femenino"],internos_graph$MESES_RECLUSION[internos_graph$GENERO == "Masculino"])

c = summary(internos_14$MESES_CONDENA[which(internos_14[,18]==1)])
names(c)
colnames(internos_14)
table_var = array(0,c(length(ind_mod),6))
rownames(table_var) = colnames(internos_14[,18:31])
colnames(table_var) = names(c)
for (i in 1:14){
  c = summary(internos_14$MESES_RECLUSION[which((internos_14[,17+i]==1)&(internos_14$SITUACION_JURIDICA == "Condenado"))])    
  for (j in 1:6){
      table_var[i,j] = c[j]
    }
}
table_var

c = summary(internos_14$FECHA_INGRESO[which((internos_14$FECHA_INGRESO<=2015)&(internos_14$FECHA_INGRESO>=2009))])
names(c)
colnames(internos_14)
table_var = array(0,c(length(ind_mod),7))
rownames(table_var) = colnames(internos_14[,18:31])
colnames(table_var) = c("2009","2010","2011","2012","2013","2014","2015")
for (i in 1:14){
  for (j in 1:7){
    c = length(internos_14$MESES_RECLUSION[which((internos_14[,17+i]==1)&(format(internos_14$FECHA_INGRESO,"%Y")==2008+j))])    
    table_var[i,j] = c
  }
}
table_var
colSums(table_var)
format(internos_14$FECHA_INGRESO,"%Y")

par(new=F)
plot(density(internos_graph$MESES_RECLUSION[internos_graph$GENERO == "Femenino"]),main = "",ylim = c(0,0.030),col = "red",xlab = "Meses de reclusión")
lines(density(internos_graph$MESES_RECLUSION[internos_graph$GENERO == "Masculino"]),main = "",col="blue")

c = internos_14[-which(is.infinite(internos_14$CUMPLIMIENTO)),]
plot(ecdf(c$CUMPLIMIENTO[c$GENERO == "Masculino"]),xlim = c(0,1),col="blue",main="")
lines(ecdf(c$CUMPLIMIENTO[c$GENERO == "Femenino"]),xlim = c(0,1),col = "red")

boxplot(c$MESES_RECLUSION~c$RANGO_ETARIO)

summary(internos_graph$MESES_RECLUSION[internos_graph$RANGO_ETARIO == levels(internos_graph$RANGO_ETARIO)[6]])

d = summary(internos_14$MESES_RECLUSION[internos_graph$RANGO_ETARIO == levels(internos_graph$RANGO_ETARIO)[6]])
length(names(d))
colnames(internos_14)
table_var = array(0,c(length(levels(c$RANGO_ETARIO)),length(names(d))))
rownames(table_var) = levels(c$RANGO_ETARIO)
colnames(table_var) = names(d)
for (i in 1:length(levels(internos_14$RANGO_ETARIO))){
  d = summary(c$CUMPLIMIENTO[which((c$RANGO_ETARIO == levels(c$RANGO_ETARIO)[i])&(c$SITUACION_JURIDICA == "Condenado"))])    
  for (j in 1:length(names(d))){
    table_var[i,j] = d[j]
  }
}
table_var

table(internos_14$ESTADO,internos_14$SITUACION_JURIDICA)
alta_condenados = internos_14[which((internos_14$ESTADO=="ALTA")&(internos_14$SITUACION_JURIDICA == "Condenado")),]
table(alta_condenados$ESTADO, alta_condenados$SITUACION_JURIDICA)

plot(density(alta_condenados$MESES_CONDENA[which((alta_condenados$MESES_CONDENA<=500)&(alta_condenados$MESES_CONDENA>0))]),main="",xlab = "Meses condena")
summary((alta_condenados$MESES_CONDENA[which((alta_condenados$MESES_CONDENA<=500)&(alta_condenados$MESES_CONDENA>0))]))
summary(alta_condenados$MESES_CONDENA[which((alta_condenados$MESES_CONDENA>0))])

plot(ecdf(c$CUMPLIMIENTO),xlim = c(0,10))

c= alta_condenados[!is.infinite(alta_condenados$CUMPLIMIENTO),]
c = c[c$CUMPLIMIENTO>=1,]
hist(c$CUMPLIMIENTO)

inter_max = c[which.max(c$CUMPLIMIENTO),]
PPL_2016[which(PPL_2016$INTERNO == "140677"),]

colSums(c[,18:31])
summary(c$CUMPLIMIENTO)

c = alta_condenados[-which((alta_condenados$MESES_CONDENA==0)),]
par(new=F)
plot(density(c$MESES_CONDENA[c$GENERO == "Femenino"]),main = "",ylim = c(0,0.020),col = "red",xlab = "Meses de condena")
lines(density(c$MESES_CONDENA[c$GENERO == "Masculino"]),main = "",col="blue")

summary(c$MESES_CONDENA[c$GENERO == "Femenino"])

c= alta_condenados[!is.infinite(alta_condenados$CUMPLIMIENTO),]
c = c[c$CUMPLIMIENTO>1,]
d = summary(c$CUMPLIMIENTO)
length(names(d))
colnames(c)
table_var = array(0,c(length((ind_mod)),length(names(d))))
rownames(table_var) = colnames(alta_condenados[,18:31])
colnames(table_var) = names(d)
for (i in 1:length((ind_mod))){
  d = summary(c$CUMPLIMIENTO[which(c[,17+i]==1)])
  d = round(d,digits = 1)
  for (j in 1:length(names(d))){
    table_var[i,j] = d[j]
  }
}
table_var

dif = foto[which((foto$FECHA_SALIDA_ESPERADA<=as.Date("2016-06-15"))&(foto$FECHA_SALIDA_ESPERADA>=as.Date("2016-02-01"))),]
table(dif$SITUACION_JURIDICA,dif$ESTADO)

dif = foto[which((foto$FECHA_SALIDA_ESPERADA<=as.Date("2016-02-01"))),]
table(dif$SITUACION_JURIDICA,dif$ESTADO)

dif = foto[which((foto$FECHA_SALIDA_ESPERADA<=as.Date("2016-06-15"))),]
table(dif$SITUACION_JURIDICA,dif$ESTADO)

summary(dif$CUMPLIMIENTO[!is.infinite(dif$CUMPLIMIENTO)])


"Analisis de las personas que ya cumplieron condena"
alta_condenados = internos_14[which((internos_14$ESTADO=="ALTA")&(internos_14$SITUACION_JURIDICA == "Condenado")),]
alta_condenados$MESES_RECLUSION = as.numeric(as.Date("2016-02-01")-alta_condenados$FECHA_INGRESO)/30
alta_condenados$CUMPLIMIENTO = alta_condenados$MESES_RECLUSION/alta_condenados$MESES_CONDENA

dif = alta_condenados[which(alta_condenados$CUMPLIMIENTO>=1),]
dif = dif[!is.infinite(dif$CUMPLIMIENTO),]
table(dif$SITUACION_JURIDICA, dif$ESTADO)
summary(dif$CUMPLIMIENTO)
length(unique(dif$INTERNO))

table(dif$CARGOS)

colSums(dif[,18:31])

d = summary(dif$CUMPLIMIENTO)
length(names(d))
colnames(dif)
table_var = array(0,c(length((ind_mod)),length(names(d))))
rownames(table_var) = colnames(alta_condenados[,18:31])
colnames(table_var) = names(d)
for (i in 1:length((ind_mod))){
  d = summary(dif$CUMPLIMIENTO[which(dif[,17+i]==1)])
  d = round(d,digits = 1)
  for (j in 1:length(names(d))){
    table_var[i,j] = d[j]
  }
}
table_var

d = summary(dif$CUMPLIMIENTO)
length(names(d))
colnames(dif)
table_var = array(0,c(length((ind_mod)),length(names(d))))
rownames(table_var) = colnames(alta_condenados[,18:31])
colnames(table_var) = names(d)
for (i in 1:length((ind_mod))){
  d = summary(dif$MESES_CONDENA[which(dif[,17+i]==1)])
  d = round(d,digits = 1)
  for (j in 1:length(names(d))){
    table_var[i,j] = d[j]
  }
}
table_var

write_dif = dif
write_dif$MESES_RECLUSION = round(write_dif$MESES_RECLUSION, digits = 2)
write_dif$CUMPLIMIENTO = round(write_dif$CUMPLIMIENTO, digits = 2)
write.table(write_dif, file = "minjusticia.csv", sep = ",", col.names = TRUE)
