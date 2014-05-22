##############################################################################
##############Encuesta Nacional de Salud y Nutrición 2011-2014################
##############TABLAS DE COMPOSICION NUTRICIONAL ##############################
##############RECETAS ESTANDARIZADAS #########################################
##############################################################################

#Coordinadora de la Investigacion ENSANUT 2011-2014: Wilma Freire.
#Cita recomendada:
#Ramírez-Luzuriaga MJ, Silva-Jaramillo MK, Belmont P, and Freire WB. 
#Tabla de Composición de Alimentos Del Ecuador: Compilación Del Equipo 
#Técnico de La ENSANUT-ECU. Compilación. 
#Quito, Ecuador: Ministerio de Salud Pública del Ecuador, 2014.
#A BibTeX entry for LaTeX users is:
#@techreport{ramirez-luzuriaga_mj_tabla_2014,
#            address = {Quito, Ecuador},
#            type = {Compilación},
#            title = {Tabla de composición de alimentos del Ecuador: 
#            Compilación del Equipo técnico de la {ENSANUT-ECU}},
#            institution = {Ministerio de Salud Pública del Ecuador},
#            author = {{Ramírez-Luzuriaga {MJ}} and {Silva-Jaramillo {MK}} and
#            {Belmont P} and {Freire {WB}}},
#            year = {2014}
#}

#Git rct_est
#Tabla de consumo completa:
read.table("consb170513.csv",sep=",",h=T)->co
gsub("f112","v",names(co))->names(co)
co[,c(1,7,30,33,14,15,17,20,29,61)]->co
co[which(co$v07<66),]->co
names(co)[3]<-"idh"
#tabla de codigos 209 unicos:
read.table ("209cor.txt",sep="\t",h=T)->cc

#Composicion de la recetas orig. por n° de ingredientes:
read.table("rct77.txt",sep="\t",h=T)->ret
for (i in 1:length(table(ret$nprep))){
  ret[which(ret$nprep==names(table(ret$nprep))[i]),"npc"]<-i
} 
for (i in 1:nrow(ret)) length(which(ret$npc==ret$npc[i]))->ret$ni[i]

#Correccion de Codigos sinonimos:
cc[match(ret[,5],cc[,2]),1]->ret$cec
cc[match(co[,7],cc[,2]),1]->co$v09u
co[which(is.na(co$v07a)),"v07a"]<-1
#Correccion de los porcentajes Agua
ret[-which(ret$cec==878),]->ret
for (i in 1:nrow(ret)) {
  sum(ret[which(ret$npc==ret$npc[i]),"cantidad"])->ret$prct[i]
}
ret$prct<-ret$cantidad/ret$prct
#Identificador de conjunto
names(table(co$idh))->tp
cbind(as.numeric(tp),1:length(tp))->tp
tp[match(co$idh,tp[,1]),2]->co$idhs
rm(tp)
co$idc<-co$idhs*10000+co$persona*100+co$v07
as.numeric(names(table(co$idc)))->ref
cbind(ref,NA)->ref
for (i in 2:152) cbind(ref,NA)->ref

#Interseccion: porcentaje concordiendo segun las cantidades de cada 
#receta estandarizada original para cada conjunto
j<-1;i<-1
ptm <- proc.time()
for (j in 1:length(table(ret$npc))){
  b<-which(ret$npc==j)
  c<-ret[b,"cec"]
for (i in 1:nrow(ref)) {
    d<-co[which(co$idc==ref[i]),"v09u"]
    e<-intersect(d,c)
    f<-b[match(e,c)]
    ref[i,(j+1)]<-sum(ret[f,"prct"])
}
proc.time() - ptm
as.data.frame(ref)->ref
#write(ref,file="prctret.txt",sep=",")

#A. % valido de la matriz de composicion de los conjuntos de transicion 
#->segun % se calcula la receta
#creacion de base
read.table("Rct77addon.txt",sep="\t",h=T)->rta
read.table("cdf.txt",sep="\t",h=T)->cdf
cbind(rta,cdf[match(rta$cod_ec,cdf$f09),])->rta
cc[match(rta[,6],cc[,2]),1]->rta$cec
ret[match(rta$nprep,ret$nprep),"npc"]->rta$npc

#Correccion de los porcentajes sacando el  Agua
co[-which(co$v09u==878),]->co

#Cantidad de recetas empatadas segun el porcentaje de empate de ingrediente
#tabla de referencia del porcentaje que junta mas de 100 conjunto por receta.
pref<-as.data.frame(matrix(NA,2,2))
for (j in seq(0.1,0.9,by=0.05)){
  for ( i in 2:153){ 
    (length(ref[which(ref[,i]>j),i]))->y[i]
  }
  pref[which(y>100),1]<-j
  pref[which(y>100),2]<-y[which(y>100)]
}

for (i in 2:153)  pref[i,3]<-length(rta[which(rta$npc==i-1),1])
names(pref)<-c("prctref","nretempat","ningret")

#Extraction de los porcentajes
for (i in c(2:43,45:113,115:153)){
  ptm <- proc.time()
  co$prct<-NA 
  for (k in 1:pref[i,2]){
  sum(co[which(co$idc==ref[which(ref[,i]>pref[i,1]),1][k]),"v20f"],na.rm=T)->
    co$prct[which(co$idc==ref[which(ref[,i]>pref[i,1]),1][k])]
  co$v20f[which(co$idc==ref[which(ref[,i]>pref[i,1]),1][k])] / 
    co$prct[which(co$idc==ref[which(ref[,i]>pref[i,1]),1][k])]->
    co$prct[which(co$idc==ref[which(ref[,i]>pref[i,1]),1][k])]
  }
  for (l in 1:pref[i,3]) {
  mean(co[which(co$prct>0 & co$v09u==rta[which(rta$npc==i-1),"cec"][l]),"prct"]) 
  -> rta[which(rta$npc==i-1)[l],"%nat"]
  print(proc.time() - ptm)
  }
}
#Receta 44 y 114 sin empates => se conservaro las recetas originales: 
#Chicha de Chonta y Pincho de chontacuro
rta[which(rta$npc==43),"cantidad"]/sum(rta[which(rta$npc==43),"cantidad"]) -> 
  rta[which(rta$npc==43),"%nat"]
rta[which(rta$npc==113),"cantidad"]/sum(rta[which(rta$npc==113),"cantidad"]) -> 
  rta[which(rta$npc==113),"%nat"]
#Promedio de los porcentajes :  
for (i in 1:152) {
  rta[which(rta$npc==i),"%nat"]/sum(rta[which(rta$npc==i),"%nat"],na.rm=T)
  ->rta[which(rta$npc==i),"%nrm"]
write.table(rta,file="rta.txt",rownames=F)

##############Matching with Lenvenshtein distance
#library(RecordLinkage)
#ClosestMatch2 = function(string, stringVector){distance = levenshteinSim(string, stringVector);
#  stringVector[distance == max(distance)]}
#a<-list()
#for (j in 1:length(tn77)) ClosestMatch2(names(tn77)[j],names(tnc))->a[[j]]
