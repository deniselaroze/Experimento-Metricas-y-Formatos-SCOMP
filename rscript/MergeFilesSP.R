
################################################ 
### Data Management for Excel SP
### Jul 2018
### Denise Laroze / Mlopez
################################################

library(plyr)
library(foreign)
library(ggplot2)
theme_set(theme_bw())
library(stargazer)
#library(reshape2)
#library(CBPS)
library(scales)
library(gridExtra)
#library(effects)
#library(plm)
#library(lmtest)
library(xtable)
library(tools)



#setwd("C:/Users/Mauro/Desktop/SP_excel")#################################
setwd("~/GitHub/Experimento-Metricas-y-Formatos-SCOMP/Excel_SP")


rm(list = ls())

###########################################
### Creando un sólo DF con todas los datos
###########################################

## Leer nombre de archivos

perfil.files <- list.files("./CSV/", recursive = T, pattern = 'co_.+csv', full.names = T)

##Leer Columnas

all.files <- NULL
for(bf in perfil.files){
  temp <- read.csv2(bf, as.is = T)
  valid_col <- ncol(temp)-3
  for(i in 1:valid_col)temp[ , i] <- gsub("\\s","", temp[, i])
  df <- sub(".+/CSV/", "\\1", bf)
  df<-file_path_sans_ext(df)
  temp$pid <- df
  all.files <- rbind.fill(all.files, temp)
}

table(table(all.files$perfil))
##Agrego ID Unico = Grupo + Perfil
all.files <- cbind(id = paste0( all.files$grupo,".", all.files$perfil,".", all.files$pid),all.files )

##Cambio ID a variable Categorica

all.files$nid<-as.numeric(all.files$id)
table(all.files$nid)

#str(all.files)
#names(all.files)
#numerize <- names(all.files)[c(1)]
#for(nu in numerize){
#  all.files[,nu]<- as.numeric(all.files[ ,nu])
#}

## Genero nuevo archivo con los cambios
write.table(all.files, "nuevaBD.csv", sep = ";", quote = F, row.names = T)

nuevaBD <- read.csv2("nuevaBD.csv", as.is = T)

# Ejemplo  - Tabla cuando ID = 1
#################################
#data_sub <- subset(all.files, nid==2)
tbl<-all.files[all.files$nid==2, c("razon_social", "val_uf_pension_bru", "riesgo")]
names(tbl)<-c("Valor Pensión", "Razon Social", "Clasificación de Riesgo")
tbl<-xtable(tbl, caption="Leyenda del Control" )
print(tbl, type="HTML", file="Tratamientos/control.html", include.rownames=FALSE  )
  

## Function

fcn.control <- function(gender, econ, mode, pair){
  id<-paste0(gender, econ, ".", mode, ".", pair)
  tbl<-all.files[all.files$id==id, c("razon_social", "val_uf_pension_bru", "riesgo")]
  names(tbl)<-c( "Razón Social", "Valor Pensión", "Clasificación de Riesgo")
  tbl<-xtable(tbl, caption="Leyenda del Control" )
  return(print(tbl, type="HTML", file=paste0("Tratamientos/control", id ,".html"), include.rownames=FALSE  ))
}



fcn.control("F", "nivel2", "1a_RVI_simple", "co_1a_rp" )

