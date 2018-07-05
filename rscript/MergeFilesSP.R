
################################################ 
### Data Management for Excel SP
### Jun 2018
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



#setwd("C:/Users/Mauro/Desktop/SP_excel")#################################
setwd("~/GitHub/Experimento-Metricas-y-Formatos-SCOMP/Excel_SP")


rm(list = ls())

##################################
### Generating and merging files
#################################

## Leer nombre de archivos

perfil.files <- list.files("./CSV/", recursive = T, pattern = 'co_.+csv', full.names = T)

##Leer Columnas

all.files <- NULL
for(bf in perfil.files){
  temp <- read.csv2(bf, as.is = T)
  valid_col <- ncol(temp)-3
  for(i in 1:valid_col)temp[ , i] <- gsub("\\s","", temp[, i])
  all.files <- rbind.fill(all.files, temp)
}

table(table(all.files$perfil))
##Agrego ID Unico = Grupo + Perfil
all.files <- cbind(id = paste(all.files$grupo,all.files$perfil),all.files )

##Cambio ID a variable Categorica
str(all.files)
names(all.files)
numerize <- names(all.files)[c(1)]
for(nu in numerize){
  all.files[,nu]<- as.numeric(all.files[ ,nu])
}

## Genero nuevo archivo con los cambios
write.table(all.files, "nuevaBD.csv", sep = ";", quote = F, row.names = T)

nuevaBD <- read.csv2("nuevaBD.csv", as.is = T)

# Ejemplo  - Tabla cuando ID = 1
#################################
data_sub <- subset(all.files, (all.files$id==1))
tbl<-ddply(data_sub, ~ perfil, summarize,  
           razon_social = data_sub$razon_social,
           pension_bruta = data_sub$val_uf_pension_bru,
           riesgo = data_sub$riesgo
)

names(tbl)<-c("Perfil", "Valor Pensión", "Razon Social", "Clasificación de Riesgo")
tbl<-xtable(tbl, caption="Treatment Summaries", label="table:sum")
print(tbl)
