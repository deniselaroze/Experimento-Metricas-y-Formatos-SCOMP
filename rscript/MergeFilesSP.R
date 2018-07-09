
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

##Cambio ID a variable Categorica, para hacer pruebas
all.files$nid<-as.numeric(all.files$id)
table(all.files$nid)



## Transformación de pensión en pesos (valor UF 27.161,48 01/07/2018)
all.files$val_uf_pension_bru<-as.numeric(all.files$val_uf_pension_bru) #make numeric
all.files$val_pesos_pension_bru<-all.files$val_uf_pension_bru*27161.48


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
  
###########################
## Function - Control
###########################


fcn.control <- function(gender, econ, mode, pair){
  id<-paste0(gender, econ, ".", mode, ".", pair)
  tbl<-all.files[all.files$id==id, c("razon_social", "val_uf_pension_bru", "riesgo")]
  names(tbl)<-c( "Razón Social", "Valor Pensión UF", "Clasificación de Riesgo")
  tbl<-xtable(tbl, caption="Leyenda del Control" )
  return(print(tbl, type="HTML", file=paste0("Tratamientos/control", id ,".html"), include.rownames=FALSE  ))
}


fcn.control("F", "nivel2", "1a_RVI_simple", "co_1a_rp" )

##########################
### Function - Treatment 1
##########################

fcn.treat1 <- function(gender, econ, mode, pair){
  id<-paste0(gender, econ, ".", mode, ".", pair)
  tbl<-all.files[all.files$id==id, c("razon_social", "val_pesos_pension_bru", "riesgo")]
  tbl$val_pesos_pension_bru<-round( tbl$val_pesos_pension_bru, 0)
  names(tbl)<-c( "Razón Social", "Valor Pensión $", "Clasificación de Riesgo")
  tbl<-xtable(tbl, caption="Leyenda del Tratamiento 1" )
  digits(tbl) <- c(0,0,0,0)
  return(print(tbl, type="HTML", file=paste0("Tratamientos/Treat1", id ,".html"), include.rownames=FALSE,
               format.args=list(big.mark = ".", decimal.mark =
                                  ","))     
  )
}


fcn.treat1("F", "nivel2", "1a_RVI_simple", "co_1a_rp" )



##########################
### Function - Treatment 2
##########################


all.files$VPN<-all.files$val_pesos_pension_bru*12*20

fcn.treat3 <- function(gender, econ, mode, pair){
  id<-paste0(gender, econ, ".", mode, ".", pair)
  tbl<-all.files[all.files$id==id, c("razon_social", "val_pesos_pension_bru", "VPN")]
  tbl$val_pesos_pension_bru<-round( tbl$val_pesos_pension_bru, 0)
  names(tbl)<-c( "Razón Social", "Valor Pensión $",  "Total VPN Pensión")
  tbl<-xtable(tbl, caption="Leyenda del Tratamiento 2" )
  digits(tbl) <- c(0,0,0,0)
  return(print(tbl, type="HTML", file=paste0("Tratamientos/Treat2", id ,".html"), include.rownames=FALSE, 
               format.args=list(big.mark = ".", decimal.mark =
                                  ","))     
  )
}


fcn.treat3("F", "nivel2", "1a_RVI_simple", "co_1a_rp" )




##########################
### Function - Treatment 3
##########################

all.files$VPN<-all.files$val_pesos_pension_bru*12*20

fcn.treat3 <- function(gender, econ, mode, pair){
  id<-paste0(gender, econ, ".", mode, ".", pair)
  tbl<-all.files[all.files$id==id, c("razon_social", "val_pesos_pension_bru", "VPN")]
  tbl$VPNDiff<- tbl$VPN - max(tbl$VPN)
  tbl$val_pesos_pension_bru<-round( tbl$val_pesos_pension_bru, 0)
  tbl$VPNDiff<-round( tbl$VPNDiff, 0)
  names(tbl)<-c( "Razón Social", "Valor Pensión $",  "Total VPN Pensión", "VPN dif")
  tbl<-xtable(tbl, caption="Leyenda del Tratamiento 3" )
  digits(tbl) <- c(0,0,0,0,0)
  return(print(tbl, type="HTML", file=paste0("Tratamientos/Treat3", id ,".html"), include.rownames=FALSE, 
               format.args=list(big.mark = ".", decimal.mark =
                                  ","))     
         )
}


fcn.treat3("F", "nivel2", "1a_RVI_simple", "co_1a_rp" )



##########################
### Function - Treatment 4 // Simulation code, this still needs to be adjusted and tested using real data
##########################

n<-12


df$Company <- factor(df$Company, levels = df$Company[rev(order(df$VPE))])
max<-max(df$VPE)+450000
min<-min(df$VPE)-500000

uf<-max(df$PensionUF)
uf<-format(uf, decimal.mark = ",", scientific = FALSE)
vpem<-max(df$VPE_mensual)
vpem<-format(vpem, big.mark = ".", scientific = FALSE)


chart_note0 <- paste("Oferta ", df$Company[df$VPE_mensual==max(df$VPE_mensual)], ": " , uf," UF mensuales",sep="")
chart_note2 <- paste(vpem," pesos",sep="")
chart_note1 <- paste(uf," UF",sep="")

point <- format_format(big.mark = ".", decimal.mark = ",", scientific = FALSE)

options(scipen=1000)
p<-ggplot(data=df, aes(x=Company, y=VPE, fill = Company)) + 
  geom_bar(stat="identity") +
  #geom_text(aes(label=VPE_mensual, vjust=-0.8)) +
  scale_fill_manual(values= rev(colorRampPalette(brewer.pal(11, "RdYlGn"))(n))) +
  theme(legend.position="") +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".",decimal.mark=",",
                                               scientific = FALSE)#,
                     #                    sec.axis = sec_axis(~./240, name = "Pensión Mensual (pesos)", labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE))
  )+
  ylab("Total Valor Económico Pensión")  + xlab("")  +
  theme(axis.text.y=element_text(size=rel(1.4) , angle=90),
        axis.title.y=element_text(size=rel(1.8) ),
        axis.text.x=element_text(size=rel(1.4), angle=90),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "Grey30", linetype = "dashed"))+
  geom_text(aes(label = paste0("$",point(pensionPesos)) , angle=90, size = 2, vjust = 0.4, hjust= -0.1)) +
  coord_cartesian(ylim=c(min,max))  #coord_flip() +

p 

ggsave("Treatment_4c.png", width=30, height = 15, units = "cm")








