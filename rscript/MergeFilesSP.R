
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
library(RColorBrewer)



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
unique(all.files$perfil2)

all.files$perfil2<-substr(all.files$perfil, start = 1, stop = 2)
unique(all.files$perfil2)

##Agrego ID Unico = Grupo + Perfil
all.files <- cbind(id = paste0( all.files$grupo,".", all.files$perfil2,".", all.files$pid),all.files )

##Cambio ID a variable Categorica, para hacer pruebas
all.files$nid<-as.numeric(all.files$id)
table(all.files$nid)



## Transformación de pensión en pesos (valor UF 27.161,48 01/07/2018)
all.files$val_uf_pension_bru<-as.numeric(all.files$val_uf_pension_bru) #make numeric
all.files$val_pesos_pension_bru<-all.files$val_uf_pension_bru*27161.48


## Genero nuevo archivo con los cambios

save(all.files, file = "nuevaBD.RData")

#################### End merge #####################################




load("nuevaBD.RData")

# Ejemplo  - Tabla cuando ID = 2
#################################
#data_sub <- subset(all.files, nid==2)
tbl<-all.files[all.files$id==2, c("razon_social", "val_uf_pension_bru", "riesgo")]
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
  return(print(tbl, type="HTML", file=paste0("Tratamientos/control", QID ,".html"), include.rownames=FALSE, 
               format.args=list(big.mark = ".", decimal.mark = ","))     
  )
}


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
  return(print(tbl, type="HTML", file=paste0("Tratamientos/Treat1", QID,".html"), include.rownames=FALSE,
               format.args=list(big.mark = ".", decimal.mark = ","))     
  )
}


##########################
### Function - Treatment 2
##########################


fcn.treat2 <- function(gender, econ, mode, pair){
  id<-paste0(gender, econ, ".", mode, ".", pair)
  tbl<-all.files[all.files$id==id, c("razon_social", "val_pesos_pension_bru", "VPN")]
  tbl$val_pesos_pension_bru<-round( tbl$val_pesos_pension_bru, 0)
  names(tbl)<-c( "Razón Social", "Valor Pensión $",  "Total VPN Pensión")
  tbl<-xtable(tbl, caption="Leyenda del Tratamiento 2" )
  digits(tbl) <- c(0,0,0,0)
  return(print(tbl, type="HTML", file=paste0("Tratamientos/Treat2", QID ,".html"), include.rownames=FALSE, 
               format.args=list(big.mark = ".", decimal.mark = ","))     
  )
}


##########################
### Function - Treatment 3
##########################

fcn.treat3 <- function(gender, econ, mode, pair){
  id<-paste0(gender, econ, ".", mode, ".", pair)
  tbl<-all.files[all.files$id==id, c("razon_social", "val_pesos_pension_bru", "VPN")]
  tbl$VPNDiff<- tbl$VPN - max(tbl$VPN)
  tbl$val_pesos_pension_bru<-round( tbl$val_pesos_pension_bru, 0)
  tbl$VPNDiff<-round( tbl$VPNDiff, 0)
  names(tbl)<-c( "Razón Social", "Valor Pensión $",  "Total VPN Pensión", "VPN dif")
  tbl<-xtable(tbl, caption="Leyenda del Tratamiento 3" )
  digits(tbl) <- c(0,0,0,0,0)
  return(print(tbl, type="HTML", file=paste0("Tratamientos/Treat3", QID ,".html"), include.rownames=FALSE, 
               format.args=list(big.mark = ".", decimal.mark = ","))     
         )
}


##########################
### Function - Treatment 4 
##########################

fcn.treat4 <- function(gender, econ, mode, pair){
  id<-paste0(gender, econ, ".", mode, ".", pair)
  tbl<-all.files[all.files$id==id, c("razon_social", "val_pesos_pension_bru", "VPN")]
  tbl$val_pesos_pension_bru<-round( tbl$val_pesos_pension_bru, 0)
  
  n<-nrow(tbl)
  
  
  tbl$Company <- factor(tbl$razon_social, levels = tbl$razon_social[rev(order(tbl$VPN))])
  max<-max(tbl$VPN, na.rm=T)+1500000
  min<-min(tbl$VPN, na.rm=T)-1000000 
  point <- format_format(big.mark = ".", decimal.mark = ",", scientific = FALSE)

  options(scipen=1000)
  p<-ggplot(data=tbl, aes(x=Company, y=VPN, fill = Company)) + 
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
    geom_text(aes(label = paste0("$",point(val_pesos_pension_bru)) , angle=90, size = 2, vjust = 0.4, hjust= -0.1)) +
    coord_cartesian(ylim=c(min,max))  #coord_flip() +

return(ggsave(paste0("Tratamientos/Treat4", QID ,".png"), width=30, height = 25, units = "cm")) 

}


##################################
###### Generating treatments
##################################

# Manual treatment generation, for testing
QID<-"qualtricsID"


all.files$VPN<-all.files$val_pesos_pension_bru*12*20

fcn.control("F", "nivel2", "1a_RVI_simple", "co_1a_rp" )

fcn.treat1("F", "nivel2", "1a", "co_1a_rp" )

fcn.treat2("F", "nivel2", "1a", "co_1a_rp" )

fcn.treat3("F", "nivel2", "1a", "co_1a_rp" )

fcn.treat4("M", "nivel4", "2a", "co_2a3a" )

fcn.treat4("F", "nivel1", "3a", "co_3a3b" )



# Simulation data that would come from Qualtrics

gender<-"F"
econ<-"nivel1"
mode1<- "2"
mode2<- "1"
pg<-"b"

mode1pg<-paste0(mode1,pg)
mode2pg<-paste0(mode2,pg)


pairvct<-c(mode1pg, mode2pg)
pairvct
pairvct<-sort(pairvct)
pairvct


pair<-paste0("co_", pairvct[1], pairvct[2])

QID<-"qualtricsID" # to be replaced by a real Qualtrics ID code, unique to each participant


vf<-c( fcn.control,  fcn.treat1,  fcn.treat2,  fcn.treat3,   fcn.treat4)

#### Random treatment assignment
selected<-sample(vf, 2, replace=FALSE)
selected

#### Generating treatment images
print(selected[[1]](gender, econ, pairvct[1], pair))
print(selected[[2]](gender, econ, pairvct[2], pair))




