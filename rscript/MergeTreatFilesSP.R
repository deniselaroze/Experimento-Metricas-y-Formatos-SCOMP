
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
library(scales)
library(gridExtra)
library(xtable)
library(tools)
library(RColorBrewer)
library(htmlTable)
library(plyr)


#setwd("C:/Users/Mauro/Desktop/SP_excel")#################################


setwd("C:/Users/Denise Laroze Prehn/Dropbox/CESS-Santiago/archive/Pensions - JFF/Design info/certificados SP")
rm(list = ls())

#Parameters
pesouf<-27205.11 ### 3 de agosto de 2018
git<-"C:/Users/Denise Laroze Prehn/Documents/GitHub/Experimento-Metricas-y-Formatos-SCOMP/Tratamientos/"



###########################################
### Creando un sólo DF con todas los datos
###########################################

## Leer nombre de archivos

perfil.files <- list.files("./csv/", recursive = T, pattern = 'co_.+csv', full.names = T)

##Leer Columnas

all.files <- NULL
for(bf in perfil.files){
  temp <- read.csv(bf, as.is = T)
  valid_col <- ncol(temp)-3
  for(i in 1:valid_col)temp[ , i] <- gsub("\\s","", temp[, i])
  df <- sub(".+/csv/", "\\1", bf)
  df<-file_path_sans_ext(df)
  temp$csvid <- df
  all.files <- rbind.fill(all.files, temp)
}

table(table(all.files$perfil))
unique(all.files$perfil)


all.files$pair<-ifelse(nchar(all.files$csvid)==5, paste0(all.files$csvid, "rp") , all.files$csvid )
unique(all.files$pair)

all.files$pair2<-ifelse(grepl("rprp", all.files$pair), paste0("co_", all.files$moda, "rp"), all.files$pair)
                        
unique(all.files$pair2)



all.files$perfil2<-substr(all.files$perfil, start = 1, stop = 2)

unique(all.files$perfil2)

table(all.files$perfil2, all.files$pair2)


##Agrego ID Unico = Grupo + Perfil
all.files$id<-paste0(all.files$grupo,".", all.files$perfil2,".", all.files$pair2)



## Transformación de pensión en pesos (valor UF 27.161,48 01/07/2018)
all.files$val_uf_pension_bru<-as.numeric(all.files$val_uf_pension_bru) #make numeric
all.files$val_uf_pension_net<-as.numeric(all.files$val_uf_pension_net) #make numeric

all.files$val_pesos_pension<-ifelse(is.na(all.files$val_uf_pension_net), all.files$val_uf_pension_bru*pesouf, 
                                    all.files$val_uf_pension_net*pesouf)
  
all.files$val_uf_pension<-ifelse(is.na(all.files$val_uf_pension_net), all.files$val_uf_pension_bru, 
                                    all.files$val_uf_pension_net)


## Genero nuevo archivo con los cambios

save(all.files, file = "nuevaBD.RData")

#################### End merge #####################################




load("nuevaBD.RData")

# Ejemplo  - Tabla cuando ID = 2
#################################
#data_sub <- subset(all.files, nid==2)
tbl<-all.files[all.files$id=="Fnivel4.1b.co_1brp", c("razon_social", "val_uf_pension", "riesgo")]
opcion <- seq.int(nrow(tbl))
tbl<-cbind(opcion, tbl)

tbl





names(tbl)<-c("Opción", "Razón Social", "Valor Pensión",  "Clasificación de Riesgo")
tbl<-xtable(tbl, caption="Leyenda del Control" )
print(tbl, type="HTML", file=paste0(git, "controlTest",".html"), include.rownames=F  )
  
###########################
## Function - Control
###########################

fcn.control <- function(gender, econ, mode, pair){
  id<-paste0(gender, econ, ".", mode, ".", pair)
  tbl<-all.files[all.files$id==id, c("razon_social", "val_uf_pension", "riesgo")]
  opcion <- seq.int(nrow(tbl))
  tbl<-cbind(opcion, tbl)
  output <- numcolwise(prettyNum)(tbl, dec = ",")
  output<-cbind(output[,1], tbl[,2], output[, 2], tbl[,4])
  return(htmlTable(output,
                   header =  c("Opci&oacuten", "Raz&oacuten Social", "Pensi&oacuten mensual  <br> en UF 
                                 <br> sin retiro de excedentes", "Clasificaci&oacuten de Riesgo <br>
                               de la Compa&ntilde;&iacutea de Seguros&lowast;"),
                   aption="Basic table with both column spanners (groups) and row groups",
                   tfoot="&lowast; Las categor&iacuteas de Clasificaci&oacuten de Riesgo que permiten a las Compa&ntilde;&iacutea ofrecer
                   Rentas Vitalicias, ordenadas de mejor a inferior clasificaci&oacuten, son las siguientes AAA 
                   (mejor clasificaci&oacuten), AA, A, BBB (inferior). Cada una de estas categor&iacuteas puede tener 
                   sub&iacutendices &quot;+&quot; o &quot;-&quot;, siendo el sub&iacutendice &quot;+&quot; mejor que el &quot;-&quot;.",
                   file=paste0(git, "control", QID ,".html"), rnames=F
                   )     
  )
}

##########################
### Function - Treatment 1
##########################

fcn.treat1 <- function(gender, econ, mode, pair){
  id<-paste0(gender, econ, ".", mode, ".", pair)
  tbl<-all.files[all.files$id==id, c("razon_social", "val_pesos_pension", "riesgo")]
  opcion <- seq.int(nrow(tbl))
  tbl<-cbind(opcion, tbl)
  tbl$val_pesos_pension<-round( tbl$val_pesos_pension, 0)
  #digits(tbl) <- c(0,0,0,0,0)
  output <- numcolwise(prettyNum)(tbl, big.mark = ".",
                                  small.mark = ",")
  output<-cbind(output[,1], tbl[,2], output[, 2], tbl[,4])
  
  return(htmlTable(output,
                   header =  c("Opci&oacuten", "Raz&oacuten Social", "Pensi&oacuten mensual en  <br> pesos al d&iacutea 03/08/2018 
                                <br> sin retiro de excedentes", "Clasificaci&oacuten de Riesgo <br>
                               de la Compa&ntilde;&iacutea de Seguros&lowast;"),
                   aption="Basic table with both column spanners (groups) and row groups",
                   tfoot="&lowast; Las categor&iacuteas de Clasificaci&oacuten de Riesgo que permiten a las Compa&ntilde;&iacutea ofrecer
                   Rentas Vitalicias, ordenadas de mejor a inferior clasificaci&oacuten, son las siguientes AAA 
                   (mejor clasificaci&oacuten), AA, A, BBB (inferior). Cada una de estas categor&iacuteas puede tener 
                   sub&iacutendices &quot;+&quot; o &quot;-&quot;, siendo el sub&iacutendice &quot;+&quot; mejor que el &quot;-&quot;.",
                   file=paste0(git, "Treat1", QID ,".html"), rnames=F
                   )   
  )
}

#########################
### Function - Treatment 2
##########################

fcn.treat2 <- function(gender, econ, mode, pair){
  id<-paste0(gender, econ, ".", mode, ".", pair)
   tbl<-all.files[all.files$id==id, c("razon_social", "val_pesos_pension", "riesgo")]
   tbl$val_pesos_pension<-round( tbl$val_pesos_pension, 0)
   tbl$pesosDiff<- tbl$val_pesos_pension - max(tbl$val_pesos_pension)
   tbl$pesosDiff<-round( tbl$pesosDiff, 0)*12
   opcion <- seq.int(nrow(tbl))
  tbl<-cbind(opcion, tbl)
 
   output <- numcolwise(prettyNum)(tbl, big.mark = ".",
                                   decimal.mark = ",")
  output<-cbind(output[,1], tbl[,2], output[, c(2,3)], tbl[,4])
  
  
  return(htmlTable(output,
                   header =  c("Opci&oacuten", "Raz&oacuten Social", "Pensi&oacuten mensual en  <br> pesos al d&iacutea 03/08/2018 
                                <br> sin retiro de excedentes", "P&eacuterdida anual estimada&dagger;",
                                "Clasificaci&oacuten de Riesgo <br> de la Compa&ntilde;&iacutea de Seguros&lowast;"),
                   aption="Basic table with both column spanners (groups) and row groups",
                   tfoot="&lowast; Las categor&iacuteas de Clasificaci&oacuten de Riesgo que permiten a las Compa&ntilde;&iacutea ofrecer Rentas Vitalicias, ordenadas de mejor a inferior
                         clasificaci&oacuten, son las siguientes AAA (mejor clasificaci&oacuten), AA, A, BBB (inferior). Cada una de estas categor&iacuteas puede tener 
                           sub&iacutendices &quot;+&quot; o &quot;-&quot;, siendo el sub&iacutendice &quot;+&quot; mejor que el &quot;-&quot;.
                          &dagger; Explicar perdida anual estimada;",
                   file=paste0(git, "Treat2", QID ,".html"), rnames=F
                   )
         )   
}

##########################
### Function - Treatment 3
##########################

fcn.treat3 <- function(gender, econ, mode, pair){
  id<-paste0(gender, econ, ".", mode, ".", pair)
  tbl<-all.files[all.files$id==id, c("razon_social", "val_pesos_pension", "VPN")]
  tbl$VPNDiff<- tbl$VPN - max(tbl$VPN)
  tbl$val_pesos_pension<-round( tbl$val_pesos_pension, 0)
  tbl$VPNDiff<-round( tbl$VPNDiff, 0)
  opcion <- seq.int(nrow(tbl))
  tbl<-cbind(opcion, tbl)
  
  output <- numcolwise(prettyNum)(tbl, big.mark = ".",
                                  decimal.mark = ",")
  output<-cbind(output[,1], tbl[,2], output[, c(2,3,4)])
  
  
  
  return(htmlTable(output,
                   header =  c("Opci&oacuten", "Raz&oacuten Social", "Pensi&oacuten mensual en  <br> pesos al d&iacutea 03/08/2018",  
                               "Valor estimado <br>largo plazo&lowast;", "P&eacuterdida total <br> estimada&dagger;"),
                   aption="Basic table with both column spanners (groups) and row groups",
                   tfoot="&lowast; Estimaci&oacuten del valor total de la oferta de pensi&oacuten, asumiendo una esperanza de vida promedio y descontando
                    el costo de los per&iacuteodos garantizados; 
                   &dagger; Estimaci&oacuten del dinero que dejar&iacutea de ganar sobre el transcurso de una vida promedio;",
                   file=paste0(git, "Treat3", QID ,".html"), rnames=F
  )
  )   
}

##########################
### Function - Treatment 4 
##########################

id<-"Fnivel4.1b.co_1brp"


fcn.treat4 <- function(gender, econ, mode, pair){
  id<-paste0(gender, econ, ".", mode, ".", pair)
  
  tbl<-all.files[all.files$id==id, c("razon_social", "val_pesos_pension", "VPN")]
  tbl$val_pesos_pension<-round( tbl$val_pesos_pension, 0)
  opcion <- seq.int(nrow(tbl))
  tbl<-cbind(opcion, tbl)
  
  n<-nrow(tbl)
  
  
  tbl$Company <- factor(tbl$razon_social, levels = tbl$razon_social[rev(order(tbl$VPN))])
  max<-max(tbl$VPN, na.rm=T)+1500000
  min<-min(tbl$VPN, na.rm=T)-1500000 
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
    ylab("Total Valor Económico Pensión\n")  + xlab("")  +
    theme(axis.text.y=element_text(size=15 , angle=90),
          axis.title.y=element_text(size=30),
          axis.text.x=element_text(size=15, angle=90),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(colour = "Grey30", linetype = "dashed"))+
    geom_text(aes(label = paste0("$",point(val_pesos_pension)) , angle=90, size = 6, vjust = 0.4, hjust= -0.1)) +
    geom_text(aes(label = paste("Opción",point(opcion), ":") ), size=5 , angle=90, vjust = 0.4, hjust= 1) +
    coord_cartesian(ylim=c(min,max))  #coord_flip() +

return(ggsave(paste0(git, "Treat4", QID ,".png"), width=25, height = 30, units = "cm")) 

}


##################################
###### Generating treatments
##################################

# Manual treatment generation, for testing
QID<-"qualtricsID"


all.files$VPN<-all.files$val_pesos_pension*12*20

fcn.control("F", "nivel2", "1a", "co_1arp" )

fcn.treat1("F", "nivel2", "1a", "co_1arp" )

fcn.treat2("F", "nivel2", "1a", "co_1arp" )

fcn.treat3("F", "nivel2", "rp", "co_1arp" )

fcn.treat4("M", "nivel4", "2a", "co_2a3a" )

fcn.treat4("F", "nivel1", "3a", "co_3a3b" )


#########################
### Random asignment
#########################


# Simulation data that would come from Qualtrics

gender<-"F"
econ<-"nivel1"
mode1<- "rp"
mode2<- "1"
pg<-"b"

mode1pg<- if (grep("rp", mode1)) mode1 else paste0(mode1,pg)
mode2pg<- if (grep("rp", mode2)) mode2 else paste0(mode2,pg) 


pairvct<-c(mode1pg, mode2pg)
pairvct
pairvct<-sort(pairvct)
pairvct


pair<-paste0("co_", pairvct[1], pairvct[2])

QID<-"qualtricsID" # to be replaced by a real Qualtrics ID code, unique to each participant


vf<-c( fcn.control,  fcn.treat1,  fcn.treat2,  fcn.treat3,   fcn.treat4)

#### Random treatment assignment
selected<-sample(vf, 2, replace=FALSE)

#### Generating treatment images
print(selected[[1]](gender, econ, pairvct[1], pair))
print(selected[[2]](gender, econ, pairvct[2], pair))




