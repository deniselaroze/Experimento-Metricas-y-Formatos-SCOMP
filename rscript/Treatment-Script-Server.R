################################################ 
### Data Management for Excel SP
### Jul 2018
### Denise Laroze / Mlopez
################################################

args <- commandArgs(TRUE)

#No incluir el llamado a librerias individuales, sino la carpeta donde estÃ¡n instaladas
.libPaths=("/usr/lib64/R/library/")

require(plyr)
require(ggplot2)
theme_set(theme_bw())
require(scales)
require(gridExtra)
require(xtable)
require(RColorBrewer)
require(htmlTable)
#library(gridBase)
require(grid)
require(forcats)
require(pacman)
pacman::p_load(ggplot2, extrafont, scales)
require(purrr, warn.conflicts = FALSE, quietly = TRUE)
require(magick,  warn.conflicts = FALSE, quietly = TRUE)
#require(OpenImageR)

#Amazon Server
setwd("/var/www/r.cess.cl/public_html/")

#Parameters
pesouf<-27205.11 ### 3 de agosto de 2018
path<-"/var/www/r.cess.cl/public_html/sp/"


load("/var/www/r.cess.cl/public_html/sp/nuevaBD.RData")



#if(length(args) != 5){
#  stop()
#}


###########################
## Function - Control
###########################
fcn.control <- function(gender, econ, mode, pair, v){
  
  if (mode=="rp") {
    
    id<-paste0(gender, econ, ".", mode, ".", pair)
    
    tbl<-all.files[all.files$id==id, c("razon_social", "val_uf_pension", "VPN")]
    output <- numcolwise(prettyNum)(tbl, dec = ",")
    output$val_uf_pension<-paste(output$val_uf_pension, "UF")
    output<-cbind(tbl[,1], output[, 1])
    op<-t(output)
    
    return(
      htmlTable(op, cgroup = c("Monto de pensi&oacute;n mensual durante el primer a&ntildeo"),
                n.cgroup = c(nrow(tbl)),
                header=paste("Opci&oacuten", 1:nrow(tbl)),
                caption="Retiro Programado",
                file=paste0(path, "TreatV", v , QID ,".html"), 
                css.cell = "padding-left: 0.5em; padding-right: 0.5em;",rnames=F
      )
    )
    
    
  }
  
  else {
    
    id<-paste0(gender, econ, ".", mode, ".", pair)
    tbl<-all.files[all.files$id==id, c("razon_social", "val_uf_pension", "riesgo", "VPN")]
    opcion <- seq.int(nrow(tbl))
    tbl<-cbind(opcion, tbl)
    output <- numcolwise(prettyNum)(tbl, dec = ",")
    output<-cbind(output[,1], tbl[,2], output[, 2], tbl[,4])
    
    title<-if(grepl("1", mode)) {"Renta Vitalicia Inmediata"
    } else if(grepl("2", mode))  {"Retiro Programado con Renta Vitalicia Diferida de 2 a&ntilde;os"
    } else {"Retiro Programado con Renta Vitalicia Diferida de 4 a&ntilde;os"}
    
    
    return(htmlTable(output,
                     header =  c("Opci&oacuten", "Raz&oacuten Social", "Pensi&oacuten mensual en UF", "Clasificaci&oacuten de Riesgo <br>
                                 &nbsp; de la Compa&ntilde;&iacutea de Seguros&lowast;"),
                     caption=title,
                     tfoot="&lowast; Las categor&iacuteas de Clasificaci&oacuten de Riesgo que permiten a las Compa&ntilde;&iacutea ofrecer
                     Rentas Vitalicias son las siguientes AAA (mejor clasificaci&oacuten), AA, A, BBB (inferior). Cada una de estas categor&iacuteas 
                     puede tener sub&iacutendices &quot;+&quot; o &quot;-&quot;, siendo el sub&iacutendice &quot;+&quot; mejor que el &quot;-&quot;.",
                     file=paste0(path, "TreatV", v , QID ,".html"), 
                     css.cell = "padding-left: 2em; padding-right: 2em;",
                     rnames=F
                     )
    )
  }
  
}

##########################
### Function - Treatment 1
##########################

fcn.treat1 <- function(gender, econ, mode, pair, v){
  
  if (mode=="rp") {
    id<-paste0(gender, econ, ".", mode, ".", pair)
    tbl<-all.files[all.files$id==id, c("razon_social", "val_pesos_pension", "VPN")]
    opcion <- seq.int(nrow(tbl))
    tbl<-cbind(opcion, tbl)
    tbl$val_pesos_pension<-round( tbl$val_pesos_pension, 0)
    #dipaths(tbl) <- c(0,0,0,0,0)
    output <- numcolwise(prettyNum)(tbl, big.mark = ".",
                                    decimal.mark = ",")
    output<-cbind(output[,1], tbl[,2], output[, 2])
    
    return(htmlTable(output,
                     header =  c("Opci&oacuten", "Raz&oacuten Social", "Pensi&oacuten mensual en pesos<br> durante el primer a&ntildeo&dagger;"),
                     caption=  "Retiro Programado ",
                     tfoot="&dagger; Valor calculado en base a UF del d&iacutea 03/08/2018",
                     file=paste0(path, "TreatV", v , QID ,".html"), 
                     rnames=F
    )   
    )
    
    
    
    
  }
  
  else {
    id<-paste0(gender, econ, ".", mode, ".", pair)
    tbl<-all.files[all.files$id==id, c("razon_social", "val_pesos_pension", "riesgo", "VPN")]
    opcion <- seq.int(nrow(tbl))
    tbl<-cbind(opcion, tbl)
    tbl$val_pesos_pension<-round( tbl$val_pesos_pension, 0)
    #digits(tbl) <- c(0,0,0,0,0)
    output <- numcolwise(prettyNum)(tbl, big.mark = ".",
                                    decimal.mark = ",")
    output<-cbind(output[,1], tbl[,2], output[, 2], tbl[,4])
    title<-if(grepl("1", mode)) {"Renta Vitalicia Inmediata"
    } else if(grepl("2", mode))  {"Retiro Programado con Renta Vitalicia Diferida de 2 a&ntilde;os"
    } else {"Retiro Programado con Renta Vitalicia Diferida de 4 a&ntilde;os"}
    
    return(htmlTable(output,
                     header =  c("Opci&oacuten", "Raz&oacuten Social", "Pensi&oacuten mensual en pesos&dagger;", "&nbsp; Clasificaci&oacuten de Riesgo <br>
                                 de la Compa&ntilde;&iacutea de Seguros&lowast;"),
                     caption=title,
                     tfoot="&dagger; Valor calculado en base a UF del d&iacutea 03/08/2018.
                     &lowast; Las categor&iacuteas de Clasificaci&oacuten de Riesgo que permiten a las Compa&ntilde;&iacutea ofrecer
                     Rentas Vitalicias son las siguientes AAA (mejor clasificaci&oacuten), AA, A, BBB (inferior). Cada una de estas categor&iacuteas 
                     puede tener sub&iacutendices &quot;+&quot; o &quot;-&quot;, siendo el sub&iacutendice &quot;+&quot; mejor que el &quot;-&quot;.",
                     file=paste0(path, "TreatV", v , QID ,".html"), 
                     rnames=F
                     )   
    )
    
    
    
    
    
  }
  
  
}

#########################
### Function - Treatment 2
##########################

fcn.treat2 <- function(gender, econ, mode, pair, v){
  
  if (mode=="rp") {
    
    
    id<-paste0(gender, econ, ".", mode, ".", pair)
    tbl<-all.files[all.files$id==id, c("razon_social", "val_pesos_pension", "VPN")]
    tbl$val_pesos_pension<-round( tbl$val_pesos_pension, 0)
    tbl$pesosDiff<- tbl$val_pesos_pension - max(tbl$val_pesos_pension)
    tbl$pesosDiff<-round( tbl$pesosDiff, 0)*12
    opcion <- seq.int(nrow(tbl))
    tbl<-cbind(opcion, tbl)
    
    output <- numcolwise(prettyNum)(tbl, big.mark = ".",
                                    decimal.mark = ",")
    output<-cbind(output[,1], tbl[,2], output[, c(2,4)])
    
    
    return(htmlTable(output,
                     header =  c("Opci&oacuten", "Raz&oacuten Social", "Pensi&oacuten mensual en pesos<br> durante el primer a&ntildeo&dagger;",
                                 "&emsp; P&eacuterdida anual&dagger;"),
                     caption="Retiro Programado",
                     tfoot="&dagger; Valor calculado en base a UF del d&iacutea 03/08/2018.
                     &lowast; Monto que dejar&iacutea de ganar el primero a&ntilde;o de pensi&oacuten.",
                     file=paste0(path, "TreatV", v , QID ,".html"), 
                     rnames=F
    )
    )   
    
    
    
    
    
    
  }
  else {
    
    
    id<-paste0(gender, econ, ".", mode, ".", pair)
    tbl<-all.files[all.files$id==id, c("razon_social", "val_pesos_pension", "riesgo", "VPN")]
    tbl$val_pesos_pension<-round( tbl$val_pesos_pension, 0)
    tbl$pesosDiff<- tbl$val_pesos_pension - max(tbl$val_pesos_pension)
    tbl$pesosDiff<-round( tbl$pesosDiff, 0)*12
    opcion <- seq.int(nrow(tbl))
    tbl<-cbind(opcion, tbl)
    
    output <- numcolwise(prettyNum)(tbl, big.mark = ".",
                                    decimal.mark = ",")
    output<-cbind(output[,1], tbl[,2], output[, c(2,4)], tbl[,4])
    
    title<-if(grepl("1", mode)) {"Renta Vitalicia Inmediata"
    } else if(grepl("2", mode))  {"Retiro Programado con Renta Vitalicia Diferida de 2 a&ntilde;os"
    } else {"Retiro Programado con Renta Vitalicia Diferida de 4 a&ntilde;os"}
    
    return(htmlTable(output,
                     header =  c("Opci&oacuten", "Raz&oacuten Social", "Pensi&oacuten mensual en  pesos&dagger;", "P&eacuterdida anual&lowast;",
                                 "Clasificaci&oacuten de Riesgo <br> de la Compa&ntilde;&iacutea de Seguros&lowast;&lowast;"),
                     caption=title,
                     tfoot="&dagger; Valor calculado en base a UF del d&iacutea 03/08/2018.
                     &lowast; Las categor&iacuteas de Clasificaci&oacuten de Riesgo que permiten a las Compa&ntilde;&iacutea ofrecer
                     Rentas Vitalicias son las siguientes AAA (mejor clasificaci&oacuten), AA, A, BBB (inferior). Cada una de estas categor&iacuteas 
                     puede tener sub&iacutendices &quot;+&quot; o &quot;-&quot;, siendo el sub&iacutendice &quot;+&quot; mejor que el &quot;-&quot;.
                     &lowast;&lowast; Monto que dejar&iacutea de ganar cada a&ntilde;o de vida.",
                     file=paste0(path, "TreatV", v , QID ,".html"), 
                     css.cell = "padding-left: 0.5em; padding-right: 0.5em;",rnames=F
    )
    )   
  }
  
}

##########################
### Function - Treatment 3
##########################

fcn.treat3 <- function(gender, econ, mode, pair, v){
  
  if (mode=="rp") {
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
                     header =  c("Opci&oacuten", "Raz&oacuten Social", "Pensi&oacuten mensual en pesos<br> durante el primer a&ntildeo&dagger;",  
                                 "Valor total estimado a recibir <br>(largo plazo)&lowast;", "P&eacuterdida total <br> estimada&lowast;&lowast;"),
                     caption="Retiro Programado",
                     tfoot="&dagger; Valor calculado en base a UF del d&iacutea 03/08/2018.
                     &lowast; Estimaci&oacuten del valor total de la oferta de pensi&oacuten, considerando esperanza de vida,
                     riesgo de quiebra de la compa&ntilde;&iacutea de seguros y la tasa de descuento de los per&iacuteodos garantizados, si corresponde. 
                     &lowast;&lowast; Estimaci&oacuten del dinero que dejar&iacutea de percibir de no elegir la opci&oacuten 1.",
                     file=paste0(path, "TreatV", v , QID ,".html"), 
                     rnames=F
    )
    )   
    
  }
  
  else {
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
    
    title<-if(grepl("1", mode)) {"Renta Vitalicia Inmediata"
    } else if(grepl("2", mode))  {"Retiro Programado con Renta Vitalicia Diferida de 2 a&ntilde;os"
    } else {"Retiro Programado con Renta Vitalicia Diferida de 4 a&ntilde;os"}
    
    
    
    return(htmlTable(output,
                     header =  c("Opci&oacuten", "Raz&oacuten Social", "Pensi&oacuten mensual en pesos&dagger;",  
                                 "Valor total estimado a recibir <br>(largo plazo)&lowast;", "P&eacuterdida total <br> estimada&lowast;&lowast;"),
                     caption=title,
                     tfoot="&dagger; Valor calculado en base a UF del d&iacutea 03/08/2018.
                     &lowast; Estimaci&oacuten del valor total de la oferta de pensi&oacuten, considerando esperanza de vida,
                     riesgo de quiebra de la compa&ntilde;&iacutea de seguros y la tasa de descuento de los per&iacuteodos garantizados, si corresponde.
                     &lowast;&lowast; Estimaci&oacuten del dinero que dejar&iacutea de percibir de no elegir la opci&oacuten 1.",
                     file=paste0(path, "TreatV", v , QID ,".html"), 
                     rnames=F
    )
    )   
  }
}

#fcn.treat3("F", "nivel4", "rp", "co_2brp" )
#fcn.treat3("F", "nivel4", "1a", "co_1arp" )



##########################
### Function - Treatment 4 
##########################

#id<-"Fnivel4.1b.co_1brp"

fcn.treat4 <- function(gender, econ, mode, pair, v){
  id<-paste0(gender, econ, ".", mode, ".", pair)
  
  tbl<-all.files[all.files$id==id, c("razon_social", "val_pesos_pension", "VPN")]
  tbl$val_pesos_pension<-round( tbl$val_pesos_pension, 0)
  opcion <- seq.int(nrow(tbl))
  tbl<-cbind(opcion, tbl)
  
  n<-nrow(tbl)
  
  
  tbl$Company <- factor(tbl$razon_social, levels = tbl$razon_social[order(tbl$opcion)])
  max<-max(tbl$VPN, na.rm=T)+1500000
  min<-min(tbl$VPN, na.rm=T)-1500000 
  point <- format_format(big.mark = ".", decimal.mark = ",", scientific = FALSE)
  
  title<-if(grepl("1", mode)) {"Renta Vitalicia Inmediata"
  } else if(grepl("2", mode))  {"Retiro Programado con Renta Vitalicia Diferida de 2 años"
  } else if(grepl("3", mode)) {"Retiro Programado con Renta Vitalicia Diferida de 4 años"
  } else {"Retiro Programado"
  }
  
  y_labels <- purrr::map2(title, paste0("Valor total estimado a recibir (largo plazo)"), 
                          ~ bquote(atop(.(.x), scriptstyle(.(.y))))
  )
  y_labels <- purrr::invoke(expression, y_labels)
  
  
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
    ylab(y_labels)  + xlab("")  +
    theme(axis.text.y=element_text(size=15 , angle=90),
          axis.title.y=element_text(size=20),
          axis.text.x=element_text(size=15, angle=90),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(colour = "Grey60", linetype = "dashed"))+
    geom_text(aes(label = paste0("$",point(val_pesos_pension)) , angle=90, size = 6, vjust = 0.4, hjust= -0.1)) +
    geom_text(aes(label = paste("Opción", tbl$opcion, ":") ), size=5 , angle=90, vjust = 0.4, hjust= 1) +
    coord_cartesian(ylim=c(min,max))  #coord_flip() +



  return(ggsave(paste0(path, "TreatV", v, QID ,".png"), width=25, height = 30, units = "cm"))

  
}


#fcn.treat4("F", "nivel4", "2a", "co_2arp" )

##################################
###### Generating treatments
##################################

# Manual treatment generation, for testing
#QID<-"qualtricsID"

#fcn.control("F", "nivel2", "1a", "co_1arp" )

#fcn.treat1("F", "nivel2", "1a", "co_1arp" )

#fcn.treat2("F", "nivel2", "1a", "co_1arp" )

#fcn.treat3("F", "nivel2", "rp", "co_1arp" )

#fcn.treat4("M", "nivel4", "2a", "co_2a3a" )

#fcn.treat4("F", "nivel1", "3a", "co_3a3b" )


#########################
### Random asignment
#########################


# Simulation data that would come from Qualtrics
# mode1Q<-"1"
# mode2Q<-"3"
# gender<-"F"
# econ<-"nivel1"
#pg<-"b"

gender<-args[1]   ## GÃ©nero
econ<-args[2]    ## SES
mode1Q<-args[3] ## primera selecciÃ³n modalidad
mode2Q<-args[4] ## segunda selecciÃ³n modalidad
pg<-args[5]


### Adaptation from Qualtrics to R
mode1<-if(mode1Q=="1") {"rp"
} else if(mode1Q=="2")  {  
  "1"
} else if(mode1Q=="3")  {  
  "2"} else {"3"}

mode2<-if(mode2Q=="1") {"rp"
} else if(mode2Q=="2")  {  
  "1"
} else if(mode2Q =="3")  {  
  "2"} else {"3"}

mode1pg<- if (grepl("rp", mode1)) mode1 else paste0(mode1,pg)
mode2pg<- if (grepl("rp", mode2)) mode2 else paste0(mode2,pg) 



pairvct<-c(mode1pg, mode2pg)
#pairvct
pairvct<-sort(pairvct)
#pairvct

pair<-paste0("co_", pairvct[1], pairvct[2])

QID<-"qualtricsID" # to be replaced by a real Qualtrics ID code, unique to each participant
all.files$VPN<-all.files$val_pesos_pension*12*20

#### list of treatment functions
namedVF<-list(control=fcn.control, treat1=fcn.treat1, treat2=fcn.treat2, treat3=fcn.treat3, treat4=fcn.treat4  )
#namedVF<-list(control=fcn.treat4, treat1=fcn.treat4, treat2=fcn.treat4, treat3=fcn.treat4, treat4=fcn.treat4 )

#### Random selection of treatment without replacement
selected<-sample(namedVF, 2, replace=FALSE)
selectedQID<-names(selected) ## list of selected treatments to send to Qualtrics



### executing treatments
selected[[1]](gender, econ, pairvct[1], pair, v=1)
selected[[2]](gender, econ, pairvct[2], pair, v=2)

#selectedQID<-list("treat1", "treat4")
#if ("treat4" %in% selectedQID){
#  p2 <- image_read(paste0("TreatV", 1, QID ,".png"))
#  image_rotate(p2, 90) %>% image_write(paste0("TreatVRotate", 1, QID ,".png")) 
#} else {""}


#### Payment lists for treatments

fcn.payment <- function(gender, econ, mode, pair){
  id<-paste0(gender, econ, ".", mode, ".", pair)
  
  payment<-all.files[all.files$id==id, c("razon_social" ,"VPN")]
  
  mn<-15-nrow(payment)
  
  payment[nrow(payment)+mn,] <- NA
  
  payment$opcion<-1:15
  payment$VPN<-ifelse(is.na(payment$VPN), 0, payment$VPN )
  
  n <- 15
  payment$pay<- ifelse(payment$VPN==0, 0, 
                       ifelse(payment$VPN==max(payment$VPN, na.rm=T), 1500, 
                              ifelse(payment$VPN==sort(payment$VPN,partial=n-1)[n-1], 1250,
                                     ifelse(payment$VPN==sort(payment$VPN,partial=n-2)[n-2], 1000, 
                                            ifelse(payment$VPN==sort(payment$VPN,partial=n-3)[n-3], 750,
                                                   ifelse(payment$VPN==sort(payment$VPN,partial=n-4)[n-4], 500, 
                                                          ifelse(payment$VPN==sort(payment$VPN,partial=n-5)[n-5], 250, 
                                                                 ifelse(payment$VPN==sort(payment$VPN,partial=n-6)[n-6], 150, 0
                                                                 ) ) ) ) ) ) ) )
  
  
  row.names(payment)<-payment$opcion
  pay.list<-payment[, "pay"]
  pay.list <- as.list(as.data.frame(t(pay.list)))
  
  return(pay.list)
}

pay.op1<-fcn.payment(gender, econ, pairvct[1], pair)
pay.op2<-fcn.payment(gender, econ, pairvct[2], pair)


#envio de datos a qualtrics
to_qs<-c(pay.op1, pay.op2, selectedQID)
cat(sprintf("%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s", to_qs[1], to_qs[2], to_qs[3], to_qs[4], to_qs[5], to_qs[6], to_qs[7], to_qs[8], to_qs[9], to_qs[10], to_qs[11],to_qs[12], to_qs[13], to_qs[14], to_qs[15], to_qs[16], to_qs[17], to_qs[18], to_qs[19], to_qs[20], to_qs[21],to_qs[22], to_qs[23], to_qs[24], to_qs[25], to_qs[26], to_qs[27], to_qs[28], to_qs[29], to_qs[30], to_qs[31], to_qs[32]))
