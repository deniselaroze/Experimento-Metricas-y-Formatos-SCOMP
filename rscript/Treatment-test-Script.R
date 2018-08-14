






#############################
### Tests Block Randomization
#############################

#https://cran.r-project.org/web/packages/blockrand/blockrand.pdf

block.id<-c("Fnivel1", "Fnivel2", "Fnivel3", "Fnivel4",
             "Mnivel1", "Mnivel2", "Mnivel3", "Mnivel4")

blockrand(200, num.levels = 5, levels = c("control", "treat1", "treat2", "treat3", "treat4"),
          block.id)













id<-"Fnivel4.rp.co_2arp"
mode<-"rp"


# Ejemplo  - Tabla cuando ID = 2
#################################
#data_sub <- subset(all.files, nid==2)
tbl<-all.files[all.files$id=="Fnivel4.rp.co_1brp", c("razon_social", "val_uf_pension", "riesgo")]
opcion <- seq.int(nrow(tbl))
tbl<-cbind(opcion, tbl)

tbl

names(tbl)<-c("Opción", "Razón Social", "Valor Pensión",  "Clasificación de Riesgo")
tbl<-xtable(tbl, caption="Leyenda del Control" )

save(htmlTable(tbl, file=paste0(git, "controlTest2",".html")))

print(tbl, type="HTML", file=paste0(git, "controlTest",".html"), include.rownames=F  )


##### Pruebas diferente tratamientos




fcn.control <- function(gender, econ, mode, pair){
  
  if (mode=="rp") {
    
    id<-paste0(gender, econ, ".", mode, ".", pair)
    
    tbl<-all.files[all.files$id==id, c("razon_social", "val_uf_pension", "VPN")]
    output <- numcolwise(prettyNum)(tbl, dec = ",")
    output$val_uf_pension<-paste(output$val_uf_pension, "UF")
    output<-cbind(tbl[,1], output[, 1])
    op<-t(output)
    
    return(
      htmlTable(op, cgroup = c("Monto de pension mensual durante el primer a&ntildeo"),
                n.cgroup = c(nrow(tbl)),
                header=paste("Opci&oacuten", 1:nrow(tbl)),
                caption="Retiro Programado",
                file=paste0(git, "control", QID ,".html"), 
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
    
    title<-if(grepl("1", mode)) {print("Renta Vitalicia Inmediata")
    } else if(grepl("2", mode))  {print("Retiro Programado con Renta Vitalicia Diferida de 2 a&ntilde;os")
    } else {print("Retiro Programado con Renta Vitalicia Diferida de 4 a&ntilde;os")}
    
    
    return(htmlTable(output,
                     header =  c("Opci&oacuten", "Raz&oacuten Social", "Pensi&oacuten mensual  en UF 
                                 <br> sin retiro de excedentes", "Clasificaci&oacuten de Riesgo <br>
                               &nbsp; de la Compa&ntilde;&iacutea de Seguros&lowast;"),
                     caption=title,
                     tfoot="&lowast; Las categor&iacuteas de Clasificaci&oacuten de Riesgo que permiten a las Compa&ntilde;&iacutea ofrecer
                   Rentas Vitalicias, ordenadas de mejor a inferior clasificaci&oacuten, son las siguientes AAA 
                   (mejor clasificaci&oacuten), AA, A, BBB (inferior). Cada una de estas categor&iacuteas puede tener 
                   sub&iacutendices &quot;+&quot; o &quot;-&quot;, siendo el sub&iacutendice &quot;+&quot; mejor que el &quot;-&quot;.",
                     file=paste0(git, "control", QID ,".html"),
                     css.cell = "padding-left: 2em; padding-right: 2em;",
                     rnames=F
    )
    )
  }
  
}




id<-"Fnivel1.3a.co_3arp"

tbl<-all.files[all.files$id==id, c("razon_social", "val_uf_pension", "riesgo", "VPN")]
opcion <- seq.int(nrow(tbl))
tbl<-cbind(opcion, tbl)

payment<-tbl

mn<-15-nrow(tbl)

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


library(httr)




POST("http://54.70.24.42:3838/cess/sp/?", body = list(age = 34, name = "Denise"), verbose())
POST("http://54.70.24.42:3838/cess/sp/", body = pay.list)





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

pay.op1<-fcn.payment1("F", "nivel2", "2b", "co_2brp" )









