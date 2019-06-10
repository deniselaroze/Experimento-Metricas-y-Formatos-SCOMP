###########################
### Descriptive Statistics
### Denise Laroze
##########################

library(plyr)
library(xtable)

setwd("C:/Users/Denise Laroze P/Dropbox/CESS-Santiago/archive/Pensions/Pensions - JFF")

load("Design info/Certificados SP final/nuevaBDfinal.RData")

table(all.files$grupo)


### Mean pension payment by gender, income and mode
all.files$mode_type<-substring(all.files$perfil2, 1, 1)


mean.tbl<-ddply(all.files, .(grupo, mode_type), summarize,
      mean.uf = round(mean(val_uf_pension), 2),
      mean.peso = round(mean(val_pesos_pension), 0),
      mean.vpn= round(mean(VPN), 0) 
      )
      
tbl<-matrix(NA, nrow = 8, ncol = 5)

tbl[,1]<-unique(all.files$grupo)
tbl[,2]<-mean.tbl$mean.peso[mean.tbl$mode_type=="r"]
tbl[,3]<-mean.tbl$mean.peso[mean.tbl$mode_type=="1"]
tbl[,4]<-mean.tbl$mean.peso[mean.tbl$mode_type=="2"]
tbl[,5]<-mean.tbl$mean.peso[mean.tbl$mode_type=="3"]


colnames(tbl) <- c("Gender_income", "Draw-down", "Annuity" ,"Mixed", "Sequential")

xtable(prettyNum(tbl,big.mark=","))


### Mean pension payment by treatment



mean.tbl2<-ddply(all.files, .(grupo), summarize,
                mean.uf = round(mean(val_uf_pension), 2),
                mean.peso = round(mean(val_pesos_pension), 0),
                mean.vpn= round(mean(VPN), 0) 
)

mean.tbl2$Treatment2<-mean.tbl2$mean.peso
mean.tbl2$Treatment4<-mean.tbl2$mean.vpn

colnames(mean.tbl2) <- c("Gender Income", "Control", "Treatment 1" ,"Treatment 3", "Treatment 2", "Treatment 4")

mean.tbl2<-mean.tbl2[,c(1,2,3,5, 4, 6)]

mean.tbl2<-format(mean.tbl2, big.mark=",", small.mark = "." )


xtable(mean.tbl2)


