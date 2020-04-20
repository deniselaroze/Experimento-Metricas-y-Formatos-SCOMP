#########################################
##### Formatos y Métricas Scomp
##### code: Denise Laroze y Nelson Brito
##### 2018-9
#########################################



library(naniar)
library(readr)
library(matrixStats)
library(BBmisc)
rm(list=ls())

setwd("C:/Users/Denise Laroze/Dropbox/CESS-Santiago/archive/Pensions/Data Analysis - prelim")
df1<-(read_csv("Data/SP_original_102019.csv")[-1:-2,])
df2<-(read_csv("Data/SP_FB_102019.csv")[-1:-2,])
df3<-(read_csv("Data/SP_unregistered_102019.csv")[-1:-2,])
ndf<-read.csv("Data/ruts_a_eliminar_5_07_19.csv")

######## Merge datasets

vars<-c(           
   "Duration (in seconds)", "ResponseId",         "LocationLatitude",      "LocationLongitude",    
   "DistributionChannel",   "UserLanguage"  ,        "Q250_First Click" ,     "Q250_Last Click" ,      "Q250_Page Submit",     
  "Q250_Click Count",      "Q255_Browser"   ,       "Q255_Version"      ,    "Q255_Operating System", "Q255_Resolution"  ,    
  "Qconsent"         ,     "Qgender"        ,       "Qbirth"            ,    "QSEC"         ,         "QEdu"            ,     
  "QPension"          ,    "Qpartner"       ,       "Qbirthpartner"     ,    "Qunderage"    ,         "Qmotherdied"     ,     
  "Qfatherdied"       ,    "Qadvice"        ,       "Qplanadvice"       ,    "Qplan"        ,         "Qplan_7_TEXT"    ,     
  "Qknowmodes_1"      ,    "Qknowmodes_2"   ,       "Qknowmodes_3"      ,    "QdifRVI_RP"   ,         "QknowAPF_1"      ,     
  "QknowAPF_2"        ,    "QknowAPF_3"     ,       "QknowAPF_4"        ,    "QknowAPF_5"   ,         "QknowAPF_6"      ,     
  "QknowCS_1"         ,    "QknowCS_2"      ,       "QknowCS_3"         ,    "QknowCS_4"    ,         "QknowCS_5"       ,     
  "QknowCS_6"         ,    "QknowCS_7"      ,       "QknowCS_8"         ,    "QknowCS_9"    ,         "QknowCS_10"      ,     
  "QknowCS_11"        ,    "QknowCS_12"     ,       "QknowCS_13"        ,    "QknowCS_14"   ,         "QknowCS_15"      ,     
  "Q200_First Click"  ,    "Q200_Last Click",       "Q200_Page Submit"  ,    "Q200_Click Count" ,     "Qreadingcomp1"   ,     
  "Qreadingcomp2"     ,    "Qreadingcomp3"  ,       "Q201_First Click"  ,    "Q201_Last Click"   ,    "Q201_Page Submit"  ,   
  "Q201_Click Count"  ,    "Qtime2_First Click" ,   "Qtime2_Last Click" ,    "Qtime2_Page Submit" ,   "Qtime2_Click Count" ,  
  "Qtime3_First Click",    "Qtime3_Last Click"  ,   "Qtime3_Page Submit",    "Qtime3_Click Count" ,   "Qtime4_First Click", 
  "Qtime4_Last Click" ,    "Qtime4_Page Submit" ,   "Qtime4_Click Count",   "Qtime5_First Click"  ,  "Qtime5_Last Click"  ,  
  "Qtime5_Page Submit",    "Qtime5_Click Count" ,   "Qtime6_First Click",    "Qtime6_Last Click"  ,   "Qtime6_Page Submit" ,  
  "Qtime6_Click Count",    "Qthink"         ,       "Qprofileselect"    ,    "Qprofileselect_6_TEXT", "Qmodeorder_1"  ,       
  "Qmodeorder_2"      ,    "Qmodeorder_3"   ,       "Qmodeorder_4"      ,    "QPG"              ,     "Qt1"         ,         
  "QtimeV1_First Click",   "QtimeV1_Last Click" ,   "QtimeV1_Page Submit",   "QtimeV1_Click Count",   "Qt2"         ,         
  "QtimeV2_First Click",   "QtimeV2_Last Click" ,   "QtimeV2_Page Submit",   "QtimeV2_Click Count",   "Qhealth_1"   ,         
  "Qhealth_2"         ,    "Qhealth_3"    ,         "Qhealth_4"     ,        "Qhealth_5"      ,       "Qhealth_6"     ,       
  "Qfuturo18500"      ,    "Qfuturo18500_2",        "Qfuturo15000"  ,        "Qfuturo13500"   ,       "Qfuturo12700" ,        
  "Qfuturo12400"      ,    "Qfuturo13100" ,         "Qfuturo14300"  ,        "Qfuturo14700"   ,       "Qfuturo13900"  ,       
  "Qfuturo16700"      ,    "Qfuturo15900" ,         "Qfuturo15500"  ,        "Qfuturo16300"   ,       "Qfuturo17600"  ,       
  "Qfuturo17100"      ,    "Qfuturo18000" ,         "Qfuturo22200"  ,        "Qfuturo24200"   ,       "Qfuturo23200"  ,       
  "Qfuturo23700"      ,    "Qfuturo22700" ,         "Qfuturo25200"  ,        "Qfuturo25800"   ,       "Qfuturo24700"  ,       
  "Qfuturo20300"      ,    "Qfuturo19400" ,         "Qfuturo18900"  ,        "Qfuturo19800"   ,       "Qfuturo21200"  ,       
  "Qfuturo20700"      ,    "Qfuturo21700" ,         "Qmath1"        ,        "Qmath2"         ,       "Qmath3"        ,       
  "Qmath4"            ,    "Qmath5"       ,         "Qrisk1"        ,        "Qrisk2"         ,       "Qrisk3_1"      ,       
  "Qrisk3_2"          ,    "Qrisk3_3"     ,         "Qrisk3_4"      ,        "Qrisk3_5"       ,       "Qrisk3_6"      ,       
  "Qrut"              ,        
  "Qtipeaccount"      ,    "Qnamebank"     ,        "Qnaccount1"    ,        "Qnaccount2"    ,        "SC0"       ,           
  "Potsize"           ,    "PotsizeK"     ,         "Drawdown"      ,        "tic"          ,       "Q_TotalDuration" ,     
  "genderQ"           ,    "econQ"        ,         "mode1Q"        ,        "mode2Q"        ,        "pgQ"     ,             
  "Treatment1"        ,    "Treatment2"   ,         "idQualtrics"   ,        "ADSversion"   ,         "UserAgent" ,           
  "genderBlock"       ,    "incomeBlock"  ,         "educationBlock" ,       "total_reward"  ,        "risk_q_reward",        
  "treat1_reward"     ,    "treat2_reward",         "treatment1"    ,        "treatment2"   ,         "clickedV1" ,           
  "clickedV2"         ,    "PC"         ,                           
  "clicked", "treatv1" ,              "treatv2"  ,   "largo1"  ,              "largo2"  
)

df1<-df1[, vars]
df2<-df2[, vars]
df3<-df3[, vars]

df<-rbind(df1, df2, df3)
rm(df1, df2, df3)

names(df)<-make.names(names(df),unique = TRUE)

df<-droplevels(df) ## Eliminate unused levels


##### Eliminate observations with missing treatment variables

df<-df[complete.cases(df$treatment1),]
summary(df$QPension) ## checking if there are any non-consenting observations



#### Eliminate duplicated observations/participations
df$rut<-gsub('-|\\.| |.{1}$','',df$Qrut)
df$rut<-as.numeric(df$rut) 
#View(df[is.na(df$rut),c("Qrut", "rut", "treatment1", "Qt1")]) To check if warnings are irrelevant
df<-df[!duplicated("rut"),] ## eliminate duplicated ruts ### Total observations, before cleaning. 


# Eliminating invalid observations, protocol violations
#df<-df[df$rut<10500000,] ## people that are too young.


# Eliminate ruts with doubtful origin
#ndf$rut_elim2<-gsub('-|\\.| |.{1}$','',ndf$rut_elim)
#elim<-as.numeric(ndf$rut_elim2)

#df<-df[!df$rut %in% elim,]


#### Identify time preference variable
# tmp<-df[, c(paste0("Q", 207:237), "Q239")]
# 
# tmp$timevalue<-NA
# for (i in 1:nrow(tmp)){
#   NonNAindex <- which(!is.na(tmp[i,]))
#   last <- max(NonNAindex)
#   tmp$timevalue[i]<-colnames(tmp)[last]
# }
# 
# df$timevalue<-tmp$timevalue




###################
### Recode 
######################
table(df$Qmath5)

### Math 5 Question, price of ball
df$Qmath5num<-parse_number(df$Qmath5)

#View(df[,c("Qmath5", "Qmath5num")])
#table(df$Qmath5num)
df$Qmath5_correct<-ifelse(df$Qmath5num == 5000,1, ifelse(df$Qmath5num==5, 1, 0))
#table(df$Qmath5num, df$Qmath5_correct)

### correct answers  for Qmath3 and Qmath4

#table(df$Qmath3)
df$Qmath3_correct<-ifelse(df$Qmath3=="Más de $125.000.000", 1, 0)
#table(df$Qmath3, df$Qmath3_correct)


#table(df$Qmath4)
df$Qmath4_correct<-ifelse(df$Qmath4=="Nunca se terminaría de pagar el crédito", 1, 0)
#table(df$Qmath4, df$Qmath4_correct)

tmp<-df[, c("Qmath3_correct", "Qmath4_correct", "Qmath5_correct") ]
tmp[is.na(tmp)] <- 0
tmp$financial_lit<-rowSums(tmp)

df$financial_lit<-tmp$financial_lit
rm(tmp)



#observed mode

df$mode.1<-ifelse(df$mode1Q==1, "rp", ifelse(
  df$mode1Q==2, 1, ifelse(
    df$mode1Q==3, 2, 3)))

#table(df$mode.1, df$mode1Q)


df$mode.2<-ifelse(df$mode2Q==1, "rp", ifelse(
  df$mode2Q==2, 1, ifelse(
    df$mode2Q==3, 2, 3)))

df$ob_mode.1<-NA
df$ob_mode.2<-NA

#table(df$mode.2, df$mode2Q)
for(i in 1 :nrow(df)){
  pairvct<-c(df$mode.1[i]  ,  df$mode.2[i])
  pairvct<-sort(pairvct)
  df$ob_mode.1[i]<-pairvct[1]
  df$ob_mode.2[i]<-pairvct[2]
  }

#View(df[,c("mode1Q", "mode2Q", "mode.1", "mode.2", "ob.mode.1", "ob.mode.2")])


#################
### Conocimiento AFP
n<-c(1:6)
AFP<-paste0( "QknowAPF_", n)
m<-df[,AFP]
m<-as.data.frame(m)
m <- data.frame(lapply(m, function(x) as.numeric(as.factor(x))))
df$totAFPknow<-rowSums(m[,AFP])
m2<-as.matrix(m)
df$AFPvar<-rowVars(m2)

### Conocimiento CS y AFP
n<-c(1:15)
CS<-paste0( "QknowCS_", n)

m<-df[,CS]
m<-as.data.frame(m)
m <- data.frame(lapply(m, function(x) as.numeric(as.factor(x))))
df$totCSknow<-rowSums(m[,CS])

m2<-as.matrix(m)
df$CSvar<-rowVars(m2)

#normalize
df$totAFPknow<-normalize(df$totAFPknow)
df$totCSknow<-normalize(df$totCSknow)



###########################################
### Reshape
###########################################


df$uid<-seq.int(nrow(df))
df2<-df[,c("uid", "Qgender", "Qbirth","QSEC","QEdu","Qt1","Qt2","treat1_reward","treat2_reward", "treatv1", "treatv2",
           "Qrisk1", "Qrisk2","Qrisk3_1", "Qrisk3_2","Qrisk3_3", "Qrisk3_4", "Qrisk3_5", "Qrisk3_6",
           "Qmath1","Qmath2","Qmath3","Qmath4", "Qmath5num",  "Qmath3_correct", "Qmath4_correct", "Qmath5_correct", "financial_lit",
           "ob_mode.1"  ,  "ob_mode.2", "AFPvar", "CSvar", "totAFPknow", "totCSknow")]
names(df2)<-c("uid", "Qgender", "Qbirth","QSEC","QEdu","op.1", "op.2","reward.1","reward.2","treat.1", "treat.2",
              "Qrisk1", "Qrisk2","Qrisk3_1", "Qrisk3_2","Qrisk3_3", "Qrisk3_4", "Qrisk3_5", "Qrisk3_6",
              "Qmath1","Qmath2","Qmath3","Qmath4", "Qmath5num",  "Qmath3_correct", "Qmath4_correct", "Qmath5_correct", "financial_lit",
              "ob_mode.1"  ,  "ob_mode.2", "AFPvar", "CSvar", "totAFPknow", "totCSknow")


df3<-df[,c("uid", "Qt1","Qt2","treat1_reward","treat2_reward", "treatv1", "treatv2", "ob_mode.1"  ,  "ob_mode.2")]
names(df3)<-c("uid", "op.1", "op.2","reward.1","reward.2","treat.1", "treat.2", "ob_mode.1"  ,  "ob_mode.2")

df3<-as.data.frame(df3)

myvar<-c("op.1", "op.2","reward.1","reward.2","treat.1", "treat.2", "ob_mode.1"  ,  "ob_mode.2")



dfl<-reshape(df3, varying=myvar, 
             direction="long", idvar="uid", sep=".")

dfl<-dfl[order(dfl$uid),]


### Merge in relevant covariates
idvar<-dput(names(df2))
idvar<-idvar[!idvar %in% myvar]  

df2<-df[, idvar]


dfl<-merge(dfl, df2, by="uid")

rm(df2, df3)


###############
### Recode dfl
##############

## Option as numeric
dfl$opn<-substring(dfl$op,8)
dfl$opn<-as.numeric(dfl$opn)
dfl$reward<-as.numeric(dfl$reward)
dfl$Qbirth<-as.numeric(dfl$Qbirth)

dfl$rp<-ifelse(dfl$ob_mode=="rp", 1, 0) # rp is mode 1


dfl$var.offer.know<-ifelse(dfl$rp==1, dfl$AFPvar, dfl$CSvar )
dfl$tot.offer.know<-ifelse(dfl$rp==1, dfl$totAFPknow, dfl$totCSknow )




##############
### Anonimise
################

# eliminate

elim<-c( "Qtipeaccount", "Qnamebank", "Qnaccount1", "Qnaccount2" ,
         "Duration..in.seconds.", "ResponseId", "LocationLatitude", "LocationLongitude", "DistributionChannel",   "UserLanguage",         
          "Q250_First.Click",   "Q250_Last.Click" ,  "Q250_Page.Submit", "Q250_Click.Count" , "Q255_Browser", "Q255_Version",         
          "Q255_Operating.System", "Q255_Resolution", "Qrut", "Qtipeaccount" , "Qnamebank", "Qnaccount1", "Qnaccount2"  ,"rut")


df <- df[ , !(names(df) %in% elim)]

###########################################
save(df, dfl, file = "Data/all_data.Rdata")

write.table(df, "Data/df_Expt_SCOMP_bd.csv", sep=",", row.names = F)
write.table(dfl, "Data/df_Expt_SCOMP_bd_largo.csv", sep=",", row.names = F)





