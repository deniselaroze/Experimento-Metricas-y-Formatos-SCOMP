#########################################
##### Formatos y Métricas Scomp
##### code: Denise Laroze 
##### 2020 replication
##### Prepared for submition
#########################################


library(stargazer)
library(lattice)
library(dplyr)
library(Rmisc)
library(ggplot2)
library(nnet)

library(plm)
library(multiwayvcov)
library(lmtest)

library(BayesTree)
library(ggpubr)
theme_set(theme_bw())
library(rms)
library(xtable)
library(matrixStats)

rm(list=ls())

setwd("C:/Users/Denise Laroze/Dropbox/CESS-Santiago/archive/Pensions/Data Analysis - prelim")
#load("Data/clean_data.Rdata") # other subset of the data used for robustness checks. Substantive results are the same.  
load("Data/all_data.Rdata")
fig.path<-"Figures"
colors<-c("blue",  "purple", "darkgreen", "orange", "red")


####################
## Recode variables
####################



#Age



dfl$Age<-2019-dfl$Qbirth
df$Age<-2019-as.numeric(df$Qbirth)


dfl$age_cat<-ifelse (dfl$Age <= 60, "-60", ifelse(dfl$Age>65,"66+", "61-65"))
table(dfl$age_cat)
dfl$age_cat<- factor(dfl$age_cat, levels = c("-60", "61-65", "66+"))
table(dfl$age_cat)

# Programmed withdrawls 
#table(dfl$rp, dfl$mode)
t1$rp<-ifelse(t1$ob_mode=="rp", 1, 0) # rp is mode 1


dfl$max<-ifelse(dfl$ob_mode=="rp" & dfl$reward==1400, 1, ifelse(dfl$reward==1500, 1, 0)) # because of a coding mistake max for programmed withdrawls was 1400
#View(dfl[, c("ob_mode", "reward", "max" )])

#t1$max<-ifelse(t1$ob_mode=="rp" & t1$reward==1400, 1, ifelse(t1$reward==1500, 1, 0)) 



dfl$financial_lit_fact<-ifelse(dfl$financial_lit==0, "No Fin. Lit.",  
                                ifelse( dfl$financial_lit==1, "Low Fin. Lit.",
                                        ifelse( dfl$financial_lit==2, "Mid. Fin. Lit.","High Fin. Lit." )))


dfl$financial_lit_fact<- factor(dfl$financial_lit_fact, levels = c("No Fin. Lit.", "Low Fin. Lit.", "Mid. Fin. Lit.", "High Fin. Lit."))                   

#table(dfl$financial_lit, dfl$financial_lit_fact)

dfl$risk_fact<-ifelse(dfl$Qrisk1=="100% probabilidades de ganar $720", "Risk level 1", 
                       ifelse(dfl$Qrisk1=="50% probabilidades de ganar  $1.080 y 50% probabilidades de ganar $540", "Risk level 2",
                              ifelse(dfl$Qrisk1=="50% probabilidades de ganar $1.440 y 50% probabilidades de ganar  $360", "Risk level 3",
                                     ifelse(dfl$Qrisk1=="50%probabilidades de ganar  $1.800 y 50% probabilidades de ganar $180", "Risk level 4"
                              ,"Risk level 5"))))

                       




#############
## subsamples
#############

vars<-c("treat1", "treat2", "treat3", "treat4")
rp<-dfl[dfl$rp ==1 & dfl$treat %in% vars ,]
t1.4<-dfl[dfl$treat %in% vars ,]


vars<-c("treat1", "treat2", "treat3")
t1.3<-dfl[dfl$treat %in% vars ,]




dfl.control<-subset(dfl, treat=="control")
dfl.t1<-subset(dfl, treat=="treat1")
dfl.t2<-subset(dfl, treat=="treat2")
dfl.t3<-subset(dfl, treat=="treat3")
dfl.t4<-subset(dfl, treat=="treat4")


dfl.base<-rbind(dfl.control, dfl.t1)



#####################
## lm Models Table 4
####################
table(dfl$treat)

lm1<-lm(reward ~ treat, data = dfl)
lm1.cl<-coeftest(lm1, vcov=vcovHC(lm1,  cluster="uid"))  


lm2<-lm(reward ~ treat + Qbirth + Qgender +QSEC + risk_fact + financial_lit_fact , data = dfl)
lm2.cl<-coeftest(lm2, vcov=vcovHC(lm2, cluster="uid"))  


stargazer(lm1.cl, lm2.cl, out="Tables/regression.tex", type="latex",
          #title = "Regression Results", 
          out.header = F,model.names = F, covariate.labels = c("T.1", "T.2", "T.3", "T.4","Age", "Male", "SEC 2", "SEC 3", "SEC 4", "Risk level 2", "Risk level 3", "Risk level 4", "Risk level 5" ,"Low Fin. Lit", "Mid Fin. Lit.", "High Fin. Lit.", "Constant"), 
          dep.var.labels.include = F,
          add.lines = list(c("N", nobs(lm1), nobs(lm2) ),
                           c("Adjusted R$^{2}$ ",round(summary(lm1)$adj.r.squared, 3),round(summary(lm2)$adj.r.squared,3))
          ),
          dep.var.caption = "", star.cutoffs = c(0.05, 0.01, 0.001), notes.align = "l",
          label="tbl:treat_effects",  table.placement = "H",
          title = "OLS estimations on amount earned in offer selection with participant clustered standard errors.", no.space=TRUE)

########################################
## GLM Maximal offer selections Table 5
########################################

# Tot sample
set.seed(2365)
glm1<-lrm(max ~ treat , data=dfl, x=T, y=T)
glm1.cl<-bootcov(glm1,cluster=dfl$uid)

set.seed(2365)
glm4<-lrm(max ~ treat , data=t1.4, x=T, y=T)
glm4.cl<-bootcov(glm4,cluster=t1.4$uid)

# RP models

set.seed(2365)
glm2<-lrm(max ~ treat, data=rp, x=T, y=T)
glm2.cl<-bootcov(glm2,cluster=rp$uid)

# cited robustness tests
lm1<-lm(reward ~ treat, data = rp)
lm1.cl<-coeftest(lm1, vcov=vcovHC(lm1,  cluster="uid"))  

lm2<-lm(reward ~ treat + Qbirth + Qgender +QSEC + risk_fact + financial_lit_fact , data = rp)
lm2.cl<-coeftest(lm2, vcov=vcovHC(lm2, cluster="uid"))  

set.seed(2365)
glm3<-lrm(max ~ treat*rp, data=t1.3, x=T, y=T)
glm3.cl<-bootcov(glm3,cluster=t1.3$uid)


stargazer(lm1.cl, lm2.cl, glm3.cl)

###Print Table 5
stargazer(glm1.cl, glm4.cl, glm2.cl, out="Tables/glm_models.tex", type="latex",
          #title = "Regression Results", 
          out.header = F,model.names = F, covariate.labels = c("T.1", "T.2", "T.3", "T.4","Constant"), 
          dep.var.labels.include = F,
          column.labels = c("All", "T1-4", "PW T1-4"),
          #add.lines = list(c("N", nobs(glm4), nobs(glm3) )),
          dep.var.caption = "", star.cutoffs = c(0.05, 0.01, 0.001), notes.align = "l",
          label="tbl:glm_treat_effects",  table.placement = "H",
          title = "Logit estimations on maximal offer selection with participant bootstrap clustered standard errors. Model 1 includes all treatments in the sample. Model 2 includes only  and Models 2-3 include only observations for people who saw treatments 1--4.", 
          no.space=TRUE)



#############
#### Balance
##############



tbl<-ddply(dfl, .(Qgender, QSEC, treat) , summarise,
           #n.treat = length(unique(treat)),
           subj.n = length(unique(uid))
)
tbl
xt<-xtable(tbl)
print(xt, type="latex", file=("Tables/balance_numbers.tex"), floating=FALSE, include.rownames=FALSE)





b<-multinom(treat ~ factor(age_cat) + Qgender +QSEC + risk_fact + financial_lit_fact, data = dfl)
summary(b)

stargazer(b, out="Tables/balance.tex", type="latex",
          covariate.labels = c("Age:61-65", "66+", "Male", "SEC 2", "SEC 3", "SEC 4", "Risk level 2", "Risk level 3", "Risk level 4", "Risk level 5", "Low Fin. Lit", "Mid Fin. Lit.", "High Fin. Lit."), 
          dep.var.labels = c("T.1", "T.2", "T.3", "T.4"), # keep.stat=c("n", "ll"),
          dep.var.caption = "", star.cutoffs = c(0.05, 0.01, 0.001), notes.align = "l", table.placement = "H",
          label="tbl:balance",
          title = "Multinomial logit models on Treatment assignment by socio-demoraphic characteristics  - balance test", no.space=TRUE)

######################################
### Proportion clicked T2
######################################

dfc<-df[df$treatment1=="treat2", c("uid", "clickedV1", "clickedV2", "financial_lit")]


dfc$clicked<-ifelse(dfc$clickedV1>0, 1,  ifelse(dfc$clickedV2 >0,1, 0 ))



glm<-glm(clicked~ financial_lit , data=dfc, family = "binomial")
summary(glm)


table<-prop.table(table(dfc$clicked, dfc$financial_lit),2)

xtable(table)

#################
## Figures Long
################
#############################
## Hist-Treat - Fig. 1
#############################

#se crea una base llamada tratamiento solo con tratamiento y op
plot.df <- summarySE(dfl, measurevar="reward", groupvars=c("treat"), na.rm=T)

plot.df<- plot.df[!is.na(plot.df$treat),] # Se saca la primera fila ya que por defecto asigna a esta cuando la persona elige opcion pero no tratamiento


ggplot(plot.df, aes(x = factor(treat), y = reward, fill=factor(treat))) + 
  geom_bar(position = position_dodge(width=0.3), stat="identity") +
  scale_fill_manual("", values=colors)+
  geom_errorbar(aes(ymin=(reward-ci), ymax=(reward+ci)), width=.3)+
  theme(legend.position = "none")+
  ylab("Earnings (CLP)") + xlab("") +
  theme(axis.text.x = element_text( color="black", 
                                   size=10),
        axis.text.y = element_text( color="black", 
                                   size=10, angle=0))+
  scale_y_continuous(breaks = c(0,50,100,500,900,1200,1400,1500))+
  scale_x_discrete("",labels= c("control"="Control","treat1"="T. 1","treat2"="T. 2", "treat3"="T. 3","treat4"="T. 4"))


ggsave(paste0("efecto_tratamiento_dfl", ".png"), path=fig.path,  width = 7, height = 4)

###############################################
#### Max offer selection by treatment - Fig. 2
###############################################
#se crea una base llamada tratamiento solo con tratamiento y op
plot.df <- summarySE(dfl, measurevar="max", groupvars=c("treat"), na.rm=T)

plot.df<- plot.df[!is.na(plot.df$treat),] # Se saca la primera fila ya que por defecto asigna a esta cuando la persona elige opcion pero no tratamiento


ggplot(plot.df, aes(x = factor(treat), y = max, fill=factor(treat))) + 
  geom_bar(position = position_dodge(width=0.3), stat="identity") +
  scale_fill_manual("", values=colors)+
  geom_errorbar(aes(ymin=(max-ci), ymax=(max+ci)), width=.3)+
  theme(legend.position = "none")+
  ylab("Proportion that elects option with highest NPV") + xlab("") +
  theme(axis.text.x = element_text( color="black", 
                                    size=10),
        axis.text.y = element_text( color="black", 
                                    size=10, angle=0))+
  ylim(0,1) +
  #scale_y_continuous(breaks = c(0,1))+
  scale_x_discrete("",labels= c("control"="Control","treat1"="T. 1","treat2"="T. 2", "treat3"="T. 3","treat4"="T. 4"))


ggsave(paste0("max_offer_tratamiento_dfl", ".png"), path=fig.path,  width = 7, height = 4)

#############################
## Hist-Treat*gender
#############################

#se crea una base llamada tratamiento solo con tratamiento y op
plot.df <- summarySE(dfl, measurevar="reward", groupvars=c("treat", "Qgender"), na.rm=T)

plot.df<- plot.df[!is.na(plot.df$treat),] # Se saca la primera fila ya que por defecto asigna a esta cuando la persona elige opcion pero no tratamiento
plot.df$gender<-ifelse(plot.df$Qgender=="F", "Female", "Male")

ggplot(plot.df, aes(x = factor(treat), y = reward, fill=factor(treat))) + 
  geom_bar(position = position_dodge(width=0.3), stat="identity") +
  scale_fill_manual("", values=colors)+
  geom_errorbar(aes(ymin=(reward-ci), ymax=(reward+ci)), width=.3)+
  theme(legend.position = "none")+
  ylab("") + xlab("") +
  theme(axis.text.x = element_text( color="black", 
                                    size=10),
        axis.text.y = element_text( color="black", 
                                    size=10, angle=0))+
  scale_y_continuous(breaks = c(0,50,100,500,900,1200,1400,1500))+
  scale_x_discrete("",labels= c("control"="Control","treat1"="T. 1","treat2"="T. 2", "treat3"="T. 3","treat4"="T. 4"))+   
  facet_wrap(vars(gender))

ggsave(paste0("efecto_tratamiento_dfl_gender", ".png"), path=fig.path,  width = 7, height = 4)


#############################
## Hist-Treat*SEC
#############################

#se crea una base llamada tratamiento solo con tratamiento y op
plot.df <- summarySE(dfl, measurevar="reward", groupvars=c("treat", "QSEC"), na.rm=T)

plot.df<- plot.df[!is.na(plot.df$treat),] # 

ggplot(plot.df, aes(x = factor(treat), y = reward, fill=factor(treat))) + 
  geom_bar(position = position_dodge(width=0.3), stat="identity") +
  scale_fill_manual("", values=colors)+
  geom_errorbar(aes(ymin=(reward-1.96*se), ymax=(reward+1.96*se)), width=.3)+
  theme(legend.position = "none")+
  ylab("Earnings") + xlab("") +
  theme(axis.text.x = element_text( color="black", 
                                    size=10),
        axis.text.y = element_text( color="black", 
                                    size=10, angle=0))+
  scale_y_continuous(breaks = c(0,50,100,500,900,1200,1400,1500))+
  scale_x_discrete("",labels= c("control"="Control","treat1"="T. 1","treat2"="T. 2", "treat3"="T. 3","treat4"="T. 4"))+   
  facet_wrap(vars(QSEC))

ggsave(paste0("efecto_tratamiento_dfl_SEC", ".png"), path=fig.path,  width = 7, height = 4)



###################
#### Figures Wide
###################

### histogramas 


png(filename="Figures/Reward_dist.png", width = 480, height = 480, units = "px")
barplot(prop.table(table(dfl$reward)),col="grey40", ylim = c(0,0.6),ylab = "Freq.", main ="", xlab="Reward earned")

dev.off()

png(filename="Figures/Treatment_dist.png", width = 480, height = 480, units = "px")

barplot(prop.table(table(dfl$treat)),col="grey40", ylim = c(0,1),ylab = "Freq.", main = "", xlab="",
        names.arg=c("Control", "T.1", "T.2", "T.3", "T.4"))

dev.off()



#################
## Densidad edad
################


summary(dfl$Age)


plot.df<-dfl
plot.df<-plot.df[complete.cases(plot.df$treat),]

ggplot(plot.df, aes(x=Age, fill=treat)) + 
    geom_density(alpha=.5,  position="identity") +
  scale_fill_manual("", values=colors,
                    labels= c("control"="Control","treat1"="T. 1","treat2"="T. 2", "treat3"="T. 3","treat4"="T. 4"))

ggsave(paste0("Age_dist", ".png"), path=fig.path,  width = 7, height = 4)


############
## Dist genero 
############

### cambiar todos las casilleras vacías en NA

#Proportion gender
prop.table(table(df$Qgender))


# Figure
plot.df <- df[complete.cases(df[ , "Qgender"]),] 
plot.df$gender<-ifelse(plot.df$Qgender=="F", "Female", "Male")

png(filename="Figures/Gender_dist.png", width = 480, height = 480, units = "px")

barplot(prop.table(table(plot.df$gender)),col=c("blue", "red"), ylim = c(0,0.6),ylab = "Frecuencias relativas", main ="", xlab="Sexo")

dev.off()



############################
## Dist nivel soc economico
############################

prop.table(table(df$QSEC))


png(filename="Figures/SEC_dist.png", width = 480, height = 480, units = "px")

barplot(prop.table(table(df$QSEC)),col="grey40", ylim = c(0,1),ylab = "Relative frequencies", main ="", xlab="SEC",
        names.arg=c("SEC 1", "SEC 2", "SEc 3", "SEc 4"))

dev.off()


###########################
### Gender Heterogeneity
###########################
## Figure gender het - BART plot

set.seed(89)

# Data set up including calculating ability rank
df.b <- dfl

risk.vars<-c("control","treat1")
df.b$treat.het<-ifelse(df.b$treat %in% risk.vars, 0, 1)


#df.b$Gender <- ifelse(df.b$Qgender == "F",1,0)
df.b$treat <- as.factor(df.b$treat)
df.b$QSEC <- as.factor(df.b$QSEC)

# Define model variables incl. outcome as column 1
vars <- c("reward", "treat.het", "Qbirth", "Qgender", "QSEC",  "risk_fact","financial_lit" )

df.b <- df.b[,vars]
df.b <- df.b[complete.cases(df.b),]

# Separate outcome and training data
y <- df.b$reward
train <- df.b[,-1]

# Gen. test data where those treated become untreated, for use in calculating ITT
test <- train
test$treat.het <- ifelse(test$treat.het == 1,0,ifelse(test$treat.het == 0,1,NA))

# Run BART for predicted values of observed and synthetic observations
bart.out <- bart(x.train = train, y.train = y, x.test = test)

# Recover CATE estimates and format into dataframe
# Logic: Take predictions for those actually treated and minus counterfactual
#        Then take counterfactually treated and deduct prediction for those actually in control
CATE <- c(bart.out$yhat.train.mean[train$treat.het == 1] - bart.out$yhat.test.mean[test$treat.het == 0],
          bart.out$yhat.test.mean[test$treat.het == 1] - bart.out$yhat.train.mean[train$treat.het == 0])

CATE_df <- data.frame(CATE = CATE)
covars <- rbind(train[train$treat.het == 1,c(2:6)], test[test$treat.het==1,c(2:6)])

CATE_df <- cbind(CATE_df,covars)
CATE_df <- CATE_df[order(CATE_df$CATE),]
CATE_df$id <- c(1:length(CATE))

# Descriptive results reported in main text:
mean(CATE_df$CATE)
summary(CATE_df$CATE)

# Proportion of CATEs that are negative:
sum(CATE_df$CATE < 0)/nrow(CATE_df)
sum(CATE_df$CATE < mean(CATE_df$CATE))/nrow(CATE_df)

# Female participant: prop. below 497 (mean)
sum(CATE_df$CATE < mean(CATE_df$CATE) & CATE_df$Qgender =="F" )/sum(CATE_df$Qgender == "F")

# Male participant: prop. below 497 (mean)
sum(CATE_df$CATE < mean(CATE_df$CATE) & CATE_df$Qgender == "M" )/sum(CATE_df$Qgender =="M")


# CATE Heterogeneity plot
hist <- CATE_df

effectsPlot <- ggplot(hist, aes(x=id, y = CATE)) +
  geom_line() +
  geom_hline(yintercept= 0, linetype="dashed", color="red") +
  geom_hline(yintercept = mean(hist$CATE), color = "blue") +
  labs(x="Individual",y = "CATE") +
  theme_minimal() +
  scale_x_continuous(limits = c(0,nrow(train)))
#ggsave(effectsPlot, filename= "test.pdf")
# Mode histogram 

modePlot <- ggplot(hist, aes(x=id, fill=factor(Qgender))) +
  geom_histogram(binwidth = 60,position="stack") +
  theme(legend.position="bottom") +
  labs(y = "Count", x = "Individual")+
  scale_x_continuous(limits = c(0,nrow(train)))+
  scale_fill_discrete(name = "", labels = c("Female", "Male"))
#scale_fill_manual(name="Mode", values=colours) +
# +
#scale_x_continuous(limits = c(0,5220))

# Combine all plots into one chart
gender_het <- ggarrange(effectsPlot, modePlot,
                      ncol = 1, nrow = 2, heights = c(2,2))

ggsave(gender_het, filename = "gender_het1_alltreats.pdf", path=fig.path, device = "pdf", height = 8, width = 6, dpi = 300)




###########################
### SEC Heterogeneity
###########################
## Figure gender het - BART plot

set.seed(89)

# Data set up including calculating ability rank
df.b <- dfl

risk.vars<-c("control","treat1")
df.b$treat.het<-ifelse(df.b$treat %in% risk.vars, 0, 1)


df.b$Gender <- ifelse(df.b$Qgender == "F",1,0)
df.b$treat <- as.factor(df.b$treat)
df.b$QSEC <- as.factor(df.b$QSEC)

# Define model variables incl. outcome as column 1
vars <- c("reward", "treat.het", "Qbirth", "Qgender", "QSEC",  "risk_fact","financial_lit" )


df.b <- df.b[,vars]
df.b <- df.b[complete.cases(df.b),]

# Separate outcome and training data
y <- df.b$reward
train <- df.b[,-1]

# Gen. test data where those treated become untreated, for use in calculating ITT
test <- train
test$treat.het <- ifelse(test$treat.het == 1,0,ifelse(test$treat.het == 0,1,NA))

# Run BART for predicted values of observed and synthetic observations
bart.out <- bart(x.train = train, y.train = y, x.test = test)

# Recover CATE estimates and format into dataframe
# Logic: Take predictions for those actually treated and minus counterfactual
#        Then take counterfactually treated and deduct prediction for those actually in control
CATE <- c(bart.out$yhat.train.mean[train$treat.het == 1] - bart.out$yhat.test.mean[test$treat.het == 0],
          bart.out$yhat.test.mean[test$treat.het == 1] - bart.out$yhat.train.mean[train$treat.het == 0])

CATE_df <- data.frame(CATE = CATE)
covars <- rbind(train[train$treat.het == 1,c(2:6)], test[test$treat.het==1,c(2:6)])

CATE_df <- cbind(CATE_df,covars)
CATE_df <- CATE_df[order(CATE_df$CATE),]
CATE_df$id <- c(1:length(CATE))

# Descriptive results reported in main text:
mean(CATE_df$CATE)
summary(CATE_df$CATE)

# Proportion of CATEs that are negative:
sum(CATE_df$CATE < 0)/nrow(CATE_df)
sum(CATE_df$CATE < mean(CATE_df$CATE))/nrow(CATE_df)

# SEC level 1 participant: prop. below 497 (mean)
sum(CATE_df$CATE < mean(CATE_df$CATE) & CATE_df$QSEC == "nivel1" )/sum(CATE_df$QSEC == "nivel1")

# SEC level 2 participant: prop. below 497 (mean)
sum(CATE_df$CATE < mean(CATE_df$CATE) & CATE_df$QSEC == "nivel2" )/sum(CATE_df$QSEC == "nivel2")

# SEC level 3 participant: prop. below 497 (mean)
sum(CATE_df$CATE < mean(CATE_df$CATE) & CATE_df$QSEC == "nivel3" )/sum(CATE_df$QSEC == "nivel3")

# SEC level 4 participant: prop. below 497 (mean)
sum(CATE_df$CATE < mean(CATE_df$CATE) & CATE_df$QSEC == "nivel4" )/sum(CATE_df$QSEC == "nivel4")


# CATE Heterogeneity plot
hist <- CATE_df

effectsPlot <- ggplot(hist, aes(x=id, y = CATE)) +
  geom_line() +
  geom_hline(yintercept= 0, linetype="dashed", color="red") +
  geom_hline(yintercept = mean(hist$CATE), color = "blue") +
  labs(x="Individual",y = "CATE") +
  theme_minimal() +
  scale_x_continuous(limits = c(0,nrow(train)))
#ggsave(effectsPlot, filename= "test.pdf")
# Mode histogram 

modePlot <- ggplot(hist, aes(x=id, fill=QSEC)) +
  geom_histogram(binwidth = 60,position="stack") +
  theme(legend.position="bottom") +
  labs(y = "Count", x = "Individual")+
  scale_x_continuous(limits = c(0,nrow(train)))+
  scale_fill_discrete(name = "", labels = c("SEC 1", "SEC 2", "SEC 3", "SEC 4"))
#scale_fill_manual(name="Mode", values=colours) +


# Combine all plots into one chart
SEC_het <- ggarrange(effectsPlot, modePlot,
                        ncol = 1, nrow = 2, heights = c(2,2))

ggsave(SEC_het, filename = "SEC_het1_alltreats.pdf", path=fig.path, device = "pdf", height = 8, width = 6, dpi = 300)




####################################
### Financial literacy Heterogeneity
####################################
## Figure gender het - BART plot

set.seed(89)

# Data set up including calculating ability rank
df.b <- dfl

risk.vars<-c("control","treat1")
df.b$treat.het<-ifelse(df.b$treat %in% risk.vars, 0, 1)


df.b$Gender <- ifelse(df.b$Qgender == "F",1,0)
df.b$treat <- as.factor(df.b$treat)
df.b$QSEC <- as.factor(df.b$QSEC)

# Define model variables incl. outcome as column 1
vars <- c("reward", "treat.het", "Qbirth", "Qgender", "QSEC",  "risk_fact","financial_lit" )


df.b <- df.b[,vars]
df.b <- df.b[complete.cases(df.b),]

# Separate outcome and training data
y <- df.b$reward
train <- df.b[,-1]

# Gen. test data where those treated become untreated, for use in calculating ITT
test <- train
test$treat.het <- ifelse(test$treat.het == 1,0,ifelse(test$treat.het == 0,1,NA))

# Run BART for predicted values of observed and synthetic observations
bart.out <- bart(x.train = train, y.train = y, x.test = test)

# Recover CATE estimates and format into dataframe
# Logic: Take predictions for those actually treated and minus counterfactual
#        Then take counterfactually treated and deduct prediction for those actually in control
CATE <- c(bart.out$yhat.train.mean[train$treat.het == 1] - bart.out$yhat.test.mean[test$treat.het == 0],
          bart.out$yhat.test.mean[test$treat.het == 1] - bart.out$yhat.train.mean[train$treat.het == 0])

CATE_df <- data.frame(CATE = CATE)
covars <- rbind(train[train$treat.het == 1,c(2:6)], test[test$treat.het==1,c(2:6)])

CATE_df <- cbind(CATE_df,covars)
CATE_df <- CATE_df[order(CATE_df$CATE),]
CATE_df$id <- c(1:length(CATE))

# Descriptive results reported in main text:
mean(CATE_df$CATE)
summary(CATE_df$CATE)

# Proportion of CATEs that are negative:
sum(CATE_df$CATE < 0)/nrow(CATE_df)
sum(CATE_df$CATE < mean(CATE_df$CATE))/nrow(CATE_df)

# FL 0 participant: prop. below 497 (mean)
sum(CATE_df$CATE < mean(CATE_df$CATE) & CATE_df$financial_lit == 0 )/sum(CATE_df$financial_lit == 0)


# FL 1 participant: prop. below 497 (mean)
sum(CATE_df$CATE < mean(CATE_df$CATE) & CATE_df$financial_lit == 1 )/sum(CATE_df$financial_lit == 1)

# FL2 participant: prop. below 497 (mean)
sum(CATE_df$CATE < mean(CATE_df$CATE) & CATE_df$financial_lit == 2 )/sum(CATE_df$financial_lit == 2)

# FL 3 participant: prop. below 497 (mean)
sum(CATE_df$CATE < mean(CATE_df$CATE) & CATE_df$financial_lit == 3 )/sum(CATE_df$financial_lit == 3)

# SEC level 1 participant: prop. below 497 (mean)


# CATE Heterogeneity plot
hist <- CATE_df

effectsPlot <- ggplot(hist, aes(x=id, y = CATE)) +
  geom_line() +
  geom_hline(yintercept= 0, linetype="dashed", color="red") +
  geom_hline(yintercept = mean(hist$CATE), color = "blue") +
  labs(x="Individual",y = "CATE") +
  theme_minimal() +
  scale_x_continuous(limits = c(0,nrow(train)))
#ggsave(effectsPlot, filename= "test.pdf")
# Mode histogram 

modePlot <- ggplot(hist, aes(x=id, fill=factor(financial_lit))) +
  geom_histogram(binwidth = 60,position="stack") +
  theme(legend.position="bottom") +
  labs(y = "Count", x = "Individual")+
  scale_x_continuous(limits = c(0,nrow(train))) + 
  scale_fill_discrete(name = "", labels = c("No Fin. Lit.", "Low Fin. Lit", "Mid. Fin. Lit.", "High. Fin. Lit."))
#scale_fill_manual(name="Mode", values=colours) +
#modePlot

# Combine all plots into one chart
FL_het <- ggarrange(effectsPlot, modePlot,
                     ncol = 1, nrow = 2, heights = c(2,2))

ggsave(FL_het, filename = "FL_het1_alltreats.pdf", path=fig.path, device = "pdf", height = 8, width = 6, dpi = 300)





##############################################
#### What explains decisions in control and T1
##############################################


#### Covariates in treatment comparisons

glm1<-lrm(max ~ Qgender +QSEC + risk_fact + financial_lit_fact + var.offer.know + tot.offer.know, data = dfl.control, x=T, y=T)
glm1.cl<-bootcov(glm1,cluster=dfl.control$uid)

glm2<-lrm(max ~ Qgender +QSEC + risk_fact + financial_lit_fact + var.offer.know + tot.offer.know, data = dfl.t1, x=T, y=T)
glm2.cl<-bootcov(glm2,cluster=dfl.t1$uid)


glm3<-lrm(max ~ Qgender +QSEC + risk_fact + financial_lit_fact + var.offer.know + tot.offer.know, data = dfl.t2, x=T, y=T)
glm3.cl<-bootcov(glm3,cluster=dfl.t2$uid)

glm4<-lrm(max ~ Qgender +QSEC + risk_fact + financial_lit_fact + var.offer.know + tot.offer.know, data = dfl.t3, x=T, y=T)
glm4.cl<-bootcov(glm4,cluster=dfl.t3$uid)

glm5<-lrm(max ~ Qgender +QSEC + risk_fact + financial_lit_fact + var.offer.know + tot.offer.know, data = dfl.t4, x=T, y=T)
glm5.cl<-bootcov(glm5,cluster=dfl.t4$uid)

stargazer(glm1.cl, glm2.cl, glm3.cl, glm4.cl, glm5.cl,
          out.header = F,model.names = F,
          covariate.labels = c("Male", "SEC 2", "SEC 3", "SEC 4", "Risk level 2", "Risk level 3", "Risk level 4", "Risk level 5", "Low Fin. Lit", "Mid Fin. Lit.", "High Fin. Lit.",
                               "Provider knowledge (variance)", "Provider knowledge (Sum)"), 
          
          out="Tables/samples.tex", type="latex",
          dep.var.labels = c("Baseline", "Baseline", "Control", "T.1"),
          keep.stat=c("n", "ll"),
          # dep.var.caption =T,
          star.cutoffs = c(0.05, 0.01, 0.001), notes.align = "l",
          label="tbl:samples",  table.placement = "H",
          title = "Logit estimations of selecting the offer with the higest NPV and socio-demographic covariates", no.space=TRUE)




### Max glm
set.seed(2365)
glm1<-lrm(max ~ treat , data=dfl.base, x=T, y=T)
glm1.cl<-bootcov(glm1,cluster=dfl.base$uid)


glm2<-lrm(max ~ treat  + Qgender + QSEC + risk_fact + financial_lit_fact + var.offer.know+ tot.offer.know, data = dfl.base, x=T, y=T)
glm2.cl<-bootcov(glm2,cluster=dfl.base$uid)
glm2.cl

glm3<-lrm(max ~ Qgender +QSEC + risk_fact + financial_lit_fact + var.offer.know + tot.offer.know, data = dfl.control, x=T, y=T)
glm3.cl<-bootcov(glm3,cluster=dfl.control$uid)
glm3.cl


glm4<-lrm(max ~ Qgender+ var.offer.know + tot.offer.know, data = dfl.base, x=T, y=T)
glm4.cl<-bootcov(glm4,cluster=dfl.base$uid)
glm4.cl

#

stargazer(glm2.cl, glm3.cl, glm4.cl,
          out.header = F,model.names = F,
          covariate.labels = c("T.1","Male", "SEC 2", "SEC 3", "SEC 4", "Risk level 2", "Risk level 3", "Risk level 4", "Risk level 5", "Low Fin. Lit", "Mid Fin. Lit.", "High Fin. Lit.",
          "Provider knowledge (variance)", "Provider knowledge (Sum)"), 
          
          out="Tables/baseline.tex", type="latex",
          dep.var.labels = c("Baseline", "Baseline", "Control", "T.1"),
          keep.stat=c("n", "ll"),
          # dep.var.caption =T,
          star.cutoffs = c(0.05, 0.01, 0.001), notes.align = "l",
          label="tbl:baseline",  table.placement = "H",
          title = "Logit estimations of selecting the offer with the higest NPV and socio-demographic covariates", no.space=TRUE)



