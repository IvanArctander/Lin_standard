setwd("C:/Users/Admin/Desktop/MUNI/nove drahy/analyzy")

library(Rcpp)
library(shiny)
library(tidyr)
library(dplyr)
library(reshape2)
library(xlsx)
library(mirt)
library(polycor)
library(GPArotation)
library(psych)
library(writexl)
library(readxl)
library("lavaan")
library("semTools")
library("semPlot")
library(ltm)
library(pwr)
library(pwrRasch)
library(ggplot2)

#export=read.csv("export.csv")

data=read_excel("Apilot.xlsx")

#warnings()

#data$d3_p20[is.na(data$d3_p20)]=0

#data$time[is.na(data$time)!=F]=25
#data=data[data$time<30,]
#data=data[,9:56]
#data$d3_p17=NULL

#data=data[data$exercise.session.schoolCode=="ZSEB",]

qplot(subset(data$time, data$time > 0), bins = 50)
boxplot(data$time)

table(data$exercise.session.schoolCode)
describe(data$time)

describeBy(data$total, data$exercise.session.schoolCode)



#as.data.frame(table(data[data$exercise.session.grade=="8",]$exercise.session.gender))[1,2]/
(as.data.frame(table(data[data$exercise.session.grade=="8",]$exercise.session.gender))[1,2]+
  as.data.frame(table(data[data$exercise.session.grade=="8",]$exercise.session.gender))[2,2])




describeBy(data$total, group= data$exercise.session.grade)

#easiness
table=cbind(
round(as.data.frame(percent_correct <- sapply(data[,9:56], mean, na.rm = T)),2),
round(as.data.frame(sapply(data[data$exercise.session.grade==1,9:56], mean, na.rm = T)),2),
round(as.data.frame(sapply(data[data$exercise.session.grade==2,9:56], mean, na.rm = T)),2),
round(as.data.frame(sapply(data[data$exercise.session.grade==3,9:56], mean, na.rm = T)),2),
round(as.data.frame(sapply(data[data$exercise.session.grade==4,9:56], mean, na.rm = T)),2),
round(as.data.frame(sapply(data[data$exercise.session.grade==5,9:56], mean, na.rm = T)),2),
round(as.data.frame(sapply(data[data$exercise.session.grade==6,9:56], mean, na.rm = T)),2),
round(as.data.frame(sapply(data[data$exercise.session.grade==7,9:56], mean, na.rm = T)),2),
round(as.data.frame(sapply(data[data$exercise.session.grade==8,9:56], mean, na.rm = T)),2)
)



table2=t(table)
describe(table2)$range

#schools
as.data.frame(table(data$exercise.session.schoolCode))

#biserial
biserial= round(biserial(x = data$total, y = data[,9:56]),2)


table=cbind(table,biserial)
names(table)=c("Celkem",1:8,"R celkem")


write.xlsx(table,"obtiznost.xlsx")




fa.parallel(data[,9:56])

write_xlsx(d, "desc2.xlsx")


d=aggregate(data$total, list(data$exercise.session.grade, data$exercise.session.schoolCode), FUN=mean)

#data=subset(data, select= -c(d_p24))


efa1 <- fa(data[,9:56], , cor="poly", nfactors = 1, fm="ml", rotate= "geominT")
print(efa1, sort=F)



B=tetrachoric(data[,9:56], na.rm=T)
write_xlsx(as.data.frame(B$rho),"poly.xlsx")



splithalf.r(data)

d=(efa1$residual)

efa1$communalities

fa.diagram(efa1)
fa.plot(efa1)

d=(efa1$loadings)
write.csv((d),"d.csv")
e=read.csv("d.csv")
write_xlsx(as.data.frame(e),"load.xlsx")

data=sapply(data, as.integer)

data=as.numeric(data)

#data=unlist(data)
data=as.data.frame(data)

irt= data


irt=irt[,c(9:56)]
#irt=na.omit(irt)

class(irt)

head(irt)
irt=as.matrix(irt)
is.numeric(irt)



model=mirt(irt, 1, itemtype = "Rasch",SE = T, method = "EM", technical= list(NCYCLES = 2000), guess= 0.25)

M2(model, impute = 10, ncpus = parallel::detectCores(), na.rm = T)

itemfit(model, na.rm=T)
itemfit(model, na.rm=T, fit_stats="infit")

irtC=na.omit(irt)
modelC=mirt(irtC, 1, itemtype = "Rasch",SE = T, method = "EM", technical= list(NCYCLES = 2000), guess= 0.25)

calibration=mirt::personfit(modelC, na.rm = T, method = "ML")
calibration$theta <- mirt::fscores(modelC,method = "ML")

dataC=data[is.na(data$d3_p20)==F,]
calibration$grade <- dataC$exercise.session.grade

histogram(calibration$outfit)
histogram(calibration$infit)

boxplot(calibration$infit)
boxplot(calibration$outfit)

# Check personfit by grade and theta
ggplot2::qplot(calibration$theta, calibration$infit, ylim = c(0,2), ylab = "Infit", xlab = "Schopnost") + ggthemes::theme_clean() + 
  ggplot2::theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))

ggplot2::qplot(calibration$theta, calibration$outfit, ylim = c(0,2), ylab = "Outfit", xlab = "Schopnost") + ggthemes::theme_clean() + 
  ggplot2::theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))

cor(calibration$infit, calibration$theta)
cor(calibration$outfit, calibration$theta)

ggplot2::qplot(x = calibration$infit, y = factor(calibration$grade), group = factor(calibration$grade), geom = "boxplot", xlim = c(0,2),
               xlab = "Infit", ylab = "Grade") + ggthemes::theme_clean() + ggplot2::theme(axis.text=element_text(size=12),
                                                  axis.title=element_text(size=14,face="bold"))

ggplot2::qplot(x = calibration$outfit, y = factor(calibration$grade), group = factor(calibration$grade), geom = "boxplot", xlim = c(0,2),
               xlab = "Outfit", ylab = "Grade") + ggthemes::theme_clean() + ggplot2::theme(axis.text=element_text(size=12),
                                                  axis.title=element_text(size=14,face="bold"))

nrow(calibration[calibration$infit > 2,])
nrow(calibration[calibration$outfit > 2,])

misfits = rownames(calibration[calibration$outfit>2 | calibration$infit>2,])

dataNM = data[!(rownames(data) %in% misfits),]
dataM = data[(rownames(data) %in% misfits),]

describe(dataNM[,2:8])
describe(dataM[,2:8])

coef(model, IRTpars = TRUE, simplify=T) 

plot(model, type='trace', facet_items=T, 
     as.table = TRUE, auto.key=list(points=F, lines=T, columns=4, space = 'top', cex = .8), 
     theta_lim = c(-3, 3), 
     main = "")


plot(model, type='infotrace', facet_items=T, 
     as.table = TRUE, auto.key=list(points=F, lines=T, columns=1, space = 'right', cex = .8), 
     theta_lim = c(-3, 3), 
     main="")

summary(model, IRTpars=T)

marginal_rxx(model)

plot(model, type = 'infoSE', theta_lim = c(-6, 6),  main="")

plot(model, type = 'rxx', theta_lim = c(-6, 6), main="" )

describe(fscores(model, scores.SE = T))

alpha(data)

library(eRm)
library(psych)
rasch=RM(irt)





empirical_plot(irt, which.items = c(1:48), smooth=T)

table(data$exercise.session.schoolCode)

describe(data$total)
scale(data$total)

table(data$exercise.session.grade)


modelCC=mirt(irtC, 1, itemtype = "Rasch",SE = T, method = "EM", technical= list(NCYCLES = 2000), guess= 0.25)

M2(modelCC, impute = 10, ncpus = parallel::detectCores(), na.rm = T)

final=dataNM[is.na(dataNM$d3_p20)==F,]

final$theta = mirt::fscores(modelCC, "ML")

describe(final$theta)



#check misfits again
calibration2=mirt::personfit(modelCC, na.rm = T, method = "ML")
calibration2$theta <- mirt::fscores(modelCC,method = "ML")


calibration2$grade <- final$exercise.session.grade

histogram(calibration2$outfit)
histogram(calibration2$infit)

boxplot(calibration2$infit)
boxplot(calibration2$outfit)


ggplot2::qplot(x = calibration2$outfit, y = factor(calibration2$grade), group = factor(calibration2$grade), geom = "boxplot", xlim = c(0,2),
               xlab = "Outfit", ylab = "RoD
nC-k") + ggthemes::theme_clean() + ggplot2::theme(axis.text=element_text(size=12),
                                                  axis.title=element_text(size=14,face="bold"))



residmat = mirt::M2(modelCC, impute = 10, ncpus = parallel::detectCores(), na.rm = T,
                     residmat = T, suppress = .15)

(g2_resid <- mirt::residuals(modelCC, type = "LDG2", suppress = .1))


xlsx::write.xlsx2(g2_resid, "raschiduals.xlsx")

# Itemfit



itemfit <- rasch_table(modelCC, boot = 1000, append.rmsea = F, print.plots = F,
                       rmsea_cut = .08, title = "", save = T)

round(mirt::itemfit(modelCC, fit_stats="infit")[,2:5],2)

itemfit=cbind(itemfit[,1:3],round(mirt::itemfit(modelCC, fit_stats="infit")[,2:5],2), round(mirt::itemfit(modelCC)[,4:5],2))

#xlsx::write.xlsx2(itemfit, "itempar.xlsx")

########################## Bez zl??ch polo??ek

zle_polozky <- c("d_p10", "d_p26","d2_p8", "d2_p11", "d2_p24", "d2_p26", "d3_p9", "d3_p11", "d3_p14", "d3_p17", "d3_p19")

library(dplyr)

finfin = dplyr::select(final, -all_of(zle_polozky))

modelF=mirt(finfin[,9:45], 1, itemtype = "Rasch",SE = T, method = "EM", technical= list(NCYCLES = 2000), guess= 0.25)

M2(modelF, impute = 10, ncpus = parallel::detectCores(), na.rm = T)
empirical_plot(finfin[,9:47], which.items = 1:39, smooth=T)

summary(model, IRTpars=T)

marginal_rxx(modelF)

plot(modelF, type = 'infoSE', theta_lim = c(-6, 6),  main="")

plot(modelF, type = 'rxx', theta_lim = c(-6, 6), main="" )

finT=round(mirt::fscores(modelF, method = "ML"),2)

names(finfin)[49]="theta"
finfin$theta=round(finfin$theta,2)
finfin=as.data.frame(finfin)

finT=cbind(finfin$theta,finT)
finT=as.data.frame(finT)

finT[finT=="Inf"]=NA
cor(finT, use="complete.obs")

mirt::itemfit(modelF)

g2_resid = mirt::residuals(modelF, type = "LDG2", df.p = FALSE, suppress = .1)
write.xlsx2(g2_resid,"g2.xlsx")




residmat = mirt::M2(modelF, impute = 10, ncpus = parallel::detectCores(), na.rm = T,
                    residmat = T)


rN=t(residmat)
residmat[is.na(residmat)==1]=0
rN[is.na(rN)==1]=0

rNN=residmat+rN
rNN[rNN==0]=1
rNN=round(rNN,2)

pca=pca(rNN, nfactors = 10, rotate="none")
pca
contrasts <- c(pca$values[1], pca$values[2], pca$values[3], pca$values[4], pca$values[5])
plot(contrasts, ylab = "Eigenvalues for Contrasts",
     xlab = "Contrast Number", 
     main = "Contrasts from PCA of Standardized Residual Correlations")

# PCA

rasch_erm <- eRm::RM(na.omit(finfin[,9:45]))

student.locations <- eRm::person.parameter(rasch_erm)
model.prob <- eRm::pmat(student.locations)
responses.without.extremes <- student.locations$X.ex
resids <- responses.without.extremes - model.prob


## Variance of the observations: VO
observations.vector <- as.vector(responses.without.extremes)
VO <- var(observations.vector, na.rm = T)

## Variance of the residuals: VR
residuals.vector <- as.vector(resids)
VR <- var(residuals.vector, na.rm = T)

## Raw variance explained by Rasch measures: (VO - VR)/VO
(VO - VR)/VO

###############3FAC

model3c <- "
F1 = 1 - 14
F2 = 15 - 28
F3 = 29 - 37
COV = F1*F2*F3
"
expmodel_3fac <- mirt::mirt(data = finfin[,9:45], model = model3c, itemtype = "Rasch", SE = T,
                            technical = list(NCYCLES = 2000), method = "MHRM")

mirt::summary(expmodel_3fac)
mirt::coef(expmodel_3fac, IRTpars = F, simplify = T)


fit_3fac <- mirt::M2(expmodel_3fac, impute = 10, ncpus = parallel::detectCores(), na.rm = T, QMC = F)
fit_3fac
anova=mirt::anova(modelF, expmodel_3fac)


############xxDIF WTF, I need to study this

#write.xlsx2(finfin, "finfin.xlsx")

finfin=read_excel("finfin.xlsx")
finfin=finfin[,2:47]
finfin=finfin[finfin$exercise.session.grade<8,]



#describe(items_only)


finfin=finfin[,-12]
items_only =  finfin[,9:45]

#finfin$exercise.session.gender=as.numeric(finfin$exercise.session.grade)
#is.numeric(finfin$exercise.session.grade)

#groups=finfin$exercise.session.grade
#table(groups)
#groups=as.factor(groups)

#finfin=finfin[order(finfin$exercise.session.grade, decreasing = T),]

R.Version()

#finfin=finfin[1:1000,]


psych::describeBy(items_only, group= finfin$exercise.session.grade)



groupmodel_restricted <- mirt::multipleGroup(data = items_only, model = 1, itemtype = 'Rasch',
                                             technical = list(NCYCLES = 40000, MAXQUAD = 160000), guess= 0.25, group = as.factor(finfin$exercise.session.grade),
                                             method = "EM", invariance = c(colnames(finfin[,9:44]),"free_mean","free_vars", "slopes"),
                                             dentype = "Gaussian")

nonIRT_dif_SIB_nunif <- difR::difSIBTEST(finfin[,9:45], group = factor(finfin$exercise.session.gender), focal.name = "m",
                                         type = "nudif", purify = F, nrIter = 25, p.adjust = "BH")
#sex

psych::describeBy(finfin$d_p11, finfin$exercise.session.gender)
psych::describeBy(finfin$d_p19, finfin$exercise.session.gender)
psych::describeBy(finfin$d2_p7, finfin$exercise.session.gender)
psych::describeBy(finfin$d3_p13, finfin$exercise.session.gender)



mirt::coef(groupmodel_restricted, simplify = T)


group_fit <- mirt::M2(groupmodel_restricted, method="EM", na.rm= T, use_dentype_estimate = F,
                      zeroExtreme = T, technical = list(NCYCLES = 40000, MAXQUAD = 160000))



group_residmat <- mirt::M2(groupmodel_restricted, na.rm = T, residmat = T, suppress = .2)


(g2_resid_group <- mirt::residuals(groupmodel_restricted, type = "LDG2", suppress = .2))

groupmodel_unrestricted <-mirt::multipleGroup(data = items_only, model = 1, itemtype = 'Rasch',
                                              technical = list(NCYCLES = 40000, MAXQUAD = 160000), guess= 0.25, group = factor(finfin$exercise.session.grade), method = "EM",
                                              invariance = c("d2_p4", "d2_p10","free_means","free_var"), dentype = "Gaussian")


group_fit_unrestricted <-  mirt::M2(groupmodel_unrestricted, method="EM", na.rm= T, use_dentype_estimate = F,
                                    zeroExtreme = T, technical = list(NCYCLES = 40000, MAXQUAD = 160000), ncpus = parallel::detectCores())

mirt::anova(groupmodel_restricted, groupmodel_unrestricted)

p <- mirt::plot(groupmodel_unrestricted, type = "score")
p["main"] <- "Vztah ocekavaneho hrubeho skoru a latentni schopnosti"
p

mirt::plot(groupmodel_unrestricted, type = "info")
mirt::plot(groupmodel_unrestricted, type = "score")
mirt::plot(groupmodel_unrestricted, type = "trace")


(dif_group <- mirt::DIF(groupmodel_unrestricted, which.par = "d", scheme = "drop",
                        ncpus = parallel::detectCores()))


(dif_group <- mirt::DIF(groupmodel_unrestricted, which.par = "d", scheme = "add", plotdif = T))

#############Post-standard

clean=read_excel("finfin.xlsx")
clean=clean[,2:46]
clean=clean[clean$exercise.session.grade<8,]

rasch_clean <- mirt::mirt(data = clean[,9:45], model = 1, guess= 0.25, na.rm= T, itemtype = 'Rasch', SE = T, method = "EM",
                          technical = list(NCYCLES = 2000, MAXQUAD = 160000))
M2(rasch_clean)

clean$exercise.session.gender[clean$exercise.session.gender=="f"]=1
clean$exercise.session.gender[clean$exercise.session.gender=="m"]=0

clean$exercise.session.gender=as.numeric(clean$exercise.session.gender)

describeBy(clean[,1:2], group= clean$exercise.session.grade)

table(clean$exercise.session.gender)

# Split-half
clean$skore1 <- rowMeans(clean[,seq(from = 9, to = 45, by = 2)], na.rm = F)
clean$skore2 <- rowMeans(clean[,seq(from = 10, to = 45, by = 2)], na.rm = F)

total_splithalf <- 2*cor(clean$skore1, clean$skore2, use = "pairwise", method = "pearson")/
  (1+cor(clean$skore1, clean$skore2, use = "pairwise", method = "pearson"))

#IRT reliabilita
plot(rasch_clean, type = "rxx", MI=1000, CI=0.95,  theta_lim = c(-4, 4), main = "Reliabilita post-standardizacni verze")


mirt::marginal_rxx(mirt::mirt(clean[,9:45],
                  itemtype = "Rasch", model = 1))


mirt::empirical_rxx(mirt::fscores(mirt::mirt(clean[,9:45],
      itemtype = "Rasch", model = 1), full.scores.SE = T))

#Norms

SH=
clean %>%
group_by(clean$exercise.session.grade) %>%
summarize(SH = 2*cor(skore1, skore2, use = "pairwise", method = "pearson")/(1+cor(skore1, skore2, use = "pairwise", method = "pearson")))


group1 = mirt::multipleGroup(data = clean[,9:45], model = 1, itemtype = 'Rasch',
                                             technical = list(NCYCLES = 40000, MAXQUAD = 160000), guess= 0.25, group = as.factor(clean$exercise.session.grade),
                                             method = "EM", invariance = c(colnames(clean[,9:45]),"free_mean","free_vars", "slopes"),
                                             dentype = "Gaussian")

groupN = mirt::multipleGroup(data = clean[,9:45], model = 1, itemtype = 'Rasch',
                             technical = list(NCYCLES = 40000, MAXQUAD = 160000), guess= 0.0, group = as.factor(clean$exercise.session.grade),
                             method = "EM", invariance = c(colnames(clean[,9:45]),"free_mean","free_vars", "slopes"),
                             dentype = "Gaussian")

mirt::marginal_rxx(mirt::mirt(clean[clean$exercise.session.grade == 1,9:45], itemtype = "Rasch", model = 1))
mirt::marginal_rxx(mirt::mirt(clean[clean$exercise.session.grade == 2,9:45], itemtype = "Rasch", model = 1))
mirt::marginal_rxx(mirt::mirt(clean[clean$exercise.session.grade == 3,9:45], itemtype = "Rasch", model = 1))
mirt::marginal_rxx(mirt::mirt(clean[clean$exercise.session.grade == 4,9:45], itemtype = "Rasch", model = 1))
mirt::marginal_rxx(mirt::mirt(clean[clean$exercise.session.grade == 5,9:45], itemtype = "Rasch", model = 1))
mirt::marginal_rxx(mirt::mirt(clean[clean$exercise.session.grade == 6,c(9:11,13:45)], itemtype = "Rasch", model = 1))
mirt::marginal_rxx(mirt::mirt(clean[clean$exercise.session.grade == 7,c(9:11,13:45)], itemtype = "Rasch", model = 1))



mirt::empirical_rxx(mirt::fscores(mirt::mirt(clean[clean$exercise.session.grade == 1,9:45], itemtype = "Rasch", model = 1), full.scores.SE = T))
mirt::empirical_rxx(mirt::fscores(mirt::mirt(clean[clean$exercise.session.grade == 2,9:45], itemtype = "Rasch", model = 1), full.scores.SE = T))
mirt::empirical_rxx(mirt::fscores(mirt::mirt(clean[clean$exercise.session.grade == 3,9:45], itemtype = "Rasch", model = 1), full.scores.SE = T))
mirt::empirical_rxx(mirt::fscores(mirt::mirt(clean[clean$exercise.session.grade == 4,9:45], itemtype = "Rasch", model = 1), full.scores.SE = T))
mirt::empirical_rxx(mirt::fscores(mirt::mirt(clean[clean$exercise.session.grade == 5,9:45], itemtype = "Rasch", model = 1), full.scores.SE = T))
mirt::empirical_rxx(mirt::fscores(mirt::mirt(clean[clean$exercise.session.grade == 6,c(9:11,13:45)], itemtype = "Rasch", model = 1), full.scores.SE = T))
mirt::empirical_rxx(mirt::fscores(mirt::mirt(clean[clean$exercise.session.grade == 7,c(9:11,13:45)], itemtype = "Rasch", model = 1), full.scores.SE = T))


clean$F1 <- mirt::fscores(group1, full.scores.SE = T)[,1]
clean$F1_SE <- mirt::fscores(group1, full.scores.SE = T)[,2]

clean$F1ml <- mirt::fscores(group1, full.scores.SE = T, method = "ML")[,1]
clean$F1_SEml <- mirt::fscores(group1, full.scores.SE = T, method = "ML")[,2]

clean$F1map <- mirt::fscores(group1, full.scores.SE = T, method = "MAP")[,1]
clean$F1_SEmap <- mirt::fscores(group1, full.scores.SE = T, method = "MAP")[,2]

clean$F1pl <- mirt::fscores(group1, full.scores.SE = T, method = "plausible")[,1]
clean$F1_SEpl <- mirt::fscores(group1, full.scores.SE = T, method = "plausible")[,2]

clean$F1wl <- mirt::fscores(group1, full.scores.SE = T, method = "WLE")[,1]
clean$F1_SEwl <- mirt::fscores(group1, full.scores.SE = T, method = "WLE")[,2]



coef(groupN, simplify = T, IRTpars = T)
describeBy(clean$F1, group= clean$exercise.session.grade)


write.xlsx(clean, "f_five.xlsx")

groupvector = coef(groupmodel_restricted, as.data.frame = T, IRTpars = T)[[1]]
groupvector = as.data.frame(groupvector[which(grepl("Group",rownames(groupvector))),1])
groupvector[2,1] = sqrt(groupvector[2,1])
groupvector
groupvector = coef(groupmodel_restricted, as.data.frame = T, IRTpars = T)[[2]]
groupvector = as.data.frame(groupvector[which(grepl("Group",rownames(groupvector))),1])
groupvector[2,1] = sqrt(groupvector[2,1])
groupvector
groupvector = coef(groupmodel_restricted, as.data.frame = T, IRTpars = T)[[3]]
groupvector = as.data.frame(groupvector[which(grepl("Group",rownames(groupvector))),1])
groupvector[2,1] = sqrt(groupvector[2,1])
groupvector
groupvector = coef(groupmodel_restricted, as.data.frame = T, IRTpars = T)[[4]]
groupvector = as.data.frame(groupvector[which(grepl("Group",rownames(groupvector))),1])
groupvector[2,1] = sqrt(groupvector[2,1])
groupvector
groupvector = coef(groupmodel_restricted, as.data.frame = T, IRTpars = T)[[5]]
groupvector = as.data.frame(groupvector[which(grepl("Group",rownames(groupvector))),1])
groupvector[2,1] = sqrt(groupvector[2,1])
groupvector
groupvector = coef(groupmodel_restricted, as.data.frame = T, IRTpars = T)[[6]]
groupvector = as.data.frame(groupvector[which(grepl("Group",rownames(groupvector))),1])
groupvector[2,1] = sqrt(groupvector[2,1])
groupvector
groupvector = coef(groupmodel_restricted, as.data.frame = T, IRTpars = T)[[7]]
groupvector = as.data.frame(groupvector[which(grepl("Group",rownames(groupvector))),1])
groupvector[2,1] = sqrt(groupvector[2,1])
groupvector

##########################ferovost

clean$IQ=0
clean[clean$exercise.session.grade==1,]$IQ= (100 +((clean[clean$exercise.session.grade==1,]$F1)/1.06)*15)
clean[clean$exercise.session.grade==2,]$IQ= (100 +((clean[clean$exercise.session.grade==2,]$F1-0.38)/1.02)*15)
clean[clean$exercise.session.grade==3,]$IQ= (100 +((clean[clean$exercise.session.grade==3,]$F1-0.76)/1.04)*15)
clean[clean$exercise.session.grade==4,]$IQ= (100 +((clean[clean$exercise.session.grade==4,]$F1-1.32)/1.06)*15)
clean[clean$exercise.session.grade==5,]$IQ= (100 +((clean[clean$exercise.session.grade==5,]$F1-1.82)/0.95)*15)
clean[clean$exercise.session.grade==6,]$IQ= (100 +((clean[clean$exercise.session.grade==6,]$F1-2.39)/1.09)*15)
clean[clean$exercise.session.grade==7,]$IQ= (100 +((clean[clean$exercise.session.grade==7,]$F1-2.65)/0.96)*15)

describe(clean$IQ)

scatter.hist(clean$F1, clean$IQ)

#sex
t.test(clean$F1 ~ clean$exercise.session.gender)
lsr::cohensD(clean$F1 ~ clean$exercise.session.gender)

t.test(clean$IQ ~ clean$exercise.session.gender)
lsr::cohensD(clean$IQ ~ clean$exercise.session.gender)

t.test(clean[clean$exercise.session.grade==1,]$IQ ~ clean[clean$exercise.session.grade==1,]$exercise.session.gender)
t.test(clean[clean$exercise.session.grade==2,]$IQ ~ clean[clean$exercise.session.grade==2,]$exercise.session.gender)
t.test(clean[clean$exercise.session.grade==3,]$IQ ~ clean[clean$exercise.session.grade==3,]$exercise.session.gender)
t.test(clean[clean$exercise.session.grade==4,]$IQ ~ clean[clean$exercise.session.grade==4,]$exercise.session.gender)
t.test(clean[clean$exercise.session.grade==5,]$IQ ~ clean[clean$exercise.session.grade==5,]$exercise.session.gender)
t.test(clean[clean$exercise.session.grade==6,]$IQ ~ clean[clean$exercise.session.grade==6,]$exercise.session.gender)
t.test(clean[clean$exercise.session.grade==7,]$IQ ~ clean[clean$exercise.session.grade==7,]$exercise.session.gender)




describeBy(clean[clean$exercise.session.grade==1,]$F1, clean[clean$exercise.session.grade==1,]$exercise.session.gender)
describeBy(clean[clean$exercise.session.grade==2,]$F1, clean[clean$exercise.session.grade==2,]$exercise.session.gender)
describeBy(clean[clean$exercise.session.grade==3,]$F1, clean[clean$exercise.session.grade==3,]$exercise.session.gender)
describeBy(clean[clean$exercise.session.grade==4,]$F1, clean[clean$exercise.session.grade==4,]$exercise.session.gender)
describeBy(clean[clean$exercise.session.grade==5,]$F1, clean[clean$exercise.session.grade==5,]$exercise.session.gender)
describeBy(clean[clean$exercise.session.grade==6,]$F1, clean[clean$exercise.session.grade==6,]$exercise.session.gender)
describeBy(clean[clean$exercise.session.grade==7,]$F1, clean[clean$exercise.session.grade==7,]$exercise.session.gender)

describeBy(clean[clean$exercise.session.grade==1,]$IQ, clean[clean$exercise.session.grade==1,]$exercise.session.gender)
describeBy(clean[clean$exercise.session.grade==2,]$IQ, clean[clean$exercise.session.grade==2,]$exercise.session.gender)
describeBy(clean[clean$exercise.session.grade==3,]$IQ, clean[clean$exercise.session.grade==3,]$exercise.session.gender)
describeBy(clean[clean$exercise.session.grade==4,]$IQ, clean[clean$exercise.session.grade==4,]$exercise.session.gender)
describeBy(clean[clean$exercise.session.grade==5,]$IQ, clean[clean$exercise.session.grade==5,]$exercise.session.gender)
describeBy(clean[clean$exercise.session.grade==6,]$IQ, clean[clean$exercise.session.grade==6,]$exercise.session.gender)
describeBy(clean[clean$exercise.session.grade==7,]$IQ, clean[clean$exercise.session.grade==7,]$exercise.session.gender)


#groupsex

sex_restricted <- mirt::multipleGroup(data = items_only, model = 1, itemtype = 'Rasch',
                                             technical = list(NCYCLES = 40000, MAXQUAD = 160000), guess= 0.25, group = as.factor(finfin$exercise.session.gender),
                                             method = "EM", invariance = c(colnames(finfin[,9:45]),"free_mean","free_vars", "slopes"),
                                             dentype = "Gaussian")

coef(sex_restricted)

sex_fit <- mirt::M2(sex_restricted, method="EM", na.rm= T, use_dentype_estimate = F,
                      zeroExtreme = T, technical = list(NCYCLES = 40000, MAXQUAD = 160000))

sex_unrestricted <-mirt::multipleGroup(data = items_only, model = 1, itemtype = 'Rasch',
                                              technical = list(NCYCLES = 40000, MAXQUAD = 160000), guess= 0.25, group = factor(finfin$exercise.session.gender), method = "EM",
                                              invariance = c("d2_p4", "d2_p10","free_means","free_var"), dentype = "Gaussian")

coef(sex_unrestricted)

sex_fit_unrestricted <-  mirt::M2(sex_unrestricted, method="EM", na.rm= T, use_dentype_estimate = F,
                                    zeroExtreme = T, technical = list(NCYCLES = 40000, MAXQUAD = 160000), ncpus = parallel::detectCores())

mirt::anova(sex_restricted, sex_unrestricted)

p <- mirt::plot(sex_unrestricted, type = "score")
p["main"] <- "Vztah ocekavaneho hrubeho skoru a latentni schopnosti"
p

mirt::plot(sex_unrestricted, type = "info")
mirt::plot(sex_unrestricted, type = "score")
mirt::plot(sex_unrestricted, type = "trace")


plot=ggplot(clean, aes(IQ))
plot + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) + geom_density(alpha= 0.5, color=NA, aes(fill= as.factor(exercise.session.gender) ))+labs( x = "IQ", y="", fill= "gender")+scale_fill_manual(labels = c("M", "F"), values=c("blue","red"))


plot=ggplot(clean[clean$exercise.session.grade==1,], aes(IQ))
plot + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) + geom_density(alpha= 0.5, color=NA, aes(fill= as.factor(exercise.session.gender) ))+labs( x = "IQ", y="", fill= "gender")+scale_fill_manual(labels = c("M", "F"), values=c("blue","red"))

plot=ggplot(clean[clean$exercise.session.grade==2,], aes(IQ))
plot + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) + geom_density(alpha= 0.5, color=NA, aes(fill= as.factor(exercise.session.gender) ))+labs( x = "IQ", y="", fill= "gender")+scale_fill_manual(labels = c("M", "F"), values=c("blue","red"))

plot=ggplot(clean[clean$exercise.session.grade==3,], aes(IQ))
plot + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) + geom_density(alpha= 0.5, color=NA, aes(fill= as.factor(exercise.session.gender) ))+labs( x = "IQ", y="", fill= "gender")+scale_fill_manual(labels = c("M", "F"), values=c("blue","red"))

plot=ggplot(clean[clean$exercise.session.grade==4,], aes(IQ))
plot + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) + geom_density(alpha= 0.5, color=NA, aes(fill= as.factor(exercise.session.gender) ))+labs( x = "IQ", y="", fill= "gender")+scale_fill_manual(labels = c("M", "F"), values=c("blue","red"))

plot=ggplot(clean[clean$exercise.session.grade==5,], aes(IQ))
plot + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) + geom_density(alpha= 0.5, color=NA, aes(fill= as.factor(exercise.session.gender) ))+labs( x = "IQ", y="", fill= "gender")+scale_fill_manual(labels = c("M", "F"), values=c("blue","red"))

plot=ggplot(clean[clean$exercise.session.grade==6,], aes(IQ))
plot + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) + geom_density(alpha= 0.5, color=NA, aes(fill= as.factor(exercise.session.gender) ))+labs( x = "IQ", y="", fill= "gender")+scale_fill_manual(labels = c("M", "F"), values=c("blue","red"))

plot=ggplot(clean[clean$exercise.session.grade==7,], aes(IQ))
plot + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) + geom_density(alpha= 0.5, color=NA, aes(fill= as.factor(exercise.session.gender) ))+labs( x = "IQ", y="", fill= "gender")+scale_fill_manual(labels = c("M", "F"), values=c("blue","red"))

bright=clean[clean$IQ>=125,]
table(bright$exercise.session.gender)



#mesto
schools=as.data.frame(table(clean$exercise.session.schoolCode))
write.xlsx(schools, "schools.xlsx")

schools=as.data.frame(read_excel("schoolsA.xlsx", col_names=F))
clean$mesto=0
clean$mesto=as.character(clean$mesto)

clean[clean$exercise.session.schoolCode=="ZSDT",]$mesto= "a"
clean[clean$exercise.session.schoolCode=="ZSHE",]$mesto= "a"
clean[clean$exercise.session.schoolCode=="ZSMA",]$mesto= "a"
clean[clean$exercise.session.schoolCode=="Z?? Brat??ice",]$mesto= "a"
clean[clean$exercise.session.schoolCode=="zsmiroslav",]$mesto= "a"
clean[clean$exercise.session.schoolCode=="z?? a m?? ??ista",]$mesto= "a"
clean[clean$exercise.session.schoolCode=="ZSPEC",]$mesto= "b"
clean[clean$exercise.session.schoolCode=="krcin",]$mesto= "b"
clean[clean$exercise.session.schoolCode=="smiskova",]$mesto= "b"
clean[clean$exercise.session.schoolCode=="GHVLOUNY",]$mesto= "c"
clean[clean$exercise.session.schoolCode=="RGZSOWPV",]$mesto= "c"
clean[clean$exercise.session.schoolCode=="ZSEB",]$mesto= "c"
clean[clean$exercise.session.schoolCode=="ZSHU",]$mesto= "c"
clean[clean$exercise.session.schoolCode=="ZSLI",]$mesto= "c"
clean[clean$exercise.session.schoolCode=="ZSNDP",]$mesto= "c"
clean[clean$exercise.session.schoolCode=="ZSOP",]$mesto= "c"
clean[clean$exercise.session.schoolCode=="ZSSV",]$mesto= "c"
clean[clean$exercise.session.schoolCode=="ZSZLS",]$mesto= "d"
clean[clean$exercise.session.schoolCode=="chalabalova",]$mesto= "d"
clean[clean$mesto=="0",]$mesto= "e"
clean[clean$exercise.session.schoolCode=="?",]$mesto= "0"

clean=clean[clean$mesto!=0,]

table(clean$mesto)

describeBy(clean$IQ, group=clean$mesto)
describe(clean$IQ)


plot=ggplot(clean, aes(IQ))
plot + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) + geom_density(alpha= 0.5, color=NA, aes(fill= as.factor(mesto) ))+labs( x = "IQ", y="")

#new f-scores

clean=read_excel("finfin.xlsx")
clean=clean[,2:46]
clean=clean[clean$exercise.session.grade<8,]


group1 = multipleGroup(data = clean[,9:45], model = 1, itemtype = 'Rasch',
                             technical = list(NCYCLES = 40000, MAXQUAD = 160000), guess= 0.25, group = as.factor(clean$exercise.session.grade),
                             method = "EM", invariance = c(colnames(clean[,9:45]),"free_mean","free_vars", "slopes"),)

rasch_clean <- mirt::mirt(data = clean[,9:45], model = 1, guess= 0.25, na.rm= T, itemtype = 'Rasch', SE = T, method = "EM",
                          technical = list(NCYCLES = 2000, MAXQUAD = 160000))
                             

coef(rasch_clean, simplify=T, IRTpars=T)

reb=read_excel("reb_data.xlsx")
it=names(clean[,9:45])
reb_items=reb[,it]


calib <- group1
vals <- mod2values(calib)
vals$est <- FALSE
vals=vals[1:150,]
vals$group="all"

newdat <- reb_items

customK=extract.mirt(calib, 'K')

newmod <- mirt(newdat, 1, pars=vals, guess = 0.25, technical = list(customK=customK))

coef(calib, simplify=T, IRTpars=T)
coef(newmod, simplify=T, IRTpars=T)
            
f=fscores(newmod, full.scores = T, method = "ML")
sum=rowSums(reb_items)

newdat=cbind(newdat,f)
write.xlsx(newdat, "newdat.xlsx")

reb=cbind(reb[,1:9],sum,f)
reb=reb[-93,]

write.xlsx(reb, "reb.xlsx")


reb=read_excel("reb.xlsx")
reb=reb[,2:12]

reb=reb[reb$COG2!=0,]

names(reb)=c("name", "sex", "vek", "nar", "rocnik", "kod", "cog2", "cog7", "cog14", "sum", "F1")

reb[reb$rocnik==4,]$F1= (100 +((reb[reb$rocnik==4,]$F1-1.32)/1.06)*15)
reb[reb$rocnik==5,]$F1= (100 +((reb[reb$rocnik==5,]$F1-1.82)/0.95)*15)

cor(reb[,7:11])          
corr.test(reb[,7:11])          
