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

data=read_excel("Ultimatni_data_Lin.xlsx", sheet=2)
warnings()

info=read_excel("Ultimatni_data_Lin.xlsx", sheet=1)

names(info)

#merge answer and info sheets
data= merge(data, info[,c("studentGrade", "student.id", "student.gender")], by = "student.id")

age= data.frame(info$student.id, info$student.birthdate, info$test.startTime)
names(age)= c("student.id", "birth","test_date")
age$age= difftime(age$test_date, age$birth, units = "days")/28
age$age=as.numeric(age$age) 

data= merge(data, age[,c("age", "student.id")], by = "student.id")

data= data[,c("student.id", "studentGrade", "student.gender",
              "testItem.id","testItem.name","correct", "time", "age")]

#drop trial items
data= data[!data$testItem.name %in% c("d_p1","d_p2", "d2_p1", "d3_p1"),]

#convert to wide
wide= reshape2::dcast(data, student.id + studentGrade + student.gender + age ~ testItem.name,
                      value.var = "correct", fun.aggregate = mean)

#write.xlsx(wide, "data_wide.xlsx")


#scoring based on norms (group1 from "irt drahy" script)
calib <- group1
vals <- mod2values(calib)
vals$est <- FALSE
vals=vals[1:150,]
vals$group="all"
customK=extract.mirt(calib, 'K')

st_score <- mirt(wide[5:41], 1, pars=vals, guess = 0.25, technical = list(customK=customK))

coef(calib, simplify=T, IRTpars=T)
coef(st_score, simplify=T, IRTpars=T)

f=fscores(st_score, full.scores = T, method = "WLE")

wide=cbind(wide,f)

#scoring based on new model
rasch_new = multipleGroup(data = wide[,5:41], model = 1, itemtype = 'Rasch',
                                    technical = list(NCYCLES = 40000, MAXQUAD = 160000), guess= 0.25, group = as.factor(wide$studentGrade),
                                    method = "EM", invariance = c(colnames(wide[,5:41]),"free_mean","free_vars", "slopes"),)

coef(rasch_new, simplify=T, IRTpars=T)
coef(st_score, simplify=T, IRTpars=T)

f2=fscores(rasch_new, full.scores = T, method = "WLE")
colnames(f2)="F2"

#compare obtained f scores
wide=cbind(wide,f2)

describe(wide)
wide[wide==Inf]=NA
cor(wide[,42:43], use= "pairwise.complete.obs")


#compare difficulties

nvals <- mod2values(rasch_new)
nvals$est <- FALSE
nvals=nvals[1:150,]
nvals$group="all"
ncustomK=extract.mirt(rasch_new, 'K')

new_score <- mirt(wide[5:41], 1, pars=nvals, guess = 0.25, technical = list(customK=ncustomK))


dif_old= data.frame(round(mirt::coef(st_score, IRTpars = T, simplify = T)$items,2))$b

dif_new= data.frame(round(mirt::coef(new_score, IRTpars = T, simplify = T)$items,2))$b

dif_change= as.data.frame(cbind(colnames(wide[,5:41]),dif_old, dif_new))

dif_change$dif_old=as.numeric(dif_change$dif_old)
dif_change$dif_new=as.numeric(dif_change$dif_new)

dif_change= dif_change[order(dif_change$dif_old),]
rownames(dif_change)= 1:37

ggplot(dif_change) +
  geom_segment( aes(x=as.numeric(rownames(dif_change)), xend=as.numeric(rownames(dif_change)), y=dif_old, yend=dif_new), color="grey") +
  geom_point( aes(x=as.numeric(rownames(dif_change)), y=dif_old), color="blue", size=3 ) +
  geom_point( aes(x=as.numeric(rownames(dif_change)), y=dif_new), color="red", size=3 ) +
  coord_flip() +
  xlab("") +
  ylab("Difficulty") +
  xlim(dif_change$V1)+
  ggtitle("Change in difficulties between versions")

dif_change$dif= dif_change$dif_old - dif_change$dif_new

table(wide$studentGrade)
table(clean$exercise.session.grade)

mean(wide$F1)
mean(wide$F2)

mean(wide$F2)/mean(wide$F1)


#scoring for export
##have the wide fresh without additions first



calib <- group1
vals <- mod2values(calib)
vals$est=F
vals=vals[!vals$group==7,]

st_score= multipleGroup(data = wide[,5:41], model = 1, pars=vals,
              technical = list(customK=customK), guess= 0.25, group = as.factor(wide$studentGrade),
              method = "EM")



lin_scores= fscores(extract.group(st_score, "2"), full.scores = T, method = "WLE", plausible.draws = 10,
                    plausible.type = "MH",  response.pattern = wide[,5:41])
 
wide$theta= fscores(st_score, full.scores = T, method = "WLE") 
  
wide=cbind(wide,do.call(cbind, lin_scores))
colnames(wide)[42:52]= c("theta", paste("PV_",1:10, sep=""))

wide$IQ=0
wide[wide$studentGrade==1,]$IQ= (100 +((wide[wide$studentGrade==1,]$theta)/1.06)*15)
wide[wide$studentGrade==2,]$IQ= (100 +((wide[wide$studentGrade==2,]$theta-0.38)/1.02)*15)
wide[wide$studentGrade==3,]$IQ= (100 +((wide[wide$studentGrade==3,]$theta-0.76)/1.04)*15)
wide[wide$studentGrade==4,]$IQ= (100 +((wide[wide$studentGrade==4,]$theta-1.32)/1.06)*15)
wide[wide$studentGrade==5,]$IQ= (100 +((wide[wide$studentGrade==5,]$theta-1.82)/0.95)*15)
wide[wide$studentGrade==6,]$IQ= (100 +((wide[wide$studentGrade==6,]$theta-2.39)/1.09)*15)


for (i in 1:10) {
  wide[[paste0("IQ_", i)]] = 0
}

grades <- c(1, 2, 3, 4, 5, 6)
offsets <- c(0, -0.38, -0.76, -1.32, -1.82, -2.39)
divisors <- c(1.06, 1.02, 1.04, 1.06, 0.95, 1.09)

for (grade in grades) {
  index <- wide$studentGrade == grade
  for (i in 1:10) {
    wide[index, paste0("IQ_", i)] <- 100 + (wide[index, paste0("PV_", i)] - offsets[grade]) / divisors[grade] * 15
  }
}

wide=as.data.frame(wide)
scored=wide[, c(1:4,42:63)]
names(scored)

write.xlsx(scored, "lin_2024_scored.xlsx")

#dif comparison with plain Rasch models

calib <- rasch_clean
vals <- mod2values(calib)
vals$est <- FALSE
vals=vals[1:150,]
vals$group="all"
customK=extract.mirt(calib, 'K')

st_score <- mirt(wide[5:41], 1, pars=vals, guess = 0.25, technical = list(customK=customK))

rasch_new <- mirt::mirt(data = wide[,5:41], model = 1, guess= 0.25, na.rm= T, itemtype = 'Rasch', SE = T, method = "EM",
                          technical = list(NCYCLES = 2000, MAXQUAD = 160000))


nvals <- mod2values(rasch_new)
nvals$est <- FALSE
nvals=nvals[1:150,]
nvals$group="all"
ncustomK=extract.mirt(rasch_new, 'K')

new_score <- mirt(wide[5:41], 1, pars=nvals, guess = 0.25, technical = list(customK=ncustomK))


dif_old= data.frame(round(mirt::coef(st_score, IRTpars = T, simplify = T)$items,2))$b

dif_new= data.frame(round(mirt::coef(new_score, IRTpars = T, simplify = T)$items,2))$b

dif_change= as.data.frame(cbind(colnames(wide[,5:41]),dif_old, dif_new))

dif_change$dif_old=as.numeric(dif_change$dif_old)
dif_change$dif_new=as.numeric(dif_change$dif_new)

dif_change= dif_change[order(dif_change$dif_old),]
rownames(dif_change)= 1:37

ggplot(dif_change) +
  geom_segment( aes(x=as.numeric(rownames(dif_change)), xend=as.numeric(rownames(dif_change)), y=dif_old, yend=dif_new), color="grey") +
  geom_point( aes(x=as.numeric(rownames(dif_change)), y=dif_old), color="blue", size=3 ) +
  geom_point( aes(x=as.numeric(rownames(dif_change)), y=dif_new), color="red", size=3 ) +
  coord_flip() +
  xlab("") +
  ylab("Difficulty") +
  xlim(dif_change$V1)+
  ggtitle("Change in difficulties between versions")

dif_change$dif= dif_change$dif_old - dif_change$dif_new

#compare fits
M2(st_score, na.rm=T)
M2(new_score, na.rm=T)
