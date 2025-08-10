#setwd("C:/Users/Admin/Desktop/MUNI/LIN regular")

library(Rcpp)
library(shiny)
library(tidyr)
library(dplyr)
library(conflicted)
conflicted::conflict_prefer("select", "dplyr")
library(reshape2)
library("xlsx")
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
library(reshape2)

#can recall what this was about
#Sys.setlocale("LC_ALL", "en_US.UTF-8")

#get "grade" via xlookup first


data_long = readxl::read_excel("Ultimatni_data_Lin_all_anonymni_v4.xlsx", sheet = 2)
warnings()
names(data_long)

table(data_long$correct)


data_long <- data_long %>%
  mutate(correct = if_else(is.na(correct), 0, correct))



data = reshape2::dcast(data_long, student.id + grade ~ testItem.name, value.var = "correct", fun.aggregate = mean)


writexl::write_xlsx(data, "lin_items_0908.xlsx")




#calib <- group1
#vals <- mod2values(calib)
#vals$est <- FALSE
#vals=vals[1:150,]
#vals$group="all"




#customK=extract.mirt(calib, 'K')

#vals2 <- mod2values(newmod)
#vals=vals[1:146,]

#newmod <- mirt(newdat[,2:37], 1, pars=vals, guess = 0.25, technical = list(customK=customK))



#get rid of training d_p1, d_p2, d2_p1, d3_p1

data <- data %>% select(-d_p1, -d_p2, -d_p8, -d2_p1, -d3_p1)


  
  
group1 = multipleGroup(data = data[,3:38], model = 1, itemtype = 'Rasch',
                       technical = list(NCYCLES = 40000, MAXQUAD = 160000), group = as.factor(data$grade),
                       method = "EM", invariance = c(colnames(data[,3:38]),"free_mean","free_vars", "slopes"),)



mirt::coef(group1, simplify = T)




wle <- fscores(group1,method = "WLE", full.scores.SE= T)
#PVs <- as.data.frame(fscores(group1, plausible.draws = 10, plausible.type = "normal", method = "MAP"))


set.seed(666)

idx_by_grade <- split(seq_len(nrow(data)), data$grade)

pv_list <- vector("list", 6)
for (g in 1:6) {
  mod_g <- mirt::extract.group(group1, g)
  
  tmp <- as.data.frame(
    mirt::fscores(
      mod_g,
      method = "MAP",
      plausible.draws = 10,
      plausible.type = "MH",
      technical = list(NCYCLES = 5000)
    )
  )
  
  # Force consistent PV column names
  colnames(tmp) <- paste0("MAP_", seq_len(ncol(tmp)))
  
  pv_list[[g]] <- tmp
}

# Now row-bind without name mismatches
PVs <- do.call(rbind, pv_list)
PVs <- PVs[order(unlist(idx_by_grade)), , drop = FALSE]
row.names(PVs) <- NULL

out <- cbind(
  id  = data$student.id,
  grade = data$grade,
  WLE = wle[,1],
  WLE_SE = wle[,2],
  PVs
)


describeBy(out$WLE, out$grade)



# Transform per grade
out$WLE_IQ    <- NA
out$WLE_SE_IQ <- NA
PVs_IQ <- as.data.frame(matrix(NA, nrow=nrow(PVs), ncol=ncol(PVs)))
names(PVs_IQ) <- paste0(names(PVs), "_IQ")

for (g in unique(data$grade)) {
  idx <- which(data$grade == g)
  mean_g <- mean(wle[idx,1], na.rm=TRUE)
  sd_g   <- sd(wle[idx,1], na.rm=TRUE)
  out$WLE_IQ[idx]    <- 100 + 15 * (wle[idx,1] - mean_g) / sd_g
  out$WLE_SE_IQ[idx] <- 15 * wle[idx,2] / sd_g
  PVs_IQ[idx,] <- lapply(PVs[idx,], function(x) 100 + 15 * (x - mean_g) / sd_g)
}

out <- cbind(out, PVs_IQ)


writexl::write_xlsx(out, "lin_clean25.xlsx")
