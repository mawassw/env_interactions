#######Data input#########
dat <-  read.table(file = "C:/Users/Walid/Desktop/Work/DataManp/Data/R-IAC_Data_230210-sub.txt", header = TRUE,stringsAsFactors = FALSE)

df <- cbind.data.frame("BIRTHY" = dat$BIRTHYW, "YEARM" = dat$YEARM, "Ind" = dat$FEMME, "AFR" = dat$AFR, "InfMort" = dat$OffMortality)

Temper <- read.table(file = "C:/Users/Walid/Desktop/Work/DataManp/Data/Temperature.txt", header = TRUE, stringsAsFactors = FALSE)

PDSI <- read.table(file = "C:/Users/Walid/Desktop/Work/DataManp/Data/PDSI_local.txt", header = TRUE, stringsAsFactors = FALSE)

NAO <- read.table(file = "C:/Users/Walid/Desktop/Work/DataManp/Data/NAO-winter-1400-2001.txt", header = TRUE, stringsAsFactors = FALSE)

Solar <- read.table(file = "C:/Users/Walid/Desktop/Work/DataManp/Data/Solar.txt", header = TRUE, stringsAsFactors = FALSE)

mortrate <- read.table(file = "C:/Users/walid/Desktop/Work/Mortality/mortrate.txt", header = TRUE)

dat$AFR <- as.numeric(scale(dat$AFR, center = TRUE, scale = TRUE))
dat$Temper<- as.numeric(scale(dat$Temper, center = TRUE, scale = TRUE))
dat$NAO <- as.numeric(scale(dat$NAO, center = TRUE, scale = TRUE))
dat$Mortrate <- as.numeric(scale(dat$Mortrate, center = TRUE, scale = TRUE))
dat$Solar <- as.numeric(scale(dat$Solar, center = TRUE, scale = TRUE))
dat$PDSI <- as.numeric(scale(dat$PDSI, center = TRUE, scale = TRUE))

colnames(NAO)[2] <- "value"
colnames(PDSI)[2] <- "value"
colnames(Solar)[2] <- "value"
colnames(PDSI)[1] <- "Year"
colnames(Solar)[1] <- "Year"
colnames(mortrate)[2] <- "value"

#########correlation coefficients######

corr.coef <- function(data, col1, col2){
  #calculte the mean values of col1 annually
  y <-  aggregate(data[,col1], list("BIRTHY" = data[,1]), mean)
  #calculte the mean values of col2 annually
  x <-  aggregate(data[,col2], list("BIRTHY" = data[,1]), mean)
  colnames(y)[2] <-  as.character(col1)
  colnames(x)[2] <-  as.character(col2)
  temp.df <-  merge(y,x, by = "BIRTHY") #merge the datasets for correlation calculation
  coef.window <-  cor(temp.df[,2], temp.df[,3], method = "pearson") #calculate Pearson's correlation coefficient
  return(coef.window)
}

#######efficient way to calculte the correaltion coefficients of all windows####

columns <- names(test[,2:37]) #assign all the columns to be analyzed
coeff <- NULL #create empty dataframe
n <- 1 #allows succession for loop
for(col in columns){
  coeff[n] <- corr.coef(test, "AFR", col) #calculte the correlation coefficient using the created function
  n <- n + 1
}

write.table(coeff,file = "C:/Users/Walid/Desktop/coeff.txt") #outputs all the correlation coefficients for all the created windows

############group by temperature#######################
for(i in 1:nrow(test)){
  test$tempgrp <- NULL
  if(i %in% 1:143){
    test$temgrp[i] <- as.character("group1")
  }else if(i %in% 144:286){
    test$temgrp[i] <- as.character("group2")
  }else if(i %in% 287:429){
    test$temgrp[i] <- as.character("group3")
  }else if(i %in% 430:572){
    test$temgrp[i] <- as.character("group4")
  }
}

#########group by 1st and 3rd Quartile##########
dat$mortrate2<- with(dat, factor(findInterval(dat$mortrate_01y, 
               c(-Inf, quantile(dat$Mortrate, probs=c(0.25, .75)), Inf)), 
                labels=c("Low","Middle","High")))
library(Hmisc)

dat$mortrate2 <- cut2(dat$Mortrate, g =3)

##########################window function#########################
test = data.frame(cbind("BIRTHYW"=dat$BIRTHYW,column=rep(0,572)))
window <- function(distance=0, len=0,dataframe, variable){
  for(m in distance){
    for(y in len){
      for(i in 1:nrow(dataframe)){
      start <- dataframe$BIRTHY[i] + m #start year of window
      end <- start + y #end year of window
      years <- seq(start,end,by = 1) #create intra-years
      z = seq(1, length(years),by = 1)
      temp <-  NULL
      #extract the variable values for each year
        for(n in z){
        temp[n] <- variable[variable$Year == years[n],]$value
        }
      mean.temp <-  mean(temp) #calculte the mean variable for the window
      test$column[i] <<- mean.temp #assign it to proper ind into intial dataframe
      }
    #naming the variable
    name <- paste(deparse(substitute(variable)), as.character(m),sep = "+" )  
    colnames(test)[dim(test)[2]] <<- paste(as.character(name), as.character(y), sep = "_")
    }
  }
}

window(distance = 0, len = seq(0,3,1), dataframe = dat, variable = mortrate)

###REGRESSION MODELS#######################

##########Infant mortality##################
m <- glm(OffMortality~1+offset(log(FERTILITY)), data = dat, family = "quasipoisson")
m1 <- glm(OffMortality~poly(AFR,3)+mortrate4+COHORTW+TWIN+offset(log(FERTILITY)), data = dat, family = "quasipoisson")
m2 <- glm(OffMortality~AFR*mortrate1+COHORTW+TWIN+offset(log(FERTILITY)), data = dat, family = "quasipoisson")
m2 <- glm(OffMortality~scale(AFR)*mortrate4+scale(COEFPAR)+scale(I(COEFPAR*COEFPAR))+COHORTW+TWIN+offset(log(FERTILITY)), data = dat, family = "quasipoisson")

anova(m1, m2, test = "Chisq")
Anova(m2, type = "II")

summary(m)
summary(m1)
summary(m2)

library(effects)

plot(effect("poly(AFR, 3)", m1, xlevels = list(AFR = seq(18,35,1),FERTILITY=seq(1,10,2))), rescale.axis = FALSE, multiline = TRUE, rug = TRUE, ylim=c(0,2))
plot(effect("poly(AFR, 3)*mortrate1", m2, xlevels = list(AFR=seq(18,35,1),FERTILITY=seq(1,10,2))), rescale.axis = FALSE, multiline = TRUE, rug = TRUE, ylim = c(0,2))
plot(allEffects(m1, default.levels = 50), rescale.axis = FALSE, multiline = TRUE)
#########LRS########
mmm <- glm(LRS~1, data = temp, family = "quasipoisson")
mmm1 <- glm(LRS~AFR+Mortrate+COEFPAR+I(COEFPAR*COEFPAR)+TWIN+scale(BIRTHYW), data = dat, family = "quasipoisson")
mmm2 <- glm(LRS~AFR*Mortrate+COEFPAR+I(COEFPAR*COEFPAR)+TWIN+scale(BIRTHYW)+OffMortality, data = dat, family = "quasipoisson")
mmm3 <- glm(LRS~AFR*mortrate1+COEFPAR+I(COEFPAR*COEFPAR)+TWIN+COHORTW, data = dat, family = "quasipoisson")
mmm4 <- glm(LRS~scale(AFR)*mortrate1+scale(COEFPAR)+scale(I(COEFPAR*COEFPAR))+TWIN+COHORTW, data = dat, family = "quasipoisson")

anova(mmm4,mmm3, test = "Chisq")

Anova(mmm3, type = "II")

plot(effect("scale(AFR):scale(Mortrate)", mmm2, xlevels = list(AFR = seq(18,35,1), Mortrate = c(0.02274367,0.08612651,0.1733076))), rescale.axis = FALSE, multiline = TRUE, rug = TRUE)
plot(effect("AFR:Mortrate", mmm2, xlevels = list(AFR = seq(18,35,1),Mortrate = c(0.02274367,0.08612651,0.1733076))), rescale.axis = FALSE, multiline = TRUE, rug = TRUE)
plot(effect("AFR:mortrate1", mmm3, xlevels = list(AFR = seq(18,35,1))), rescale.axis = FALSE, multiline = TRUE, rug = TRUE)
#########FERTILITY########
mmm <- glm(FERTILITY~1, data = temp, family = "quasipoisson")
mmm1 <- glm(FERTILITY~AFR+Mortrate+COEFPAR+I(COEFPAR*COEFPAR)+TWIN+scale(BIRTHYW), data = dat, family = "quasipoisson")
mmm2 <- glm(FERTILITY~scale(AFR)*mortrate1+scale(COEFPAR)+scale(I(COEFPAR*COEFPAR))+TWIN+COHORTW+scale(OffMortality), data = dat, family = "quasipoisson")
mmm3 <- glm(FERTILITY~AFR*mortrate4+COEFPAR+I(COEFPAR*COEFPAR)+TWIN+COHORTW, data = dat, family = "quasipoisson")
mmm4 <- glm(FERTILITY~scale(AFR)*mortrate1+scale(COEFPAR)+scale(I(COEFPAR*COEFPAR))+TWIN+COHORTW, data = dat, family = "quasipoisson")

anova(mmm4,mmm3, test = "Chisq")

Anova(mmm4, type = "II")

plot(effect("scale(AFR):scale(Mortrate)", mmm2, xlevels = list(AFR = seq(18,35,1), Mortrate = c(0.02274367,0.08612651,0.1733076))), rescale.axis = FALSE, multiline = TRUE, rug = TRUE)
plot(effect("scale(AFR):mortrate1", mmm2, xlevels = list(AFR = seq(18,35,1))), rescale.axis = FALSE, multiline = TRUE, rug = TRUE)
plot(effect("AFR:mortrate4", mmm3, xlevels = list(AFR = seq(18,35,1))), rescale.axis = FALSE, multiline = TRUE, rug = TRUE)

#####fitdem######
mm <- lm(fitdem~1, data = dat)
mm1 <- lm(fitdem~AFR*FERTILITY+FERTILITY*Mortrate+COEFPAR+I(COEFPAR*COEFPAR)+TWIN+scale(BIRTHYW)*Mortrate, data = dat)
mm1 <- lm(scale(fitdem)~scale(AFR)*scale(FERTILITY)+scale(COEFPAR)+scale(I(COEFPAR*COEFPAR))+TWIN+scale(BIRTHYW), data = dat)
mm2 <- lm(fitdem~AFR*mortrate1+COEFPAR+I(COEFPAR*COEFPAR)+TWIN, data = dat)
mm3 <- lm(scale(fitdem)~scale(AFR)*mortrate1+scale(COEFPAR)+scale(I(COEFPAR*COEFPAR))+TWIN+COHORTW, data = dat)

library(car)
Anova(mm2, type = "II")

af <- anova(mm2)
afss <- af$"Sum Sq"
print(cbind(af,PctExp=afss/sum(afss)*100))

summary(mm)
summary(mm2)
summary(mm3)

anova(mm1,mm2, test = "Chisq")

plot(effect("scale(AFR):scale(Mortrate)", mm1, xlevels = list(AFR = seq(18,35,1), Mortrate = c(0.02274367,0.08612651,0.1733076))), rescale.axis = FALSE, multiline = TRUE, rug = TRUE)
plot(effect("mortrate1:COHORTW", mm2, xlevels = list(AFR=seq(18,35,1), COHORTW=seq(1,8,1))), rescale.axis = FALSE, multiline = TRUE, rug = TRUE)
plot(effect("scale(AFR):mortrate1", mm3, xlevels = list(AFR = seq(18,35,1))), rescale.axis = FALSE, multiline = TRUE, rug = TRUE)

##AFR###
model.null <- lm(AFR~1, data = dat)
model <- lm(scale(AFR)~scale(Mortrate_3y)+scale(BIRTHYW)+scale(COEFPAR)+scale(I(COEFPAR*COEFPAR))+TWIN+OffMortality, data = dat)
model <- lm(AFR~Mortrate_3y+BIRTHYW+COEFPAR+I(COEFPAR*COEFPAR)+TWIN+OffMortality, data = dat)
#without 2.5% extremes
model_cens <- lm(AFR~Mortrate_3y+BIRTHYW+COEFPAR+I(COEFPAR*COEFPAR)+TWIN+OffMortality, data = test)
model.1 <- lm(AFR~mortrate4+COHORTW+TWIN, data = dat)
model.2 <- lm(AFR~mortrate4*COHORTW+TWIN, data = dat)
model.3 <- lm(scale(AFR)~mortrate1+COHORTW+TWIN+scale(OffMortality), data = dat)

Anova(model.1, type = "II")
anova(model.1, model.2)

af <- anova(model.1)
afss <- af$"Sum Sq"
print(cbind(af,PctExp=afss/sum(afss)*100))

summary(model)
summary(model.2)

plot(effect("scale(Mortrate)", model), rescale.axis = FALSE, multiline = TRUE, rug = TRUE)
plot(effect("mortrate_equal", model.1, xlevels = list()), rescale.axis = FALSE, multiline = TRUE, rug = TRUE)
plot(effect("mortrate1*COHORTW", model.1, xlevels = list(COHORTW=seq(1,8,1))), rescale.axis = FALSE, multiline = TRUE, rug = TRUE)
######################################################
a <- list("Low","Middle", "High")
b <- list("AFR", "FERTILITY","fitdem", "OffMortality", "Mortrate_3y")

for (i in a) {
  for (j in b) {
    print(paste(round(mean(dat[dat$mortrate1 == i,j]),2), i, j))
    print(paste(round(sd(dat[dat$mortrate1 == i,j]),2), i,j))
  }
}

for (i in 1:nrow(IAC)){
  if(IAC$COHORTW[i] %in% seq(1,4,1)){
   IAC$period[i] <- 1  
  }else{
    IAC$period[i] <- 2
  }
}
##################################################
#reproductive lifespan
for (i in 1:length(row.names(dat))) {
  dat$repspan[i] <- abs(dat$AFR[i] - dat$ALR[i])
}
#rate of reproduction
for (i in 1:length(row.names(IAC))) {
  dat$reprate[i] <- dat$FERTILITY[i]/dat$repspan[i]
}

