IAC <- as.data.frame(read.table(file = "C:/Users/walid/OneDrive/Bureau/Work/DataManp/dat.txt", header = TRUE))

Ped <- as.data.frame(read.table(file = "C:/Users/walid/OneDrive/Bureau/Work/DataManp/pedigreeIAC.txt", header = TRUE))

names(IAC)[3] <- "animal"

names(Ped)[1] <- "animal"

for (i in 1:3) {
  Ped[,i] <- as.factor(Ped[,i])
}

IAC$animal <- as.factor(IAC$animal)
IAC$TWIN <- as.factor(IAC$TWIN)
IAC$MOTHERW <- as.factor(IAC$MOTHERW)

require(MCMCglmm)

prunedPed <- prunePed(pedigree = Ped, dat$animal)

prior <- list(R=list(V=diag(2), nu=3), G=list(G1=list(V=diag(2), nu=3),G2=list(V=diag(2), nu=3)))

model_afr_fert <- MCMCglmm(cbind(AFR, FERTILITY)~trait-1 + trait:COEFPAR + trait:I(COEFPAR*COEFPAR) + at.level(trait,2):TWIN + trait:YEARM, random = ~us(trait):animal+us(trait):MOTHERW, rcov = ~us(trait):units, data = IAC, pedigree = prunedPed, family = c("gaussian", "poisson"), nitt = 550000, burnin = 50000, thin = 500, prior = prior, verbose = FALSE)

IAC_low <- IAC[IAC$mortrate1 == "Q1",]
IAC_med <- IAC[IAC$mortrate1 == "Q2",]
IAC_high <- IAC[IAC$mortrate1 == "Q3",]

prunedPed_low <- prunePed(pedigree = Ped, IAC_low$animal)
prunedPed_med <- prunePed(pedigree = Ped, IAC_med$animal)
prunedPed_high <- prunePed(pedigree = Ped, IAC_high$animal)

model_afr_fert_low <- MCMCglmm(cbind(AFR, FERTILITY)~trait-1 + trait:COEFPAR + trait:I(COEFPAR*COEFPAR) + trait:TWIN + trait:YEARM, random = ~us(trait):animal+us(trait):MOTHERW, rcov = ~us(trait):units, data = IAC_low, pedigree = prunedPed_low, family = c("gaussian", "poisson"), nitt = 550000, burnin = 50000, thin = 500, prior = prior, verbose = FALSE)
model_afr_fert_med <- MCMCglmm(cbind(AFR, FERTILITY)~trait-1 + trait:COEFPAR + trait:I(COEFPAR*COEFPAR) + trait:TWIN + trait:YEARM, random = ~us(trait):animal+us(trait):MOTHERW, rcov = ~us(trait):units, data = IAC_med, pedigree = prunedPed_med, family = c("gaussian", "poisson"), nitt = 550000, burnin = 50000, thin = 500, prior = prior, verbose = FALSE)
model_afr_fert_high <- MCMCglmm(cbind(AFR, FERTILITY)~trait-1 + trait:COEFPAR + trait:I(COEFPAR*COEFPAR) + trait:TWIN + trait:YEARM, random = ~us(trait):animal+us(trait):MOTHERW, rcov = ~us(trait):units, data = IAC_high, pedigree = prunedPed_high, family = c("gaussian", "poisson"), nitt = 550000, burnin = 50000, thin = 500, prior = prior, verbose = FALSE)
##############################heritabilites#####################################
#AFR
h2.afr_low<-model_afr_fert_low$VCV[,1]/(model_afr_fert_low$VCV[,1]+model_afr_fert_low$VCV[,5]+model_afr_fert_low$VCV[,9])
h2.afr_med<-model_afr_fert_med$VCV[,1]/(model_afr_fert_med$VCV[,1]+model_afr_fert_med$VCV[,5]+model_afr_fert_med$VCV[,9])
h2.afr_high<-model_afr_fert_high$VCV[,1]/(model_afr_fert_high$VCV[,1]+model_afr_fert_high$VCV[,5]+model_afr_fert_high$VCV[,9])

posterior.mode(h2.afr_low)
HPDinterval(h2.afr_low)

posterior.mode(h2.afr_med)
HPDinterval(h2.afr_med)

posterior.mode(h2.afr_high)
HPDinterval(h2.afr_high)
#Fertlity
h2.fert_low <- model_afr_fert_low$VCV[,4]/(model_afr_fert_low$VCV[,4]+model_afr_fert_low$VCV[,8]+model_afr_fert_low$VCV[,12]+log(1+1/exp(model_afr_fert_low$Sol[,2])))
h2.fert_med <- model_afr_fert_med$VCV[,4]/(model_afr_fert_med$VCV[,4]+model_afr_fert_med$VCV[,8]+model_afr_fert_med$VCV[,12]+log(1+1/exp(model_afr_fert_med$Sol[,2])))
h2.fert_high <- model_afr_fert_high$VCV[,4]/(model_afr_fert_high$VCV[,4]+model_afr_fert_high$VCV[,8]+model_afr_fert_high$VCV[,12]+log(1+1/exp(model_afr_fert_high$Sol[,2])))

posterior.mode(h2.fert_low)
HPDinterval(h2.fert_low)

posterior.mode(h2.fert_med)
HPDinterval(h2.fert_med)

posterior.mode(h2.fert_high)
HPDinterval(h2.fert_high)

############################################genetic correlation###########################
gencorr_afr_fert <- model_afr_fert$VCV[,2]/sqrt(model_afr_fert$VCV[,1]*model_afr_fert$VCV[,4])
gencorr_afr_fert_low <- model_afr_fert_low$VCV[,2]/sqrt(model_afr_fert_low$VCV[,1]*model_afr_fert_low$VCV[,4])
gencorr_afr_fert_med <- model_afr_fert_med$VCV[,2]/sqrt(model_afr_fert_med$VCV[,1]*model_afr_fert_med$VCV[,4])
gencorr_afr_fert_high <- model_afr_fert_high$VCV[,2]/sqrt(model_afr_fert_high$VCV[,1]*model_afr_fert_high$VCV[,4])

posterior.mode(gencorr_afr_fert)
HPDinterval(gencorr_afr_fert)

posterior.mode(gencorr_afr_fert_low)
HPDinterval(gencorr_afr_fert_low)

posterior.mode(gencorr_afr_fert_med)
HPDinterval(gencorr_afr_fert_med)

posterior.mode(gencorr_afr_fert_high)
HPDinterval(gencorr_afr_fert_high)

##############################plotting###################################
library(ggplot2)
library(gridExtra)
library(ggpubr)

#plotting variance components###
df <- NULL
df <- cbind.data.frame("var" = model_afr_fert$VCV[1:1000,4], "Level" = "Population", "legend" = "Additive Va")
df <- rbind(df,cbind.data.frame("var" = model_afr_fert_low$VCV[1:1000,4], "Level" = "Low", "legend" = "Additive Va"))
df <- rbind(df, cbind.data.frame("var" = model_afr_fert_med$VCV[1:1000,4], "Level" = "Mid", "legend" = "Additive Va"))
df <- rbind(df, cbind.data.frame("var" = model_afr_fert_high$VCV[1:1000,4], "Level" = "High", "legend" = "Additive Va"))


df1 <- NULL
df1 <- cbind.data.frame("var" = model_afr_fert$VCV[1:1000,1], "Level" = "Population", "legend" = "Additive Va")
df1 <- rbind(df1,cbind.data.frame("var" = model_afr_fert_low$VCV[1:1000,1], "Level" = "Low", "legend" = "Additive Va"))
df1 <- rbind(df1, cbind.data.frame("var" = model_afr_fert_med$VCV[1:1000,1], "Level" = "Mid", "legend" = "Additive Va"))
df1 <- rbind(df1, cbind.data.frame("var" = model_afr_fert_high$VCV[1:1000,1], "Level" = "High", "legend" = "Additive Va"))


p <- ggboxplot(df, x = "Level", y = "var")+ggtitle("Fertility")+coord_flip()+xlab("Infant mortality rate") +theme(axis.text=element_text(size=12),axis.title.x = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))
p <- p+annotate("point", x =c(1,2,3,4), y =c(0.1,0.13,0.09,0.2), colour = "red")
p <- p+annotate("rect", xmin=c(0.5,1.5,2.5,3.5), xmax=c(1.5,2.5,3.5,4.5), ymin=c(0.06,0.06,0.06,0.07) , ymax=c(0.24,0.24,0.19,0.36), alpha=0.2, color="blue", fill="blue")

p1 <- ggboxplot(df1, x = "Level", y = "var")+ggtitle("AFR")+coord_flip()+ylab("Additive variance")+theme(axis.text=element_text(size=12),axis.title.y = element_blank(),axis.title = element_text(size = 14),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))
p1 <- p1+annotate("point", x =c(1,2,3,4), y =c(0.46,0.24,0.34,0.42), colour = "red")
p1 <- p1+annotate("rect", xmin=c(0.5,1.5,2.5,3.5), xmax=c(1.5,2.5,3.5,4.5), ymin=c(0.24,0.08,0.14,0.12) , ymax=c(0.89,0.67,0.68,1.12), alpha=0.2, color="blue", fill="blue")
ggarrange(p,p1,labels = c("A", "B"),ncol = 1, nrow = 2)

#plotting genetic correlation###
df2 <- NULL
df2 <- cbind.data.frame("gencorr" = (model_afr_fert$VCV[1:1000,2]/sqrt(model_afr_fert$VCV[1:1000,1]*model_afr_fert$VCV[1:1000,4])),"condition"="Population", "legend" = "AFR~FERTILITY")
df2 <- rbind(df2,cbind.data.frame("gencorr" = (model_afr_fert_low$VCV[1:1000,2]/sqrt(model_afr_fert_low$VCV[1:1000,1]*model_afr_fert_low$VCV[1:1000,4])),"condition"="Low","legend" = "AFR~FERTILITY"))
df2 <- rbind(df2,cbind.data.frame("gencorr" = (model_afr_fert_med$VCV[1:1000,2]/sqrt(model_afr_fert_med$VCV[1:1000,1]*model_afr_fert_med$VCV[1:1000,4])),"condition"="Mid","legend" = "AFR~FERTILITY"))
df2 <- rbind(df2,cbind.data.frame("gencorr" = (model_afr_fert_high$VCV[1:1000,2]/sqrt(model_afr_fert_high$VCV[1:1000,1]*model_afr_fert_high$VCV[1:1000,4])),"condition"="High","legend" = "AFR~FERTILITY"))

b <- ggplot(aes(y = gencorr, x = condition), data = df2) + geom_boxplot()+ylab("Correlation")+xlab("Infant mortality rate")+geom_hline(yintercept=0, linetype="dotted")+theme(axis.text=element_text(size=12),axis.title = element_text(size = 14),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))
b <- b + annotate("segment", x = 0.6, xend = 1.4, y = -0.414163, yend = -0.414163, colour = "red", size=2, alpha=0.6)
b <- b + annotate("segment", x = 1.6, xend = 2.4, y = -0.352877, yend = -0.352877, colour = "red", size=2, alpha=0.6)
b <- b + annotate("segment", x = 2.6, xend = 3.4, y = -0.3943422, yend = -0.3943422, colour = "red", size=2, alpha=0.6)
b <- b + annotate("segment", x = 3.6, xend = 4.4, y = -0.5042256, yend = -0.5042256, colour = "red", size=2, alpha=0.6)
b

#plotting heritabilites###
df3 <- NULL
df3 <- cbind.data.frame("h2" = (model_afr_fert$VCV[1:1000,1]/(model_afr_fert$VCV[1:1000,1]+model_afr_fert$VCV[1:1000,5]+model_afr_fert$VCV[1:1000,9])),"condition"="Population", "legend" = "AFR")
df3 <- rbind(df3,cbind.data.frame("h2" = (model_afr_fert$VCV[1:1000,4]/(model_afr_fert$VCV[1:1000,4]+model_afr_fert$VCV[1:1000,8]+model_afr_fert$VCV[1:1000,12]+log(1+1/exp(model_afr_fert$Sol[1:1000,2])))),"condition"="Population", "legend" = "Fertility"))
df3 <- rbind(df3,cbind.data.frame("h2" = (model_afr_fert_low$VCV[1:1000,1]/(model_afr_fert_low$VCV[1:1000,1]+model_afr_fert_low$VCV[1:1000,5]+model_afr_fert_low$VCV[1:1000,9])),"condition"="Low", "legend" = "AFR"))
df3 <- rbind(df3,cbind.data.frame("h2" = (model_afr_fert_med$VCV[1:1000,1]/(model_afr_fert_med$VCV[1:1000,1]+model_afr_fert_med$VCV[1:1000,5]+model_afr_fert_med$VCV[1:1000,9])),"condition"="Mid","legend" = "AFR"))
df3 <- rbind(df3,cbind.data.frame("h2" = (model_afr_fert_high$VCV[1:1000,1]/(model_afr_fert_high$VCV[1:1000,1]+model_afr_fert_high$VCV[1:1000,5]+model_afr_fert_high$VCV[1:1000,9])),"condition"="High","legend" = "AFR"))
df3 <- rbind(df3,cbind.data.frame("h2" = (model_afr_fert_low$VCV[1:1000,4]/(model_afr_fert_low$VCV[1:1000,4]+model_afr_fert_low$VCV[1:1000,8]+model_afr_fert_low$VCV[1:1000,12]+log(1+1/exp(model_afr_fert_low$Sol[1:1000,2])))),"condition"="Low", "legend" = "Fertility"))
df3 <- rbind(df3,cbind.data.frame("h2" = (model_afr_fert_med$VCV[1:1000,4]/(model_afr_fert_med$VCV[1:1000,4]+model_afr_fert_med$VCV[1:1000,8]+model_afr_fert_med$VCV[1:1000,12]+log(1+1/exp(model_afr_fert_med$Sol[1:1000,2])))),"condition"="Mid","legend" = "Fertility"))
df3 <- rbind(df3,cbind.data.frame("h2" = (model_afr_fert_high$VCV[1:1000,4]/(model_afr_fert_high$VCV[1:1000,4]+model_afr_fert_high$VCV[1:1000,8]+model_afr_fert_high$VCV[1:1000,12]+log(1+1/exp(model_afr_fert_high$Sol[1:1000,2])))),"condition"="High","legend" = "Fertility"))

ggplot(aes(y = h2, x = condition, fill = legend), data = df3) + geom_boxplot()+ylab("Heritability")+xlab("Early-life environmental condition")+theme_classic()
#plotting heritabilites2###
df3 <- NULL
df3 <- cbind.data.frame("h2" = posterior.mode(model_afr_fert$VCV[,1]/(model_afr_fert$VCV[,1]+model_afr_fert$VCV[,5]+model_afr_fert$VCV[,9])),"condition"="Population", "legend" = "AFR")
df3 <- rbind(df3,cbind.data.frame("h2" = posterior.mode(model_afr_fert$VCV[,4]/(model_afr_fert$VCV[,4]+model_afr_fert$VCV[,8]+model_afr_fert$VCV[,12]+log(1+1/exp(model_afr_fert$Sol[,2])))),"condition"="Population", "legend" = "Fertility"))
df3 <- rbind(df3,cbind.data.frame("h2" = posterior.mode(model_afr_fert_low$VCV[,1]/(model_afr_fert_low$VCV[,1]+model_afr_fert_low$VCV[,5]+model_afr_fert_low$VCV[,9])),"condition"="Low", "legend" = "AFR"))
df3 <- rbind(df3,cbind.data.frame("h2" = posterior.mode(model_afr_fert_med$VCV[,1]/(model_afr_fert_med$VCV[,1]+model_afr_fert_med$VCV[,5]+model_afr_fert_med$VCV[,9])),"condition"="Mid","legend" = "AFR"))
df3 <- rbind(df3,cbind.data.frame("h2" = posterior.mode(model_afr_fert_high$VCV[,1]/(model_afr_fert_high$VCV[,1]+model_afr_fert_high$VCV[,5]+model_afr_fert_high$VCV[,9])),"condition"="High","legend" = "AFR"))
df3 <- rbind(df3,cbind.data.frame("h2" = posterior.mode(model_afr_fert_low$VCV[,4]/(model_afr_fert_low$VCV[,4]+model_afr_fert_low$VCV[,8]+model_afr_fert_low$VCV[,12]+log(1+1/exp(model_afr_fert_low$Sol[,2])))),"condition"="Low", "legend" = "Fertility"))
df3 <- rbind(df3,cbind.data.frame("h2" = posterior.mode(model_afr_fert_med$VCV[,4]/(model_afr_fert_med$VCV[,4]+model_afr_fert_med$VCV[,8]+model_afr_fert_med$VCV[,12]+log(1+1/exp(model_afr_fert_med$Sol[,2])))),"condition"="Mid","legend" = "Fertility"))
df3 <- rbind(df3,cbind.data.frame("h2" = posterior.mode(model_afr_fert_high$VCV[,4]/(model_afr_fert_high$VCV[,4]+model_afr_fert_high$VCV[,8]+model_afr_fert_high$VCV[,12]+log(1+1/exp(model_afr_fert_high$Sol[,2])))),"condition"="High","legend" = "Fertility"))

p <- ggplot(data = df3, aes(x = condition, y = h2, group = legend, color = legend))+geom_line(stat = "summary", fun.y = "mean", size = 1.5)+geom_point(stat = "summary", fun.y = "mean", size =2)+ylab("Heritability")+xlab("Early-life environmental condition")+theme(axis.text=element_text(size=12),axis.title = element_text(size = 14),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))
p +annotate("text", x = 1:4, y = 0, label = c("n = 572", "n = 141", "n = 288","n = 143"))
p
#plotting change in variance components####
df4 <- NULL
df4 <- cbind.data.frame("var" = model_afr_fert_med$VCV[1:1000,4]-model_afr_fert_low$VCV[1:1000,4], "condition" = "Middle~Low", "legend" = "VA")
df4 <- rbind(df4, cbind.data.frame("var" = model_afr_fert_med$VCV[1:1000,12]-model_afr_fert_low$VCV[1:1000,12], "condition" = "Middle~Low", "legend" = "VR"))
df4 <- rbind(df4, cbind.data.frame("var" = model_afr_fert_med$VCV[1:1000,4]-model_afr_fert_high$VCV[1:1000,4], "condition" = "Middle~High", "legend" = "VA"))
df4 <- rbind(df4, cbind.data.frame("var" = model_afr_fert_med$VCV[1:1000,12]-model_afr_fert_high$VCV[1:1000,12], "condition" = "Middle~High", "legend" = "VR"))

df5 <- NULL
df5 <- cbind.data.frame("var" = model_afr_fert_med$VCV[1:1000,1]-model_afr_fert_low$VCV[1:1000,1], "condition" = "Middle~Low", "legend" = "VA")
df5 <- rbind(df5, cbind.data.frame("var" = model_afr_fert_med$VCV[1:1000,9]-model_afr_fert_low$VCV[1:1000,9], "condition" = "Middle~Low", "legend" = "VR"))
df5 <- rbind(df5, cbind.data.frame("var" = model_afr_fert_med$VCV[1:1000,1]-model_afr_fert_high$VCV[1:1000,1], "condition" = "Middle~High", "legend" = "VA"))
df5 <- rbind(df5, cbind.data.frame("var" = model_afr_fert_med$VCV[1:1000,9]-model_afr_fert_high$VCV[1:1000,9], "condition" = "Middle~High", "legend" = "VR"))

p <- ggplot(aes(y = var, x = condition, fill = legend), data = df4)+geom_boxplot(varwidth = T)+ylab("Change in variance")+ggtitle("Fertility")+geom_hline(yintercept=0)+theme_classic()
p1 <- ggplot(aes(y = var, x = condition, fill = legend), data = df5)+geom_boxplot(varwidth = T)+ylab("Change in variance")+ggtitle("AFR")+geom_hline(yintercept=0)+theme_classic()

ggarrange(p,p1,labels = c("A", "B"),ncol = 1, nrow = 2)
##########################

library(hexbin)

A <- model_afr_fert_low$VCV[1:1000,1]
B <- model_afr_fert_low$VCV[1:1000,4]

bin<-hexbin(A, B, xbins=50)
plot(bin, main="Hexagonal Binning")
sunflowerplot(A,B)
C <- model_afr_fert_med$VCV[1:1000,1]
D <- model_afr_fert_med$VCV[1:1000,4]
sunflowerplot(C,D)
E <- model_afr_fert_high$VCV[1:1000,1]
F <- model_afr_fert_high$VCV[1:1000,4]
sunflowerplot(E,F)
######################plotting breeding values##########################
qplot(BIRTHYW, BV, data = BV_all, colour = infmortrate)+xlab("Year of Birth")+ylab("Breeding Value")+geom_smooth(method='lm',se=FALSE)+theme(axis.text=element_text(size=12),axis.title = element_text(size = 14),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))
#############################animal model infant survival#############################
require(MCMCglmm)
#to include indirect maternal genetic effect
df$MOTHER2 <- df$MOTHER
df$MOTHER2 <- factor(df$MOTHER2, levels = levels(df$animal))

invA <- inverseA(Ped)$Ainv

prior.2 <- list(R=list(V=1, fix=1), G=list(G1=list(V=1, nu=1000,alpha.mu=0, alpha.V=1),G2=list(V=1, nu=1000,alpha.mu=0, alpha.V=1),G3=list(V=1, nu=1000,alpha.mu=0, alpha.V=1)))

mcmc.mort2 <- MCMCglmm(InfantMort ~ 1, random = ~animal+MOTHER+MOTHER2, ginverse = list(animal = invA, MOTHER2 = invA),family = "ordinal", data = df, prior = prior.2, nitt = 5500, burnin = 500, thin = 5,pl=FALSE,trunc=TRUE, verbose = FALSE)

summary(mcmc.mort2)
###############################multivariate MCMCglmm using QGglmm##########################################################
library(MCMCglmm)
library(QGglmm)

G.low <- matrix(c(mean(model_afr_lrs_low[["VCV"]][,"traitAFR:traitAFR.animal"]),mean(model_afr_lrs_low[["VCV"]][,"traitAFR:traitALR.animal"]),mean(model_afr_lrs_low[["VCV"]][,"traitAFR:traitLRS.animal"]),mean(model_afr_lrs_low[["VCV"]][,"traitAFR:traitALR.animal"]),mean(model_afr_lrs_low[["VCV"]][,"traitALR:traitALR.animal"]),mean(model_afr_lrs_low[["VCV"]][,"traitALR:traitLRS.animal"]),mean(model_afr_lrs_low[["VCV"]][,"traitAFR:traitLRS.animal"]),mean(model_afr_lrs_low[["VCV"]][,"traitALR:traitLRS.animal"]),mean(model_afr_lrs_low[["VCV"]][,"traitLRS:traitLRS.animal"])),ncol = 3)
M.low <- matrix(c(mean(model_afr_lrs_low[["VCV"]][,"traitAFR:traitAFR.MOTHERW"]),mean(model_afr_lrs_low[["VCV"]][,"traitAFR:traitALR.MOTHERW"]),mean(model_afr_lrs_low[["VCV"]][,"traitAFR:traitLRS.MOTHERW"]),mean(model_afr_lrs_low[["VCV"]][,"traitAFR:traitALR.MOTHERW"]),mean(model_afr_lrs_low[["VCV"]][,"traitALR:traitALR.MOTHERW"]),mean(model_afr_lrs_low[["VCV"]][,"traitALR:traitLRS.MOTHERW"]),mean(model_afr_lrs_low[["VCV"]][,"traitAFR:traitLRS.MOTHERW"]),mean(model_afr_lrs_low[["VCV"]][,"traitALR:traitLRS.MOTHERW"]),mean(model_afr_lrs_low[["VCV"]][,"traitLRS:traitLRS.MOTHERW"])),ncol = 3)
R.low <- matrix(c(mean(model_afr_lrs_low[["VCV"]][,"traitAFR:traitAFR.units"]),mean(model_afr_lrs_low[["VCV"]][,"traitAFR:traitALR.units"]),mean(model_afr_lrs_low[["VCV"]][,"traitAFR:traitLRS.units"]),mean(model_afr_lrs_low[["VCV"]][,"traitAFR:traitALR.units"]),mean(model_afr_lrs_low[["VCV"]][,"traitALR:traitALR.units"]),mean(model_afr_lrs_low[["VCV"]][,"traitALR:traitLRS.units"]),mean(model_afr_lrs_low[["VCV"]][,"traitAFR:traitLRS.units"]),mean(model_afr_lrs_low[["VCV"]][,"traitALR:traitLRS.units"]),mean(model_afr_lrs_low[["VCV"]][,"traitLRS:traitLRS.units"])),ncol = 3)
P.low <- G+M+R

QGmvparams(predict = yhat.low, vcv.G = G.low, vcv.P = P.low, model=c("Gaussian", "Gaussian", "Poisson.log"))

#################reproductive window################
df <- IAC[IAC$FERTILITY != 1 & IAC$repspan > 1,]

y <-  aggregate(df[,"repspan"], list("YEARM" = df[,2]), mean)

y.1 <-  aggregate(df[,"reprate"], list("Cohort" = df[,6]), mean)
y.1 <-  aggregate(df[,"repspan"], list("AFR" = df[,20]), mean)

plot(x = y$YEARM, y = y$x)
plot(x = y.1$AFR, y = y.1$x)

load(file = "C:/Users/walid/Desktop/Work/DataManp/Data/model_repspan.rda")
load(file = "C:/Users/walid/Desktop/Work/DataManp/Data/model_repspan_low.rda")
load(file = "C:/Users/walid/Desktop/Work/DataManp/Data/model_repspan_med.rda")
load(file = "C:/Users/walid/Desktop/Work/DataManp/Data/model_repspan_high.rda")

yhat = predict.MCMCglmm(model_repspan, type = "terms")
yhat.low <- predict.MCMCglmm(model_repspan_low, type = "terms")
yhat.med <- predict.MCMCglmm(model_repspan_med, type = "terms")
yhat.high <- predict.MCMCglmm(model_repspan_high, type = "terms")

df.post <- data.frame(va= as.vector(model_repspan_high[["VCV"]][,"animal"]), vp= rowSums(model_repspan_high[["VCV"]]))

head(df.post)

post <- do.call("rbind", apply(df.post, 1, function(row){
  QGparams(predict = yhat.high, var.a = row["va"], var.p = row["vp"], model="Poisson.log", verbose = FALSE)
}))

head(post)


dd <- density(post$h2.obs)
which.max(dd$y)

dd$x[which.max(dd$y)]
plot(dd)
rug(x)
abline(v=dd$x[which.max(dd$y)])

#heritability model total 0.165812
#heritability model low 0.01982446
#heritability model med 0.5292635
#heritability model med 0.01193333
##############################################
load(file="C:/Users/walid/Desktop/Work/DataManp/Data/model_afr_fert.rda")

b <- predict.MCMCglmm(model_multi, type = "terms", it =3)
yhat.bi <- matrix(c(a[1:572], a[573:1144]), ncol = 2)

df.post <- data.frame(model_afr_fert[["VCV"]])
df.post <- df.post[,-c(5,6,7,8)]
head(df.post)

post <- apply(df.post,1,function(row){
  G <- matrix(c(row["traitAFR:traitAFR.animal"],
                row["traitFERTILITY:traitAFR.animal"],
                row["traitAFR:traitFERTILITY.animal"],
                row["traitFERTILITY:traitFERTILITY.animal"]),
              ncol = 2)
  P <- matrix(c(row["traitAFR:traitAFR.units"],
                row["traitFERTILITY:traitAFR.units"],
                row["traitAFR:traitFERTILITY.units"],
                row["traitFERTILITY:traitFERTILITY.units"]),
              ncol = 2)
  QGmvparams(predict = yhat.bi, vcv.G = G, vcv.P = P, models = c("Gaussian", "Poisson.log"), verbose = FALSE)
})
########################extracting breeding values#################################################
EBVextract(IAC, model_afr_fert, "FERTILITY")

EBVs <- readRDS(file = "C:/Users/walid/Desktop/EBVs.r")

EBV <- EBVs[,-c(1,2,3)]
EBV_afr <- NULL
EBV_fert <- NULL
for(i in 1:572){
  dd <- density(as.numeric(as.vector(EBV[i,])))
  EBV_fert[i] <- dd$x[which.max(dd$y)]
}


library(gplots)

x <- EBV$AFR
y <- EBV$FERTILITY
plot(x,y, main="Plot of Breeding Values",mgp=c(2,1,0), col=rgb(0,100,0,50,maxColorValue=255), pch=16, xlab = "AFR EBVs", ylab = "Fertility EBVs")
par(new=TRUE)
textplot(G,halign = "right", valign = "top", cex = 0.8)
temp <- locator(1)
text(temp, "G =")
#########################################################################
library(ggplot2)

ggplot(data = IAC, aes(x = mortrate1, y = AFR))+stat_boxplot(mapping = NULL, data = NULL, geom = "boxplot",position = "dodge", coef = 1.5, na.rm = FALSE, show.legend = NA,inherit.aes = TRUE)+theme_classic()+ylab("Age at first reproduction")+xlab("Early-life infant mortality")

ggplot(data=IAC, aes(x=BIRTHYW, y=Mortrate, group=1)) +geom_line(linetype = "dashed")+geom_point()+theme(axis.text=element_text(size=12),axis.title = element_text(size = 14),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+xlab("Year")+ylab("Infant mortality rate")

ggplot(test, aes(x = BV_afr, y = BV_fert, color = IMR))+geom_point()+ theme(axis.text=element_text(size=12),axis.title = element_text(size = 14),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+xlab("Breeding value of AFR")+ylab("Breeding value of fertility")

##############################################################
G <- matrix(
  c(posterior.mode(model_afr_fert$VCV[,1]),posterior.mode(model_afr_fert$VCV[,2]),posterior.mode(model_afr_fert$VCV[,3]),posterior.mode(model_afr_fert$VCV[,4])),
    nrow =2, 
    ncol=2
)

colnames(G) <- c("AFR", "FERTILITY") 

G_low <- matrix(
  c(posterior.mode(model_afr_fert_low$VCV[,1]),posterior.mode(model_afr_fert_low$VCV[,2]),posterior.mode(model_afr_fert_low$VCV[,3]),posterior.mode(model_afr_fert_low$VCV[,4])),
  nrow =2, 
  ncol=2
)

colnames(G_low) <- c("AFR", "FERTILITY") 

G_med <- matrix(
  c(posterior.mode(model_afr_fert_med$VCV[,1]),posterior.mode(model_afr_fert_med$VCV[,2]),posterior.mode(model_afr_fert_med$VCV[,3]),posterior.mode(model_afr_fert_med$VCV[,4])),
  nrow =2, 
  ncol=2
)

colnames(G_med) <- c("AFR", "FERTILITY") 

G_high <- matrix(
  c(posterior.mode(model_afr_fert_high$VCV[,1]),posterior.mode(model_afr_fert_high$VCV[,2]),posterior.mode(model_afr_fert_high$VCV[,3]),posterior.mode(model_afr_fert_high$VCV[,4])),
  nrow =2, 
  ncol=2
)

colnames(G_high) <- c("AFR", "FERTILITY")
#####################multivariate animal model#########################################3
load(file = "C:/Users/walid/OneDrive/Bureau/Work/DataManp/model_multi.rda")
library(MCMCglmm)
colnames(model_multi$VCV)

G_multi <- matrix(
  c(posterior.mode(model_multi$VCV[,1:9])),
  nrow =3, 
  ncol=3
)

colnames(G_multi) <- c("AFR", "fitdem", "OffMortality") 

posterior.mode(model_multi$VCV[,1])
HPDinterval(model_multi$VCV[,1])
posterior.mode(model_multi$VCV[,5])
HPDinterval(model_multi$VCV[,5])
posterior.mode(model_multi$VCV[,9])
HPDinterval(model_multi$VCV[,9])

gencorr_1 <- model_multi$VCV[,2]/sqrt(model_multi$VCV[,1]*model_multi$VCV[,5]) #AFR and fitness
gencorr_2 <- model_multi$VCV[,3]/sqrt(model_multi$VCV[,1]*model_multi$VCV[,9]) #AFR and infant mort
gencorr_3 <- model_multi$VCV[,6]/sqrt(model_multi$VCV[,5]*model_multi$VCV[,9]) #fitness and infant mort

posterior.mode(gencorr_3)
HPDinterval(gencorr_3)

herit_1 <- model_multi$VCV[,1]/(model_multi$VCV[,1]+model_multi$VCV[,10]+model_multi$VCV[,19])
herit_2 <- model_multi$VCV[,5]/(model_multi$VCV[,5]+model_multi$VCV[,14]+model_multi$VCV[,23])
herit_3 <- model_multi$VCV[,9]/(model_multi$VCV[,9]+model_multi$VCV[,18]+model_multi$VCV[,27]+log(1+1/exp(model_multi$Sol[,3])))

herit_latent <- model_multi$VCV[,9]/(model_multi$VCV[,9]+model_multi$VCV[,18]+model_multi$VCV[,27])

posterior.mode(herit_3)
HPDinterval(herit_3)

posterior.mode(herit_latent)
HPDinterval(herit_latent)
#####by env#######
load(file = "C:/Users/walid/OneDrive/Bureau/Work/DataManp/model_multi_low_rev.rda")
load(file = "C:/Users/walid/OneDrive/Bureau/Work/DataManp/model_multi_med_rev.rda")
load(file = "C:/Users/walid/OneDrive/Bureau/Work/DataManp/model_multi_high_rev.rda")
colnames(model_multi_low$VCV)

G_multi_high <- matrix(
  c(posterior.mode(model_multi_high$VCV[,1:9])),
  nrow =3, 
  ncol=3
)

colnames(G_multi_high) <- c("AFR", "fitdem","OffMortality") 

posterior.mode(model_multi_low$VCV[,c(1,10,19)])
HPDinterval(model_multi_low$VCV[,c(1,10,19)])
posterior.mode(model_multi_low$VCV[,c(5,14,23)])
HPDinterval(model_multi_low$VCV[,c(5,14,23)])
posterior.mode(model_multi_low$VCV[,c(9,18,27)])
HPDinterval(model_multi_low$VCV[,c(9,18,27)])

#low
gencorr_1 <- model_multi_low$VCV[,2]/sqrt(model_multi_low$VCV[,1]*model_multi_low$VCV[,5]) #AFR and fitness
gencorr_2 <- model_multi_low$VCV[,3]/sqrt(model_multi_low$VCV[,1]*model_multi_low$VCV[,9]) #AFR and IMC
gencorr_3 <- model_multi_low$VCV[,6]/sqrt(model_multi_low$VCV[,5]*model_multi_low$VCV[,9]) #fitness and IMC

#med
gencorr_1 <- model_multi_med$VCV[,2]/sqrt(model_multi_med$VCV[,1]*model_multi_med$VCV[,5]) #AFR and fitness
gencorr_2 <- model_multi_med$VCV[,3]/sqrt(model_multi_med$VCV[,1]*model_multi_med$VCV[,9]) #AFR and IMC
gencorr_3 <- model_multi_med$VCV[,6]/sqrt(model_multi_med$VCV[,5]*model_multi_med$VCV[,9]) #fitness and IMC

#high
gencorr_1 <- model_multi_high$VCV[,2]/sqrt(model_multi_high$VCV[,1]*model_multi_high$VCV[,5]) #AFR and fitness
gencorr_2 <- model_multi_high$VCV[,3]/sqrt(model_multi_high$VCV[,1]*model_multi_high$VCV[,9]) #AFR and IMC
gencorr_3 <- model_multi_high$VCV[,6]/sqrt(model_multi_high$VCV[,5]*model_multi_high$VCV[,9]) #fitness and IMC

posterior.mode(gencorr_1)
HPDinterval(gencorr_1)

herit_1 <- model_multi_high$VCV[,1]/(model_multi_high$VCV[,1]+model_multi_high$VCV[,10]+model_multi_high$VCV[,19])
herit_2 <- model_multi_high$VCV[,5]/(model_multi_high$VCV[,5]+model_multi_high$VCV[,14]+model_multi_high$VCV[,23])
herit_3 <- model_multi_high$VCV[,9]/(model_multi_high$VCV[,9]+model_multi_high$VCV[,18]+model_multi_high$VCV[,27]+log(1+1/exp(model_multi_high$Sol[,3])))

herit_latent <- model_multi_high$VCV[,9]/(model_multi_high$VCV[,9]+model_multi_high$VCV[,18]+model_multi_high$VCV[,27])

posterior.mode(herit_3)
HPDinterval(herit_3)

posterior.mode(herit_latent)
HPDinterval(herit_latent)
##################by period#######################
load(file = "C:/Users/walid/Desktop/Work/DataManp/Data/model_multi_per1.rda")
load(file = "C:/Users/walid/Desktop/Work/DataManp/Data/model_multi_per2.rda")
colnames(model_multi_per2$VCV)

G_multi_per2 <- matrix(
  c(posterior.mode(model_multi_per2$VCV[,1:9])),
  nrow =3, 
  ncol=3
)

colnames(G_multi_per2) <- c("AFR", "fitdem","OffMortality") 

posterior.mode(model_multi_per2$VCV[,1])
HPDinterval(model_multi_per2$VCV[,1])
posterior.mode(model_multi_per2$VCV[,5])
HPDinterval(model_multi_per2$VCV[,5])
posterior.mode(model_multi_per2$VCV[,9])
HPDinterval(model_multi_per2$VCV[,9])

gencorr_1 <- model_multi_per2$VCV[,2]/sqrt(model_multi_per2$VCV[,1]*model_multi_per2$VCV[,5])#AFR & fitness
gencorr_2 <- model_multi_per2$VCV[,3]/sqrt(model_multi_per2$VCV[,1]*model_multi_per2$VCV[,9])#AFR & offmortality
gencorr_3 <- model_multi_per2$VCV[,6]/sqrt(model_multi_per2$VCV[,5]*model_multi_per2$VCV[,9])#fitness & offmortality

posterior.mode(gencorr_3)
HPDinterval(gencorr_3)

herit_1 <- model_multi_per2$VCV[,1]/(model_multi_per2$VCV[,1]+model_multi_per2$VCV[,10]+model_multi_per2$VCV[,19])
herit_2 <- model_multi_per2$VCV[,5]/(model_multi_per2$VCV[,5]+model_multi_per2$VCV[,14]+model_multi_per2$VCV[,23])
herit_3 <- model_multi_per2$VCV[,9]/(model_multi_per2$VCV[,9]+model_multi_per2$VCV[,18]+model_multi_per2$VCV[,27]+log(1+1/exp(model_multi_per2$Sol[,3])))

herit_latent <- model_multi_per2$VCV[,9]/(model_multi_per2$VCV[,9]+model_multi_per2$VCV[,18]+model_multi_per2$VCV[,27])

posterior.mode(herit_3)
HPDinterval(herit_3)

posterior.mode(herit_latent)
HPDinterval(herit_latent)
############ Genetic correlation matrices############################
####Matrices
#Low
gencorr_low <- matrix(c(1,posterior.mode(gencorr_1),
                        posterior.mode(gencorr_2),posterior.mode(gencorr_1),1,
                        posterior.mode(gencorr_3),posterior.mode(gencorr_2),
                        posterior.mode(gencorr_3),1),nrow =3, ncol=3)

colnames(gencorr_low) <- c("AFR", "Fitness","IM") 
rownames(gencorr_low) <- c("AFR", "Fitness","IM")
#Middle
gencorr_med <- matrix(c(1,posterior.mode(gencorr_1),
                        posterior.mode(gencorr_2),posterior.mode(gencorr_1),1,
                        posterior.mode(gencorr_3),posterior.mode(gencorr_2),
                        posterior.mode(gencorr_3),1),nrow =3, ncol=3)

colnames(gencorr_med) <- c("AFR", "Fitness","IM") 
rownames(gencorr_med) <- c("AFR", "Fitness","IM")
#High
gencorr_high <- matrix(c(1,posterior.mode(gencorr_1),
                        posterior.mode(gencorr_2),posterior.mode(gencorr_1),1,
                        posterior.mode(gencorr_3),posterior.mode(gencorr_2),
                        posterior.mode(gencorr_3),1),nrow =3, ncol=3)

colnames(gencorr_high) <- c("AFR", "Fitness","IM") 
rownames(gencorr_high) <- c("AFR", "Fitness","IM")

get_lower_tri <- function(cormat){
  cormat[upper.tri(cormat)]<- NA
  return(cormat)
}

lower_melted_low <- get_lower_tri(gencorr_low)
lower_melted_med <- get_lower_tri(gencorr_med)
lower_melted_high <- get_lower_tri(gencorr_high)
#Period 1
gencorr_per1 <- matrix(c(1,posterior.mode(gencorr_1),
                        posterior.mode(gencorr_2),posterior.mode(gencorr_1),1,
                        posterior.mode(gencorr_3),posterior.mode(gencorr_2),
                        posterior.mode(gencorr_3),1),nrow =3, ncol=3)

colnames(gencorr_per1) <- c("AFR", "Fitness","IM") 
rownames(gencorr_per1) <- c("AFR", "Fitness","IM")
#Period 2
gencorr_per2<- matrix(c(1,posterior.mode(gencorr_1),
                        posterior.mode(gencorr_2),posterior.mode(gencorr_1),1,
                        posterior.mode(gencorr_3),posterior.mode(gencorr_2),
                        posterior.mode(gencorr_3),1),nrow =3, ncol=3)

colnames(gencorr_per2) <- c("AFR", "Fitness","IM") 
rownames(gencorr_per2) <- c("AFR", "Fitness","IM")

lower_melted_per1 <- get_lower_tri(gencorr_per1)
lower_melted_per2 <- get_lower_tri(gencorr_per2)
###Melting
library(reshape2)
#by condition
melted_gencorr <- rbind(melt(lower_melted_low, na.rm = TRUE),
                        melt(lower_melted_med, na.rm = TRUE),
                        melt(lower_melted_high, na.rm = TRUE))

melted_gencorr$cond <- c(rep("Low",6),rep("Middle",6),rep("High",6))
melted_gencorr$cond <- factor(melted_gencorr$cond, levels = c("Low", "Middle","High"))

#by cohort period
melted_gencorr <- rbind(melt(lower_melted_per1, na.rm = TRUE),
                        melt(lower_melted_per2, na.rm = TRUE))

melted_gencorr$cond <- c(rep("Period 1",6),rep("Period 2",6))
melted_gencorr$cond <- factor(melted_gencorr$cond, levels = c("Period 1", "Period 2"))

# Heatmap
library(ggplot2)
tiff("test13.tiff", units="in", width=5, height=5, res=300)
ggplot(data = melted_gencorr, aes(Var2, Var1, fill = value))+
  geom_tile(color = "black",)+facet_grid(cond ~ .)+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-0.75,1), space = "Lab", 
                       name="Genetic\nCorrelation") +
  theme_classic()+ labs(x= "Trait 1", y="Trait 2")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
dev.off()
  
ggcorrplot(gencorr_low, hc.order = TRUE, type = "lower",
             outline.col = "black")
#####################matrix visualisation##############################
library(reshape2)
library(ggplot2)

longData<-A
longData<-longData[longData$value!=0,]

ggplot(longData, aes(x = Var2, y = Var1)) + 
  geom_raster(aes(fill=value)) + 
  scale_fill_gradient(low="yellow", high="red") +
  labs(x="letters", y="LETTERS", title="Matrix") +
  theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                     axis.text.y=element_text(size=9),
                     plot.title=element_text(size=11))
#random regression
load(file = "C:/Users/walid/OneDrive/Bureau/Work/DataManp/model_afr_fit_rev_ram_2.rda")
#load(file = "C:/Users/walid/OneDrive/Bureau/Work/DataManp/model_afr_fit_rev_ram_2_cens.rda")
G_ram <- matrix(
  c(posterior.mode(model_afr_fit_rev_ram_2$VCV[,1:16])),
  nrow =4, 
  ncol=4
)

G_ram_afr<-matrix(
  c(posterior.mode(model_afr_fit_rev_ram_2$VCV[,c(1,9,3,11)])),
  nrow =2, 
  ncol=2
)

G <- (matrix(dat$Mortrate_3y[1:2]) %*% G_ram_afr) %*% t(matrix(dat$Mortrate_3y))


G_ram_2 <- matrix(
  c(posterior.mode(model_afr_fit_rev_ram_2_cens$VCV[,1:16])),
  nrow =4, 
  ncol=4
)

dat_sum_full <- summarySE(dat, measurevar="AFR", groupvars=c("Mortrate_3y"))
dat_sum_cens <- summarySE(test, measurevar="AFR", groupvars=c("Mortrate_3y"))

df_mcmc_cors_ram <- data.frame(Term = as.factor(c("Intercept", "Slope")),
                           Estimate = c(posterior.mode(model_afr_fit_rev_ram_2$VCV[,2]),
                                        posterior.mode(model_afr_fit_rev_ram_2$VCV[,15])),
                           Lower = c(HPDinterval(model_afr_fit_rev_ram_2$VCV[,2])[,"lower"],
                                     HPDinterval(model_afr_fit_rev_ram_2$VCV[,15])[,"lower"]),
                           Upper = c(HPDinterval(model_afr_fit_rev_ram_2$VCV[,2])[,"upper"],
                                     HPDinterval(model_afr_fit_rev_ram_2$VCV[,15])[,"upper"]))



tiff("corr_visual_ram.tiff", units="in", width=5, height=5, res=300)
p_ram <- ggplot(df_mcmc_cors_ram, aes(x = Term, y = Estimate)) +
  geom_pointrange(aes(ymin = Lower,
                      ymax = Upper)) +
  geom_hline(yintercept = 0,
             linetype = "dotted",alpha = 0.3) +
  scale_x_discrete(limits = c("Intercept", "Slope")) +
  labs(x = "Paramter term",
       y = "Genetic covariance (Estimate +/- 95% CIs)") +
  ylim(-.3,.05) +
  coord_flip() +
  theme_Publication()+scale_colour_Publication()
p_ram <- p_ram+theme(plot.title = element_blank(),
                     axis.title.x = element_blank(),
                     axis.title.y = element_blank(),
                     text = element_text(size=8),
                     axis.text.y = element_text(angle=90, hjust=.5))
p_ram
dev.off()

#regression based on parameters from LM model
library(effects)
library(Rmisc)
#AFR by mortrate continous
ef <- effect("Mortrate_3y", model, xlevels = list(AFR = seq(18,35,1), Mortrate_3y=seq(0,0.3,0.01)))
efdata <- as.data.frame(ef)
head(efdata)
ef_2 <- effect("Mortrate_3y", model_cens, xlevels = list(AFR = seq(18,35,1), Mortrate_3y=seq(0.01,0.25,0.01)))
efdata_2 <- as.data.frame(ef_2)
head(efdata_2)


plot_ram <- ggplot(data=dat_sum_full, aes(x=Mortrate_3y, y=AFR))+
  geom_point(alpha=0.1)+geom_errorbar(data = dat_sum_full,aes(ymin=AFR-se, ymax=AFR+se),size=0.1, width=0, alpha=0.1)+
  geom_line(data = efdata,aes(x = Mortrate_3y, y = fit),size=1.5, colour = "black")+
  geom_vline(xintercept = mean(dat$Mortrate_3y), linetype = "dotted")+geom_vline(xintercept = c(0.06700, 0.12925), linetype="dashed")+geom_hline(yintercept = mean(dat$AFR), linetype = "dotted")+
  labs(x= "IMR", y="Average AFR", size =1)+annotate("text", x= 0.28, y=29, label = )
  theme_Publication()+scale_colour_Publication()+ylim(19,30)

plot_ram_cens <- ggplot(data=dat_sum_cens, aes(x=Mortrate_3y, y=AFR))+
  geom_point(alpha=0.1)+geom_errorbar(data = dat_sum_cens,aes(ymin=AFR-se, ymax=AFR+se),size=0.1, width=0, alpha=0.1)+
  geom_line(data = efdata_2,aes(x = Mortrate_3y, y = fit),size=1.5, colour = "black")+
  geom_vline(xintercept = mean(test$Mortrate_3y), linetype = "dotted")+geom_vline(xintercept = c(0.05700, 0.12825), linetype="dashed")+geom_hline(yintercept = mean(test$AFR), linetype = "dotted")+
  labs(x= "IMR", y="Average AFR", size =1)+
  theme_Publication()+scale_colour_Publication()+ylim(19,30)

#Variance from MCMCglmm
x <- 0.1829189*scale(dat$Mortrate_3y)+0.3189580 
addvar <- data.frame(IMR=dat$Mortrate_3y, Var=x)
x_2 <- 0.2132131*scale(test$Mortrate_3y)+0.3460306 
addvar_2 <- data.frame(IMR=test$Mortrate_3y, Var=x_2)
#
plot_ram_mcmc <- ggplot()+ 
  geom_line(data = addvar,aes(x = IMR, y = Var),size=1.5, colour = "black")+
  labs(y="Additive variance in AFR",x="IMR")+geom_vline(xintercept = c(0.05700, 0.12825), linetype="dashed")+
  scale_y_continuous(limits = c(0,1))+
  theme_Publication()+scale_colour_Publication()

plot_ram_censr_mcmc <- ggplot()+ 
  geom_line(data = addvar_2,aes(x = IMR, y = Var),size=1.5, colour = "black")+
  labs(y="Additive variance in AFR",x="IMR")+geom_vline(xintercept = c(0.06700, 0.12925), linetype="dashed")+
  scale_y_continuous(limits = c(0,1))+
  theme_Publication()+scale_colour_Publication()


tiff("plot_IMR_variance.tiff", units="in", width=8, height=8, res=300)
ggarrange(plot_ram,plot_ram_cens, plot_ram_mcmc,plot_ram_censr_mcmc,labels = c("A","B","C", "D"),ncol = 2, nrow = 2, ) 
dev.off()


############################Genetic Correlation visual##########################
load(file = "C:/Users/walid/OneDrive/Bureau/Work/DataManp/model_afr_fit_rev_2.rda")


gencorr_1 <- model_afr_fit_rev_2$VCV[,2]/sqrt(model_afr_fit_rev_2$VCV[,1]*model_afr_fit_rev_2$VCV[,8]) #Low
gencorr_2 <- model_afr_fit_rev_2$VCV[,16]/sqrt(model_afr_fit_rev_2$VCV[,15]*model_afr_fit_rev_2$VCV[,22]) #Middle
gencorr_3 <- model_afr_fit_rev_2$VCV[,30]/sqrt(model_afr_fit_rev_2$VCV[,29]*model_afr_fit_rev_2$VCV[,36]) #High

gencorr_env1 <- model_afr_fit_rev_2$VCV[,3]/sqrt(model_afr_fit_rev_2$VCV[,1]*model_afr_fit_rev_2$VCV[,15]) #Low:Middle
gencorr_env2 <- model_afr_fit_rev_2$VCV[,5]/sqrt(model_afr_fit_rev_2$VCV[,1]*model_afr_fit_rev_2$VCV[,29]) #Low:High
gencorr_env3 <- model_afr_fit_rev_2$VCV[,17]/sqrt(model_afr_fit_rev_2$VCV[,15]*model_afr_fit_rev_2$VCV[,29]) #Middle:High

gencorr_env1_f <- model_afr_fit_rev_2$VCV[,10]/sqrt(model_afr_fit_rev_2$VCV[,8]*model_afr_fit_rev_2$VCV[,22]) #Low:Middle
gencorr_env2_f <- model_afr_fit_rev_2$VCV[,12]/sqrt(model_afr_fit_rev_2$VCV[,8]*model_afr_fit_rev_2$VCV[,36]) #Low:High
gencorr_env3_f <- model_afr_fit_rev_2$VCV[,24]/sqrt(model_afr_fit_rev_2$VCV[,22]*model_afr_fit_rev_2$VCV[,36])#Middle:High

df_mcmc_cors <- data.frame(Condition = c("Low","Middle","High"),
                           Estimate = c(posterior.mode(model_afr_fit_rev_2$VCV[,2]),
                                        posterior.mode(model_afr_fit_rev_2$VCV[,16]),
                                        posterior.mode(model_afr_fit_rev_2$VCV[,30])),
                           Lower = c(HPDinterval(model_afr_fit_rev_2$VCV[,2])[,"lower"],
                                     HPDinterval(model_afr_fit_rev_2$VCV[,16])[,"lower"],
                                     HPDinterval(model_afr_fit_rev_2$VCV[,30])[,"lower"]),
                           Upper = c(HPDinterval(model_afr_fit_rev_2$VCV[,2])[,"upper"],
                                     HPDinterval(model_afr_fit_rev_2$VCV[,16])[,"upper"],
                                     HPDinterval(model_afr_fit_rev_2$VCV[,30])[,"upper"]))
df_mcmc_cors_env <- data.frame(Envcomb = c("Low, Middle",
                                           "Low, High",
                                           "Middle, High",
                                           "Low, Middle",
                                           "Low, High",
                                           "Middle, High"),
                               Estimate = c(posterior.mode(gencorr_env1),
                                            posterior.mode(gencorr_env2),
                                            posterior.mode(gencorr_env3),
                                            posterior.mode(gencorr_env1_f),
                                            posterior.mode(gencorr_env2_f),
                                            posterior.mode(gencorr_env3_f)),
                               Lower = c(HPDinterval(gencorr_env1)[,"lower"],
                                         HPDinterval(gencorr_env2)[,"lower"],
                                         HPDinterval(gencorr_env3)[,"lower"],
                                         HPDinterval(gencorr_env1_f)[,"lower"],
                                         HPDinterval(gencorr_env2_f)[,"lower"],
                                         HPDinterval(gencorr_env3_f)[,"lower"]),
                               Upper = c(HPDinterval(gencorr_env1)[,"upper"],
                                         HPDinterval(gencorr_env2)[,"upper"],
                                         HPDinterval(gencorr_env3)[,"upper"],
                                         HPDinterval(gencorr_env1_f)[,"upper"],
                                         HPDinterval(gencorr_env2_f)[,"upper"],
                                         HPDinterval(gencorr_env3_f)[,"upper"]),
                               Traits = c(rep("AFR",3), rep("Fitness",3)))


levels(df_mcmc_cors$Condition) <- c("Low", "Middle", "High")

p_g <- ggplot(df_mcmc_cors, aes(x = Condition, y = Estimate, color=Condition)) +
        geom_pointrange(aes(ymin = Lower,
                      ymax = Upper)) +
        geom_hline(yintercept = 0,
             linetype = "dotted",alpha = 0.3) +
        scale_x_discrete(limits = c("Low",
                              "Middle",
                              "High")) +
        labs(x = "IMR group",
        y = "Genetic covariance (Estimate +/- 95% CIs)", color = "IMR group") +
        ylim(-.6,.6) +
        coord_flip() +
        theme_Publication()+scale_colour_Publication()

p_g <- p_g + annotation_custom(ggplotGrob(p_ram), ymin = 0.1, ymax = 0.6)


p_env <- ggplot(df_mcmc_cors_env, aes(x = Envcomb, y = Estimate, color=Envcomb)) +
  geom_pointrange(aes(ymin = Lower,
                      ymax = Upper)) +
  geom_hline(yintercept = 0,
             linetype = "dotted",alpha = 0.3) +facet_wrap(.~Traits)+
  scale_x_discrete(limits = c("Low, Middle",
                              "Low, High",
                              "Middle, High")) +
  labs(x = "Environment combination",
       y = "Cross-env genetic correlation (Estimate +/- 95% CIs)", color = " ") +
  ylim(-.5,1) +
  coord_flip() +
  theme_Publication()+scale_colour_Publication()


tiff("corr_visual_2.tiff", units="in", width=7, height=8, res=300)
ggarrange(p_g,p_env,labels = c("A","B"),ncol = 1, nrow = 2, legend = "none",common.legend = F)
dev.off()
########################################################################
load(file = "C:/Users/walid/OneDrive/Bureau/Work/DataManp/model_afr_fit_rev.rda")
library(MCMCglmm)
colnames(model_afr_fit_rev$VCV)

G_afr_fit <- matrix(
  c(posterior.mode(model_afr_fit_rev$VCV[,1:4])),
  nrow =2, 
  ncol=2
)

colnames(G_afr_fit) <- c("AFR", "fitdem") 

posterior.mode(model_afr_fit$VCV[,1])
HPDinterval(model_afr_fit$VCV[,1])
posterior.mode(model_afr_fit$VCV[,4])
HPDinterval(model_afr_fit$VCV[,4])

gencorr_1 <- model_afr_fit$VCV[,2]/sqrt(model_afr_fit$VCV[,1]*model_afr_fit$VCV[,4]) #AFR and fitdem

posterior.mode(gencorr_1)
HPDinterval(gencorr_1)

herit_1 <- model_afr_fit$VCV[,1]/(model_afr_fit$VCV[,1]+model_afr_fit$VCV[,5]+model_afr_fit$VCV[,9])
herit_2 <- model_afr_fit$VCV[,4]/(model_afr_fit$VCV[,4]+model_afr_fit$VCV[,8]+model_afr_fit$VCV[,12])

posterior.mode(herit_2)
HPDinterval(herit_2)

#####by env#######
load(file = "C:/Users/walid/Desktop/Work/DataManp/model_afr_fit_low.rda")
load(file = "C:/Users/walid/Desktop/Work/DataManp/model_afr_fit_med.rda")
load(file = "C:/Users/walid/Desktop/Work/DataManp/model_afr_fit_high.rda")
colnames(model_afr_fit_high$VCV)

G_afr_fit_high <- matrix(
  c(posterior.mode(model_afr_fit_high$VCV[,1:4])),
  nrow =2, 
  ncol=2
)

colnames(G_afr_fit_high) <- c("AFR", "fitdem") 

posterior.mode(model_afr_fit_high$VCV[,1])
HPDinterval(model_afr_fit_high$VCV[,1])
posterior.mode(model_afr_fit_high$VCV[,4])
HPDinterval(model_afr_fit_high$VCV[,4])

gencorr_1 <- model_afr_fit_high$VCV[,2]/sqrt(model_afr_fit_high$VCV[,1]*model_afr_fit_high$VCV[,4]) #AFR and LRS

posterior.mode(gencorr_1)
HPDinterval(gencorr_1)

herit_1 <- model_afr_fit_high$VCV[,1]/(model_afr_fit_high$VCV[,1]+model_afr_fit_high$VCV[,5]+model_afr_fit_high$VCV[,9])
herit_2 <- model_afr_fit_high$VCV[,4]/(model_afr_fit_high$VCV[,4]+model_afr_fit_high$VCV[,8]+model_afr_fit_high$VCV[,12])

posterior.mode(herit_2)
HPDinterval(herit_2)
##################by period#######################
load(file = "C:/Users/walid/OneDrive/Bureau/Work/DataManp/model_multi_env.rda")

colnames(model_multi_env$VCV)

G_multi_env <- matrix(
  c(posterior.mode(model_multi_env$VCV[,1:4])),
  nrow =2, 
  ncol=2
)

colnames(G_multi_env) <- c("AFR", "fitdem") 

posterior.mode(model_multi_env$VCV[,1])
HPDinterval(model_multi_env$VCV[,1])
posterior.mode(model_multi_env$VCV[,4])
HPDinterval(model_multi_env$VCV[,4])

gencorr_1 <- model_afr_fit_3$VCV[,2]/sqrt(model_afr_fit_3$VCV[,1]*model_afr_fit_3$VCV[,8])#AFR & LRS
gencorr_2 <- model_afr_fit_3$VCV[,16]/sqrt(model_afr_fit_3$VCV[,15]*model_afr_fit_3$VCV[,22])#AFR & LRS
gencorr_3 <- model_afr_fit_3$VCV[,30]/sqrt(model_afr_fit_3$VCV[,29]*model_afr_fit_3$VCV[,36])#AFR & LRS

posterior.mode(gencorr_3)
HPDinterval(gencorr_3)
#AFR
herit_1 <- model_afr_fit_3$VCV[,1]/(model_afr_fit_3$VCV[,1]+model_afr_fit_3$VCV[,37]+model_afr_fit_3$VCV[,73])
herit_2 <- model_afr_fit_3$VCV[,15]/(model_afr_fit_3$VCV[,15]+model_afr_fit_3$VCV[,51]+model_afr_fit_3$VCV[,87])
herit_3 <- model_afr_fit_3$VCV[,29]/(model_afr_fit_3$VCV[,29]+model_afr_fit_3$VCV[,65]+model_afr_fit_3$VCV[,101])
#fitness
herit_1 <- model_afr_fit_3$VCV[,8]/(model_afr_fit_3$VCV[,8]+model_afr_fit_3$VCV[,44]+model_afr_fit_3$VCV[,80])
herit_2 <- model_afr_fit_3$VCV[,22]/(model_afr_fit_3$VCV[,22]+model_afr_fit_3$VCV[,58]+model_afr_fit_3$VCV[,94])
herit_3 <- model_afr_fit_3$VCV[,36]/(model_afr_fit_3$VCV[,36]+model_afr_fit_3$VCV[,72]+model_afr_fit_3$VCV[,108])

posterior.mode(herit_3)
HPDinterval(herit_3)
########################################
load(file = "C:/Users/walid/OneDrive/Bureau/Work/DataManp/model_afr_3.rda")
library(MCMCglmm)
colnames(model_afr_3$VCV)

G_afr_3 <- matrix(
  c(posterior.mode(model_afr_3$VCV[,1:4])),
  nrow =2, 
  ncol=2
)

colnames(G_afr_3) <- c("AFR", "3dem") 

posterior.mode(model_afr_3$VCV[,1])
HPDinterval(model_afr_3$VCV[,1])
posterior.mode(model_afr_3$VCV[,4])
HPDinterval(model_afr_3$VCV[,4])

gencorr_1 <- model_afr_3$VCV[,2]/sqrt(model_afr_3$VCV[,1]*model_afr_3$VCV[,4]) #AFR and 3dem

posterior.mode(gencorr_1)
HPDinterval(gencorr_1)

herit_1 <- model_afr_3$VCV[,1]/(model_afr_3$VCV[,1]+model_afr_3$VCV[,4]+model_afr_3$VCV[,7])
herit_2 <- model_afr_3$VCV[,2]/(model_afr_3$VCV[,2]+model_afr_3$VCV[,5]+model_afr_3$VCV[,8])
herit_3 <- model_afr_3$VCV[,3]/(model_afr_3$VCV[,3]+model_afr_3$VCV[,6]+model_afr_3$VCV[,9])

posterior.mode(herit_2)
HPDinterval(herit_2)

IAC$per<-NULL
for (i in 1:nrow(IAC)){
  IAC[IAC$COHORT %in% seq(1,4,1),i]$per<-1
  IAC[IAC$COHORT %in% seq(5,8,1),i]$per<-2
}
#################diagnosing posterior distributions of genetic cov#############
nrow(cov_mcmc[which(cov_mcmc$cov_high < cov_mcmc$cov_low),])/1000 *100 # 70.4%
nrow(cov_mcmc[which(cov_mcmc$cov_high < cov_mcmc$cov_middle),])/1000 *100 # 63.7 %
nrow(cov_mcmc[which(cov_mcmc$cov_high < cov_mcmc$cov_middle & cov_mcmc$cov_high < cov_mcmc$cov_low),])/1000 *100 # 54.4 %

mean(cov_comp$cov_high-cov_comp$cov_low) # average difference of -0.2275095
mean(cov_comp$cov_high-cov_comp$cov_middle) # average difference of -0.1981771

View(cov_mcmc)

cov_mcmc$va_AFR_low <- as.vector(model_afr_fit_rev$VCV[,15])
cov_mcmc$va_AFR_middle <- as.vector(model_afr_fit_rev$VCV[,29])
cov_mcmc$va_AFR_high <- as.vector(model_afr_fit_rev$VCV[,1])

cov_mcmc$va_fit_low <- as.vector(model_afr_fit_rev$VCV[,22])
cov_mcmc$va_fit_middle <- as.vector(model_afr_fit_rev$VCV[,36])
cov_mcmc$va_fit_high <- as.vector(model_afr_fit_rev$VCV[,8])

cov_comp <- cov_mcmc[which(cov_mcmc$cov_high < cov_mcmc$cov_middle & cov_mcmc$cov_high < cov_mcmc$cov_low),]
cov_comp_1 <- cov_mcmc[which(cov_mcmc$cov_high < cov_mcmc$cov_low),]


nrow(cov_comp[which(cov_comp$va_AFR_high > cov_comp$va_AFR_middle & cov_comp$va_AFR_high > cov_comp$va_AFR_low),])
nrow(cov_comp[which(cov_comp$va_fit_high > cov_comp$va_fit_middle & cov_comp$va_fit_high > cov_comp$va_fit_low),])

nrow(cov_comp_1[which(cov_comp_1$va_AFR_high > cov_comp_1$va_AFR_low),])
nrow(cov_comp_1[which(cov_comp_1$va_fit_high > cov_comp_1$va_fit_low),])
############################################################
#bayestest

bayesttest_var_afr <- function(posterior, var=0.1, threshold=0.1){
  describe_posterior(posterior,
                     centrality = "MAP",
                     ci = 0.95,
                     ci_method = "HDI",
                     test = c("pd", "p_map","rope","equitest"),
                     rope_range = c(-(var*threshold),(var*threshold)),
                     rope_ci = 0.95)
}

bayesttest_var_fit <- function(posterior,var=0.1, threshold=0.1){
  describe_posterior(posterior,
                     centrality = "MAP",
                     ci = 0.95,
                     ci_method = "HDI",
                     test = c("pd", "p_map","rope","equitest"),
                     rope_range = c(-(var*threshold),(var*threshold)),
                     rope_ci = 0.95)
}

bayesttest_covar <- function(posterior,var=0.01, threshold=0.1){
  describe_posterior(posterior,
                     centrality = "MAP",
                     ci = 0.95,
                     ci_method = "HDI",
                     test = c("pd", "p_map","rope","equitest"),
                     rope_range = c(-(var*threshold),(var*threshold)),
                     rope_ci = 0.95)
}

bayesttest_slope <- function(posterior,var=0.1, threshold=0.1){
  describe_posterior(posterior,
                     centrality = "MAP",
                     ci = 0.95,
                     ci_method = "HDI",
                     test = c("pd", "p_map","rope","equitest"),
                     rope_range = c(-(var*threshold),(var*threshold)),
                     rope_ci = 0.95)
}

bayesttest_var_afr(model_afr_fit_rev_2$VCV[,c(1,15,29)], var=0.37, threshold = 0.05)
bayesttest_var_fit(model_afr_fit_rev_2$VCV[,c(8,22,36)], var=0.20, threshold = 0.05)
bayesttest_covar(model_afr_fit_rev_ram_2$VCV[,c(2,15)], var=0.07, threshold = 0.25)

bayesttest_slope((model_afr_fit_rev_2$VCV[,2]/model_afr_fit_rev_2$VCV[,1]), var=0.17, threshold = 0.10)

write.table(x =bayesttest_var(model_afr_fit_rev_2$VCV[,c(1,6,11,16)]), file = "Bayestest_results.txt")
write.table(x =bayesttest_covar(model_afr_fit_rev_2$VCV[,c(2,12)]), file = "Bayestest_results_co.txt")


