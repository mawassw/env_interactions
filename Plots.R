#######set preferred theme########
theme_Publication <- function(base_size=14, base_family="Cambria") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size= unit(0.2, "cm"),
            legend.spacing = unit(0, "cm"),
            legend.title = element_text(face="italic"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
    ))
  
}

scale_fill_Publication <- function(...){
  library(scales)
  discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

scale_colour_Publication <- function(...){
  library(scales)
  discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

library(ggplot2)
library(gridExtra)
library(ggpubr)
library(ggridges)
library(plyr)
library(corrplot)
theme_set(theme_Publication())
####################Plots###########################
#correlation between yearly mortality rate and other environmental variables
mydata.corr <- rcorr(as.matrix(mean.var))

tiff("testcorr.tiff", units="in", width=5, height=5, res=300)
corrplot(mydata.corr$r)
dev.off()
#comparing traits in subenv

box1 <- ggplot(dat, aes(x=mortrate4, y=AFR, fill= mortrate4)) + 
    geom_boxplot(outlier.shape=NA) + #avoid plotting outliers twice
    geom_jitter(size=1,position=position_jitter(width=.1, height=0))+labs(fill='') +ylab("AFR")+xlab("IMR group")+theme_Publication()+scale_fill_Publication()
box2 <- ggplot(dat, aes(x=mortrate4, y=fitdem,fill= mortrate4)) + 
  geom_boxplot(outlier.shape=NA) + #avoid plotting outliers twice
  geom_jitter(size=1,position=position_jitter(width=.1, height=0))+labs(fill='')+ylab("Relative fitness")+xlab("IMR group")+theme_Publication()+scale_fill_Publication()
box3 <- ggplot(dat, aes(x=mortrate4, y=OffMortality,fill= mortrate4)) + 
  geom_boxplot(outlier.shape=NA) + #avoid plotting outliers twice
  geom_jitter(size=1,position=position_jitter(width=.1, height=0))+labs(fill='')+ylab("Infant mortality")+xlab("IMR group")+theme_Publication()+scale_fill_Publication()
box4 <- ggplot(dat, aes(x=mortrate4, y=FERTILITY, fill= mortrate4)) + 
  geom_boxplot(outlier.shape=NA) + #avoid plotting outliers twice
  geom_jitter(size=1,position=position_jitter(width=.1, height=0))+labs(fill='') +ylab("Fertility")+xlab("IMR group")+theme_Publication()+scale_fill_Publication()

tiff("test10_rev.tiff", units="in", width=7, height=7, res=300)
ggarrange(box1,box4, box3,box2,labels = c("A","B","C", "D"),ncol = 2, nrow = 2, legend = "bottom", common.legend = TRUE)
dev.off()
#remove outlier in AFR (very early and late AFRs)
test <- IAC[IAC$AFR >= 18 & IAC$AFR <= 35,]
#A - ploting change in AFR due to early-life infant mortality rate
ggplot(dat, aes(x=Mortrate_3y)) +
  geom_histogram( fill="white", position = "dodge", binwidth = 0.01) +geom_density(alpha=.2, fill="#FF6666")

##histogram of proportions##

df.new <- ddply(dat, .(mortrate4), summarise,
                prop=as.numeric(round(prop.table(table(COHORTW)),digits = 2)),
                cohort=names(table(COHORTW)))

library(DataCombine)
df.new <- InsertRow(df.new,NewRow = c("Low",0,7),RowNum = 7)
df.new$mortrate1 <- factor(df.new$mortrate1, levels = c("Low", "Middle","High"))
df.new$cohort <- as.factor(df.new$cohort)

tiff("test5_rev.tiff", units="in", width=5, height=5, res=300)
p <- ggplot(df.new,aes(cohort,prop,fill=mortrate4))+
  geom_bar(stat = "identity",position="dodge")+
  labs(x = "Woman's birth cohort",y="Proportion of women", fill="IMR group")+
  scale_fill_Publication()
p
dev.off()
###histogram of densities###

plot_histogram <- function(df, feature) {
  plt <- ggplot(df, aes(x=eval(parse(text=feature)))) +
    geom_histogram(aes(y = ..density..),binwidth = 0.01, alpha=0.4, fill="blue",position="identity", color="black") +
    geom_density(data=dat, aes(x=mortrate_04y),alpha=0.3 ,fill="#FF6666") + 
    geom_vline(aes(xintercept=0.051), color="black", linetype="dashed", size=1) +
    geom_vline(aes(xintercept=0.128), color="black", linetype="dashed", size=1) +
    annotate("text", x = c(0.03,0.11,0.2), y = 9.5, label = c("Low","Middle","High"), parse = TRUE) +
    annotate("text", x = c(0.03,0.11,0.2), y = 9, label = c("italic(N)== 142","italic(N)== 285","italic(N)== 145"), parse = TRUE) +
    labs(x="Infant mortality rate", y = "Density", color="Birth cohort")+ theme(legend.position="top")+scale_colour_Publication()
  print(plt)
}

tiff("test6_rev.tiff", units="in", width=6, height=5, res=300)
plot_histogram(dat, "mortrate_04y")
dev.off()
####
#mortality rate mean change with year
y <- aggregate(dat[,"Mortrate_3y"], list("BIRTHY" = dat[,5]), mean)
colnames(y)[2] <-  "Mortrate"

Scatter <- ggplot(y, aes(x = BIRTHY,y=Mortrate)) + geom_smooth(method = lm, se = FALSE, col = "red", size =1)+geom_point(size=2) + xlab("Woman's birth year")+ylab("Mean infant mortality rate")+labs(title="Mean infant mortality rate by woman's birth year") +scale_colour_Publication()+ theme_Publication()
Scatter

#mortality rate mean change by cohort
y <- aggregate(dat[,"mortrate_04y"], list("Cohort" = dat[,6]), mean)
colnames(y)[2] <-  "Mortrate"

Scatter <- ggplot(y, aes(x = Cohort,y=Mortrate)) + geom_smooth(method = lm, se = FALSE, col = "black", size = 1)+geom_point(size=2) + xlab("Woman's birth cohort")+ylab("Average IMR") +scale_colour_Publication()+ theme_Publication()
Scatter #A

y <- aggregate(temp[,"AFR"], list("Cohort" = temp[,"COHORTW"], "mortrate1" = temp[,"mortrate1"]), mean)
colnames(y)[3] <-  "AFR"

Scatter <- ggplot(y, aes(x = Cohort,y=AFR, color = mortrate1, shape = mortrate1)) + geom_smooth(method = lm, se = FALSE, fullrange = TRUE)+geom_point(size=2) + xlab("Woman's cohort")+ylab("AFR") +scale_colour_Publication()+ theme_Publication()
Scatter <- ggplot(y, aes(x = Cohort,y=AFR, color = mortrate1, shape = mortrate1)) + geom_point(size =2)+ xlab("Woman's cohort")+ylab("AFR") +scale_colour_Publication()+ theme_Publication()

Scatter

ggplot(temp, aes(x=COHORTW, y=AFR, color = mortrate1, shape = mortrate1)) +
  geom_point() +
  geom_smooth(method = lm, se=FALSE, fullrange = TRUE) +
  theme_classic()
#mortality rate variance change by cohort
y <- aggregate(dat[,"mortrate_04y"], list("Cohort" = dat[,6]), var)
colnames(y)[2] <-  "Mortrate"
y$Mortrate_scaled = y$Mortrate

Scatter_1 <- ggplot(y, aes(x = Cohort,y=Mortrate_scaled)) + geom_smooth(method = lm, se = FALSE, col = "black", size = 1)+geom_point(size=2) + xlab("Woman's cohort")+ylab("variance of IMR x100")+scale_colour_Publication()+ theme_Publication()
Scatter_1#B

y <- aggregate(dat[,"AFR"], list("Cohort" = dat[,6]), var)
colnames(y)[2] <-  "AFR"
y$Mortrate_scaled = y$Mortrate*100

Scatter_1 <- ggplot(y, aes(x = Cohort,y=AFR)) + geom_smooth(method = lm, se = FALSE, col = "black", size = 1)+geom_point(size=2) + xlab("Woman's cohort")+ylab("variance of fitness")+scale_colour_Publication()+ theme_Publication()
Scatter_1

#infant mortality per woman's AFR by her fertility
y <- aggregate(list(dat[,"OffMortality"], dat[,"fitdem"]), list("AFR" = dat[,20]), mean)
colnames(y)[2] <-  "OffMortality"
colnames(y)[3] <-  "Fitness"
y$n <- a
y$Fertility <- round(y$Fertility, digits = 1)
Scatter_2 <- ggplot(y, aes(x = AFR,y =OffMortality,size =n, color=Fitness))+geom_point() + xlab("Age at first reproduction")+ylab("Mean infant mortality")+ theme_Publication()
Scatter_2

tiff("test11_rev.tiff", units="in", width=8, height=4, res=300)
ggarrange(Scatter, Scatter_1,labels = c("A","B"),ncol = 2, nrow = 1)
dev.off()
#####################3d plot##############################
y_low <- aggregate(list(dat[dat$mortrate1 == "Low","OffMortality"], dat[dat$mortrate1 == "Low","fitdem"],dat[dat$mortrate1 == "Low","FERTILITY"],dat[dat$mortrate1 == "Low","Mortrate"]), list("AFR" = dat[dat$mortrate1 == "Low",20]), mean)
y_mid <- aggregate(list(dat[dat$mortrate1 == "Middle","OffMortality"], dat[dat$mortrate1 == "Middle","fitdem"],dat[dat$mortrate1 == "Middle","FERTILITY"],dat[dat$mortrate1 == "Middle","Mortrate"]), list("AFR" = dat[dat$mortrate1 == "Middle",20]), mean)
y_high <- aggregate(list(dat[dat$mortrate1 == "High","OffMortality"], dat[dat$mortrate1 == "High","fitdem"],dat[dat$mortrate1 == "High","FERTILITY"],dat[dat$mortrate1 == "High","Mortrate"]), list("AFR" = dat[dat$mortrate1 == "High",20]), mean)

y_full <- aggregate(list(dat[,"AFR"], dat[,"fitdem"],dat[,"OffMortality"]), list("COHORTW" = dat[,6], "Mortrate"= dat[,30]), mean)

Scatter_3 <- ggplot(y_full, aes(x = COHORTW,y =AFR, color = Mortrate))+geom_smooth(method = "lm", se =FALSE)+geom_point() + xlab("Woman's birth cohort")+ylab("mean age at first reproduction")+ theme_Publication()+scale_colour_Publication()
Scatter_3

Scatter_3 <- ggplot(dat, aes(x = BIRTHYW,y =AFR, color = mortrate1))+geom_smooth(method = "lm", se =FALSE)+geom_point() + xlab("Woman's birthyear")+ylab("Age at first reproduction")+ theme_Publication()+scale_colour_Publication()
Scatter_3

Scatter_4 <- ggplot(y_full, aes(x = BIRTHYW,y =Fertility, color = Mortrate))+geom_smooth(method = "lm", se =FALSE)+geom_point() + xlab("Woman's birthyear")+ylab("mean fertility")+ theme_Publication()+scale_colour_Publication()
Scatter_4

Scatter_4 <- ggplot(dat, aes(x = BIRTHYW,y =FERTILITY, color = mortrate1))+geom_smooth(method = "lm", se =FALSE)+geom_point() + xlab("Woman's birthyear")+ylab("Fertility")+ theme_Publication()+scale_colour_Publication()
Scatter_4

Scatter_5 <- ggplot(y_full, aes(x = BIRTHYW,y =Fitness, color = Mortrate))+geom_smooth(method = "lm", se =FALSE)+geom_point() + xlab("Woman's birthyear")+ylab("mean demographic fitness")+ theme_Publication()+scale_colour_Publication()
Scatter_5

Scatter_5 <- ggplot(dat, aes(x = BIRTHYW,y =fitdem, color = mortrate1))+geom_smooth(method = "lm", se =FALSE)+geom_point() + xlab("Woman's AFR")+ylab("demographic fitness")+ theme_Publication()+scale_colour_Publication()
Scatter_5

ggplot(dat_cen,aes(x=BIRTHYW, y=AFR, color = mortrate1))+geom_point()+geom_smooth(method = "lm", se =FALSE)

y_low[,"Mortgroup"] <- "Low"
y_mid[,"Mortgroup"] <- "Middle"
y_high[,"Mortgroup"] <- "High"

colnames(y_full)[3] <-  "AFR"
colnames(y_full)[4] <-  "Fitness"
colnames(y_full)[5] <-  "InfantMort"
colnames(y_full)[5] <-  "Mortrate"
y <- rbind(y_low,y_mid,y_high)
head(y)
y$Fertility <- round(y$Fertility, digits = 2)
y$Fitness <- round(y$Fitness, digits = 2)
y$Mortrate <- round(y$Mortrate, digits = 3)
y$OffMortality <- round(y$OffMortality, digits = 3)

p <- plot_ly(y, x = ~AFR, y = ~OffMortality, z = ~LRS, color = ~Mortgroup, colors = c('#BF382A', '#0C4B8E','#0C8E10' )) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'AFR'),
                      yaxis = list(title = 'Infant Mortality'),
                      zaxis = list(title = 'LRS')))

p <- plot_ly(y_full, x = ~Mortrate, y = ~Fertility, z = ~AFR,
             marker = list(color = ~Fitness, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE)) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'IMR'),
                      yaxis = list(title = 'Fertility'),
                      zaxis = list(title = 'AFR')),
         annotations = list(
           x = 1.13,
           y = 1.05,
           text = 'Fitness',
           xref = 'paper',
           yref = 'paper',
           showarrow = FALSE
         ))
p

#######################scaling############################
a <- scale(test$AFR, center = TRUE, scale = TRUE)[,1]
b <- scale(test$Mortrate, center = TRUE, scale = TRUE)[,1]
c <- scale(test$BIRTHYW, center = TRUE, scale = TRUE)[,1]
d <- scale(test$FERTILITY, center = TRUE, scale = TRUE)[,1]
e <- scale(test$repspan, center = TRUE, scale = TRUE)[,1]
f <- scale(test$ALR, center = TRUE, scale = TRUE)[,1]
g <- scale(test$OffMortality, center = TRUE, scale = TRUE)[,1]
datafr <- cbind.data.frame(AFR = a, Mortrate = b, BIRTHYW = c,Fertility = d ,Mortfact = test$mortrate1, Cohort = test$COHORTW,ALR = f, repspan = e, offmortality = g, fitness = test$LRS)

head(datafr)
###############plots##################3

m <- lm(ALR~1+Mortrate+BIRTHYW, data = datafr)
af <- anova(m)
afss <- af$"Sum Sq"
print(cbind(af,PctExp=afss/sum(afss)*100))

#infant mortality rate explains only 0.2% of the variation in AFR

#B - relation between AFR and fertility with infant mortality rate as a factorial
ggplot(datafr, aes(x=AFR, y=Fertility, color = Mortfact, shape = Mortfact)) +
  geom_point() +
  geom_smooth(method = lm, se=FALSE, fullrange = TRUE) +
  theme_classic()

m <- glm(repspan~1+AFR*mortrate1, data = test, family = "poisson")

#under the highest infant mortality rate we observe extreme cases where woman who start
#reproduction very early tend to have the highest fertility while those who begin late tend
#to have the lowest fertility, and that is compared to women from the other environments

#Temporal (secular) change in AFR and Fertility
#a1 - AFR
ggplot(IAC, aes(x=COHORTW, y=AFR)) +
  geom_point()+
  geom_smooth(method = lm, se = FALSE)+
  theme_classic()
#a2 - by-environment
dat_sum <- summarySE(dat, measurevar="AFR", groupvars=c("BIRTHYW","mortrate1"))
dat_sum <- summarySE(dat, measurevar="AFR", groupvars=c("COHORTW","mortrate1"))

ggplot(dat_sum, aes(x=COHORTW, y=AFR, color = mortrate1, shape = mortrate1)) +
  geom_point()+geom_errorbar(data = dat_sum,aes(ymin=AFR-se, ymax=AFR+se), width=.1)+
  geom_smooth(data=dat_sum,aes(x=COHORTW, y=AFR,color=mortrate1), method ="lm", se=FALSE )+
  theme_Publication()+scale_colour_Publication()

m <- lm(AFR~1+BIRTHYW*Mortfact, data = datafr)
#temporal populational decline in AFR
#the decline is strongest under the most favorable conditions while under
#the highest levels of infant mortality,the temporal change in AFR is the weakest
#b1 - Fertility
ggplot(IAC, aes(x=BIRTHYW, y=Fertility)) +
  geom_point()+
  geom_smooth(method = lm, se = FALSE)+
  theme_classic()
#b2 - by-environment
ggplot(datafr, aes(x=BIRTHYW, y=Fertility, color = Mortfact, shape = Mortfact)) +
  geom_point() +
  geom_smooth(method = lm, se=FALSE, fullrange = TRUE) +
  theme_classic()
#temporal increase in Fertility
#strong temporal increase in fertility under all environmental conditions
####even under the harsh conditions where there is less temporal change in AFR towards lower
####values, the fertility of these women increases maybe reprenting the availability of resources on the island
####for women who begin later than average to have a higher than average fertility.
####we dont observe late AFRs in the most favorable environment
#b1 - Reproductive Period
ggplot(IAC, aes(x=BIRTHYW, y=repspan)) +
  geom_point()+
  geom_smooth(method = lm, se = FALSE)+
  theme_classic()
#b2 - by-environment
ggplot(datafr, aes(x=BIRTHYW, y=repspan, color = Mortfact, shape = Mortfact)) +
  geom_point() +
  geom_smooth(method = lm, se=FALSE, fullrange = TRUE) +
  theme_classic()
###############predicted effect plots###################
library(effects)
library(Rmisc)
####infant mortality#####
#not necessary
ef <- effect("poly(AFR, 3)*mortrate4", m2, xlevels = list(AFR = seq(18,35,1)))
efdata <- as.data.frame(ef)
head(efdata)
efdata$mortrate4 <- factor(efdata$mortrate4, levels = c("Low", "Middle","High"))
efdata$AFR <- factor(efdata$AFR)

dat_sum <- summarySE(dat, measurevar="OffMortality", groupvars=c("AFR","mortrate4"))

tiff("test4_rev.tiff", units="in", width=5, height=5, res=300)

p <- ggplot(data=dat_sum, aes(x=AFR, y=OffMortality, color=mortrate4))
p <- p + geom_point()+geom_errorbar(data = dat_sum,aes(ymin=OffMortality-se, ymax=OffMortality+se), width=.1)
p <- p+ geom_smooth(data=efdata,aes(x=AFR,y=fit,color=mortrate4),method = "loess", se =FALSE)
p <- p+ geom_vline(xintercept = mean(dat$AFR), linetype = "dotted")+geom_hline(yintercept = mean(dat$OffMortality), linetype = "dotted")
p <- p+ labs(x= "AFR", y="Mean IMC", color="IMR group", fill="IMR group", size =1)
p <- p+ theme_Publication()+scale_colour_Publication()
p <- p+ ylim(0,2)
p <- p+ xlim(18,35)
p

dev.off()
####fitness by AFR#####
ef <- effect("AFR:mortrate4", mm2, xlevels = list(AFR = seq(18,35,1)))
efdata <- as.data.frame(ef)
head(efdata)
efdata$mortrate4 <- factor(efdata$mortrate4, levels = c("Low", "Middle","High"))
efdata$AFR <- factor(efdata$AFR)

dat_sum <- summarySE(dat, measurevar="fitdem", groupvars=c("AFR","mortrate4"))

tiff("test3_rev.tiff", units="in", width=5, height=5, res=300)

p1 <- ggplot(data=dat_sum, aes(x=AFR, y=fitdem, color = mortrate4))
p1 <- p1+ geom_point()+geom_errorbar(data = dat_sum,aes(ymin=fitdem-se, ymax=fitdem+se), width=.1)
p1 <- p1+ geom_smooth(data=efdata,aes(x=AFR, y=fit,color=mortrate4), method ="lm", se=FALSE ) 
p1 <- p1+ geom_vline(xintercept = mean(dat$AFR), linetype = "dotted")+geom_hline(yintercept = mean(dat$fitdem), linetype = "dotted")
p1 <- p1+ labs(x= "AFR", y="Mean relative fitness", color="IMR group", fill="IMR group")
p1 <- p1+ theme_Publication()+scale_colour_Publication()
p1 <- p1+ xlim(18,35)
p1

dev.off()
####fitness by cohort#####
ef <- effect("mortrate1:COHORTW", mm2, xlevels = list(COHORTW=seq(1,8,1)))
efdata <- as.data.frame(ef)
head(efdata)
efdata$mortrate1 <- factor(efdata$mortrate1, levels = c("Low", "Middle","High"))
efdata$COHORTW <- factor(efdata$COHORTW)

dat_sum <- summarySE(dat, measurevar="fitdem", groupvars=c("COHORTW","mortrate1"))

tiff("test17.tiff", units="in", width=5, height=5, res=300)

p1 <- ggplot(data=dat_sum, aes(x=COHORTW, y=fitdem, color = mortrate1))
p1 <- p1+ geom_point()+geom_errorbar(data = dat_sum,aes(ymin=fitdem-se, ymax=fitdem+se), width=.1)
p1 <- p1+ geom_smooth(data=efdata,aes(x=COHORTW, y=fit,color=mortrate1), method ="lm", se=FALSE ) 
p1 <- p1+ geom_vline(xintercept = mean(dat$AFR), linetype = "dotted")+geom_hline(yintercept = mean(dat$fitdem), linetype = "dotted")
p1 <- p1+ labs(x= "Birth cohort", y="Average fitness", color="IMR cond", fill="IMR cond")
p1 <- p1+ theme_Publication()+scale_colour_Publication()
p1 <- p1+ ylim(1,6)+xlim(1,8)
p1

dev.off()
####AFR#####
ef <- effect("mortrate4", model.1, xlevels = list())
efdata <- as.data.frame(ef)
head(efdata)
efdata$mortrate4 <- factor(efdata$mortrate4, levels = c("Low", "Middle","High"))
levels(efdata$mortrate4) <- c("Low", "Middle", "High")

dat_sum <- summarySE(dat, measurevar="AFR", groupvars="mortrate4")

tiff("test1_rev.tiff", units="in", width=5, height=5, res=300)

p2 <- ggplot(data = efdata, aes(x=mortrate4, y= fit, group = 1))
p2 <- p2+ geom_point(size =2)
p2 <- p2+ geom_errorbar(data= efdata,aes(ymin=fit-se, ymax=fit+se), width=.1)
p2 <- p2+ geom_line()
p2 <- p2+ geom_hline(yintercept = mean(dat$AFR), linetype = "dotted")
p2 <- p2+ labs(x= "IMR group", y="Mean AFR", color="IMR group", fill="IMR group")
p2 <- p2+ theme_Publication()+scale_colour_Publication()+scale_fill_Publication()
p2 <- p2+ ylim(22,25)
p2

dev.off()
####AFR by cohort#########
ef <- effect("mortrate4:COHORTW", model.2, xlevels = list(COHORTW = seq(1,8,1)))
efdata <- as.data.frame(ef)
head(efdata)
efdata$mortrate4 <- factor(efdata$mortrate4, levels = c("Low", "Middle","High"))
efdata$COHORTW <- factor(efdata$COHORTW)

dat_sum <- summarySE(dat, measurevar="AFR", groupvars=c("COHORTW","mortrate4"))

tiff("test2_rev.tiff", units="in", width=5, height=5, res=300)

p3 <- ggplot(data = dat_sum, aes(x=COHORTW, y= AFR, color=mortrate4))
p3 <- p3+ geom_point()+geom_errorbar(data = dat_sum,aes(ymin=AFR-se, ymax=AFR+se), width=.1)
p3 <- p3+ geom_smooth(data = efdata, aes(x = COHORTW, y=fit,color=mortrate4), method = "lm", se=FALSE) 
p3 <- p3+ geom_hline(yintercept = mean(dat$AFR), linetype = "dotted")
p3 <- p3+ labs(x= "Woman's birth cohort", y="Mean AFR", color="IMR group", fill="IMR group")
p3 <- p3+ theme_Publication()+scale_colour_Publication()
p3 <- p3+ ylim(20,31)
p3

dev.off()
####fertility#####
ef <- effect("AFR:mortrate4", mmm3, xlevels = list(AFR = seq(18,35,1)))
efdata <- as.data.frame(ef)
head(efdata)
efdata$mortrate4 <- factor(efdata$mortrate4, levels = c("Low", "Middle","High"))
efdata$AFR <- factor(efdata$AFR)

dat_sum <- summarySE(dat, measurevar="FERTILITY", groupvars=c("AFR","mortrate4"))

tiff("test7.tiff", units="in", width=5, height=5, res=300)

p3 <- ggplot(data = dat_sum, aes(x=AFR, y= FERTILITY, color=mortrate4,group=mortrate4))
p3 <- p3+ geom_point()+geom_errorbar(data = dat_sum,aes(ymin=FERTILITY-se, ymax=FERTILITY+se), width=.1)
p3 <- p3+ geom_smooth(data = efdata, aes(x = AFR, y=fit,color=mortrate4), method = "lm", se=FALSE) 
p3 <- p3+ geom_vline(xintercept = mean(dat$AFR), linetype = "dotted")+ geom_hline(yintercept = mean(dat$FERTILITY), linetype = "dotted")
p3 <- p3+ labs(x= "Age at first reproduction", y="Average fertility", color="IMR group", fill="IMR group")
p3 <- p3+ theme_Publication()+scale_colour_Publication()+scale_fill_Publication()
p3 <- p3+xlim(18,35)
p3

dev.off()

tiff("test_all_rev.tiff", units="in", width=7, height=7, res=300)
dev.off()
##################################plotting results###############################
#From MCMCglmm
#AFR~fertility

library(ggplot2)
library(gridExtra)
library(ggpubr)

#plotting additive variance###
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
#AFR and offmortality
df2 <- NULL
df2 <- cbind.data.frame("gencorr" = (model_multi$VCV[1:1000,3]/sqrt(model_multi$VCV[1:1000,1]*model_multi$VCV[1:1000,11])),"condition"="Population", "legend" = "AFR~FERTILITY")
df2 <- rbind(df2,cbind.data.frame("gencorr" = (model_multi_low$VCV[1:1000,3]/sqrt(model_multi_low$VCV[1:1000,1]*model_multi_low$VCV[1:1000,11])),"condition"="Low","legend" = "AFR~FERTILITY"))
df2 <- rbind(df2,cbind.data.frame("gencorr" = (model_multi_med$VCV[1:1000,3]/sqrt(model_multi_med$VCV[1:1000,1]*model_multi_med$VCV[1:1000,11])),"condition"="Mid","legend" = "AFR~FERTILITY"))
df2 <- rbind(df2,cbind.data.frame("gencorr" = (model_multi_high$VCV[1:1000,3]/sqrt(model_multi_high$VCV[1:1000,1]*model_multi_high$VCV[1:1000,11])),"condition"="High","legend" = "AFR~FERTILITY"))

b <- ggplot(aes(y = gencorr, x = condition), data = df2) + geom_boxplot(fatten = NULL, varwidth = TRUE, alpha = 0.2)+coord_flip()+ylab("Correlation")+xlab("Infant mortality rate")+geom_hline(yintercept=0, linetype="dotted")+theme(axis.text=element_text(size=12),axis.title = element_text(size = 14),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))
b <- b + annotate("segment", x = 0.6, xend = 1.4, y = -0.1185323, yend = -0.1185323, colour = "red", size=2, alpha=0.6)
b <- b + annotate("segment", x = 1.6, xend = 2.4, y = -0.1346377, yend = -0.1346377, colour = "red", size=2, alpha=0.6)
b <- b + annotate("segment", x = 2.6, xend = 3.4, y = -0.06750786, yend = -0.06750786, colour = "red", size=2, alpha=0.6)
b <- b + annotate("segment", x = 3.6, xend = 4.4, y = -0.2119109, yend = -0.2119109, colour = "red", size=2, alpha=0.6)
b <- b + annotate("point", x =c(1,2,3,4), y =c(0.00,-0.18,-0.20,-0.12), colour = "red")
b
#fertility and offmortality
df2 <- NULL
df2 <- cbind.data.frame("gencorr" = (model_multi$VCV[1:1000,7]/sqrt(model_multi$VCV[1:1000,6]*model_multi$VCV[1:1000,11])),"condition"="Population", "legend" = "AFR~FERTILITY")
df2 <- rbind(df2,cbind.data.frame("gencorr" = (model_multi_low$VCV[1:1000,7]/sqrt(model_multi_low$VCV[1:1000,6]*model_multi_low$VCV[1:1000,11])),"condition"="Low","legend" = "AFR~FERTILITY"))
df2 <- rbind(df2,cbind.data.frame("gencorr" = (model_multi_med$VCV[1:1000,7]/sqrt(model_multi_med$VCV[1:1000,6]*model_multi_med$VCV[1:1000,11])),"condition"="Mid","legend" = "AFR~FERTILITY"))
df2 <- rbind(df2,cbind.data.frame("gencorr" = (model_multi_high$VCV[1:1000,7]/sqrt(model_multi_high$VCV[1:1000,6]*model_multi_high$VCV[1:1000,11])),"condition"="High","legend" = "AFR~FERTILITY"))

b <- ggplot(aes(y = gencorr, x = condition), data = df2) + geom_boxplot(fatten = NULL, varwidth = TRUE, alpha = 0.2)+coord_flip()+ylab("Correlation")+xlab("Infant mortality rate")+geom_hline(yintercept=0, linetype="dotted")+theme(axis.text=element_text(size=12),axis.title = element_text(size = 14),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))
b <- b + annotate("segment", x = 0.6, xend = 1.4, y = 0.31736, yend = 0.31736, colour = "red", size=2, alpha=0.6)
b <- b + annotate("segment", x = 1.6, xend = 2.4, y = 0.3059556, yend = 0.3059556, colour = "red", size=2, alpha=0.6)
b <- b + annotate("segment", x = 2.6, xend = 3.4, y = 0.3254832, yend = 0.3254832, colour = "red", size=2, alpha=0.6)
b <- b + annotate("segment", x = 3.6, xend = 4.4, y = 0.3213158, yend = 0.3213158, colour = "red", size=2, alpha=0.6)
b <- b + annotate("point", x =c(1,2,3,4), y =c(0.15,0.10,0.15,-0.01), colour = "red")
b
#RP and offmortality
df2 <- NULL
df2 <- cbind.data.frame("gencorr" = (model_multi$VCV[1:1000,12]/sqrt(model_multi$VCV[1:1000,16]*model_multi$VCV[1:1000,11])),"condition"="Population", "legend" = "AFR~FERTILITY")
df2 <- rbind(df2,cbind.data.frame("gencorr" = (model_multi_low$VCV[1:1000,12]/sqrt(model_multi_low$VCV[1:1000,16]*model_multi_low$VCV[1:1000,11])),"condition"="Low","legend" = "AFR~FERTILITY"))
df2 <- rbind(df2,cbind.data.frame("gencorr" = (model_multi_med$VCV[1:1000,12]/sqrt(model_multi_med$VCV[1:1000,16]*model_multi_med$VCV[1:1000,11])),"condition"="Mid","legend" = "AFR~FERTILITY"))
df2 <- rbind(df2,cbind.data.frame("gencorr" = (model_multi_high$VCV[1:1000,12]/sqrt(model_multi_high$VCV[1:1000,16]*model_multi_high$VCV[1:1000,11])),"condition"="High","legend" = "AFR~FERTILITY"))

b <- ggplot(aes(y = gencorr, x = condition), data = df2) + geom_boxplot(fatten = NULL, varwidth = TRUE, alpha = 0.2)+coord_flip()+ylab("Correlation")+xlab("Infant mortality rate")+geom_hline(yintercept=0, linetype="dotted")+theme(axis.text=element_text(size=12),axis.title = element_text(size = 14),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))
b <- b + annotate("segment", x = 0.6, xend = 1.4, y = 0.2304188, yend = 0.2304188, colour = "red", size=2, alpha=0.6)
b <- b + annotate("segment", x = 1.6, xend = 2.4, y = 0.2066221, yend = 0.2066221, colour = "red", size=2, alpha=0.6)
b <- b + annotate("segment", x = 2.6, xend = 3.4, y = 0.2649497, yend = 0.2649497, colour = "red", size=2, alpha=0.6)
b <- b + annotate("segment", x = 3.6, xend = 4.4, y = 0.1937642, yend = 0.1937642, colour = "red", size=2, alpha=0.6)
b <- b + annotate("point", x =c(1,2,3,4), y =c(0.19,-0.06,0.21,-0.05), colour = "red")
b

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
#############################breeding values########################
#using ggplot
g <- ggplot(EBV_afr_fert, aes(x=EBV_afr, y=EBV_fert, color = mortrate1, shape = mortrate1)) +
      geom_point() +
      geom_smooth(method = glm, se=FALSE, fullrange = TRUE) + coord_fixed()+
      theme_classic()

g <- g + stat_ellipse(type = "norm")

eigen <- eigen(G)
eigen$slopes[1] <- eigen$vectors[1,1]/eigen$vectors[2,1]
eigen$slopes[2] <- eigen$vectors[1,1]/eigen$vectors[1,2]

g <- g + geom_abline(intercept = 0, slope = eigen$slopes[1], colour = "green")  # plot pc1
g <- g + geom_abline(intercept = 0, slope = eigen$slopes[2], colour = "red")  # plot pc2
g <- g + geom_segment(x = 0, y = 0, xend = eigen$values[1], yend = eigen$slopes[1] * eigen$values[1], colour = "green", arrow = arrow(length = unit(0.2, "cm")))  # add arrow for pc1
g <- g + geom_segment(x = 0, y = 0, xend = eigen$values[2], yend = eigen$slopes[2] * eigen$values[2], colour = "red", arrow = arrow(length = unit(0.2, "cm")))  # add arrow for pc2

perp.segment.coord <- function(x0, y0, a=0,b=1){
  #finds endpoint for a perpendicular segment from the point (x0,y0) to the line
  # defined by lm.mod as y=a+b*x
  x1 <- (x0+b*y0-a*b)/(1+b^2)
  y1 <- a + b*x1
  list(x0=x0, y0=y0, x1=x1, y1=y1)
}
ss <- perp.segment.coord(EBV_afr_fert$EBV_afr, EBV_afr_fert$EBV_fert, 0, eigen$slopes[1])
g <- g + geom_segment(data=as.data.frame(ss), aes(x = x0, y = y0, xend = x1, yend = y1), colour = "green", linetype = "dotted")
g


#using lattice plot
xyplot(EBV_fert ~ EBV_afr, groups=EBV_afr_fert$mortrate1, data = EBV_afr_fert,auto.key = list(corner = c(1, .98)), cex = 1.5)
xyplot(EBV_fert ~ EBV_afr | mortrate1, data = EBV_afr_fert, auto.key = list(corner = c(1, .98)), cex = 1.5)


#####################3d scatterplot for EBVs###################
library(plotly)
p <- plot_ly(test, x = ~EBV_afr, y = ~EBV_fert, z = ~Mortrate,
             marker = list(color = ~OffMortality, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE, size =4)) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'AFR BV'),
                      yaxis = list(title = 'FERT BV'),
                      zaxis = list(title = 'Inf Mort Rate')),
         annotations = list(
           x = 1.13,
           y = 1.05,
           text = 'Offspring mortality',
           xref = 'paper',
           yref = 'paper',
           showarrow = FALSE
         ))
p

#####################plotting ellipse for G matrix######################
ctr <- c(0,0,0,0)
RR <- chol(G_multi)
angles <- seq(0,2*pi, length.out = 200)
ell <- 1 * cbind(cos(angles), sin(angles)) %*% RR
ellCtr <- sweep(ell, 2, ctr, "+")
plot(ellCtr, type = "l", lwd = 2, asp = 1)
points(ctr[1], ctr[2], pch=4, lwd = 2)

library(car)
ellipse(c(0,0), shape = G, radius = 0.98, col = "red", lty=2)

eigVal <- eigen(G_multi)$values
eigVec <- eigen(G_multi)$vectors
eigScl <- eigVec %*% diag(sqrt(eigVal))
xMat    <- rbind(ctr[1] + eigScl[1, ], ctr[1] - eigScl[1, ],ctr[1] + eigScl[1, ])
yMat    <- rbind(ctr[2] + eigScl[2, ], ctr[2] - eigScl[2, ])
ellBase <- cbind(sqrt(eigVal[1])*cos(angles), sqrt(eigVal[2])*sin(angles)) # normal ellipse
ellRot  <- eigVec %*% t(ellBase)                                          # rotated ellipse
plot((ellRot+ctr)[1, ], (ellRot+ctr)[2, ], asp=1, type="l", lwd=2)
matlines(xMat, yMat, lty=1, lwd=2, col="green")
points(ctr[1], ctr[2], pch=4, col="red", lwd=3)
#####################################################################
library(ggplot2)
fit <- glm(FERTILITY ~ 1+AFR + I(AFR^2), data = test, family = "poisson")
prd <- data.frame(AFR = seq(from = range(test$AFR)[1], to = range(test$AFR)[2], length.out = 100))
err <- predict(fit, newdata = prd, se.fit = TRUE)

prd$lci <- err$fit - 1.96 * err$se.fit
prd$fit <- err$fit
prd$uci <- err$fit + 1.96 * err$se.fit

ggplot(prd, aes(x = AFR, y = fit)) +
  theme_bw() +
  geom_line() +
  geom_smooth(aes(ymin = lci, ymax = uci), stat = "identity") +
  geom_point(data = test, aes(x = AFR, y = FERTILITY))

############################histograms#####################################3
library(ggplot2)

ggplot(test, aes(x= AFR)) + geom_histogram(binwidth = 0.5, aes(fill =..count..))+theme_bw()

hist(df[df$mortrate1 == "Med",]$AFR, breaks=30, xlim=c(18,35), col=rgb(1,0,0,0.5), xlab="AFR", 
     ylab="number of women", main="distribution of AFR in 3 different environments" )
hist(df[df$mortrate1 == "Low",]$AFR, breaks=30, xlim=c(18,35), col=rgb(0,1,0,0.5), add=T)
hist(df[df$mortrate1 == "High",]$AFR, breaks=30, xlim=c(18,35), col=rgb(0,0,1,0.5), add=T)
legend("topright", legend=c("Middle","Low", "High"), col=c(rgb(1,0,0,0.5),rgb(0,1,0,0.5),rgb(0,0,1,0.5)), pt.cex=2, pch=15)

bp <- ggbarplot(df, x = "AFR", fill = "mortrate1", color = "white", palette = "jco", sort.by.groups = TRUE) 
bp + font("x.text", size = 8)
bp

plot(density(df[df$mortrate1 == "Med",]$AFR), col = "red", xlab = "AFR", main = "")
lines(density(df[df$mortrate1 == "Low",]$AFR), col = "green")
lines(density(df[df$mortrate1 == "High",]$AFR), col = "blue")
legend("topright", legend=c("Middle","Low", "High"), col=c("red","green","blue"), pt.cex=2, pch=15)

abline(v = mean(datafr[datafr$Mortfact == "Middle",]$AFR), col = "red")
abline(v = mean(datafr[datafr$Mortfact == "Low",]$AFR), col = "green")
abline(v = mean(datafr[datafr$Mortfact == "High",]$AFR), col = "blue")

hist(IAC_med$FERTILITY, breaks=30, xlim=c(0,20), col=rgb(1,0,0,0.5), xlab="Fertility", 
     ylab="number of women", main="distribution of fertility in 3 different environments" )
hist(IAC_low$FERTILITY, breaks=30, xlim=c(0,20), col=rgb(0,1,0,0.5), add=T)
hist(IAC_high$FERTILITY, breaks=30, xlim=c(0,20), col=rgb(0,0,1,0.5), add=T)

plot(density(datafr[datafr$Mortfact == "Middle",]$Fertility), col = "red")
lines(density(datafr[datafr$Mortfact == "Low",]$Fertility), col = "green")
lines(density(datafr[datafr$Mortfact == "High",]$Fertility), col = "blue")
abline(v = mean(datafr[datafr$Mortfact == "Middle",]$Fertility), col = "red")
abline(v = mean(datafr[datafr$Mortfact == "Low",]$Fertility), col = "green")
abline(v = mean(datafr[datafr$Mortfact == "High",]$Fertility), col = "blue")

hist(datafr[datafr$Mortfact == "Middle",]$repspan, breaks=20, xlim=c(-2,2), col=rgb(1,0,0,0.5), xlab="Reproductive preiod", 
     ylab="number of women", main="distribution of RP in 3 different environments" )
hist(datafr[datafr$Mortfact == "Low",]$repspan, breaks=20, xlim=c(-2,2), col=rgb(0,1,0,0.5), add=T)
hist(datafr[datafr$Mortfact == "High",]$repspan, breaks=20, xlim=c(-2,2), col=rgb(0,0,1,0.5), add=T)

plot(density(datafr[datafr$Mortfact == "Middle",]$repspan), col = "red")
lines(density(datafr[datafr$Mortfact == "Low",]$repspan), col = "green")
lines(density(datafr[datafr$Mortfact == "High",]$repspan), col = "blue")
abline(v = mean(datafr[datafr$Mortfact == "Middle",]$repspan), col = "red")
abline(v = mean(datafr[datafr$Mortfact == "Low",]$repspan), col = "green")
abline(v = mean(datafr[datafr$Mortfact == "High",]$repspan), col = "blue")

plot(density(datafr[datafr$Mortfact == "Middle",]$offmortality), col = "red")
lines(density(datafr[datafr$Mortfact == "Low",]$offmortality), col = "green")
lines(density(datafr[datafr$Mortfact == "High",]$offmortality), col = "blue")
abline(v = mean(datafr[datafr$Mortfact == "Middle",]$offmortality), col = "red")
abline(v = mean(datafr[datafr$Mortfact == "Low",]$offmortality), col = "green")
abline(v = mean(datafr[datafr$Mortfact == "High",]$offmortality), col = "blue")
#########################################################################
library(ggplot2)
library(scales)

# Plot
tiff("test12.tiff", units="in", width=5, height=5, res=300)
ggplot(coeff_year, aes(x=Year, y=corrcoeff)) + 
  geom_point(col="tomato2", size=3) + # Draw points
  geom_hline(yintercept = 0, linetype="dotted") +   
  geom_segment(aes(x=Year, 
                   xend=Year, 
                   y=min(corrcoeff), 
                   yend=max(corrcoeff)), 
               linetype="dashed", 
               size=0.1) +   # Draw dashed lines
  labs(x="Years since birthyear", y="Pearson correlation coefficient")+  
  coord_flip()
dev.off()
###########plotting cov densities##########
plot(density(cov_low), lwd = 2, col = "blue",
     main = "Densities of between-trait genetic\ncovariance by IMR group", xlab = "",
     xlim = c(-0.7, 0.2),  # Min and Max X-axis limits
     ylim = c(0, 5))  # Min and Max Y-axis limits

lines(density(cov_mid), col = "orange", lwd = 2)
lines(density(cov_high), col = "green", lwd = 2)
legend(x = "topleft",          # Position
       legend = c("Low","Middle", "High"),  # Legend texts
       lty = 1,           # Line types
       col = c("blue", "orange","green"),           # Line colors
       lwd = 2)  
