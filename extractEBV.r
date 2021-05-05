EBVextract <- function(Dataset,ModelName, covariate){
  nitt <- nrow(ModelName$Sol) #number of iterations within the model
  EBVs <- cbind(Dataset["animal"],Dataset["Mortrate_3y"], Dataset["mortrate1"],Dataset[covariate]) #create dataframe with Ind and Covariate
  
  for(i in 1:nitt){
    BV <- ModelName$Sol[i,] #group all residuals by iteration (model row)
    BV <- BV[substr(names(BV),1,31)=="traitfitdem:Mortrate_3y.animal."] #substract the wanted breeding values per individual 
	#change AFR in line 7 depending on the model
    names(BV)<-substr(names(BV),32,37) #substract the names of the indvidual
	#change the margins 17,22 in line 9 to the margins that match individual's ID
    
    EBVs<-merge(EBVs,BV,by.x="animal",by.y=0) #merge and match breeding values to their individual in the dataset
    colnames(EBVs)[dim(EBVs)[2]]<-paste("BreedingValue",i,sep = "_") #name the column
  }
  saveRDS(EBVs, file = "C:/Users/walid/OneDrive/Bureau/EBVs_fit_mort.r") #save the complete dataset as an R object for future loading
}

EBVextract(dat,model_afr_fit_rev_ram_2,"COHORTW")
#full trait no conditioning
EBVs_afr <- readRDS(file = "C:/Users/walid/OneDrive/Bureau/EBVs.r")
EBVs_fit <- readRDS(file = "C:/Users/walid/OneDrive/Bureau/EBVs_fit.r")
#by IMR group
#AFR
EBVs_low<- readRDS(file = "C:/Users/walid/OneDrive/Bureau/EBVs_low.r")
EBVs_med<- readRDS(file = "C:/Users/walid/OneDrive/Bureau/EBVs_med.r")
EBVs_high<- readRDS(file = "C:/Users/walid/OneDrive/Bureau/EBVs_high.r")
#fitness
EBVs_fit_low<- readRDS(file = "C:/Users/walid/OneDrive/Bureau/EBVs_fit_low.r")
EBVs_fit_med<- readRDS(file = "C:/Users/walid/OneDrive/Bureau/EBVs_fit_med.r")
EBVs_fit_high<- readRDS(file = "C:/Users/walid/OneDrive/Bureau/EBVs_fit_high.r")

EBVs_low<- EBVs_low[EBVs_low$mortrate1 == "Low",]
EBVs_med<- EBVs_med[EBVs_med$mortrate1 == "Middle",]
EBVs_high<- EBVs_high[EBVs_high$mortrate1 == "High",]

EBVs_fit_low<- EBVs_fit_low[EBVs_fit_low$mortrate1 == "Low",]
EBVs_fit_med<- EBVs_fit_med[EBVs_fit_med$mortrate1 == "Middle",]
EBVs_fit_high<- EBVs_fit_high[EBVs_fit_high$mortrate1 == "High",]
#RAM model
EBVs_afr_ram <- readRDS(file = "C:/Users/walid/OneDrive/Bureau/EBVs_afr_ram.r")
EBVs_fit_ram <- readRDS(file = "C:/Users/walid/OneDrive/Bureau/EBVs_fit_ram.r")
EBVs_afr_mort_ram <- readRDS(file = "C:/Users/walid/OneDrive/Bureau/EBVs_afr_mort.r")
EBVs_fit_mort_ram <- readRDS(file = "C:/Users/walid/OneDrive/Bureau/EBVs_fit_mort.r")
#replace ... in line 15 and 18 with your %username% or change the path if you are using windows

EBVs_all <- rbind.data.frame(EBVs_low,EBVs_med, EBVs_high)
EBVs_fit_all <- rbind.data.frame(EBVs_fit_low,EBVs_fit_med, EBVs_fit_high)

#calculate posterior mode of breeding value for each individual in dataset

col_posteriormode <- function(dat){
  a <- grep("^BreedingValue_1$",colnames(dat))
  b <- grep("^BreedingValue_1000$",colnames(dat))
  for (i in 1:nrow(dat)) {
    dat$modebv[i] <- posterior.mode(mcmc(as.numeric(dat[i,a:b])))
  }
  return(dat)
}
#RAM
EBVs_afr_ram <- col_posteriormode(EBVs_afr_ram)
EBVs_fit_ram <- col_posteriormode(EBVs_fit_ram)
EBVs_afr_mort_ram <- col_posteriormode(EBVs_afr_mort_ram)
EBVs_fit_mort_ram <- col_posteriormode(EBVs_fit_mort_ram)
#Categorical
EBVs_all <- col_posteriormode(EBVs_all)

EBVs_fit_all <- col_posteriormode(EBVs_fit_all)
######################plot breeding values###########################

temp_sum <- summarySE(EBVs_all, measurevar="modebv", groupvars=c("mortrate1"))

ggplot(EBVs_all, aes(mortrate1, modebv, fill = mortrate1)) + geom_boxplot()
###
temp_sum <- summarySE(EBVs_all, measurevar="modebv", groupvars=c("COHORTW","mortrate1"))
temp_sum_fit <- summarySE(EBVs_fit_all, measurevar="modebv", groupvars=c("COHORTW","mortrate1"))
#RAM
temp_sum_afr <- summarySE(EBVs_afr_ram, measurevar="modebv", groupvars=c("COHORTW"))
temp_sum_fit <- summarySE(EBVs_fit_ram, measurevar="modebv", groupvars=c("COHORTW"))
#calcualte GxE
EBVs_afr_mort_ram$modebv <- EBVs_afr_mort_ram$modebv*scale(EBVs_afr_mort_ram$Mortrate_3y)
EBVs_fit_mort_ram$modebv <- EBVs_fit_mort_ram$modebv*scale(EBVs_fit_mort_ram$Mortrate_3y)

temp_sum_afr_mort_ram <- summarySE(EBVs_afr_mort_ram, measurevar="modebv", groupvars=c("COHORTW"))
temp_sum_fit_mort_ram <- summarySE(EBVs_fit_mort_ram, measurevar="modebv", groupvars=c("COHORTW"))

dat_ebvs <- cbind.data.frame(EBVs_afr_ram$animal,EBVs_afr_ram$Mortrate_3y,EBVs_afr_ram$mortrate1,EBVs_afr_ram$COHORTW,EBVs_afr_ram$modebv,EBVs_afr_mort_ram$modebv, EBVs_fit_ram$modebv, EBVs_fit_mort_ram$modebv)
colnames(dat_ebvs)<-c("woman","Mortrate","IMR_group","Cohort","AFR", "AFR_mort", "Fitness","Fitness_mort")

###################covariance between BVs of AFR and fitness per cohort and per env#######################
ebv_cor <- data.frame("COHORTW" = temp_sum$COHORTW,"mortrate1"= as.factor(as.character(temp_sum$mortrate1)))
se <- function(x) sqrt(var(x)/length(x))

col_covar <- function(dat){
  post_col <- colnames(dat)[4:1003]
  for (i in 1:nrow(temp_sum)) {
    covg <- numeric(1000)
    for (col in post_col) {
      bv_afr <- EBVs_all[which(EBVs_all$mortrate1 == temp_sum$mortrate1[i] & EBVs_all$COHORTW == temp_sum$COHORTW[i]),col]
      bv_fit <- EBVs_fit_all[which(EBVs_fit_all$mortrate1 == temp_sum$mortrate1[i] & EBVs_fit_all$COHORTW == temp_sum$COHORTW[i]),col]
      covg[grep(col,post_col)] <- cov(bv_afr,bv_fit)
    }
    ebv_cor$mean[i] <- mean(covg)
    ebv_cor$se[i] <- se(covg)
  }
  return(ebv_cor)
}

ebv_cor <- col_covar(EBVs_all)

for (i in 1:nrow(temp_sum)) {
  bv_afr <- EBVs_all[which(EBVs_all$mortrate1 == temp_sum$mortrate1[i] & EBVs_all$COHORTW == temp_sum$COHORTW[i]),]$modebv
  bv_fit <- EBVs_fit_all[which(EBVs_fit_all$mortrate1 == temp_sum$mortrate1[i] & EBVs_fit_all$COHORTW == temp_sum$COHORTW[i]),]$modebv
  temp_sum$covg[i] <- cov(bv_afr,bv_fit)
}

for (i in 1:nrow(temp_sum)) {
  bv_afr <- EBVs_all[which(EBVs_all$COHORTW == temp_sum$COHORTW[i]),]$modebv
  bv_fit <- EBVs_fit_all[which(EBVs_fit_all$COHORTW == temp_sum$COHORTW[i]),]$modebv
  temp_sum$covg_total[i] <- cov(bv_afr,bv_fit)
}

##################################
#used for article
#average EBVs over IMR
temp_afr_ram <- summarySE(EBVs_afr_ram, measurevar="modebv", groupvars=c("Mortrate_3y","mortrate1"))
temp_afr_mort_ram <- summarySE(EBVs_afr_mort_ram, measurevar="modebv", groupvars=c("Mortrate_3y","mortrate1"))
colnames(temp_afr_ram)[4] <- "genetic"
colnames(temp_afr_mort_ram)[4] <- "conditional"
#bind values in one dataframe
temp_afr_ebv <- cbind(temp_afr_ram, temp_afr_mort_ram)[,c(1,2,3,4,5,6,7,11,12,13,14)]
temp_afr_ebv$total <- temp_afr_ebv$genetic+temp_afr_ebv$conditional

tiff("afr_ram.tiff", units="in", width=5, height=5, res=300)

p1 <- ggplot(data=temp_afr_ebv)
p1 <- p1+ geom_point(aes(x=Mortrate_3y,y=total, shape = "Total",colour= mortrate1), size =2)
p1 <- p1+ geom_point(aes(x=Mortrate_3y,y=genetic, shape = "Genetic",colour= mortrate1), size =2)
p1 <- p1+ geom_point(aes(x=Mortrate_3y,y=conditional, shape = "Conditional",colour= mortrate1), size =2)
p1 <- p1+ geom_smooth(aes(x=Mortrate_3y, y=total,linetype = "dotted"), method ="lm", se=FALSE,show.legend = F, col="red")
p1 <- p1+ geom_smooth(aes(x=Mortrate_3y, y=genetic,linetype = "solid"), method ="lm", se=FALSE,show.legend = F, col="black")
p1 <- p1+ geom_smooth(aes(x=Mortrate_3y, y=conditional), linetype="dotted",method ="lm", se=FALSE,show.legend = F, col="red")
p1 <- p1+ geom_hline(yintercept = 0, linetype = "dashed", color = "gray")+labs(x= "IMR", y="EBVs of AFR", shape="EBV type", fill="EBV type", size =1)
p1 <- p1+theme_Publication()+scale_colour_Publication()
p1 <- p1+theme(legend.position="bottom",legend.title = element_text( size = 5),
               legend.text = element_text(size = 5))
p1

dev.off()

tiff("afr_ram_cohrot.tiff", units="in", width=5, height=5, res=300)

p1 <- ggplot(data=temp_sum_afr_mort_ram, aes(x=COHORTW, y=modebv))
p1 <- p1+ geom_point()+geom_errorbar(data = temp_sum_afr_mort_ram,aes(ymin=modebv-se, ymax=modebv+se), width=.1)
p1 <- p1+ geom_smooth(data=temp_sum_afr_mort_ram,aes(x=COHORTW, y=modebv), method ="lm", se=FALSE )
p1 <- p1+ geom_hline(yintercept = 0, linetype = "dashed", color = "gray")+labs(x= "Woman's birth cohort", y="EBVs of AFR", size =1)
p1 <- p1+theme_Publication()+scale_colour_Publication()+scale_x_continuous(breaks = 1:8)
p1

dev.off()

tiff("bv_cov.tiff", units="in", width=5, height=5, res=300)

p <- ggplot(data=temp_sum, aes(x=COHORTW))
p <- p+ geom_point(aes(y=modebv,color=mortrate1),alpha = 0.4)+geom_errorbar(data = temp_sum,aes(ymin=modebv-se, ymax=modebv+se),alpha=0.2, width=.1)
p <- p+ geom_smooth(aes(x=COHORTW, y=modebv), method ="lm", se=FALSE)
p <- p+ geom_point(aes(y = covg*5, color = mortrate1),alpha = 1.5, size = 4)
p <- p+ geom_point(aes(y = covg_total*5), alpha = 1, size = 2)
p <- p+ scale_y_continuous(name= "Mean EBV of AFR" ,sec.axis = sec_axis(~./5, name = "Genetic covariance"))
p <- p+ scale_x_continuous(breaks = 1:8)
p <- p+ geom_hline(yintercept = 0, linetype = "dashed", color = "gray")+labs(x= "Birth cohort (20 years)", y="EBV of AFR", color="IMR group", fill="IMR group", size =1)
p <- p+theme_Publication()+scale_colour_Publication()
p



dev.off()


tiff("test18.tiff", units="in", width=5, height=5, res=300)

p1 <- ggplot(data=temp_sum, aes(x=COHORTW, y=modebv, color=mortrate1))
p1 <- p1+ geom_point()+geom_errorbar(data = temp_sum,aes(ymin=modebv-se, ymax=modebv+se), width=.1)
p1 <- p1+ geom_smooth(data=temp_sum,aes(x=COHORTW, y=modebv,color=mortrate1), method ="lm", se=FALSE )
p1 <- p1+ geom_hline(yintercept = 0, linetype = "dashed", color = "gray")+labs(x= "Woman's birth cohort", y="EBVs of AFR", color="IMR group", fill="IMR group", size =1)
p1 <- p1+theme_Publication()+scale_colour_Publication()+scale_x_continuous(breaks = 1:8)
p1

dev.off()

tiff("test19.tiff", units="in", width=5, height=5, res=300)

p2 <- ggplot(data=temp_sum_fit, aes(x=COHORTW, y=modebv, color=mortrate1))
p2 <- p2+ geom_point()+geom_errorbar(data = temp_sum_fit,aes(ymin=modebv-se, ymax=modebv+se), width=.1)
p2 <- p2+ geom_smooth(data=temp_sum_fit,aes(x=COHORTW, y=modebv,color=mortrate1), method ="lm", se=FALSE )
p2 <- p2+ geom_hline(yintercept = 0, linetype = "dashed", color = "gray")+labs(x= "Woman's birth cohort", y="EBVs of relative fitness", color="IMR group", fill="IMR group", size =1)
p2 <- p2+theme_Publication()+scale_colour_Publication()+scale_x_continuous(breaks = 1:8)
p2

dev.off()

tiff("trend_BV.tiff", units="in", width=8, height=4, res=300)
ggarrange(p1,p2,labels = c("A","B"),ncol = 2, nrow = 1, legend = "bottom",common.legend = TRUE)
dev.off()

################################################################
ggplot(EBVs_new,aes(x=mortrate1,y=meanbv))+geom_boxplot()

for (i in 4:1003) {
  fit <- lm(EBVs_low[,i]~EBVs_low$COHORTW)
  abline(a=coef(fit)[1], b=coef(fit)[2], col = "black")
}
for (i in 4:1003) {
  fit <- lm(EBVs_med[,i]~EBVs_med$COHORTW)
  abline(a=coef(fit)[1], b=coef(fit)[2], col = "red")
}
for (i in 4:1003) {
  fit <- lm(EBVs_high[,i]~EBVs_high$COHORTW)
  abline(a=coef(fit)[1], b=coef(fit)[2], col = "green")
}

fit <- lm(EBVs_new$BreedingValue~EBVs_new$COHORTW)
abline(a=coef(fit)[1], b=coef(fit)[2], col="black")

ggplot(data = EBVs_all,aes(x=COHORTW, y= BreedingValue, color =mortrate1))+geom_point()

EBVs_bv <- cbind.data.frame("animal"=EBVs_all$animal, "mortrate1"=EBVs_all$mortrate1, "bv_afr"=EBVs_all$modebv, "bv_fit"=EBVs_fit_all$modebv)

#slope from main model
slope <- posterior.mode(model_afr_lrs$VCV[,2]/model_afr_lrs$VCV[,1])
HPDinterval(model_afr_lrs$VCV[,2]/model_afr_lrs$VCV[,1])

#slope from categorical model
slope_middle <- posterior.mode(model_afr_fit_rev_2$VCV[,16]/model_afr_fit_rev_2$VCV[,15])
HPDinterval(model_afr_fit_rev_2$VCV[,16]/model_afr_fit_rev_2$VCV[,15])
slope_high <- posterior.mode(model_afr_fit_rev_2$VCV[,30]/model_afr_fit_rev_2$VCV[,29])
HPDinterval(model_afr_fit_rev_2$VCV[,30]/model_afr_fit_rev_2$VCV[,29])
slope_low <- posterior.mode(model_afr_fit_rev_2$VCV[,2]/model_afr_fit_rev_2$VCV[,1])
HPDinterval(model_afr_fit_rev_2$VCV[,2]/model_afr_fit_rev_2$VCV[,1])

df.slope<-data.frame(IMR = EBVs_bv$mortrate1, 
                     y = c(EBVs_bv[which(EBVs_bv$mortrate1=="Low"),]$bv_afr*-0.2051206,
                           EBVs_bv[which(EBVs_bv$mortrate1=="Middle"),]$bv_afr*-0.3142285,
                           EBVs_bv[which(EBVs_bv$mortrate1=="High"),]$bv_afr*-0.4162696),
                     x = EBVs_bv$bv_afr,
                     low_hdi = c(EBVs_bv[which(EBVs_bv$mortrate1=="Low"),]$bv_afr*-0.5326362,
                           EBVs_bv[which(EBVs_bv$mortrate1=="Middle"),]$bv_afr*-0.5944841,
                           EBVs_bv[which(EBVs_bv$mortrate1=="High"),]$bv_afr*-0.7208802),
                     high_hdi = c(EBVs_bv[which(EBVs_bv$mortrate1=="Low"),]$bv_afr*0.07610469,
                            EBVs_bv[which(EBVs_bv$mortrate1=="Middle"),]$bv_afr*-0.08403442,
                            EBVs_bv[which(EBVs_bv$mortrate1=="High"),]$bv_afr*-0.04359924))

#to be used for article
tiff("plot_bv_afr_fit.tiff", units="in", width=5, height=5, res=300)
ggplot(EBVs_bv, aes(x=bv_afr, y=bv_fit)) + 
  geom_point(aes(col=mortrate1), size=2) + 
  geom_smooth(data=EBVs_bv,aes(x=bv_afr, y=bv_fit,color=mortrate1), method ="lm", se=FALSE ) +
  labs(y="EBVs of fitness",x="EBVs of AFR",color="IMR group")+
  theme_Publication()+scale_colour_Publication()

dev.off()



tiff("plot_bv_afr_fit_posterior.tiff", units="in", width=5, height=5, res=300)
ggplot(EBVs_bv, aes(x=bv_afr, y=bv_fit)) + 
  geom_point(aes(col=mortrate1), size=2) + 
  geom_line(data=df.slope, aes(x= x,y=y,ymin= low_hdi, ymax=high_hdi,color=IMR), size=1)+
  geom_ribbon(data = df.slope, aes(ymin= low_hdi, ymax=high_hdi, color=IMR), alpha=0.2)+
  labs(y="EBVs of relative fitness",x="EBVs of AFR",color="IMR group")+ scale_x_continuous(limits = c(-1,2.5))+
  theme_Publication()+scale_colour_Publication()

dev.off()

#################################

plotAllLayers<-function(df){
  p<-ggplot(data=df,aes(df[,3]))+facet_grid(cols = vars(mortrate1))
  for(i in sample(x=names(df)[c(-1,-2,-3,-1004)],size=100)){ 
    p<-p+geom_smooth(aes_string(y=i),method=lm,se=FALSE,col="grey")
  }
  p <- p+geom_smooth(aes(y=modebv), method=lm,se=FALSE,col="black")
  p <- p+geom_hline(yintercept = 0, linetype = "dashed", color = "gray")
  p <- p+labs(x = "Woman's birth cohort", y = "EBVs of relative fitness")
  return(p)
}

a<- plotAllLayers(EBVs_all)
b <- plotAllLayers(EBVs_fit_all)


tiff("test_BV_all.tiff", units="in", width=10, height=8, res=300)
ggarrange(a,b,labels = c("A","B"),ncol = 1, nrow = 2, legend = "bottom",common.legend = TRUE)
dev.off()
