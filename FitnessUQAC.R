#données
dat <- read.csv(file = "/Data/charlevoix_ind.txt", header = FALSE, stringsAsFactors = FALSE,na.strings=c("","NA"))

  # charger un jeu de donn?es "dat" concernant les individus n?s au Saguenay, dont les 1eres colonnes sont dans l'ordre (peu importe les noms)
  #1. identifiant de l'individu
  #2. identifiant du p?re
  #3. identifiant de la m?re
  #4. sexe (1 pour les hommes et 2 pour les femmes)
  #5. ann?e de naissance (si dates compl?tes ou m?lange de dates et d'ann?es, ca marche mais il faut installer la library lubridate avant de faire tourner)
  #6. ann?e de d?c?s (idem)
  #7 ann?e de mariage (idem)

colnames(dat)<-c("ind","pere","mere","sexe","birthy","dated","datm")



#Si il y a des ann?es=0

dat[dat$birthy ==0,5 ] <- NA
dat[dat$dated ==0, 6] <- NA
dat[dat$datm == 0, 7] <- NA

dat$birthy <- as.Date(dat$birthy, format = "%m/%d/%Y")
dat$dated <- as.Date(dat$dated, format = "%m/%d/%Y")
dat$datm <- as.Date(dat$datm, format = "%m/%d/%Y")

#Si il y a des dates compl?tes et pas que des ann?es pour le mariage (seulement):

require(lubridate)
married=0

for (i in 1:nrow(dat)) { 
if (is.na(dat[i,"datm"])==FALSE){married[i]=year(dat[i,"datm"])
}else{married[i]=NA}
}

dat$datm=married


#Si il y a des dates compl?tes et pas que des ann?es (et pas que pour le mariage):

datn=0
died=0
married=0


library(lubridate)

 for (i in 1:nrow(dat)) { 
    
  if (is.numeric(dat[i,"birthy"])==FALSE&is.na(dat[i,"birthy"])==FALSE){datn[i]=year(dat[i,"birthy"])
   } else if (is.na(dat[i,"birthy"])==TRUE){datn[i]=NA
    } else{datn[i]=dat[i,"birthy"]}
    if (is.numeric(dat[i,"dated"])==FALSE&is.na(dat[i,"dated"])==FALSE){died[i]=year(dat[i,"dated"])
    }else if (is.na(dat[i,"dated"])==TRUE){died[i]=NA
    }else{died[i]=dat[i,"dated"]}
    if (is.numeric(dat[i,"datm"])==FALSE&is.na(dat[i,"datm"])==FALSE){married[i]=year(dat[i,"datm"])
    }else if (is.na(dat[i,"datm"])==TRUE){married[i]=NA
    }else{married[i]=dat[i,"datm"]}
  }

dat$birthy=datn
dat$dated=died
dat$datm=married


dat=dat[is.na(dat$birthy) == FALSE,]
##PARAMETRES##
interval<-20  # intervalle entre 2 cohortes
maxyear<-1973  #limite sup?rieure (ann?e) des donn?es
age.limite=15  # ?ge max auxquel on consid?re qu'un enfant est pr?sent sans preuve de pr?sence (telle que mariage)
fonly=FALSE #true si on veut ne prendre en compte que les enfants de sexe f?minin quand on calcule la fitness des femmes
sexe=2 #si on veut les 2, sinon 1 pour les H et 2 pour les femmes
agemax=110

if(sexe==2 |is.na(sexe)==TRUE){
#####FEMMMES#############

##preparation du jeu de donn?es##
sex=2
femmes=dat[is.na(dat$sexe)== FALSE &dat$sexe==sex,] 
femmes=femmes[is.na(femmes$birthy)==FALSE,] #que les femmes avec date de naissance
femmes=femmes[femmes$birthy<maxyear,] #que les femmes avec date de naissance inferieure a maxyear
nbfemmes<-nrow(femmes) #nbre de femmes
cohorte=trunc((femmes[,"birthy"]-min(dat$birthy,na.rm=TRUE))/interval+1)#d?fini le num?ro de cohorte
femmes=cbind(femmes, cohorte)#ajoute le num?ro de cohorte au tableau


##########################           MATRICE PAR INDIVIDU           #######################
seq1=c(0,0,0,0,1,0,0)
MATRICE_GEANTE = matrix(0, nrow=0, ncol=7)
colnames(MATRICE_GEANTE)<-c("FEMME","YEAR","AGE","REPRO","SURV","MARIAGE","COHORTE")

for (f in 1:nbfemmes){
  
  print(paste("femme",f,"sur",nbfemmes))

    
    MAT=matrix(seq1, nrow= agemax, ncol= 7, byrow=TRUE)
    colnames(MAT)<-c("FEMME","YEAR","AGE","REPRO","SURV","MARIAGE","COHORTE")
    MAT[,"FEMME"]=femmes[f,"ind"]
    MAT[,"YEAR"]=c(femmes[f,"birthy"]:(femmes[f,"birthy"]+(agemax-1)))
    MAT[,"AGE"]<-c(0:(agemax-1))
    MAT[,"COHORTE"]<-femmes[f,"cohorte"]
    
    
    rows<-dat[dat$ind==femmes[f,"ind"],]    
    

    if(fonly==TRUE){rows2<-dat[dat[,"mere"]==femmes[f,"ind"]&dat[,"sexe"]==2,]#sort les infos sur les enfants
    }else {rows2<-dat[dat[,"mere"]==femmes[f,"ind"],] }#sort les infos sur les enfants filles 
    
    #femmes[f,"FECONDITE"]=nrow(rows2)
    
    if(nrow(rows2)>0){
      
      for (x in 1:nrow(rows2)){
        if(!is.na(rows2$sexe[x])){
          MAT[is.element(MAT[,"YEAR"],rows2$birthy[x]),"REPRO"]<-MAT[is.element(MAT[,"YEAR"],rows2$birthy[x]),"REPRO"]+1
        }}
    }
    if (is.na(rows$dated[1])){
      
      max_ile=max(rows[,c(5,7)],na.rm=TRUE)
      m=rows[,5]
      max_ile=max(c(max_ile,m))
      
      # ci-dessous on consid?re que les femmes sont pr?sentes au moins jusqu'? l'?ge limite (sp?cifi? au d?but) sauf si d?c?d?es avant
      ifelse(max_ile<(femmes[f,"birthy"]+age.limite),max_ile<-(femmes[f,"birthy"]+age.limite),max_ile<-max_ile)
      
      max_enf= max(rows2$birthy,na.rm=TRUE)
      if(is.na(max_enf)==FALSE & max_ile < max_enf){max_ile=max_enf}
      
      # ci-dessous pour ?ter les femmes du jeu de donn?es apr?s derni?re date de pr?sence attest?e/pr?sum?e...
      #MAT[MAT[,"YEAR"]>=max_ile+1,4:6]<-NA
      
      # ... ou alternativement on les consid?re comme mortes apr?s cette date
      # on consid?re que les femmes qui ne sont pas mortes avant l'?ge limite (parties ou pas) sont comme mortes, car elles ne participent pas ? la repro
      MAT[MAT[,"YEAR"]==max_ile+1,5]<-0
      MAT[MAT[,"YEAR"]>max_ile+1,4:6]<--1
    }
    
    
    if(!is.na(rows$dated[1]))
    { MAT[is.element(MAT[,"YEAR"],rows$dated[1]),"SURV"]<- MAT[is.element(MAT[,"YEAR"],rows$dated[1]),"SURV"]-1
    MAT[MAT[,"YEAR"]>rows$dated[1],4:6]<--1}
    
    
    
   MATRICE_GEANTE<-rbind(MATRICE_GEANTE,MAT)

  
}

nbfemmes<-nrow(femmes)   



################################          Cr?ation de la matrice de taux vitaux       ########################################

maxCohorte<-max(MATRICE_GEANTE[,"COHORTE"])   #donne nombre de cohortes
MAT_GEANTE_TAUX=vector(mode="list",length=maxCohorte)
minCohorte=min(MATRICE_GEANTE[,"COHORTE"])
for (i in 1 :maxCohorte){
  
  MAT_TAUX=matrix(NA,nrow=agemax,ncol=9)
  colnames(MAT_TAUX)<-c("COHORTE","AGE","n","REPRO","varREPRO","SURV","varSURV","mu.x","lx")
  
  MAT_TAUX[,"COHORTE"]= i
  MAT_TAUX[,"AGE"]=c(0:(agemax-1))
  
  dat<-MATRICE_GEANTE[MATRICE_GEANTE[,"COHORTE"]==i,]
  dat=as.data.frame(dat)
  if(nrow(dat)==0){ MAT_GEANTE_TAUX[[i]]=NA
  }else {
  dat<-na.omit(dat)
  dat<-dat[dat[,"REPRO"]!=-1,]
  if(nrow(dat)==0){ MAT_GEANTE_TAUX[[i]]=NA
  }else {
  repro<-tapply(dat[,"REPRO"],dat[,"AGE"],mean)        
  n<-  tapply(dat[,"REPRO"],dat[,"AGE"],length)
  MAT_TAUX[1:length(repro),"REPRO"]<-repro
  MAT_TAUX[1:length(n),"n"]<-n
  var.repro=tapply(dat[,"REPRO"],dat[,"AGE"],var)
  MAT_TAUX[1:length(var.repro),"varREPRO"]<-var.repro
  surv<-  tapply(dat[,"SURV"],dat[,"AGE"],mean)
  MAT_TAUX[1:length(surv),"SURV"]<-surv
  var.surv=tapply(dat[,"SURV"],dat[,"AGE"],var)
  MAT_TAUX[1:length(var.surv),"varSURV"]<-var.surv
  mu.x<-log(1/surv)
  MAT_TAUX[1:length(surv),"mu.x"]<-mu.x
  MAT_TAUX[1,"lx"]<-1
  for (j in 2:nrow(MAT_TAUX)){
    MAT_TAUX[j,"lx"]<-MAT_TAUX[j-1,"lx"]*MAT_TAUX[j-1,"SURV"]
  }
  
  MAT_GEANTE_TAUX[[i]]<-MAT_TAUX  # =liste de matrices de taux, une par cohorte
if(MAT_GEANTE_TAUX[[i]][1,"n"]<10){ MAT_GEANTE_TAUX[[i]]<-NA}
  }}}
#En sp?cifiant la cohorte voulue (1?8) dans MAT_GEANTE_TAUX[[i]], on obtient la table de survie de cette cohorte

################################                 MATRICES DE LESLIE            ########################################

MATRICE_LESLIE=vector(mode="list",length=maxCohorte)

for (i in 1 :maxCohorte){
  if (is.na(MAT_GEANTE_TAUX[[i]][i])==TRUE){MATRICE_LESLIE[[i]]=NA}
else{
     indice.na=which(is.na(MAT_GEANTE_TAUX[[i]][,"REPRO"]))[1]-1
     age.na=indice.na-1
     
     if(is.na(indice.na)){
       indice.na=agemax-1
       age.na=indice.na-1
     }

     l.vide= matrix(0,ncol=indice.na,nrow=age.na)
     
     #avec survie brute
     surv_leslie=MAT_GEANTE_TAUX[[i]][(1:age.na),"SURV"]
     
     diag(l.vide)=surv_leslie
    
     repro_leslie=MAT_GEANTE_TAUX[[i]][(1:indice.na),"REPRO"]
     
     MATRICE_LESLIE[[i]]=rbind(repro_leslie,l.vide)
}
}

# matrice bas?e sur taux mortalit? instantan? (mu.x) plut?t que taux brut (voir Moorad 2013)
MATRICE_LESLIE.mu.x=vector(mode="list",length=maxCohorte)

for (i in 1 :maxCohorte){
  if (is.na(MAT_GEANTE_TAUX[[i]][i])==TRUE){MATRICE_LESLIE.mu.x[[i]]=NA
  }
    else{
  indice.na=which(is.na(MAT_GEANTE_TAUX[[i]][,"REPRO"]))[1]-1
  age.na=indice.na-1
  
  if(is.na(indice.na)){
    indice.na=agemax-1
    age.na=indice.na-1
  }
  
  l.vide= matrix(0,ncol=indice.na,nrow=age.na)
    
  #avec survie=1-mu.x
  surv_leslie=1-MAT_GEANTE_TAUX[[i]][(1:age.na),"mu.x"]
  
  diag(l.vide)=surv_leslie
  
  repro_leslie=MAT_GEANTE_TAUX[[i]][(1:indice.na),"REPRO"]
  
  MATRICE_LESLIE.mu.x[[i]]=rbind(repro_leslie,l.vide)
}}
##En sp?cifiant la cohorte voulue (1?8) dans MATRICE_LESLIE.mu.x[[i]], on obtient la matrice de leslie de la cohorte
# o? la premi?re ligne est la fertilit? ? chaque ?ge et la sous-diagonale la survie

################################                 TAUX D'ACCROISSEMENT            #######################################

# taux de croissance bas? sur survie brute
TAUX_CROISSANCE=matrix(0, nrow=max(femmes$cohorte), ncol=2)
colnames(TAUX_CROISSANCE)=c("COHORTE","TAUX CROISSANCE")

 for (i in minCohorte : maxCohorte){
   if (is.na(MATRICE_LESLIE[[i]][i])==TRUE){
     TAUX_CROISSANCE[i,1]=i
     TAUX_CROISSANCE[i,2]=NA
   }
   else{
   eigen=eigen(MATRICE_LESLIE[[i]])
   taux.croissance=eigen$values[1]
   
   TAUX_CROISSANCE[i,1]=i
   TAUX_CROISSANCE[i,2]=taux.croissance
   
 }    }
     
TAUX_CROISSANCE<-as.numeric(TAUX_CROISSANCE[,2])    #vecteur des taux de croissance de toutes les cohortes

# taux de croissance bas? sur mu.x
TAUX_CROISSANCE.mu.x=matrix(0, nrow=max(femmes$cohorte), ncol=2)
colnames(TAUX_CROISSANCE.mu.x)=c("COHORTE","TAUX CROISSANCE")

for (i in 1 : maxCohorte){
  if (is.na(MATRICE_LESLIE.mu.x[[i]][i])==TRUE){
    TAUX_CROISSANCE.mu.x[i,1]=i
    TAUX_CROISSANCE.mu.x[i,2]=NA
  }
  else{
    eigen=eigen(MATRICE_LESLIE.mu.x[[i]])
    taux.croissance=eigen$values[1]
    
    TAUX_CROISSANCE.mu.x[i,1]=i
    TAUX_CROISSANCE.mu.x[i,2]=taux.croissance
    
  }    }

TAUX_CROISSANCE.mu.x<-as.numeric(TAUX_CROISSANCE.mu.x[,2])    #vecteur des taux de croissance de toutes les cohortes

################################                        FITNESS                 #######################################

MATRICE_GEANTE=cbind(MATRICE_GEANTE,"lm"=MATRICE_GEANTE[,"REPRO"]*MATRICE_GEANTE[,"SURV"])

# fitness bas? sur survie brute 

fit=(rep(NA, nrow(femmes)))
for(f in 1:nbfemmes){
  print(paste("femme",f,"sur",nbfemmes))

  rows=MATRICE_GEANTE[MATRICE_GEANTE[,1]==femmes[f,1]&MATRICE_GEANTE[,4]!=-1&!is.na(MATRICE_GEANTE[,4]),]
  if(class(rows)=="numeric") rows<-t(as.matrix(rows))
  
    if(length(rows)>0){
      cohorte<-rows[1,7]
      lambda<-TAUX_CROISSANCE[cohorte]

      fit[f]<-sum(rows[,8]*(lambda^-rows[,3]))
      }else{ fit[f]<-NA}
    }
  fitness=cbind(femmes,fit)

# fitness bas? sur mu.x
  fitmu=(rep(NA, nrow(femmes)))

for(f in 1:nbfemmes){
  print(paste("femme",f,"sur",nbfemmes))
  
    rows=MATRICE_GEANTE[MATRICE_GEANTE[,1]==femmes[f,1]&MATRICE_GEANTE[,4]!=-1&!is.na(MATRICE_GEANTE[,4]),]
    if(class(rows)=="numeric") rows<-t(as.matrix(rows))
    
    if(length(rows)>0){
      cohorte<-rows[1,7]
      lambda<-TAUX_CROISSANCE.mu.x[cohorte]
      
      fitmu[f]<-sum(rows[,8]*(lambda^-rows[,3]))
    }
  
}
  fitnessF=cbind(fitness,fitmu)
}
  ####HOMMES######
if(sexe==1 |is.na(sexe)==TRUE){
 ##preparation du jeu de donn?es##
  sex=1 
  femmes=dat[is.na(dat$sexe)==FALSE & dat$sexe== sex,] 
  femmes=femmes[is.na(femmes$birthy)==FALSE,] #que les femmes avec date de naissance
  femmes=femmes[femmes$birthy<maxyear,] #que les femmes avec date de naissance
  nbfemmes<-nrow(femmes) #nbre de femmes
  cohorte=trunc((femmes[,"birthy"]-min(dat$birthy,na.rm=TRUE))/interval+1)#d?fini le num?ro de cohorte
  femmes=cbind(femmes, cohorte)#ajoute le num?ro de cohorte au tableau
  
  
  ##########################           MATRICE PAR INDIVIDU           #######################
  seq1=c(0,0,0,0,1,0,0)
  MATRICE_GEANTE = matrix(0, nrow=0, ncol=7)
  colnames(MATRICE_GEANTE)<-c("FEMME","YEAR","AGE","REPRO","SURV","MARIAGE","COHORTE")
  
  for (f in 1:nbfemmes){
    
    print(paste("femme",f,"sur",nbfemmes))
    
    
    MAT=matrix(seq1, nrow= agemax, ncol= 7, byrow=TRUE)
    colnames(MAT)<-c("FEMME","YEAR","AGE","REPRO","SURV","MARIAGE","COHORTE")
    MAT[,"FEMME"]=femmes[f,"ind"]
    MAT[,"YEAR"]=c(femmes[f,"birthy"]:(femmes[f,"birthy"]+(agemax-1)))
    MAT[,"AGE"]<-c(0:(agemax-1))
    MAT[,"COHORTE"]<-femmes[f,"cohorte"]
    
    
    rows<-dat[dat$ind==femmes[f,"ind"],]    
    rows2<-dat[dat[,"pere"]==femmes[f,"ind"],] #sort les infos sur les enfants

    
    femmes[f,"FECONDITE"]=nrow(rows2)
    
    if(nrow(rows2)>0){
      
      for (x in 1:nrow(rows2)){
        if(!is.na(rows2$sexe[x])){
          MAT[is.element(MAT[,"YEAR"],rows2$birthy[x]),"REPRO"]<-MAT[is.element(MAT[,"YEAR"],rows2$birthy[x]),"REPRO"]+1
        }}
    }
    if (is.na(rows$dated[1])){
      
      max_ile=max(rows[,c(5,7)],na.rm=T)
      m=rows[,5]
      max_ile=max(c(max_ile,m))
      
      # ci-dessous on consid?re que les femmes sont pr?sentes au moins jusqu'? l'?ge limite (sp?cifi? au d?but) sauf si d?c?d?es avant
      ifelse(max_ile<(femmes[f,"birthy"]+age.limite),max_ile<-(femmes[f,"birthy"]+age.limite),max_ile<-max_ile)
      max_enf= max(rows2$birthy,na.rm=TRUE)
      if(is.na(max_enf) == FALSE & max_ile < max_enf){max_ile=max_enf}
      # ci-dessous pour ?ter les femmes du jeu de donn?es apr?s derni?re date de pr?sence attest?e/pr?sum?e...
      #MAT[MAT[,"YEAR"]>=max_ile+1,4:6]<-NA
      
      # ... ou alternativement on les consid?re comme mortes apr?s cette date
      # on consid?re que les femmes qui ne sont pas mortes avant l'?ge limite (parties ou pas) sont comme mortes, car elles ne participent pas ? la repro
      MAT[MAT[,"YEAR"]==max_ile+1,5]<-0
      MAT[MAT[,"YEAR"]>max_ile+1,4:6]<--1
    }
    
    
    if(!is.na(rows$dated[1]))
    { MAT[is.element(MAT[,"YEAR"],rows$dated[1]),"SURV"]<- MAT[is.element(MAT[,"YEAR"],rows$dated[1]),"SURV"]-1
    MAT[MAT[,"YEAR"]>rows$dated[1],4:6]<--1}
    
    
    
      MATRICE_GEANTE<-rbind(MATRICE_GEANTE,MAT)

    
  }
  
  nbfemmes<-nrow(femmes)   #donne les nombre de femmes du jeu qui restent apr?s le tri
  
  
  
  
  
  ################################          Cr?ation de la matrice de taux vitaux       ########################################
  
  maxCohorte<-max(MATRICE_GEANTE[,"COHORTE"])   #donne nombre de cohortes
  MAT_GEANTE_TAUX=vector(mode="list",length=maxCohorte)
  minCohorte=min(MATRICE_GEANTE[,"COHORTE"])
  for (i in 1 :maxCohorte){
    
    MAT_TAUX=matrix(NA,nrow=agemax,ncol=9)
    colnames(MAT_TAUX)<-c("COHORTE","AGE","n","REPRO","varREPRO","SURV","varSURV","mu.x","lx")
    
    MAT_TAUX[,"COHORTE"]= i
    MAT_TAUX[,"AGE"]=c(0:(agemax-1))
    
    dat<-MATRICE_GEANTE[MATRICE_GEANTE[,"COHORTE"]==i,]
    dat=as.data.frame(dat)
    if(nrow(dat)==0){ MAT_GEANTE_TAUX[[i]]=NA
    }else {
      dat<-na.omit(dat)
      dat<-dat[dat[,"REPRO"]!=-1,]
      if(nrow(dat)==0){ MAT_GEANTE_TAUX[[i]]=NA
      }else {
        
        repro<-tapply(dat[,"REPRO"],dat[,"AGE"],mean)        
        n<-  tapply(dat[,"REPRO"],dat[,"AGE"],length)
        MAT_TAUX[1:length(repro),"REPRO"]<-repro
        MAT_TAUX[1:length(n),"n"]<-n
        
        var.repro=tapply(dat[,"REPRO"],dat[,"AGE"],var)
        MAT_TAUX[1:length(var.repro),"varREPRO"]<-var.repro
        
        surv<-  tapply(dat[,"SURV"],dat[,"AGE"],mean)
        MAT_TAUX[1:length(surv),"SURV"]<-surv
        
        var.surv=tapply(dat[,"SURV"],dat[,"AGE"],var)
        MAT_TAUX[1:length(var.surv),"varSURV"]<-var.surv
        
        mu.x<-log(1/surv)
        MAT_TAUX[1:length(surv),"mu.x"]<-mu.x
        
        MAT_TAUX[1,"lx"]<-1
        for (j in 2:nrow(MAT_TAUX)){
          MAT_TAUX[j,"lx"]<-MAT_TAUX[j-1,"lx"]*MAT_TAUX[j-1,"SURV"]
        }
        
        MAT_GEANTE_TAUX[[i]]<-MAT_TAUX  # =liste de matrices de taux, une par cohorte
        if(MAT_GEANTE_TAUX[[i]][1,"n"]<10){ MAT_GEANTE_TAUX[[i]]<-NA}
      }}}
  #En sp?cifiant la cohorte voulue (1?8) dans MAT_GEANTE_TAUX[[i]], on obtient la table de survie de cette cohorte
  
  ################################                 MATRICES DE LESLIE            ########################################
  
  MATRICE_LESLIE=vector(mode="list",length=maxCohorte)
  
  for (i in 1 :maxCohorte){
    if (is.na(MAT_GEANTE_TAUX[[i]][i])==TRUE){MATRICE_LESLIE[[i]]=NA}
    else{
      indice.na=which(is.na(MAT_GEANTE_TAUX[[i]][,"REPRO"]))[1]-1
      age.na=indice.na-1
      
      if(is.na(indice.na)){
        indice.na=agemax-1
        age.na=indice.na-1
      }
      
      l.vide= matrix(0,ncol=indice.na,nrow=age.na)
      
      #avec survie brute
      surv_leslie=MAT_GEANTE_TAUX[[i]][(1:age.na),"SURV"]
      
      diag(l.vide)=surv_leslie
      
      repro_leslie=MAT_GEANTE_TAUX[[i]][(1:indice.na),"REPRO"]
      
      MATRICE_LESLIE[[i]]=rbind(repro_leslie,l.vide)
    }
  }
  
  # matrice bas?e sur taux mortalit? instantan? (mu.x) plut?t que taux brut (voir Moorad 2013)
  MATRICE_LESLIE.mu.x=vector(mode="list",length=maxCohorte)
  
  for (i in 1 :maxCohorte){
    if (is.na(MAT_GEANTE_TAUX[[i]][i])==TRUE){MATRICE_LESLIE.mu.x[[i]]=NA
    }
    else{
      indice.na=which(is.na(MAT_GEANTE_TAUX[[i]][,"REPRO"]))[1]-1
      age.na=indice.na-1
      
      if(is.na(indice.na)){
        indice.na=agemax-1
        age.na=indice.na-1
      }
      
      l.vide= matrix(0,ncol=indice.na,nrow=age.na)
      
      #avec survie=1-mu.x
      surv_leslie=1-MAT_GEANTE_TAUX[[i]][(1:age.na),"mu.x"]
      
      diag(l.vide)=surv_leslie
      
      repro_leslie=MAT_GEANTE_TAUX[[i]][(1:indice.na),"REPRO"]
      
      MATRICE_LESLIE.mu.x[[i]]=rbind(repro_leslie,l.vide)
    }}
  ##En sp?cifiant la cohorte voulue (1?8) dans MATRICE_LESLIE.mu.x[[i]], on obtient la matrice de leslie de la cohorte
  # o? la premi?re ligne est la fertilit? ? chaque ?ge et la sous-diagonale la survie
  
  ################################                 TAUX D'ACCROISSEMENT            #######################################
  
  # taux de croissance bas? sur survie brute
  TAUX_CROISSANCE=matrix(0, nrow=max(femmes$cohorte), ncol=2)
  colnames(TAUX_CROISSANCE)=c("COHORTE","TAUX CROISSANCE")
  
  for (i in minCohorte : maxCohorte){
    if (is.na(MATRICE_LESLIE[[i]][i])==TRUE){
      TAUX_CROISSANCE[i,1]=i
      TAUX_CROISSANCE[i,2]=NA
    }
    else{
      eigen=eigen(MATRICE_LESLIE[[i]])
      taux.croissance=eigen$values[1]
      
      TAUX_CROISSANCE[i,1]=i
      TAUX_CROISSANCE[i,2]=taux.croissance
      
    }    }
  
  TAUX_CROISSANCE<-as.numeric(TAUX_CROISSANCE[,2])    #vecteur des taux de croissance de toutes les cohortes
  
  # taux de croissance bas? sur mu.x
  TAUX_CROISSANCE.mu.x=matrix(0, nrow=max(femmes$cohorte), ncol=2)
  colnames(TAUX_CROISSANCE.mu.x)=c("COHORTE","TAUX CROISSANCE")
  
  for (i in 1 : maxCohorte){
    if (is.na(MATRICE_LESLIE.mu.x[[i]][i])==TRUE){
      TAUX_CROISSANCE.mu.x[i,1]=i
      TAUX_CROISSANCE.mu.x[i,2]=NA
    }
    else{
      eigen=eigen(MATRICE_LESLIE.mu.x[[i]])
      taux.croissance=eigen$values[1]
      
      TAUX_CROISSANCE.mu.x[i,1]=i
      TAUX_CROISSANCE.mu.x[i,2]=taux.croissance
      
    }    }
  
  TAUX_CROISSANCE.mu.x<-as.numeric(TAUX_CROISSANCE.mu.x[,2])    #vecteur des taux de croissance de toutes les cohortes
  
  ################################                        FITNESS                 #######################################
  
  MATRICE_GEANTE=cbind(MATRICE_GEANTE,"lm"=MATRICE_GEANTE[,"REPRO"]*MATRICE_GEANTE[,"SURV"])
  
  # fitness bas? sur survie brute 
  
  fit=(rep(NA, nrow(femmes)))
  for(f in 1:nbfemmes){
    print(paste("femme",f,"sur",nbfemmes))
    
    rows=MATRICE_GEANTE[MATRICE_GEANTE[,1]==femmes[f,1]&MATRICE_GEANTE[,4]!=-1&!is.na(MATRICE_GEANTE[,4]),]
    if(class(rows)=="numeric") rows<-t(as.matrix(rows))
    
    if(length(rows)>0){
      cohorte<-rows[1,7]
      lambda<-TAUX_CROISSANCE[cohorte]
      
      fit[f]<-sum(rows[,8]*(lambda^-rows[,3]))}
    else{ fit[f]<-NA}
  }
  fitness=cbind(femmes,fit)
  
  # fitness bas? sur mu.x
  fitmu=(rep(NA, nrow(femmes)))
  
  for(f in 1:nbfemmes){
    print(paste("femme",f,"sur",nbfemmes))
    
    rows=MATRICE_GEANTE[MATRICE_GEANTE[,1]==femmes[f,1]&MATRICE_GEANTE[,4]!=-1&!is.na(MATRICE_GEANTE[,4]),]
    if(class(rows)=="numeric") rows<-t(as.matrix(rows))
    
    if(length(rows)>0){
      cohorte<-rows[1,7]
      lambda<-TAUX_CROISSANCE.mu.x[cohorte]
      
      fitmu[f]<-sum(rows[,8]*(lambda^-rows[,3]))
    }
    
  }
  fitnessH=cbind(fitness,fitmu)
}
  
if(is.na(sexe)==TRUE) {fitTOT=rbind(fitnessF,fitnessH)}
if(is.na(sexe)==FALSE & sexe==1)  {fitTOT=fitnessH}
if(is.na(sexe)==FALSE & sexe==2)  {fitTOT=fitnessF}

datFIT=fitTOT[,c("ind","cohorte","fit","fitmu")]
 





#------ Version avec que les enfants filles:
  
fonly=TRUE
sexe=2


  ##preparation du jeu de donn?es##
  sex=2
  femmes=dat[is.na(dat$sexe) == FALSE & dat$sexe == sex,]
  femmes=femmes[is.na(femmes$birthy)==FALSE,] #que les femmes avec date de naissance
  femmes=femmes[femmes$birthy<maxyear,] #que les femmes avec date de naissance
  nbfemmes<-nrow(femmes) #nbre de femmes
  cohorte=trunc((femmes[,"birthy"]-min(dat$birthy,na.rm=TRUE))/interval+1)#d?fini le num?ro de cohorte
  femmes=cbind(femmes, cohorte)#ajoute le num?ro de cohorte au tableau

  
  ##########################           MATRICE PAR INDIVIDU           #######################
  seq1=c(0,0,0,0,1,0,0)
  MATRICE_GEANTE = matrix(0, nrow=0, ncol=7)
  colnames(MATRICE_GEANTE)<-c("FEMME","YEAR","AGE","REPRO","SURV","MARIAGE","COHORTE")
  
  for (f in 1:nbfemmes){
    
    print(paste("femme",f,"sur",nbfemmes))
    
    
    MAT=matrix(seq1, nrow= agemax, ncol= 7, byrow=TRUE)
    colnames(MAT)<-c("FEMME","YEAR","AGE","REPRO","SURV","MARIAGE","COHORTE")
    MAT[,"FEMME"]=femmes[f,"ind"]
    MAT[,"YEAR"]=c(femmes[f,"birthy"]:(femmes[f,"birthy"]+(agemax-1)))
    MAT[,"AGE"]<-c(0:(agemax-1))
    MAT[,"COHORTE"]<-femmes[f,"cohorte"]
    
    
    rows<-dat[dat$ind==femmes[f,"ind"],]    
    
    
    if(fonly == TRUE){rows2<-dat[dat[,"mere"]== femmes[f,"ind"] & dat[,"sexe"]== 2,]
    }else {rows2<-dat[dat[,"mere"]==femmes[f,"ind"],] }#sort les infos sur les enfants filles 
    
    femmes[f,"FECONDITE"]=nrow(rows2)
    
    if(nrow(rows2)>0){
      
      for (x in 1:nrow(rows2)){
        if(!is.na(rows2$sexe[x])){
          MAT[is.element(MAT[,"YEAR"],rows2$birthy[x]),"REPRO"]<-MAT[is.element(MAT[,"YEAR"],rows2$birthy[x]),"REPRO"]+1
        }}
    }
    if (is.na(rows$dated[1])){
      
      max_ile=max(rows[,c(5,7)],na.rm= TRUE)
      m=rows[,5]
      max_ile=max(c(max_ile,m))
      
      # ci-dessous on consid?re que les femmes sont pr?sentes au moins jusqu'? l'?ge limite (sp?cifi? au d?but) sauf si d?c?d?es avant
      ifelse(max_ile<(femmes[f,"birthy"]+age.limite),max_ile<-(femmes[f,"birthy"]+age.limite),max_ile<-max_ile)
      max_enf= max(rows2$birthy,na.rm= TRUE)
      if(is.na(max_enf) == FALSE & max_ile < max_enf){max_ile=max_enf}
      # ci-dessous pour ?ter les femmes du jeu de donn?es apr?s derni?re date de pr?sence attest?e/pr?sum?e...
      #MAT[MAT[,"YEAR"]>=max_ile+1,4:6]<-NA
      
      # ... ou alternativement on les consid?re comme mortes apr?s cette date
      # on consid?re que les femmes qui ne sont pas mortes avant l'?ge limite (parties ou pas) sont comme mortes, car elles ne participent pas ? la repro
      MAT[MAT[,"YEAR"] == max_ile+1,5]<-0
      MAT[MAT[,"YEAR"] > max_ile+1,4:6]<--1
    }
    
    
    if(!is.na(rows$dated[1]))
    { MAT[is.element(MAT[,"YEAR"],rows$dated[1]),"SURV"]<- MAT[is.element(MAT[,"YEAR"],rows$dated[1]),"SURV"]-1
    MAT[MAT[,"YEAR"]>rows$dated[1],4:6]<--1}
    
    
    
    #code pour ?liminer femmes sans enfants ET sans date de mort
      MATRICE_GEANTE<-rbind(MATRICE_GEANTE,MAT)
   
  
  }
  
  nbfemmes<-nrow(femmes)   #donne les nombre de femmes du jeu qui restent apr?s le tri
  
  
  
  ################################          Cr?ation de la matrice de taux vitaux       ########################################
  
  maxCohorte<-max(MATRICE_GEANTE[,"COHORTE"])   #donne nombre de cohortes
  MAT_GEANTE_TAUX=vector(mode="list",length=maxCohorte)
  minCohorte=min(MATRICE_GEANTE[,"COHORTE"])
  for (i in 1 :maxCohorte){
    
    MAT_TAUX=matrix(NA,nrow=agemax,ncol=9)
    colnames(MAT_TAUX)<-c("COHORTE","AGE","n","REPRO","varREPRO","SURV","varSURV","mu.x","lx")
    
    MAT_TAUX[,"COHORTE"]= i
    MAT_TAUX[,"AGE"]=c(0:(agemax-1))
    
    dat<-MATRICE_GEANTE[MATRICE_GEANTE[,"COHORTE"]==i,]
    dat=as.data.frame(dat)
    if(nrow(dat)==0){ MAT_GEANTE_TAUX[[i]]=NA
    }else {
      dat<-na.omit(dat)
      dat<-dat[dat[,"REPRO"]!=-1,]
      if(nrow(dat)==0){ MAT_GEANTE_TAUX[[i]]=NA
      }else {
        
        repro<-tapply(dat[,"REPRO"],dat[,"AGE"],mean)        
        n<-  tapply(dat[,"REPRO"],dat[,"AGE"],length)
        MAT_TAUX[1:length(repro),"REPRO"]<-repro
        MAT_TAUX[1:length(n),"n"]<-n
        
        var.repro=tapply(dat[,"REPRO"],dat[,"AGE"],var)
        MAT_TAUX[1:length(var.repro),"varREPRO"]<-var.repro
        
        surv<-  tapply(dat[,"SURV"],dat[,"AGE"],mean)
        MAT_TAUX[1:length(surv),"SURV"]<-surv
        
        var.surv=tapply(dat[,"SURV"],dat[,"AGE"],var)
        MAT_TAUX[1:length(var.surv),"varSURV"]<-var.surv
        
        mu.x<-log(1/surv)
        MAT_TAUX[1:length(surv),"mu.x"]<-mu.x
        
        MAT_TAUX[1,"lx"]<-1
        for (j in 2:nrow(MAT_TAUX)){
          MAT_TAUX[j,"lx"]<-MAT_TAUX[j-1,"lx"]*MAT_TAUX[j-1,"SURV"]
        }
        
        MAT_GEANTE_TAUX[[i]]<-MAT_TAUX  # =liste de matrices de taux, une par cohorte
        if(MAT_GEANTE_TAUX[[i]][1,"n"]<10){ MAT_GEANTE_TAUX[[i]]<-NA}
      }}}
  #En sp?cifiant la cohorte voulue (1?8) dans MAT_GEANTE_TAUX[[i]], on obtient la table de survie de cette cohorte
  
  ################################                 MATRICES DE LESLIE            ########################################
  
  MATRICE_LESLIE=vector(mode="list",length=maxCohorte)
  
  for (i in 1 :maxCohorte){
    if (is.na(MAT_GEANTE_TAUX[[i]][i])==TRUE){MATRICE_LESLIE[[i]]=NA}
    else{
      indice.na=which(is.na(MAT_GEANTE_TAUX[[i]][,"REPRO"]))[1]-1
      age.na=indice.na-1
      
      if(is.na(indice.na)){
        indice.na=agemax-1
        age.na=indice.na-1
      }
      
      l.vide= matrix(0,ncol=indice.na,nrow=age.na)
      
      #avec survie brute
      surv_leslie=MAT_GEANTE_TAUX[[i]][(1:age.na),"SURV"]
      
      diag(l.vide)=surv_leslie
      
      repro_leslie=MAT_GEANTE_TAUX[[i]][(1:indice.na),"REPRO"]
      
      MATRICE_LESLIE[[i]]=rbind(repro_leslie,l.vide)
    }
  }
  
  # matrice bas?e sur taux mortalit? instantan? (mu.x) plut?t que taux brut (voir Moorad 2013)
  MATRICE_LESLIE.mu.x=vector(mode="list",length=maxCohorte)
  
  for (i in 1 :maxCohorte){
    if (is.na(MAT_GEANTE_TAUX[[i]][i])==TRUE){MATRICE_LESLIE.mu.x[[i]]=NA
    }
    else{
      indice.na=which(is.na(MAT_GEANTE_TAUX[[i]][,"REPRO"]))[1]-1
      age.na=indice.na-1
      
      if(is.na(indice.na)){
        indice.na=agemax-1
        age.na=indice.na-1
      }
      
      l.vide= matrix(0,ncol=indice.na,nrow=age.na)
      
      #avec survie=1-mu.x
      surv_leslie=1-MAT_GEANTE_TAUX[[i]][(1:age.na),"mu.x"]
      
      diag(l.vide)=surv_leslie
      
      repro_leslie=MAT_GEANTE_TAUX[[i]][(1:indice.na),"REPRO"]
      
      MATRICE_LESLIE.mu.x[[i]]=rbind(repro_leslie,l.vide)
    }}
  ##En sp?cifiant la cohorte voulue (1?8) dans MATRICE_LESLIE.mu.x[[i]], on obtient la matrice de leslie de la cohorte
  # o? la premi?re ligne est la fertilit? ? chaque ?ge et la sous-diagonale la survie
  
  ################################                 TAUX D'ACCROISSEMENT            #######################################
  
  # taux de croissance bas? sur survie brute
  TAUX_CROISSANCE=matrix(0, nrow=max(femmes$cohorte), ncol=2)
  colnames(TAUX_CROISSANCE)=c("COHORTE","TAUX CROISSANCE")
  
  for (i in minCohorte : maxCohorte){
    if (is.na(MATRICE_LESLIE[[i]][i])==TRUE){
      TAUX_CROISSANCE[i,1]=i
      TAUX_CROISSANCE[i,2]=NA
    }
    else{
      eigen=eigen(MATRICE_LESLIE[[i]])
      taux.croissance=eigen$values[1]
      
      TAUX_CROISSANCE[i,1]=i
      TAUX_CROISSANCE[i,2]=taux.croissance
      
    }    }
  
  TAUX_CROISSANCE<-as.numeric(TAUX_CROISSANCE[,2])    #vecteur des taux de croissance de toutes les cohortes
  
  # taux de croissance bas? sur mu.x
  TAUX_CROISSANCE.mu.x=matrix(0, nrow=max(femmes$cohorte), ncol=2)
  colnames(TAUX_CROISSANCE.mu.x)=c("COHORTE","TAUX CROISSANCE")
  
  for (i in 1 : maxCohorte){
    if (is.na(MATRICE_LESLIE.mu.x[[i]][i])==TRUE){
      TAUX_CROISSANCE.mu.x[i,1]=i
      TAUX_CROISSANCE.mu.x[i,2]=NA
    }
    else{
      eigen=eigen(MATRICE_LESLIE.mu.x[[i]])
      taux.croissance=eigen$values[1]
      
      TAUX_CROISSANCE.mu.x[i,1]=i
      TAUX_CROISSANCE.mu.x[i,2]=taux.croissance
      
    }    }
  
  TAUX_CROISSANCE.mu.x<-as.numeric(TAUX_CROISSANCE.mu.x[,2])    #vecteur des taux de croissance de toutes les cohortes
  
  
  ################################                        FITNESS                 #######################################
  
  MATRICE_GEANTE=cbind(MATRICE_GEANTE,"lm"=MATRICE_GEANTE[,"REPRO"]*MATRICE_GEANTE[,"SURV"])
  
  # fitness bas? sur survie brute 
  
  fit=(rep(NA, nrow(femmes)))
  for(f in 1:nbfemmes){
    print(paste("femme",f,"sur",nbfemmes))
    
    rows=MATRICE_GEANTE[MATRICE_GEANTE[,1]==femmes[f,1]&MATRICE_GEANTE[,4]!=-1&!is.na(MATRICE_GEANTE[,4]),]
    if(class(rows)=="numeric") rows<-t(as.matrix(rows))
    
    if(length(rows)>0){
      cohorte<-rows[1,7]
      lambda<-TAUX_CROISSANCE[cohorte]
      
      fit[f]<-sum(rows[,8]*(lambda^-rows[,3]))}
    else{ fit[f]<-NA}
  }
  fitness=cbind(femmes,fit)
  
  # fitness bas? sur mu.x
  fitmu=(rep(NA, nrow(femmes)))
  
  for(f in 1:nbfemmes){
    print(paste("femme",f,"sur",nbfemmes))
    
    rows=MATRICE_GEANTE[MATRICE_GEANTE[,1]==femmes[f,1]&MATRICE_GEANTE[,4]!=-1&!is.na(MATRICE_GEANTE[,4]),]
    if(class(rows)=="numeric") rows<-t(as.matrix(rows))
    
    if(length(rows)>0){
      cohorte<-rows[1,7]
      lambda<-TAUX_CROISSANCE.mu.x[cohorte]
      
      fitmu[f]<-sum(rows[,8]*(lambda^-rows[,3]))
    }
    
  }
  fitnessF=cbind(fitness,fitmu)
  
  fit=fitnessF[,c("ind","fit","fitmu")]
  colnames(fit)=c("ind","fitGirls","fitmuGirls")
  datFIT=merge(datFIT,fit,by="ind",all.x=TRUE, all.y=TRUE)
  
  write.table(datFIT,file="datFIT2.txt")
