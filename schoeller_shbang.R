setwd("/Users/Warren.Sink/Desktop/OneDrive-Van-Andel-Institute")
library(tidyverse)
####### Phenotypic data######

#nonpaired

Co_twins_ID <- c('1','1','1','2','2','3','3','4','4','5','6','7','8','9','10','10','11','11','8','12','12','13','14','14','15','15','7','16','16','17','17','18','18','18','18','19',
                 '20','20','21','21','22','22','6','23','23','23','9','24','25','25','26','5','27','27','28','28','29','30','30','31','31','32','32','33','33','34','34',
                 '13','35','35','36','36','37','37','38','38','39','39','40','40','26','29','41','42','42','43','43','44','44','41','45','45','46','46','47','24','24','8','19','24','48',
                 '48','49','49','8','47','50','50','51','51',
                 '52','52','53','53','54','54',
                 '52_broad','52_broad','53_broad','53_broad',
                 'pos1','pos1','pos2','pos2','pos3','pos3','pos4','pos4','pos5','pos5',
                 'neg1','neg1','neg2','neg2','neg3','neg3','neg4','neg4','neg5','neg5')

Individual_ID <- c('1A','1B','1C','2A','2B','3A','3B','4A','4B','5A','6A','7A','8A','9A','10A','10B','11A','11B','8B','12A','12B','13A','14A','14B','15A','15B','7B','16A','16B','17A','17B','18A','18B','18C','18D','19A',
                   '20A','20B','21A','21B','22A','22B','6B','23A','23B','23C','9B','24A','25A','25B','26A','5B','27A','27B','28A','28B','29A','30A','30B','31A','31B','32A','32B','33A','33B','34A','34B',
                   '13B','35A','35B','36A','36B','37A','37B','38A','38B','39A','39B','40A','40B','26B','29B','41A','42A','42B','43A','43B','44A','44B','41B','45A','45B','46A','46B','47A','24B','24C','8C','19B','24D','48A',
                   '48B','49A','49B','8D','47B','50A','50B','51A','51B',
                   '52A','52B','53A','53B','54A','54B',
                   '52_hdA','52_hdB','53_hdA','53_hdB',
                   'pos1A','pos1B','pos2A','pos2B','pos3A','pos3B','pos4A','pos4B','pos5A','pos5B',
                   'neg1A','neg1B','neg2A','neg2B','neg3A','neg3B','neg4A','neg4B','neg5A','neg5B')

Names <- c("Allooh_Jonas","Allooh_Moses","Allooh_Noah","Andrews_Doug","Andrews_Ross",
           "Ayer_Carly","Ayer_Lily","Bowen_David","Bowen_Richard","Bowers_Brenda",
           "Braswell_Orlean","Bridges_Carolyn","Burns_Catherine","Burns_McKenzie","Catalano_Joe",
           "Catalano_Sal","Cepeda_Eurides","Cepeda_Ramon","Claucherty_Marion",
           "Croll-Baehre_Emma","Croll-Baehre_Marta","Davis_June","Demonet_David","Demonet_Larry",
           "Dwomoh-Piper_Chantelle","Dwomoh-Piper_Danielle","Elder_Marilyn","Estrada_Antonion",
           "Estrada_Jesus","Everingham_Douglas","Everingham_Steve","Furtick_Jason","Furtick_Keith",
           "Furtick_Kevin","Furtick_Victor","Garzilli_Denise","Ginley_Adalynn","Ginley_Breanna",
           "Goldberg_Adam","Goldberg_Peter","Haick_Ava","Haick_Isabel","Hudson_Irene","Humphery_Emma",
           "Humphery_Megan","Humphery_Sarah","Jackson_Sidney","Jacques_Mary","Jensen_Dane","Jensen_Jordan",
           "Kerner_Sharon","Key_Aidan","Kobylski_Cecelia","Kobylski_Elizabeth","Lipfird_Garry","Lipfird_Larry",
           "Maassen_Jamie","Martin_Kate","Martin_Emily","McClure_Bryan","McClure_Bryce","McLeod_Keisher",
           "McLeod_Teisher","Mielke_Alyssa","Mielke_Emily","Mitchell_Fred","Mitchell_Ned","Moyer_Jean","Mueller_Kirk",
           "Mueller_Nate","Nagel_Jeff","Nagel_Steve","Nelson_Audri","Nelson_Erin","Nick_Skyler",
           "Nick_Spencer","Oliver_David","Oliver_Walter","Parks_Katie","Parks_Sarah","Prijatel_Karen","Qualkinbush_Jodie","Rabi_Kate",
           "Ramsdell_Cole","Ramsdell_Seth","Ream_Alyssa","Ream_Rachel","Rowan_Hannah","Rowan_Kaci",
           "Scott-Wolf_Adriana","Serra_Ivory","Serra_Shelter","Smith_Connor","Smith_Kyle","Starr_Ellen",
           "Steeves_Carrie","Steeves_Patty","Stewart_Martha","Stillwell_Diane","Thornhill_Jennie","Timotiwu_Jason",
           "Timotiwu_Jordan","Tynski_Daniel","Tynski_Kristen","Ullman_Helen","Underwood_Helen","Whited_Jackie",
           "Whited_Jessica","Widerka_Amanda","Widerka_Beth",
           "Darren_1", "Darren_2", "Karen_1", "Karen_2", "Lois_1", "Lois_2",
           "Darren_2","Darren_2_broad","Karen_1","Karen_1_broad",
           "Elder_Marilyn(1)","Elder_Marilyn", "Everingham_Steve(1)","Everingham_Steve","Parks_Katie(1)","Parks_Katie", "Ramsdell_Cole(1)","Ramsdell_Cole","Starr_Ellen(1)","Starr_Ellen",
           "Andrews_Doug","Estrada_Antonion","McClure_Bryan","Mitchell_Fred","Nagel_Jeff", "Oliver_David", "Rowan_Hannah", "Starr_Ellen","Timotiwu_Jordan", 'Tynski_Daniel')

Sex <- c('M','M','M','M','M','F','F','M','M','F','F','F','F','F','M','M','M','M','F','F','F','F','M','M','F','F','F','M','M','M','M','M','M','M','M','F',
         'F','F','M','M','F','F','F','F','F','F','F','F','M','M','F','M','F','F','M','M','F','F','F','M','M','F','F','F','F','M','M',
         'F','M','M','M','M','F','F','M','M','M','M','F','F','F','F','F','M','M','F','F','F','F','F','M','M','M','M','F','F','F','F','F','F','M',
         'M','M','F','F','F','F','F','F','F',
         'M','M','F','F','F','F',
         'M','M','F','F',
         'F','F','M','M','F','F','M','M','F','F',
         'M','M','M',"M",'M','M','F','F','M','M')

Ethnicity <- c('caucasian','caucasian','caucasian','caucasian','caucasian','caucasian','caucasian','caucasian','caucasian','caucasian','black','caucasian','caucasian','caucasian','caucasian','caucasian','hispanic','hispanic','caucasian','caucasian','caucasian','caucasian','caucasian','caucasian','black','black','caucasian','caucasian','caucasian','caucasian','caucasian','black','black','black','black','caucasian',
               'black','black','caucasian','caucasian','caucasian','caucasian','black','caucasian','caucasian','caucasian','caucasian','caucasian','caucasian','caucasian','caucasian','caucasian','caucasian','caucasian','caucasian','caucasian','caucasian','caucasian','caucasian','caucasian','caucasian','black','black','caucasian','caucasian','black','black',
               'caucasian','caucasian','caucasian','caucasian','caucasian','caucasian','caucasian','caucasian','caucasian','caucasian','caucasian','caucasian','caucasian','caucasian','caucasian','asian','caucasian','caucasian','caucasian','caucasian','caucasian','caucasian','asian','caucasian','caucasian','caucasian','caucasian','black','caucasian','caucasian','caucasian','caucasian','caucasian','asian',
               'asian','caucasian','caucasian','caucasian','black','caucasian','caucasian','hispanic','hispanic',
               'caucasian','caucasian','caucasian','caucasian','caucasian','caucasian',
               'caucasian','caucasian','caucasian','caucasian',
               'caucasian','caucasian','caucasian','caucasian','caucasian','caucasian','caucasian','caucasian','black','black',
               'caucasian','caucasian','caucasian','black','caucasian','caucasian','caucasian','black','asian','caucasian')

Eye_color <- c('brown','brown','brown','blue','blue','blue','blue','brown','brown','brown','brown','brown','green','brown','brown','brown','brown','brown','green','blue','blue','blue','blue','blue','brown','brown','brown','brown','brown','brown','brown','brown','brown','brown','brown','blue',
               'brown','brown','blue','blue','blue','blue','brown','blue','blue','blue','brown','blue','blue','blue','blue','brown','blue','blue','blue','blue','blue','blue','blue','blue','blue','brown','brown','blue','blue','brown','brown',
               'blue','brown','brown','brown','brown','brown','brown','blue','blue','blue','blue','blue','blue','blue','blue','brown','brown','brown','green','green','blue','blue','brown','brown','brown','green','green','brown','blue','blue','green','blue','blue','brown',
               'brown','brown','brown','brown','brown','blue','blue','brown','brown',
               'green','green','blue','blue','blue','blue',
               'green','green','blue','blue',
               'brown','brown','brown','brown','blue','blue','brown','brown','brown','brown',
               'blue','brown','blue','brown','brown','blue','blue','brown','brown','brown')

Hair_color <- c('black','black','black','bald','bald','blond','blond','grey','grey','brown','brown','brown','red','black','black','black','black','black','black','red','red','grey','grey','grey','black','black','brown','black','black','grey','grey','black','black','black','black','brown',
                'black','black','bald','bald','brown','brown','brown','blond','blond','blond','blond','blond','blond','blond','blond','grey','brown','brown','grey','grey','brown','blond','blond','blond','blond','black','black','blond','blond','black','black',
                'grey','brown','brown','brown','brown','brown','brown','brown','brown','grey','grey','blond','blond','blond','brown','brown','brown','brown','red','red','blond','blond','brown','black','black','brown','brown','grey','blond','blond','black','brown','blond','black',
                'black','brown','brown','black','grey','blond','blond','brown','brown',
                'brown','brown','blond','blond','blond','blond',
                'brown','brown','blond','blond',
                'brown','brown','grey','grey','blond','blond','brown','brown','grey','grey',
                'bald','black','blond','black','brown','grey','blond','grey','black','brown')

Birth_year <- c('1985','1985','1985','1965','1965','2006','2006','1957','1957','1964','1945','1944','1958','1991','1943','1943','1977','1977','1958','1996','1996','1939','1940','1940','1987','1987','1944','1988','1988','1947','1947','1990','1990','1990','1990','1953',
                '2005','2005','1970','1970','2007','2007','1945','1998','1998','1998','1991','1982','1986','1986','1949','1964','1996','1996','1954','1954','1981','2002','2002','2000','2000','1977','1977','2000','2000','1950','1950',
                '1939','1984','1984','1967','1967','1977','1977','1992','1992','1936','1936','2001','2001','1949','1981','1982','1988','1988','1997','1997','1992','1992','1982','1972','1972','2001','2001','1942','1982','1982','1958','1953','1982','1994',
                '1994','1984','1984','1958','1942','1991','1991','2005','2005',
                '1992','1992','1972','1972','1952','1952',
                '1992','1992','1972','1972',
                '1944','1944','1947','1947','2001','2001','1988','1988','1942','1942',
                '1965','1988','2000','1950','1967','1936','1942','1942','1994','1984')

pheno_data <- data.frame(Names, Individual_ID, Co_twins_ID, Sex, Ethnicity, Birth_year, Eye_color, Hair_color)
pheno_data_noname <-
  pheno_data[,1:8] <- lapply(pheno_data[,1:8] , function(x) as.factor(as.character(x)))
pheno_data <- pheno_data %>% arrange(Co_twins_ID)
pheno_data$Birth_year = as.numeric(as.character(pheno_data$Birth_year))
pheno_data <- pheno_data %>% mutate(., Age = 2012-pheno_data$Birth_year)

rm(list = ls()[!ls() %in% c("pheno_data")])


####### Generate distances and ratios of distances x pheno######

#distance delta/FC

rd <- read.csv(file = 'landmarks_13_nose_normlized.csv', header = TRUE, stringsAsFactors = FALSE)
df1 <- read.csv(file = 'Gimp_features.csv', header = TRUE, stringsAsFactors = FALSE)
df3 <- read.csv(file = 'all_schoeller_neg_ctrls.csv', header = TRUE, stringsAsFactors = FALSE)
df2 <- read.csv(file = 'all_schoeller_pos_ctrls.csv', header = TRUE, stringsAsFactors = FALSE)
df <- read.csv(file = 'new_schoeller_noncontrols.csv', header = TRUE, stringsAsFactors = FALSE)

df <- slice(df, 2:31)
df1 <- slice(df1, 2:31)
df2 <- slice(df2, 2:31)
df3 <- slice(df3, 2:31)

rd <- full_join(rd,df,by="X")
rd <- full_join(rd,df1,by="X")
rd <- full_join(rd,df2,by="X")
rd <- full_join(rd,df3,by="X")
rd <- rd[c(-18),]

vect<-function(n){
  a=strsplit(strsplit(n,split=",")[[1]][1],split="[[]")[[1]][2]
  b=strsplit(strsplit(n,split=",")[[1]][2],split="[]]")[[1]][1]
  return(c(as.numeric(a),as.numeric(b)))
}

eud<-function(x1,y1,x2,y2){
  sqrt((x2-x1)**2+(y2-y1)**2)
}

L=list()
for (i in 1:(nrow(rd)-1)){
  for (j in (i+1):nrow(rd)){
    
    n1=rd[i,1]; n2=rd[j,1] 
    r=as.numeric()
    
    for (v in 2:dim(rd)[2]){
      im.att1=vect(rd[i,v]); im.att2=vect(rd[j,v]) 
      d1= eud(im.att1[1],im.att1[2],im.att2[1],im.att2[2])
      r=c(r,d1)
    }
    
    name=paste(n1,n2,sep="_")
    L[[name]]=r
    #cat(name,"\n")
  }
}

M=do.call(rbind,L)
write.csv(M,"shbang_features_distance.csv")

pairwise_ratio <- function(z){
  tot=choose(length(z),2)
  count=1
  ratio_feature =as.numeric(rep(0,tot))
  
  for (k in 1:(length(z)-1)){
    for (l in (k+1):length(z)){
      
      #ratio_feature = c(ratio_feature, z[k] / z[l])
      ratio_feature[count]=z[k]/z[l]
      
      count=count+1
      if (count%%5000==0){
        print(paste(round((count/tot)*100,1),"%",sep=""))
      }
      
    }
  }
  return(ratio_feature)
}

ratio_names=as.character(rep(0,choose(dim(M)[1],2)))
counter=0
for (k in 1:(length(M[,1])-1)){
  for (l in (k+1):length(M[,1])){
    n3=names(M[k,1]); n4=names(M[l,1]) 
    counter=counter+1
    name1=paste(n3,n4,sep="__")
    ratio_names[counter]=name1
  }
}

M.ratio=matrix(nrow=choose(dim(M)[1],2),ncol=dim(M)[2])
row.names(M.ratio)<-ratio_names

for (j in 1:dim(M)[2]){
  columnz = M[,j]
  column=pairwise_ratio(columnz)
  M.ratio[,j]=column
  cat(paste("column ",j," completed",sep=""))}

M_t <- t(M)
M.ratio_t <- t(M.ratio)
all_dist_pheno <- cbind(pheno_data,M_t[,1:406])
all_ratio_pheno <- cbind(pheno_data,M.ratio_t[,1:82215])

#test for eud distsances between twin pairs

rd<-t(rd)
colnames(rd)<-rd[1,]
rd<-rd[-1,]
rd_pheno<-cbind(pheno_data,rd[,1:29])

vect<-function(n){
  a=strsplit(strsplit(n,split=",")[[1]][1],split="[[]")[[1]][2]
  b=strsplit(strsplit(n,split=",")[[1]][2],split="[]]")[[1]][1]
  return(c(as.numeric(a),as.numeric(b)))
}

eud<-function(x1,y1,x2,y2){
  sqrt((x2-x1)**2+(y2-y1)**2)
}

rd_pheno[10:38] <- sapply(rd_pheno[10:38],as.character)

CoIds= rd_pheno[,3]

UniqIds=unique(CoIds)
      
L.delta<-list()

for (UniqId in UniqIds){
  
  idx<-as.numeric(which(CoIds==UniqId))
  
  for (v in 1:(length(idx)-1)){
    for (w in (v+1):length(idx)){
      i=idx[v]
      j=idx[w] 
      n1=rd_pheno[i,1]; n2=rd_pheno[j,1] 
      r=as.numeric()
      
      for (a in 10:dim(rd_pheno)[2]){
        im.att1=vect(rd_pheno[i,a]); im.att2=vect(rd_pheno[j,a]) 
        d1= eud(im.att1[1],im.att1[2],im.att2[1],im.att2[2])
        r=c(r,d1) 
      }
      
      name=paste(n1,n2,sep="_")
      L.delta[[name]]=r
      cat(name,"\n")
    }
  }
}

all_dist_pheno_eucd<-do.call(rbind,L.delta)
colnames(all_dist_pheno_eucd)<-colnames(rd_pheno)[10:ncol(rd_pheno)]
all_dist_pheno_eucd<-cbind(all_dist_pheno_fc[,c(1,413:419)],all_dist_pheno_eucd[,1:29])

#bind pheno and dist/ratio tables

all_dist_pheno <- cbind(pheno_data,M_t[,1:406])
all_ratio_pheno <- cbind(pheno_data,M.ratio_t[,1:82215])

all_dist_pheno <- cbind(pheno_data,M_t[,1:406])
all_ratio_pheno <- cbind(pheno_data,M.ratio_t[,1:82215])

rm(list = ls()[!ls() %in% c("pheno_data","all_dist_pheno","all_ratio_pheno","all_dist_pheno_eucd")])

all_dist_pheno <- all_dist_pheno%>%group_by(Co_twins_ID)%>%arrange(desc(ChinBottom_EyeLeft))
all_ratio_pheno <- all_ratio_pheno%>%group_by(Co_twins_ID)%>%arrange(desc(ChinBottom_EyeLeft__ChinBottom_EyeRight))

#####Delta/FC###########

#delta
CoIds= all_dist_pheno[,3]


UniqIds=unique(CoIds)

count=1
L.delta<-list()

for (UniqId in UniqIds){
  
  idx<-as.numeric(which(CoIds==UniqId))
  
  for (v in 1:(length(idx)-1)){
    for (w in (v+1):length(idx)){
      i=idx[v]
      j=idx[w]
      nam= paste0(all_dist_pheno[i,1],"__",all_dist_pheno[j,1])
      delta<-(as.numeric(all_dist_pheno[i,10:dim(all_dist_pheno)[2]]) -as.numeric(all_dist_pheno[j,10:dim(all_dist_pheno)[2]]))
      L.delta[[nam]]<-delta
      cat(nam,"\n")
    }
  }
}

all_dist_pheno_delta<-do.call(rbind,L.delta)
colnames(all_dist_pheno_delta)<-colnames(all_dist_pheno)[10:ncol(all_dist_pheno)]


#dist abs(delta)/fc

CoIds= all_dist_pheno[,3]


UniqIds=unique(CoIds)

count=1
L.ratio<-list()
L.delta<-list()

for (UniqId in UniqIds){
  
  idx<-as.numeric(which(CoIds==UniqId))
  
  for (v in 1:(length(idx)-1)){
    for (w in (v+1):length(idx)){
      i=idx[v]
      j=idx[w]
      
      nam= paste0(all_dist_pheno[i,1],"__",all_dist_pheno[j,1])
      ratio<-as.numeric(all_dist_pheno[i,10:dim(all_dist_pheno)[2]]) /as.numeric(all_dist_pheno[j,10:dim(all_dist_pheno)[2]])
      delta<-abs(as.numeric(all_dist_pheno[i,10:dim(all_dist_pheno)[2]]) -as.numeric(all_dist_pheno[j,10:dim(all_dist_pheno)[2]]))
      L.ratio[[nam]]<-ratio
      L.delta[[nam]]<-delta
      cat(nam,"\n")
    }
  }
}

all_dist_pheno_fc<-do.call(rbind,L.ratio)
colnames(all_dist_pheno_fc)<-colnames(all_dist_pheno)[10:ncol(all_dist_pheno)]
all_dist_pheno_absdelta<-do.call(rbind,L.delta)
colnames(all_dist_pheno_absdelta)<-colnames(all_dist_pheno)[10:ncol(all_dist_pheno)]

#ratio delta

CoIds= all_ratio_pheno[,3]


UniqIds=unique(CoIds)

count=1

L.delta<-list()

for (UniqId in UniqIds){
  
  idx<-as.numeric(which(CoIds==UniqId))
  
  for (v in 1:(length(idx)-1)){
    for (w in (v+1):length(idx)){
      i=idx[v]
      j=idx[w]
      
      nam= paste0(all_ratio_pheno[i,1],"__",all_ratio_pheno[j,1])
      delta<-abs(as.numeric(all_ratio_pheno[i,10:dim(all_ratio_pheno)[2]]) -as.numeric(all_ratio_pheno[j,10:dim(all_ratio_pheno)[2]]))
      L.delta[[nam]]<-delta
      cat(nam,"\n")
    }
  }
}

all_ratio_pheno_delta<-do.call(rbind,L.delta)
colnames(all_ratio_pheno_delta)<-colnames(all_ratio_pheno)[10:ncol(all_ratio_pheno)]

#ratio abs(delta)/fc

CoIds= all_ratio_pheno[,3]


UniqIds=unique(CoIds)

count=1
L.ratio<-list()
L.delta<-list()

for (UniqId in UniqIds){
  
  idx<-as.numeric(which(CoIds==UniqId))
  
  for (v in 1:(length(idx)-1)){
    for (w in (v+1):length(idx)){
      i=idx[v]
      j=idx[w]
      
      nam= paste0(all_ratio_pheno[i,1],"__",all_ratio_pheno[j,1])
      ratio<-as.numeric(all_ratio_pheno[i,10:dim(all_ratio_pheno)[2]]) /as.numeric(all_ratio_pheno[j,10:dim(all_ratio_pheno)[2]])
      delta<-abs(as.numeric(all_ratio_pheno[i,10:dim(all_ratio_pheno)[2]]) -as.numeric(all_ratio_pheno[j,10:dim(all_ratio_pheno)[2]]))
      L.ratio[[nam]]<-ratio
      L.delta[[nam]]<-delta
      cat(nam,"\n")
    }
  }
}
all_ratio_pheno_fc<-do.call(rbind,L.ratio)
colnames(all_ratio_pheno_fc)<-colnames(all_ratio_pheno)[10:ncol(all_ratio_pheno)]
all_ratio_pheno_absdelta<-do.call(rbind,L.delta)
colnames(all_ratio_pheno_absdelta)<-colnames(all_ratio_pheno)[10:ncol(all_ratio_pheno)]

#######Log2 and FC/Delta#######

#Log2FC twins tables
all_dist_pheno_log2 <- all_dist_pheno %>% group_by(Co_twins_ID)  %>%
  mutate_at(vars(names(.[,c(10:415)])), list(log2 = ~ log2(.))) %>%
  ungroup() %>% subset(., select = -c(10:415))
all_dist_pheno_abslog2 <- all_dist_pheno_log2 %>% group_by(Co_twins_ID)  %>%
  mutate_at(vars(names(.[,c(10:415)])), list(abslog2 = ~ abs(.))) %>%
  ungroup() %>% subset(., select = -c(10:415))
#all_ratio_pheno_log2 <- all_ratio_pheno %>% group_by(Co_twins_ID)  %>%
#mutate_at(vars(names(.[,c(10:82224)])), list(log2 = ~ log2(.))) %>%
#ungroup() %>% subset(., select = -c(10:82224))
#all_ratio_pheno_abslog2 <- all_ratio_pheno_log2 %>% group_by(Co_twins_ID)  %>%
#mutate_at(vars(names(.[,c(10:82224)])), list(abslog2 = ~ abs(.))) %>%
#ungroup() %>% subset(., select = -c(10:82224))

#log2 dist delta, fc, and absdelta

CoIds= all_dist_pheno[,3]


UniqIds=unique(CoIds)

count=1
L.ratio<-list()
L.delta<-list()

for (UniqId in UniqIds){
  
  idx<-as.numeric(which(CoIds==UniqId))
  
  for (v in 1:(length(idx)-1)){
    for (w in (v+1):length(idx)){
      i=idx[v]
      j=idx[w]
      
      nam= paste0(all_dist_pheno[i,1],"__",all_dist_pheno[j,1])
      ratio<-log2(as.numeric(all_dist_pheno[i,10:dim(all_dist_pheno)[2]])) / log2(as.numeric(all_dist_pheno[j,10:dim(all_dist_pheno)[2]]))
      delta<-log2(as.numeric(all_dist_pheno[i,10:dim(all_dist_pheno)[2]])) - log2(as.numeric(all_dist_pheno[j,10:dim(all_dist_pheno)[2]]))
      L.ratio[[nam]]<-ratio
      L.delta[[nam]]<-delta
      cat(nam,"\n")
    }
  }
}

all_dist_pheno_log2_fc<-do.call(rbind,L.ratio)
colnames(all_dist_pheno_log2_fc)<-colnames(all_dist_pheno)[10:ncol(all_dist_pheno)]
all_dist_pheno_log2_delta<-do.call(rbind,L.delta)
colnames(all_dist_pheno_log2_delta)<-colnames(all_dist_pheno)[10:ncol(all_dist_pheno)]

CoIds= all_dist_pheno[,3]


UniqIds=unique(CoIds)

count=1

L.delta<-list()

for (UniqId in UniqIds){
  
  idx<-as.numeric(which(CoIds==UniqId))
  
  for (v in 1:(length(idx)-1)){
    for (w in (v+1):length(idx)){
      i=idx[v]
      j=idx[w]
      
      nam= paste0(all_dist_pheno[i,1],"__",all_dist_pheno[j,1])
      delta<-abs(log2(as.numeric(all_dist_pheno[i,10:dim(all_dist_pheno)[2]])) - log2(as.numeric(all_dist_pheno[j,10:dim(all_dist_pheno)[2]])))
      L.delta[[nam]]<-delta
      cat(nam,"\n")
    }
  }
}

all_dist_pheno_log2_absdelta<-do.call(rbind,L.delta)
colnames(all_dist_pheno_log2_absdelta)<-colnames(all_dist_pheno)[10:ncol(all_dist_pheno)]

#abslog2 dist delta, fc, and absdelta

CoIds= all_dist_pheno[,3]


UniqIds=unique(CoIds)

count=1
L.ratio<-list()
L.delta<-list()

for (UniqId in UniqIds){
  
  idx<-as.numeric(which(CoIds==UniqId))
  
  for (v in 1:(length(idx)-1)){
    for (w in (v+1):length(idx)){
      i=idx[v]
      j=idx[w]
      
      nam= paste0(all_dist_pheno[i,1],"__",all_dist_pheno[j,1])
      ratio<-abs(log2(as.numeric(all_dist_pheno[i,10:dim(all_dist_pheno)[2]]))) / abs(log2(as.numeric(all_dist_pheno[j,10:dim(all_dist_pheno)[2]])))
      delta<-abs(log2(as.numeric(all_dist_pheno[i,10:dim(all_dist_pheno)[2]]))) - abs(log2(as.numeric(all_dist_pheno[j,10:dim(all_dist_pheno)[2]])))
      L.ratio[[nam]]<-ratio
      L.delta[[nam]]<-delta
      cat(nam,"\n")
    }
  }
}

all_dist_pheno_abslog2_fc<-do.call(rbind,L.ratio)
colnames(all_dist_pheno_abslog2_fc)<-colnames(all_dist_pheno)[10:ncol(all_dist_pheno)]
all_dist_pheno_abslog2_delta<-do.call(rbind,L.delta)
colnames(all_dist_pheno_abslog2_delta)<-colnames(all_dist_pheno)[10:ncol(all_dist_pheno)]

CoIds= all_dist_pheno[,3]


UniqIds=unique(CoIds)

count=1

L.delta<-list()

for (UniqId in UniqIds){
  
  idx<-as.numeric(which(CoIds==UniqId))
  
  for (v in 1:(length(idx)-1)){
    for (w in (v+1):length(idx)){
      i=idx[v]
      j=idx[w]
      
      nam= paste0(all_dist_pheno[i,1],"__",all_dist_pheno[j,1])
      delta<-abs(abs(log2(as.numeric(all_dist_pheno[i,10:dim(all_dist_pheno)[2]])) - abs(log2(as.numeric(all_dist_pheno[j,10:dim(all_dist_pheno)[2]])))))
      L.delta[[nam]]<-delta
      cat(nam,"\n")
    }
  }
}

all_dist_pheno_abslog2_absdelta<-do.call(rbind,L.delta)
colnames(all_dist_pheno_abslog2_absdelta)<-colnames(all_dist_pheno)[10:ncol(all_dist_pheno)]

#log2 and abslog2 ratio

CoIds= all_ratio_pheno[,3]


UniqIds=unique(CoIds)

count=1
L.logtwo<-list()
L.abslogtwo<-list()

for (UniqId in UniqIds){
  
  idx<-as.numeric(which(CoIds==UniqId))
  
  for (v in 1:length(idx)){
    i=idx[v]
    nam= paste0(all_ratio_pheno[i,1])
    logtwo<-log2(as.numeric(all_ratio_pheno[i,10:dim(all_ratio_pheno)[2]]))
    abslogtwo<-abs(log2(as.numeric(all_ratio_pheno[i,10:dim(all_ratio_pheno)[2]])))
    L.logtwo[[nam]]<-logtwo
    L.abslogtwo[[nam]]<-abslogtwo
    cat(nam,"\n")
  }
}
all_ratio_pheno_log2<-do.call(rbind,L.logtwo)
colnames(all_ratio_pheno_log2)<-colnames(all_ratio_pheno)[10:ncol(all_ratio_pheno)]
all_ratio_pheno_abslog2<-do.call(rbind,L.abslogtwo)
colnames(all_ratio_pheno_abslog2)<-colnames(all_ratio_pheno)[10:ncol(all_ratio_pheno)]

#log2 ratio delta, fc

CoIds= all_ratio_pheno[,3]


UniqIds=unique(CoIds)

count=1
L.ratio<-list()
L.delta<-list()

for (UniqId in UniqIds){
  
  idx<-as.numeric(which(CoIds==UniqId))
  
  for (v in 1:(length(idx)-1)){
    for (w in (v+1):length(idx)){
      i=idx[v]
      j=idx[w]
      
      nam= paste0(all_ratio_pheno[i,1],"__",all_ratio_pheno[j,1])
      ratio<-log2(as.numeric(all_ratio_pheno[i,10:dim(all_ratio_pheno)[2]])) / log2(as.numeric(all_ratio_pheno[j,10:dim(all_ratio_pheno)[2]]))
      delta<-log2(as.numeric(all_ratio_pheno[i,10:dim(all_ratio_pheno)[2]])) - log2(as.numeric(all_ratio_pheno[j,10:dim(all_ratio_pheno)[2]]))
      L.ratio[[nam]]<-ratio
      L.delta[[nam]]<-delta
      cat(nam,"\n")
    }
  }
}

all_ratio_pheno_log2_fc<-do.call(rbind,L.ratio)
colnames(all_ratio_pheno_log2_fc)<-colnames(all_ratio_pheno)[10:ncol(all_ratio_pheno)]
all_ratio_pheno_log2_delta<-do.call(rbind,L.delta)
colnames(all_ratio_pheno_log2_delta)<-colnames(all_ratio_pheno)[10:ncol(all_ratio_pheno)]

#log2 absdelta

CoIds= all_ratio_pheno[,3]


UniqIds=unique(CoIds)

count=1

L.delta<-list()

for (UniqId in UniqIds){
  
  idx<-as.numeric(which(CoIds==UniqId))
  
  for (v in 1:(length(idx)-1)){
    for (w in (v+1):length(idx)){
      i=idx[v]
      j=idx[w]
      
      nam= paste0(all_ratio_pheno[i,1],"__",all_ratio_pheno[j,1])
      delta<-abs(log2(as.numeric(all_ratio_pheno[i,10:dim(all_ratio_pheno)[2]])) - log2(as.numeric(all_ratio_pheno[j,10:dim(all_ratio_pheno)[2]])))
      L.delta[[nam]]<-delta
      cat(nam,"\n")
    }
  }
}

all_ratio_pheno_log2_absdelta<-do.call(rbind,L.delta)
colnames(all_ratio_pheno_log2_absdelta)<-colnames(all_ratio_pheno)[10:ncol(all_ratio_pheno)]


#abslog2 ratio delta, fc, and absdelta

CoIds= all_ratio_pheno[,3]


UniqIds=unique(CoIds)

count=1
L.ratio<-list()
L.delta<-list()

for (UniqId in UniqIds){
  
  idx<-as.numeric(which(CoIds==UniqId))
  
  for (v in 1:(length(idx)-1)){
    for (w in (v+1):length(idx)){
      i=idx[v]
      j=idx[w]
      
      nam= paste0(all_ratio_pheno[i,1],"__",all_ratio_pheno[j,1])
      ratio<-abs(log2(as.numeric(all_ratio_pheno[i,10:dim(all_ratio_pheno)[2]]))) / abs(log2(as.numeric(all_ratio_pheno[j,10:dim(all_ratio_pheno)[2]])))
      delta<-abs(log2(as.numeric(all_ratio_pheno[i,10:dim(all_ratio_pheno)[2]]))) - abs(log2(as.numeric(all_ratio_pheno[j,10:dim(all_ratio_pheno)[2]])))
      L.ratio[[nam]]<-ratio
      L.delta[[nam]]<-delta
      cat(nam,"\n")
    }
  }
}

all_ratio_pheno_abslog2_fc<-do.call(rbind,L.ratio)
colnames(all_ratio_pheno_abslog2_fc)<-colnames(all_ratio_pheno)[10:ncol(all_ratio_pheno)]
all_ratio_pheno_abslog2_delta<-do.call(rbind,L.delta)
colnames(all_ratio_pheno_abslog2_delta)<-colnames(all_ratio_pheno)[10:ncol(all_ratio_pheno)]

CoIds= all_ratio_pheno[,3]

UniqIds=unique(CoIds)

count=1
L.delta<-list()

for (UniqId in UniqIds){
  
  idx<-as.numeric(which(CoIds==UniqId))
  
  for (v in 1:(length(idx)-1)){
    for (w in (v+1):length(idx)){
      i=idx[v]
      j=idx[w]
      
      nam= paste0(all_ratio_pheno[i,1],"__",all_ratio_pheno[j,1])
      delta<-abs(abs(log2(as.numeric(all_ratio_pheno[i,10:dim(all_ratio_pheno)[2]])) - log2(as.numeric(all_ratio_pheno[j,10:dim(all_ratio_pheno)[2]]))))
      L.delta[[nam]]<-delta
      cat(nam,"\n")
    }
  }
}

all_ratio_pheno_abslog2_absdelta<-do.call(rbind,L.delta)
colnames(all_ratio_pheno_abslog2_absdelta)<-colnames(all_ratio_pheno)[10:ncol(all_ratio_pheno)]


####### getting rid of NAs + paired pheno x paired dfs########

#getting rid of NAs

all_dist_pheno_fc[sapply(all_dist_pheno_fc, is.infinite)] <- NA
all_dist_pheno_fc<-all_dist_pheno_fc[, colSums(is.na(all_dist_pheno_fc)) != nrow(all_dist_pheno_fc)]

all_dist_pheno_log2[sapply(all_dist_pheno_log2, is.infinite)] <- NA
all_dist_pheno_log2<-all_dist_pheno_log2[, colSums(is.na(all_dist_pheno_log2)) != nrow(all_dist_pheno_log2)]

all_dist_pheno_log2_fc[sapply(all_dist_pheno_log2_fc, is.infinite)] <- NA
all_dist_pheno_log2_fc<-all_dist_pheno_log2_fc[, colSums(is.na(all_dist_pheno_log2_fc)) != nrow(all_dist_pheno_log2_fc)]

all_dist_pheno_log2_delta[sapply(all_dist_pheno_log2_delta, is.infinite)] <- NA
all_dist_pheno_log2_delta<-all_dist_pheno_log2_delta[, colSums(is.na(all_dist_pheno_log2_delta)) != nrow(all_dist_pheno_log2)]

all_dist_pheno_log2_absdelta[sapply(all_dist_pheno_log2_absdelta, is.infinite)] <- NA
all_dist_pheno_log2_absdelta<-all_dist_pheno_log2_absdelta[, colSums(is.na(all_dist_pheno_log2_absdelta)) != nrow(all_dist_pheno_log2_absdelta)]

all_dist_pheno_abslog2[sapply(all_dist_pheno_abslog2, is.infinite)] <- NA
all_dist_pheno_abslog2<-all_dist_pheno_abslog2[, colSums(is.na(all_dist_pheno_abslog2)) != nrow(all_dist_pheno_abslog2)]

all_dist_pheno_abslog2_fc[sapply(all_dist_pheno_abslog2_fc, is.infinite)] <- NA
all_dist_pheno_abslog2_fc<-all_dist_pheno_abslog2_fc[, colSums(is.na(all_dist_pheno_abslog2_fc)) != nrow(all_dist_pheno_abslog2_fc)]

all_dist_pheno_abslog2_delta[sapply(all_dist_pheno_abslog2_delta, is.infinite)] <- NA
all_dist_pheno_abslog2_delta<-all_dist_pheno_abslog2_delta[, colSums(is.na(all_dist_pheno_abslog2_delta)) != nrow(all_dist_pheno_abslog2)]

all_dist_pheno_abslog2_absdelta[sapply(all_dist_pheno_abslog2_absdelta, is.infinite)] <- NA
all_dist_pheno_abslog2_absdelta<-all_dist_pheno_abslog2_absdelta[, colSums(is.na(all_dist_pheno_abslog2_absdelta)) != nrow(all_dist_pheno_abslog2_absdelta)]

all_ratio_pheno[sapply(all_ratio_pheno, is.infinite)] <- NA
all_ratio_pheno<-all_ratio_pheno[, colSums(is.na(all_ratio_pheno)) != nrow(all_ratio_pheno)]

all_ratio_pheno_delta[sapply(all_ratio_pheno_delta, is.infinite)] <- NA
all_ratio_pheno_delta<-all_ratio_pheno_delta[, colSums(is.na(all_ratio_pheno_delta)) != nrow(all_ratio_pheno_delta)]

all_ratio_pheno_fc[sapply(all_ratio_pheno_fc, is.infinite)] <- NA
all_ratio_pheno_fc<-all_ratio_pheno_fc[, colSums(is.na(all_ratio_pheno_fc)) != nrow(all_ratio_pheno_fc)]

all_ratio_pheno_log2[sapply(all_ratio_pheno_log2, is.infinite)] <- NA
all_ratio_pheno_log2<-all_ratio_pheno_log2[, colSums(is.na(all_ratio_pheno_log2)) != nrow(all_ratio_pheno_log2)]

all_ratio_pheno_log2_fc[sapply(all_ratio_pheno_log2_fc, is.infinite)] <- NA
all_ratio_pheno_log2_fc<-all_ratio_pheno_log2_fc[, colSums(is.na(all_ratio_pheno_log2_fc)) != nrow(all_ratio_pheno_log2_fc)]

all_ratio_pheno_log2_delta[sapply(all_ratio_pheno_log2_delta, is.infinite)] <- NA
all_ratio_pheno_log2_delta<-all_ratio_pheno_log2_delta[, colSums(is.na(all_ratio_pheno_log2_delta)) != nrow(all_ratio_pheno_log2_delta)]

all_ratio_pheno_log2_absdelta[sapply(all_ratio_pheno_log2_absdelta, is.infinite)] <- NA
all_ratio_pheno_log2_absdelta<-all_ratio_pheno_log2_absdelta[, colSums(is.na(all_ratio_pheno_log2_absdelta)) != nrow(all_ratio_pheno_log2_absdelta)]

all_ratio_pheno_abslog2[sapply(all_ratio_pheno_abslog2, is.infinite)] <- NA
all_ratio_pheno_abslog2<-all_ratio_pheno_abslog2[, colSums(is.na(all_ratio_pheno_abslog2)) != nrow(all_ratio_pheno_abslog2)]

all_ratio_pheno_abslog2_fc[sapply(all_ratio_pheno_abslog2_fc, is.infinite)] <- NA
all_ratio_pheno_abslog2_fc<-all_ratio_pheno_abslog2_fc[, colSums(is.na(all_ratio_pheno_abslog2_fc)) != nrow(all_ratio_pheno_abslog2_fc)]

all_ratio_pheno_abslog2_delta[sapply(all_ratio_pheno_abslog2_delta, is.infinite)] <- NA
all_ratio_pheno_abslog2_delta<-all_ratio_pheno_abslog2_delta[, colSums(is.na(all_ratio_pheno_abslog2_delta)) != nrow(all_ratio_pheno_abslog2_delta)]

all_ratio_pheno_abslog2_absdelta[sapply(all_ratio_pheno_abslog2_absdelta, is.infinite)] <- NA
all_ratio_pheno_abslog2_absdelta<-all_ratio_pheno_abslog2_absdelta[, colSums(is.na(all_ratio_pheno_abslog2_absdelta)) != nrow(all_ratio_pheno_abslog2_absdelta)]

#paired pheno x paired dfs#

pheno_data_paired = pheno_data[,c(3:9)]
Co_twins_ID <- c('1','1','1','10','11','12','13','14','15','16','17','18','18','18','18',
                        '18','18','19','2','20','21','22','23','23','23','24','24','24','24','24',
                        '24','25','26','27','28','29','3','30','31','32','33','34','35','36','37','38',
                        '39','4','40','41','42','43','44','45','46','47','48','49','5','50','51','52',
                        '52_broad','53','53_broad','54','6','7','8','8','8','8','8','8','9',
                        'pos1','pos2','pos3','pos4','pos5','neg1','neg2','neg3','neg4','neg5')

all_dist_pheno_fc <- data.frame(all_dist_pheno_fc)
all_dist_pheno_fc <- cbind(Co_twins_ID, all_dist_pheno_fc)
all_dist_pheno_fc <- left_join(all_dist_pheno_fc, pheno_data_paired,by = "Co_twins_ID")
all_dist_pheno_fc <- distinct(all_dist_pheno_fc,ChinBottom_EyeLeft,.keep_all= TRUE)

all_dist_pheno_delta<-data.frame(all_dist_pheno_delta)
all_dist_pheno_delta<- cbind(Co_twins_ID, all_dist_pheno_delta)
all_dist_pheno_delta<-left_join(all_dist_pheno_delta, pheno_data_paired,by = "Co_twins_ID")
all_dist_pheno_delta <- distinct(all_dist_pheno_delta,ChinBottom_EyeLeft,.keep_all= TRUE)

all_dist_pheno_absdelta<-data.frame(all_dist_pheno_absdelta)
all_dist_pheno_absdelta<- cbind(Co_twins_ID, all_dist_pheno_absdelta)
all_dist_pheno_absdelta<-left_join(all_dist_pheno_absdelta, pheno_data_paired,by = "Co_twins_ID")
all_dist_pheno_absdelta <- distinct(all_dist_pheno_absdelta,ChinBottom_EyeLeft,.keep_all= TRUE)

all_dist_pheno_log2<-data.frame(all_dist_pheno_log2)
all_dist_pheno_abslog2<-data.frame(all_dist_pheno_abslog2)

all_dist_pheno_log2_delta<-data.frame(all_dist_pheno_log2_delta)
all_dist_pheno_log2_delta<- cbind(Co_twins_ID, all_dist_pheno_log2_delta)
all_dist_pheno_log2_delta<-left_join(all_dist_pheno_log2_delta, pheno_data_paired,by = "Co_twins_ID")
all_dist_pheno_log2_delta <- distinct(all_dist_pheno_log2_delta,ChinBottom_EyeLeft,.keep_all= TRUE)

all_dist_pheno_log2_absdelta<-data.frame(all_dist_pheno_log2_absdelta)
all_dist_pheno_log2_absdelta<- cbind(Co_twins_ID, all_dist_pheno_log2_absdelta)
all_dist_pheno_log2_absdelta<-left_join(all_dist_pheno_log2_absdelta, pheno_data_paired,by = "Co_twins_ID")
all_dist_pheno_log2_absdelta <- distinct(all_dist_pheno_log2_absdelta,ChinBottom_EyeLeft,.keep_all= TRUE)

all_dist_pheno_log2_fc<-data.frame(all_dist_pheno_log2_fc)
all_dist_pheno_log2_fc<- cbind(Co_twins_ID, all_dist_pheno_log2_fc)
all_dist_pheno_log2_fc<-left_join(all_dist_pheno_log2_fc, pheno_data_paired,by = "Co_twins_ID")
all_dist_pheno_log2_fc <- distinct(all_dist_pheno_log2_fc,ChinBottom_EyeLeft,.keep_all= TRUE)

all_dist_pheno_abslog2_delta<-data.frame(all_dist_pheno_abslog2_delta)
all_dist_pheno_abslog2_delta<- cbind(Co_twins_ID, all_dist_pheno_abslog2_delta)
all_dist_pheno_abslog2_delta<-left_join(all_dist_pheno_abslog2_delta, pheno_data_paired,by = "Co_twins_ID")
all_dist_pheno_abslog2_delta <- distinct(all_dist_pheno_abslog2_delta,ChinBottom_EyeLeft,.keep_all= TRUE)

all_dist_pheno_abslog2_absdelta<-data.frame(all_dist_pheno_abslog2_absdelta)
all_dist_pheno_abslog2_absdelta<- cbind(Co_twins_ID, all_dist_pheno_abslog2_absdelta)
all_dist_pheno_abslog2_absdelta<-left_join(all_dist_pheno_abslog2_absdelta, pheno_data_paired,by = "Co_twins_ID")
all_dist_pheno_abslog2_absdelta <- distinct(all_dist_pheno_abslog2_absdelta,ChinBottom_EyeLeft,.keep_all= TRUE)

all_dist_pheno_abslog2_fc<-data.frame(all_dist_pheno_abslog2_fc)
all_dist_pheno_abslog2_fc<- cbind(Co_twins_ID, all_dist_pheno_abslog2_fc)
all_dist_pheno_abslog2_fc<-left_join(all_dist_pheno_abslog2_fc, pheno_data_paired,by = "Co_twins_ID")
all_dist_pheno_abslog2_fc <- distinct(all_dist_pheno_abslog2_fc,ChinBottom_EyeLeft,.keep_all= TRUE)

all_ratio_pheno_fc <- data.frame(all_ratio_pheno_fc)
all_ratio_pheno_fc <- cbind(Co_twins_ID, all_ratio_pheno_fc)
all_ratio_pheno_fc <- left_join(all_ratio_pheno_fc, pheno_data_paired,by = "Co_twins_ID")
all_ratio_pheno_fc <- distinct(all_ratio_pheno_fc,ChinBottom_EyeLeft__ChinBottom_EyeRight,.keep_all= TRUE)

all_ratio_pheno_delta<-data.frame(all_ratio_pheno_delta)
all_ratio_pheno_delta<- cbind(Co_twins_ID, all_ratio_pheno_delta)
all_ratio_pheno_delta<-left_join(all_ratio_pheno_delta, pheno_data_paired,by = "Co_twins_ID")
all_ratio_pheno_delta <- distinct(all_ratio_pheno_delta,ChinBottom_EyeLeft__ChinBottom_EyeRight,.keep_all= TRUE)

all_ratio_pheno_absdelta<-data.frame(all_ratio_pheno_absdelta)
all_ratio_pheno_absdelta<- cbind(Co_twins_ID, all_ratio_pheno_absdelta)
all_ratio_pheno_absdelta<-left_join(all_ratio_pheno_absdelta, pheno_data_paired,by = "Co_twins_ID")
all_ratio_pheno_absdelta <- distinct(all_ratio_pheno_absdelta,ChinBottom_EyeLeft__ChinBottom_EyeRight,.keep_all= TRUE)

all_ratio_pheno_log2<-data.frame(all_ratio_pheno_log2)
all_ratio_pheno_abslog2<-data.frame(all_ratio_pheno_abslog2)

all_ratio_pheno_log2_delta<-data.frame(all_ratio_pheno_log2_delta)
all_ratio_pheno_log2_delta<- cbind(Co_twins_ID, all_ratio_pheno_log2_delta)
all_ratio_pheno_log2_delta<-left_join(all_ratio_pheno_log2_delta, pheno_data_paired,by = "Co_twins_ID")
all_ratio_pheno_log2_delta <- distinct(all_ratio_pheno_log2_delta,ChinBottom_EyeLeft__ChinBottom_EyeRight,.keep_all= TRUE)

all_ratio_pheno_log2_absdelta<-data.frame(all_ratio_pheno_log2_absdelta)
all_ratio_pheno_log2_absdelta<- cbind(Co_twins_ID, all_ratio_pheno_log2_absdelta)
all_ratio_pheno_log2_absdelta<-left_join(all_ratio_pheno_log2_absdelta, pheno_data_paired,by = "Co_twins_ID")
all_ratio_pheno_log2_absdelta <- distinct(all_ratio_pheno_log2_absdelta,ChinBottom_EyeLeft__ChinBottom_EyeRight,.keep_all= TRUE)

all_ratio_pheno_log2_fc<-data.frame(all_ratio_pheno_log2_fc)
all_ratio_pheno_log2_fc<- cbind(Co_twins_ID, all_ratio_pheno_log2_fc)
all_ratio_pheno_log2_fc<-left_join(all_ratio_pheno_log2_fc, pheno_data_paired,by = "Co_twins_ID")
all_ratio_pheno_log2_fc <- distinct(all_ratio_pheno_log2_fc,ChinBottom_EyeLeft__ChinBottom_EyeRight,.keep_all= TRUE)

all_ratio_pheno_abslog2_fc<-data.frame(all_ratio_pheno_abslog2_fc)
all_ratio_pheno_abslog2_fc<- cbind(Co_twins_ID, all_ratio_pheno_abslog2_fc)
all_ratio_pheno_abslog2_fc<-left_join(all_ratio_pheno_abslog2_fc, pheno_data_paired,by = "Co_twins_ID")
all_ratio_pheno_abslog2_fc <- distinct(all_ratio_pheno_abslog2_fc,ChinBottom_EyeLeft__ChinBottom_EyeRight,.keep_all= TRUE)

all_ratio_pheno_abslog2_delta<-data.frame(all_ratio_pheno_abslog2_delta)
all_ratio_pheno_abslog2_delta<- cbind(Co_twins_ID, all_ratio_pheno_abslog2_delta)
all_ratio_pheno_abslog2_delta<-left_join(all_ratio_pheno_abslog2_delta, pheno_data_paired,by = "Co_twins_ID")
all_ratio_pheno_abslog2_delta <- distinct(all_ratio_pheno_abslog2_delta,ChinBottom_EyeLeft__ChinBottom_EyeRight,.keep_all= TRUE)

all_ratio_pheno_abslog2_absdelta<-data.frame(all_ratio_pheno_abslog2_absdelta)
all_ratio_pheno_abslog2_absdelta<- cbind(Co_twins_ID, all_ratio_pheno_abslog2_absdelta)
all_ratio_pheno_abslog2_absdelta<-left_join(all_ratio_pheno_abslog2_absdelta, pheno_data_paired,by = "Co_twins_ID")
all_ratio_pheno_abslog2_absdelta <- distinct(all_ratio_pheno_abslog2_absdelta,ChinBottom_EyeLeft__ChinBottom_EyeRight,.keep_all= TRUE)

save(all_ratio_pheno_delta, file="all_ratio_pheno_delta_real.Rdata")
save(all_ratio_pheno_absdelta, file="all_ratio_pheno_absdelta.Rdata")
save(all_ratio_pheno_fc, file="all_ratio_pheno_fc.Rdata")
save(all_ratio_pheno_abslog2_delta, file="all_ratio_pheno_abslog2_delta.Rdata")
save(all_ratio_pheno_abslog2_absdelta, file="all_ratio_pheno_abslog2_absdelta.Rdata")
save(all_ratio_pheno_abslog2_fc, file="all_ratio_pheno_abslog2_fc.Rdata")


########hpc-ratio-tsne trampoline########

write.table(x=all_ratio_pheno[,10:ncol(all_ratio_pheno)], file="/secondary/projects/mnp/warren/test/all_ratio_2.tab", sep ="\t",  col.names = F, row.names = F, quote=F)
write.table(x=all_ratio_pheno_delta[,1:ncol(all_ratio_pheno_delta)], file="/secondary/projects/mnp/warren/test/all_ratio_delta.tab", sep ="\t",  col.names = F, row.names = F, quote=F)
write.table(x=all_ratio_pheno_fc[,1:ncol(all_ratio_pheno_fc)], file="/secondary/projects/mnp/warren/test/all_ratio_fc.tab", sep ="\t",  col.names = F, row.names = F, quote=F)

tab_all_ratio_pheno_absdelta<-read.table(file="/Volumes/projects_secondary/mnp/warren/test/tsne_all_ratio_delta.tab", sep ="\t", header=F)
tab_all_ratio_pheno_fc<-read.table(file="/Volumes/projects_secondary/mnp/warren/test/tsne_all_ratio_fc.tab", sep ="\t", header=F)

all_ratio_pheno_delta_tsne<-cbind(all_dist_pheno_fc[c(1,413:419)],tab_all_ratio_pheno_absdelta)
all_ratio_pheno_fc_tsne<-cbind(all_dist_pheno_fc[c(1,413:419)],tab_all_ratio_pheno_fc)

all_ratio_pheno_fc_tsne_male<-all_ratio_pheno_fc_tsne%>%filter(.,Sex.y=="M")
all_ratio_pheno_fc_tsne_female<-all_ratio_pheno_fc_tsne%>%filter(.,Sex.y=="F")
all_ratio_pheno_delta_tsne_male<-all_ratio_pheno_delta_tsne%>%filter(.,Sex.y=='M')
all_ratio_pheno_delta_tsne_female<-all_ratio_pheno_delta_tsne%>%filter(.,Sex.y=='F')

rm(list = ls()[!ls() %in% c("pheno_data","all_dist_pheno","all_ratio_pheno",'all_dist_pheno_delta',
                            'all_dist_pheno_fc','all_ratio_pheno_delta','all_ratio_pheno_fc',
                            "all_dist_pheno_log2","all_dist_pheno_abslog2", "all_dist_pheno_log2_fc",
                            'all_dist_pheno_log2_delta','all_dist_pheno_abslog2_fc','all_dist_pheno_abslog2_delta',
                            "all_ratio_pheno_log2","all_ratio_pheno_abslog2",'all_ratio_pheno_log2_fc',
                            'all_ratio_pheno_log2_delta',"all_ratio_pheno_abslog2_fc","all_ratio_pheno_abslog2_delta",
                            "all_ratio_pheno_delta_tsne","all_ratio_pheno_fc_tsne","all_ratio_pheno_fc_tsne_male",
                            "all_ratio_pheno_fc_tsne_female","all_ratio_pheno_delta_tsne_male",
                            "all_ratio_pheno_delta_tsne_female","all_dist_pheno_eucd")])


####Elimination of highly correlative or retention of most covariate elements#####

tab_all_ratio_pheno_delta_corr<-read.table(file="/Volumes/projects_secondary/mnp/warren/correlation/alpd_corr.tab", sep ="\t", header=F)
tab_all_ratio<-read.table(file="/Volumes/projects_secondary/mnp/warren/test/tsne_ratiotable.tab", sep ="\t", header=F)
tab_all_ratio<-read.table(file="/Volumes/projects_secondary/mnp/warren/test/tsne_ratiotable.tab", sep ="\t", header=F)




##correlation matrix##
write.csv(all_ratio_pheno_delta,"schoeller_all_ratio_pheno_delta.csv")
write.csv(all_ratio_pheno_fc,"schoeller_all_ratio_pheno_fc.csv")
write.csv(all_ratio_pheno_log2_delta,"schoeller_all_ratio_pheno_log2_delta.csv")
write.csv(all_ratio_pheno_log2_fc,"schoeller_all_ratio_pheno_log2_fc.csv")
write.csv(all_ratio_pheno_abslog2_delta,"schoeller_all_ratio_pheno_abslog2_delta.csv")
write.csv(all_ratio_pheno_abslog2_fc,"schoeller_all_ratio_pheno_abslog2_fc.csv")


library(corrplot)
correlation_matrix_ratio_delta <- cor(all_ratio_pheno_delta, all_ratio_pheno_delta, method = "pearson" )
correlation_matrix_ratio_fc <- cor(all_ratio_pheno_fc, all_ratio_pheno_fc, method = "pearson" )
#correlation_matrix <- cor(data[c(6)] , data[c(11:50)] , use="complete.obs" , method = "pearson" ) ###this is to skip NA data###

library("Hmisc") ##this is to obtain pval associated to correlation matrix##
all_ratio_pheno_delta_mat<-as.matrix(all_ratio_pheno_delta)
all_ratio_pheno_fc_mat<-as.matrix(all_ratio_pheno_fc)
correlation_matrix_ratio_delta_2 <- rcorr(all_ratio_pheno_delta_mat , all_ratio_pheno_delta_mat , type  = "pearson" )
correlation_matrix_ratio_fc_2 <- rcorr(as.matrix(all_ratio_pheno_fc) , as.matrix(data[all_ratio_pheno_fc]) , type  = "pearson" )
correlation_matrix_ratio_delta_2$r ##Extract the correlation coefficients##
correlation_matrix_ratio_delta_2$P ##Extract p-values##
correlation_matrix_ratio_fc_2$r
correlation_matrix_ratio_fc_2$P

##this is to format the correlation matrix##

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
} 
flattenCorrMatrix(correlation_matrix_2$r, correlation_matrix_2$P)
flatten_corr_matrix <- flattenCorrMatrix(correlation_matrix_2$r, correlation_matrix_2$P)
View(flatten_corr_matrix)
##the following is for correlation matrix with corrplot##
library(corrplot)
correlation_matrix_total <- cor(data[c(6,11:50)] , data[c(6,11:50)] , use="complete.obs" , method = "pearson" )
corrplot(correlation_matrix_total, type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45, tl.cex = 0.7)
##this is to add pval information##
corrplot(correlation_matrix_2$r, type="upper", order="hclust",
         p.mat = correlation_matrix_2$P, sig.level = 0.05, insig = "blank", tl.col = "black", tl.srt = 45, tl.cex = 0.7)









###########PCA########

library("FactoMineR")
library("factoextra")

pca <- prcomp(all_dist_pheno[10:415])
groups <- as.factor(all_dist_pheno$Sex) 
fviz_pca_ind(pca, pointsize = 10, axes = c(1, 2), repel = T, geom = "point",
             mean.point = F, 
             pointshape = c(21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,
                            21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,
                            21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,
                            21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,
                            21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,
                            21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,
                            24,24,24,24,24,24,24,24,24,24,25,25,25,25,25,25,25,25,25,25),
             col.ind = groups, fill.ind = groups, 
             alpha.ind = 0.4 , 
             #habillage = groups,# color by groups
             palette = c(), 
             legend.title = "Age" ) #+ theme(legend.position = "none") 

pca <- prcomp(all_dist_pheno_fc[2:405])
groups <- as.factor(all_dist_pheno_fc$Sex.x) 
fviz_pca_ind(pca, pointsize = 10, axes = c(1, 2), repel = T, geom = "point",
             mean.point = F, 
             pointshape = c(21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,
                            21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,
                            21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,
                            21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,22,22,22,23,23,
                            24,24,24,24,24,25,25,25,25,25),
             col.ind = groups, fill.ind = groups, 
             alpha.ind = 0.4 , 
             #habillage = groups,# color by groups
             palette = c(), 
             legend.title = "Age" ) #+ theme(legend.position = "none") 

pca <- prcomp(all_dist_pheno_delta[2:405])
groups <- as.factor(all_dist_pheno_delta$Sex) 
fviz_pca_ind(pca, pointsize = 10, axes = c(1, 2), repel = T, geom = "point",
             mean.point = F, 
             pointshape = c(21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,
                            21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,
                            21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,
                            21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,22,22,22,23,23,
                            24,24,24,24,24,25,25,25,25,25),
             col.ind = groups, fill.ind = groups, 
             alpha.ind = 0.4 , 
             #habillage = groups,# color by groups
             palette = c(), 
             legend.title = "Age" ) #+ theme(legend.position = "none") 

pca <- prcomp(all_dist_pheno_absdelta[2:405])
groups <- as.factor(all_dist_pheno_absdelta$Sex) 
fviz_pca_ind(pca, pointsize = 10, axes = c(1, 2), repel = T, geom = "point",
             mean.point = F, 
             pointshape = c(21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,
                            21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,
                            21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,
                            21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,22,22,22,23,23,
                            24,24,24,24,24,25,25,25,25,25),
             col.ind = groups, fill.ind = groups, 
             alpha.ind = 0.4 , 
             #habillage = groups,# color by groups
             palette = c(), 
             legend.title = "Age" ) #+ theme(legend.position = "none") 

pca <- prcomp(all_dist_pheno_log2_delta[2:407])
groups <- as.factor(all_dist_pheno$Sex) 
fviz_pca_ind(pca, pointsize = 10, axes = c(1, 2), repel = T, geom = "point",
             mean.point = F, 
             pointshape = c(21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,
                            21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,
                            21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,
                            21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,
                            21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,
                            21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,
                            24,24,24,24,24,24,24,24,24,24,25,25,25,25,25,25,25,25,25,25),
             col.ind = groups, fill.ind = groups, 
             alpha.ind = 0.4 , 
             #habillage = groups,# color by groups
             palette = c(), 
             legend.title = "Age" ) #+ theme(legend.position = "none") 


pca<- prcomp(all_dist_pheno_log2_absdelta[2:405])
fviz_pca_ind(pca, pointsize = 10, axes = c(1, 2), repel = T, geom = "point", 
             #geom.ind = "point", 
             mean.point = F, 
             pointshape = c(21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,
                            21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,
                            21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,
                            21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,22,22,22,23,23,
                            24,24,24,24,24,25,25,25,25,25),  # show points only (nbut not "text")
             #col.ind = "black", fill.ind = c("none","none","none","none","none","none","none","none","none","none",
                                             #"none","none","none","none","none","none","none","none","none","none",
                                             #"none","none","none","none","none","none","none","none","none","none",
                                             #"none","none","none","none","none","none","none","none","none","none",
                                             #"none","none","none","none","none","none","none","none","none","none",
                                             #"none","none","none","none","none","none","none","none","none","none","#FF0000","#FF0000","#FF0000","#FF0000","#FF0000","#668d3c","#668d3c","#668d3c","#668d3c","#668d3c"),
             #alpha.ind = 0.4 , 
             #habillage = groups,# color by groups
             #palette = c(), #addEllipses = TRUE, # Concentration ellipses 
             #ellipse.type = "confidence",
             legend.title = "PCA_individuals_fc" ) + theme(legend.position = "none") 

pca<- prcomp(all_dist_pheno_abslog2[10:415])
fviz_pca_ind(pca, pointsize = 10, axes = c(1, 2), repel = T, geom = "point", 
             #geom.ind = "point", 
             mean.point = F, 
             pointshape = c(21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,
                            21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,
                            21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,
                            21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,
                            21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,
                            21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,23,23,23,23,
                            24,24,24,24,24,24,24,24,24,24,25,25,25,25,25,25,25,25,25,25),  # show points only (nbut not "text")
             col.ind = "black", fill.ind = c("none","none","none","none","none","none","none","none","none","none","none","none","none","none","none","none","none","none","none","none",
                                             "none","none","none","none","none","none","none","none","none","none","none","none","none","none","none","none","none","none","none","none",
                                             "none","none","none","none","none","none","none","none","none","none","none","none","none","none","none","none","none","none","none","none",
                                             "none","none","none","none","none","none","none","none","none","none","none","none","none","none","none","none","none","none","none","none",
                                             "none","none","none","none","none","none","none","none","none","none","none","none","none","none","none","none","none","none","none","none",
                                             "none","none","none","none","none","none","none","none","none","none","#FF0000","#FF0000","#FF0000","#FF0000","#668d3c","#668d3c","#668d3c","#668d3c"),
             #alpha.ind = 0.4 , 
             #habillage = groups,# color by groups
             #palette = c(), #addEllipses = TRUE, # Concentration ellipses 
             #ellipse.type = "confidence",
             legend.title = "PCA_individuals_fc" ) + theme(legend.position = "none") 

# Contributions of variables to PC1
fviz_contrib(pca, choice = "ind", axes = 1:2, top = 120)  +
  theme(axis.text.x = element_text(angle=90))
# Contributions of variables to PC2
fviz_contrib(pca, choice = "var", axes = 2, top = 20)


pca <- prcomp(ratio_table_co_twins_log2FC_abs[10:92244])
groups <- as.factor(ratio_table_co_twins_log2FC$Hair_color) # group by Twin_ID
fviz_pca_ind(pca, pointsize = 10, axes = c(1, 2), repel = T, 
             #geom.ind = "point", 
             mean.point = F, 
             pointshape = 20, #c(15,16,17,18,19,20,15,16,17,18,19,20,15,16,17,18,19,20,15,16,17,18,19,20,
             #15,16,17,18,19,20,15,16,17,18,19,20,15,16,17,18,19,20,15,16,17,18),  # show points only (nbut not "text")
             habillage = groups, alpha.ind = 0.7 , # color by groups
             palette = c(), #addEllipses = TRUE, # Concentration ellipses 
             #ellipse.type = "confidence",
             legend.title = "Feature Distances' abs(log2(FC)) / Cotwins" ) + theme(legend.position = "none")
fviz_contrib(pca, choice = "var", axes = 1:2, top = 30)  +
  theme(axis.text.x = element_text(angle=90))


pca <- prcomp(total_table_co_twins_FC[10:439])
groups <- as.factor(dist_table_co_twins_log2FC$Eye_color) # group by Twin_ID
fviz_pca_ind(pca, pointsize = 10, axes = c(1, 2), repel = T, 
             #geom.ind = "point", 
             mean.point = F, 
             pointshape = 20, #c(15,16,17,18,19,20,15,16,17,18,19,20,15,16,17,18,19,20,15,16,17,18,19,20,
             #15,16,17,18,19,20,15,16,17,18,19,20,15,16,17,18,19,20,15,16,17,18),  # show points only (nbut not "text")
             habillage = groups, alpha.ind = 0.7 , # color by groups
             palette = c(), #addEllipses = TRUE, # Concentration ellipses 
             #ellipse.type = "confidence",
             legend.title = "Feature Distances / Cotwins" )# + theme(legend.position = "none")

pca <- prcomp(dist_table_co_twins_log2FC_abs[10:439])
groups <- as.factor(dist_table_co_twins_log2FC_abs$Sex) # group by Twin_ID
fviz_pca_ind(pca, pointsize = 10, axes = c(1, 2), repel = T, 
             #geom.ind = "point", 
             mean.point = F, 
             pointshape = 20, #c(15,16,17,18,19,20,15,16,17,18,19,20,15,16,17,18,19,20,15,16,17,18,19,20,
             #15,16,17,18,19,20,15,16,17,18,19,20,15,16,17,18,19,20,15,16,17,18),  # show points only (nbut not "text")
             habillage = groups, alpha.ind = 0.7 , # color by groups
             palette = c(), #addEllipses = TRUE, # Concentration ellipses 
             #ellipse.type = "confidence",
             legend.title = "SEX" )# + theme(legend.position = "none")




# Contributions of variables to PC1
fviz_contrib(pca, choice = "ind", axes = 1, top = 30)  +
  theme(axis.text.x = element_text(angle=90))
# Contributions of variables to PC2
fviz_contrib(pca, choice = "var", axes = 2, top = 30)+
  theme(axis.text.x = element_text(angle=90))


##### t-SNE#######
library(tidyverse)

library(Rtsne)

library(ggplot2)

#ratio matrix tsne unranked/uncorrelated
ggplot(all_ratio_pheno_delta_tsne[,9:10]) + 
  geom_point(aes(x=all_ratio_pheno_delta_tsne[,9], 
                 y=all_ratio_pheno_delta_tsne[,10], 
                 color=all_ratio_pheno_delta_tsne[,2], 
                 fill=all_ratio_pheno_delta_tsne[,2]), 
             alpha = 0.3, 
             size = 10, shape = c(21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,
                                  21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,
                                  21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,
                                  21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,22,22,22,23,23,
                                  24,24,24,24,24,25,25,25,25,25)) + 
  theme_classic() +# alpha is the opacity 
  geom_text(aes(x=all_ratio_pheno_delta_tsne[,9], y=all_ratio_pheno_delta_tsne[,10], label=all_ratio_pheno_delta_tsne[,1])) +
  labs(title = "t-SNE of ratio distances", x="tSNE dim1", y="tSNE dim2", color=all_ratio_pheno_delta_tsne[,2], fill=all_ratio_pheno_delta_tsne[,2]) + # labs = labels
  scale_colour_gradientn(aesthetics = c("colour", "fill"), colors =rev(heat.colors(10))) + # this is to use a color scale rather then discrete colors ## heat.colors = palette
  theme(plot.title = element_text(hjust = 0.5, size = 25), ## all these are just sixe of texts, position of title
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.text.x= element_text(size = 22),
        axis.text.y= element_text(size = 22), 
        legend.title=element_blank(), 
        legend.text=element_text(size=20))#, 
#legend.position = "none") 

#ratio matrix tsne
ggplot(all_ratio_pheno_delta_tsne_male[,9:10]) + 
  geom_point(aes(x=all_ratio_pheno_delta_tsne_male[,9], y=all_ratio_pheno_delta_tsne_male[,10], color=all_ratio_pheno_delta_tsne_male[,8], fill=all_ratio_pheno_delta_tsne_male[,9]), alpha = 0.3, size = 10, shape=21) + 
  theme_classic() +# alpha is the opacity 
  geom_text(aes(x=all_ratio_pheno_delta_tsne_male[,9], y=all_ratio_pheno_delta_tsne_male[,10], label=all_ratio_pheno_delta_tsne_male[,1])) +
  labs(title = "t-SNE of ratio distances", x="tSNE dim1", y="tSNE dim2", color=all_ratio_pheno_delta_tsne_male[,2], fill=all_ratio_pheno_delta_tsne_male[,2]) + # labs = labels
  scale_colour_gradientn(aesthetics = c("colour", "fill"), colors =rev(heat.colors(10))) + # this is to use a color scale rather then discrete colors ## heat.colors = palette
  theme(plot.title = element_text(hjust = 0.5, size = 25), ## all these are just sixe of texts, position of title
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.text.x= element_text(size = 22),
        axis.text.y= element_text(size = 22), 
        legend.title=element_blank(), 
        legend.text=element_text(size=20))#, 
#legend.position = "none") 

ggplot(all_ratio_pheno_delta_tsne_female[,9:10]) + 
  geom_point(aes(x=all_ratio_pheno_delta_tsne_female[,9], y=all_ratio_pheno_delta_tsne_female[,10], color=all_ratio_pheno_delta_tsne_female[,8], fill=all_ratio_pheno_delta_tsne_female[,9]), alpha = 0.3, size = 10, shape=21) + 
  theme_classic() +# alpha is the opacity 
  geom_text(aes(x=all_ratio_pheno_delta_tsne_female[,9], y=all_ratio_pheno_delta_tsne_female[,10], label=all_ratio_pheno_delta_tsne_female[,8])) +
  labs(title = "t-SNE of ratio distances", x="tSNE dim1", y="tSNE dim2", color=all_ratio_pheno_delta_tsne_female[,2], fill=all_ratio_pheno_delta_tsne_female[,2]) + # labs = labels
  scale_colour_gradientn(aesthetics = c("colour", "fill"), colors =rev(heat.colors(10))) + # this is to use a color scale rather then discrete colors ## heat.colors = palette
  theme(plot.title = element_text(hjust = 0.5, size = 25), ## all these are just sixe of texts, position of title
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.text.x= element_text(size = 22),
        axis.text.y= element_text(size = 22), 
        legend.title=element_blank(), 
        legend.text=element_text(size=20))#, 
#legend.position = "none") 

ggplot(all_ratio_pheno_fc_tsne[,9:10]) + 
  geom_point(aes(x=all_ratio_pheno_fc_tsne[,9], y=all_ratio_pheno_fc_tsne[,10], color=all_ratio_pheno_fc_tsne[,8], fill=all_ratio_pheno_fc_tsne[,9]), alpha = 0.3, size = 10, shape=21) + 
  theme_classic() +# alpha is the opacity 
  geom_text(aes(x=all_ratio_pheno_fc_tsne[,9], y=all_ratio_pheno_fc_tsne[,10], label=all_ratio_pheno_fc_tsne[,8])) +
  labs(title = "t-SNE of ratio distances", x="tSNE dim1", y="tSNE dim2", color=all_ratio_pheno_fc_tsne[,2], fill=all_ratio_pheno_fc_tsne[,2]) + # labs = labels
  scale_colour_gradientn(aesthetics = c("colour", "fill"), colors =rev(heat.colors(10))) + # this is to use a color scale rather then discrete colors ## heat.colors = palette
  theme(plot.title = element_text(hjust = 0.5, size = 25), ## all these are just sixe of texts, position of title
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.text.x= element_text(size = 22),
        axis.text.y= element_text(size = 22), 
        legend.title=element_blank(), 
        legend.text=element_text(size=20))#, 
#legend.position = "none") 

ggplot(all_ratio_pheno_fc_tsne_male[,9:10]) + 
  geom_point(aes(x=all_ratio_pheno_fc_tsne_male[,9], y=all_ratio_pheno_fc_tsne_male[,10], color=all_ratio_pheno_fc_tsne_male[,8], fill=all_ratio_pheno_fc_tsne_male[,9]), alpha = 0.3, size = 10, shape=21) + 
  theme_classic() +# alpha is the opacity 
  geom_text(aes(x=all_ratio_pheno_fc_tsne_male[,9], y=all_ratio_pheno_fc_tsne_male[,10], label=all_ratio_pheno_fc_tsne_male[,8])) +
  labs(title = "t-SNE of ratio distances", x="tSNE dim1", y="tSNE dim2", color=all_ratio_pheno_fc_tsne_male[,2], fill=all_ratio_pheno_fc_tsne_male[,2]) + # labs = labels
  scale_colour_gradientn(aesthetics = c("colour", "fill"), colors =rev(heat.colors(10))) + # this is to use a color scale rather then discrete colors ## heat.colors = palette
  theme(plot.title = element_text(hjust = 0.5, size = 25), ## all these are just sixe of texts, position of title
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.text.x= element_text(size = 22),
        axis.text.y= element_text(size = 22), 
        legend.title=element_blank(), 
        legend.text=element_text(size=20))#, 
#legend.position = "none") 

ggplot(all_ratio_pheno_fc_tsne_female[,9:10]) + 
  geom_point(aes(x=all_ratio_pheno_fc_tsne_female[,9], y=all_ratio_pheno_fc_tsne_female[,10], color=all_ratio_pheno_fc_tsne_female[,8], fill=all_ratio_pheno_fc_tsne_female[,9]), alpha = 0.3, size = 10, shape=21) + 
  theme_classic() +# alpha is the opacity 
  geom_text(aes(x=all_ratio_pheno_fc_tsne_female[,9], y=all_ratio_pheno_fc_tsne_female[,10], label=all_ratio_pheno_fc_tsne_female[,8])) +
  labs(title = "t-SNE of ratio distances", x="tSNE dim1", y="tSNE dim2", color=all_ratio_pheno_fc_tsne_female[,2], fill=all_ratio_pheno_fc_tsne_female[,2]) + # labs = labels
  scale_colour_gradientn(aesthetics = c("colour", "fill"), colors =rev(heat.colors(10))) + # this is to use a color scale rather then discrete colors ## heat.colors = palette
  theme(plot.title = element_text(hjust = 0.5, size = 25), ## all these are just sixe of texts, position of title
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.text.x= element_text(size = 22),
        axis.text.y= element_text(size = 22), 
        legend.title=element_blank(), 
        legend.text=element_text(size=20))#, 
#legend.position = "none") 
