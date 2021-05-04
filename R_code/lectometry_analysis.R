setwd("D:/KUL-MSCA/Box Sync/Model/mds_tokencloud")
Sys.setlocale(category = "LC_ALL", locale = "Chinese")
path = "D:/KUL-MSCA/Box Sync/Model/mds_tokencloud"


library(ggplot2)
library(tidyr)
library(knitr)
library(dplyr)
library(plotly, quietly=T)
library(devEMF)
library(RColorBrewer, quietly=T)
library(dynamicTreeCut)
library(dendextend, quietly=T)
library(colorspace)
library(RcppCNPy)
library(rjson)
library(plyr)
library(DT)
library(pinyin)
library(plyr) # ldply()
library(tidyverse) # remove_rownames()
library(entropy)
# library(phylogram) #write.dendrogram
library(translate)
library(reshape2) #melt()
library("rjson")
#------------------------
#after WSD
#------------------------
fname <- paste(path,"/top20dca_collocates_tree_manualcoded.csv",sep="")
cluster_cleaning <- read.csv(fname,fileEncoding = "UTF-8",sep="\t",header = T) %>%
  filter(manual_culling !="rerun") %>% droplevels()

conceptlist <- as.character(levels(cluster_cleaning[,3]))

datalist <- list()
for (i in 1:length(conceptlist)){

cluster_cleaning_temp <- cluster_cleaning %>%
  filter(concept==conceptlist[i])%>%droplevels()

cluster_select<-cluster_cleaning_temp%>%
  filter(manual_culling=="in")%>%droplevels()
cluster_select$cluster


fname <- paste(path,"/output_loop/cluster_output/",conceptlist[i],"_tree_clusterID.csv",sep="")
cluster.df <- read.csv(fname,fileEncoding = "UTF-8",sep=",",header=T)
cluster.df.select<-cluster.df%>%
  filter(cluster%in%c(cluster_select$cluster))%>%droplevels()

tokenlabels <- as.character(cluster.df.select$tokenlabel)

##variant
variant_labels <- sapply(strsplit(tokenlabels,"/"), '[',1)

##lect
corpusDay<-sapply(strsplit(tokenlabels,"/"), '[',3)
lect_label_temp<-as.factor(sapply(strsplit(corpusDay,"_"), '[',1))
lect_labels <- revalue(lect_label_temp, c("XIN"="ML", "CNA"="TW", "ZBN"="SG"))

uniformity.df<-data.frame(variant_labels,lect_labels)
xtab <- table(uniformity.df$variant_labels,uniformity.df$lect_labels)

sum(xtab)

proptable <- round(prop.table(xtab,2),4)

proptable_tw_ml <- proptable[,c(1,2)]
proptable_tw_sg <- proptable[,c(1,3)]
proptable_ml_sg <- proptable[,c(2,3)]
minimum_tw_ml <- apply(proptable_tw_ml,1,min)
minimum_tw_ml
u_tw_ml <- sum(minimum_tw_ml)
u_tw_ml

minimum_tw_sg <- apply(proptable_tw_sg,1,min)
minimum_tw_sg
u_tw_sg <- sum(minimum_tw_sg)
u_tw_sg

minimum_ml_sg <- apply(proptable_ml_sg,1,min)
minimum_ml_sg
u_ml_sg <- sum(minimum_ml_sg)
u_ml_sg

concept <- conceptlist[i]
freq_after <- sum(xtab)
freq_before <- nrow(cluster.df)
u_pair<- data.frame(concept,u_tw_ml,u_tw_sg,u_ml_sg,freq_before,freq_after)
datalist[[i]]<- u_pair
}

u_pair_df <- ldply(datalist,rbind)
u_pair_df$prop <- prop.table(u_pair_df[,"freq_after"])

fname <- paste(path,'/pair_U_after_WSD.csv',sep="")
write.csv(u_pair_df,fname,fileEncoding = "UTF-8",row.names = FALSE)


## U_agg
U_agg_tw_ml <- mean(u_pair_df[,"u_tw_ml"])
U_agg_tw_ml
U_agg_tw_sg <- mean(u_pair_df[,"u_tw_sg"])
U_agg_tw_sg
U_agg_ml_sg <- mean(u_pair_df[,"u_ml_sg"])
U_agg_ml_sg

## weighted U1_agg
U1_agg_tw_ml <- sum(u_pair_df$u_tw_ml*u_pair_df$prop)
U1_agg_tw_ml
U1_agg_tw_sg <- sum(u_pair_df$u_tw_sg*u_pair_df$prop)
U1_agg_tw_sg
U1_agg_ml_sg <- sum(u_pair_df$u_ml_sg*u_pair_df$prop)
U1_agg_ml_sg

#---------------------------
#before WSD
#---------------------------

datalist <- list()
for (i in 1:length(conceptlist)){
  
fname <- paste(path,"/output_loop/cluster_output/",conceptlist[i],"_tree_clusterID.csv",sep="")
cluster.df <- read.csv(fname,fileEncoding = "UTF-8",sep=",",header=T)

tokenlabels <- as.character(cluster.df$tokenlabel)

##variant
variant_labels <- sapply(strsplit(tokenlabels,"/"), '[',1)

##lect
corpusDay<-sapply(strsplit(tokenlabels,"/"), '[',3)
lect_label_temp<-as.factor(sapply(strsplit(corpusDay,"_"), '[',1))
lect_labels <- revalue(lect_label_temp, c("XIN"="ML", "CNA"="TW", "ZBN"="SG"))

uniformity.df<-data.frame(variant_labels,lect_labels)
xtab <- table(uniformity.df$variant_labels,uniformity.df$lect_labels)

sum(xtab)

proptable <- round(prop.table(xtab,2),4)

proptable_tw_ml <- proptable[,c(1,2)]
proptable_tw_sg <- proptable[,c(1,3)]
proptable_ml_sg <- proptable[,c(2,3)]
minimum_tw_ml <- apply(proptable_tw_ml,1,min)
minimum_tw_ml
u_tw_ml_beforeWSD <- sum(minimum_tw_ml)
u_tw_ml_beforeWSD

minimum_tw_sg <- apply(proptable_tw_sg,1,min)
minimum_tw_sg
u_tw_sg_beforeWSD <- sum(minimum_tw_sg)
u_tw_sg_beforeWSD

minimum_ml_sg <- apply(proptable_ml_sg,1,min)
minimum_ml_sg
u_ml_sg_beforeWSD <- sum(minimum_ml_sg)
u_ml_sg_beforeWSD

concept <- conceptlist[i]
freq_after <- sum(xtab)
freq_before <- nrow(cluster.df)
u_pair<- data.frame(concept,u_tw_ml_beforeWSD,u_tw_sg_beforeWSD,u_ml_sg_beforeWSD,freq_before,freq_after)
datalist[[i]]<- u_pair
}

u0_pair_df <- ldply(datalist,rbind) # U before WSD

fname <- paste(path,'/pair_U_before_WSD.csv',sep="")
write.csv(u0_pair_df,fname,fileEncoding = "UTF-8",row.names = FALSE)

#------------------------------------------------
# merge the U dataframes before and after WSD
#------------------------------------------------
u_before <- u0_pair_df%>%select("u_tw_ml_beforeWSD","u_tw_sg_beforeWSD","u_ml_sg_beforeWSD","concept")
u_before_after <- merge(u_pair_df,u_before,by="concept")


#-------------------------
# adding semantic fields
#-------------------------
fname <- ("D:/KUL-MSCA/Box Sync/Model/wordnet_uniformity/uniformity_final_annotated.csv")
semfield_df <- read.csv(fname, sep='\t',fileEncoding = "UTF-8",header=T)%>%
  select("semantic_field_conflated","synset","synset_eng")

### remove duplicate rows: unique concepts
semfield_df <- unique(semfield_df)
str(semfield_df)

### merge "synset" and "synset_eng" into a new column "concept"
semfield_df <- unite(semfield_df, concept, c(synset_eng, synset), remove=FALSE)
semfield_df$concept <- as.factor(semfield_df$concept)

semfield_df <- semfield_df%>% select("semantic_field_conflated","concept")
str(semfield_df)

### merge semfild with U dataset
u_before_after$semfield <- semfield_df[match(u_before_after$concept,semfield_df$concept),"semantic_field_conflated"]

### add difference of U before and after WSD
u_before_after$Udiff_tw_ml <- abs(u_before_after$u_tw_ml-u_before_after$u_tw_ml_beforeWSD)
u_before_after$Udiff_tw_sg <- abs(u_before_after$u_tw_sg-u_before_after$u_tw_sg_beforeWSD)
u_before_after$Udiff_ml_sg <- abs(u_before_after$u_ml_sg-u_before_after$u_ml_sg_beforeWSD)

fname <- paste(path,'/pair_U_before_after_WSD.csv',sep="")
write.csv(u_before_after,fname,fileEncoding = "UTF-8",row.names = FALSE)




### manually add semantic fields for NA categories
### read the manually added version
fname <- paste(path,'/pair_U_before_after_WSD_coded.csv',sep="")
u_before_after <- read.csv(fname, sep='\t',fileEncoding = "UTF-8",header=T)
u_df <- u_before_after

# wide table to long table
data_wide_tw_ml <- u_before_after %>% select("u_tw_ml_beforeWSD","concept",
                                             "u_tw_ml","semfield")
data_wide_tw_ml$u_tw_ml_afterWSD <- data_wide_tw_ml$u_tw_ml
data_wide_tw_ml <- data_wide_tw_ml[,!(names(data_wide_tw_ml) %in% c("u_tw_ml"))]

data_long_tw_ml <- melt(data_wide_tw_ml,id.vars=c("concept","semfield"))

q1<-qplot(data_long_tw_ml$value, data=data_long_tw_ml, geom="density", fill=data_long_tw_ml$variable, alpha=I(.5), 
          main="Distribution of u_tw_ml", xlab="u_tw_ml", 
          ylab="Density")
q1

data_wide_tw_sg <- u_before_after %>% select("u_tw_sg_beforeWSD","concept",
                                             "u_tw_sg","semfield")
data_wide_tw_sg$u_tw_sg_afterWSD <- data_wide_tw_sg$u_tw_sg
data_wide_tw_sg <- data_wide_tw_sg[,!(names(data_wide_tw_sg) %in% c("u_tw_sg"))]

data_long_tw_sg <- melt(data_wide_tw_sg,id.vars=c("concept","semfield"))

q2<-qplot(data_long_tw_sg$value, data=data_long_tw_sg, geom="density", fill=data_long_tw_ml$variable, alpha=I(.5), 
          main="Distribution of u_tw_sg", xlab="u_tw_sg", 
          ylab="Density")
q2


data_wide_ml_sg <- u_before_after %>% select("u_ml_sg_beforeWSD","concept",
                                             "u_ml_sg","semfield")
data_wide_ml_sg$u_ml_sg_afterWSD <- data_wide_ml_sg$u_ml_sg
data_wide_ml_sg <- data_wide_ml_sg[,!(names(data_wide_ml_sg) %in% c("u_ml_sg"))]

data_long_ml_sg <- melt(data_wide_ml_sg,id.vars=c("concept","semfield"))

q3<-qplot(data_long_ml_sg$value, data=data_long_ml_sg, geom="density", fill=data_long_ml_sg$variable, alpha=I(.5), 
          main="Distribution of u_ml_sg", xlab="u_ml_sg", 
          ylab="Density")
q3

#---------------------------
# concept characteristics
#----------------------------

# analyzing U~semantic field
p1 <- ggplot(u_df,aes(x=semfield,y=u_tw_ml))+geom_boxplot() +
          theme(axis.text.x = element_text(angle = 75, hjust = 1,size=12)) +
          ggtitle("U_tw_ml~semantic field")
p1

p2 <- ggplot(u_df,aes(x=semfield,y=u_tw_sg))+geom_boxplot() +
          theme(axis.text.x = element_text(angle = 75, hjust = 1,size=12)) +
          ggtitle("U_tw_sg~semantic field")
p2

p3 <- ggplot(u_df,aes(x=semfield,y=u_ml_sg))+geom_boxplot() +
          theme(axis.text.x = element_text(angle = 75, hjust = 1,size=12)) +
          ggtitle("U_ml_sg~semantic field")
p3

y <- u_df$u_tw_ml
x <- u_df$semfield
m1 <-lm(y~x)
summary(m1)

y <- u_df$u_tw_sg
x <- u_df$semfield
m2 <-lm(y~x)
summary(m2)

y <- u_df$u_ml_sg
x <- u_df$semfield
m3 <-lm(y~x)
summary(m3)

# analyzing U ~ concept entrenchment
fname <- paste(path,"/gigasub.target.vocab.txt",sep="")
freqlist <- read_tsv(fname,col_names = F) 
colnames(freqlist) <- c("variant","freq")
str(freqlist)

wordnet_df <- read_tsv("wordnet_add_final.csv")   %>%
  filter(testsample=="yes") %>% 
  select("concept","wordnet_variant","variant_added") %>%
  droplevels()

wordnet_df$variant <- paste(wordnet_df$wordnet_variant,wordnet_df$variant_added,sep=",")
wordnet_df <- wordnet_df%>%
  select("concept","variant")

wordnet_df <- separate_rows(wordnet_df,variant,sep = ",")

wordnet_df <- wordnet_df %>% filter(variant!="NA") %>% droplevels() # remove NA 


merge_df <- merge(freqlist,wordnet_df,by.x="variant",by.y="variant")
merge_df$concept <- as.factor(merge_df$concept)
# setdiff(wordnet_df$variant, freqlist$variant) # check non-overlapped variant in two variant lists

concept_freq <- aggregate(merge_df$freq,by=list(concept=merge_df$concept),FUN=sum)
colnames(concept_freq) <- c("concept","concept_freq")


# add concept_freq to pair_U_before_after_WSD.csv
u_df_final <- merge(u_before_after,concept_freq,by="concept")
u_df_final$in_concept_rate <- u_df_final$freq_after / u_df_final$freq_before
u_df_final$concept_entrench <- u_df_final$concept_freq*u_df_final$in_concept_rate


# conflate some semantic field
u_df_final$semfield_cnfl <- "artifact_object"
u_df_final$semfield_cnfl[u_df_final$semfield=="group"]<-"living_thing"
u_df_final$semfield_cnfl[u_df_final$semfield=="possession"]<-"artifact_object"
u_df_final$semfield_cnfl[u_df_final$semfield=="state"]<-"attribute"
u_df_final$semfield_cnfl[u_df_final$semfield=="living_thing"]<-"living_thing"
u_df_final$semfield_cnfl[u_df_final$semfield=="time"]<-"time"
u_df_final$semfield_cnfl[u_df_final$semfield=="cognition"]<-"cognition"
u_df_final$semfield_cnfl[u_df_final$semfield=="communication"]<-"activity"
u_df_final$semfield_cnfl[u_df_final$semfield=="attribute"]<-"attribute"
u_df_final$semfield_cnfl[u_df_final$semfield=="attribute"]<-"activity"



# linear regression
u_ml_sg <- u_df_final$u_ml_sg
semantic_field <- u_df_final$semfield_corrected
entrenchment <- u_df_final$concept_entrench

model <-lm(u_ml_sg~semantic_field+entrenchment+semantic_field:entrenchment)
summary(model)

# exploring the interaction

conceptname <- as.character(u_df_final$concept)
concept_en <- gsub("^(.+?)(?=\\d).*", "\\1", conceptname, perl = TRUE)
concept <- gsub ("_$","",concept_en) # remove the "_" at the end
u_df_final$concept_label <- concept

# change some misleading english label for concept

u_df_final$concept_label <- as.character(u_df_final$concept_label)
u_df_final$concept_label[u_df_final$concept_label=="means"] <- "device_means"
u_df_final$concept_label <- as.factor(u_df_final$concept_label)

u_df_final$concept_salience <- u_df_final$concept_entrench

qplot(x = concept_salience, y = u_ml_sg, facets = ~ semfield_corrected,
      data=u_df_final) +
  geom_smooth(method = "lm") +
  geom_point() +geom_text(aes(label=concept_label),hjust=0, vjust=0) +
  ggtitle("Interaction plot for U_ml_sg")