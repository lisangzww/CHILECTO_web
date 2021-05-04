
### boostrap sample for the concept with max Udiff
## after WSD

fname <- paste(path,"/top20dca_collocates_tree_manualcoded.csv",sep="")
cluster_cleaning <- read.csv(fname,fileEncoding = "UTF-8",sep="\t",header = T) %>%
  filter(manual_culling !="rerun") %>% droplevels()

concept_eg <- as.character(u_before_after[which.max(u_before_after$Udiff_ml_sg),]$concept)

cluster_cleaning <- cluster_cleaning %>% filter(concept==concept_eg) %>% droplevels()


cluster_select<-cluster_cleaning%>%
  filter(manual_culling=="in")%>%droplevels()
cluster_select$cluster


fname <- paste(path,"/output_loop/cluster_output/",concept_eg,"_tree_clusterID.csv",sep="")
cluster.df.eg <- read.csv(fname,fileEncoding = "UTF-8",sep=",",header=T)

cluster.df.select<-cluster.df.eg%>%
  filter(cluster%in%c(cluster_select$cluster))%>%droplevels()

tokenlabels <- as.character(cluster.df.select$tokenlabel)

#variant
variant_labels <- sapply(strsplit(tokenlabels,"/"), '[',1)

#lect
corpusDay<-sapply(strsplit(tokenlabels,"/"), '[',3)
lect_label_temp<-as.factor(sapply(strsplit(corpusDay,"_"), '[',1))
lect_labels <- revalue(lect_label_temp, c("XIN"="ML", "CNA"="TW", "ZBN"="SG"))

uniformity.df<-data.frame(variant_labels,lect_labels)
xtab <- table(uniformity.df$variant_labels,uniformity.df$lect_labels)

t_after <- xtab[,c("ML","SG")] #uniformity.fn only works for two lects

# function to convert table of frequencies to dataframe of cases
countsToCases <- function(x, countcol = "Freq") {
  idx <- rep.int(seq_len(nrow(x)), x[[countcol]])
  x[[countcol]] <- NULL
  x[idx, ]
}

d.conceptsFreqs <- countsToCases(as.data.frame(t_after), countcol = "Freq")
colnames(d.conceptsFreqs) <- c("Variant","Lect")
head(d.conceptsFreqs)
summary(d.conceptsFreqs)

# function to calculate uniformity
uniformity.fn <- function(data, variant, lects, index) {
  profFreq <- table(data[index,c(variant,lects)])
  profRelFreq <- prop.table(profFreq,2)
  uniformity <- sum(apply(profRelFreq, 1, min), na.rm=T)
  return(uniformity)
}

bootSample = nrow(d.conceptsFreqs)
uniformity.fn(d.conceptsFreqs, variant="Variant", lects="Lect", index=sample(bootSample,bootSample,replace=T))


library(boot)
bootObj.after <- boot(d.conceptsFreqs, uniformity.fn, variant="Variant", lects="Lect", R=1000)
bootObj.after


# before WSD
fname <- paste(path,"/output_loop/cluster_output/",concept_eg,"_tree_clusterID.csv",sep="")
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

t_before <- xtab[,c("ML","SG")] #uniformity.fn only works for two lects

d.conceptsFreqs <- countsToCases(as.data.frame(t_before), countcol = "Freq")
colnames(d.conceptsFreqs) <- c("Variant","Lect")
head(d.conceptsFreqs)
summary(d.conceptsFreqs)

bootSample = nrow(d.conceptsFreqs)
uniformity.fn(d.conceptsFreqs, variant="Variant", lects="Lect", index=sample(bootSample,bootSample,replace=T))

bootObj.before <- boot(d.conceptsFreqs, uniformity.fn, variant="Variant", lects="Lect", R=1000)

d.boot <- data.frame(uniformity=rbind(bootObj.after$t,bootObj.before$t),
                     mean=rep(c(bootObj.after$t0,bootObj.before$t0),each=1000),
                     data=rep(c("automatically\ndisambiguated", "all_tokens"), each=1000))

title <- paste("boostrapping uniformity_ml_sg for",concept_eg,sep=" ")
ggplot(d.boot, aes(x=uniformity, fill=data, color=data)) +
  geom_density(alpha=0.1) +
  #geom_histogram(aes(y=..density..), alpha=0.2, position="identity") +
  geom_vline(data=d.boot, aes(xintercept=mean, color=data), linetype="dashed", size=1) +
  ggtitle(title)
