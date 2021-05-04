library(RcppCNPy)
library (rjson)
library(dynamicTreeCut)
library(dendextend, quietly=T)
library(pinyin)
library(colorspace)
library(dplyr)
library(data.table)
library(gtools)
library(plyr) # ldply()
library(tidyverse) # remove_rownames()
library(entropy)
library(phylogram) #write.dendrogram
library(stringr) #str_detect


setwd("D:/KUL-MSCA/Box Sync/Model/mds_tokencloud")
Sys.setlocale(category = "LC_ALL", locale = "Chinese")
path <- "D:/KUL-MSCA/Box Sync/Model/mds_tokencloud"


concept <- c("railway_04048075-n")
fname <- paste(path,"/output_loop/",concept,"/",concept,".freq0ppmi1llr1.ttmx.pac",sep="")


get_tokvecs <- function(input_file){
  temp <- unzip(input_file, unzip="internal")
  tokvecs <- npyLoad(temp[2])
  metadata <- fromJSON(file = temp[1])
  # dimid2item <- names(metadata$`dim-freq-dict`)
  dimid2item <- metadata$'row_items'
  print(head(dimid2item))
  dimnames(tokvecs) <- list(dimid2item, dimid2item)
  file.remove(temp[1], temp[2])
  return(tokvecs)
}


tokvecs_dist <- get_tokvecs(fname)


library(vegan)
set.seed(1026)
d <- tokvecs_dist
sampleItems <- sample(rownames(d),500)
d.sub <- d[sampleItems,sampleItems]

hcl <- hclust(as.dist(d.sub), method="ward.D2")
dend <- as.dendrogram(hcl)

# # write out dendrogram
# Encoding(labels(dend))<-"utf-8" #先改为utf-8,encoding其实是unknown，这样才不会有乱码
# fname <- paste(path,"/output_loop/cluster_output/",concept,"_dendrogram",sep="")
# write.dendrogram(dend,file=fname)
# 
# # load dendrogram (encoding problem!!!)
# dend <- read.dendrogram(fname)
# Encoding(labels(dend))<-"UTF-8" # 再convert labels to UTF-8

variant_labels <- sapply(strsplit(labels(dend),"/"), '[',1)

# loop for coloring (a simpler version)

n_var = length(unique(variant_labels))
colors = rainbow(n_var)
variants = unique(variant_labels)
true_cols = c(1:n_var)

for (idx in seq_along(variant_labels)){
  true_cols[idx]=colors[match(variant_labels[idx], variants)]
}

#true_cols

py_label <- py(variant_labels) # convert to pinyin
variant_py_label <- paste0(variant_labels,py_label,sep="")


#-------------------------cutreeDynamic----------------------------------------
# Find special clusters:
clusters <- cutreeDynamic(hcl, distM = as.matrix(dist(d.sub)), method = "hybrid",deepSplit = 4)
#clusters <- cutreeDynamicTree(hcl, maxTreeHeight = 1, deepSplit = TRUE, minModuleSize = 20)

# we need to sort them to the order of the dendrogram:
clusters <- clusters[order.dendrogram(dend)]
clusters_numbers <- unique(clusters) - (0 %in% clusters)
n_clusters <- length(clusters_numbers)
n_clusters


# get n_clusters from cutreeDynamic()

# seting a cutting height for the dendgram

# clusters <- cutree(hcl, h=unique(hcl$height)[0.8])
# clusters <- clusters[order.dendrogram(dend)]
# clusters_numbers <- unique(clusters) - (0 %in% clusters)
# n_clusters <- length(clusters_numbers)


clusters_final <- cutree(dend, k=n_clusters, order_clusters_as_data = FALSE)

#------------------------plotting-----------------------------------------
clusters_final <- read.csv(fname)


cols <- rainbow_hcl(n_clusters)
dend2 <- dend %>% 
  branches_attr_by_clusters(clusters_final, values = cols) %>% 
  color_labels(col = true_cols)

labels(dend2) <- variant_labels
par(mar = c(2,2,0,0))
dend2 <- set(dend2, "labels_cex", 0.5)
plot(dend2)
legend("topleft", legend = unique(variant_py_label), fill = unique(true_cols))


#----------------------------looking at clusters------------------------------
head(clusters_final)
table(clusters_final)

clusters.df <- data.frame(tokenlabel=names(clusters_final),cluster=clusters_final)


# to get all names of tokens in a given cluster
cluster.tokens <- filter(clusters.df,cluster == 3)$tokenlabel %>% droplevels() %>%
  as.character()

# get variants in selected cluster
variant_labels_cluster <- sapply(strsplit(cluster.tokens,"/"), '[',1)

# distribution of variants in selected cluster
variant_no <- table(variant_labels_cluster)

#-------------aggregate--------------------
varcluster <- function(labels){
  cluster.tokens <- labels %>% as.character()
  variant_labels <- sapply(strsplit(cluster.tokens,"/"), '[',1)
  variant_no <- table(variant_labels)
  variant_no
}

cluster_variant <- aggregate(clusters.df$tokenlabel,by=list(cluster=clusters.df$cluster),FUN=varcluster)
cluster_variant$x
cluster_variant_df<-ldply(cluster_variant$x,rbind)
cluster_variant_df[is.na(cluster_variant_df)] <- 0


#------------for loop--------------------
datalist = list()

for (i in 1:n_clusters){
  cluster <- clusters.df %>%
    filter(cluster==i)
  label <- cluster$tokenlabel %>% droplevels()%>%as.character()
  variant_labels <- sapply(strsplit(label,"/"), '[',1)
  variant_no <- table(variant_labels)
  variant_no$cluster <- paste0("cluster",i,sep="")
  tmp <- as.data.frame(variant_no)
  datalist[[i]] <- tmp
}

cluster_variant_df <- bind_rows(datalist)
cluster_variant_df[is.na(cluster_variant_df)] <- 0
cluster_variant_df <- cluster_variant_df %>% remove_rownames %>% column_to_rownames(var="cluster")
cluster_variant_df


# ------------get lects in selected cluster
datalist = list()

for (i in 1:n_clusters){
  cluster <- clusters.df %>%
    filter(cluster==i)
  label <- cluster$tokenlabel %>% droplevels()%>%as.character()
  corpusDay_cluster<-sapply(strsplit(label,"/"), '[',3)
  lect_labels_temp<-as.factor(sapply(strsplit(corpusDay_cluster,"_"), '[',1))
  lect_labels <- revalue(lect_labels_temp, c("XIN"="ML", "CNA"="TW", "ZBN"="SG"))
  lect_no <- table(lect_labels)
  lect_no$cluster <- paste0("cluster",i,sep="")
  tmp <- as.data.frame(lect_no)
  datalist[[i]] <- tmp
}

cluster_lect_df <- bind_rows(datalist)
cluster_lect_df[is.na(cluster_lect_df)] <- 0
cluster_lect_df <- cluster_lect_df %>% remove_rownames %>% column_to_rownames(var="cluster")
cluster_lect_df


#--------------entropy for variant distr in clusters--------------
# entropy: the higher, the distribution is more balanced (mixed)
entropy_fun <- function(x){
  entropy(as.integer(x))
}   

cluster_variant_entropy <- apply(cluster_variant_df,MARGIN = 1,FUN=entropy_fun)
cluster_lect_entropy <- apply(cluster_lect_df,MARGIN = 1,FUN=entropy_fun)

x <- as.data.frame(cluster_variant_entropy)
y <- as.data.frame(cluster_lect_entropy)

entropy <- merge(x,y,by=0)


#-------------whole table----------------
df_tmp <- cbind(cluster_variant_df,cluster_lect_df)
df_all <- cbind(df_tmp,entropy)


#---------------------

lect_no$cluster <- "cluster1"
lect_no3$cluster <- "cluster3"
list <- Map(rbind,lect_no,lect_no3)
df <- as.data.frame(list)
df[is.na(df)] <- 0


variant_no$cluster <- "cluster1"
variant_no3$cluster <- "cluster3"
x <- as.data.frame(variant_no)
y <- as.data.frame(variant_no3)

library(gtools)
df <- smartbind(tmp1,tmp2,tmp3,tmp4,tmp5)
df[is.na(df)] <- 0

df2 <- select(df,-c(cluster))


#-------------------------------pvclust---------------------------------------

library(pvclust)
set.seed(1026)
result <- pvclust(d.sub,method.dist="cor",method.hclust="average",nboot=10)

# with pvrect
plot(result)
pvrect(result)

# with a dendrogram of pvrect
dend <- as.dendrogram(result)
labels(dend) <- variant_labels

dend2 <- dend %>% 
  branches_attr_by_clusters(clusters) %>%
  color_labels(col = true_cols)


dend2  %>% 
  plot(main = "Cluster dendrogram with AU/BP values (%)", cex.main=0.5)
result %>% text
result %>% pvrect(0.95)


#-------------------------------------------------------------------------------------
convar <- read.table("wordnet_add_final.csv",sep="\t",header=T) %>%
  filter(testsample=="yes") %>% 
  select("concept") %>%
  droplevels()
conceptlist <- as.character(levels(convar[,1]))


#clusterID dataframe for all concepts
clusters_df_all <- data.frame() # all selected concepts
for (i in 1:length(conceptlist)){
  fname <- paste(path,"/output_loop/cluster_output/",conceptlist[i],"_clusterID",sep="")
  df <- read.csv(fname, sep=',',fileEncoding = "UTF-8")
  df$id<-df$tokenlabel
  df$concept <- as.factor(conceptlist[i])
  clusters_df_all <- rbind(clusters_df_all, df)
} # 9000 rows, id 8997 levels
clusters_df_all$id[duplicated(clusters_df_all$id)] # three duplicated ids
clusters_df_all$mergeID <- paste(clusters_df_all$id,clusters_df_all$concept,sep="_")


#token concordance dataframe for all concepts
token_all <- data.frame() # all selected concepts
for (i in 1:length(conceptlist)){
  fname <- paste(path,"/output_loop/mds_output/",conceptlist[i],"_mds.csv",sep="")
  df <- read.csv(fname, sep=',',fileEncoding = "UTF-8")
  df$concept <- as.factor(conceptlist[i])
  token_all <- rbind(token_all, df)
} # 9000 rows, id 8997 levels
token_all$id[duplicated(token_all$id)] # three duplicated ids
token_all$mergeID <- paste(token_all$id,token_all$concept,sep="_")

#merge the two dataframes
tokencluster <- merge(token_all,clusters_df_all,by="mergeID",all.x=TRUE)%>%
  select("id.x","concept.x","variant","corpus","year","text","lect","cluster") # 9000 rows

colnames(tokencluster)<-c("id","concept","variant","corpus","year","text","lect","cluster")

fname <- paste(path,"/output_loop/cluster_output/","all_token_clusterID.csv",sep="")
write.csv(tokencluster,fname,fileEncoding = "UTF-8")

#----------------------top best collocates------------------------------------------------
convar <- read.table("wordnet_add_final.csv",sep="\t",header=T) %>%
  filter(testsample=="yes") %>% 
  select("concept") %>%
  droplevels()
conceptlist <- as.character(levels(convar[,1]))

# loop for all concepts
col_all_list <- list()
for (i in 1:length(conceptlist)){

  fname <- paste(path,"/output_loop/mds_output/",conceptlist[i],"_ppmi_mds.csv",sep="")
  df <- read.csv(fname, sep=',',fileEncoding = "UTF-8") %>%
    select("id","text_ppmi")
  context <-as.character(df$text_ppmi)
  context<-gsub("<b>","",context) 
  context<-gsub("</b>","",context) 
  context<-gsub("<br>"," ",context) 

 
  # loop for all tokens
  datalist <- list()

    for (j in 1:length(context)){
    string <- unlist(strsplit(context[j]," "))
    #stringlist <- strsplit(string,"/") # split into lemma, pos, ppmi (donot use this line inside the loop)
    context_df<-ldply(string,rbind)
    context_df$tokenid <- df$id[j]
    datalist[[j]]<-context_df
    }

  col_df <- ldply(datalist,rbind)
  col_df$concept <- as.factor(conceptlist[i]) # add the concept to the dataframe
  col_all_list[[i]]<-col_df
}

col_all_df <- ldply(col_all_list,rbind)
colnames(col_all_df) <- c("coll_ppmi","id","concept")

# select collocates with ppmi (>1)
# significant collocates, i.e. ppmi > 1 (we set ppmi cutoff =1 in tokenlevel python loop)
sigcol_df <- col_all_df %>%
  filter(str_detect(col_all_df$coll_ppmi,"[0-9]{2}$")) # select those rows with coll_ppmi ending with a number
# problem: 有/V_2 also ends with numbers, but it is not ppmi; therefore, we have to specify that the string contains with 2 digits at the end!

# extract the significant collocates
x <- as.character(sigcol_df$coll_ppmi)
sigcollocates <- sapply(strsplit(x,"/"), '[',1)

# add the significant collocates to be a new column
sigcol_df$sigcol <- as.factor(sigcollocates)

# add the clusterID info to the dataframe
sigcol_cluster <- merge(sigcol_df,clusters_df_all,by="id") %>%
  select("id","coll_ppmi","concept","sigcol","cluster")
sigcol_cluster$cluster<-as.factor(sigcol_cluster$cluster)


# get dac_input files for each concept; a loop
for (i in 1:length(conceptlist)){
  dca_input <- sigcol_cluster %>%
  filter(concept==conceptlist[i]) %>% droplevels()%>%
  select("cluster","sigcol")
  fname <- paste(path,"/output_loop/cluster_output/",conceptlist[i],"_dcaInput.txt",sep="")
  write.table(dca_input,fname,fileEncoding = "UTF-8",sep="\t",row.names = FALSE)
}

# run gries's script
for (i in 1:length(conceptlist)){
  fname <- paste(path,"/output_loop/cluster_output/",conceptlist[i],"_dcaInput.txt",sep="")
  mdca.data<-read.table(fname,fileEncoding = "UTF-8",header=T)
  names(mdca.data)<-c("W_C", "Coll_Word")

  tab.mca.data<-table(mdca.data$Coll_Word, mdca.data$W_C) # generate table for multiple dca
  colfreq<-table(mdca.data$W_C)
  verb<-rownames(tab.mca.data); constr<-colnames(tab.mca.data)
  n.verb<-length(verb); n.constr<-length(constr)

  result.table<-data.frame(matrix(nrow=n.verb, ncol=(n.constr*3)+3))
  colnames(result.table)<-c("Coll_Word", as.character(constr), paste("exp", as.character(constr), sep="_"), paste("pbin", as.character(constr), sep="_"), "SumAbsDev", "LargestDev")
  result.table[,1]<-rownames(tab.mca.data)
  result.table[,2:(n.constr+1)]<-tab.mca.data[,1:n.constr]

  which.accuracy<- 3 #the number of decimals you'd like to see in the results

  for (f in 1:n.verb) {
  
    cur.obs<-tab.mca.data[f,]
    cur.exp<-sum(cur.obs)*(colfreq/sum(colfreq))
    result.table[f,(n.constr+2):(n.constr+n.constr+1)]<-round(cur.exp, which.accuracy)
  
    counter<-0
  for (g in (n.constr*2+2):(length(result.table)-2)) {
    counter<-counter+1
    if (cur.obs[counter]>=cur.exp[counter]) {
      result.table[f,g]<-round(-log(sum(dbinom(cur.obs[counter]:sum(cur.obs), sum(cur.obs), (cur.exp[counter]/sum(cur.obs)))), 10), which.accuracy)
    } else {
      result.table[f,g]<-round(log(sum(dbinom(0:cur.obs[counter], sum(cur.obs), (cur.exp[counter]/sum(cur.obs)))), 10), which.accuracy)
    }
  }
  
  result.table[f,length(result.table)-1]<-round(sum(abs(result.table[f,(length(names(result.table))-n.constr-1):(length(names(result.table))-2)])), which.accuracy)
  largest.value<-round(max(abs(result.table[f,(length(result.table)-n.constr-1):(length(result.table)-2)])), which.accuracy)
  largest.word<-as.character(constr[which(abs(result.table[f,(length(result.table)-n.constr-1):(length(result.table)-2)])==largest.value)])
  if (length(largest.word)>1) { largest.word<-paste(largest.word, collapse="_&_") }
  result.table[f,length(result.table)]<-largest.word
}

  result.table<-as.data.frame(result.table)
  fname <- paste(path,"/output_loop/cluster_output/",conceptlist[i],"_dcaOutput.txt",sep="")
  write.table(result.table,fname,fileEncoding = "UTF-8",sep="\t",row.names = FALSE)
}
