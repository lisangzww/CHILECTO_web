setwd("D:/KUL-MSCA/Box Sync/Model/mds_tokencloud")
Sys.setlocale(category = "LC_ALL", locale = "Chinese")
path = "D:/KUL-MSCA/Box Sync/Model/mds_tokencloud"



library(ggplot2)
library(tidyr)
library(knitr)
library(dplyr)
library(kableExtra)
library(kfigr)
library(plotly, quietly=T)
library(devEMF)
library(RColorBrewer, quietly=T)
library(dynamicTreeCut)
library(dendextend, quietly=T)
library(colorspace)
library(RcppCNPy)
library (rjson)
library(plyr)
library(pinyin)

convar <- read.table("D:/KUL-MSCA/Box Sync/Model/wordnet_uniformity/wordnet_add_final.csv",sep="\t",header=T) %>%
  filter(testsample=="yes") %>% # select some concept samples for test
  select("concept") %>%
  droplevels()

conceptlist <- as.character(levels(convar[,1]))
conceptlist


concept <- "accident_07301336-n"

for (concept in conceptlist){
fname <- paste(path,"/output_loop/mds_output/",concept,"_ppmi_mds.csv",sep="")

d.mds <- read.csv(fname, sep=',',fileEncoding = "UTF-8")
# d.mds <- read.csv(fname, sep=',')

p <- ggplot(d.mds, aes(x,y)) +
  geom_point(aes(colour=variant,shape=lect,text=text)) +
  #   scale_shape_manual(value=c(3,16)) +
  #   scale_color_manual(values=c("red","green")) +
  theme_bw() +
  ggtitle(concept) +
  scale_shape_manual(values = c(1, 2, 3))


q <- ggplotly(p,tooltip=c("text","variant","lect"),opacity=0.5)

q %>% layout(hoverlabel = list(bgcolor=c("rgb(255,255,204")))

}



# cluster analysis
concept <- c("program_broadcast_06619428-n")
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
set.seed(1026)
d <- tokvecs_dist
sampleItems <- sample(rownames(d),50)
d.sub <- d[sampleItems,sampleItems]

variant = '广播'

hcl <- hclust(as.dist(d.sub), method="ward.D2")
dend <- as.dendrogram(hcl)
clusters <- cutree(hcl, h=unique(hcl$height)[0.8])
clusters <- clusters[order.dendrogram(dend)]
clusters_numbers <- unique(clusters) - (0 %in% clusters)
n_clusters <- length(clusters_numbers)

variant_label <- sapply(strsplit(labels(dend),"/"), '[',1)
py_label <- py(variant_label) # convert to pinyin
variant_py_label <- paste0(variant_label,py_label,sep="")


true_species_cols <- ifelse(variant_label == variant,"red",'blue')

dend2 <- dend %>% 
  branches_attr_by_clusters(clusters) %>%
  color_labels(col = true_species_cols)

labels(dend2) <- variant_label
par(mar = c(2,2,0,0))
dend2 <- set(dend2, "labels_cex", 0.5)
plot(dend2)
legend("topleft", legend =c(levels(as.factor(variant_py_label))), fill = c("red","blue"))


# plot dendrogram 
corpusDay<-sapply(strsplit(labels(dend),"/"), '[',3)
lect_label_temp<-as.factor(sapply(strsplit(corpusDay,"_"), '[',1))
lect_label <- revalue(lect_label_temp, c("XIN"="ML", "CNA"="TW", "ZBN"="SG"))

lect = "TW"
cols <- rainbow_hcl(nlevels(as.factor(lect_label)))
true_species_cols <- ifelse(lect_label == lect,"red",ifelse(lect_label == 'ML', "green", "blue"))

dend2 <- dend %>% 
  branches_attr_by_clusters(clusters, values = cols) %>%
  color_labels(col = true_species_cols)


labels(dend2) <- lect_label
par(mar = c(2,2,0,0))
dend2 <- set(dend2, "labels_cex", 0.5)
plot(dend2)
legend("topleft", legend =c(levels(as.factor(lect_label))), fill = c("red","green","blue"))



#----------------add pinyin to variants
## add pinyin to each variant
convar <- read.table("wordnet_add_final.csv",sep="\t",header=T) %>%
  filter(testsample=="yes") %>% 
  select("concept") %>%
  droplevels()

conceptlist <- as.character(levels(convar[,1]))

concept = "accident_07301336-n"

fname <- paste(path,"/output_loop/mds_output/",concept,"_pos_mds.csv",sep="")
d.mds <- read.csv(fname, sep=',',fileEncoding = "UTF-8")

library(pinyin) # The function "pinyin()" is deprecated. Please use "py()" instead.
d.mds$pinyin <- py(as.character(d.mds$variant))
d.mds$variant_py <- paste0(d.mds$variant,d.mds$pinyin,sep="")


#------------mds plot with cluster labeling------------------------------
for (concept in conceptlist){
fname<-paste(path,"/output_loop/cluster_output/",concept,"_hybrid_clusterID.csv",sep="")

cluster_hybrid.df <- read.table(fname,sep=',',fileEncoding = "UTF-8",header=T)
cluster_hybrid <- cluster_hybrid.df %>% select ("tokenlabel","cluster")
colnames(cluster_hybrid)<-c("id","cluster")

fname<-paste(path,"/output_loop/cluster_output/",concept,"_tree_clusterID.csv",sep="")
cluster_tree.df <- read.table(fname,sep=',',fileEncoding = "UTF-8",header=T)
cluster_tree <- cluster_tree.df %>% select ("tokenlabel","cluster")
colnames(cluster_tree)<-c("id","cluster")

fname <- paste(path,"/output_loop/mds_output/",concept,"_pos_mds.csv",sep="")
d.mds <- read.csv(fname, sep=',',fileEncoding = "UTF-8")

d.mds$pinyin <- py(as.character(d.mds$variant))
d.mds$variant_py <- paste0(d.mds$variant,d.mds$pinyin,sep="")

p <- ggplot(d.mds, aes(x,y)) +
  geom_point(aes(colour=variant_py,shape=lect,
                 text=paste('<b>Lect</b>: ', d.mds$lect,
                            '<br><b>Variant</b>: ', d.mds$variant_py,
                            '<br><b>Context</b>: ', d.mds$text)))+
  theme_bw() +
  ggtitle(concept) +
  scale_shape_manual(values = c(1, 2, 3))

#merge mds and clusterID dataframes into one (add a column "cluter" to d.mds)
mds.cluster_hybrid <- merge(d.mds,cluster_hybrid,by="id",all.x=TRUE)
mds.cluster_hybrid$cluster<-as.factor(mds.cluster_hybrid$cluster)

mds.cluster_tree <- merge(d.mds,cluster_tree,by="id",all.x=TRUE)
mds.cluster_tree$cluster<-as.factor(mds.cluster_tree$cluster)
}


require(gridExtra)
library(gridExtra)
p1 <- ggplot(mds.cluster_hybrid, aes(x,y,colour=cluster)) +
  theme_bw() +
  geom_text(aes(label=cluster))+
  ggtitle(paste(concept,"hybrid_cluster")) 

p2 <- ggplot(mds.cluster_tree, aes(x,y,colour=cluster)) +
  theme_bw() +
  geom_text(aes(label=cluster))+
  ggtitle(paste(concept,"tree_cluster")) 

grid.arrange(p1,p2,ncol=2)

library(grid)
grid.newpage()
pushViewport(viewport(layout=grid.layout(2,2)))
vplayout <- function(x,y)
  viewport(layout.pos.row=x, layout.pos.col=y)
print(p, vp=vplayout(1,1:2))
print(p1, vp=vplayout(2,1))
print(p2, vp=vplayout(2,2))