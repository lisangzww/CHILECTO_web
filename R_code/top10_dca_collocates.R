dca_col_all_list <- list()

for (i in 1:length(conceptlist)){

fname <- paste(path,"/output_loop/cluster_output/",conceptlist[i],"_dcaOutput.txt",sep="")
result.table <- read.table(fname,fileEncoding = "UTF-8",header=T)


temp <- tokencluster %>% select("concept","cluster") 

cluster.df <- aggregate(temp$cluster, by=list(Category=temp$concept), FUN=max)
colnames(cluster.df)<-c("concept","n_cluster")
ncluster <- cluster.df[cluster.df$concept==conceptlist[i],]$n_cluster


# loop to get the top 10 distinctive collocates for each cluster 
datalist <- list()

for (j in 1:ncluster) {
  
result.table.filtered <- result.table%>% 
  filter(result.table[,(ncluster*2+j+1)]>1.30103)%>%
  droplevels()

dca_col_top10<-as.character(result.table.filtered[order(result.table.filtered[,(ncluster*2+j+1)],decreasing = T),c(1)])[c(1:10)]
dca_col_top10 <- paste(dca_col_top10,collapse=" ")
datalist[[j]]<-dca_col_top10

}

dca_col_df <- ldply(datalist,rbind)
dca_col_df$cluster <- c(1:ncluster)
dca_col_df$concept <- conceptlist[i]
colnames(dca_col_df)<-c("top10dca_collo","cluster","concept")

dca_col_all_list[[i]]<-dca_col_df
}

dca_col_all_df <- ldply(dca_col_all_list,rbind)

library(translateR)
google.dataset.out<-translate(dataset=dca_col_all_df,content.field = "top20dca_collo",google.api.key = my.api.key,source.lang = "cn",
                              target.lang = "en")


library(googleLanguageR)
text <- as.character(dca_col_all_df$top10dca_collo[1])
gl_translate(text,target = "en")$translatedText
