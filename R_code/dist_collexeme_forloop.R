fname <- paste(path,"/output_loop/cluster_output/",conceptlist[i],"dcaOutput.txt",sep="")
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
fname <- paste(path,"/output_loop/cluster_output/","test_output.txt",sep="")
write.table(result.table,fname,fileEncoding = "UTF-8",sep="\t",row.names = FALSE)
