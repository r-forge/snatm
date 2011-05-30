# Choose which list (help or devel) and which terms (subjects or content) you want to analyse.
list <- "devel" # or "help"
terms.from <- "subjects" # or "content"


# Create folder tree
dir.create(list)

# Choose the months you want to analyse.
files <- c("2008-January.txt","2008-February.txt","2008-March.txt","2008-April.txt","2008-May.txt"
          ,"2008-June.txt","2008-July.txt","2008-August.txt","2008-September.txt","2008-October.txt","2008-November.txt","2008-December.txt"
          ,"2009-January.txt","2009-February.txt","2009-March.txt","2009-April.txt","2009-May.txt")


# Download the mailing lists into folter "list", e.g.
#setInternet2(use = TRUE)
#for (i in files){
#  download.file(paste("https://stat.ethz.ch/pipermail/r-devel/",i,".gz",sep=""), paste(file.path(list,i)))
#}
          
                    
# Write all these e-mails in one file.
filename <- file.path(list,"allthreads")
as.one.file(files,dest=paste(filename,".txt",sep=""),list=list)


# Create a forest of the mailing list data stored in filename. Result will be a file named "filename_forest.rda".
forest <- makeforest(filename)
save(forest,file=paste(filename,"_forest.rda",sep=""))


# Find and replace aliases.
load(paste(filename,"_forest.rda",sep=""))
authors <- forest[,3]
a <- normalizeauthors(authors)
b <- sapply(a,sortnames,USE.NAMES=FALSE)
c <- sapply(b,emailfirst,USE.NAMES=FALSE)
# Load databases for aliases that already have been found and have been accpted and not accepted, respectively.
data(take.memory) # First vector element of each list element i (take.memory[[i]][1]) can be replaced by any of the following.
data(not.take.memory) # First vector element of each list element i (not.take.memory[[i]][1]) cannot be replaced by any of the following.
d <- changenames(clusters=take.memory,forest=c,accept=1:length(take.memory))
# clusters is a list where each list element contains a vector.
# First vector element is matched string and following elements are matches found.
#clusters <- findclusters(unique(d),not.take.memory=not.take.memory)
# if length(clusters)>0:
# Manually look at every cluster [[i]] and decide whether to accept it or not.
# Write numbers of accepted clusters like this: accept <- c(1,3,4) (if clusters 1, 3 and 4 are accepted)
# and numbers of not accepted clusters like this: not.accept <- c(2,6) (if clusters 2 and 5 are not accepted)
# and type
# take.memory <- c(take.memory,clusters[accept])
# not.take.memory <- c(not.take.memory,clusters[not.accept])
# save(take.memory,file="take.memory.rda")
# save(not.take.memory,file="not.take.memory.rda")
# to store the new databases.
# Please report any false entries in take.memory or not.take.memory to angela.bohn@gmail.com.
# If clusters contain some aliases that should be accepted and some that should not
# (e.g. "johnsmith|John Smith" "jsmith|John Smith" "jsmithers|John Smithers")
# type
# take.memory <- list(c("johnsmith|John Smith" "jsmith|John Smith" ))
# not.take.memory <- list(c("johnsmith|John Smith","jsmithers|John Smithers"))
# and then
# take.memory <- c(take.memory,take)
# not.take.memory <- c(not.take.memory,not.take)
# save(take.memory,file="take.memory.rda")
# save(not.take.memory,file="not.take.memory.rda")
# If any more clusters to accept were found:
# e <- changenames(clusters,forest=d,accept=accept)
# f <- final(e)
# otherwise
f <- final(d)
# Replace Author-Column in forest and save as forest_corrected
forest_corrected <- cbind(forest[,1:2],f,forest[,4:5])
colnames(forest_corrected)[3] <- colnames(forest)[3]
save(forest_corrected,file=paste(filename,"_forest_corrected.rda",sep=""))


# Build entire communication network
load(file=paste(filename,"_forest_corrected.rda",sep=""))
commlist <- createedges(forest_corrected)
save(commlist,file=file.path(list,paste("commlist_",list,".rda",sep="")))
commnet <- adjacency(commlist) # might require a lot of working space
save(commnet,file=file.path(list,paste("commnet_",list,".rda",sep="")))


# Prepare textual data:
load(file=paste(filename,"_forest_corrected.rda",sep=""))
temp <- prepare.text(forest_corrected,terms.from=terms.from,list=list)
termfreq <- temp[[1]]
save(termfreq,file=file.path(list,paste("termfreq_",terms.from,".rda",sep="")))
forest_corrected <- temp[[2]]
save(forest_corrected,file=paste(filename,"_forest_corrected_prep_",terms.from,".rda",sep=""))


# Create communication networks of all people who used a certain term contained in termfreq_subjects/termfreq_content
# Results are saved as net_terms[i].
load(file=paste(filename,"_forest_corrected_prep_",terms.from,".rda",sep=""))
load(file=file.path(list,paste("termfreq_",terms.from,".rda",sep="")))
dir.create(file.path(list,terms.from))
extract.commnet(forest_corrected,names(termfreq),apply.to=terms.from,list=list)


# Create two-mode network: people and terms
# Result is saved as "peopleandterms_edgelist.rda"
load(file=file.path(list,paste("termfreq_",terms.from,".rda",sep="")))
edgelist <- centrality.edgelist(terms=names(termfreq),apply.to=terms.from,list=list)
save(edgelist,file=file.path(list,paste("peopleandterms_",terms.from,"_edgelist.rda",sep="")))
net <- adjacency(edgelist,mode="addvalues",directed=F)
save(net,file=file.path(list,paste("peopleandterms_",terms.from,"_net.rda",sep="")))


# 2-mode plot
#load(file.path("help","peopleandterms_subjects_net.rda"))
#load(file.path("help","peopleandterms_subjects_edgelist.rda"))
#peoplelist <- edgelist[,1]
#peoplelist <- peoplelist[peoplelist!="data"]
#peoplelist <- peoplelist[peoplelist!="dat"]
#peoplelist <- peoplelist[peoplelist!="start"]
#peoplelist <- peoplelist[peoplelist!="linux"]
#twomode <- net
#twomode[twomode<0.9955] <- 0
#deg <- sna::degree(twomode,cmode="freeman")
#twomode <- twomode[,deg>0]
#twomode <- twomode[deg>0,]
#twomode <- sna::component.largest(twomode,connected="weak",result="graph")
#deg <- sna::degree(twomode)
#people <- which(is.element(rownames(twomode),unique(peoplelist)))
#labelcol <- rep(rgb(0,0,1,0.75),dim(twomode)[1])
#labelcol[people] <- "red"
#par(mar=c(0,0,0,0))
#gplot.snatm(twomode
#     ,gmode="graph"
#     ,vertex.col="white"
#     ,vertex.cex=1
#     ,label=rownames(twomode)
#     ,label.col=labelcol
#     ,label.cex=(deg^0.25)*0.35
#     ,label.pos=5
#     ,boxed.labels=FALSE
#     ,edge.lwd=0.1
#     ,vertex.border="white"
#     ,edge.col="grey")


# Make interest network from 2-mode-network (help/subjects version in the article)
load(file=file.path(list,paste("peopleandterms_",terms.from,"_net.rda",sep="")))
load(file=file.path(list,paste("peopleandterms_",terms.from,"_edgelist.rda",sep="")))
people <- which(is.element(rownames(net),unique(edgelist[,1])))
interestnet <- shrink(net,by="row",keep=people,values="min")
save(interestnet,file=file.path(list,paste("interestnet_",terms.from,".rda",sep="")))


# Compare communication network and interest network
    
    
# Figure 3
load(file.path(list,paste("interestnet_",terms.from,".rda",sep="")))
load(file.path(list,paste("commnet_",list,".rda",sep="")))
if (any(is.na(rownames(commnet)))){
  for (i in 1:which(is.na(rownames(commnet)))){
    rownames(commnet)[is.na(rownames(commnet))][i] <- colnames(commnet)[is.na(rownames(commnet))][i] <- paste("NA",i,sep="")
  }
}
if (any(is.na(rownames(interestnet)))){
  for (i in 1:which(is.na(rownames(commnet)))){
    rownames(commnet)[is.na(rownames(commnet))][i] <- colnames(commnet)[is.na(rownames(commnet))][i] <- paste("NA",i,sep="")
  }
}
network_red <- commnet[is.element(rownames(commnet),rownames(interestnet)),is.element(rownames(commnet),rownames(interestnet))]
network_red <- permutation(network_red,rownames(interestnet))
save(network_red,file=file.path(list,paste("network_red_",terms.from,"_permuted.rda",sep="")))
network_red_ig <- graph.adjacency(network_red, mode="directed")
save(network_red_ig,file=file.path(list,paste("network_red_",terms.from,"_permuted_ig.rda",sep="")))
write.graph(network_red_ig,file=file.path(list,paste("network_red_",terms.from,"_permuted.net",sep="")),format="pajek")
deg <- sna::degree(network_red,cmode="freeman",gmode="graph",ignore.eval=TRUE)
betw <- igraph::betweenness(network_red_ig,directed=F)
# Calculate closeness externally in Pajek (http://vlado.fmf.uni-lj.si/pub/networks/pajek/) save it or
# load it from the package
#clo <- read.table(file.path(list,paste("network_red_",terms.from,"_permuted_closeness.vec",sep="")),skip=1)
#clo <- as.vector(as.matrix(clo))
data(devel.subjects.closeness) 
clo <- devel.subjects.closeness # or
#data(devel.content.closeness) 
#clo <- devel.content.closeness # or
#data(help.subjects.closeness) 
#clo <- help.subjects.closeness # or
#data(help.content.closeness)
#clo <- help.content.closeness 
centm <- list(deg,betw,clo)
save(centm,file=file.path(list,paste("network_red_",terms.from,"_permuted_centm.rda",sep="")))

#load(file.path("help","network_red_subjects_permuted.rda"))
#diag(network_red) <- 0
#load(file.path("help","interestnet_subjects.rda"))
#par(mar=c(4,4,4,0.5))
#load(file.path("help","network_red_subjects_permuted_centm.rda"))
#for (k in seq_along(centm)){
#  a <- seq(0,max(centm[[k]]),by=max(centm[[k]])/100)
#  c <- c()
#  for (i in a){
#    b <- cor(as.vector(interestnet[centm[[k]]>=i,centm[[k]]>=i]),as.vector(network_red[centm[[k]]>=i,centm[[k]]>=i]))
#    if (!is.na(b)){
#      c <- c(c,b)
#    }
#  }
#  names(c) <- seq(0,max(centm[[k]]),length.out=length(c))#,by=max(centm[[k]])/100)
#  d <- unique(c)
#  for (i in seq_along(d)){
#    names(d)[i] <- which(d[i]==c)[1]
#  }
#  x <- as.numeric(names(d))
#  names(d) <- as.character((x-min(x))/(max(x)-min(x)))
#  plot(as.numeric(names(d)),d
#      ,ylab="Correlation"
#      ,cex.lab=1
#      ,ylim=c(0,1)
#      ,cex.axis=1
#      ,main="R-help subjects"
#      ,cex.main=1
#      ,xlab=""
#      ,col=c("skyblue3","blue","darkblue")[k]
#      ,type="l"
#      ,lwd=3
#      ,lty=c(1,2,3)[k])
#  par(new=T,yaxt="n",xaxt="n")
#}
#text(labels=paste("n=",dim(network_red)[1],sep=""),x=0.15,y=1)

#load(file.path("help","network_red_content_permuted.rda"))
#diag(network_red) <- 0
#load(file.path("help","interestnet_content.rda"))
#par(yaxt="s",xaxt="s")
#load(file.path("help","network_red_content_permuted_centm.rda"))
#for (k in seq_along(centm)){
#  a <- seq(0,max(centm[[k]]),by=max(centm[[k]])/100)
#  c <- c()
#  for (i in a){
#    b <- cor(as.vector(interestnet[centm[[k]]>=i,centm[[k]]>=i]),as.vector(network_red[centm[[k]]>=i,centm[[k]]>=i]))
#    if (!is.na(b)){
#      c <- c(c,b)
#    }
#  }
#  names(c) <- seq(0,max(centm[[k]]),length.out=length(c))#,by=max(centm[[k]])/100)
#  d <- unique(c)
#  for (i in seq_along(d)){
#    names(d)[i] <- which(d[i]==c)[1]
#  }
#  x <- as.numeric(names(d))
#  names(d) <- as.character((x-min(x))/(max(x)-min(x)))
#  plot(as.numeric(names(d)),d
#      ,ylab=""
#      ,ylim=c(0,1)
#      ,cex.lab=1
#      ,cex.axis=1
#      ,main="R-help content"
#      ,cex.main=1
#      ,xlab=""
#      ,col=c("skyblue3","blue","darkblue")[k]
#      ,type="l"
#      ,lwd=3
#      ,lty=c(1,2,3)[k])
#  par(new=T,yaxt="n",xaxt="n")
#}
#text(labels=paste("n=",dim(network_red)[1],sep=""),x=0.15,y=1)

load(file.path("devel","network_red_subjects_permuted.rda"))
diag(network_red) <- 0
load(file.path("devel","interestnet_subjects.rda"))
par(mfrow=c(1,2),mar=c(4,4,4,0.5))
load(file.path("devel","network_red_subjects_permuted_centm.rda"))
for (k in seq_along(centm)){
  a <- seq(0,max(centm[[k]]),by=max(centm[[k]])/100)
  c <- c()
  for (i in a){
    b <- cor(as.vector(interestnet[centm[[k]]>=i,centm[[k]]>=i]),as.vector(network_red[centm[[k]]>=i,centm[[k]]>=i]))
    if (!is.na(b)){
      c <- c(c,b)
    }
  }
  names(c) <- seq(0,max(centm[[k]]),length.out=length(c))#,by=max(centm[[k]])/100)
  d <- unique(c)
  for (i in seq_along(d)){
    names(d)[i] <- which(d[i]==c)[1]
  }
  x <- as.numeric(names(d))
  names(d) <- as.character((x-min(x))/(max(x)-min(x)))
  plot(as.numeric(names(d)),d
      ,ylab=paste("Correlation")
      ,cex.lab=1
      ,cex.axis=1
      ,main="R-devel subjects"
      ,cex.main=1
      ,xlab="Centrality"
      ,col=c("skyblue3","blue","darkblue")[k]
      ,type="l"
      ,ylim=c(0,1)
      ,lwd=3
      ,lty=c(1,2,3)[k])
  par(new=T,yaxt="n",xaxt="n")
}
text(labels=paste("n=",dim(network_red)[1],sep=""),x=0.15,y=1)

#load(file.path("devel","network_red_content_permuted.rda"))
#diag(network_red) <- 0
#load(file.path("devel","interestnet_content.rda"))
#par(yaxt="s",xaxt="s")
#load(file.path("devel","network_red_content_permuted_centm.rda"))
#for (k in seq_along(centm)){
#  a <- seq(0,max(centm[[k]]),by=max(centm[[k]])/100)
#  c <- c()
#  for (i in a){
#    b <- cor(as.vector(interestnet[centm[[k]]>=i,centm[[k]]>=i]),as.vector(network_red[centm[[k]]>=i,centm[[k]]>=i]))
#    if (!is.na(b)){
#      c <- c(c,b)
#    }
#  }
#  names(c) <- seq(0,max(centm[[k]]),length.out=length(c))#,by=max(centm[[k]])/100)
#  d <- unique(c)
#  for (i in seq_along(d)){
#    names(d)[i] <- which(d[i]==c)[1]
#  }
#  x <- as.numeric(names(d))
#  names(d) <- as.character((x-min(x))/(max(x)-min(x)))
#  plot(as.numeric(names(d)),d
#      ,ylab=""
#      ,cex.lab=1
#      ,cex.axis=1
#      ,main="R-devel content"
#      ,cex.main=1
#      ,xlab="Centrality"
#      ,col=c("skyblue3","blue","darkblue")[k]
#      ,type="l"
#      ,lwd=3
#      ,ylim=c(0,1)
#      ,lty=c(1,2,3)[k])
#  par(new=T,yaxt="n",xaxt="n")
#}
#text(labels=paste("n=",dim(network_red)[1],sep=""),x=0.15,y=1)

par(mar=c(0,0,0,0))
plot(1,1,col="transparent",ann=F,axes=F)
legend(legend=c("Degree","Betweenness","Closeness")
      ,x="center"
      ,col=c("skyblue3","blue","darkblue")
      ,lty=c(1,2,3)
      ,lwd=3
      ,bty="n"
      ,horiz=T)


# Figure 4

#load(file.path("help","allthreads_forest_corrected.rda"))
#ansquest <- ans.quest(forest_corrected)
#save(ansquest,file=file.path("help","ansquest.rda"))
#load(file.path("help","network_red_subjects_permuted_centm.rda"))
#load(file.path("help","network_red_subjects_permuted.rda"))
#deg <- cbind(rownames(network_red),centm[[1]])
#cent <- c()
#for (i in 1:dim(deg)[1]){
#  cent <- rbind(cent
#  ,c(as.numeric(ansquest[deg[i,1]==ansquest[,1],2])
#    ,as.numeric(ansquest[deg[i,1]==ansquest[,1],3])))
#}
#cent <- cbind(cent,as.numeric(deg[,2]))
#rownames(cent) <- deg[,1]
#colnames(cent) <- c("questions","answers","deg")
#save(cent,file=file.path("help","network_red_subjects_permuted_cent.rda"))

load(file.path("devel","allthreads_forest_corrected.rda"))
ansquest <- ans.quest(forest_corrected)
save(ansquest,file=file.path("devel","ansquest.rda"))
load(file.path("devel","network_red_subjects_permuted_centm.rda"))
load(file.path("devel","network_red_subjects_permuted.rda"))
deg <- cbind(rownames(network_red),centm[[1]])
cent <- c()
for (i in 1:dim(deg)[1]){
  cent <- rbind(cent
  ,c(as.numeric(ansquest[deg[i,1]==ansquest[,1],2])
    ,as.numeric(ansquest[deg[i,1]==ansquest[,1],3])))
}
cent <- cbind(cent,as.numeric(deg[,2]))
rownames(cent) <- deg[,1]
colnames(cent) <- c("questions","answers","deg")
save(cent,file=file.path("devel","network_red_subjects_permuted_cent.rda"))

#par(mfrow=c(1,2),mar=c(4.2,4,3,0.5))
#load(file=file.path("help","network_red_subjects_permuted_cent.rda"))
# adjust scales
#cent[dim(cent)[1],] <- c(max(cent[,1]),max(cent[,2])+200,0)
#deg <- normalize(cent[,3])
#col <- rep("black",dim(cent)[1])
#col[deg>0.4] <- "red"
#col[dim(cent)[1]] <- "transparent"
#plot(cent[,1],cent[,2]
#    ,cex=deg*5,col=col
#    ,xlab=paste("Number of questions","\n","(log scale)")
#    ,ylab="Number of answers (log scale)"
#    ,main="R-help",log="xy")
#
load(file.path("devel","network_red_subjects_permuted_cent.rda"))
# adjust scales
cent[dim(cent)[1],] <- c(max(cent[,1])+40,max(cent[,2])+100,0)
deg <- normalize(cent[,3])
col <- rep("black",dim(cent)[1])
col[deg>0.2] <- "red"
col[dim(cent)[1]] <- "transparent"
plot(cent[,1],cent[,2]
    ,cex=deg*5,col=col
    ,ylab="",xlab=paste("Number of questions","\n","(log scale)")
    ,main="R-devel",log="xy")
    
load(file.path("help","network_red_subjects_permuted_cent.rda"))
# adjust scales
cent[dim(cent)[1],] <- c(max(cent[,1])+40,max(cent[,2])+100,0)
deg <- normalize(cent[,3])
col <- rep("black",dim(cent)[1])
col[deg>0.2] <- "red"
col[dim(cent)[1]] <- "transparent"
pdf("")
plot(cent[,1],cent[,2]
    ,cex=deg*5,col=col
    ,ylab="",xlab=paste("Number of questions","\n","(log scale)")
    ,main="R-help",log="xy")

