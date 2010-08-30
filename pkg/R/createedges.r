createedges <- function(forest,subjectfilter=NULL,contentfilter=NULL,lv="nom"){
  # lv: nom = number of mails, d = 1/distance in time tree
  # mode 2: everybody answers all parents in time tree
  edgelist <- c()
  if (length(subjectfilter)>0){
    forest <- forest[grep(forest[,4],pattern=subjectfilter),]
  }
  if (length(contentfilter)>0){
    forest <- forest[grep(forest[,5],pattern=contentfilter),]
  }
  if (length(forest)>0){
  forest <- matrix(forest,ncol=5)
    for (i in min(as.numeric(forest[,2])):max(as.numeric(forest[,2]))){
      thread <- forest[as.numeric(forest[,2])==i,]
      if (length(thread)==5){
        aparty <- bparty <- thread[3]
        value <- 1
        threadid <- thread[2]
        subject <- thread[4]
        content <- thread[5]
        edgelist <- rbind(edgelist,cbind(aparty,bparty,1,threadid,subject,content))
      }
      else if (dim(thread)[1]>1){
        for (j in 2:dim(thread)[1]){
          aparty <- thread[j,3]                       #aparty ansers bparty  #author in column 3
          bparty <- thread[1:(j-1),3]
          subject <- thread[j,4]
          content <- thread[j,5]
          threadid <- thread[1,2]
          if (lv=="nom"){
            value <- 1
          }
          if (lv=="d"){
            if (length(bparty)==1){
              value <- 1
            }
            if (length(bparty)>1){
              value <- 1/order(seq(length(bparty):1),decreasing=TRUE)
            }
          }
          edgelist <- rbind(edgelist,cbind(aparty,bparty,value,threadid,subject,content))
        }
      }
    }
    rownames(edgelist) <- NULL
    colnames(edgelist)[3] <- "value"
    edgelist
  }
}
