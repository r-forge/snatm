shrink <- function(network,by=c("row","column")
                  ,keep=NULL # numeric vector
                  ,values=c("sum","min","max")){ # how to aggregate values
  newnet <- matrix(0,ncol=dim(network)[1],nrow=dim(network)[1])
  if (by=="row"){
    for (i in 1:dim(network)[1]){
      neighbors <- which(network[i,]>0)
      if (length(neighbors)>0){
        for (j in seq_along(neighbors)){
          ord2neighbors <- which(network[,neighbors[j]]>0)
          if (length(ord2neighbors)>0){
            if (values=="sum"){
              newnet[i,ord2neighbors] <- network[i,neighbors[j]]+network[neighbors[j],ord2neighbors]
            }
            if (values=="min"){
              newnet[i,ord2neighbors] <- pmin(network[i,neighbors[j]],network[neighbors[j],ord2neighbors])
            }
           if (values=="max"){
              newnet[i,ord2neighbors] <- pmax(network[i,neighbors[j]],network[neighbors[j],ord2neighbors])
            }
          }
        }
      }
    }
  }
  if (by=="column"){
    for (i in 1:dim(network)[1]){ 
      neighbors <- which(network[,i]>0)
      if (length(neighbors)>0){
        for (j in seq_along(neighbors)){
          ord2neighbors <- which(network[neighbors[j],]>0)
          if (length(ord2neighbors)>0){
            if (values=="sum"){
              newnet[ord2neighbors,i] <- network[neighbors[j],i]+network[ord2neighbors,neighbors[j]]
            }
            if (values=="min"){
              newnet[ord2neighbors,i] <- pmin(network[neighbors[j],i],network[ord2neighbors,neighbors[j]])
            }
           if (values=="max"){
              newnet[ord2neighbors,i] <- pmax(network[neighbors[j],i],network[ord2neighbors,neighbors[j]])
            }
          }
        }
      }
    }
  }
  diag(newnet) <- 0
  rownames(newnet) <- colnames(newnet) <- rownames(network)
  if (length(keep)>0){
    newnet <- newnet[,keep]
    newnet <- newnet[keep,]
  }
  newnet
}
