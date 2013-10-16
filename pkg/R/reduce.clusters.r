reduce.clusters <- function(clusters){
  for (i in seq_along(clusters)){
    match <- clusters[[i]][1]
    split <- unlist(strsplit(match,split=" "))
    first <- split[1]
    last <- split[length(split)]
    if (nchar(first)>0&nchar(last)>0){
      for (j in 2:length(clusters[[i]])){
        matchj <- clusters[[i]][j]
        splitj <- unlist(strsplit(matchj,split=" "))
        firstj <- splitj[1]
        lastj <- splitj[length(splitj)]
        if (first==firstj&last!=lastj&length(agrep(last,lastj,max.distance=0.3))==0){
          clusters[[i]][j] <- ""
        }
        if (last==lastj&first!=firstj&length(agrep(first,firstj,max.distance=0.3))==0){
          clusters[[i]][j] <- ""
        }
      }
    }
    clusters[[i]] <- clusters[[i]][nchar(clusters[[i]])>0]
  }
  len <- unlist(lapply(clusters,length))
  clusters1 <- clusters[len>1]
  clusters1
}
