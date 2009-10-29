extract.commnet <- function(forest,terms,apply.on=c("subjects","content"),list=c("rhelp","rdevel")){
  for (i in seq_along(terms)){
    if (apply.on=="subjects"){
      net <- createedges(forest,subjectfilter=terms[i])
    }
    if (apply.on=="content"){
      net <- createedges(forest,contentfilter=terms[i])
    }
    if (dim(net)[1]>0){
      net <- makematrix(net)
      save(net,file=paste(list,"/",apply.on,"/net_",terms[i],".rda",sep=""))
    }
  }
  print("Done")
}
