centrality.edgelist <- function(terms,apply.to=c("subjects","content"),list=c("rhelp","rdevel")){
  edgelist <- c()
  for (i in seq_along(terms)){
    load(paste(list,"/",apply.to,"/net_",terms[i],".rda",sep=""))
    if (dim(net)[1]>19){
      net <- sna::component.largest(net,result="graph",connected="weak")
      if (dim(net)[1]>19){
        authors <- rownames(net)
        value <- sna::degree(net,cmode="outdegree")    #outdegree
        value <- cbind(authors,value)
        value <- ordermatrix(value,2)
        value <- cbind(value[,1],seq(1:dim(value)[1])/dim(value)[1])
        edgelist <- rbind(edgelist,cbind(value[,1],terms[i],value[,2]))
      }
    }
  }
  edgelist
}
