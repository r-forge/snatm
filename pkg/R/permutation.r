permutation <- function(network,namesequence){
  if (dim(network)[1]!=length(namesequence)){
    stop("Dimension of network and length of namesequence must be equal.")
  }
  if (length(unique(namesequence))<dim(network)[1]){
    stop("Each name in namesequence must be different.")
  }
  newnetwork <- network
  while (!all(rownames(newnetwork)==namesequence)){
    for (i in 1:dim(network)[1]){
      if (rownames(network)[i]!=namesequence[i]){
        columnposition <- which(rownames(network)==namesequence[i])
        newnetwork[,i] <- network[,columnposition]
        newnetwork[,columnposition] <- network[,i]
        tempcolumn <- newnetwork[i,]
        newnetwork[i,] <- newnetwork[columnposition,]
        newnetwork[columnposition,] <- tempcolumn
        temprowname <- colnames(newnetwork)[i]
        colnames(newnetwork)[i] <- rownames(newnetwork)[i] <- rownames(network)[columnposition]
        colnames(newnetwork)[columnposition] <- rownames(newnetwork)[columnposition] <- temprowname#colnames(network)[i]
        network <- newnetwork
      }
    }
  }
  newnetwork
}
