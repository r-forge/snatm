ordermatrix <- function(input,by){
  ranks <- rank(as.numeric(input[,by]),ties.method="first")
  output <- input[order(ranks),]
  output
}