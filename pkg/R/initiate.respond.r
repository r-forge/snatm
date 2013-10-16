# Message initiation vs. response calculations

# Arguments: forest_corrected
# Value: matrix with dimension n x 3 where n is the number of authors in forest
# firest column contains author name
# second column contains number of initiations
 #third column contains number of responses

initiate.respond <- function(forest){
  authors <- c()
  forest <- ordermatrix(forest,by=2)

  ## Replace NA entries for the author with an explicit "NA" string
  forest[, "author"][is.na(forest[,"author"])] <- "NA"

  author.idx <- which(colnames(forest) == "author")
  for (i in unique(forest[,"threadID"])) {
    ## Select all messages in thread i
    thread <- forest[forest[,"threadID"]==i,]
    ## The author of the first message in the thread is identified as
    ## thread starter
    ## NOTE: We cannot use textual labels to select the author column
    ## since they are not preserved when there is only a single result row
    initiator <- thread[author.idx][1]

    if (!is.null(dim(thread)) && dim(thread)[1] > 1) {
      ## The thread is composed of more than one message
      ## First row gives initiating message, following rows describe
      ## responses
      answerers <- thread[2:dim(thread)[1], "author"]
    } else {
      answerers <- c()             #
    }                              #

    if (!is.element(initiator,authors[,1])){
      authors <- rbind(authors,c(initiator,1,0))
    }
    if (is.element(initiator,authors[,1])){
      authors[authors[,1]==initiator,2] <- as.numeric(authors[authors[,1]==initiator,2])+1
    }
    for (j in seq_along(answerers)){
      if (!is.element(answerers[j],authors[,1])){
        authors <- rbind(authors,c(answerers[j],0,1))
      }
      if (is.element(answerers[j],authors[,1])){
        authors[authors[,1]==answerers[j],3] <- as.numeric(authors[authors[,1]==answerers[j],3])+1
      }
    }
  }
  authors
}
#a <- ans.quest(forest_corrected)
