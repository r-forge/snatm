# Message initiation vs. response calculations

# Arguments: forest_corrected
# Value: matrix with dimension n x 3 where n is the number of authors in forest
# firest column contains author name
# second column contains number of initiations
 #third column contains number of responses

initiate.respond <- function(forest){
  ir <- data.frame(name=unique(na.omit(unique(forest[,"author"]))),
                   initiations=0, responses=0)
  forest <- ordermatrix(forest, by="threadID")

  author.idx <- which(colnames(forest) == "author")
  for (i in unique(forest[,"threadID"])) {
    ## Select all messages in thread i
    thread <- forest[forest[,"threadID"]==i,]
    ## The author of the first message in the thread is identified as
    ## thread starter
    ## NOTE: We cannot use textual labels to select the author column
    ## since they are not preserved when there is only a single result row
    if (is.null(dim(thread))) {
      initiator <- thread[author.idx]
    } else {
      initiator <- thread[1, author.idx]
    }


    if (!is.null(dim(thread)) && dim(thread)[1] > 1) {
      ## The thread is composed of more than one message
      ## First row gives initiating message, following rows describe
      ## responses
      answerers <- thread[2:dim(thread)[1], "author"]
    } else {
      answerers <- NULL
    }

    ## Credit one more thread initiation to the current initiator
    if (!is.na(initiator)) {
      idx <- which(ir$name==initiator)
      if (length(idx) != 1) {
        stop("Internal error: Thread initiator not found in author list!")
      }

      ir$initiations[idx] <- as.numeric(ir$initiations[idx]) + 1
    }

    ## For all response messages in the thread, give one response credit
    ## to the respective author
    for (answerer in answerers) {
      if (is.na(answerer))
        next

      idx <- which(ir$name==answerer)
      if (length(idx) != 1) {
        stop("Internal error: Thread replyer not found in author list!")
      }

      ir$responses[idx] <- as.numeric(ir$responses[idx]) + 1
    }
  }

  return(ir)
}
#a <- ans.quest(forest_corrected)
