createedges <- 
function (forest, subjectfilter = NULL, contentfilter = NULL, 
    lv = "nom") 
{
    edgelist <- c()
    if (is.null(dim(forest))) {
      cat("Pathological case: Forest with a single message; not handling ",
          "this case\n")
      return(NA)
    }
    threadID.idx <- which(colnames(forest)=="threadID")
    author.idx <- which(colnames(forest)=="author")
    subject.idx <- which(colnames(forest)=="subject")
    content.idx <- which(colnames(forest)=="content")
    ncol=dim(forest)[2]

    if (length(subjectfilter) > 0) {
        forest <- forest[grep(forest[, subject.idx], pattern = subjectfilter,
                              fixed=TRUE), ]
    }
    if (length(contentfilter) > 0) {
        forest <- forest[grep(forest[, content.idx], pattern = contentfilter,
                         fixed=TRUE), ]
    }
    if (length(forest) > 0) {
        forest <- matrix(forest, ncol=ncol)
        for (i in unique(as.numeric(forest[,threadID.idx]))) {
            thread <- forest[as.numeric(forest[,threadID.idx]) == i, ]
            if (is.null(dim(thread))) {
                ## Exactly one contribution in the thread, author thread
                ## starter talks to himself
                aparty <- bparty <- thread[author.idx]
                value <- 1
                threadid <- thread[threadID.idx]
                subject <- thread[subject.idx]
                content <- thread[content.idx]
                edgelist <- rbind(edgelist, cbind(aparty, bparty, 
                  1, threadid, subject, content))
            }
            else if (dim(thread)[1] > 1) {
                for (j in 2:dim(thread)[1]) {
                  aparty <- thread[j, author.idx]
                  bparty <- thread[1:(j - 1), author.idx]
                  subject <- thread[j, subject.idx]
                  content <- thread[j, content.idx]
                  threadid <- thread[1, threadID.idx]
                  if (lv == "nom") {
                    value <- 1
                  }
                  if (lv == "d") {
                    if (length(bparty) == 1) {
                      value <- 1
                    }
                    if (length(bparty) > 1) {
                      value <- 1/order(seq(length(bparty):1), 
                        decreasing = TRUE)
                    }
                  }
                  edgelist <- rbind(edgelist, cbind(aparty, bparty, 
                    value, threadid, subject, content))
                }
            }
        }
        rownames(edgelist) <- NULL
        colnames(edgelist)[3] <- "value"
        edgelist
    }
}
