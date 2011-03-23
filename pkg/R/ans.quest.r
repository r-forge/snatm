# questioner vs. answerer

# Arguments: forest_corrected
# Value: matrix with dimension n x 3 where n is the number of authors in forest
# firest column contains author name
# second column contains number of questions
#third column contains number of answers

ans.quest <- function(forest){
  authors <- c()
  forest <- ordermatrix(forest,by=2)
  forest[,3][is.na(forest[,3])] <- "NA"
  for (i in seq_along(unique(forest[,2]))){
    thread <- forest[forest[,2]==i,]
    questioner <- matrix(thread,ncol=5)[1,3]
    if (length(thread)>5){
      answerers <- thread[2:dim(thread)[1],3]
    }
    if (length(thread)<=5){       #
      answerers <- c()             #
    }                              #
    if (!is.element(questioner,authors[,1])){
      authors <- rbind(authors,c(questioner,1,0))
    }
    if (is.element(questioner,authors[,1])){
      authors[authors[,1]==questioner,2] <- as.numeric(authors[authors[,1]==questioner,2])+1
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
