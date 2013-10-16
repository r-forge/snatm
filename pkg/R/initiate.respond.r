## This file is part of snatm. snatm is free software: you can redistribute it
## and/or modify it under the terms of the GNU General Public License as
## published by the Free Software Foundation, either version 2 of the License,
## or (at your option) any later version.
##
## snatm is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Foobar.  If not, see <http://www.gnu.org/licenses/>.
##
## Copyright 2011 by Angela Bohn <angela.bohn@gmail.com>
## Copyright 2013 by Siemens AG, Wolfgang Mauerer <wolfgang.mauerer@siemens.com>
## All Rights Reserved.

# Message initiation vs. response calculations

# Arguments: forest_corrected
# Value: matrix with dimension n x 3 where n is the number of authors in forest
# firest column contains author name
# second column contains number of initiations
 #third column contains number of responses

initiate.respond <- function(forest){
  ir <- data.frame(name=unique(na.omit(unique(forest[,"author"]))),
                   initiations=0, responses=0, responses.received=0)
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

      idx <- which(ir$name==initiator)
      if (length(idx) != 1) {
        stop("Internal error: Thread initiator not found in author list!")
      }

      ir$initiations[idx] <- as.numeric(ir$initiations[idx]) + 1

      ## Note how many responses the thread intiator received
      ## in responses.received
      if (!is.null(answerers)) {
        ir$responses.received[idx] <- ir$responses.received[idx] + length(answerers)
      }
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
