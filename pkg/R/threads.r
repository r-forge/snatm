threads <- function(x) {
    # Hash table storing (thread ID, thread level) for a given message ID
    ht <- new.env()
    tid <- 1
    threadIDs <- threadLevels <- integer(length(x))
    for (i in seq_along(x)) {
        messageID <- tm::ID(x[[i]])[1]
        parentID <- gsub("In-Reply-To: ", "", grep("^In-Reply-To:", attr(x[[i]], "Header"), value = TRUE))
        # Generate new thread
        if (!length(parentID)) {
            ht[[messageID]] <- c(tid, 1)
            threadIDs[i] <- tid
            threadLevels[i] <- 1
            tid <- tid + 1
        }
        # Use existing thread
        else {
            threadID <- if (!is.numeric(ht[[parentID]][1])) NA else as.integer(ht[[parentID]][1])
            threadLevel <- if (!is.numeric(ht[[parentID]][2])) 2 else as.integer(ht[[parentID]][2] + 1)
            ht[[messageID]] <- c(threadID, threadLevel)
            threadIDs[i] <- threadID
            threadLevels[i] <- threadLevel
        }
    }
    list(ThreadID = threadIDs, ThreadDepth = threadLevels)
}
