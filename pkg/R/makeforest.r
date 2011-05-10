makeforest <- 
function (month)
{
    tm.plugin.mail::convert_mbox_eml(paste(month, ".txt", sep = ""),
        paste(month, "/", sep = ""))
    workingobject <- tm::Corpus(DirSource(month), readerControl = list(reader = readMail(DateFormat = "%a, %d %b %Y %H:%M:%S")))
    workingobject <- sapply(workingobject, tm.plugin.mail::removeCitation)
    workingobject <- sapply(workingobject, tm.plugin.mail::removeSignature)
    threadid <- threads(workingobject)$ThreadID
    workingobject <- workingobject[!is.na(threadid)]
    Content <- sapply(sapply(workingobject, "Content"), paste,
        collapse = "\n")
    numberofmail <- 1:length(workingobject)
    authorlist <- sapply(workingobject, "Author")
    authors <- c()
    for (i in seq_along(authorlist)) {
        authors <- c(authors, authorlist[[i]][1])
    }
    headinglist <- sapply(workingobject, "Heading")
    headings <- c()
    for (i in 1:length(headinglist)) {
        headings <- c(headings, headinglist[[i]][1])
    }
    forest <- cbind(numberofmail, threadid[!is.na(threadid)],
        authors, headings, Content[!is.na(threadid)])
    Encoding(forest[,3:5]) <- "UTF-8"
    colnames(forest) <- c("emailID", "threadID", "author", "subjects",
        "content")
    forest
}