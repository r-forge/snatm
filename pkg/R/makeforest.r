gen.corpus <- function (ml, repo.path="./", suffix=".txt", outdir=NULL,
                        marks=character(0), encoding="UTF-8", preprocess=NULL,
                        postprocess=NULL)
{
  ml.base <- file.path(repo.path, ml)
  ## Skip the conversion step if the result directory already
  ## exists. TODO: We need some more intelligent algorithm to deal
  ## with incremental updates.
  if (!file.exists(ml.base)) {
    tm.plugin.mail::convert_mbox_eml(paste(ml.base, suffix, sep = ""),
                                     paste(ml.base, "/", sep = ""))
  }
  corp <- tm::Corpus(DirSource(ml.base, encoding=encoding),
                     readerControl = list(reader=readMail(DateFormat = "%a, %d %b %Y %H:%M:%S")))

  if (!is.null(preprocess)) {
    corp <- preprocess(corp)
  }
  corp.orig <- corp

  corp <- tm_map(corp, function(x) iconv(enc2utf8(x), sub="byte"))
  corp <- tm_map(corp, tm.plugin.mail::removeCitation, removeQuoteHeader=T)
  corp <- tm_map(corp, tm.plugin.mail::removeSignature, marks=marks)
  corp <- tm_map(corp, tm.plugin.mail::removeMultipart)
  ## NOTE: It's important to apply tolower before stopword removal;
  ## otherwise, phrases like "I'm" won't be removed properly
  corp <- tm_map(corp, tolower)
  corp <- tm_map(corp, tm::removeWords, stopwords("english"))
  corp <- tm_map(corp, tm::removeNumbers)
  corp <- tm_map(corp, tm::removePunctuation)
  corp <- tm_map(corp, tm::stripWhitespace)
  corp <- tm_map(corp, tm::stemDocument)

  ## NOTE: stemCompletion should only be done for the terms that are
  ## actually displayed to a user -- there are only a few ones, and they will
  ## be very fast to unstem.
  ##  corp <- tm_map(corp, tm::stemCompletion, dictionary=corp.orig)

  if (!is.null(postprocess)) {
    corp <- postprocess(corp)
  }

  return(list(corp=corp, corp.orig=corp.orig))
}


make.forest <- function(corp, encoding="UTF-8") {
  thread.ids <- threads(corp)$ThreadID

  ## If there are threads without IDs, something is wrong with the
  ## thread detection. Bail out in this case.
  if (sum(is.na(thread.ids)))
    stop("NAs in thread list")

  Content <- sapply(sapply(corp, "Content"), paste,
                    collapse = "\n")

  ## Provide consecutive identifiers for each message
  mail.ids <- 1:length(corp)

  ## Extract authors and headings from the corpus and build simple
  ## character vectors
  authors <- sapply(corp, function (x) { Author(x)[1] } )
  attributes(authors) <- NULL

  headings <- sapply(corp, function (x) { Heading(x)[1] } )
  attributes(headings) <- NULL

  ## The forst collects unique (numeric) identifiers for every mail
  ## and the thread it is associated with, together with author, subject line
  ## and content of each mail
  forest <- cbind(mail.ids, thread.ids, authors,
                  headings, Content)

  colnames(forest) <- c("emailID", "threadID", "author", "subject",
                        "content")
  Encoding(forest[,c("author", "subject", "content")]) <- encoding

  forest[,"author"] <- do.normalise(forest[,"author"])

  return(forest)
}
