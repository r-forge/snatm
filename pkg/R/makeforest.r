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

## Modified version of tm::removeWords that sets the useBytes=TRUE flag
## in gsub and therefore avoids issues with invalid (but correctly encoded)
## UTF-8 strings like rawToChar(as.raw(c(0xf5, 0x93, 0xb3, 0x84)))
removeWords.useBytes <- function(x, words) {
  gsub(sprintf("(*UCP)\\b(%s)\\b", paste(words, collapse = "|")), "", x,
       perl = TRUE, useBytes=TRUE)
}

gen.corpus <- function (ml, repo.path="./", suffix=".txt", outdir=NULL,
                        marks=character(0), encoding="UTF-8", preprocess=NULL,
                        postprocess=NULL)
{
  ml.base <- file.path(repo.path, ml)
  ## Skip the conversion step if the result directory already
  ## exists. TODO: We need some more intelligent algorithm to deal
  ## with incremental updates.
  timestamp("Starting mbox conversion")
  if (!file.exists(ml.base)) {
    tm.plugin.mail::convert_mbox_eml(paste(ml.base, suffix, sep = ""),
                                     paste(ml.base, "/", sep = ""))
  }
  timestamp("mbox conversion finished, starting corpus generation")
  corp <- tm::Corpus(DirSource(ml.base, encoding=encoding),
                     readerControl = list(reader=readMail(DateFormat = "%a, %d %b %Y %H:%M:%S")))
  timestamp("corpus generation finished!")
  
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
  corp <- tm_map(corp, removeWords.useBytes, stopwords("english"))
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


make.forest <- function(corp, normalise.FUN=NULL, encoding="UTF-8") {
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

  ## The forest collects unique (numeric) identifiers for every mail
  ## and the thread it is associated with, together with author, subject line
  ## and content of each mail
  forest <- cbind(mail.ids, thread.ids, authors,
                  headings, Content)

  colnames(forest) <- c("emailID", "threadID", "author", "subject",
                        "content")
  Encoding(forest[,c("author", "subject", "content")]) <- encoding

  ## Normalisation may transform the author names in arbitrary ways,
  ## for instance by assigning numerical IDs to them. Preserve
  ## the original names in author.orig.
  if (!is.null(normalise.FUN)) {
    authors.normalised <- normalise.FUN(forest[,"author"])
    forest <- cbind(forest, author.orig=forest[,"author"])
    forest[,"author"] <- authors.normalised
  } else {
    forest <- cbind(forest, author.orig=forest[,"author"])
  }


  return(forest)
}
