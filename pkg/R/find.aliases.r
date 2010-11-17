normalizeauthors <- function(authors){
  # discard bounces
  a <- gsub(x=authors,pattern="[[:print:]]*r-help-bounces[[:print:]]*",replacement="bounce")
  a <- gsub(x=a,pattern="[[:print:]]*r-project.org[[:print:]]*",replacement="bounce")
  a <- gsub(x=a,pattern="[[:print:]]*Mail Delivery System[[:print:]]*",replacement="bounce")
  a <- gsub(x=a,pattern="[[:print:]]*[Bb]ehalf[[:print:]]*",replacement="bounce")
  # discard e-mail domain and parentheses
  a <- gsub(pattern=" at [[:print:]]*\\.[[:print:]]{2,3} \\(",x=authors,replacement="|")
  a <- gsub(pattern="\\)$",x=a,replacement="")
  # discard ISO-names
  a <- gsub(pattern="\\|\\=\\?[[:print:]]*\\?\\=",x=a,replacement="|")
  # if there are still email addresses: get user names
  a <- gsub(pattern=" \\[mailto:",x=a,replacement="|")
  a <- gsub(pattern=" at [[:print:]]*>?|\\]?",x=a,replacement="")
  a <- gsub(pattern=" <",x=a,replacement="|")
  a <- gsub(pattern=" \\[",x=a,replacement="|")
  # discard company name or location in parentheses
  a <- gsub(pattern=" \\([[:print:]]*\\)?$",x=a,replacement="")
  a <- gsub(pattern=" \\([[:print:]]*\\)?\\|",x=a,replacement="|")
  a <- gsub(pattern=" \\{[[:print:]]*\\}?$",x=a,replacement="")
  a <- gsub(pattern=" \\{[[:print:]]*\\}?\\|",x=a,replacement="")
  # discard titles
  a <- gsub(pattern=" Jr\\.?",x=a,replacement="")
  a <- gsub(pattern=",? ?\\(?Prof(essor)?\\.?\\)? ?",x=a,replacement="")
  a <- gsub(pattern="Dr ",x=a,replacement="")
  a <- gsub(pattern="Dr\\.",x=a,replacement="")
  a <- gsub(pattern=",? ?\\(?Dr\\.?\\)? ",x=a,replacement="")
  a <- gsub(pattern=",? ?\\(?Dr\\.?\\)?$",x=a,replacement="")
  a <- gsub(pattern=",? ?Dr\\.",x=a,replacement="")
  a <- gsub(pattern=",? ?\\(?Mag\\.\\)? ?",x=a,replacement="")
  a <- gsub(pattern=",? ?\\(?Ph\\.?D\\.?\\)?",x=a,replacement="")
  # discard numbers (some user names consist only of numbers, therefore delete max. 2 digits)
  a <- sub(pattern="[[:digit:]]{1,2}",x=a,replacement="")
  # discard punctuation in \"Name\"
  a <- gsub(pattern='"',x=a,replacement="")
  # discard middle name
  a <- gsub(pattern=" [[:upper:]]\\. ",x=a,replacement=" ")
  # discard e-mail domain
  a <- gsub(pattern="@[[:print:]]*",x=a,replacement="")
  # discard Ltd.
  a <- gsub(pattern="Ltd\\.?",x=a,replacement="")
  a
}

sortnames <- function(x){
  x1 <- x
  pattern <- grep(x,pattern="[[:print:]]*, ?[[:print:]]*")
  if (is.element(1,pattern)){
    last <- gsub(x,pattern=", ?[[:print:]]*",replacement="")
    last <- gsub(last,pattern="[[:print:]]*\\|",replacement="")
    first <- gsub(x,pattern="[[:print:]]*, ?",replacement="")
    first <- gsub(first,pattern="\\|[[:print:]]*",replacement="")
    mail <- gsub(x,pattern=paste(last,","," ?",first,"\\|",sep=""),replacement="")
    mail <- gsub(mail,pattern=paste("\\|",last,","," ?",first,sep=""),replacement="")
    x1 <- paste(mail,"|",first," ",last,sep="")
  }
  x1
}

emailfirst <- function (y)
{
    if (length(grep(y, pattern = " ")) > 0) {
        email <- gsub(y, pattern = "[[:print:]]* [[:print:]]*\\||\\|[[:print:]]* [[:print:]]*",
            replacement = "")
        email <- gsub(email, pattern = "\\|", replacement = "")
        if (email != y) {
            realname <- gsub(y, pattern = paste(email, "|",sep=""),replacement="",fixed=TRUE)
            realname <- gsub(realname,pattern=paste("|",email, sep = ""), replacement = "",fixed=TRUE)
            final <- paste(email, realname, sep = "|")
            final <- gsub(final, pattern = "\\|$", replacement = "")
        }
        if (email == y) {
            final <- y
        }
    }
    if (length(grep(y, pattern = " ")) == 0) {
        final <- y
    }
    final
}

findclusters <- function(v                                                # v=unique(c)
                        ,distance=0.3
                        ,not.take.memory=NULL){                                 # file containing aliases to be rejected
  clusters <- list()
  len <- c()
  for (i in seq_along(v)) {
    mat <- agrep(pattern=v[i],x=v,value=TRUE,ignore.case=TRUE,max.distance=distance)
    if (length(mat)>1){                                                         # omit empty clusters
      if (!identical(mat[1]==mat,rep(TRUE,length(mat)))){                       # omit clusters in which all elements are identical
        clusters <- c(clusters,list(c(v[i],unique(mat[mat!=v[i]]))))
      }
    }
  }
  sortedclusters <- lapply(clusters,sort)
  clusters <- clusters[!duplicated(base::tolower(sortedclusters))]
  clusters <- clusters[!unlist(lapply(lapply(clusters,is.na),any))]
  if (length(clusters)>0&length(not.take.memory)>0){
    for (i in seq_along(clusters)){
      for (j in seq_along(not.take.memory)){
        if (any(clusters[[i]][1]==not.take.memory[[j]][1])){
          if (clusters[[i]][1]==not.take.memory[[j]][1]){
            newname <- clusters[[i]][1]
            take.altnames <- clusters[[i]][2:length(clusters[[i]])]
            not.take.altnames <- not.take.memory[[j]][2:length(not.take.memory[[j]])]
            if (max(is.element(not.take.altnames,take.altnames)==1)){
              clusters[[i]]<- c(newname,take.altnames[!is.element(take.altnames,not.take.altnames)])
            }
          }
        }
      }
    }
    len <- unlist(lapply(clusters,length))
    clusters <- clusters[len>1]
  }
  clusters
}

changenames <- function(clusters,forest,accept){
  if (length(accept)>0){
    clusters <- clusters[accept]
  }
  for (i in 1:length(clusters)){
    containsspace <- grep(clusters[[i]],pattern="[[:space:]]")
    altnames <- clusters[[i]]
    if (length(containsspace)>0){
      newname <- clusters[[i]][containsspace][1]
    }
    if (length(containsspace)==0){
      newname <- clusters[[i]][1]
    }
    if (max(is.element(forest,altnames))==1){
      forest[is.element(forest,altnames)] <- newname
    }
  }
  forest
}

final <- function(d){
  # Extract Author Name (discard e-mail address)
  e <- gsub(d,pattern="\\|$",replacement="")
  e <- gsub(e,pattern="[[:print:]][^[:space:]]*\\|",replacement="")
  # remove punctuation and unnecessary whitespaces
  e <- gsub(pattern="[[:punct:]] ",x=e,replacement=" ")
  e <- gsub(pattern=" [[:punct:]]",x=e,replacement=" ")
  e <- gsub(pattern="[[:punct:]]",x=e,replacement=" ")
  e <- gsub(pattern="^ \\<",x=e,replacement="")
  e <- gsub(pattern="[[:blank:]]{2,}",x=e,replacement=" ")
  e
}
