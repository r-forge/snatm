extract.commnet <- function (forest, terms, apply.to, data.path) {
    if (!(apply.to %in% c("subject", "content")))
      stop("apply.to must be either 'subject' or 'content'")

    ## NOTE: We assume that the directory has been created by the caller.
    dir <- file.path(data.path, "commnet.terms", apply.to)

    for (i in seq_along(terms)) {
        if (apply.to == "subject") {
            net <- createedges(forest, subjectfilter = terms[i])
        }
        if (apply.to == "content") {
            net <- createedges(forest, contentfilter = terms[i])
        }
        if (is.na(net) || is.null(net))
          next

        if (dim(net)[1] > 0) {
            if (length(grep(terms[i], pattern = "\001|\002|\003|\004|\005|\030|\bb|\037|\a|\021")) == 
                0) {
                net <- adjacency(net)

                save(net, file = file.path(data.path, "commnet.terms",
                            apply.to, paste("net_", terms[i], ".rda", sep = "")))
            }
        }
    }
}
