library("sna")
centrality.edgelist <- 
function (terms, apply.to, data.path, max.terms=20)
{
    if (!(apply.to %in% c("subject", "content")))
      stop("apply.to must be either 'subject' or 'content'")
    edgelist <- c()
    keywords <- new.env()
    sizes <- rep(0, length(terms))

    for (i in seq_along(terms)) {
      tmp <- unlist(strsplit(file.path(data.path, "commnet.terms", apply.to,
                                       paste("net_", terms[i], ".rda", sep = "")),
                             split = "/"))
      tmp <- tmp[length(tmp)]
      filelist <- list.files(file.path(data.path, "commnet.terms", apply.to))

      if (is.element(tmp, filelist)) {
        load(file.path(data.path, "commnet.terms", apply.to,
                       paste("net_", terms[i], ".rda", sep = "")))

        net <- sna::component.largest(net, result = "graph",
                                      connected = "weak")
        if (!is.null(dim(net))) {
          sizes[i] <- dim(net)[1]
        }

        rm(net)
      }
    }

    if (max.terms > length(sizes))
      max.terms <- length(sizes)

    for (i in order(sizes, decreasing=T)[1:max.terms]) {
      if (sizes[i] == 0)
        next

      load(file.path(data.path, "commnet.terms", apply.to,
                     paste("net_", terms[i], ".rda", sep = "")))
      net <- sna::component.largest(net, result = "graph",
                                    connected = "weak")
      authors <- rownames(net)
      value <- sna::degree(net, cmode = "outdegree")
      value <- cbind(authors, value)
      value <- ordermatrix(value, 2)
      value <- cbind(value[, 1], seq(1:dim(value)[1])/dim(value)[1])
      edgelist <- rbind(edgelist, cbind(value[, 1], terms[i], value[, 2]))
    }

    return (list(edgelist, sizes))
}
