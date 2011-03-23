library("sna")
centrality.edgelist <-
function (terms, apply.to = c("subjects", "content"), list = c("rhelp",
    "rdevel"))
{
    edgelist <- c()
    for (i in seq_along(terms)) {
       if (is.element(unlist(strsplit(file.path(list, apply.to, paste("net_",terms[i], ".rda", sep = "")),split="/"))[3]
        ,list.files(paste(list,"/", apply.to, sep = "")))) {
            load(file.path(list, apply.to, paste("net_", terms[i],
                ".rda", sep = "")))
            if (dim(net)[1] > 19) {
                net <- sna::component.largest(net, result = "graph",
                  connected = "weak")
                if (dim(net)[1] > 19) {
                  authors <- rownames(net)
                  value <- sna::degree(net, cmode = "outdegree")
                  value <- cbind(authors, value)
                  value <- ordermatrix(value, 2)
                  value <- cbind(value[, 1], seq(1:dim(value)[1])/dim(value)[1])
                  edgelist <- rbind(edgelist, cbind(value[, 1],
                    terms[i], value[, 2]))
                }
            }
        }
    }
    edgelist
}