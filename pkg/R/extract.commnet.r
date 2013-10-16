extract.commnet <- 
function (forest, terms, apply.to = c("subjects", "content"), 
    list = c("rhelp", "rdevel")) 
{
    for (i in seq_along(terms)) {
        if (apply.to == "subjects") {
            net <- createedges(forest, subjectfilter = terms[i])
        }
        if (apply.to == "content") {
            net <- createedges(forest, contentfilter = terms[i])
        }
        if (dim(net)[1] > 0) {
            if (length(grep(terms[i], pattern = "\001|\002|\003|\004|\005|\030|\bb|\037|\a|\021")) == 
                0) {
                net <- adjacency(net)
                save(net, file = file.path(list, apply.to, paste("net_", 
                  terms[i], ".rda", sep = "")))
            }
        }
    }
}