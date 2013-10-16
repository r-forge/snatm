adjacency <- function (edgelist, directed = TRUE, valued = TRUE, mode = "multiple") 
{
    if (!is.data.frame(edgelist) & !is.matrix(edgelist)) {
        stop("Input data must be a matrix or dataframe with two columns.")
    }
    if (is.data.frame(edgelist)) {
        edgelist1 <- cbind(as.vector(edgelist[, 1]), as.vector(edgelist[, 
            2]))
        vnames <- unique(c(as.vector(edgelist1[, 1]), as.vector(edgelist[, 
            2])))
        vnumbers <- cbind(vnames, seq(1:length(vnames)))
    }
    if (!is.data.frame(edgelist)) {
        vnames <- unique(as.vector(edgelist[, 1:2]))
        vnumbers <- cbind(vnames, seq(1:length(vnames)))
    }
    dimension <- length(vnames)
    adjmatrix <- matrix(rep(0, dimension * dimension), nrow = dimension)
    rownames(adjmatrix) <- vnames
    colnames(adjmatrix) <- vnames
    for (i in 1:dim(edgelist)[1]) {
        aparty <- which(vnumbers[, 1] == edgelist[i, 1])
        bparty <- which(vnumbers[, 1] == edgelist[i, 2])
        if (mode == "multiple") {
            value <- 1
        }
        if (mode == "addvalues") {
            value <- as.numeric(edgelist[i, 3])
        }
        adjmatrix[aparty, bparty] <- adjmatrix[aparty, bparty] + 
            value
        if (directed == FALSE) {
            adjmatrix[bparty, aparty] <- adjmatrix[bparty, aparty] + 
                value
        }
    }
    if (valued == FALSE) {
        adjmatrix[adjmatrix > 1] <- 1
    }
    adjmatrix
}