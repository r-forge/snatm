edgelist <- function (adjmatrix) 
{
    if (!is.matrix(adjmatrix)) {
        stop("Input must be an adjacency matrix (square).")
    }
    if (dim(adjmatrix)[1] != dim(adjmatrix)[2]) {
        stop("Input matrix must be an adjacency matrix (square).")
    }
    elist <- c()
    for (i in seq_along(adjmatrix[, 1])) {
        for (j in seq_along(adjmatrix[, 1])) {
            if (adjmatrix[i, j] > 0) {
                if (sum(unique(as.vector(adjmatrix))) != 1) {
                  elist <- rbind(elist, c(rownames(adjmatrix)[i], 
                    colnames(adjmatrix)[j], adjmatrix[i, j]))
                }
                if (sum(unique(as.vector(adjmatrix))) == 1) {
                  elist <- rbind(elist, c(rownames(adjmatrix)[i], 
                    colnames(adjmatrix)[j]))
                }
            }
        }
    }
    elist
}