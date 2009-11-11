library(sna)
gplot.snatm <-
function (dat, g = 1, gmode = "digraph", diag = FALSE, label = c(1:dim(dat)[2]),
    coord = NULL, jitter = TRUE, thresh = 0, usearrows = TRUE,
    mode = "fruchtermanreingold", displayisolates = TRUE, interactive = FALSE,
    xlab = NULL, ylab = NULL, xlim = NULL, ylim = NULL, pad = 0.2,
    label.pad = 0.5, displaylabels = !missing(label), boxed.labels = TRUE,
    label.pos = 0, label.bg = "white", vertex.sides = 8, vertex.rot = 0,
    arrowhead.cex = 1, label.cex = 1, loop.cex = 1, vertex.cex = 1,
    edge.col = 1, label.col = 1, vertex.col = 2, label.border = 1,
    vertex.border = 1, edge.lty = 1, label.lty = NULL, vertex.lty = 1,
    edge.lwd = 0, label.lwd = par("lwd"), edge.len = 0.5, edge.curve = 0.1,
    edge.steps = 50, loop.steps = 20, object.scale = 0.01, uselen = FALSE,
    usecurve = FALSE, suppress.axes = TRUE, vertices.last = TRUE,
    new = TRUE, layout.par = NULL, ...)
{
    bellstate <- options()$locatorBell
    expstate <- options()$expression
    on.exit(options(locatorBell = bellstate, expression = expstate))
    options(locatorBell = FALSE, expression = Inf)
    "%iin%" <- function(x, int) (x >= int[1]) & (x <= int[2])
    dat <- as.sociomatrix.sna(dat)
    if (is.list(dat))
        dat <- dat[[g]]
    else if (length(dim(dat)) > 2)
        d <- dat[g, , ]
    else d <- dat
    if (gmode == "graph") {
        usearrows <- FALSE
        n <- dim(d)[1]
    }
    else if (gmode == "twomode") {
        n <- sum(dim(d))
        temp <- matrix(0, nrow = n, ncol = n)
        temp[1:dim(d)[1], (dim(d)[1] + 1):n] <- d
        d <- temp
        if (all(label == 1:dim(dat)[2]))
            label <- 1:n
    }
    else n <- dim(d)[1]
    d[is.na(d)] <- 0
    d.raw <- d
    d <- matrix(as.numeric(d > thresh), n, n)
    if (!is.null(coord)) {
        x <- coord[, 1]
        y <- coord[, 2]
    }
    else {
        layout.fun <- try(match.fun(paste("gplot.layout.", mode,
            sep = "")), silent = TRUE)
        if (class(layout.fun) == "try-error")
            stop("Error in gplot: no layout function for mode ",
                mode)
        temp <- layout.fun(d, layout.par)
        x <- temp[, 1]
        y <- temp[, 2]
    }
    if (jitter) {
        x <- jitter(x)
        y <- jitter(y)
    }
    use <- displayisolates | (!is.isolate(d, ego = 1:dim(d)[1]))
    if (is.null(xlab))
        xlab = ""
    if (is.null(ylab))
        ylab = ""
    if (is.null(xlim))
        xlim <- c(min(x[use]) - pad, max(x[use]) + pad)
    if (is.null(ylim))
        ylim <- c(min(y[use]) - pad, max(y[use]) + pad)
    xrng <- diff(xlim)
    yrng <- diff(ylim)
    xctr <- (xlim[2] + xlim[1])/2
    yctr <- (ylim[2] + ylim[1])/2
    if (xrng < yrng)
        xlim <- c(xctr - yrng/2, xctr + yrng/2)
    else ylim <- c(yctr - xrng/2, yctr + xrng/2)
    baserad <- min(diff(xlim), diff(ylim)) * object.scale
    if (new) {
        plot(0, 0, xlim = xlim, ylim = ylim, type = "n", xlab = xlab,
            ylab = ylab, asp = 1, axes = !suppress.axes, ...)
    }
    vertex.cex <- rep(vertex.cex, length = n)
    vertex.radius <- rep(baserad * vertex.cex, length = n)
    vertex.sides <- rep(vertex.sides, length = n)
    vertex.border <- rep(vertex.border, length = n)
    vertex.col <- rep(vertex.col, length = n)
    vertex.lty <- rep(vertex.lty, length = n)
    vertex.rot <- rep(vertex.rot, length = n)
    loop.cex <- rep(loop.cex, length = n)
    if (!vertices.last)
        gplot.vertex(x[use], y[use], radius = vertex.radius[use],
            sides = vertex.sides[use], col = vertex.col[use],
            border = vertex.border[use], lty = vertex.lty[use],
            rot = vertex.rot[use])
    px0 <- vector()
    py0 <- vector()
    px1 <- vector()
    py1 <- vector()
    e.lwd <- vector()
    e.curv <- vector()
    e.type <- vector()
    e.col <- vector()
    e.hoff <- vector()
    e.toff <- vector()
    e.diag <- vector()
    e.rad <- vector()
    if (!is.array(edge.col))
        edge.col <- array(edge.col, dim = dim(d))
    if (!is.array(edge.lty))
        edge.lty <- array(edge.lty, dim = dim(d))
    dist <- as.matrix(dist(cbind(x, y)))
    tl <- d.raw * dist
    tl.max <- max(tl)
    for (i in (1:n)[use]) for (j in (1:n)[use]) if (d[i, j]) {
        px0 <- c(px0, as.real(x[i]))
        py0 <- c(py0, as.real(y[i]))
        px1 <- c(px1, as.real(x[j]))
        py1 <- c(py1, as.real(y[j]))
        e.toff <- c(e.toff, vertex.radius[i])
        e.hoff <- c(e.hoff, vertex.radius[j])
        e.col <- c(e.col, edge.col[i, j])
        e.type <- c(e.type, edge.lty[i, j])
        if (!is.array(edge.lwd)) {
            if (edge.lwd > 0)
                e.lwd <- c(e.lwd, edge.lwd * d.raw[i, j])
            else e.lwd <- c(e.lwd, 1)
        }
        else e.lwd <- c(e.lwd, edge.lwd[i, j])
        e.diag <- c(e.diag, i == j)
        e.rad <- c(e.rad, vertex.radius[i] * loop.cex[i])
        if (uselen) {
            if (tl[i, j] > 0) {
                e.len <- dist[i, j] * tl.max/tl[i, j]
                e.curv <- c(e.curv, edge.len * sqrt((e.len/2)^2 -
                  (dist[i, j]/2)^2))
            }
            else {
                e.curv <- c(e.curv, 0)
            }
        }
        else {
            if (!is.array(edge.curve)) {
                if (!is.null(edge.curve))
                  e.curv <- c(e.curv, edge.curve * d.raw[i, j])
                else e.curv <- c(e.curv, 0)
            }
            else {
                e.curv <- c(e.curv, edge.curve[i, j])
            }
        }
    }
    if (diag && (length(px0) > 0) && sum(e.diag > 0)) {
        gplot.loop(as.vector(px0)[e.diag], as.vector(py0)[e.diag],
            length = 1.5 * baserad * arrowhead.cex, angle = 25,
            width = e.lwd[e.diag] * baserad/10, col = e.col[e.diag],
            border = e.col[e.diag], lty = e.type[e.diag], offset = e.hoff[e.diag],
            edge.steps = loop.steps, radius = e.rad[e.diag],
            arrowhead = usearrows, xctr = mean(x[use]), yctr = mean(y[use]))
    }
    if (length(px0) > 0) {
        px0 <- px0[!e.diag]
        py0 <- py0[!e.diag]
        px1 <- px1[!e.diag]
        py1 <- py1[!e.diag]
        e.curv <- e.curv[!e.diag]
        e.lwd <- e.lwd[!e.diag]
        e.type <- e.type[!e.diag]
        e.col <- e.col[!e.diag]
        e.hoff <- e.hoff[!e.diag]
        e.toff <- e.toff[!e.diag]
        e.rad <- e.rad[!e.diag]
    }
    if (!usecurve & !uselen) {
        if (length(px0) > 0)
            gplot.arrow(as.vector(px0), as.vector(py0), as.vector(px1),
                as.vector(py1), length = 2 * baserad * arrowhead.cex,
                angle = 20, col = e.col, border = e.col, lty = e.type,
                width = e.lwd * baserad/10, offset.head = e.hoff,
                offset.tail = e.toff, arrowhead = usearrows)
    }
    else {
        if (length(px0) > 0) {
            gplot.arrow(as.vector(px0), as.vector(py0), as.vector(px1),
                as.vector(py1), length = 2 * baserad * arrowhead.cex,
                angle = 20, col = e.col, border = e.col, lty = e.type,
                width = e.lwd * baserad/10, offset.head = e.hoff,
                offset.tail = e.toff, arrowhead = usearrows,
                curve = e.curv, edge.steps = edge.steps)
        }
    }
    if (vertices.last)
        gplot.vertex(x[use], y[use], radius = vertex.radius[use],
            sides = vertex.sides[use], col = vertex.col[use],
            border = vertex.border[use], lty = vertex.lty[use],
            rot = vertex.rot[use])
    if (displaylabels & (!all(label == "")) & (!all(use == FALSE))) {
        if (label.pos == 0) {
            xoff <- x[use] - mean(x[use])
            yoff <- y[use] - mean(y[use])
            roff <- sqrt(xoff^2 + yoff^2)
            xhat <- xoff/roff
            yhat <- yoff/roff
        }
        else if (label.pos < 5) {
            xhat <- switch(label.pos, 0, -1, 0, 1)
            yhat <- switch(label.pos, -1, 0, 1, 0)
        }
        else {
            xhat <- 0
            yhat <- 0
        }
        os <- par()$cxy * label.cex
        lw <- strwidth(label[use], cex = label.cex)/2
        lh <- strheight(label[use], cex = label.cex)/2
        if (boxed.labels) {
            rect(x[use] - lw * (1 + label.pad) + xhat * (lw *
                (1 + label.pad + 0.2) + vertex.radius[use]),
                y[use] - lh * (1 + label.pad) + yhat * (lh *
                  (1 + label.pad + 0.2) + vertex.radius[use]),
                x[use] + lw * (1 + label.pad) + xhat * (lw *
                  (1 + label.pad + 0.2) + vertex.radius[use]),
                y[use] + lh * (1 + label.pad) + yhat * (lh *
                  (1 + label.pad + 0.2) + vertex.radius[use]),
                col = label.bg, border = label.border, lty = label.lty,
                lwd = label.lwd)
        }
        text(x[use] + xhat * (lw * (1 + label.pad + 0.2) + vertex.radius[use]),
            y[use] + yhat * (lh * (1 + label.pad + 0.2) + vertex.radius[use]),
            label[use], cex = label.cex, col = label.col, offset = 0)
    }
    if (interactive && ((length(x) > 0) && (!all(use == FALSE)))) {
        os <- c(0.2, 0.4) * par()$cxy
        textloc <- c(min(x[use]) - pad, max(y[use]) + pad)
        tm <- "Select a vertex to move, or click \"Finished\" to end."
        tmh <- strheight(tm)
        tmw <- strwidth(tm)
        text(textloc[1], textloc[2], tm, adj = c(0, 0.5))
        fm <- "Finished"
        finx <- c(textloc[1], textloc[1] + strwidth(fm))
        finy <- c(textloc[2] - 3 * tmh - strheight(fm)/2, textloc[2] -
            3 * tmh + strheight(fm)/2)
        finbx <- finx + c(-os[1], os[1])
        finby <- finy + c(-os[2], os[2])
        rect(finbx[1], finby[1], finbx[2], finby[2], col = "white")
        text(finx[1], mean(finy), fm, adj = c(0, 0.5))
        clickpos <- unlist(locator(1))
        if ((clickpos[1] %iin% finbx) && (clickpos[2] %iin% finby)) {
            cl <- match.call()
            cl$interactive <- FALSE
            cl$coord <- cbind(x, y)
            cl$dat <- dat
            return(eval(cl))
        }
        else {
            clickdis <- sqrt((clickpos[1] - x[use])^2 + (clickpos[2] -
                y[use])^2)
            selvert <- match(min(clickdis), clickdis)
            if (all(label == ""))
                label <- 1:n
            rect(textloc[1], textloc[2] - tmh/2, textloc[1] +
                tmw, textloc[2] + tmh/2, border = "white", col = "white")
            tm <- "Where should I move this vertex?"
            tmh <- strheight(tm)
            tmw <- strwidth(tm)
            text(textloc[1], textloc[2], tm, adj = c(0, 0.5))
            fm <- paste("Vertex", label[use][selvert], "selected")
            finx <- c(textloc[1], textloc[1] + strwidth(fm))
            finy <- c(textloc[2] - 3 * tmh - strheight(fm)/2,
                textloc[2] - 3 * tmh + strheight(fm)/2)
            finbx <- finx + c(-os[1], os[1])
            finby <- finy + c(-os[2], os[2])
            rect(finbx[1], finby[1], finbx[2], finby[2], col = "white")
            text(finx[1], mean(finy), fm, adj = c(0, 0.5))
            clickpos <- unlist(locator(1))
            x[use][selvert] <- clickpos[1]
            y[use][selvert] <- clickpos[2]
            cl <- match.call()
            cl$coord <- cbind(x, y)
            cl$dat <- dat
            return(eval(cl))
        }
    }
    invisible(cbind(x, y))
}