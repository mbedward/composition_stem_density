library(dplyr)
library(ggplot2)


# Function to load object form RData file under
# a given name
load_from <- function(.path) {
  load(.path)
  objs <- ls(all.names = FALSE)
  get(objs[1], inherits = FALSE)
}


pagesize <- function(size = c("A4", "A3", "A2", "A1", "A0"), 
                     orientation = c("portrait", "landscape"),
                     units = c("cm", "mm")) {
  
  size <- match.arg(size)
  orientation <- match.arg(orientation)
  units <- match.arg(units)
  
  alpha <- 1000 * 2^(1/4)
  i <- as.integer(substr(size, 2, 2))
  long <- alpha * 2^(-i/2)
  
  page <- switch(
    orientation,
    portrait = c(width = long / sqrt(2), height = long),
    landscape = c(width = long, height = long / sqrt(2))
  )
  
  page <- round(page)
  if (units == "cm") page <- page / 10
  
  page <- c(as.list(page), units = units)
  class(page) <- "pagesize"
  
  page
}



# Function to save graphs to file
gg_pdf <- function(plot, filename, size = pagesize("A4", "landscape", "cm")) {
  
  if (!inherits(size, "pagesize")) stop("The size argument should be a pagesize (list) object")
  
  ggsave(
    filename, 
    plot, 
    width = size$width,
    height = size$height,
    units = size$units)
}


#################################################
# ggplot stat to draw convex hulls

StatCHull <- ggproto(
  "StatCHull", Stat,
  compute_group = function(data, scales) {
    ivertex <- chull(data$x, data$y)
    data[ivertex, , drop = FALSE]
  },
  
  required_aes = c("x", "y")
)

stat_chull <- function(mapping = NULL, data = NULL, geom = "polygon",
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, ...) {
  layer(
    stat = StatCHull,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


# Function to calculate highest posterior density interval
# (adapted from coda::HPDinterval)
hpdi <- function (x, prob = 0.95) 
{
  x <- as.matrix(x)
  Nr <- nrow(x)
  if (Nr <= 1) stop("x must have more than 1 vector element or matrix row")
  x <- apply(x, 2, sort)
  
  Nc <- ncol(x)
  gap <- max(1, min(Nr - 1, round(Nr * prob)))
  init <- 1:(Nr - gap)
  
  inds <- apply(x[init + gap, , drop = FALSE] - x[init, , drop = FALSE], 
                MARGIN = 2, 
                which.min)
  
  out <- cbind(x[cbind(inds, 1:Nc)], x[cbind(inds + gap, 1:Nc)])
  dimnames(out) <- list(colnames(x), c("lower", "upper"))
  
  out
}

