### Copyright (C) Angela Bohn

normalize <- function(x){
  x <- (x-min(x))/(max(x)-min(x))
  x
}