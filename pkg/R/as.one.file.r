as.one.file <- function(files,filename="allthreads.txt",list=c("rdevel","rhelp")){
  outFiles <- file(filename, "w")
  for (i in files){
    x <- readLines(paste(list,"/",i,sep=""))
    writeLines(x,outFiles)
  }
  close(outFiles)
  print("Done")
}