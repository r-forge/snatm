as.one.file <- function(files,source=NULL,dest="allthreads.txt",list=c("devel","help")){
  outFiles <- file(dest, "w")
  for (i in files){
   # x <- readLines(paste(list,"/",i,sep=""))
    if (length(source)>0){
      x <- readLines(file.path(source,list,i))
    }
    if (length(source)==0){
      x <- readLines(file.path(list,i))
    }
    writeLines(x,outFiles)
  }
  close(outFiles)
}