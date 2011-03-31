library(tm)
prepare.text <- function(forest,terms.from=c("subjects","content"),list=c("devel","help"),protect=NULL,ae.to.be=TRUE,replace=TRUE,stem=TRUE){
  colnames(forest)[4:5] <- c("subjects","content")
  if (ae.to.be){
    forest[,terms.from] <- Simpleplus(forest[,terms.from])
  }
  text <- unlist(lapply(Corpus(VectorSource(forest[,terms.from])),FUN=removePunctuation))
  text <- unlist(lapply(Corpus(VectorSource(text)),removeNumbers))
  text <- gsub(text,pattern="([[:space:]]+)?\n([[:space:]]+)?",replacement=" \n ")
  text <- tolower(text)
  for (i in seq_along(text)){
    text.temp <- text[i]
    text.temp <- unlist(strsplit(text.temp,split="[[:space:]]+"))
    text.temp <- text.temp[nchar(text.temp)>2]
    if (length(text.temp)>0){
      text[i] <- paste(text.temp,collapse=" ")
    }
    if (length(text.temp)==0){
      text[i] <- ""
    }
  }
  text1 <- text
  text <- unlist(strsplit(text,split="[[:space:]]+"))
  if (replace){
    replacement <- wn.replace(unique(text))
    if (is.null(protect)){
      if (terms.from=="subjects"&list=="devel"){
        protect <- c(4,5,11,12,16,22,25,30,31,32:36)
      }
      if (terms.from=="content"&list=="devel"){
        #protect <- c(2,3,5,6,9,10,16,17,18,19,20,28,31,37,38,39,50,55,56,65,85,87,91,93,94,99,105,120,121,126,135,136,138,148,149,156,164,165,166,170,172,177,178,181,183,184,195,196,203,209,222,225,239,240,245,247,256,257,261,262,270:282,284,286:288,293,300,301,307,308,317:320,329,330,344,346,351,359,367,371,373,375,408,418,432,433,436,437,438,439,444,445,453:457,459,471,472,475,476,493,498:500)
        protect <- c(1:5,7,9,10,12,18,19,20,21,30,32,33,39,40,41,52,56,58,67,74,76,79,84,87,89,93,94,95,96,98,100,104,110,111,124,127,137,138,141,142,151,152,155,156,163,169,171,172,173,178,185,187,189,191,192,197,198,199,203,204,209,211,217,222,226,231,233,234,238,243,250,251,253,254,256,258,267,268,274,275:277,283:291,294:297,299,301:303,308,315,320,321:323,328,332,333,335,339,344,345,348,359,366,367,370,374,377,382,383,386:390,399,401,407,412,413,421,423,424,433,448,451,452:454,458:460,461,468:471,480,490,491,492,508,513,515,522)
      }
      if (terms.from=="subjects"&list=="help"){
        #protect <- c(2,5,12,15,20,21,23,26,32,33,36,38,41,45,47,48,51,56,58,63,65,68,69,72,76,79,80,81,85,89,90,93,94,96,99,100,102,104,105,112,114:119,123,124:126,144:153,156:159,166,168,169,173,183:184,187:188,190,191)
        protect <- c(2,8,12,13,15,16,20,21,22,24,27,28,33,34,37,38,39,42,44,47,49,50,53,56,58,60,65,68,72,75,79,82,83,84,85,91,93,96,99,100,105,106,108,110,111,119:125,128,129,130,132,139,149,151:155,157,158,161,173,187,188,190,191,194:197,200)
      }
      if (terms.from=="content"&list=="help"){
        #protect <- c(2,5,7,9,11,13,18,25,33,37,38,40,55,45,53,61,63,73,74,76,78,84,85,86,90,94,96,97,111,114,116,117,122,123,125,131,135,140,144,145,156,159,165,170,173,174,180,193,194,195,202,208,210,212,222,223,229,235,241,245,252,256,257,260,261,264,266,271,273,278,280,282,296,299,313,316,321,333,337,338,352,354,359,367,368,371,373,374,382,385,394,397,403,404,408,413,414,419,420,425,429,430,442,450,462,467,475,492,495,499,500,503,504,507,510,511,525,527,550,573:576,577,578,585,588:590,595,596,598,602:604,617,618:621,629,643,644,645,653,654,660,661,664,667,678,681,688,689,693,694,699,700:706,709,712,714,715,723:728,734,742,743:745,747,758,759,760,766:770,775,775,779,780,789,790,797,798,805,809:815,821:824,837:839,857,865,866,867,890,892,900,929,930:932,937,955,956,963,965:971,975,976,981,982,991,993:994,1004,1005,1008,1009,1013:1015,1016,1026,1029,1030,1035,1044,1059,1068,1076,1077,1098,1100,1106,1110,1119,1132,1141,1159,1158,1161,1170,1205,1208,1228,1229,1230,1237,1239,1242,1244,1245,1247:1250,1284)
        protect <- c(2,3,4,7,9,11,12,13,14,17,21,26,27,35,36,39,40,41,42,43,47,48,50,56,60,66,68,78,79,90,91,92,96,103,104,108,119,122,124,125,131,133,139,143,152,153,164,167,171,177,180,183,191,204,205,206,213,219,226,228,234,264,269,273,281,286,288,290,292,293,295,297,314,326,329,331,354,356,365,367,369,379,382,386,388,389,400,403,407,409,416,417,428,434,440,445,448,457,470,477,482,486,507,510,514,515,518,519,520,525,542,544,551,568,578,591:598,602,603,606:608,613,620:622,636:638,647,651:652,660,663:665,675,676,679,682,683,689,702,704,714,715,719,720,723,725:729,731,735,738,740:743,749:752,767,769,770,774,786,787,790,793:797,805,807,809,816,818,819,824,826,827,831,834,838:843,850,851,852,863,866:868,874,884,886:888,893,894,895,896,899,901,929,958:961,984,985,992,993,994:1000,1004,1005,1021,1022,1030,1031,1034,1038,1039,1043,1053,1054,1059,1061,1074,1089,1105,1106,1128,1142,1155,1164,1173,1174,1190,1191,1202,1237,1240,1260,1261,1262,1266,1269,1271,1274,1275:1277,1279,1281,1282,1283:1285,1294,1298,1299
          ,1310,1315)
      }
    }
    replacement[[2]] <- replacement[[2]][-protect,]
    replaced.terms <- unlist(strsplit(replacement[[2]][,1],split=" "))
    for (i in seq_along(text)){
      if (text[i] %in% replaced.terms){
        text[i] <- replacement[[2]][replacement[[2]][,1]==text[i],2]
      }
    }
  }
  if (stem){
    text.stem <- stemDocument(text)
    replacement.stem <- replacement[[2]]
    replacement.stem[,2] <- stemDocument(replacement.stem[,2])
    for (i in seq_along(replacement.stem[,1])){
      replacement.stem[i,1] <- paste(stemDocument(unlist(strsplit(replacement.stem[i,1],split="[[:space:]]+"))),collapse=" ")
    }
    replacement.stem <- replacement.stem[replacement.stem[,1]!=replacement.stem[,2],]
    for (i in seq_along(text.stem)){
      if (text.stem[i] %in% replacement.stem[,1]){
        text.stem[i] <- replacement.stem[grep(replacement.stem[,1],pattern=text.stem[i])[1],2]
      }
    }
    text <- text.stem
  }
  termfreq <- tm::Corpus(VectorSource(paste(text,collapse=" ")))
  if (terms.from=="subjects"){
    mindocfreq <- 10
  }
  if (terms.from=="content"){
    mindocfreq <- 20
  }
  termfreq <- termFreq(termfreq[[1]],control=list(minWordLength=3,stemming=FALSE,minDocFreq=mindocfreq,stopwords=TRUE))
  termfreq <- sort(termfreq,decreasing=TRUE)
  text <- text1
  for (i in seq_along(text)){
    if (replace){
      text.temp <- stemDocument(unlist(strsplit(text[i],split="[[:space:]]+")))
      for (j in seq_along(text.temp)){
        if (text.temp[j] %in% replacement.stem[,1]){
          text.temp[j] <- replacement.stem[replacement.stem[,1]==text.temp[j],2][1]
        }
      }
    }
    text[i] <- paste(text.temp,collapse=" ")
  }
  forest[,terms.from] <- text
  output <- list(termfreq,forest)
  names(output) <- c("termfreq","forest")
  output
}