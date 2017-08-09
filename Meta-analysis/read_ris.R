read_ris <- function(filepath,nRows){
  library(stringr)
  myIdx <- 1
  title <- NULL
  authorList <- NULL
  
  paperList <- data.frame(matrix(ncol = 2, nrow = nRows))
  colnames(paperList) <-c("Title","Authors")
  
  con = file(filepath, "r")
  while ( TRUE ) {
    line = readLines(con, n = 1)
    if ( length(line) == 0 ) {
      # print("end of file")
      break
    }
    if (!is.na(str_match(line, '^%T'))){
      # print(paste("title #", myIdx))
      title <- str_extract(line, '(?<=%T ).*')
      # print(title)
      
    } 
    if (!is.na(str_match(line, '^%A'))){
      author<- str_extract(line, '(?<=%A ).*')
      if (is.null(authorList)){
        authorList <- author
        # print(authorList)
      }else{
        authorList <- paste0(authorList, ',', author)
      }
      
    }
    if (nchar(line)==0){
      
      # time to save the entry
      paperList$Title[myIdx] <- title
      paperList$Authors[myIdx] <- authorList
      title <- NULL
      authorList <- NULL
      myIdx <- myIdx+1
    }
    
    
  }
  return(paperList)
}
