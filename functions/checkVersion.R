checkWebsiteVersion <- function() {
  # load packages
  library(RCurl)
  library(XML)
  
  # download html
  html <- getURL("http://www.stat.wisc.edu/~loh/guide.html", followlocation = TRUE)
  
  # parse html
  doc = htmlParse(html, asText=TRUE)
  plain.text <- xpathSApply(doc, "//div[@id='content']/h1[@align='center']/strong", xmlValue)
  
  # Get version number
  ver <- gsub("version ", "", 
              regmatches(
                paste(plain.text), 
                gregexpr("(?<=\\().*?(?=\\))", 
                         paste(plain.text), 
                         perl=T))[[1]])
  
  return(ver)
  
}

checkLocalVersion <- function() {

  log <- shell(
    paste0("cd ../../GUIDE/ & guide"),
    intern=TRUE
  )
  
  ver <- strsplit(trimws(log[2]), " ", fixed=TRUE)[[1]][2]
  
  return(ver)
}