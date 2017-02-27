readin.csv <- function(i_filepath) {
  
  # Generating RData data file 
  p_dataRDatapath <- paste0(substr(i_filepath,1,nchar(i_filepath)-3), "RData")
  
  # Data file MD5sum
  p_dataMD5path <- paste0(substr(i_filepath,1,nchar(i_filepath)-3), "md5")
  p_dataMD5sum  <- md5sum(i_filepath)
  
  if (!file.exists(p_dataMD5path)) {
    cat("Data file is new. Reading in and saving as RData file...\n")
    cat(p_dataMD5sum,file=p_dataMD5path)
    p_data <- read.csv(i_filepath)
    save(p_data, file=p_dataRDatapath)
  } else {
    p_dataMD5sum_saved <- readLines(p_dataMD5path,n=1)
    if (p_dataMD5sum_saved == p_dataMD5sum) {
      cat("Reading in data from RData file...\n")
      load(p_dataRDatapath)
    } else {
      cat("Data file has been modified. Reading in csv file again and saving as RData file...\n")
      p_data <- read.csv(i_filepath)
      save(p_data, file=p_dataRDatapath)
    }
  }
  
  return(p_data)
}