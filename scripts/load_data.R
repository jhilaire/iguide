#=================================================================================
# Load data
#=================================================================================
if (!file.exists(p_dataMD5path)) {
  cat("Data file is new. Reading in and saving as RData file...\n")
  cat(p_dataMD5sum,file=p_dataMD5path)
  p_data <- read.csv(u_dataPath)
  save(p_data, file=p_dataRDatapath)
} else {
  p_dataMD5sum_saved <- readLines(p_dataMD5path,n=1)
  if (p_dataMD5sum_saved == p_dataMD5sum) {
    cat("Reading in data from RData file...\n")
    load(p_dataRDatapath)
  } else {
    cat("Data file has been modified. Reading it in again...\n")
    p_data <- read.csv(u_dataPath)
    save(p_data, file=p_dataRDatapath)
  }
}
cat("\n")