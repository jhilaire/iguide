data_write <- function(i_data, i_vardef, i_treeType, i_treeOptions, i_fpath, i_fname) {
  cat(paste0("  - Removing country variable to avoid problems with GUIDE...\n"))
  i_data <- i_data %>% 
    select(-country, -year)
  
  #-- Write description file ---------------
  cat(paste0("  - Description file ", file.path(i_fpath$outPath, i_fname$desc),"\n"))
  catret(v_fname_data, file=file.path(i_fpath$outPath, i_fname$desc))             # Name of the training sample file (relative or absolute path)
  catret(u_missVal,    file=file.path(i_fpath$outPath, i_fname$desc), append=T)   # Missing value code (must always be provided)
  catret(u_startLine,  file=file.path(i_fpath$outPath, i_fname$desc), append=T)   # Line number of the first data record in the data file
  # Loop over variables and define column number, name and type
  for (kvar in names(i_data)) {
    catret(paste(
      which(names(i_data) == kvar),                         # Column number
      kvar,                                                 # Variable name 
      i_vardef$type[which(i_vardef$variable == kvar)]),     # Variable type
      file=file.path(i_fpath$outPath, i_fname$desc), append=T)
  }
  
  #-- Write data file ---------------------
  cat(paste0("  - data file ", file.path(i_fpath$outPath, i_fname$data),"\n"))
  write.table(i_data, file=file.path(i_fpath$outPath, i_fname$data), quote=FALSE, na=u_missVal, sep="\t", row.names=FALSE)
  
  #-- Write batch input file --------------
  cat(paste0("  - Batch input file ", file.path(i_fpath$outPath, i_fname[["in"]]),"\n"))
  if (i_treeType == "Single tree > LMS - Constant") {
    v_singleTree = TRUE
    v_treeType   = "LMS-constant"
    write_batchInputFile(v_singleTree, v_treeType, i_treeOptions, i_fpath)
  }
  if (i_treeType == "Single tree > LS - Constant") {
    v_singleTree = TRUE
    v_treeType   = "LS-constant"
    write_batchInputFile(v_singleTree, v_treeType, i_treeOptions, i_fpath)
  }
  
  return(i_data)
}