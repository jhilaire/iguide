runGUIDE <- function(i_fpath, i_fname, SHOW_LOG=TRUE) {
  cat("===================================== START OF GUIDE LOG FILE =========================================\n")
  if (Sys.info()['sysname'] == "Windows") {
    log <- shell(
      paste0("cd ", normalizePath(u_guidePath), " & guide < ", i_fname[["in"]]),
      intern=TRUE
    )
    cat(sapply(log, function(k) paste0(k, "\n")))
    writeLines(log, file.path(i_fpath$outPath, "guide.log"))
    file.copy(i_fpath$out, file.path(i_fpath$outPath, i_fname$out))
    file.copy(i_fpath$fn, file.path(i_fpath$outPath, i_fname$fn))
  } else {
    stop("Other OS not yet supported.")
  }
  cat("====================================== END OF GUIDE LOG FILE =========================================\n\n")
}