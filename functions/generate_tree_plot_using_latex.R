generate_tree_plot_using_latex <- function(i_path_guide, i_path_out, i_path_tex, i_file_tex, i_path_pdf, i_file_pdf) {
  if (Sys.info()['sysname'] == "Windows") {
    shell(
      paste0("cd ", normalizePath(i_path_guide),                            " & latex ",  
             paste0(strsplit(i_file_tex, ".", fixed=TRUE)[[1]][1], ".tex"), " & dvips ",  
             paste0(strsplit(i_file_tex, ".", fixed=TRUE)[[1]][1], ".dvi"), " & ps2pdf ", 
             paste0(strsplit(i_file_tex, ".", fixed=TRUE)[[1]][1], ".ps"),  " ",
             paste0(strsplit(i_file_tex, ".", fixed=TRUE)[[1]][1], ".pdf"), " & rm ", 
             paste0(strsplit(i_file_tex, ".", fixed=TRUE)[[1]][1], ".aux"), " & rm ", 
             paste0(strsplit(i_file_tex, ".", fixed=TRUE)[[1]][1], ".log"), " & rm ", 
             paste0(strsplit(i_file_tex, ".", fixed=TRUE)[[1]][1], ".dvi"), " & rm ", 
             paste0(strsplit(i_file_tex, ".", fixed=TRUE)[[1]][1], ".ps"),  " & mv ",
             paste0(strsplit(i_file_tex, ".", fixed=TRUE)[[1]][1], ".pdf"), " ",
             i_file_pdf),
      intern=TRUE
    )
    file.copy(i_path_tex, file.path(i_path_out, i_file_tex))
    file.copy(i_path_pdf, file.path(i_path_out, i_file_pdf))
  } else {
    stop("Other OS not yet supported.")
  }
}
