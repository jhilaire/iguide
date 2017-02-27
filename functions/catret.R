# Modified cat function that appends a return carriage character at the end of a submitted string
catret <- function(i_text, ...) {
  cat(paste0(i_text,"\n"), ...)
}