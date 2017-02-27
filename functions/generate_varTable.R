generate_varTable <- function(i_varDef) {
  if (!is.null(i_varDef)) {
    
    col_highlight = 1
    row_highlight = which(i_varDef$type != "x")
    
    hot <- rhandsontable(
      i_varDef %>%
        filter(!variable %in% c("country", "year"), type != "x"), 
      col_highlight = col_highlight, 
      row_highlight = row_highlight,
      rowHeaders = FALSE,
      width='800') %>%
      hot_cols(colWidths = c(200, 50, 100, 80, 70, 70)) %>%
      hot_col("variable",  format = "", source=paste(i_varDef$variable), language = "en-US", default="x") %>%
      hot_col("type",      format = "", source=c("d", "n", "c", "x"), language = "en-US", default="x") %>%
      hot_col("factor",    type="numeric", format = "0.0e+0", language = "en-US", default=1.0) %>%
      hot_col("transform", format = "", source=c("", "log"), language = "en-US", default="") %>%
      hot_col("demean",    type = "checkbox", format="logical", source=c(TRUE, FALSE), language = "en-US", default=TRUE) %>%
      hot_col("firstdiff", type = "checkbox", format="logical", source=c(TRUE, FALSE), language = "en-US", default=FALSE) %>%
      #hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) # %>%
    # hot_cols(renderer = "
    #            function(instance, td, row, col, prop, value, cellProperties) {
    #              Handsontable.renderers.TextRenderer.apply(this, arguments);
    # 
    #              td.style.background = 'lightgreen';
    # 
    #              return td;
    #            }")
    
    
    
    # ,
    # useTypes = TRUE, 
    # stretchH = "all"
  }
  
  return(hot)
}