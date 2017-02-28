parse_GUIDEOutput <- function(i_outfpath) {
  
  #============ INITIALISE =================
  #---- Load libraries ----
  #library(WDI)
  library(lazyeval)
  library(dplyr)
  library(tidyr)
  library(tools)
  library(ggplot2)
  
  
  v_OutputData <- readLines(i_outfpath)
  
  # Get the basic structure information of the regression tree
  v_treeLineStart <- grep(" Regression tree:", v_OutputData) +2
  v_treeLineEnd   <- grep(" ***************************************************************", v_OutputData, fixed = TRUE) -2
  
  v_nbNodes <- v_treeLineEnd - v_treeLineStart +1
  
  v_tree            <- list()
  v_parent          <- "root"
  v_curparentNodeID <- 0
  
  # Loop over nodes
  for (kitr in 1:v_nbNodes) {
    v_curLine <- v_OutputData[v_treeLineStart + kitr-1]
    
    # Tree level
    v_pos <- (as.numeric(regexpr("Node", v_curLine)) - 3)/2
    
    # Node ID
    v_tmp1      <- strsplit(v_curLine, ":", fixed=TRUE)
    v_curNodeID <- trimws(gsub("Node ", "", v_tmp1[[1]][1]))
    
    # Splitting variable, value and operator
    v_tmp2        <- strsplit(trimws(v_tmp1[[1]][2]), " ", fixed=TRUE)
    v_curSplitVar <- v_tmp2[[1]][1]
    v_curSplitOp  <- v_tmp2[[1]][2]
    v_curSplitVal <- paste(v_tmp2[[1]][4:length(v_tmp2[[1]])], collapse=" ")
    
    # Save data
    v_tree[[kitr]] <- data.frame(position     = v_pos, 
                                 nodeID       = v_curNodeID, 
                                 parentNodeID = v_curparentNodeID, 
                                 variable     = v_curSplitVar, 
                                 operator     = v_curSplitOp, 
                                 value        = v_curSplitVal, 
                                 stringsAsFactors = FALSE)
    
    v_curparentNodeID <- v_curNodeID
    
  }
  
  v_tree <- do.call("rbind", v_tree) %>% 
    mutate(nodeType = "")
  
  # Get additional information of the regression tree
  v_treeLineStart <- grep(" ***************************************************************", v_OutputData, fixed = TRUE) +2
  v_treeLineEnd   <- grep(" Proportion of deviance explained by tree model =", v_OutputData, fixed = TRUE) -3
  v_treeLineSep   <- grep(" ----------------------------", v_OutputData, fixed = TRUE)
  
  v_nbLines <- v_treeLineEnd - v_treeLineStart +1
  
  # Loop over edges
  for(kitr in 1:(length(v_treeLineSep)-1)) {
    
    v_curLineStart <- c(v_treeLineStart, v_treeLineSep)[kitr] +1
    v_curLineEnd   <- c(v_treeLineStart, v_treeLineSep)[kitr+1] -1
    
    v_curLine <- v_OutputData[v_curLineStart]
    
    # Line containing Node ID information (always the first one)
    v_tmp1  <- strsplit(v_curLine, ":", fixed=TRUE)
    # Node ID
    v_curNodeID <- trimws(gsub("Node ", "", v_tmp1[[1]][1]))
    # Node Type
    v_curNodeType <- trimws(v_tmp1[[1]][2])
    
    # print(v_curNodeID)
    # print(v_curNodeType)
    
    v_tree$nodeType[which(v_tree$nodeID == v_curNodeID)] <- v_curNodeType
    
  }
  
  # Correct tree architecture
  for (klvl in 2:max(v_tree$position)) {
    v_curNodeIDs <- v_tree$nodeID[which(v_tree$position == klvl & v_tree$nodeType == "Intermediate node")]
    
    for (kid in unique(v_curNodeIDs)) {
      v_tree$parentNodeID[which(v_tree$position == klvl & v_tree$nodeType == "Intermediate node" & v_tree$nodeID == kid)][2] = v_tree$parentNodeID[which(v_tree$position == klvl & v_tree$nodeType == "Intermediate node" & v_tree$nodeID == kid)][1]
    }
  }
  
  if (length(which(v_tree$childNodeIDs != "" & v_tree$nodeType == "Terminal node" & is.na(v_tree$value))) != 0) {
    v_tree$nodeType[which(v_tree$childNodeIDs != "" & v_tree$nodeType == "Terminal node" & is.na(v_tree$value))]       <- "Intermediate node"
  }
  if (length(which(v_tree$childNodeIDs != "" & v_tree$nodeType == "Terminal node" & !is.na(v_tree$value))) != 0) {
    v_tree$childNodeIDs[which(v_tree$childNodeIDs != "" & v_tree$nodeType == "Terminal node" & !is.na(v_tree$value))] <- ""
  }
  
  
  # Define child nodes
  v_tree <- v_tree %>% 
    mutate(childNodeIDs = "")
  nodeIDs <- unique(v_tree$parentNodeID)[2:length(unique(v_tree$parentNodeID))]
  
  # Define parent nodes
  for (kid in nodeIDs) {
    v_tree$childNodeIDs[which(v_tree$nodeID == kid)] <- paste0(unique(v_tree$nodeID[which(v_tree$parentNodeID == kid)]), collapse=",")
  }
  v_tree$nodeType[which(v_tree$operator == "=")] = "Terminal node"
  v_tree$nodeType[which(v_tree$nodeID == 1)]     = "Root node"
  v_tree$parentNodeID[which(v_tree$nodeID == 1)] = NA
  
  # Remove NAs
  v_tree <- v_tree %>% 
    mutate(value = trimws(gsub("or NaN", "", value))) %>% 
    mutate(value = trimws(gsub("NA", "", value))) %>% 
    mutate(value = as.numeric(value))
  
  #print(which(is.na(v_tree$value)))
  
  for (kna in which(is.na(v_tree$value))) {
    
    val <- v_tree$value[which(v_tree$nodeID == v_tree$nodeID[kna])]
    
    #print(val)
    
    v_tree$value[kna] <- ifelse(length(val[which(!is.na(val))]) == 0, NA, val[which(!is.na(val))])
  }
  
  return(v_tree)

}