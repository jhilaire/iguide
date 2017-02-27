# Allocate terminal nodes to data
allocateDataToNodes <- function(i_data, i_nodeInfo) {
  # Get all terminal nodes
  tNodeIDs <- i_nodeInfo$nodeID[which(i_nodeInfo$nodeType == "Terminal node")]
  
  paths <- lapply(tNodeIDs, getParentNodeID)
  
  # Get list of function sequences
  llfs <- lapply(
    # 1. Generate paths
    lapply(
      paths, 
      function(path) {
        as.numeric(rev(path))
      }
    ), 
    function(path) {
      # 3. Transform rules to functions
      lapply(
        # 2. Generate list of rules from paths
        # TODO: Check if this produces the right output
        lapply(
          1:(length(path)-1),
          function(node) {
            tmp <- i_nodeInfo[which(i_nodeInfo$nodeID == path[node]),][which(as.numeric(strsplit(i_nodeInfo$childNodeIDs[which(i_nodeInfo$nodeID == path[node])][1], ",")[[1]]) == path[node+1]), c(4,5,6)]
          }),
        function(rule) {
          if (rule[2] == "<=") {
            out <- function(data) { 
              filter_(data, interp(~ var1 <= var2, var1 = as.name(paste(rule[1])), var2 = as.numeric(paste(rule[3])))) %>% 
                mutate(tnode = as.numeric(path[length(path)]))
            }
          } else {
            out <- function(data) {
              filter_(data, interp(~ var1 > var2, var1 = as.name(paste(rule[1])), var2 = as.numeric(paste(rule[3])))) %>% 
                mutate(tnode = as.numeric(path[length(path)]))
            }
          }
          return(out)
        }
      )
    }
  )
  
  myfreduce <- function(i_lfs) {
    if (length(i_lfs) == 1) {
      i_lfs[[1]](i_data)
    } else {
      return(i_lfs[[1]](myfreduce(i_lfs[2:length(i_lfs)])))
    }
  }
  
  # Perform allocation
  out <- do.call("rbind", lapply(llfs, myfreduce))
  
  return(out)
  
}