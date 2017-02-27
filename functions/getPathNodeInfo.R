getPathNodeInfo <- function(i_tnode, i_nodeInfo) {
  
  path <- as.numeric(rev(getParentNodeID(i_tnode)))
  
  
  
  # 3. Transform rules to functions
  tmp <- lapply(
    # 2. Generate list of rules from paths
    # TODO: Check if this produces the right output
    lapply(
      1:(length(path)-1),
      function(node) {
        tmp <- i_nodeInfo[which(i_nodeInfo$nodeID == path[node]),][which(as.numeric(strsplit(i_nodeInfo$childNodeIDs[which(i_nodeInfo$nodeID == path[node])][1], ",")[[1]]) == path[node+1]), c(2,4,5,6)]
      }),
    function(rule) {
      if (rule[2] == "<=") {
        out <- data.frame(nodeID=rule[1], variable=rule[2], operator=rule[3], value=rule[4])
      } else {
        out <- data.frame(nodeID=rule[1], variable=rule[2], operator=rule[3], value=rule[4])
      }
      return(out)
    }
  )
  
  out <- do.call("rbind", tmp)
  
  return(out)
  
}