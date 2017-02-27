plot.tree <- function(i_tree, filename="") {
  library(igraph)
  
  # Generate simple tree  
  g <- graph.tree(1,mode="out")
  
  # Populate tree
  # Vertices
  g <- g + vertices(unique(i_tree$nodeID)) #%>%
  #  set_vertex_attr("splitting_variable", value = i_tree$variable[which(i_tree$nodeID %in% unique(i_tree$nodeID) & !duplicated(i_tree$nodeID))]) %>% 
  #  set_vertex_attr("splitting_value",    value = i_tree$value[which(i_tree$nodeID %in% unique(i_tree$nodeID) & !duplicated(i_tree$nodeID))])
  
  # Edges
  vertex_seq = c()
  for (v in unique(i_tree$nodeID[which(i_tree$nodeType != "Terminal node")])) {
    left_edge  = trimws(strsplit(i_tree$childNodeIDs[which(i_tree$nodeID == v)[1]], ",", fixed=TRUE)[[1]][1])
    right_edge = trimws(strsplit(i_tree$childNodeIDs[which(i_tree$nodeID == v)[1]], ",", fixed=TRUE)[[1]][2])
    
    if (!is.na(left_edge))  vertex_seq = c(vertex_seq, v, left_edge)
    if (!is.na(right_edge)) vertex_seq = c(vertex_seq, v, right_edge)
  }
  g <- g + edge(vertex_seq)
  
  # Additional information
  
  #cols_node  = gsub("Root node", "black", gsub("Intermediate node", "orange", gsub("Terminal node", "red", i_tree$nodeType[which(i_tree$nodeID %in% unique(i_tree$nodeID) & !duplicated(i_tree$nodeID))])))
  #cols_label = gsub("Root node", "white", gsub("Intermediate node", "black", gsub("Terminal node", "black", i_tree$nodeType[which(i_tree$nodeID %in% unique(i_tree$nodeID) & !duplicated(i_tree$nodeID))])))
  
  #cols = c("blue","red","black")
  
  node_labels_simple = paste(sapply(strsplit(
    i_tree$variable[which(i_tree$nodeID %in% unique(i_tree$nodeID) & !duplicated(i_tree$nodeID))], "_", fixed=TRUE), 
    function(x) {
      
      if (x[1] == "log") {
        out=x[2]
      } else { 
        if (x[1] == "share") {
          out=paste0("%",x[3])
        } else {
          if (x[1] == "ratio") {
            out=paste0("%",x[2])
          } else {
            out=x[1]
          }
        }
      }
      return(out)
      }))
  
  node_labels = paste(sapply(
    which(i_tree$nodeID %in% unique(i_tree$nodeID) & !duplicated(i_tree$nodeID)), 
    function(x) {
      
      cur_node = i_tree$nodeID[x]
      cur_var  = strsplit(i_tree$variable[x], "_", fixed=TRUE)[[1]]
      cur_val  = round(i_tree$value[x], digits=2)
      
      if (cur_var[1] == "log") {
        out=paste0(cur_node, "\n", cur_var[2], "\n", paste(cur_val))
      } else { 
        if (cur_var[1] == "share") {
          out=paste0(cur_node, "\n", "%",cur_var[3], "\n", paste(cur_val))
        } else {
          if (cur_var[1] == "ratio") {
            out=paste0(cur_node, "\n", "%",cur_var[2], "\n", paste(cur_val))
          } else {
            out=paste0(cur_node, "\n", cur_var[1], "\n", paste(cur_val))
          }
        }
      }
      return(out)
    }))
  
  vars = unique(i_tree$variable)
  cols = rainbow(length(vars)) #RColorBrewer::brewer.pal(length(vars), "Set3")
  cols[which(grepl("co2", vars))] = "black"
  cols_node = paste(i_tree$variable[which(i_tree$nodeID %in% unique(i_tree$nodeID) & !duplicated(i_tree$nodeID))])

  for (k in vars) {
    cols_node  = gsub(k, cols[which(vars == k)], cols_node)
  }
  cols_label = gsub("co2", "white", node_labels_simple)
  cols_label[which(cols_label != "white")] = rep("black", length(which(cols_label != "white")))
  
  g <- g  %>%
    set_vertex_attr("color", value = cols_node) %>% 
    set_vertex_attr("frame.color", value = cols_node) %>% 
    set_vertex_attr("label.color", value = cols_label) %>% 
    set_vertex_attr("label", value = node_labels) %>%  
    set_vertex_attr("splitting_variable", value = i_tree$variable[which(i_tree$nodeID %in% unique(i_tree$nodeID) & !duplicated(i_tree$nodeID))]) %>% 
    set_vertex_attr("splitting_value",    value = i_tree$value[which(i_tree$nodeID %in% unique(i_tree$nodeID) & !duplicated(i_tree$nodeID))])
  
  #g <- g  %>%
  #  set_edge_attr("label", value = paste("<=", i_tree$value[which(i_tree$nodeID %in% unique(i_tree$nodeID) & !duplicated(i_tree$nodeID))]))
  
  # Plot tree
  if (filename != "") {
    png(filename)
    plot(g, layout=layout.reingold.tilford, 
         vertex.size=4,
         vertex.label.cex=0.5,
         edge.arrow.size=0.1, edge.curved=FALSE,
         asp = 0.4,
         margin=-0.1) #, vertex.label.dist = 1, vertex.label.degree=pi/2
    dev.off()
  } else {
    plot(g, layout=layout.reingold.tilford, 
         vertex.size=4,
         vertex.label.cex=0.5,
         edge.arrow.size=0.1, edge.curved=FALSE,
         asp = 0.4,
         margin=-0.1) #, vertex.label.dist = 1, vertex.label.degree=pi/2
  }
  
  return(p)
}