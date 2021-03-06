plotly.tree <- function(i_tree, i_alloc) {
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
  
  # Generate Simple Node labels
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
  
  # Generate Node labels
  node_labels = paste(sapply(
    which(i_tree$nodeID %in% unique(i_tree$nodeID) & !duplicated(i_tree$nodeID)), 
    function(x) {
      
      cur_nodeType = i_tree$nodeType[x]
      cur_nodeid   = i_tree$nodeID[x]
      cur_var      = i_tree$variable[x] #strsplit(i_tree$variable[x], "_", fixed=TRUE)[[1]]
      cur_val      = round(i_tree$value[x], digits=2)
      
      if (cur_nodeType != "Terminal node") {
        out=paste0(cur_nodeType," [", cur_nodeid, "]\n", "Splitting variable: ", cur_var, "\n", "Cutting value: ", paste(cur_val))
      } else {
        depvar <- gsub("-mean", "", cur_var)
        tmp <- i_alloc %>% 
          select_("tnode",depvar) %>% 
          rename_("value"=depvar) %>% 
          filter(tnode == cur_nodeid) %>% 
          group_by(tnode) %>% 
          summarise(
            count=n(),
            se   = sd(value)) %>% 
          ungroup()
        cur_n  = tmp$count
        cur_se = round(tmp$se, digits=3)
          
        out=paste0(cur_nodeType," [", cur_nodeid, "]\n", "Dependent variable: ", cur_var, "\n", "Central value: ", paste(cur_val), "\n",
                   "N:", cur_n, " (",cur_se,")")
      }
      return(out)
    }))
  
  vars = unique(i_tree$variable)
  cols = rainbow(length(vars))
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
  
  
  L <- layout.reingold.tilford(g)
  
  vs <- V(g)
  es <- as.data.frame(get.edgelist(g))
  
  Nv <- length(vs)
  Ne <- length(es[1]$V1)
  
  Xn <- L[,1]
  Yn <- L[,2]
  
  data = data.frame(X=Xn, Y=Yn, 
                    varname = sapply(strsplit(vs$label, "\n"), function(x) strsplit(x[2],":")[[1]][2]), 
                    value   = sapply(strsplit(vs$label, "\n"), function(x) x[3]),
                    label   = gsub("\n", "</br>", vs$label),
                    color   = vs$color,
                    stringsAsFactors = FALSE)
  
  cols = data.frame(varname=sapply(strsplit(vs$label, "\n"), function(x) strsplit(x[2],":")[[1]][2]), color=gsub("black", "#000000FF", vs$color)) %>% 
    filter(!duplicated(varname))
  
  colors=cols$color
  names(colors) = cols$varname
  
  network <- plot_ly(data, x = ~X, y = ~Y, 
                     type="scatter", mode = "markers", color=~varname, colors="Set1", 
                     text = data$label, hoverinfo = "text", 
                     marker = list(size = 20, opacity=1.0))
  
  edge_shapes <- list()
  for(i in 1:Ne) {
    v0 <- as.numeric(paste(es[i,1])) # Parent node
    v1 <- as.numeric(paste(es[i,2])) # Child node
    
    edge_shape = list(
      type = "line",
      line = list(color = "#030303", width = 0.3),
      x0 = Xn[which(as.numeric(paste(vs$name)) == v0)], # Parent node - X coordinate
      y0 = Yn[which(as.numeric(paste(vs$name)) == v0)], # Parent node - Y coordinate
      x1 = Xn[which(as.numeric(paste(vs$name)) == v1)], # Child node - X coordinate
      y1 = Yn[which(as.numeric(paste(vs$name)) == v1)]  # Child node - Y coordinate
    )
    
    edge_shapes[[i]] <- edge_shape
  }
  
  axis <- list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)
  
  p <- layout(
    network,
    title  = 'Regression tree',
    shapes = edge_shapes,
    xaxis  = axis,
    yaxis  = axis
  )

  return(p)

}