plotTransitionTNodes <- function(i_alloc, i_nodeInfo) {
  data_TransitionNodes <- computeTransitionMatrix(i_alloc, i_nodeInfo)
  
  # Get all terminal nodes
  tNodeIDs <- i_nodeInfo$nodeID[which(i_nodeInfo$nodeType == "Terminal node")]
  
  library(igraph)
  
  # Initialise graph  
  g <- graph_from_data_frame(data_TransitionNodes, directed = TRUE, vertices=i_nodeInfo[which(i_nodeInfo$nodeType == "Terminal node"), ] %>% select_("nodeID", "parentNodeID", "childNodeIDs", "variable", "operator", "value"))
  
  # Color code nodes
  colGrad1 <- colorRampPalette(c("#fef0d9ff", "#b30000ff"))
  vertex_attr(g, "colour") <- colGrad1(length(V(g)))
  
  # Color code edges
  colGrad2 <- colorRampPalette(c("#ffffff", "#0000ffff"))
  ecols    <- colGrad2(1000)
  
  edge_attr(g, "colour") <- ecols[round(edge.attributes(g)$probability*1000, digits=0)]
  edge_attr(g, "width")  <- ifelse(round(edge.attributes(g)$probability*5, digits=0) == 0, 1, round(edge.attributes(g)$probability*5, digits=0))
  
  # Define layout
  igraph.drl.options <- igraph.drl.default
  igraph.drl.options$edge.cut               <- 0.8
  igraph.drl.options$init.iterations        <- 0
  igraph.drl.options$init.temperature       <- 2000
  igraph.drl.options$init.attraction        <- 7 #10
  igraph.drl.options$init.damping.mult      <- 1
  igraph.drl.options$liquid.iterations      <- 200
  igraph.drl.options$liquid.temperature     <- 2000
  igraph.drl.options$liquid.attraction      <- 10
  igraph.drl.options$liquid.damping.mult    <- 1
  igraph.drl.options$expansion.iterations   <- 200
  igraph.drl.options$expansion.temperature  <- 2000
  igraph.drl.options$expansion.attraction   <- 2
  igraph.drl.options$expansion.damping.mult <- 1
  igraph.drl.options$cooldown.iterations    <- 200
  igraph.drl.options$cooldown.temperature   <- 2000
  igraph.drl.options$cooldown.attraction    <- 1
  igraph.drl.options$cooldown.damping.mult  <- 0.1
  igraph.drl.options$crunch.iterations      <- 50
  igraph.drl.options$crunch.temperature     <- 250
  igraph.drl.options$crunch.attraction      <- 3 # 1
  igraph.drl.options$crunch.damping.mult    <- 0.25
  igraph.drl.options$simmer.iterations      <- 100
  igraph.drl.options$simmer.temperature     <- 250
  igraph.drl.options$simmer.attraction      <- 1 # 0.5
  igraph.drl.options$simmer.damping.mult    <- 0
  layout <- layout.drl(g, use.seed = 1, weights = NULL, fixed = NULL, dim = 2, options = igraph.drl.options)
  #layout <- layout.gem(g)
  
  # Plot graph
  plot(g, 
       layout = layout,
       # Vertex options
       vertex.size=12,
       vertex.label.cex=0.9,
       vertex.label.family='helvetica bold',
       vertex.label.font=2, # 1: plain text, 2: bold face, 3: italic, 4: bold and italic, 5: symbol font
       vertex.label.color="black",
       vertex.color=V(g)$colour,
       # Edge options
       edge.width=E(g)$width, 
       edge.curved=TRUE,
       edge.color =E(g)$colour,
       edge.arrow.size=0.3,
       # Other options
       asp = 0.4,
       margin=-0.1)
  
  #write.graph(g,"Tree17nodes_transitions.gml", format="gml")
  
}
