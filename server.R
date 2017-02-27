#== USER SETTINGS ================================================================
source("scripts/user_settings.R")

#== INITITALISATION ==============================================================
source("scripts/init.R")

#== LOAD DATA ====================================================================
source("scripts/load_data.R")

#== PROCESS DATA =================================================================
source("scripts/proc_data.R")


#=================================================================================
# Shiny Server configuration
#=================================================================================
shinyServer(function(input, output, session) {
  
  #=========== REACTIVE ======================
  values <- reactiveValues()
  
  #=========== OBSERVE EVENTS ================
  
  #=========== OBSERVE =======================
  # Panel 1: Define variables
  observe({
    if (!is.null(input$variableDefinition)) {
      u_variableDefinition <<- hot_to_r(input$variableDefinition)
    } else {
      if (is.null(values[["u_variableDefinition"]]))
        u_variableDefinition <<- u_variableDefinition
      else
        u_variableDefinition <<- values[["u_variableDefinition"]]
    }
    
    # Update variable choice
    choice_vars        <<- as.list(u_variableDefinition$variable[which(u_variableDefinition$type != "x")])
    names(choice_vars) <<- u_variableDefinition$variable[which(u_variableDefinition$type != "x")]
    
  })
  
  # Panel 3: Generate regression tree
  observe({
    if(input$run) {
      
      # Prepare input data
      cat("\nPreparing data...\n")
      v_dataProc <- data_prepare(p_data, u_variableDefinition, input$period[1], input$period[2], DEMEAN=TRUE)
      v_variableDefinition <- get_renamedVariables(u_variableDefinition)
      
      # Write data
      cat("\nWriting out files...\n")
      v_dataWrite <- data_write(v_dataProc, v_variableDefinition, input$treemod, v_fpath, v_fname)
      
      # Run GUIDE
      cat("\nRunning GUIDE...\n")
      runGUIDE(v_fpath, v_fname, SHOW_LOG=TRUE)
      
      # Plot tree
      cat("Plotting regression tree...\n")
      cat("  > Parsing GUIDE output...\n")
      v_tree  <<- parse_GUIDEOutput(v_fpath$out)
      cat("  > Allocating data to nodes...\n")
      v_alloc <<- allocateDataToNodes(v_dataProc, v_tree)
      cat("  > Rendering tree...\n")      
      output$generateTree <- renderPlotly({
        
        library(igraph)
        
        # Generate simple tree  
        g <- graph.tree(1,mode="out")
        
        # Populate tree
        # Vertices
        g <- g + vertices(unique(v_tree$nodeID))
        
        # Edges
        vertex_seq = c()
        for (v in unique(v_tree$nodeID[which(v_tree$nodeType != "Terminal node")])) {
          left_edge  = trimws(strsplit(v_tree$childNodeIDs[which(v_tree$nodeID == v)[1]], ",", fixed=TRUE)[[1]][1])
          right_edge = trimws(strsplit(v_tree$childNodeIDs[which(v_tree$nodeID == v)[1]], ",", fixed=TRUE)[[1]][2])
          
          if (!is.na(left_edge))  vertex_seq = c(vertex_seq, v, left_edge)
          if (!is.na(right_edge)) vertex_seq = c(vertex_seq, v, right_edge)
        }
        g <- g + edge(vertex_seq)
        
        # Additional information
        node_labels_simple = paste(sapply(strsplit(
          v_tree$variable[which(v_tree$nodeID %in% unique(v_tree$nodeID) & !duplicated(v_tree$nodeID))], "_", fixed=TRUE), 
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
          which(v_tree$nodeID %in% unique(v_tree$nodeID) & !duplicated(v_tree$nodeID)), 
          function(x) {
            
            cur_var = strsplit(v_tree$variable[x], "_", fixed=TRUE)[[1]]
            cur_val = round(v_tree$value[x], digits=2)
            
            if (cur_var[1] == "log") {
              out=paste0(cur_var[2], "\n", paste(cur_val))
            } else { 
              if (cur_var[1] == "share") {
                out=paste0("%",cur_var[3], "\n", paste(cur_val))
              } else {
                if (cur_var[1] == "ratio") {
                  out=paste0("%",cur_var[2], "\n", paste(cur_val))
                } else {
                  out=paste0(cur_var[1], "\n", paste(cur_val))
                }
              }
            }
            return(out)
          }))
        
        vars = unique(v_tree$variable)
        cols = rainbow(length(vars)) #RColorBrewer::brewer.pal(length(vars), "Set3")
        cols[which(grepl("co2", vars))] = "black"
        cols_node = paste(v_tree$variable[which(v_tree$nodeID %in% unique(v_tree$nodeID) & !duplicated(v_tree$nodeID))])
        
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
          set_vertex_attr("splitting_variable", value = v_tree$variable[which(v_tree$nodeID %in% unique(v_tree$nodeID) & !duplicated(v_tree$nodeID))]) %>% 
          set_vertex_attr("splitting_value",    value = v_tree$value[which(v_tree$nodeID %in% unique(v_tree$nodeID) & !duplicated(v_tree$nodeID))])
        
        
        L <- layout.reingold.tilford(g)
        
        vs <- V(g)
        es <- as.data.frame(get.edgelist(g))
        
        Nv <- length(vs)
        Ne <- length(es[1]$V1)
        
        Xn <- L[,1]
        Yn <- L[,2]
        
        data = data.frame(X=Xn, Y=Yn, 
                          varname = sapply(strsplit(vs$label, "\n"), function(x) x[1]), 
                          value   = sapply(strsplit(vs$label, "\n"), function(x) x[2]),
                          label   = vs$label,
                          color   = vs$color,
                          stringsAsFactors = FALSE)
        
        cols = data.frame(varname=sapply(strsplit(vs$label, "\n"), function(x) x[1]), color=gsub("black", "#000000FF", vs$color)) %>% 
          filter(!duplicated(varname))
        
        colors=cols$color
        names(colors) = cols$varname
        
        network <- plot_ly(data, x = ~X, y = ~Y, type="scatter", mode = "markers", color=~varname, colors="Set1", text = data$label, hoverinfo = "text", marker = list(size = 14, opacity=1.0))
        
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
        
        p
        
      })
      
    }
  })
  
  
  #============ UI =========================
  # Panel 1: Define variables
  output$variableDefinition <- renderRHandsontable({
    #u_variableDefinition <- values[["u_variableDefinition"]]
    if (!is.null(u_variableDefinition)) {
      
      col_highlight = 1
      row_highlight = which(u_variableDefinition$type != "x")
      
      rhandsontable(
        u_variableDefinition %>%
          filter(!variable %in% c("country", "year"), type != "x"), 
        col_highlight = col_highlight, 
        row_highlight = row_highlight,
        rowHeaders = FALSE,
        width='800') %>%
        hot_cols(colWidths = c(200, 50, 100, 80)) %>%
        hot_col("variable",  format = "", source=paste(u_variableDefinition$variable), language = "en-US", default="x") %>%
        hot_col("type",      format = "", source=c("d", "n", "c", "x"), language = "en-US", default="x") %>%
        hot_col("factor",    type="numeric", format = "0.0e+0", language = "en-US", default=1.0) %>%
        hot_col("transform", format = "", source=c("", "log"), language = "en-US", default="") %>%
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
    
  })
  
  # Panel 2: Explore original and processed input data
  output$chooseCountry <- renderUI({
    
    lvls <- paste((u_variableDefinition %>% filter(type != "x"))$variable)
    
    tmp = p_data %>%
      filter(country != "World") %>%
      filter(year >= input$period[1], year <= input$period[2]) %>%
      gather(variable, value, -country, -year) %>%
      inner_join(u_variableDefinition, by = "variable") %>%
      filter(type != "x") %>%
      mutate(variable = factor(variable,
                               levels=lvls,
                               ordered=TRUE)) %>%
      group_by(country) %>%
      mutate(count=ifelse(is.na(value), 0, 1)) %>%
      summarise(value=sum(count)) %>%
      ungroup() %>%
      mutate(ratio=value/max(value))
    
    select_countries = tmp$country[which(tmp$ratio >= (1-input$ratio))]
    
    selectizeInput("country", 
                   label    = h3("Countrie(s):"), 
                   choices  = choice_countries,
                   selected = select_countries, 
                   multiple = TRUE,
                   options  = list(maxItems = 10, placeholder = 'Select countries'))
    
  })
  output$chooseVar     <- renderUI({
    
    choice_vars <<- as.list(u_variableDefinition$variable[which(u_variableDefinition$type != "x")])
    names(choice_vars) <<- u_variableDefinition$variable[which(u_variableDefinition$type != "x")]
    
    selectizeInput("variable",
                   label = h3("Variable:"),
                   choices = choice_vars,
                   selected = "co2_terr_CDIAC_pc",
                   multiple=FALSE,
                   options = list(maxItems = 1, placeholder = 'Select variable'))
  })
  
  # Panel 3: Generate regression tree
  output$chooseTreeModel <- renderUI({
    
    selectizeInput("treemod",
                   label = h3("Tree model:"),
                   choices = list("Single tree > LMS - Constant"="Single tree > LMS - Constant"),
                   selected = "Single tree > LMS - Constant",
                   multiple=FALSE,
                   options = list(maxItems = 1, placeholder = 'Select tree model'))
  })
  
  # Panel 4: Explore results # 1 - T-nodes  
  output$chooseTnode <- renderUI({
    
    choice_tnodes <- unique(v_tree$nodeID[which(v_tree$nodeType == "Terminal node")])
    select_tnode  <- choice_tnodes[1]    
    
    selectizeInput("tnode", 
                   label    = h3("Terminal node:"), 
                   choices  = choice_tnodes,
                   selected = select_tnode, 
                   multiple = FALSE,
                   options = list(maxItems = 1, placeholder = 'Select T-node'))
    
  })
  
  # Panel 5: Explore results # 2 - Country pathways
  output$chooseCountry2 <- renderUI({
    
    lvls <- paste((u_variableDefinition %>% filter(type != "x"))$variable)
    
    tmp = p_data %>%
      filter(country != "World") %>%
      filter(year >= input$period[1], year <= input$period[2]) %>%
      gather(variable, value, -country, -year) %>%
      # filter(variable == input$variable) %>%
      inner_join(u_variableDefinition, by = "variable") %>%
      filter(type != "x") %>%
      mutate(variable = factor(variable,
                               levels=lvls,
                               ordered=TRUE)) %>%
      group_by(country) %>%
      mutate(count=ifelse(is.na(value), 0, 1)) %>%
      summarise(value=sum(count)) %>%
      ungroup() %>%
      mutate(ratio=value/max(value))
    
    select_countries = c("China", "Germany")
    
    checkboxGroupInput("country2", 
                       label = h3("Countrie(s):"), 
                       choices = choice_countries,
                       selected = select_countries, inline=TRUE)
    
    
    
  })
  
  
  #============ TEXT =========================
  # Panel 1: Define variables
  output$GUIDEversion <- renderText({
    out    <- ""
    # tryCatch(
    #   ver_ws <<- checkWebsiteVersion(),
    #   error = function(e) {
    #     ver_ws <<- NULL
    #   } 
    # )
    ver_ws <- NULL
    tryCatch(
      ver_lo <<- checkLocalVersion(),
      error = function(e) {
        ver_lo <<- NULL
      } 
    )
    
    if (!is.null(ver_ws) & !is.null(ver_lo)) {
      if (as.numeric(ver_lo) < as.numeric(ver_ws)) {
        out <- paste0("A new GUIDE version is available: ", ver_ws, "\n(Local version: ", ver_lo,").\n")
      }
    } else {
      print("Warning: could not check GUIDE versions.")
      print(paste("ver_ws:", ver_ws))
      print(paste("ver_lo:", ver_lo))
    }
    
    return(out)
  })
  
  # Panel 2: Explore original and processed input data
  output$info_year    <- renderText({
    print(paste0("Number of selected years: ", paste(input$period[2]-input$period[1]+1)))
  })
  output$info_country <- renderText({
    
    lvls <- paste((u_variableDefinition %>% filter(type != "x"))$variable)
    
    tmp = p_data %>%
      filter(country != "World") %>%
      filter(year >= input$period[1], year <= input$period[2]) %>%
      gather(variable, value, -country, -year) %>%
      # filter(variable == input$variable) %>%
      inner_join(u_variableDefinition, by = "variable") %>%
      filter(type != "x") %>%
      mutate(variable = factor(variable,
                               levels=lvls,
                               ordered=TRUE)) %>%
      group_by(country) %>%
      mutate(count=ifelse(is.na(value), 0, 1)) %>%
      summarise(value=sum(count)) %>%
      ungroup() %>%
      mutate(ratio=value/max(value))
    
    select_countries = tmp$country[which(tmp$ratio >= (1-input$ratio))]
    
    print(paste0("Number of selected countries: ", paste(length(select_countries)), " out of ", paste(length(unique(p_data$country))-1)))
    
  })
  
  
  #============ PLOT =========================
  # Panel 2: Explore original and processed input data
  output$inputDataPlot <- renderPlotly({
    
    # Original data
    v_plotOriginal <- p_data %>% 
      filter(country %in% input$country) %>% 
      filter(year >= input$period[1], year <= input$period[2]) %>% 
      gather(variable, value, -country, -year) %>% 
      inner_join(u_variableDefinition, by = "variable") %>% 
      filter(type != "x") %>% 
      filter(variable %in% input$variable)
    
    p1 = ggplot(v_plotOriginal) + 
      geom_line(aes(x=year,y=value,color=country)) + 
      geom_point(aes(x=year,y=value,color=country)) +
      facet_wrap(~variable, scales = "free_y") +
      theme_bw() +
      xlab("") +
      ggtitle("Original") +
      theme(legend.position="none")
    
    # Processed data
    v_plotProcessed <- p_data %>% 
      filter(country %in% input$country) %>% 
      filter(year >= input$period[1], year <= input$period[2]) %>% 
      gather(variable, value, -country, -year) %>% 
      inner_join(u_variableDefinition, by = "variable") %>% 
      filter(type != "x") %>% 
      filter(variable %in% input$variable) %>% 
      mutate(value=value*factor)  +
      ggtitle("Processed") +
      
      tmp_numVars = (u_variableDefinition %>% 
                       filter(type %in% c("d", "n", "f", "s"), 
                              transform != "") %>% 
                       select(variable)
      )$variable %>% 
      paste()
    
    for (kvar in tmp_numVars) {
      # Logarithm
      if (u_variableDefinition$transform[which(u_variableDefinition$variable == kvar)] == "log") {
        # Apply transformation
        v_plotProcessed$value[which(v_plotProcessed$variable == kvar)] <- log(v_plotProcessed$value[which(v_plotProcessed$variable == kvar)])
        # Rename variable
        #p_variableRenaming <- rbind(p_variableRenaming,
        #                            data.frame(old=kvar, new=paste0("log_",kvar)))
      }
      # Other functions ...
    }
    
    if (input$demean) {
      for (kvar in tmp_numVars) {
        v_plotProcessed <- v_plotProcessed %>%
          group_by(country,variable) %>%
          mutate(value=ifelse(type %in% c("d","n","s","f"), value-mean(value), value)) %>% 
          ungroup()
      }
      
    }
    
    p2 = ggplot(v_plotProcessed) + 
      geom_line(aes(x=year,y=value,color=country)) + 
      geom_point(aes(x=year,y=value,color=country)) +
      facet_wrap(~variable, scales = "free_y") +
      theme_bw() +
      xlab("") +
      theme(legend.position="none")
    
    if (input$ylog) {
      p1 = p1 + scale_y_log10()
      p2 = p2 + scale_y_log10()
    }
    
    subplot(
      ggplotly(p1), # Original data
      ggplotly(p2)) # Processed data
    
  })
  
  # Panel 3: Generate interactive regression tree
  output$generateTree <- renderPlotly({
    
    library(igraph)
    
    # Generate simple tree  
    g <- graph.tree(1,mode="out")
    
    # Populate tree
    # Vertices
    g <- g + vertices(unique(v_tree$nodeID))
    
    # Edges
    vertex_seq = c()
    for (v in unique(v_tree$nodeID[which(v_tree$nodeType != "Terminal node")])) {
      left_edge  = trimws(strsplit(v_tree$childNodeIDs[which(v_tree$nodeID == v)[1]], ",", fixed=TRUE)[[1]][1])
      right_edge = trimws(strsplit(v_tree$childNodeIDs[which(v_tree$nodeID == v)[1]], ",", fixed=TRUE)[[1]][2])
      
      if (!is.na(left_edge))  vertex_seq = c(vertex_seq, v, left_edge)
      if (!is.na(right_edge)) vertex_seq = c(vertex_seq, v, right_edge)
    }
    g <- g + edge(vertex_seq)
    
    # Additional information
    node_labels_simple = paste(sapply(strsplit(
      v_tree$variable[which(v_tree$nodeID %in% unique(v_tree$nodeID) & !duplicated(v_tree$nodeID))], "_", fixed=TRUE), 
      function(x) {
        
        if (x[1] == "log") {          # if variable has a log prefix
          out=x[2]
        } else { 
          if (x[1] == "share") {      # if variable has a share prefix
            out=paste0("%",x[3])
          } else {
            if (x[1] == "ratio") {    # if variable has a ratio prefix
              out=paste0("%",x[2])
            } else {
              out=x[1]
            }
          }
        }
        return(out)
      }))
    
    node_labels = paste(sapply(
      which(v_tree$nodeID %in% unique(v_tree$nodeID) & !duplicated(v_tree$nodeID)), 
      function(x) {
        
        cur_var = strsplit(v_tree$variable[x], "_", fixed=TRUE)[[1]]
        cur_val = round(v_tree$value[x], digits=2)
        
        if (cur_var[1] == "log") {
          out=paste0(cur_var[2], "\n", paste(cur_val))
        } else { 
          if (cur_var[1] == "share") {
            out=paste0("%",cur_var[3], "\n", paste(cur_val))
          } else {
            if (cur_var[1] == "ratio") {
              out=paste0("%",cur_var[2], "\n", paste(cur_val))
            } else {
              out=paste0(cur_var[1], "\n", paste(cur_val))
            }
          }
        }
        return(out)
      }))
    
    vars = unique(v_tree$variable)
    cols = rainbow(length(vars)) #RColorBrewer::brewer.pal(length(vars), "Set3")
    cols[which(grepl("co2", vars))] = "black"
    cols_node = paste(v_tree$variable[which(v_tree$nodeID %in% unique(v_tree$nodeID) & !duplicated(v_tree$nodeID))])
    
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
      set_vertex_attr("splitting_variable", value = v_tree$variable[which(v_tree$nodeID %in% unique(v_tree$nodeID) & !duplicated(v_tree$nodeID))]) %>% 
      set_vertex_attr("splitting_value",    value = v_tree$value[which(v_tree$nodeID %in% unique(v_tree$nodeID) & !duplicated(v_tree$nodeID))])
    
    
    L <- layout.reingold.tilford(g)
    
    vs <- V(g)
    es <- as.data.frame(get.edgelist(g))
    
    Nv <- length(vs)
    Ne <- length(es[1]$V1)
    
    Xn <- L[,1]
    Yn <- L[,2]
    
    data = data.frame(X=Xn, Y=Yn, 
                      varname = sapply(strsplit(vs$label, "\n"), function(x) x[1]), 
                      value   = sapply(strsplit(vs$label, "\n"), function(x) x[2]),
                      label   = vs$label,
                      color   = vs$color,
                      stringsAsFactors = FALSE)
    
    cols = data.frame(varname=sapply(strsplit(vs$label, "\n"), function(x) x[1]), color=gsub("black", "#000000FF", vs$color)) %>% 
      filter(!duplicated(varname))
    
    colors=cols$color
    names(colors) = cols$varname
    
    network <- plot_ly(data, x = ~X, y = ~Y, type="scatter", mode = "markers", color=~varname, colors="Set1", text = data$label, hoverinfo = "text", marker = list(size = 14, opacity=1.0))
    
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
    
    p
    
  })
  
  # Panel 4: Explore results (T-node)
  plot_height <- function() {
    # calculate values$facetCount
    height <- 80*length(unique(v_alloc$country[which(v_alloc$tnode == input$tnode)]))
    return(height)
  }
  output$TnodePlot <- renderPlot({
    
    plot_TNode(v_alloc, input$tnode, v_tree, PLOT_MAP=FALSE)
    
  })
  output$ui_TNodePlot <- renderUI({
    plotOutput("TnodePlot", height = plot_height(), width = "100%")
  })
  
  
  #============ TABLE =========================
  # Panel 2: Explore original and processed input data
  output$BalancedTable <- renderTable({
    
    lvls <- paste((u_variableDefinition %>% filter(type != "x"))$variable)
    
    tmp = p_data %>%
      filter(country != "World") %>%
      filter(year >= input$period[1], year <= input$period[2]) %>%
      gather(variable, value, -country, -year) %>%
      # filter(variable == input$variable) %>%
      inner_join(u_variableDefinition, by = "variable") %>%
      filter(type != "x") %>%
      mutate(variable = factor(variable,
                               levels=lvls,
                               ordered=TRUE)) %>%
      group_by(country) %>%
      mutate(count=ifelse(is.na(value), 0, 1)) %>%
      summarise(value=sum(count)) %>%
      ungroup() %>%
      mutate(ratio=value/max(value))
    
    p_data %>% 
      filter(country != "World") %>% 
      filter(year >= input$period[1], year <= input$period[2]) %>% 
      gather(variable, value, -country, -year) %>% 
      # filter(variable == input$variable) %>% 
      inner_join(u_variableDefinition, by = "variable") %>% 
      filter(type != "x") %>% 
      mutate(variable = factor(variable, 
                               levels=lvls, 
                               ordered=TRUE)) %>% 
      group_by(country,variable) %>% 
      mutate(count=ifelse(is.na(value), 0, 1)) %>% 
      summarise(value=sum(count)) %>% 
      ungroup() %>% 
      spread(variable,value) %>% 
      inner_join(tmp %>% rename(sum=value), by=c("country")) %>% 
      filter(ratio == 1)
    
  })
  
  output$MissingTable1 <- renderTable({
    
    lvls <- paste((u_variableDefinition %>% filter(type != "x"))$variable)
    
    tmp = p_data %>%
      filter(country != "World") %>%
      filter(year >= input$period[1], year <= input$period[2]) %>%
      gather(variable, value, -country, -year) %>%
      # filter(variable == input$variable) %>%
      inner_join(u_variableDefinition, by = "variable") %>%
      filter(type != "x") %>%
      mutate(variable = factor(variable,
                               levels=lvls,
                               ordered=TRUE)) %>%
      group_by(country) %>%
      mutate(count=ifelse(is.na(value), 0, 1)) %>%
      summarise(value=sum(count)) %>%
      ungroup() %>%
      mutate(ratio=value/max(value))
    
    p_data %>% 
      filter(country != "World") %>% 
      filter(year >= input$period[1], year <= input$period[2]) %>% 
      gather(variable, value, -country, -year) %>% 
      # filter(variable == input$variable) %>% 
      inner_join(u_variableDefinition, by = "variable") %>% 
      filter(type != "x") %>% 
      mutate(variable = factor(variable, 
                               levels=lvls, 
                               ordered=TRUE)) %>% 
      group_by(country,variable) %>% 
      mutate(count=ifelse(is.na(value), 0, 1)) %>% 
      summarise(value=sum(count)) %>% 
      ungroup() %>% 
      spread(variable,value) %>% 
      inner_join(tmp %>% rename(sum=value), by=c("country")) %>% 
      filter(ratio < 1, ratio >= (1-input$ratio))
    
  })
  
  output$MissingTable2 <- renderTable({
    
    lvls <- paste((u_variableDefinition %>% filter(type != "x"))$variable)
    
    tmp = p_data %>%
      filter(country != "World") %>%
      filter(year >= input$period[1], year <= input$period[2]) %>%
      gather(variable, value, -country, -year) %>%
      # filter(variable == input$variable) %>%
      inner_join(u_variableDefinition, by = "variable") %>%
      filter(type != "x") %>%
      mutate(variable = factor(variable,
                               levels=lvls,
                               ordered=TRUE)) %>%
      group_by(country) %>%
      mutate(count=ifelse(is.na(value), 0, 1)) %>%
      summarise(value=sum(count)) %>%
      ungroup() %>%
      mutate(ratio=value/max(value))
    
    p_data %>% 
      filter(country != "World") %>% 
      filter(year >= input$period[1], year <= input$period[2]) %>% 
      gather(variable, value, -country, -year) %>% 
      # filter(variable == input$variable) %>% 
      inner_join(u_variableDefinition, by = "variable") %>% 
      filter(type != "x") %>% 
      mutate(variable = factor(variable, 
                               levels=lvls, 
                               ordered=TRUE)) %>% 
      group_by(country,variable) %>% 
      mutate(count=ifelse(is.na(value), 0, 1)) %>% 
      summarise(value=sum(count)) %>% 
      ungroup() %>% 
      spread(variable,value) %>% 
      inner_join(tmp %>% rename(sum=value), by=c("country")) %>% 
      filter(ratio < (1-input$ratio))
    
  })
  
  
})