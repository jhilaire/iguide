plotNodeResiduals <- function(i_alloc, i_nodeInfo, i_depvar, HISTOGRAM=FALSE, CUT=0.05) {

  cut_threshold = CUT
  
  # i_alloc = v_alloc
  # i_nodeInfo = v_tree
  # i_depvar = paste(v_variableDefinition$variable[which(v_variableDefinition$type == "d")])

  residuals <- lapply(unique(i_nodeInfo$nodeID[which(i_nodeInfo$nodeType == "Terminal node")]),
       function(x) {
         tmp <- i_alloc[which(i_alloc$tnode == x),
                 which(names(i_alloc) %in% c("country", "year", i_depvar, "tnode"))] %>%
           as.data.frame() %>% 
           rename_("value" = i_depvar) %>% 
           mutate(regval   = i_nodeInfo$value[which(i_nodeInfo$nodeID == x)]) %>% 
           mutate(residual = value - regval) %>% 
           select(tnode, country, year, value, regval, residual)
         return(tmp)
       })
  
  residuals <- do.call("rbind", residuals) %>% 
    mutate(tnode=factor(tnode))
  
  if (!HISTOGRAM) {
    p = ggplot(residuals) + 
      geom_point(aes(x=tnode, y=value)) +
      geom_point(aes(x=tnode, y=mean), data=residuals %>% group_by(tnode) %>% summarise(mean=mean(value)) %>% ungroup(), shape=3, color="red") +
      geom_point(aes(x=tnode, y=median), data=residuals %>% group_by(tnode) %>% summarise(median=median(value)) %>% ungroup(), shape=4, color="blue") +
      theme_bw() +
      ylab("") + ylab(i_depvar)
    
  } else {
  
    tmp <- residuals %>% left_join(
      residuals %>% 
        select(tnode, value) %>% 
        group_by(tnode) %>% 
        summarise(
          mean   = round(mean(value), digits=3),
          median = round(median(value), digits=3),
          se     = round(sd(value), digits=3),
          count  = n()) %>% 
        ungroup(),
      by=c("tnode")) %>% 
      mutate(label=factor(paste0("Node: ",tnode, " (", count, ")\n", regval, " (",se,")")))
    
    tmp2 <- tmp %>% 
      mutate(cuts = cut(tmp$value, 
                        c(-Inf,seq(floor(min(tmp$value)), ceiling(max(tmp$value)), cut_threshold), Inf))) %>% 
      group_by(tnode, cuts) %>% 
      summarise(count = n()) %>% 
      ungroup()
    
    tmp3 <- residuals %>% 
      select(tnode, value) %>% 
      group_by(tnode) %>% 
      summarise(
        mean   = round(mean(value), digits=3),
        median = round(median(value), digits=3),
        se     = round(sd(value), digits=3)) %>% 
      ungroup() %>% 
      mutate(mean_x    = mean) %>% 
      mutate(mean_xend = mean) %>% 
      mutate(mean_y    = 0) %>% 
      mutate(mean_yend = max(tmp2$count)) %>% 
      mutate(med_x    = median) %>% 
      mutate(med_xend = median) %>% 
      mutate(med_y    = 0) %>% 
      mutate(med_yend = max(tmp2$count)) %>% 
      left_join(tmp %>% select(tnode, label), by=c("tnode"))
  
    p = ggplot(tmp) +
      geom_histogram(aes(value), binwidth = cut_threshold) +
      geom_segment(aes(x=med_x, xend=med_xend, y=med_y, yend=med_yend), data=tmp3, color="blue") +
      geom_segment(aes(x=mean_x, xend=mean_xend, y=mean_y, yend=mean_yend), data=tmp3, color="red") +
      facet_wrap(~label, ncol=6, scales = "free_x") +
      theme_bw() +
      ylab("") + ylab(i_depvar)

  }
  return(p)
}
