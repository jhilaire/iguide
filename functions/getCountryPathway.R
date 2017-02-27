getCountryPathway <- function(i_alloc, i_country) {
  v = i_alloc$tnode[which(i_alloc$country == i_country)]
  names(v) = i_alloc$year[which(i_alloc$country == i_country)]
  v = v[order(names(v))]
  
  v2 = i_alloc$tnode[which(i_alloc$country == i_country)][which(duplicated(i_alloc$tnode[which(i_alloc$country == i_country)]) == FALSE)]
  names(v2) = names(v)[which(duplicated(i_alloc$tnode[which(i_alloc$country == i_country)]) == FALSE)]
  names(v2) <- paste0(as.numeric(names(v2)), "-", c((as.numeric(names(v2))-lag(as.numeric(names(v2))))[2:length(as.numeric(names(v2)))]-1, 0) + as.numeric(names(v2)))
  names(v2)[length(v2)] <- paste0(strsplit(names(v2)[length(v2)], "-")[[1]][1], "-", max(as.numeric(names(v))))
  
  pathway = list()
  kcnt = 1
  for (knode in unique(v)) {
    
    ids = which(v == knode)
    
    lags = as.numeric(names(v[which(v == knode)])) - lag(as.numeric(names(v[which(v == knode)])), default = min(as.numeric(names(v[which(v == knode)])))-1)
    cuts = which(lags != 1)
    
    startid = min(ids)
    endid   = max(ids)
    
    if (length(cuts) != 0) {
      for (kcut in 1:length(cuts)) {
        endid = ids[cuts[kcut]-1]
        pathway[[kcnt]] <- 
          data.frame(start=as.numeric(names(v)[startid]), end=as.numeric(names(v)[endid]), period=paste0(names(v)[startid],"-",names(v)[endid]), node=knode)
        startid = ids[cuts[kcut]]
        kcnt = kcnt +1
      }
      endid   = max(ids)
      pathway[[kcnt]] <- 
        data.frame(start=as.numeric(names(v)[startid]), end=as.numeric(names(v)[endid]), period=paste0(names(v)[startid],"-",names(v)[endid]), node=knode)
    } else {
      pathway[[kcnt]] <- 
        data.frame(start=as.numeric(names(v)[startid]), end=as.numeric(names(v)[endid]), period=paste0(names(v)[startid],"-",names(v)[endid]), node=knode)
    }
    kcnt = kcnt +1
    
  }
  
  pathway = do.call("rbind", pathway) %>% 
    mutate(type=ifelse(start == end, "S", "M"))
  pathway %<>% left_join(
    pathway %>% 
      filter(!duplicated(node)) %>% 
      mutate(pos=1:length(unique(node))) %>% 
      select(node, pos),
    by=c("node")
  )
  
  return(pathway)
}
