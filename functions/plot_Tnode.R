plot_TNode <- function(i_alloc, i_node, i_tree, PLOT_MAP=FALSE) {
  
  tmp <- i_alloc %>% 
    filter(tnode == i_node) %>% 
    group_by(country) %>% 
    arrange(year) %>% 
    ungroup() 
  
  tmp_bis <- i_alloc %>% 
    filter(tnode == i_node) %>% 
    group_by(country) %>% 
    arrange(year) %>% 
    ungroup() %>% 
    group_by(country) %>% 
    mutate(yd = year-lag(year, default=min(year)-1)) %>% mutate(jump=ifelse(yd != 1, 1, 0)) %>% 
    mutate(groups = cumsum(jump)) %>% 
    select(-yd, -jump) %>% 
    ungroup()
  
  tmp2 <- i_alloc %>% 
    filter(country %in% tmp$country) %>% 
    left_join(
      tmp %>% 
        select(country, year) %>% 
        group_by(country) %>% 
        mutate(year_min=min(year)) %>% 
        mutate(year_max=max(year)) %>% 
        ungroup() %>% 
        select(-year),
      by=c("country")
    ) %>% 
    group_by(country) %>% 
    arrange(year) %>% 
    ungroup()
  
  tmp3 = data.frame(
    country  = sort(unique(tmp$country)),
    ypos_pt  = 1:length(unique(tmp$country)),
    ypos_lab = 1:length(unique(tmp$country)) + 0.2
  )
  
  tmp  = tmp %>% left_join(tmp3, by=c("country"))
  tmp_bis  = tmp_bis %>% left_join(tmp3, by=c("country")) %>% 
    unite("grps", country, groups, remove=FALSE)
  tmp2 = tmp2 %>% left_join(tmp3, by=c("country"))
  
  tmp5 = tmp %>% 
    select(country, year) %>% 
    group_by(country) %>% 
    mutate(yd = year-lag(year, default=min(year)-1)) %>% mutate(jump=ifelse(yd != 1, 1, 0)) %>% 
    mutate(groups = cumsum(jump)) %>% 
    select(-yd, -jump) %>% 
    ungroup() %>% 
    group_by(country,groups) %>% 
    summarise(year_min=min(year), year_max=max(year)) %>% 
    ungroup()
  
  
  p1 = ggplot(data=tmp_bis) +
    geom_path(aes(x=year, y=ypos_pt, group=country), data=tmp2, colour="lightgrey", alpha=0.33) +
    geom_line(aes(x=year, y=ypos_pt, group=grps), colour="black") +
    geom_point(aes_string(x="year", y="ypos_pt", fill="log_co2_terr_CDIAC_pc", colour="log_co2_terr_CDIAC_pc"), data=tmp2, pch=21, size=1.2, alpha=0.33) +
    geom_point(aes_string(x="year", y="ypos_pt", fill="log_co2_terr_CDIAC_pc", colour="log_co2_terr_CDIAC_pc"), pch=21, size=3) +
    geom_text(aes(x=year, y=ypos_lab, label=tnode), data=tmp2, size=3, colour="lightgrey") +
    geom_text(aes(x=year, y=ypos_lab, label=tnode), data=tmp_bis, size=3) +
    theme_bw() +
    theme(legend.position="bottom") + 
    ggtitle(paste0("T-Node ID: ", i_node, " - Mean: ", i_tree$value[i_tree$nodeID == i_node]," - (", length(tmp3$country), " countries)")) +
    scale_y_continuous(breaks=tmp3$ypos_pt, labels=tmp3$country) + 
    xlab("") + ylab("") +
    #xlim(min(tmp$year), max(tmp$year)) +
    scale_x_continuous(breaks=seq(min(tmp2$year), max(tmp2$year), 5))  
  
  p1 = p1 +
    scale_colour_gradient(low = "#F5F5DC", high = "#8B0000", space = "Lab", na.value = "grey50", guide = "colourbar") +
    scale_fill_gradient(low = "#F5F5DC", high = "#8B0000", space = "Lab", na.value = "grey50", guide = "colourbar")
  
  #print(p)
  
  
  if (PLOT_MAP) {
    require(maps)
    world_map<-map_data("world")
    tmp4 = world_map %>% 
      left_join(
        tmp3 %>% 
          select(country) %>% 
          mutate(value=1), 
        by=c("region"="country")) %>% 
      mutate(value=ifelse(is.na(value), 0, value))
    p2 = ggplot() +
      geom_map(aes(map_id=region), fill = "lightgrey", data = tmp4 %>% filter(value == 0), map = world_map) + 
      geom_map(aes(map_id=region), fill = "red", data = tmp4 %>% filter(value == 1), map = world_map) + 
      expand_limits(x = world_map$long, y = world_map$lat) +
      theme_bw() +
      scale_fill_manual(values = c("lightgrey","red")) +
      xlab("") + ylab("")  + 
      theme(axis.line=element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            legend.position="none",
            panel.background=element_blank(),
            panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            plot.background=element_blank())
    #print(p2)
  }
  
  tmp6 = i_alloc %>% 
    filter(country %in% tmp$country) %>% 
    select(country, year, tnode) %>% 
    left_join(tmp5 %>% select(country,groups,year_min), by=c("country")) %>% 
    group_by(country,groups) %>% 
    arrange(year) %>% 
    mutate(ym1 = lead(year)) %>% 
    ungroup() %>% 
    filter(ym1 == year_min) %>% 
    select(country,year,tnode) %>% 
    mutate(type="Previous")
  tmp7 = i_alloc %>% 
    filter(country %in% tmp$country) %>% 
    select(country, year, tnode) %>% 
    left_join(tmp5 %>% select(country,groups,year_max), by=c("country")) %>% 
    group_by(country,groups) %>% 
    arrange(year) %>% 
    mutate(yp1 = lag(year)) %>% 
    ungroup() %>% 
    filter(yp1 == year_max) %>% 
    select(country,year,tnode) %>% 
    mutate(type="Next")
  
  p3 = ggplot(data=rbind(tmp6,tmp7) %>%
                mutate(type=factor(type, levels=c("Previous", "Next"), labels=c("From prev. T-node", "To next T-node"), ordered=TRUE)) %>% 
                group_by(type,tnode) %>% 
                summarise(n = n()) %>% 
                mutate(freq=n/sum(n)*100) %>% 
                ungroup() %>% 
                mutate(tnode=factor(tnode))) +
    geom_bar(aes(x=tnode, y=freq, fill=tnode), stat="identity") +
    facet_wrap(~type,ncol=2) +
    theme_bw() +
    xlab("T-node") + ylab("Probability of occurence (%)") +
    ggtitle("Probability of transition") +
    theme(legend.position="none")
  #print(p3)
  
  if (PLOT_MAP) {
    layout <- matrix(c(1, 1, 2, 3), nrow = 2, byrow = TRUE)
    multiplot(p1,p2,p3, layout=layout)
  } else {
    multiplot(p1,p3)
  }

}
