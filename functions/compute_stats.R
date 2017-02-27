compute_stats1 <- function(i_data) {
  lvls <- paste((u_variableDefinition %>% filter(type != "x"))$variable)
  
  out <- i_data %>% 
    filter(country != "World") %>% 
    gather(variable, value, -country, -year) %>% 
    inner_join(u_variableDefinition, by = "variable") %>% 
    filter(type != "x") %>% 
    mutate(variable = factor(variable, 
                             levels=lvls, 
                             ordered=TRUE)) %>% 
    group_by(country,variable) %>% 
    mutate(count=ifelse(is.na(value), 0, 1)) %>% 
    summarise(value=sum(count)) %>% 
    ungroup() %>% 
    spread(variable,value)
  
  return(out)
} 

compute_stats2 <- function(i_data) {
  out <- i_data %>% 
    select(-co2_terr_CDIAC_pc) %>% 
    gather(variable,value,-country) %>% 
    group_by(country) %>% 
    summarize(value=sum(value)) %>% 
    ungroup()
  
  return(out)
}

compute_stats3 <- function(i_data) {
  lvls <- paste((u_variableDefinition %>% filter(type != "x"))$variable)
  
  out <- i_data %>% 
    filter(country != "World") %>% 
    filter(year >= u_startYear, year <= u_endYear) %>% 
    gather(variable, value, -country, -year) %>% 
    inner_join(u_variableDefinition, by = "variable") %>% 
    filter(type != "x") %>% 
    mutate(variable = factor(variable, 
                             levels=lvls, 
                             ordered=TRUE)) %>% 
    group_by(country,variable) %>% 
    mutate(count=ifelse(is.na(value), 0, 1)) %>% 
    summarise(value=sum(count)) %>% 
    ungroup() %>% 
    spread(variable,value)
  
  out <- out[which(apply(apply(out, 2, function(x) {x > (u_endYear - u_startYear)}), 1, all)),]
  
  return(out)
} 