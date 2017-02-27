filter_NA <- function(i_data, i_vardef) {
  # Select independent and dependent variables of current model (x means exclude)
  varnames <- paste((i_vardef %>% filter(type != "x"))$variable)
  
  # Count number of NA values for each couple (country, variable)
  data_NA_country_variable <- i_data %>% 
    gather(variable, value, -country, -year) %>%    # Switch to long format
    inner_join(i_vardef, by = "variable") %>%       # Join with variable definition table
    filter(type != "x") %>%                         # Remove excluded variables
    mutate(variable = factor(variable, 
                             levels=varnames, 
                             ordered=TRUE)) %>%     # Re-factor variables
    group_by(country,variable) %>%                  # For each couple (country, variable)
    mutate(count=ifelse(is.na(value), 0, 1)) %>%    #  . count number of NA values
    summarise(value=sum(count)) %>%                 #  . and sum them up
    ungroup() %>%                                   #
    spread(variable,value)                          # Switch back to wide format
  
  # Count number of NA values for each country
  data_NA_country <- dataStats %>% 
    gather(variable,value,-country) %>% 
    group_by(country) %>% 
    summarize(value=sum(value)) %>% 
    ungroup()
  
  # Select countries with the minimum number of NAs
  country_minNA <- paste(data_NA_country$country[which(data_NA_country$value == max(data_NA_country$value))])
  
  out <- i_data %>% filter(country %in% country_minNA)
  
  return(out)
}

filter_X  <- function(i_data, i_vardef) {
  
  # Get selected variable names 
  varnames <- paste(i_vardef$variable[which(i_vardef$type != "x")])
  
  # Select variables
  i_data <- i_data[,c("country","year",varnames)]
  
  return(i_data)
}

rescaleVariable   <- function(i_data, i_vardef) {
  # Get names of numerical variables
  numVarnames <- paste((i_vardef %>% filter(type %in% c("d", "n", "f", "s"), factor != 1))$variable)
  
  # Loop over variables and apply factor
  for (kvar in numVarnames) {
    tmp_factor <- as.numeric(i_vardef$factor[which(i_vardef$variable == kvar)])
    cat("    - ", kvar, " <- ", kvar, "*", paste(tmp_factor), "\n")
    i_data[,kvar] <- i_data[,kvar] * tmp_factor
  }
  
  return(i_data)
}

transformVariable <- function(i_data, i_vardef) {
  # Get names of numerical variables
  numVarnames <- paste((i_vardef %>% filter(type %in% c("d", "n", "f", "s")))$variable)
  
  # Loop over variables and apply transforming function
  for (kvar in numVarnames) {
    # Logarithm
    if (i_vardef$transform[which(i_vardef$variable == kvar)] == "log") {
      cat("    - ", kvar, " <- log(", kvar, ")\n")
      # Apply transformation
      i_data[,kvar] <- log(i_data[,kvar])
    }
    # Other functions (TODO)...
  }
  
  return(i_data)
}

applyFirstDiff <- function(i_data, i_vardef) {
  # Get names of numerical variables
  numVarnames <- paste((i_vardef %>% filter(type %in% c("d", "n", "f", "s")))$variable)
  
  # Loop over variables and apply transforming function
  for (kvar in numVarnames) {
    i_data <- i_data %>% 
      group_by(country) %>% 
      arrange(year) %>% 
      firstdiff(kvar) %>% 
      ungroup()
  }
  
  return(i_data)
}

demeanVariable <- function(i_data, i_vardef) {
  # Get names of numerical variables
  numVarnames <- paste((i_vardef %>% filter(type %in% c("d", "n", "f", "s")))$variable)
  
  # Loop over variables and demean
  for (kvar in numVarnames) {
    cat("    - ", kvar, "<-", kvar, "- mean(", kvar, ")\n")
    i_data <- i_data %>% 
      group_by(country) %>% 
      demean(kvar) %>% 
      ungroup()
  }
  
  return(i_data)
}

get_renamedVariables <- function(i_vardef) {
  # Initialise
  v_variableRenaming = data.frame()
  
  # Get names of numerical variables
  numVarnames <- paste((i_vardef %>% filter(type %in% c("d", "n", "f", "s")))$variable)
  
  # Loop over variables and apply transforming function
  for (kvar in numVarnames) {
    # Logarithm
    if (i_vardef$transform[which(i_vardef$variable == kvar)] == "log") {
      cat("    - ", kvar, " -> ", paste0("log_",kvar), "\n")
      # Rename variable
      v_variableRenaming <- rbind(v_variableRenaming,
                                  data.frame(old=kvar, new=paste0("log_",kvar)))
    }
    # Other functions (TODO)...
  }
  
  return(v_variableRenaming)
}

renameVariable <- function(i_data, i_vardef) {
  
  v_variableRenaming <- get_renamedVariables(i_vardef)
  
  names(i_data) <- paste(sapply(names(i_data), function(x) {
    ifelse(length(which(v_variableRenaming$old == x)) != 0,                  
           paste(v_variableRenaming$new[which(v_variableRenaming$old == x)]),
           x)
  }))
  
  return(i_data)
}

data_prepare <- function(i_data, i_vardef, period_start, period_end, FIRSTDIFF=FALSE, DEMEAN=TRUE) {
  
  # Filter years (and remove World data)
  cat("> Filtering  years...\n")
  i_data %<>% filter(country != "World") %>% 
    filter(year >= period_start, year <= period_end)
  
  # Select variables which have not been excluded
  cat("> Removing excluded variable from data (keep country and year)...\n")
  i_data %<>% filter_X(i_vardef)
  
  # Select countries with the minimum number of NAs
  cat("> Selecting countries with minimum number of NAs...\n")
  i_data %<>% filter_NA(i_vardef)
  
  # Rescale variables
  cat("> Rescaling numerical variables...\n")
  i_data %<>% rescaleVariable(i_vardef)
 
  # Transform variables
  cat("> Transforming numerical variables...\n")
  i_data %<>% transformVariable(i_vardef)

  # Apply first differences to variables
  if (FIRSTDIFF) {
    cat("> Aplying first differences to variables...\n")
    i_data %<>% applyFirstDiff(i_vardef)
  }
    
  # Demean variables
  if (DEMEAN) {
    cat("> Demeaning numerical variables...\n")
    i_data %<>% demeanVariable(i_vardef)
  }

  # Rename transformed variables in data
  cat("> Renaming transformed variables...\n")
  i_data %<>% renameVariable(i_vardef)
  
  return(i_data)
}