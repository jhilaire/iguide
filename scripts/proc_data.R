#=================================================================================
# Process data
#=================================================================================
cat("Processing data...\n")
p_dataLong <- p_data %>% gather(variable,value,-country,-year) #,-longname,-unit,-source)

choice_countries <- as.list(levels(p_dataLong$country))
names(choice_countries) <- levels(p_dataLong$country)

choice_vars <- as.list(unique(p_dataLong$variable))
names(choice_vars) <- unique(p_dataLong$variable)
cat("\n")