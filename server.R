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
  observe(label="observe_Panel1_defineVariables", {
    if (!is.null(input$variableDefinition)) {
      u_variableDefinition <<- hot_to_r(input$variableDefinition)
      
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
    } else {
      if (is.null(values[["u_variableDefinition"]])) {
        u_variableDefinition <<- u_variableDefinition
        
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
      } else {
        u_variableDefinition <<- values[["u_variableDefinition"]]
        
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
      }
    }
    
    # Update variable choice
    choice_vars        <<- as.list(u_variableDefinition$variable[which(u_variableDefinition$type != "x")])
    names(choice_vars) <<- u_variableDefinition$variable[which(u_variableDefinition$type != "x")]
  })
  
  observeEvent(label="observe_Panel1_ApplyLogTransform", input$data_log, {
    if (DEBUG) print(paste0("[DEBUG] data_log has been changed! ", input$data_log))
    
    if (input$data_log) {
      u_variableDefinition$transform[which(u_variableDefinition$type %in% c("d", "n", "s", "f"))] = "log"
      
      if (DEBUG) {
        print("[DEBUG] Showing variable definition table:")
        print(head(u_variableDefinition))
        print(str(u_variableDefinition))
      }
      
      output$variableDefinition <- renderRHandsontable({
        generate_varTable(u_variableDefinition)
      })
    } else {
      u_variableDefinition$transform[which(u_variableDefinition$type %in% c("d", "n", "s", "f"))] = ""
      
      if (DEBUG) {
        print("[DEBUG] Showing variable definition table:")
        print(head(u_variableDefinition))
        print(str(u_variableDefinition))
      }
      
      output$variableDefinition <- renderRHandsontable({
        generate_varTable(u_variableDefinition)
      })
    }
  })
  
  observeEvent(label="observe_Panel1_ApplyDemean", input$data_demean, {
    if (DEBUG) print(paste0("[DEBUG] data_demean has been changed! ", input$data_demean))
    
    if (input$data_demean) {
      u_variableDefinition$demean[which(u_variableDefinition$type %in% c("d", "n", "s", "f"))] = TRUE
      
      if (DEBUG) {
        print("[DEBUG] Showing variable definition table:")
        print(head(u_variableDefinition))
        print(str(u_variableDefinition))
      }
      
      output$variableDefinition <- renderRHandsontable({
        generate_varTable(u_variableDefinition)
      })
    } else {
      u_variableDefinition$demean[which(u_variableDefinition$type %in% c("d", "n", "s", "f"))] = FALSE
      
      if (DEBUG) {
        print("[DEBUG] Showing variable definition table:")
        print(head(u_variableDefinition))
        print(str(u_variableDefinition))
      }
      
      output$variableDefinition <- renderRHandsontable({
        generate_varTable(u_variableDefinition)
      })
    }
  })
  
  observeEvent(label="observe_Panel1_ApplyFirstDiff", input$data_firstdiff, {
    print(paste0("[DEBUG] data_firstdiff has been changed! ", input$data_firstdiff))
    
    if (input$data_firstdiff) {
      u_variableDefinition$firstdiff[which(u_variableDefinition$type %in% c("d", "n", "s", "f"))] = TRUE
      
      if (DEBUG) {
        print("[DEBUG] Showing variable definition table:")
        print(head(u_variableDefinition))
        print(str(u_variableDefinition))
      }
      
      output$variableDefinition <- renderRHandsontable({
        generate_varTable(u_variableDefinition)
        
      })
    } else {
      u_variableDefinition$firstdiff[which(u_variableDefinition$type %in% c("d", "n", "s", "f"))] = FALSE
      
      if (DEBUG) {
        print("[DEBUG] Showing variable definition table:")
        print(head(u_variableDefinition))
        print(str(u_variableDefinition))
      }
      
      output$variableDefinition <- renderRHandsontable({
        generate_varTable(u_variableDefinition)
      })
    }
  })
  
  # Panel 3: Generate regression tree
  observeEvent(input$treemod, {
    if (input$advOpts %% 2) {
      toggle("tree_search")
      toggle("tree_splitfrac")
    }
  })
  observeEvent(input$advOpts, {
    if (input$advOpts %% 2) {
      toggle("tree_nbcv")
      toggle("tree_cvtype")
      toggle("tree_se")
      if (input$treemod == "Single tree > LS - Constant")  showElement("tree_search")
      if (input$treemod == "Single tree > LMS - Constant") showElement("tree_splitfrac")   
      toggle("tree_maxsplit")
      toggle("tree_minnode")  
    } else {
      toggle("tree_nbcv")
      toggle("tree_cvtype")
      toggle("tree_se")
      hideElement("tree_search")
      hideElement("tree_splitfrac")   
      toggle("tree_maxsplit")
      toggle("tree_minnode")
    }
  })
  
  observeEvent(label="observe_Panel3_GenerateRegressionTree", input$run, {
    # Prepare input data
    cat("\nPreparing data...\n")
    v_dataProc <- data_prepare(p_data, u_variableDefinition, input$period[1], input$period[2], DEMEAN=input$data_demean, FIRSTDIFF=input$data_firstdiff)
    
    # Redefine variable definition table
    v_variableDefinition <<- u_variableDefinition 
    v_variableDefinition$variable <<- factor(v_variableDefinition$variable, 
                                             levels=paste(get_renamedVariables(u_variableDefinition, ALL=TRUE)$old), 
                                             labels=paste(get_renamedVariables(u_variableDefinition, ALL=TRUE)$new))
    
    if (DEBUG) {
      print("[DEBUG] Showing v_variableDefinition:")
      print(head(v_variableDefinition))
    }
    
    # Write data
    cat("\nWriting out files...\n")
    if (DEBUG) print(paste0("[DEBUG] Showing input$treemod: ", input$treemod))
    tree_opts   <- data.frame(
      nbcv       = input$tree_nbcv,
      cvtype     = ifelse(input$tree_cvtype == "Mean-based CV tree", "1", "2"),
      sevalue    = input$tree_se,
      splitfrac  = input$tree_splitfrac,
      searchtype = ifelse(input$tree_search == "Split point from quantiles", "1", "2"),
      maxsplits  = input$tree_maxsplit,
      minnbnodes = input$tree_minnode
    )
    if (DEBUG) {
      print("[DEBUG] Showing tree_opts:")
      print(tree_opts)
    }
    v_dataWrite <- data_write(v_dataProc, v_variableDefinition, input$treemod, tree_opts, v_fpath, v_fname)
    
    # Run GUIDE
    cat("\nRunning GUIDE...\n")
    runGUIDE(v_fpath, v_fname, SHOW_LOG=TRUE)
    
    # Plot tree
    cat("Plotting regression tree...\n")
    cat("  > Parsing GUIDE output...\n")
    v_tree  <<- parse_GUIDEOutput(v_fpath$out)
    
    if (DEBUG) {
      print("[DEBUG] Showing v_tree:")
      print(head(v_tree))
    }
    
    cat("  > Allocating data to nodes...\n")
    v_alloc <<- allocateDataToNodes(v_dataProc, v_tree)
    
    if (DEBUG) {
      print("[DEBUG] Showing v_alloc:")
      print(head(v_alloc))
    }
    
    cat("  > Rendering tree...\n")      
    output$generateTree <- renderPlotly({
      
      plotly.tree(v_tree, v_alloc)
    
    })
    output$plotResiduals1 <- renderPlot({
      plotNodeResiduals(v_alloc, v_tree, paste(v_variableDefinition$variable[which(v_variableDefinition$type == "d")]))
    })
    output$plotResiduals2 <- renderPlot({
      plotNodeResiduals(v_alloc, v_tree, paste(v_variableDefinition$variable[which(v_variableDefinition$type == "d")]), HISTOGRAM=TRUE)
    })
  })
  
  
  # Panel 6: Node transitions
  observeEvent(input$updateNodeTransition, {
    cat("Updating transition node plot...\n")  
    output$TransitionNodePlot <- renderPlot({
      plotTransitionTNodes(v_alloc, v_tree)
    })
    output$ui_TransitionNodePlot <- renderUI({
      plotOutput("TransitionNodePlot", height = "800px", width = "100%")
    })
  })
  
  
  #============ UI =========================
  # Panel 1: Define variables
  output$variableDefinition <- renderRHandsontable({
    generate_varTable(u_variableDefinition)
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
    
    select_countries = c("Brazil", "China", "France", "Germany", "India", "Indonesia", "Japan", "Mozambique", "Russia", "United States of America")
    
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
                   choices = list("Single tree > LMS - Constant"="Single tree > LMS - Constant",
                                  "Single tree > LS - Constant"="Single tree > LS - Constant"),
                   selected = "Single tree > LMS - Constant",
                   multiple=FALSE,
                   options = list(maxItems = 2, placeholder = 'Select tree model'))
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
    
    choice_countries <- sort(unique(v_alloc$country))
    
    select_country <- ifelse("China" %in% choice_countries, "China", choice_countries[1])
    
    output$countryName <- renderText({ return(select_country) })
    
    selectizeInput("country2", 
                   label    = h3("Country:"), 
                   choices  = choice_countries,
                   selected = select_country, 
                   multiple = FALSE,
                   options  = list(maxItems = 1, placeholder = 'Select countries'))
    
  })
  output$chooseVarX     <- renderUI({
    
    choice_varX        <- as.list(v_variableDefinition$variable[which(v_variableDefinition$type != "x")])
    names(choice_varX) <- v_variableDefinition$variable[which(v_variableDefinition$type != "x")]
    
    select_VarX <- v_variableDefinition$variable[which(v_variableDefinition$type == "n")[1]]
    
    selectizeInput("varX",
                   label = h3("Variable (X):"),
                   choices = choice_varX,
                   selected = select_VarX,
                   multiple=FALSE,
                   options = list(maxItems = 1, placeholder = 'Select variable'))
  })
  output$chooseVarY     <- renderUI({
    
    choice_varY        <- as.list(v_variableDefinition$variable[which(v_variableDefinition$type != "x")])
    names(choice_varY) <- v_variableDefinition$variable[which(v_variableDefinition$type != "x")]
    
    select_VarY <- v_variableDefinition$variable[which(v_variableDefinition$type == "n")[2]]
      
      selectizeInput("varY",
                     label = h3("Variable (Y):"),
                     choices = choice_varY,
                     selected = select_VarY,
                     multiple=FALSE,
                     options = list(maxItems = 1, placeholder = 'Select variable'))
  })
  output$chooseVarC     <- renderUI({
    
    choice_varC        <- as.list(v_variableDefinition$variable[which(v_variableDefinition$type != "x")])
    names(choice_varC) <- v_variableDefinition$variable[which(v_variableDefinition$type != "x")]
    
    select_VarC <- v_variableDefinition$variable[which(v_variableDefinition$type == "d")]
    
    selectizeInput("varC",
                   label = h3("Variable (Color):"),
                   choices = choice_varC,
                   selected = select_VarC,
                   multiple=FALSE,
                   options = list(maxItems = 1, placeholder = 'Select variable'))
  })
  
  
  #============ TEXT =========================
  # Panel 1: Define variables
  output$GUIDEversion <- renderText({
    out    <- ""
    tryCatch(
      ver_ws <<- checkWebsiteVersion(),
      error = function(e) {
        ver_ws <<- NULL
      }
    )
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
      if (DEBUG) print(paste("[DEBUG] ver_ws:", ver_ws))
      if (DEBUG) print(paste("[DEBUG] ver_lo:", ver_lo))
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
    if (DEBUG) print("[DEBUG] Generate plot data (original)")
    v_plotOriginal <- p_data %>% 
      filter(country %in% input$country) %>% 
      filter(year >= input$period[1], year <= input$period[2]) %>% 
      gather(variable, value, -country, -year) %>% 
      inner_join(u_variableDefinition, by = "variable") %>% 
      filter(type != "x") %>% 
      filter(variable %in% input$variable) %>% 
      mutate(variable = paste0(input$variable, " (Original)")) %>% 
      mutate(variable = factor(variable))
    
    if (DEBUG) print("[DEBUG] Plot data (original)")
    p1 = ggplot(v_plotOriginal) + 
      geom_line(aes(x=year,y=value,color=country)) + 
      geom_point(aes(x=year,y=value,color=country)) +
      facet_wrap(~variable, scales = "free_y") +
      theme_bw() +
      xlab("") +
      theme(legend.position="none")
    
    # Processed data
    if (DEBUG) print("[DEBUG] Generate plot data (processed)")
    v_plotProcessed <- p_data %>%
      filter(country %in% input$country) %>%
      filter(year >= input$period[1], year <= input$period[2]) %>%
      gather(variable, value, -country, -year) %>%
      inner_join(u_variableDefinition, by = "variable") %>%
      filter(type != "x") %>%
      filter(variable %in% input$variable) %>%
      mutate(variable = paste0(input$variable, " (Processed)")) %>% 
      mutate(variable = factor(variable)) %>% 
      mutate(value=value*factor) 

      tmp_numVars = (u_variableDefinition %>%
                       filter(type %in% c("d", "n", "f", "s"),
                              transform != "") %>%
                       select(variable)
      )$variable %>%
      paste()

    if (input$data_log) {
      if (DEBUG) print("[DEBUG] Log-transform numerical variables (processed)")
      for (kvar in tmp_numVars) {
        # Logarithm
        if (u_variableDefinition$transform[which(u_variableDefinition$variable == kvar)] == "log") {
          # Apply transformation
          v_plotProcessed$value[which(v_plotProcessed$variable == kvar)] <- log(v_plotProcessed$value[which(v_plotProcessed$variable == kvar)])
        }
      }
    }

    if (input$data_demean) {
      if (DEBUG) print("[DEBUG] Demean numerical variables (processed)")
      for (kvar in tmp_numVars) {
        v_plotProcessed <- v_plotProcessed %>%
          group_by(country,variable) %>%
          mutate(value=ifelse(type %in% c("d","n","s","f"), value-mean(value), value)) %>%
          ungroup()
      }
    }
      
    if (input$data_firstdiff) {
      if (DEBUG) print("[DEBUG] Apply first-differences to numerical variables (processed)")
      for (kvar in tmp_numVars) {
        v_plotProcessed <- v_plotProcessed %>%
          group_by(country,variable) %>%
          arrange(year) %>% 
          mutate(value=ifelse(type %in% c("d","n","s","f"), value-lag(value), value)) %>%
          ungroup()
      }
    }

    if (DEBUG) print("[DEBUG] Plot data (Processed)")
    p2 = ggplot(v_plotProcessed) + 
      geom_line(aes(x=year,y=value,color=country)) + 
      geom_point(aes(x=year,y=value,color=country)) +
      facet_wrap(~variable, scales = "free_y") +
      theme_bw() +
      xlab("")  +
      theme(legend.position="none")
    
    if (input$ylog) {
      p1 = p1 + scale_y_log10()
      p2 = p2 + scale_y_log10()
    }
    
    if (DEBUG) print("[DEBUG] Combine plots (original + processed)")
    subplot(
      ggplotly(p1), # Original data
      ggplotly(p2)) # Processed data
    
  })
  
  # Panel 3: Generate interactive regression tree
  output$generateTree <- renderPlotly({
    
    if(exists("v_tree")) {
      plotly.tree(v_tree, v_alloc)
    } else {
      p = plot(0,0, type = "n")
      p
    }
    
  })
  output$plotResiduals1 <- renderPlot({
    plotNodeResiduals(v_alloc, v_tree, paste(v_variableDefinition$variable[which(v_variableDefinition$type == "d")]))
  })
  output$plotResiduals2 <- renderPlot({
    plotNodeResiduals(v_alloc, v_tree, paste(v_variableDefinition$variable[which(v_variableDefinition$type == "d")]), HISTOGRAM=TRUE)
  })
  
  # Panel 4: Explore results (Country pathways)
  output$countryPathwayPlot <- renderPlot({
    
    pathway <- getCountryPathway(v_alloc, input$country2)
    if (DEBUG) {
      print("[DEBUG] Get country pathway:")
      print(pathway)
    }
    
    plotCountryPathway(pathway, v_tree)
    
    
    
  })
  output$scatterCountryPlot <- renderPlot({
    plotScatterCountry(v_alloc, v_tree, input$country2, input$varX, input$varY, input$varC, PLOT_LABELS=TRUE)
  })
  output$ui_countryPathwayPlot <- renderUI({
    plotOutput("countryPathwayPlot", height = "500px", width = "100%")
  })
  output$ui_scatterCountryPlot <- renderUI({
    plotOutput("scatterCountryPlot", height = "500px", width = "100%")
  })
  
  # Panel 5: Explore results (T-node)
  plot_height <- function() {
    # calculate height as a function of the number of countries belonging to the Tnode
    height <- 80*length(unique(v_alloc$country[which(v_alloc$tnode == input$tnode)]))
    return(height)
  }
  output$TnodePlot <- renderPlot({
    
    plot_TNode(v_alloc, input$tnode, v_tree, PLOT_MAP=FALSE)
    
  })
  output$ui_TNodePlot <- renderUI({
    plotOutput("TnodePlot", height = plot_height(), width = "100%")
  })
  
  # Panel 6: Explore results (Node transitions)
  output$TransitionNodePlot <- renderPlot({
    
    plotTransitionTNodes(v_alloc, v_tree)
    
  })
  output$ui_TransitionNodePlot <- renderUI({
    plotOutput("TransitionNodePlot", height = "800px", width = "100%")
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