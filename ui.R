#=================================================================================
# Initialisation
#=================================================================================
# Load libraries
library(shiny)
library(shinyjs)
library(rhandsontable)
library(lazyeval)
library(dplyr)
library(tidyr)
library(tools)
library(ggplot2)
library(plotly)


#=================================================================================
# Shiny client configuration
#=================================================================================
shinyUI(
  tabsetPanel(
    
    #============= PANEL1: DEFINE VARIABLES ===========================
    tabPanel(
      "Define variables", 
      pageWithSidebar(
        
        headerPanel("Define variables"),
        sidebarPanel(
          textOutput("GUIDEversion"),
          tags$head(tags$style("#GUIDEversion{color: red;
                               font-size: 12px;
                               font-style: italic;
                               }"
                         )
          ),
          helpText("Select variables to be included in the regression tree model (d=dependent, n=numerical, c=categorical, x=exclude).\n", 
                   "Double-click on a cell to edit.\n",
                   "Right-click on the table to add a variable."),
          h4("Data transformations (applied to all numerical variables):"),
          checkboxInput("data_log",       "Log-transform", TRUE),
          checkboxInput("data_demean",    "De-meaning", TRUE),
          checkboxInput("data_firstdiff", "First-differences", FALSE)
          ),
        mainPanel(
          rHandsontableOutput("variableDefinition")
        )
        )),
    
    #============ PANEL2: ANALYSE INPUT DATA ==========================
    tabPanel(
      "Analyse and process input data", 
      pageWithSidebar(
        
        headerPanel("Input data explorer"),
        
        #================= UI =======================
        sidebarPanel(
          h2("Data to plot"),
          uiOutput("chooseVar"),
          uiOutput("chooseCountry"),
          h2("Data selection"),
          sliderInput("period", label = h3("Period"), min = 1960, max = 2015, value = c(1980, 2012)),
          sliderInput("ratio",  label = h3("Ratio of missing data"), min = 0.00, max = 0.20, value = 0.05),
          h2("Plot options"),
          checkboxInput("ylog", "Y log scale", FALSE)
        ),
        
        #=========== TEXT/PLOT/TABLE ================
        mainPanel(
          h2("General information:"),
          textOutput("info_year"),
          textOutput("info_country"),
          
          h2("Compare original and processed data:"),
          plotlyOutput("inputDataPlot", width = "100%"),
          
          h2("Check missing data:"),
          tableOutput("BalancedTable"),
          tableOutput("MissingTable1"),
          tableOutput("MissingTable2")
        )
      )),
    
    #========== PANEL3: GENERATE REGRESSION TREE ======================
    tabPanel(
      "Generate regression tree", 
      pageWithSidebar(
        
        headerPanel("Generate regression tree"),
        
        #================= UI =======================
        sidebarPanel(
          #uiOutput("chooseTreeModel"),
          selectInput("treemod",        "Tree type", choices=c("Single tree > LS - Constant",  "Single tree > LMS - Constant"), selected="Single tree > LMS - Constant", multiple=FALSE),
          numericInput("tree_nbcv",     "Number of CVs", 10, 5, 20, 1),
          selectInput("tree_cvtype",    "CV type", choices=c("Mean-based CV tree",  "Median-based CV tree"), selected="Mean-based CV tree", multiple=FALSE),
          numericInput("tree_se",       "SE number for pruning", 0.50, 0, 1, 0.01),
          numericInput("tree_splitfrac","Splitting fraction", 1.00, 0, 1, 0.001),
          selectInput("tree_search",    "Search type", choices=c("Split point from quantiles",  "Use exhaustive search"), selected="Use exhaustive search", multiple=FALSE),
          numericInput("tree_maxsplit", "Max. splits", 13, 1, 50, 1),
          numericInput("tree_minnode",  "Min. node size", 16, 1, 100, 1),
          actionButton("run", "Generate regression tree")
        ),
        
        #=========== TEXT/PLOT/TABLE ================
        mainPanel(
          plotlyOutput("generateTree", width = "100%", height="100%")
        )
      )),
    
    #============== PANEL4: EXPLORE RESULTS #1 ==========================
    tabPanel(
      "Explore results - T-nodes", 
      pageWithSidebar(
        
        headerPanel("Explore results (Terminal nodes)"),
        
        #================= UI =======================
        sidebarPanel(
          uiOutput("chooseTnode")
        ),
        
        #=========== TEXT/PLOT/TABLE ================
        mainPanel(
          uiOutput("ui_TNodePlot")
        )
      )),
    
    #============== PANEL5: EXPLORE RESULTS #2 ==========================
    tabPanel(
      "Explore results #2", 
      pageWithSidebar(
        
        headerPanel("Explore results (transitions)"),
        
        #================= UI =======================
        sidebarPanel(
          "Choose a country"
        ),
        
        #=========== TEXT/PLOT/TABLE ================
        mainPanel(
          "TODO"
        )
      ))
    
  ))