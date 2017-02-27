#=================================================================================
# Initialisation
#=================================================================================
cat("Initializing server...\n")
cat("  > Loading libraries...\n")
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
library(magrittr)

#---- Own functions ----
cat("  > Loading functions...\n")
dump <- lapply(list.files("functions", pattern=".R", full.names = TRUE), source)

#---- Files and directories ----
cat("  > Processing files and directores...\n")
v_expPath = paste0(u_expName, "_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"))

# Create output folder
v_outPath <- file.path(u_outPath, v_expPath)
dir.create(v_outPath, recursive = TRUE)

#---- Other stuff ----
# Variable renaming
p_variableRenaming = data.frame()

# Transform variable definition list to data.frame
u_variableDefinition <- transformListToDataframe(u_variableDefinition) %>% 
  mutate(factor = as.numeric(paste(factor)))

# Dependent variable
v_varDep <- paste(u_variableDefinition$variable[which(u_variableDefinition$type=="d")])

# Generate GUIDE file names and paths
v_fname_desc <- paste0(u_expName, "desc.txt")
v_fname_data <- paste0(u_expName, "data.txt")
v_fname_in   <- paste0(u_expName, "in.txt")
v_fname_out  <- paste0(u_expName, "out.txt")
v_fname_fn   <- paste0(u_expName, "fitnod.txt")
v_fname_tex  <- paste0(u_expName, ".tex")
v_fname_pdf  <- paste0(u_expName, "_tree.pdf")
v_fpath_desc <- file.path(u_guidePath, v_fname_desc)
v_fpath_data <- file.path(u_guidePath, v_fname_data)
v_fpath_in   <- file.path(u_guidePath, v_fname_in)
v_fpath_out  <- file.path(u_guidePath, v_fname_out)
v_fpath_fn   <- file.path(u_guidePath, v_fname_fn)
v_fpath_tex  <- file.path(u_guidePath, v_fname_tex)
v_fpath_pdf  <- file.path(u_guidePath, v_fname_pdf)

v_fpath         <- list()
v_fpath[["in"]] <- v_fpath_in
v_fpath$out     <- v_fpath_out
v_fpath$desc    <- v_fpath_data
v_fpath$data    <- v_fpath_desc
v_fpath$tex     <- v_fpath_tex
v_fpath$fn      <- v_fpath_fn
v_fpath$outPath <- v_outPath

v_fname         <- list()
v_fname[["in"]] <- v_fname_in
v_fname$out     <- v_fname_out
v_fname$desc    <- v_fname_desc
v_fname$data    <- v_fname_data
v_fname$tex     <- v_fname_tex
v_fname$fn      <- v_fname_fn

# Generating RData data file 
p_dataRDatapath <- paste0(substr(u_dataPath,1,nchar(u_dataPath)-3), "RData")

# Data file MD5sum
p_dataMD5path <- paste0(substr(u_dataPath,1,nchar(u_dataPath)-3), "md5")
p_dataMD5sum  <- md5sum(u_dataPath)
cat("\n")