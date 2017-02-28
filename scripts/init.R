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
library(ggrepel)
library(ggnet)
library(GGally)
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
  mutate(factor    = as.numeric(paste(factor))) %>% 
  mutate(demean    = as.logical(paste(demean))) %>% 
  mutate(firstdiff = as.logical(paste(firstdiff)))

# Dependent variable
v_varDep <- paste(u_variableDefinition$variable[which(u_variableDefinition$type=="d")])

# Generate GUIDE file names and paths
p_commonName <- "GUIDEfile"
v_fname_desc <- paste0(p_commonName, "_desc.txt")
v_fname_data <- paste0(p_commonName, "_data.txt")
v_fname_in   <- paste0(p_commonName, "_in.txt")
v_fname_out  <- paste0(p_commonName, "_out.txt")
v_fname_sfv  <- paste0(p_commonName, "_sfv.txt")
v_fname_fn   <- paste0(p_commonName, "_fitnod.txt")
v_fname_tex  <- paste0(p_commonName, ".tex")
v_fname_R    <- paste0(p_commonName, "_predict.R")
v_fname_pdf  <- paste0(p_commonName, "_tree.pdf")
v_fpath_desc <- file.path(v_outPath, v_fname_desc)
v_fpath_data <- file.path(v_outPath, v_fname_data)
v_fpath_in   <- file.path(v_outPath, v_fname_in)
v_fpath_out  <- file.path(v_outPath, v_fname_out)
v_fpath_sfv  <- file.path(v_outPath, v_fname_sfv)
v_fpath_fn   <- file.path(v_outPath, v_fname_fn)
v_fpath_tex  <- file.path(v_outPath, v_fname_tex)
v_fpath_R    <- file.path(v_outPath, v_fname_R)
v_fpath_pdf  <- file.path(v_outPath, v_fname_pdf)

v_fpath         <- list()
v_fpath[["in"]] <- v_fpath_in
v_fpath$out     <- v_fpath_out
v_fpath$desc    <- v_fpath_desc
v_fpath$data    <- v_fpath_data
v_fpath$tex     <- v_fpath_tex
v_fpath$sfv     <- v_fpath_sfv
v_fpath$fn      <- v_fpath_fn
v_fpath$R       <- v_fpath_R
v_fpath$outPath <- v_outPath

v_fname         <- list()
v_fname[["in"]] <- v_fname_in
v_fname$out     <- v_fname_out
v_fname$desc    <- v_fname_desc
v_fname$data    <- v_fname_data
v_fname$tex     <- v_fname_tex
v_fname$sfv     <- v_fname_sfv
v_fname$fn      <- v_fname_fn
v_fname$R       <- v_fname_R

# Generating RData data file 
p_dataRDatapath <- paste0(substr(u_dataPath,1,nchar(u_dataPath)-3), "RData")

# Data file MD5sum
p_dataMD5path <- paste0(substr(u_dataPath,1,nchar(u_dataPath)-3), "md5")
p_dataMD5sum  <- md5sum(u_dataPath)
cat("\n")