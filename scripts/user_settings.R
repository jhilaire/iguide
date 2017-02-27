#=================================================================================
# User settings
#=================================================================================
DEBUG = TRUE

# Paths, files and folders
u_dataPath   <- "data/typ_data_20_05.csv"
u_outPath    <- "output"
u_expName    <- "Shiny_UserSession"
u_guidePath  <- "GUIDE"

# GUIDE parameters
u_missVal    <- "NaN"                    # Missing value symbol
u_startLine  <- "2"                      # Data starting line (>=2 in case of file header)

# Variable definitions
u_variableDefinition <- list(
  #---- (d) dependent variable (choose one) -----------------------------
  "co2_cons_UNFCCC_pc" = c(type="x", factor=1,     transform="",    demean=TRUE,  firstdiff=FALSE),
  "co2_terr_UNFCCC_pc" = c(type="x", factor=1,     transform="",    demean=TRUE,  firstdiff=FALSE),
  "co2_terr_CDIAC_pc"  = c(type="d", factor=1,     transform="log", demean=TRUE,  firstdiff=FALSE),
  "co2_cons_UNFCCC"    = c(type="x", factor=1,     transform="",    demean=TRUE,  firstdiff=FALSE),
  "co2_terr_UNFCCC"    = c(type="x", factor=1,     transform="",    demean=TRUE,  firstdiff=FALSE),
  "co2_terr_CDIAC"     = c(type="x", factor=1,     transform="",    demean=TRUE,  firstdiff=FALSE),
  #---- (t) observation time --------------------------------------------
  "year"               = c(type="x", factor=1,     transform="",    demean=TRUE,  firstdiff=FALSE), 
  #---- (c) categorical variable used for splitting only ----------------
  "dummy_oil"          = c(type="x", factor=1,     transform="",    demean=FALSE, firstdiff=FALSE),
  "dummy_gas"          = c(type="x", factor=1,     transform="",    demean=FALSE, firstdiff=FALSE),
  "dummy_coal"         = c(type="x", factor=1,     transform="",    demean=FALSE, firstdiff=FALSE),
  #---- (r) categorical treatment variable used for fitting only --------
  #---- (n) numerical variable used for both splitting and fitting ------
  "gdp_ppp_PWT_pc"     = c(type="n", factor=1e-3,  transform="log", demean=TRUE,  firstdiff=FALSE),
  "energy_tpes_IEA_pc" = c(type="x", factor=1,     transform="",    demean=TRUE,  firstdiff=FALSE),
  "energy_fec_IEA_pc"  = c(type="n", factor=1,     transform="log", demean=TRUE,  firstdiff=FALSE),
  "gini_SWIID"         = c(type="x", factor=1,     transform="",    demean=TRUE,  firstdiff=FALSE),
  "pop_UN"             = c(type="x", factor=1e-9,  transform="log", demean=TRUE,  firstdiff=FALSE), 
  "lex_WB"             = c(type="x", factor=1,     transform="",    demean=TRUE,  firstdiff=FALSE),         
  "gini_WB"            = c(type="x", factor=1,     transform="",    demean=TRUE,  firstdiff=FALSE),
  "urban_WB"           = c(type="x", factor=1,     transform="",    demean=TRUE,  firstdiff=FALSE),
  "pop_density_WB"     = c(type="x", factor=1e-3,  transform="",    demean=TRUE,  firstdiff=FALSE), 
  "share_gdp_agri_WB"  = c(type="x", factor=1,     transform="",    demean=TRUE,  firstdiff=FALSE),
  "share_gdp_ind_WB"   = c(type="x", factor=1,     transform="",    demean=TRUE,  firstdiff=FALSE),
  "share_gdp_serv_WB"  = c(type="x", factor=1,     transform="",    demean=TRUE,  firstdiff=FALSE),
  "share_gdp_trade_WB" = c(type="x", factor=1,     transform="",    demean=TRUE,  firstdiff=FALSE),
  "inst_WGI"           = c(type="x", factor=1,     transform="",    demean=TRUE,  firstdiff=FALSE),
  "ratio_ee"           = c(type="x", factor=1,     transform="",    demean=TRUE,  firstdiff=FALSE),
  "ratio_ef"           = c(type="x", factor=1,     transform="",    demean=TRUE,  firstdiff=FALSE),
  "gdp_ppp_PWT"        = c(type="x", factor=1e-9,  transform="log", demean=TRUE,  firstdiff=FALSE),
  "energy_tpes_IEA"    = c(type="x", factor=1e-9,  transform="log", demean=TRUE,  firstdiff=FALSE),
  "energy_fec_IEA"     = c(type="x", factor=1e-9,  transform="",    demean=TRUE,  firstdiff=FALSE),
  #---- (f) numerical variable used for fitting only --------------------
  #---- (s) numerical variable used for splitting only ------------------
  #---- other variables -------------------------------------------------
  "country"            = c(type="x", factor=NA,    transform="",    demean=FALSE, firstdiff=FALSE))