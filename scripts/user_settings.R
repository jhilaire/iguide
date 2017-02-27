#=================================================================================
# User settings
#=================================================================================
# Paths, files and folders
u_dataPath   <- "../../data/typ_data_20_05.csv"
u_outPath    <- "output"
u_expName    <- "Shiny_LMS-Constant"
u_guidePath  <- "../../GUIDE"

# GUIDE parameters
# TODO: Implement longitudinal regression tree type
#u_singleTree <- TRUE
#u_treeType   <- "LMS-constant"           # Tree definitions are in functions/write_batchInputFile.R
#u_demeaning  <- TRUE                     # Demean variables? If yes, dummy variables should be excluded
#u_period     <- c(1980, 2012)            # Select period of time
u_missVal    <- "NaN"                    # Missing value symbol
u_startLine  <- "2"                      # Data starting line (>=2 in case of file header)

# Variable definitions
u_variableDefinition <- list(
  #---- (d) dependent variable (choose one) -----------------------------
  "co2_cons_UNFCCC_pc" = c(type="x", factor=1,     transform=""),
  "co2_terr_UNFCCC_pc" = c(type="x", factor=1,     transform=""),
  "co2_terr_CDIAC_pc"  = c(type="d", factor=1,     transform="log"),
  "co2_cons_UNFCCC"    = c(type="x", factor=1,     transform=""),
  "co2_terr_UNFCCC"    = c(type="x", factor=1,     transform=""),
  "co2_terr_CDIAC"     = c(type="x", factor=1,     transform=""),
  #---- (t) observation time --------------------------------------------
  "year"               = c(type="x", factor=1,     transform=""), 
  #---- (c) categorical variable used for splitting only ----------------
  "dummy_oil"          = c(type="x", factor=1,     transform=""),
  "dummy_gas"          = c(type="x", factor=1,     transform=""),
  "dummy_coal"         = c(type="x", factor=1,     transform=""),
  #---- (r) categorical treatment variable used for fitting only --------
  #---- (n) numerical variable used for both splitting and fitting ------
  "gdp_ppp_PWT_pc"     = c(type="n", factor=1e-3,  transform="log"),
  "energy_tpes_IEA_pc" = c(type="x", factor=1,     transform=""),
  "energy_fec_IEA_pc"  = c(type="n", factor=1,     transform="log"),
  "gini_SWIID"         = c(type="x", factor=1,     transform=""),
  "pop_UN"             = c(type="x", factor=1e-9,  transform="log"), 
  "lex_WB"             = c(type="x", factor=1,     transform=""),         
  "gini_WB"            = c(type="x", factor=1,     transform=""),
  "urban_WB"           = c(type="x", factor=1,     transform=""),
  "pop_density_WB"     = c(type="x", factor=1e-3,  transform=""), 
  "share_gdp_agri_WB"  = c(type="x", factor=1,     transform=""),
  "share_gdp_ind_WB"   = c(type="x", factor=1,     transform=""),
  "share_gdp_serv_WB"  = c(type="x", factor=1,     transform=""),
  "share_gdp_trade_WB" = c(type="x", factor=1,     transform=""),
  "inst_WGI"           = c(type="x", factor=1,     transform=""),
  "ratio_ee"           = c(type="x", factor=1,     transform=""),
  "ratio_ef"           = c(type="x", factor=1,     transform=""),
  "gdp_ppp_PWT"        = c(type="x", factor=1e-9,  transform="log"),
  "energy_tpes_IEA"    = c(type="x", factor=1e-9,  transform="log"),
  "energy_fec_IEA"     = c(type="x", factor=1e-9,  transform=""),
  #---- (f) numerical variable used for fitting only --------------------
  #---- (s) numerical variable used for splitting only ------------------
  #---- other variables -------------------------------------------------
  "country"            = c(type="x", factor=NA,    transform=""))