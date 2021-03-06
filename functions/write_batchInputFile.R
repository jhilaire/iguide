write_batchInputFile <- function(i_singleTree, i_treeType, i_treeOptions, i_fpath) {

  v_fname_out  <- basename(i_fpath$out)
  v_fname_desc <- basename(i_fpath$desc)
  v_fname_tex  <- basename(i_fpath$tex)  
  v_fname_fn   <- basename(i_fpath$fn)
  v_fname_sfv  <- basename(i_fpath$sfv)
  v_fname_R    <- basename(i_fpath$R)
  #v_outPath    <- i_fpath$outPath

  regexpr("(\\(.*\\))", "LMS-constant (mean-based)")
  
  # Single trees
  if (i_singleTree) {  
    #------ Single tree header -----------------------------------------------------------------------
    catret("GUIDE       (do not edit this file unless you know what you are doing)", file=i_fpath[["in"]])
    catret("  24.6      (version of GUIDE that generated this file)", file=i_fpath[["in"]], append=T)
    catret(" 1          (1=model fitting, 2=importance or DIF scoring, 3=data conversion)", file=i_fpath[["in"]], append=T)
    catret(paste0("\"", v_fname_out, "\"","  (name of output file)"), file=i_fpath[["in"]], append=T)
    
    # Tree type
    catret(" 1          (1=one tree, 2=ensemble)", file=i_fpath[["in"]], append=T)
    catret(" 2          (1=classification, 2=regression, 3=propensity score grouping)", file=i_fpath[["in"]], append=T)
    
    #------ Linear trees -------------------------------------------------------------------------------
    if (substr(i_treeType,1,2) == "LS" || substr(i_treeType,1,3) == "LMS") {
      catret(" 1          (1=linear, 2=quantile, 3=Poisson, 4=hazard, 5=multiresponse or itemresponse, 6=longitudinal with T variables)", file=i_fpath[["in"]], append=T)
      
      #------ Least squares ----------------------------------------------------------------------------
      if (substr(i_treeType,1,2) == "LS") {
        catret(" 1          (1=least squares, 2=least median of squares)", file=i_fpath[["in"]], append=T)
        
        if (i_treeType == "LS-constant") {
          cat(paste0("  - batch input file (case: Single tree > Regression > Linear > Least squares > Constant) ", i_fpath[["in"]],"\n"))
          catret(" 3          (0=stepwise linear, 1=multiple linear, 2=best polynomial, 3=constant, 4=steptwise simple ANCOVA)", file=i_fpath[["in"]], append=T)
        }
        if (i_treeType == "LS-linear") {
          cat(paste0("  - batch input file (case: Single tree > Regression > Linear > Least squares > Stepwise linear) ", i_fpath[["in"]],"\n"))
          catret(" 0          (0=stepwise linear, 1=multiple linear, 2=best polynomial, 3=constant, 4=steptwise simple ANCOVA)", file=i_fpath[["in"]], append=T)
          cat("Not yet implemented. Stopping program...")
          stop()
        }
        if (i_treeType == "LS-bestPolynomial") {
          cat(paste0("  - batch input file (case: Single tree > Regression > Linear > Least squares > Best polynomial) ", i_fpath[["in"]],"\n"))
          catret(" 2          (0=stepwise linear, 1=multiple linear, 2=best polynomial, 3=constant, 4=steptwise simple ANCOVA)", file=i_fpath[["in"]], append=T)
          cat("Not yet implemented. Stopping program...")
          stop()
        }
        if (i_treeType == "LS-multiLinear") {  #==== default LS ====
          cat(paste0("  - batch input file (case: Single tree > Regression > Linear > Least squares > Multiple linear) ", i_fpath[["in"]],"\n"))
          catret(" 1          (0=stepwise linear, 1=multiple linear, 2=best polynomial, 3=constant, 4=steptwise simple ANCOVA)", file=i_fpath[["in"]], append=T)
          
          # Specific options
          catret(" 1          (1=intercept included, 2=intercept excluded)", file=i_fpath[["in"]], append=T)
          catret(" 3          (0=none, 1=node range, 2=+10% node range, 3=global range)", file=i_fpath[["in"]], append=T)
        }
        if (i_treeType == "LS-simpleANCOVA") {
          cat(paste0("  - batch input file (case: Single tree > Regression > Linear > Least squares > Stepwise simple ANCOVA) ", i_fpath[["in"]],"\n"))
          catret(" 4          (0=stepwise linear, 1=multiple linear, 2=best polynomial, 3=constant, 4=steptwise simple ANCOVA)", file=i_fpath[["in"]], append=T)
          cat("Not yet implemented. Stopping program...")
          stop()
        }
      }
      
      #------ Least median of squares ------------------------------------------------------------------
      if (substr(i_treeType,1,3) == "LMS") {
        catret(" 2          (1=least squares, 2=least median of squares)", file=i_fpath[["in"]], append=T)
        
        if (i_treeType == "LMS-constant") {
          catret(" 3          (1=multiple linear, 2=simple linear, 3=constant)", file=i_fpath[["in"]], append=T)
        }
        if (i_treeType == "LMS-multilinear") {
          catret(" 1          (1=multiple linear, 2=simple linear, 3=constant)", file=i_fpath[["in"]], append=T)
          
          # Specific options (all set to default values, instead of modifying the value directly create your own tree type)
          # Choose a truncatin method for predicted values
          catret(" 3          (0=none, 1=node range, 2=+10% node range, 3=global range)", file=i_fpath[["in"]], append=T)
        }
        if (i_treeType == "LMS-bestSimpleLinear") { #==== default LMS ====
          catret(" 2          (1=multiple linear, 2=simple linear, 3=constant)", file=i_fpath[["in"]], append=T)
          
          # Specific options (all set to default values, instead of modifying the value directly create your own tree type)
          # Choose a truncatin method for predicted values
          catret(" 3          (0=none, 1=node range, 2=+10% node range, 3=global range)", file=i_fpath[["in"]], append=T)
        }
      }
      
      #------ Single LS/LMS tree footer ---------------------------------------------------------------
      # (all set to default values, all files are generated)
      catret(" 1          (1=interaction tests, 2=skip them)", file=i_fpath[["in"]], append=T)
      catret(" 1          (1=prune by CV, 2=no pruning)", file=i_fpath[["in"]], append=T)
      catret(paste0("\"", v_fname_desc, "\"","  (name of data description file)"), file=i_fpath[["in"]], append=T)
      
      if (i_treeType %in% c("LMS-multilinear", "LMS-bestSimpleLinear")) {
        catret(" 2          (missing values: 1=separate models, 2=impute with means, 3=constant model)", file=i_fpath[["in"]], append=T)
      }

      catret(paste0("        ",i_treeOptions$nbcv,"  (number of cross-validations)"), file=i_fpath[["in"]], append=T)
      catret(paste0(" ",i_treeOptions$cvtype,"          (1=mean-based CV tree, 2=median-based CV tree)"), file=i_fpath[["in"]], append=T)
      catret(paste0("     ",format(i_treeOptions$seval, nsmall=3),"  (SE number for pruning)"), file=i_fpath[["in"]], append=T)
      
      if (i_treeType %in% c("LS-constant")) {
        catret(paste0(" ",i_treeOptions$search,"          (1=split point from quantiles, 2=use exhaustive search)"), file=i_fpath[["in"]], append=T)
      } 
      if (i_treeType %in% c("LMS-multilinear", "LMS-bestSimpleLinear", "LMS-constant")) {
        catret(" 2          (1=accept default splitting fraction, 2=change it)", file=i_fpath[["in"]], append=T)
        catret(paste0("  ",format(i_treeOptions$splitfrac, nsmall=4),"     (frac, where #splits = max(9,fract*n), with n = #cases in node)"), file=i_fpath[["in"]], append=T)
      } 
      
      catret(" 2          (1=default max. number of split levels, 2=specify no. in next line)", file=i_fpath[["in"]], append=T)
      catret(paste0("        ",i_treeOptions$maxsplits,"   (max. no. split levels)"), file=i_fpath[["in"]], append=T)
      catret(" 2          (1=default min. node size, 2=specify min. value in next line)", file=i_fpath[["in"]], append=T)
      catret(paste0("        ",i_treeOptions$minnbnodes,"  (min. node sample size)"), file=i_fpath[["in"]], append=T)
      catret(" 1          (1=write latex, 2=skip latex)", file=i_fpath[["in"]], append=T)
      catret(paste0("\"", v_fname_tex, "\"","  (latex file name)"), file=i_fpath[["in"]], append=T)
      catret(" 1          (1=vertical tree, 2=sideways tree)", file=i_fpath[["in"]], append=T)
      catret(" 1          (1=include node numbers, 2=exclude)", file=i_fpath[["in"]], append=T)
      catret(" 1          (1=number all nodes, 2=only terminal nodes)", file=i_fpath[["in"]], append=T)
      catret(" 6          (1=white, 2=lightgray, 3=gray, 4=darkgray, 5=black, 6=yellow, 7=red, 8=blue, 9=green, 10=magenta, 11=cyan)", file=i_fpath[["in"]], append=T)
      catret(" 2          (1=no storage, 2=store fit and split variables, 3=store split variables and values)", file=i_fpath[["in"]], append=T)
      catret(paste0("\"", v_fname_sfv, "\"","  (split variable file name)"), file=i_fpath[["in"]], append=T)
      
      if (i_treeType %in% c("LMS-multilinear", "LMS-bestSimpleLinear")) {
        catret(" 1          (1=do not save, 2=save regression coefs in separate file)", file=i_fpath[["in"]], append=T)
      }
      
      catret(" 2          (1=do not save individual fitted values and node IDs, 2=save in a file)", file=i_fpath[["in"]], append=T)
      catret(paste0("\"", v_fname_fn, "\"","  (file name for individual fitted values and node IDs)"), file=i_fpath[["in"]], append=T)
      if (i_treeType %in% c("LMS-multilinear", "LMS-bestSimpleLinear")) {
        catret(" 1          (1=do not save terminal node IDs for importance scoring in a file, 2=save them)", file=i_fpath[["in"]], append=T)
      }
      catret(" 2          (1=do not write R function, 2=write R function)", file=i_fpath[["in"]], append=T)
      catret(paste0("\"", v_fname_R, "\"","  (R code file)"), file=i_fpath[["in"]], append=T)
      
      #file.copy(i_fpath[["in"]], file.path(v_outPath, v_fname_in), overwrite = TRUE)
      #file.copy(i_fpath[["in"]], file.path("../GUIDE", v_fname_in), overwrite = TRUE)
      
    }
    
    #------ Quantile regression trees ------------------------------------------------------------------
    if (substr(i_treeType,1,1) == "Q") {
      catret(" 2          (1=linear, 2=quantile, 3=Poisson, 4=proportional Hazards, 5=multiresponse or itemresponse, 6=longitudinal with T variables)", file=i_fpath[["in"]], append=T)
      if (i_treeType == "Q1-Constant") {
        cat(paste0("  - batch input file (case: Single tree > Regression > Quantile > Constant (1 quantile: 0.25)) ", i_fpath[["in"]],"\n"))
        catret(" 3          (1=multiple linear, 2=polynomial, 3=constant)", file=i_fpath[["in"]], append=T)
        catret(" 1          (1=one quantile, 2=two quantiles)", file=i_fpath[["in"]], append=T)
        catret(" 0.2500     (quantile)", file=i_fpath[["in"]], append=T)
      }
      if (i_treeType == "Q2-Constant") {
        cat(paste0("  - batch input file (case: Single tree > Regression > Quantile > Constant (2 quantiles: 0.25, 0.75)) ", i_fpath[["in"]],"\n"))
        catret(" 3          (1=multiple linear, 2=polynomial, 3=constant)", file=i_fpath[["in"]], append=T)
        catret(" 1          (1=one quantile, 2=two quantiles)", file=i_fpath[["in"]], append=T)
        catret(" 0.2500     (1st quantile probability)", file=i_fpath[["in"]], append=T)
        catret(" 0.7500     (2nd quantile probability)", file=i_fpath[["in"]], append=T)
      }
      if (i_treeType == "Q1-multiLinear") {
        cat(paste0("  - batch input file (case: Single tree > Regression > Quantile > Multiple linear (1 quantile: 0.25)) ", i_fpath[["in"]],"\n"))
        catret(" 2          (1=multiple linear, 2=polynomial, 3=constant)", file=i_fpath[["in"]], append=T)
        catret(" 1          (highest degree of polynomial model)", file=i_fpath[["in"]], append=T)
        catret(" 1          (1=one quantile, 2=two quantiles)", file=i_fpath[["in"]], append=T)
        catret(" 0.2500     (1st quantile probability)", file=i_fpath[["in"]], append=T)
      }
      if (i_treeType == "Q2-multiLinear") {
        cat(paste0("  - batch input file (case: Single tree > Regression > Quantile > Multiple linear (2 quantiles: 0.25, 0.75)) ", i_fpath[["in"]],"\n"))
        catret(" 2          (1=multiple linear, 2=polynomial, 3=constant)", file=i_fpath[["in"]], append=T)
        catret(" 1          (highest degree of polynomial model)", file=i_fpath[["in"]], append=T)
        catret(" 2          (1=one quantile, 2=two quantiles)", file=i_fpath[["in"]], append=T)
        catret(" 0.2500     (1st quantile probability)", file=i_fpath[["in"]], append=T)
        catret(" 0.7500     (2nd quantile probability)", file=i_fpath[["in"]], append=T)
      }
      if (i_treeType == "Q1-bestPolynomial") {
        cat(paste0("  - batch input file (case: Single tree > Regression > Quantile > Best polynomial (1 quantile: 0.25)) ", i_fpath[["in"]],"\n"))
        catret(" 2          (1=multiple linear, 2=polynomial, 3=constant)", file=i_fpath[["in"]], append=T)
        catret(" 1          (highest degree of polynomial model)", file=i_fpath[["in"]], append=T)
        catret(" 1          (1=one quantile, 2=two quantiles)", file=i_fpath[["in"]], append=T)
        catret(" 0.2500     (1st quantile probability)", file=i_fpath[["in"]], append=T)
      }
      if (i_treeType == "Q2-bestPolynomial") {
        cat(paste0("  - batch input file (case: Single tree > Regression > Quantile > Best polynomial (2 quantiles: 0.25, 0.75)) ", i_fpath[["in"]],"\n"))
        catret(" 2          (1=multiple linear, 2=polynomial, 3=constant)", file=i_fpath[["in"]], append=T)
        catret(" 1          (highest degree of polynomial model)", file=i_fpath[["in"]], append=T)
        catret(" 2          (1=one quantile, 2=two quantiles)", file=i_fpath[["in"]], append=T)
        catret(" 0.2500     (1st quantile probability)", file=i_fpath[["in"]], append=T)
        catret(" 0.7500     (2nd quantile probability)", file=i_fpath[["in"]], append=T)
      }
      
      #------ Single Q tree footer ---------------------------------------------------------------
      # (all set to default values, all files are generated)
      catret(" 1          (1=interaction tests, 2=skip them)", file=i_fpath[["in"]], append=T)
      catret(" 1          (1=prune by CV, 2=no pruning)", file=i_fpath[["in"]], append=T)
      catret(paste0("\"", v_fname_desc, "\"","  (name of data description file)"), file=i_fpath[["in"]], append=T)
      catret(" 2          (missing values: 1=separate models, 2=impute with means, 3=constant model)", file=i_fpath[["in"]], append=T)
      catret("        10  (number of cross-validations)", file=i_fpath[["in"]], append=T)
      catret(" 1          (1=mean-based CV tree, 2=median-based CV tree)", file=i_fpath[["in"]], append=T)
      catret("     0.500  (SE number for pruning)", file=i_fpath[["in"]], append=T)
      catret(" 2          (1=split point from quantiles, 2=use exhaustive search)", file=i_fpath[["in"]], append=T)
      catret(" 1          (1=default max number of split levels, 2=specify no. in next line)", file=i_fpath[["in"]], append=T)
      catret(" 1          (1=default min node size, 2=specify node size in next line)", file=i_fpath[["in"]], append=T)
      catret(" 1          (1=write latex, 2=skip latex)", file=i_fpath[["in"]], append=T)
      catret(paste0("\"", v_fname_tex, "\"","  (latex file name)"), file=i_fpath[["in"]], append=T)
      catret(" 1          (1=vertical tree, 2=sideways tree)", file=i_fpath[["in"]], append=T)
      catret(" 1          (1=include node numbers, 2=exclude)", file=i_fpath[["in"]], append=T)
      catret(" 1          (1=number all nodes, 2=only terminal nodes)", file=i_fpath[["in"]], append=T)
      catret(" 6          (1=white, 2=lightgray, 3=gray, 4=darkgray, 5=black, 6=yellow, 7=red, 8=blue, 9=green, 10=magenta, 11=cyan)", file=i_fpath[["in"]], append=T)
      catret(" 1          (1=no storage, 2=store fit and split variables, 3=store split variables and values)", file=i_fpath[["in"]], append=T)
      catret(" 1          (1=do not save, 2=save regression coefs in separate file)", file=i_fpath[["in"]], append=T)
      catret(" 2          (1=do not save individual fitted values and node IDs, 2=save in a file)", file=i_fpath[["in"]], append=T)
      catret(paste0("\"", v_fname_fn, "\"","  (file name for individual fitted values and node IDs)"), file=i_fpath[["in"]], append=T)
      catret(" 1          (1=do not save terminal node IDs for importance scoring in a file, 2=save them)", file=i_fpath[["in"]], append=T)
      catret(" 1          (1=do not write R function, 2=write R function)", file=i_fpath[["in"]], append=T)
      
    }
    
  # Tree ensemble
  } else {
    #------ Ensemble tree header -----------------------------------------------------------------------
    catret("123321      (do not edit this file unless you know what you are doing)", file=i_fpath[["in"]])
    catret("  23.6      (version of GUIDE that generated this file)", file=i_fpath[["in"]], append=T)
    catret(" 1          (1=model fitting, 2=importance or DIF scoring, 3=data conversion)", file=i_fpath[["in"]], append=T)
    catret(paste0("\"", v_fname_out, "\"","  (name of output file)"), file=i_fpath[["in"]], append=T)
    
    # Tree type
    catret(" 2          (1=single tree, 2=ensemble)", file=i_fpath[["in"]], append=T)
    
    if (i_treeType == "Bagging") {
      catret(" 1          (1=Bagging, 2=rForest)", file=i_fpath[["in"]], append=T)
      catret(" 2          (1=classification, 2=regression, 3=propensity score grouping)", file=i_fpath[["in"]], append=T)
      cat("Not yet implemented. Stopping program...")
      stop()
    }
    if (i_treeType == "RF-random") {
      catret(" 2          (1=Bagging, 2=rForest)", file=i_fpath[["in"]], append=T)
      catret(" 1          (1=Random splits of missing values, 2=Non-random splits of missing values)", file=i_fpath[["in"]], append=T)
      catret(" 2          (1=classification, 2=regression, 3=propensity score grouping)", file=i_fpath[["in"]], append=T)
      cat("Not yet implemented. Stopping program...")
      stop()
    }
    if (i_treeType == "RF-nonRandom") {
      catret(" 2          (1=Bagging, 2=rForest)", file=i_fpath[["in"]], append=T)
      catret(" 2          (1=Random splits of missing values, 2=Non-random splits of missing values)", file=i_fpath[["in"]], append=T)
      catret(" 2          (1=classification, 2=regression, 3=propensity score grouping)", file=i_fpath[["in"]], append=T)
      cat("Not yet implemented. Stopping program...")
      stop()
    }
  }
}