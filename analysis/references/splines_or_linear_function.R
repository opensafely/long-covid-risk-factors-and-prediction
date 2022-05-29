cov_num = input$cov_num_gp_consultation
splines_or_linear <- function(cov_num){
  knot_placement=as.numeric(quantile(cov_num, probs=c(0.1,0.5,0.9)))
  print(knot_placement)
  surv_formula <- paste0(
    "Surv(lcovid_surv, lcovid_cens) ~ ",
    paste(covariate_names, collapse = "+"),
    "+rms::rcs(cov_num,parms=knot_placement)", 
    "+ cluster(practice_id)"
  )
  
  ## age is added as a linear predictor
  surv_formula_lp <- paste0(
    "Surv(lcovid_surv, lcovid_cens) ~ ",
    paste(covariate_names, collapse = "+"),
    "+ cov_num", 
    "+ cluster(practice_id)"
  )
  
  if(analysis == "all_vax_td"){
    surv_formula <- paste0(
      "Surv(lcovid_surv, lcovid_cens) ~ ",
      paste(covariate_names, collapse = "+"),
      "+rms::rcs(cov_num,parms=knot_placement)", 
      #    "+ ie.status",
      "+ cluster(practice_id)"
    )
    ## age is added as a linear predictor
    surv_formula_lp <- paste0(
      "Surv(lcovid_surv, lcovid_cens) ~ ",
      paste(covariate_names, collapse = "+"),
      "+ cov_num_age", 
      #    "+ ie.status",
      "+ cluster(practice_id)"
    )
  }
  ## only predictors
  surv_formula_predictors <- stringr::str_extract(surv_formula, " ~.+")
  ## only linear predictors
  surv_formula_predictors_lp <- stringr::str_extract(surv_formula_lp, " ~.+")
  
  print(paste0("survival formula: ", surv_formula))
  
  ## set up before using rms::cph
  dd <<- datadist(input) #
  options(datadist="dd", contrasts=c("contr.treatment", "contr.treatment")) #
  
  print("Part 2: define survival analysis formula is completed!")
  ################################################################################
  # Part 3: Assess if non-linear term is needed for continuous age               #
  #         and redefine the survival analysis formula                           #
  ################################################################################
  
  fit_cox_model_splines <-rms::cph(formula= as.formula(surv_formula),
                                   data= input, weight=input$weight,surv = TRUE,x=TRUE,y=TRUE)
  
  fit_cox_model_linear <-rms::cph(formula= as.formula(surv_formula_lp),
                                  data= input, weight=input$weight,surv = TRUE,x=TRUE,y=TRUE)
  
  if(AIC(fit_cox_model_linear) < AIC(fit_cox_model_splines)){
    surv_formula = surv_formula_lp
    surv_formula_predictors = surv_formula_predictors_lp
    fit_cox_model <- fit_cox_model_linear
  } else {
    fit_cox_model <- fit_cox_model_splines
  }
  splines <- ifelse(grepl("rms::rcs", surv_formula), "Splines", "Linear")
  print("Splines or linear")
  return(splines)
}

splines_or_linear(input$cov_num_age)
try(splines_or_linear(input$cov_num_gp_consultation))
hist(input$cov_num_gp_consultation)
mean(input$cov_num_gp_consultation)
