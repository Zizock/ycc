# this script runs regressions using the results obtained from BVAR simulations.

# run the script to get data ready: must have this file under the same folder
library(here)
source(here("post_BVAR_part", "post_bvar_data_processing.R"))

# this version is:
# Alternative4: add covid and add ifg
# Alternative part3:
# use QQE first stage as the base

# this is the final version of Part 2 regression:
# Daily changes ~ const + Policy_dummies + Interactions_for_overlap_policies

# data process above doesn't include dummy generating

# ==== create dummy variables ====
# do each period separately for easier control
yt_select <- yt_select %>% mutate(
  dummy_qep = ifelse(Date >= as.Date("2001-03-01") & Date <= as.Date("2006-03-01"), 1,0),
  dummy_conv = ifelse(Date >= as.Date("2006-04-01") & Date < as.Date("2010-10-01"), 1,0),
  dummy_cme = ifelse(Date >= as.Date("2010-10-01") & Date <= as.Date("2013-03-01"), 1,0),
  
  dummy_nip = ifelse(Date < as.Date("2016-01-01"), 0,1),
  dummy_ycc1 = ifelse(Date >= as.Date("2016-09-01") & Date <= as.Date("2018-07-01"), 1,0),
  dummy_ycc2 = ifelse(Date >= as.Date("2018-08-01") & Date <= as.Date("2021-03-01"), 1,0),
  dummy_ycc3 = ifelse(Date < as.Date("2021-03-01"), 0,1),
  dummy_ycct = ifelse(Date < as.Date("2016-09-01"), 0,1),
  dummy_ifg = ifelse(Date < as.Date("2019-10-31"), 0,1),
  dummy_zlb = ifelse(Date <= as.Date("2006-07-14") | Date >= as.Date("2010-10-05"), 1,0),
#  dummy_covid = ifelse(Date >= as.Date("2020-03-01"), 1, 0)
  dummy_covid = ifelse(Date >= as.Date("2020-03-01") & Date <= as.Date("2022-03-01"), 1, 0)
)

# ==== significance marks ====
# mark the significance level for each coefficient
get_significance <- function(pval) {
  if (is.na(pval)) {
    return("")
  } else if (pval <= 0.01) {
    return("***")
  } else if (pval > 0.01 & pval <= 0.05) {
    return("**")
  } else if (pval > 0.05 & pval <= 0.1) {
    return("*")
  } else {
    return("")
  }
}

# ==== construct the names ====
# set up variables used in loop
bonds = c("JGB1","JGB2","JGB5","JGB10","JGB20")

dummies <- c( "dummy_qep","dummy_conv","dummy_cme","dummy_nip","dummy_ycct", "dummy_ycc1","dummy_ycc2","dummy_ycc3","dummy_ifg","dummy_covid")
types <- c("coef_", "stdr_", "pval_", "sigl_")

vector_names <- c("coef_(Intercept)","stdr_(Intercept)",
                  "pval_(Intercept)", "sigl_(Intercept)",
                  "coef_abs(yt_select[[yt_mdd]])","stdr_abs(yt_select[[yt_mdd]])",
                  "pval_abs(yt_select[[yt_mdd]])", "sigl_abs(yt_select[[yt_mdd]])")

# generate separate terms
for (dummy in dummies) {
  for (type in types) {
    vector_names <- c(vector_names, paste0(type, dummy))
  }
}

# generate names for cross terms
for (dummy in dummies) {
  for (type in types) {
    vector_names <- c(vector_names, paste0(type, "abs(yt_select[[yt_mdd]])", ":", dummy))
  }
}

# add a final row for R-square
vector_names <- c(vector_names, "R-square")


# ==== regression loop ====

# an empty list to store results
results_list <- list()

for(yt_count in 1:3){
  
  for(B in bonds){
    yt_mdd <- paste0("mdd_", B)
    
    # set yt for raw ,mps or info
    if(yt_count==1){
      yt_raw <- paste0("g_", B)
    } else if(yt_count==2){
      yt_raw <- paste0("mps_G_", B, "_JGB1")
      
    } else{ # yt_count==3 for info
      yt_raw <- paste0("info_G_", B, "_JGB1")
    }
    
    
    for (mod_ver in 1:2){
      
      results_vector<- rep(NA,length(vector_names))
      
      names(results_vector) <- vector_names
      
      if(mod_ver ==1){
        # mod_var =1 
        model_raw <- lm(abs(yt_select[[yt_raw]]) ~ abs(yt_select[[yt_mdd]])
                        + abs(yt_select[[yt_mdd]]) * (dummy_qep + dummy_conv + dummy_cme + dummy_nip + dummy_ycct + dummy_ifg + dummy_covid), data=yt_select)
      } else{
        # mod_ver =2
        model_raw <- lm(abs(yt_select[[yt_raw]]) ~ abs(yt_select[[yt_mdd]]) + 
                          abs(yt_select[[yt_mdd]]) * (dummy_qep + dummy_conv + dummy_cme + dummy_nip+ dummy_ycc1 + dummy_ycc2 + dummy_ycc3 + dummy_ifg+ dummy_covid), data=yt_select)
      }
      # collect results
      raw_summary <- summary(model_raw)
      
      # extract coefficients and p-values from raw_summary
      for (var in rownames(raw_summary$coefficients)) {
        
        coef_name <- paste0("coef_", var)
        stderr_name <- paste0("stdr_", var)
        pval_name <- paste0("pval_", var)
        stars_name <- paste0("sigl_", var)
        
        if (coef_name %in% names(results_vector)) {
          
          results_vector[coef_name] <- raw_summary$coefficients[var, "Estimate"]
          results_vector[stderr_name] <- raw_summary$coefficients[var,"Std. Error"]
          results_vector[pval_name] <- raw_summary$coefficients[var, "Pr(>|t|)"]
          results_vector[stars_name] <- get_significance(raw_summary$coefficients[var, "Pr(>|t|)"])
          
        }
      }
      
      
      # store R-square
      results_vector["R-square"] <- raw_summary$adj.r.squared
      
      # now results_vector collects needed results from current regression
      
      # merge information in results_vector into the results_list
      if(yt_count==1){
        
        result_G <- c(Model = "G", B = B, mod_ver = mod_ver, results_vector)
        # store results in the list, if the column doesn't exist, create it
        results_list[[paste0(B, "_modver_", mod_ver, "_raw")]] <- result_G
        
      } else if(yt_count==2){
        
        result_mps <- c(Model = "mps", B = B, mod_ver = mod_ver, results_vector)
        # store results in the list
        results_list[[paste0(B, "_modver_", mod_ver, "_mps")]] <- result_mps
        
      } else{ # if yt_count==3
        
        result_info <- c(Model = "info", B = B, mod_ver = mod_ver, results_vector)
        # store results in the list
        results_list[[paste0(B, "_modver_", mod_ver, "_info")]] <- result_info
        
      }
    }
    
  }
  
}
results_df <- do.call(cbind, results_list) # each regression is a column. do.call applies cbind to each elements in results_list

write.csv(results_df, here("post_BVAR_part", "alternative4_dummy_part31_with_const&ifg.csv"))




#Try:
# model_try <- lm(abs(yt_select[["mps_G_JGB10_JGB1"]]) ~ abs(yt_select[["mdd_JGB10"]]) +
#                   abs(yt_select[["mdd_JGB10"]]) * (dummy_qep + dummy_conv + dummy_cme + dummy_nip
#                                                    + dummy_ycct+ + dummy_ifg + dummy_covid), data=yt_select)

#Try:
# model_try <- lm(abs(yt_select[["mps_G_JGB10_JGB1"]]) ~ abs(yt_select[["mdd_JGB10"]]) +
#                   abs(yt_select[["mdd_JGB10"]]) * (dummy_qep + dummy_conv + dummy_cme + dummy_nip
#                                                  + dummy_ycc1 + dummy_ycc2 + dummy_ycc3+ dummy_ifg + dummy_covid), data=yt_select)
# 
# summary(model_try)
# linearHypothesis(model_try, 'abs(yt_select[["mdd_JGB10"]]):dummy_ycc3 + abs(yt_select[["mdd_JGB10"]]):dummy_nip = 0')


# test_result <- linearHypothesis(model_try, 'abs(yt_select[["mdd_JGB10"]]):dummy_ycct + abs(yt_select[["mdd_JGB10"]]):dummy_nip = 0')
# 
# two_sided_p_value <- test_result$`Pr(>F)`[2] # Assuming F-test, p-value is in the second row
# one_sided_p_value <- two_sided_p_value / 2   # For one-sided test
# 
# # Output the result
# summary(model_try)
# cat("One-sided p-value for testing beta_1 + beta_2 > 0:", one_sided_p_value, "\n")











