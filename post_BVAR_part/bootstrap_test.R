# try bootstrap
# not a runnable script. just a piece for manual operations

library(boot)

bootstrap_sum <- function(mydata, indices){
  #resample
  boot_data <- mydata[indices, ]
  
  boot_model <- lm(abs(boot_data[["mps_G_JGB1_JGB1"]]) ~ abs(boot_data[["mdd_JGB1"]])
                   + abs(boot_data[["mdd_JGB1"]]) * (dummy_qep + dummy_conv + dummy_cme + dummy_nip + dummy_ycct + dummy_ifg + dummy_covid), data=boot_data)
  
  coef1 <- coef(boot_model)['abs(boot_data[["mdd_JGB1"]]):dummy_nip']
  coef2 <- coef(boot_model)['abs(boot_data[["mdd_JGB1"]]):dummy_ycct']
  
  coef1 + coef2
  
}

boot_result <- boot(data = yt_select, statistic = bootstrap_sum, R = 10000)

valid_sums <- boot_result$t[!is.na(boot_result$t)]
p_value <- mean(valid_sums > 0)
cat("Bootstrap-based p-value for the sum of Predictor:Category1 and Predictor:Category2 being positive:", p_value, "\n")

#---------------------------------------------------------

bootstrap_sum <- function(mydata, indices){
  #resample
  boot_data <- mydata[indices, ]
  
  boot_model <- lm(abs(boot_data[["mps_G_JGB1_JGB1"]]) ~ abs(boot_data[["mdd_JGB1"]])
                   + abs(boot_data[["mdd_JGB1"]]) * (dummy_qep + dummy_conv + dummy_cme + dummy_nip + 
                                                        dummy_ycc1 + dummy_ycc2 + dummy_ycc3 + dummy_ifg + dummy_covid), data=boot_data)
  
  coef1 <- coef(boot_model)['abs(boot_data[["mdd_JGB1"]]):dummy_nip']
  coef2 <- coef(boot_model)['abs(boot_data[["mdd_JGB1"]]):dummy_ycc3']
  
  coef1 + coef2
  
}

boot_result <- boot(data = yt_select, statistic = bootstrap_sum, R = 10000 )

valid_sums <- boot_result$t[!is.na(boot_result$t)]
p_value <- mean(valid_sums > 0)
cat("Bootstrap-based p-value for the sum of Predictor:Category1 and Predictor:Category2 being negative:", p_value, "\n")


#---------------------------------------------------------
# Test: extra

bootstrap_sum <- function(mydata, indices){
  #resample
  boot_data <- mydata[indices, ]
  
  boot_model <- lm(abs(boot_data[["mps_G_CB1_JGB1"]]) ~ abs(boot_data[["mdd_CB1"]])
                   + abs(boot_data[["mdd_CB1"]]) * (dummy_qep + dummy_conv + dummy_cme + dummy_nip + dummy_ycct + dummy_ifg + dummy_covid), data=boot_data)
  
  coef1 <- coef(boot_model)['abs(boot_data[["mdd_CB1"]]):dummy_nip']
  coef2 <- coef(boot_model)['abs(boot_data[["mdd_CB1"]]):dummy_ycct']
  
  coef1 + coef2
  
}

boot_result <- boot(data = yt_select, statistic = bootstrap_sum, R = 10000)

valid_sums <- boot_result$t[!is.na(boot_result$t)]
p_value <- mean(valid_sums > 0)
cat("Bootstrap-based p-value for the sum of Predictor:Category1 and Predictor:Category2 being positive:", p_value, "\n")

#---------------------------------------------------------
# Test: extra
bootstrap_sum <- function(mydata, indices){
  #resample
  boot_data <- mydata[indices, ]
  
  boot_model <- lm(abs(boot_data[["mps_G_CB1_JGB1"]]) ~ abs(boot_data[["mdd_CB1"]])
                   + abs(boot_data[["mdd_CB1"]]) * (dummy_qep + dummy_conv + dummy_cme + dummy_nip + 
                                                        dummy_ycc1 + dummy_ycc2 + dummy_ycc3 + dummy_ifg + dummy_covid), data=boot_data)
  
  coef1 <- coef(boot_model)['abs(boot_data[["mdd_CB1"]]):dummy_nip']
  coef2 <- coef(boot_model)['abs(boot_data[["mdd_CB1"]]):dummy_ycc1']
  
  coef1 + coef2
  
}

boot_result <- boot(data = yt_select, statistic = bootstrap_sum, R = 10000 )

valid_sums <- boot_result$t[!is.na(boot_result$t)]
p_value <- mean(valid_sums > 0)
cat("Bootstrap-based p-value for the sum of Predictor:Category1 and Predictor:Category2 being negative:", p_value, "\n")










