# a data processing script to be run after the MATLAB BVAR model.
# this script prepares data ready to use for:
# 1, policy dummy regression.
# 2, everything needed for the ESP VAR section.

rm(list=ls())
library(tidyverse)
library(here)
library(zoo)


# ==== Read data sets ====

# use the existing yt_select data from BVAR section
yt_select <- read.csv(here("matlab", "data_var", "yt_select.csv"))
yt_select$Date <- as.Date(paste(yt_select$year, yt_select$month, "01", sep = "-"), format = "%Y-%m-%d")

# import esp data
espdata <- read_excel(here("data", "espdata.xlsx",sheet="Sheet2"))
espdata$Date <- as.Date(paste(espdata$year, espdata$month, "01", sep = "-"), format = "%Y-%m-%d")

# import macro variable data
macrovariables <- read_excel(here("data", "JP_macrovariables.xlsx",sheet="Sheet1"))
macrovariables$Date <- as.Date(paste("01", macrovariables$Date, sep="."), format="%d.%b.%Y")


# ==== Read results of BVAR: MPS and INFO series ====

A_list <- c("raw", "G")
B_list <- c("euroyen1","euroyen3","JGB1","JGB2","JGB5","JGB10","JGB20","target","path")
C_list <- c("JGB1")
#C_list <- c("JGB1","JGB2","JGB5")
B_sub_list <- B_list[1:7]


for (A in A_list){
  for (B in B_list){
    for (C in C_list){
      folder_name <- paste0("matlab/final_result_matlab/",A,"_",B,"_",C)
      var_name <- paste0(A,"_",B,"_",C)
      file_path <- file.path(folder_name, "shocks_output.csv")
      
      shocks <- read.csv(file_path, header=FALSE)
      colnames(shocks) <- c("mps","info")
      
      mps_col_name <- paste("mps", var_name, sep="_")
      info_col_name <- paste("info", var_name, sep="_")
      
      yt_select <-yt_select %>% mutate(
        !!sym(mps_col_name) := shocks$mps,
        !!sym(info_col_name) := shocks$info
      )
      rm(shocks)
    }
  }
}

yt_select <- yt_select %>% rename(d_target = target_factor, d_path = path_factor)


# ==== Generate policy dummies ====
# Note: this section has been moved to dummy ols program
# 
# generate policy dummies
# D_list = c("qep","cme","qqe","nip","ycc1","ycc2","ycc3","ycct","ifg")
# 
# # generate dummies for the policies
# yt_select <- yt_select %>% mutate(
#   dummy_qep = ifelse(Date < as.Date("2001-03-01") | Date > as.Date("2006-03-01"), 0,1),
#   dummy_cme = ifelse(Date < as.Date("2010-10-01") | Date > as.Date("2013-03-01"), 0,1),
#   dummy_qqe = ifelse(Date < as.Date("2013-04-01"), 0,1),
#   dummy_nip = ifelse(Date < as.Date("2016-01-01"), 0,1),
#   dummy_ycc1 = ifelse(Date < as.Date("2016-09-01") | Date > as.Date("2018-06-01"), 0,1),
#   dummy_ycc2 = ifelse(Date < as.Date("2018-7-01") | Date > as.Date("2021-02-01"), 0,1),
#   dummy_ycc3 = ifelse(Date < as.Date("2021-03-01"), 0,1),
#   dummy_ycct = ifelse(Date < as.Date("2016-09-01"), 0,1),
#   dummy_ifg = ifelse(Date < as.Date("2019-10-01"), 0,1),
#   dummy_zlb = ifelse(Date < as.Date("2006-07-01") | Date > as.Date("2010-09-01"), 1,0)
# )
# 
# # mark the first day of each announcement
# yt_select <- yt_select %>% mutate(
#   regime_switch = 0,
# )
# 
# for (policy in D_list){
#   dummy_name <- paste0("dummy_", policy)
#   loc <- which(yt_select[[dummy_name]] ==1)[1]
#   yt_select$regime_switch[loc] = 1
# }


# ==== Merge sets to form the final outputs ====


yt_select <- full_join(yt_select, espdata, by="Date") %>% 
  full_join(macrovariables, by="Date") %>% 
  arrange(Date) %>%
  filter(Date >= as.Date("2001-03-01") & Date <= as.Date("2022-11-01"))


# ==== Extra prepares for the factor dummy regressions ====

fadata <- yt_select[,c("mdd_euroyen1","mdd_euroyen3","mdd_JGB1","mdd_JGB2","mdd_JGB5","mdd_JGB10","mdd_JGB20")]
fadata <- scale(fadata)

Aunique <- read.csv(here("data", "Aunique_loading_matrix.csv"), header=TRUE)
Aunique <- as.matrix(Aunique[,2:8])

tAunique <- t(Aunique)
squareAunique <- Aunique %*% tAunique
invt_square <- solve(squareAunique)

# formula: fadata = FU * U'A, where U'A is the Aunique loading matrix
# FU = fadata * t(Aunique) * (Aunique * t(Aqunique))^-1
mdd_factor <- fadata %*% tAunique %*% invt_square

# merge mdd factors into the large dataset
yt_select <- yt_select %>% mutate(
  mdd_target = mdd_factor[,1],
  mdd_path = mdd_factor[,2]
)

# now the data is ready for the dummy regression

yt_esp <- yt_select %>% filter(Date >= as.Date("2004-05-01") & Date <= as.Date("2022-11-01"))

# Note: the 3 d_ESP series have 3 NAs in the first month. But no problem, I won't use them.

# Decide: not going to write to a new file. Instead, run this script before doing ESP VAR
# select the needed variables from three data sets
# temp_yt <- yt_select %>% dplyr::select(
#   Date, euroyen1, euroyen3, JGB1, JGB2, JGB5, JGB10, N225,
#   d_euroyen1, d_euroyen3, d_JGB1, d_JGB2, d_JGB5, d_JGB10, target_factor, path_factor,
#   cbi, tp1, tp2, tp5,
#   mps0, mps03, mps1, mps2, mps5, mps10, info0, info03, info1, info2, info5, info10, mpst, infot, mpsp, infop)
# 
# temp_macro <- macrovariables %>% dplyr::select(Date, CPI, IP, unemployment)
# temp_espdata <- espdata %>% dplyr::select(Date, egdp, ecpi, eune, difgdp, difcpi, difune, d_egdp, d_ecpi, d_eune)
# 
# # combine all variables
# yt_espvar <- full_join(temp_yt, temp_macro, by="Date") %>% full_join(temp_espdata, by="Date") %>% arrange(Date)
# 
# rm(list = setdiff(ls(), "yt_espvar"))
# 
# write.csv(yt_espvar, here("data", "yt_espvar.csv"),row.names=FALSE)
