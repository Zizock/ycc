# This program reads all raw data, calculate daily responses around each MPM, and do factor analysis.
# It exports a file called yt.csv containing all necessary variables to run the BVAR in matlab
# without needing any further operations in matlab.
# Both raw series and Gertler's modified series are included.

# Note: must run in RStudio.

rm(list=ls())
library(tidyverse)  # loads ggplot2, dplyr, tidyr, readxl, lubridate
library(here)
library(zoo)        
library(psych)      


# ==== Import raw data files ====

#import raw daily yield data
raw_yield_data <- read_excel(here("data", "rawassetprice0122.xlsx"), sheet="Sheet1")

#import raw corporate bond yield data
raw_corporate <- read_excel(here("data", "rawassetprice0122.xlsx"), sheet="Sheet2")

#import raw MP event list
mpmlist <- read_excel(here("data", "mpmlist0122.xlsx"), sheet="Sheet1")

#import raw macro series data
raw_macrodata <- read_excel(here("data", "JP_macrovariables.xlsx"), sheet="Sheet2")
diff_macrodata <- read_excel(here("data", "JP_macrovariables.xlsx"), sheet="Sheet1")

# ==== Basic preparations ====

#sort by date
raw_yield_data$Date <- as.Date(raw_yield_data$Date)

yield_data <- raw_yield_data[order(raw_yield_data$Date),]

#fill NA values (very few amount) replace NAs with the last value
yield_data <- yield_data %>% fill(everything(), .direction = "down")

#log the stock
yield_data <- yield_data %>%
  mutate(
    N225raw = N225,
    N225 = log(N225)*100
  )

#Generate 1st difference of all the asset yields
yield_data_diff <- yield_data[-1,]
yield_data_diff[,-1] <- apply(yield_data[,-1],2,diff)

# ==== Match MPM time ====

#convert time, and divide the MPMs by the announcement time: before 2:50PM and after 2:50PM
mpmlist$Time <-strptime(mpmlist$Time, format="%I:%M%p")

#create a new variable in the mpmlist dataset denoting the mark mentioned above
mpmlist <- mpmlist %>% 
  mutate(
    beforeafter = ifelse(Time < strptime("2:50PM", format = "%I:%M%p"),0,1)
  )
mpmlist$Time <- format(mpmlist$Time, "%I:%M%p")

#merge daily yield diff series and MP event series together
yield_data$Date <- as.Date(yield_data$Date, format = "%Y-%m-%d")
mpmlist$Date <- as.Date(mpmlist$Date, format = "%Y-%m-%d")
yield_data_marked <- merge(yield_data,mpmlist, by = "Date", all.x=TRUE)

#calculate yield change: 1-day window.
#Rule: if the announcement was made before 2:50PM, the yield change is measured by the difference
#between today's closing yield(3:00PM). 

#If an announcement was made after close, the yield change is measured by the difference between
#tomorrow's closing yield and today's.

#only one announcement is at 2:59PM, a sensitive time. All other announcements are normal

#define function: calcualte the daily change around MPM
calculate_yield_change <- function(yield, beforeafter) {
  case_when(
    !is.na(beforeafter) & beforeafter == 0 ~ yield - dplyr::lag(yield),
    !is.na(beforeafter) & beforeafter == 1 ~ dplyr::lead(yield) - yield,
    TRUE ~ NA_real_
  )
}

#apply function to all variables I want to check
all_assets <- c("euroyen1", "euroyen3", "JGB1", "JGB2", "JGB5", "JGB10", "JGB20", "JGB30", "N225")
yield_data_marked <- yield_data_marked %>%
  mutate(across(all_of(all_assets), 
                ~ calculate_yield_change(.x, beforeafter), 
                .names = "d_{col}"))

#collect only the MP announcement days
mpmdays_diff <- yield_data_marked %>% filter(!is.na(d_euroyen1))


# ==== Factor analysis ====

# Factor analysis:
# I use the first two factors with eigenvalues>1.
# Assume the format is as X = FUU'A
# where X is the data matrix, F is the TX2 original factor matrix, A is loading matrix, U is the rotation matrix to be derived below

#generate a data frame for FA and scale it to 0 mean 1 var.
fadata <- mpmdays_diff[,c("d_euroyen1", "d_euroyen3", "d_JGB1", "d_JGB2", "d_JGB5","d_JGB10", "d_JGB20")]
fadata <- scale(fadata)
#fa.parallel(fadata, fa = "fa")

#gen factors and collect the first two factors
fa_result <- fa(fadata, nfactors=2, rotate="varimax")

#rotate: derive the two orthogonal factors, one for the shorter end of yield curve and one for the longer end.
#the U matrix in my note above
Umat <- matrix(0, nrow=2, ncol=2)

#the TX2 factor Fmat
Fmat <- fa_result$scores

#the 2X7 A loading mat
Amat <- t(fa_result$loadings)

#calculate rotated factor 2
#assume the second factor doesn't affect the shortest maturity asset [euroyen1]
A1mat <- matrix(Amat[1,1])
b1vec <- matrix(-Amat[2,1])

solution1 <- solve(A1mat,b1vec)

#collect solution to Umat
Umat[1,2] <- solution1[1]
Umat[2,2] <- 1

#rescale
vec_temp <- matrix(Umat[1:2,2])
norm_U2 <- sqrt(sum(vec_temp^2))
vec_temp_rescaled <- vec_temp / norm_U2

Umat[1,2] <- vec_temp_rescaled[1]
Umat[2,2] <- vec_temp_rescaled[2]

#factor1 is orthogonal to factor 2:
A2mat <- matrix(Umat[1,2])
b2vec <- matrix(-Umat[2,2])

solution2 <- solve(A2mat,b2vec)

#collect solution to Umat
Umat[1,1] <- solution2[1]
Umat[2,1] <- 1

#rescale
vec_temp <- matrix(Umat[1:2,1])
norm_U1 <- sqrt(sum(vec_temp^2))
vec_temp_rescaled <- vec_temp / norm_U1

Umat[1,1] <- vec_temp_rescaled[1]
Umat[2,1] <- vec_temp_rescaled[2]

#last step: gen real F and A matrices (the format is X=FUU'A+eps). U is the rotation matrix
Funique <- Fmat%*%Umat
Aunique <- t(Umat)%*%Amat

#get rounded clear results:
Aunique_round <- round(Aunique, digits=3)

#save the derived factors into a new data frame
divided_factors <- as.data.frame(Fmat%*% Umat)

# save the laoding matrix:
write.csv(Aunique, file=here("data", "Aunique_loading_matrix.csv"))

# put the factors into the mpmdays_diff data frame
mpmdays_diff <- mpmdays_diff %>% mutate(
  target_factor = divided_factors$V1,
  path_factor = divided_factors$V2
)


# ==== Summarize HFI by month and generate the monthly HFI series ====

#generate high frequency part in the model (summing by month)
yt <- mpmdays_diff %>%
  mutate(yearmonth = floor_date(Date, "month")) %>% group_by(yearmonth) %>%
  summarise(
    d_euroyen1 = sum(d_euroyen1, na.rm=TRUE),
    d_euroyen3 = sum(d_euroyen3, na.rm=TRUE),
    d_JGB1 = sum(d_JGB1, na.rm=TRUE),
    d_JGB2 = sum(d_JGB2, na.rm=TRUE),
    d_JGB5 = sum(d_JGB5, na.rm=TRUE),
    d_JGB10 = sum(d_JGB10, na.rm=TRUE),
    d_N225 = sum(d_N225, na.rm=TRUE),
    target_factor = sum(target_factor, na.rm=TRUE),
    path_factor = sum(path_factor, na.rm=TRUE),
    
    d_JGB20 = sum(d_JGB20, na.rm=TRUE)
  ) %>%
  complete(yearmonth = seq(as.Date("2001-01-01"), as.Date("2022-11-01"), by = "month"),
           fill = list(d_euroyen1 = 0, d_euroyen3 = 0, d_JGB1 = 0, d_JGB2 = 0, d_JGB5 = 0, 
                       d_JGB10 = 0, d_JGB20 = 0, d_N225 = 0, target_factor = 0, path_factor = 0))


# ==== Gertler's method to accumulate shocks ====

# an empty daily structure
full_daily <- data.frame(Date = seq(as.Date("2001-01-01"), as.Date("2023-01-31"), by="day"))

# add variables into it
full_daily <- full_daily %>% left_join(mpmdays_diff, by="Date") %>% replace(is.na(.),0)

# mutate by month: accumulate the total shocks in the past 31 days for every day.
full_daily <- full_daily %>% mutate(
  cumu_d_euroyen1 = rollapply(d_euroyen1, width=31, FUN = sum, fill=0, align = "right", partial = TRUE),
  cumu_d_euroyen3 = rollapply(d_euroyen3, width=31, FUN = sum, fill=0, align = "right", partial = TRUE),
  cumu_d_JGB1 = rollapply(d_JGB1, width=31, FUN = sum, fill=0, align = "right", partial = TRUE),
  cumu_d_JGB2 = rollapply(d_JGB2, width=31, FUN = sum, fill=0, align = "right", partial = TRUE),
  cumu_d_JGB5 = rollapply(d_JGB5, width=31, FUN = sum, fill=0, align = "right", partial = TRUE),
  cumu_d_JGB10 = rollapply(d_JGB10, width=31, FUN = sum, fill=0, align = "right", partial = TRUE),
  cumu_d_N225 = rollapply(d_N225, width=31, FUN = sum, fill=0, align = "right", partial = TRUE),
  cumu_target_factor = rollapply(target_factor, width=31, FUN = sum, fill=0, align = "right", partial = TRUE),
  cumu_path_factor = rollapply(path_factor, width=31, FUN = sum, fill=0, align = "right", partial = TRUE),
  
  cumu_d_JGB20 = rollapply(d_JGB20, width=31, FUN = sum, fill=0, align = "right", partial = TRUE),
  
  abcumu_d_euroyen1 = rollapply(d_euroyen1, width=31, FUN = function(x) sum(abs(x)), fill=0, align = "right", partial = TRUE),
  abcumu_d_euroyen3 = rollapply(d_euroyen3, width=31, FUN = function(x) sum(abs(x)), fill=0, align = "right", partial = TRUE),
  abcumu_d_JGB1 = rollapply(d_JGB1, width=31, FUN = function(x) sum(abs(x)), fill=0, align = "right", partial = TRUE),
  abcumu_d_JGB2 = rollapply(d_JGB2, width=31, FUN = function(x) sum(abs(x)), fill=0, align = "right", partial = TRUE),
  abcumu_d_JGB5 = rollapply(d_JGB5, width=31, FUN = function(x) sum(abs(x)), fill=0, align = "right", partial = TRUE),
  abcumu_d_JGB10 = rollapply(d_JGB10, width=31, FUN = function(x) sum(abs(x)), fill=0, align = "right", partial = TRUE),
  abcumu_d_N225 = rollapply(d_N225, width=31, FUN = function(x) sum(abs(x)), fill=0, align = "right", partial = TRUE),
  abcumu_target_factor = rollapply(target_factor, width=31, FUN = function(x) sum(abs(x)), fill=0, align = "right", partial = TRUE),
  abcumu_path_factor = rollapply(path_factor, width=31, FUN = function(x) sum(abs(x)), fill=0, align = "right", partial = TRUE),
  
  abcumu_d_JGB20 = rollapply(d_JGB20, width=31, FUN = function(x) sum(abs(x)), fill=0, align = "right", partial = TRUE)
)

# calculate monthly average
monthly_average_shock <- full_daily %>%
  mutate(yearmonth = floor_date(Date, "month")) %>% group_by(yearmonth) %>%
  summarise(
    g_euroyen1 = mean(cumu_d_euroyen1, na.rm=TRUE),
    g_euroyen3 = mean(cumu_d_euroyen3, na.rm=TRUE),
    g_JGB1 = mean(cumu_d_JGB1, na.rm=TRUE),
    g_JGB2 = mean(cumu_d_JGB2, na.rm=TRUE),
    g_JGB5 = mean(cumu_d_JGB5, na.rm=TRUE),
    g_JGB10 = mean(cumu_d_JGB10, na.rm=TRUE),
    g_target_factor = mean(cumu_target_factor, na.rm=TRUE),
    g_path_factor = mean(cumu_path_factor, na.rm=TRUE),
    g_N225 = mean(cumu_d_N225, na.rm=TRUE),
    
    g_JGB20 = mean(cumu_d_JGB20, na.rm=TRUE)
    ) %>% ungroup() %>% rename(Date = yearmonth)
 
monthly_average_shock <- monthly_average_shock %>% mutate(   
    lg_euroyen1 = dplyr::lead(g_euroyen1),
    lg_euroyen3 = dplyr::lead(g_euroyen1),
    lg_JGB1 = dplyr::lead(g_JGB1),
    lg_JGB2 = dplyr::lead(g_JGB2),
    lg_JGB5 = dplyr::lead(g_JGB5),
    lg_JGB10 = dplyr::lead(g_JGB10),
    lg_JGB20 = dplyr::lead(g_JGB20),
    lg_target_factor = dplyr::lead(g_target_factor),
    lg_path_factor = dplyr::lead(g_path_factor),
    lg_N225 = dplyr::lead(g_N225)
  )


# ==== Low frequency yield series ====

#merge low frequency variables:
monthly_yield <- yield_data %>%
  mutate(yearmonth = floor_date(Date, "month")) %>%
  group_by(yearmonth) %>%
  summarise(
    euroyen1 = mean(euroyen1, na.rm=TRUE),
    euroyen3 = mean(euroyen3, na.rm=TRUE),
    JGB1 = mean(JGB1, na.rm=TRUE),
    JGB2 = mean(JGB2, na.rm=TRUE),
    JGB5 = mean(JGB5, na.rm=TRUE),
    JGB10 = mean(JGB10, na.rm=TRUE),
    JGB20 = mean(JGB20, na.rm=TRUE),
    N225 = log(mean(N225raw,na.rm=TRUE))*100
  ) %>% ungroup()

monthly_yield <- monthly_yield %>%
  mutate(
    md_euroyen1 = euroyen1 - lag(euroyen1),
    md_euroyen3 = euroyen3 - lag(euroyen3),
    md_JGB1 = JGB1 - lag(JGB1),
    md_JGB2 = JGB2 - lag(JGB2),
    md_JGB5 = JGB5 - lag(JGB5),
    md_JGB10 = JGB10 - lag(JGB10),
    md_JGB20 = JGB20 - lag(JGB20),
    md_N225 = N225 - lag(N225),
    tp = JGB10-JGB2,
    md_tp = (JGB10 - lag(JGB10)) - (JGB2 - lag(JGB2))) %>%
  replace_na(list(JGB2 = 0, JGB10 = 0, N225 = 0))

# Calculate monthly averages of daily yield change
library(purrr)
temp_daily_diff <- yield_data %>%
  mutate(across(all_assets, ~ c(NA, diff(.)), .names = "daydiff_{.col}"))

monthly_average_diff <- temp_daily_diff %>%
  filter(!is.na(daydiff_euroyen1)) %>%
  mutate(yearmonth = floor_date(Date, "month")) %>%
  group_by(yearmonth) %>%
  summarise(
    mdd_euroyen1 = mean(abs(daydiff_euroyen1), na.rm=TRUE),
    mdd_euroyen3 = mean(abs(daydiff_euroyen3), na.rm=TRUE),
    mdd_JGB1 = mean(abs(daydiff_JGB1), na.rm=TRUE),
    mdd_JGB2 = mean(abs(daydiff_JGB2), na.rm=TRUE),
    mdd_JGB5 = mean(abs(daydiff_JGB5), na.rm=TRUE),
    mdd_JGB10 = mean(abs(daydiff_JGB10), na.rm=TRUE),
    mdd_JGB20 = mean(abs(daydiff_JGB20), na.rm=TRUE)
  ) %>% ungroup()


# ==== Monthly macro variables ====

#read macro variables
#Sheet2: raw macro data
#Sheet1: yearly change
raw_macrodata$Date <- as.Date(paste("01", raw_macrodata$Date, sep="."), format="%d.%b.%Y")

diff_macrodata$Date <- as.Date(paste("01", diff_macrodata$Date, sep="."), format="%d.%b.%Y")

# corporate bond section added:
#sort by date
raw_corporate$Date <- as.Date(raw_corporate$Date)
corporate <- raw_corporate[order(raw_corporate$Date),]
corporate <- corporate %>% fill(everything(), .direction = "down")

monthly_corporate <- corporate %>%
  mutate(yearmonth = floor_date(Date, "month")) %>% group_by(yearmonth) %>%
  summarise(
    cbi = mean(CB1, na.rm=TRUE),
    tp1 = mean(CB1-monthly_yield$JGB1, na.rm=TRUE),
    tp2 = mean(CB1-monthly_yield$JGB2, na.rm=TRUE),
    tp5 = mean(CB1-monthly_yield$JGB5, na.rm=TRUE)
  ) %>% ungroup()

# ==== Merge all data variables ====
#merge dataset
yt <- yt %>% rename(Date = yearmonth)

monthly_yield <- monthly_yield %>% rename(Date = yearmonth)
yt <- merge(yt, monthly_yield, by="Date")

yt <- merge(yt, raw_macrodata, by="Date")

yt <- merge(yt, diff_macrodata, by="Date")

monthly_corporate <- monthly_corporate %>% rename(Date=yearmonth)
yt <- merge(yt, monthly_corporate, by="Date")

yt <- merge(yt, monthly_average_shock, by="Date")

monthly_average_diff <- monthly_average_diff %>% rename(Date = yearmonth)
yt <- merge(yt, monthly_average_diff, by="Date")


yt <- yt %>% mutate(
  year = year(Date),
  month = month(Date)
)


# ==== Select period of interest and variables needed for MatLab ====

yt_select <- yt %>% filter(Date >= as.Date("2001-03-01") & Date <= as.Date("2022-11-01")) %>% 
  dplyr::select(year, month,
         d_euroyen1, d_euroyen3, d_JGB1, d_JGB2, d_JGB5, d_JGB10, d_JGB20, d_N225, target_factor, path_factor,
         g_euroyen1, g_euroyen3, g_JGB1, g_JGB2, g_JGB5, g_JGB10, g_JGB20, g_N225, g_target_factor, g_path_factor,
         lg_euroyen1, lg_euroyen3, lg_JGB1, lg_JGB2, lg_JGB5, lg_JGB10, lg_JGB20, lg_N225, lg_target_factor, lg_path_factor,
         euroyen1, euroyen3, JGB1, JGB2, JGB5, JGB10, JGB20, N225, 
         md_euroyen1, md_euroyen3, md_JGB1, md_JGB2, md_JGB5, md_JGB10, md_JGB20, md_N225,
         mdd_euroyen1, mdd_euroyen3, mdd_JGB1, mdd_JGB2, mdd_JGB5, mdd_JGB10, mdd_JGB20,
         IP, CPI, md_IP, md_CPI,
         tp, md_tp, cbi, tp1, tp2, tp5)

# A back up of the total set
write.csv(yt, here("data", "yt.csv"), row.names = FALSE, quote = FALSE)
# The file used in MatLab calculation
write.csv(yt_select, here("matlab", "data_var", "yt_select.csv"),row.names = FALSE, quote = FALSE)
