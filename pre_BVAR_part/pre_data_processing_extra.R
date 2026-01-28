# This script prepares the extra assets data for the MATLAB BVAR

# same as pre_data_processing.R

rm(list=ls())
library(here)
library(tidyverse)
library(zoo)
library(psych)

# ================================================================
# Import raw data files
# ================================================================

#import swap rate daily yield data
raw_tsr10_data <- read_excel(here("data", "raw_extra_yields.xlsx"), sheet="Sheet1",na = c("", "N/A", "NULL"))

#import swap rate daily yield data
raw_tsr5_data <- read_excel(here("data", "raw_extra_yields.xlsx"), sheet="Sheet2",na = c("", "N/A", "NULL"))

#import corporate bond daily yield data
#raw_cor_data <- read_excel(here("data", "raw_extra_yields.xlsx"), sheet="Sheet2",na = c("", "N/A", "NULL"))

#import raw N225 data
raw_n225_data <- read_excel(here("data", "raw_jgb10f.xlsx"), sheet="Sheet2",na = c("", "N/A", "NULL"))

#import raw corporate bond yield data
raw_corporate <- read_excel(here("data", "rawassetprice0122.xlsx"), sheet="Sheet2",na = c("", "N/A", "NULL"))

raw_corporate10 <- read.csv(here("data", "eikondata241124.csv"))

#import raw MP event list
mpmlist <- read_excel(here("data", "mpmlist0122.xlsx"), sheet="Sheet2")

#import raw macro series data
raw_macrodata <- read_excel(here("data", "JP_macrovariables.xlsx"), sheet="Sheet2")
diff_macrodata <- read_excel(here("data", "JP_macrovariables.xlsx"), sheet="Sheet1")

# ================================================================
# Basic preparations
# ================================================================
raw_tsr5_data$Date <-as.Date(raw_tsr5_data$Date)
raw_tsr10_data$Date <-as.Date(raw_tsr10_data$Date)
raw_corporate$Date <- as.Date(raw_corporate$Date)
raw_corporate10$Date <- as.Date(raw_corporate10$Date, format = "%m/%d/%Y")
raw_n225_data$Date <- as.Date(raw_n225_data$Date)
mpmlist$Date <- as.Date(mpmlist$Date)

# Merging the two data sets based on the 'Date' column
raw_yield_data <- inner_join(raw_n225_data, raw_tsr10_data, by = "Date")
raw_yield_data <- inner_join(raw_yield_data, raw_tsr5_data, by = "Date")
raw_yield_data <- inner_join(raw_yield_data, raw_corporate, by = "Date")
raw_yield_data <- inner_join(raw_yield_data, raw_corporate10, by = "Date")
yield_data <- raw_yield_data[order(raw_yield_data$Date),]
yield_data <- select(yield_data, -CB35, -CB57)


# check if all MPM dates are covered
missing_dates <- mpmlist$Date[!(mpmlist$Date %in% yield_data$Date)]

# Print missing dates
if (length(missing_dates) == 0) {
  print("All dates in mpmlist are covered in yield_data.")
} else {
  print("The following dates are missing from yield_data:")
  print(missing_dates)
}
# results: yes

#log the stock
yield_data <- yield_data %>%
  mutate(
    N225raw = N225,
    N225 = log(N225)*100
  )

#Generate 1st difference of all the asset yields
yield_data_diff <- yield_data[-1,]
yield_data_diff[,-1] <- apply(yield_data[,-1],2,diff)


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
all_assets <- c("TSR5", "TSR10", "CB1", "CB5", "CB10", "N225")
yield_data_marked <- yield_data_marked %>%
  mutate(across(all_of(all_assets), 
                ~ calculate_yield_change(.x, beforeafter), 
                .names = "d_{col}"))

#collect only the MP announcement days
mpmdays_diff <- yield_data_marked %>% filter(!is.na(d_TSR5))


# ================================================================
# Summarize HFI by month and generate the monthly HFI series
# ================================================================

#generate high frequency part in the model (summing by month)
yt <- mpmdays_diff %>%
  mutate(yearmonth = floor_date(Date, "month")) %>% group_by(yearmonth) %>%
  summarise(
    d_TSR5 = sum(d_TSR5, na.rm=TRUE),
    d_TSR10 = sum(d_TSR10, na.rm=TRUE),
    d_CB1 = sum(d_CB1, na.rm=TRUE),
    d_CB5 = sum(d_CB5, na.rm=TRUE),
    d_CB10 = sum(d_CB10, na.rm=TRUE),
    d_N225 = sum(d_N225, na.rm=TRUE),
  ) %>%
  complete(yearmonth = seq(as.Date("2001-01-01"), as.Date("2022-11-01"), by = "month"),
           fill = list(d_TSR5 = 0, d_TSR10 = 0, d_CB1 = 0, d_CB5 = 0, d_CB10 = 0, d_N225 = 0))

# ================================================================
# Gertler's method to accumulate shocks
# ================================================================

# an empty daily structure
full_daily <- data.frame(Date = seq(as.Date("2001-01-01"), as.Date("2022-12-31"), by="day"))

# add variables into it
full_daily <- full_daily %>% left_join(mpmdays_diff, by="Date") %>% replace(is.na(.),0)

# mutate by month: accumulate the total shocks in the past 31 days for every day.
full_daily <- full_daily %>% mutate(
  cumu_d_TSR5 = rollapply(d_TSR5, width=31, FUN = sum, fill=0, align = "right", partial = TRUE),
  cumu_d_TSR10 = rollapply(d_TSR10, width=31, FUN = sum, fill=0, align = "right", partial = TRUE),
  cumu_d_CB1 = rollapply(d_CB1, width=31, FUN = sum, fill=0, align = "right", partial = TRUE),
  cumu_d_CB5 = rollapply(d_CB5, width=31, FUN = sum, fill=0, align = "right", partial = TRUE),
  cumu_d_CB10 = rollapply(d_CB10, width=31, FUN = sum, fill=0, align = "right", partial = TRUE),
  cumu_d_N225 = rollapply(d_N225, width=31, FUN = sum, fill=0, align = "right", partial = TRUE),
  
  abcumu_d_TSR5 = rollapply(d_TSR5, width=31, FUN = function(x) sum(abs(x)), fill=0, align = "right", partial = TRUE),
  abcumu_d_TSR10 = rollapply(d_TSR10, width=31, FUN = function(x) sum(abs(x)), fill=0, align = "right", partial = TRUE),
  abcumu_d_CB1 = rollapply(d_CB1, width=31, FUN = function(x) sum(abs(x)), fill=0, align = "right", partial = TRUE),
  abcumu_d_CB5 = rollapply(d_CB5, width=31, FUN = function(x) sum(abs(x)), fill=0, align = "right", partial = TRUE),
  abcumu_d_CB10 = rollapply(d_CB10, width=31, FUN = function(x) sum(abs(x)), fill=0, align = "right", partial = TRUE),
  abcumu_d_N225 = rollapply(d_N225, width=31, FUN = function(x) sum(abs(x)), fill=0, align = "right", partial = TRUE)
)

# calculate monthly average
monthly_average_shock <- full_daily %>%
  mutate(yearmonth = floor_date(Date, "month")) %>% group_by(yearmonth) %>%
  summarise(
    g_TSR5 = mean(cumu_d_TSR5, na.rm=TRUE),
    g_TSR10 = mean(cumu_d_TSR10, na.rm=TRUE),
    g_CB1 = mean(cumu_d_CB1, na.rm=TRUE),
    g_CB5 = mean(cumu_d_CB1, na.rm=TRUE),
    g_CB10 = mean(cumu_d_CB10, na.rm=TRUE),
    g_N225 = mean(cumu_d_N225, na.rm=TRUE)
  ) %>% ungroup() %>% rename(Date = yearmonth)

monthly_average_shock <- monthly_average_shock %>% mutate(   
  lg_TSR5 = dplyr::lead(g_TSR5),
  lg_TSR10 = dplyr::lead(g_TSR10),
  lg_CB1 = dplyr::lead(g_CB1),
  lg_CB5 = dplyr::lead(g_CB5),
  lg_CB10 = dplyr::lead(g_CB10),
  lg_N225 = dplyr::lead(g_N225)
)

# ================================================================
# Low frequency yield series
# ================================================================

#merge low frequency variables:
monthly_yield <- yield_data %>%
  mutate(yearmonth = floor_date(Date, "month")) %>%
  group_by(yearmonth) %>%
  summarise(
    TSR5 = mean(TSR5, na.rm=TRUE),
    TSR10 = mean(TSR10, na.rm=TRUE),
    CB1 = mean(CB1, na.rm=TRUE),
    CB5 = mean(CB5, na.rm=TRUE),
    CB10 = mean(CB10, na.rm=TRUE),
    N225 = log(mean(N225raw,na.rm=TRUE))*100
  ) %>% ungroup()

monthly_yield <- monthly_yield %>%
  mutate(
    md_TSR5 = TSR5 - lag(TSR5),
    md_TSR10 = TSR10 - lag(TSR10),
    md_CB1 = CB1 - lag(CB1),
    md_CB5 = CB5 - lag(CB5),
    md_CB10 = CB10 - lag(CB10),
    md_N225 = N225 - lag(N225))

# Calculate monthly averages of daily yield change
library(purrr)
temp_daily_diff <- yield_data %>%
  mutate(across(all_assets, ~ c(NA, diff(.)), .names = "daydiff_{.col}"))

monthly_average_diff <- temp_daily_diff %>%
  filter(!is.na(daydiff_TSR5)) %>%
  mutate(yearmonth = floor_date(Date, "month")) %>%
  group_by(yearmonth) %>%
  summarise(
    mdd_TSR5 = mean(abs(daydiff_TSR5), na.rm=TRUE),
    mdd_TSR10 = mean(abs(daydiff_TSR10), na.rm=TRUE),
    mdd_CB1 = mean(abs(daydiff_CB1), na.rm=TRUE),
    mdd_CB5 = mean(abs(daydiff_CB5), na.rm=TRUE),
    mdd_CB10 = mean(abs(daydiff_CB10), na.rm=TRUE),
    mdd_N225 = mean(abs(daydiff_N225), na.rm=TRUE)
  ) %>% ungroup()


# ================================================================
# Merge all data variables
# ================================================================
#merge dataset
yt <- yt %>% rename(Date = yearmonth)

monthly_yield <- monthly_yield %>% rename(Date = yearmonth)
yt <- merge(yt, monthly_yield, by="Date")

raw_macrodata$Date <- as.Date(paste("01", raw_macrodata$Date, sep="."), format="%d.%b.%Y")
yt <- merge(yt, raw_macrodata, by="Date")

#yt <- merge(yt, diff_macrodata, by="Date")

#monthly_corporate <- monthly_corporate %>% rename(Date=yearmonth)
#yt <- merge(yt, monthly_corporate, by="Date")

yt <- merge(yt, monthly_average_shock, by="Date")

monthly_average_diff <- monthly_average_diff %>% rename(Date = yearmonth)
yt <- merge(yt, monthly_average_diff, by="Date")


yt <- yt %>% mutate(
  year = year(Date),
  month = month(Date)
)

# ================================================================
# Select period of interest and variables needed for MatLab
# ================================================================

yt_select_extra <- yt %>% filter(Date >= as.Date("2001-03-01") & Date <= as.Date("2022-11-01")) %>% 
  dplyr::select(year, month,
                d_TSR5, d_TSR10, d_CB1, d_CB5, d_CB10, d_N225,
                g_TSR5, g_TSR10, g_CB1, g_CB5, g_CB10, g_N225,
                lg_TSR5, lg_TSR10, lg_CB1, lg_CB5, lg_CB10, lg_N225,
                TSR5, TSR10, CB1, CB5, CB10, N225,
                md_TSR5, md_TSR10, md_CB1, md_CB5, md_CB10, md_N225,
                mdd_TSR5, mdd_TSR10, mdd_CB1, mdd_CB5, mdd_CB10, mdd_N225,
                IP, CPI)

# A back up of the total set
write.csv(yt, here("data", "yt_extra.csv"), row.names = FALSE, quote = FALSE)
# The file used in MatLab calculation
write.csv(yt_select_extra, here("matlab", "data_var", "yt_select_extra.csv"),row.names = FALSE, quote = FALSE)

#write.csv(yield_data, here("matlab", "data_var", "yield_daily_extra.csv"),row.names = FALSE, quote = FALSE)



# # Parameters
# futures_price <- 145  # Example JGB 10-year futures index value
# face_value <- 100     # Face value of the bond
# coupon_rate <- 0.06  # Coupon rate (0.1%)
# years_to_maturity <- 10
# 
# # Calculate coupon payment
# coupon_payment <- face_value * coupon_rate
# 
# # Calculate the yield
# yield <- (coupon_payment / futures_price) + ((face_value - futures_price) / (futures_price * years_to_maturity))
# 
# # Output the implied yield
# yield













