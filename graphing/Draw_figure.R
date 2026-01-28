# script for drawing figures

rm(list = ls())
library(here)

# ==== Part 1 before BVAR ====

source(here("pre_BVAR_part", "pre_data_processing_200101_202211.R"))
library(reshape2)

# ==== figure 1.2 ====
data_draw2 <- yield_data %>% filter(Date >= as.Date("2013-04-04") & Date <= as.Date("2022-12-19")) %>% dplyr::select(Date, JGB1, JGB2, JGB5, JGB10, JGB20)

yield_data_long <- melt(data_draw2, id.vars = "Date", variable.name = "Maturity", value.name = "Yield")

policy_regimes <- data.frame(
  start_date = as.Date(c("2013-04-04", "2016-01-29", "2016-09-21", "2018-07-31", "2021-03-21")),
  end_date = as.Date(c("2016-01-28", "2016-09-20", "2018-07-30", "2021-03-20", "2022-12-19")),
  regime_name = c("QQE", "NIP", "YCC1", "YCC2", "YCC3")
)

policy_regimes <- policy_regimes %>%
  mutate(mid_date = start_date + (end_date - start_date) / 2)

# Plot using ggplot
ggplot(yield_data_long, aes(x = Date, y = Yield, color = Maturity)) +
  # Add shaded regions for each policy regime
  geom_rect(data = policy_regimes, aes(xmin = start_date, xmax = end_date, ymin = -Inf, ymax = Inf, fill = regime_name),
            alpha = 0.2, inherit.aes = FALSE) +
 #geom_line() +
  # Add regime names directly on the graph
  geom_text(data = policy_regimes, aes(x = mid_date, y = 1.6, label = regime_name),
            color = "black", size = 4, fontface = "bold", vjust = -0.5) +
  geom_line(aes(linetype = Maturity)) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "",
       x = "Date",
       y = "Yield (%)",
       color = "Maturity") +
  theme(legend.position = "bottom", 
        legend.text = element_text(size = 15), 
        legend.title = element_text(size = 15)) +
  guides(color = guide_legend(nrow = 2, override.aes = list(size = 10)), fill = "none") # Remove fill legend


# ==== figure 1.3 (removed) ====
data_draw3 <- temp_daily_diff %>% filter(Date >= as.Date("2013-04-04") & Date <= as.Date("2022-12-19"))  %>% mutate(
  JGB1 = abs(daydiff_JGB1),
  JGB2 = abs(daydiff_JGB2),
  JGB5 = abs(daydiff_JGB5),
  JGB10 = abs(daydiff_JGB10),
  JGB20 = abs(daydiff_JGB20)
) %>% 
  dplyr::select(Date, Date, JGB1, JGB2, JGB5, JGB10, JGB20) 

yield_data_long <- melt(data_draw3, id.vars = "Date", variable.name = "Maturity", value.name = "Yield")

# Plot using ggplot
ggplot(yield_data_long, aes(x = Date, y = Yield, color = Maturity)) +
  # Add shaded regions for each policy regime
  geom_rect(data = policy_regimes, aes(xmin = start_date, xmax = end_date, ymin = -Inf, ymax = Inf, fill = regime_name),
            alpha = 0.2, inherit.aes = FALSE) +
  geom_line() +
  theme_minimal() +
  labs(title = "",
       x = "Date",
       y = "Abs. Daily Changes (%)",
       color = "Maturity",
       fill = "Policy Regime") +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 2), fill = guide_legend(nrow = 2)) +
  facet_wrap(~ Maturity, scales = "free", ncol = 2)



# ==== figure 1.4, MPM time plot ====
data_draw4 <- mpmlist %>% filter(Date >= as.Date("2001-03-19") & Date <= as.Date("2022-12-19")) %>% dplyr::select(Date, Time)

library(lubridate)

# Assuming data_draw4 has columns 'Date' and 'Time' (in AM/PM format)
# Convert 'Time' to a proper time format (POSIXct)
data_draw4$Time <- as.POSIXct(data_draw4$Time, format = "%I:%M %p")

# Filter the data to only include times from 8 AM to midnight
data_draw4_filtered <- data_draw4 %>% 
  filter(format(Time, "%H:%M:%S") >= "08:00:00" & format(Time, "%H:%M:%S") <= "23:59:59")

highlight_time <- as.POSIXct("15:00", format = "%H:%M")

# Plot using ggplot
ggplot(data_draw4_filtered, aes(x = Time, y = Date)) +
  geom_point() +
  geom_vline(xintercept = as.numeric(highlight_time), linetype = "dashed", color = "red") +
  scale_x_datetime(date_breaks = "1 hours", date_labels = "%I %p") +
  scale_y_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_minimal() +
  labs(title = "",
       x = "Time of Day",
       y = "Year") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ==== figure 1.5, JGB and N225 ====
draw_data5 <- mpmdays_diff %>% filter(Date >= as.Date("2001-03-19") & Date <= as.Date("2022-12-19"))%>% dplyr::select(
  Date, d_JGB1, d_JGB2, d_JGB5, d_JGB10, d_JGB20, d_N225)

# Create individual plots with explicit data specification
plot1 <- ggplot(draw_data5, aes(x = d_JGB1, y = d_N225)) +
  geom_point() +
  geom_vline(xintercept = 0, linetype = "solid", color = "gray") +
  geom_hline(yintercept = 0, linetype = "solid", color = "gray") +
  labs(x = "Surprise in 1-year JGB", y = "Surprise in the Nikkei 225 index") +
  annotate("text", x = 0.1, y = 8, label = "I", size = 5) +
  annotate("text", x = 0.1, y = -8, label = "IV", size = 5) +
  annotate("text", x = -0.1, y = 8, label = "II", size = 5) +
  annotate("text", x = -0.1, y = -8, label = "III", size = 5) +
  geom_smooth(method = "lm", color = "blue", fullrange = TRUE) +
  xlim(-0.1, 0.1) + ylim(-8, 8) + theme_minimal()

plot2 <- ggplot(draw_data5, aes(x = d_JGB2, y = d_N225)) +
  geom_point() +
  geom_vline(xintercept = 0, linetype = "solid", color = "gray") +
  geom_hline(yintercept = 0, linetype = "solid", color = "gray") +
  labs(x = "Surprise in 2-year JGB", y = "Surprise in the Nikkei 225 index") +
  annotate("text", x = 0.1, y = 8, label = "I", size = 5) +
  annotate("text", x = 0.1, y = -8, label = "IV", size = 5) +
  annotate("text", x = -0.1, y = 8, label = "II", size = 5) +
  annotate("text", x = -0.1, y = -8, label = "III", size = 5) +
  geom_smooth(method = "lm", color = "blue", fullrange = TRUE) +
  xlim(-0.1, 0.1) + ylim(-8, 8) + theme_minimal()

plot3 <- ggplot(draw_data5, aes(x = d_JGB5, y = d_N225)) +
  geom_point() +
  geom_vline(xintercept = 0, linetype = "solid", color = "gray") +
  geom_hline(yintercept = 0, linetype = "solid", color = "gray") +
  labs(x = "Surprise in 5-year JGB", y = "Surprise in the Nikkei 225 index") +
  annotate("text", x = 0.1, y = 8, label = "I", size = 5) +
  annotate("text", x = 0.1, y = -8, label = "IV", size = 5) +
  annotate("text", x = -0.1, y = 8, label = "II", size = 5) +
  annotate("text", x = -0.1, y = -8, label = "III", size = 5) +
  geom_smooth(method = "lm", color = "blue", fullrange = TRUE) +
  xlim(-0.1, 0.1) + ylim(-8, 8) + theme_minimal()

plot4 <- ggplot(draw_data5, aes(x = d_JGB10, y = d_N225)) +
  geom_point() +
  geom_vline(xintercept = 0, linetype = "solid", color = "gray") +
  geom_hline(yintercept = 0, linetype = "solid", color = "gray") +
  labs(x = "Surprise in 10-year JGB", y = "Surprise in the Nikkei 225 index") +
  annotate("text", x = 0.1, y = 8, label = "I", size = 5) +
  annotate("text", x = 0.1, y = -8, label = "IV", size = 5) +
  annotate("text", x = -0.1, y = 8, label = "II", size = 5) +
  annotate("text", x = -0.1, y = -8, label = "III", size = 5) +
  geom_smooth(method = "lm", color = "blue", fullrange = TRUE) +
  xlim(-0.1, 0.1) + ylim(-8, 8) + theme_minimal()

plot5 <- ggplot(draw_data5, aes(x = d_JGB20, y = d_N225)) +
  geom_point() +
  geom_vline(xintercept = 0, linetype = "solid", color = "gray") +
  geom_hline(yintercept = 0, linetype = "solid", color = "gray") +
  labs(x = "Surprise in 20-year JGB", y = "Surprise in the Nikkei 225 index") +
  annotate("text", x = 0.1, y = 8, label = "I", size = 5) +
  annotate("text", x = 0.1, y = -8, label = "IV", size = 5) +
  annotate("text", x = -0.1, y = 8, label = "II", size = 5) +
  annotate("text", x = -0.1, y = -8, label = "III", size = 5) +
  geom_smooth(method = "lm", color = "blue", fullrange = TRUE) +
  xlim(-0.2, 0.2) + ylim(-8, 8) + theme_minimal()

# Arrange the plots using patchwork
library(patchwork)
# Center `plot5` by adding `plot_spacer()` on both sides to ensure uniform size
# Ensure all plots have the same aspect ratio by modifying their width settings
final_plot <- (plot1 + plot2) / (plot3 + plot4) / (plot5 + plot_spacer()) +
  plot_layout(heights = c(1, 1, 1), widths = c(2, 2, 2))

# Display the final plot
final_plot

# ==== Part 2 after BVAR ====

# ==== figure 1.6 plot mps and info ====
source(here("post_BVAR_part", "post_bvar_data_processing.R"))
library(reshape2)

#graph: 1year
shocks_data <- data.frame(
  Date = yt_select$Date,
  MPS = yt_select$mps_G_JGB1_JGB1,
  INFO = yt_select$info_G_JGB1_JGB1
)

shocks_long <- melt(shocks_data, id.var="Date", variable.name = "Type", value.name = "Value")

jgb1_plot <- ggplot(shocks_long, aes(x = Date, y = Value, fill = Type)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("black", "blue"),
                    labels = c("Monetary policy shock", "Information effect")) +
  theme_minimal() + labs(x = "", y="Indicator: JGB1", fill = "") +
  theme(legend.position = "none", legend.title = element_blank(), axis.text.x = element_text(angle = 60, hjust = 1)) +
  ylim(-0.17, 0.17) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")
rm(shocks_data)
rm(shocks_long)

#graph: 2year
shocks_data <- data.frame(
  Date = yt_select$Date,
  MPS = yt_select$mps_G_JGB2_JGB1,
  INFO = yt_select$info_G_JGB2_JGB1
)

shocks_long <- melt(shocks_data, id.var="Date", variable.name = "Type", value.name = "Value")

jgb2_plot <- ggplot(shocks_long, aes(x = Date, y = Value, fill = Type)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("black", "blue"),
                    labels = c("Monetary policy shock", "Information effect")) +
  theme_minimal() + labs(x = "", y="Indicator:JGB2", fill = "") +
  theme(legend.position = "none", legend.title = element_blank(), axis.text.x = element_text(angle = 60, hjust = 1)) +
  ylim(-0.17, 0.17) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")
rm(shocks_data)
rm(shocks_long)

#graph: 5year
shocks_data <- data.frame(
  Date = yt_select$Date,
  MPS = yt_select$mps_G_JGB5_JGB1,
  INFO = yt_select$info_G_JGB5_JGB1
)

shocks_long <- melt(shocks_data, id.var="Date", variable.name = "Type", value.name = "Value")

jgb5_plot <- ggplot(shocks_long, aes(x = Date, y = Value, fill = Type)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("black", "blue"),
                    labels = c("Monetary policy shock", "Information effect")) +
  theme_minimal() + labs(x = "", y="Indicator: JGB5", fill = "") +
  theme(legend.position = "none", legend.title = element_blank(), axis.text.x = element_text(angle = 60, hjust = 1)) +
  ylim(-0.17, 0.17) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")
rm(shocks_data)
rm(shocks_long)

#graph: 10year
shocks_data <- data.frame(
  Date = yt_select$Date,
  MPS = yt_select$mps_G_JGB10_JGB1,
  INFO = yt_select$info_G_JGB10_JGB1
)

shocks_long <- melt(shocks_data, id.var="Date", variable.name = "Type", value.name = "Value")

jgb10_plot <- ggplot(shocks_long, aes(x = Date, y = Value, fill = Type)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("black", "blue"),
                    labels = c("Monetary policy shock", "Information effect")) +
  theme_minimal() + labs(x = "", y="Indicator: JGB10", fill = "") +
  theme(legend.position = "none", legend.title = element_blank(), axis.text.x = element_text(angle = 60, hjust = 1)) +
  ylim(-0.17, 0.17) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")
rm(shocks_data)
rm(shocks_long)

#graph: 20year
shocks_data <- data.frame(
  Date = yt_select$Date,
  MPS = yt_select$mps_G_JGB20_JGB1,
  INFO = yt_select$info_G_JGB20_JGB1
)

shocks_long <- melt(shocks_data, id.var="Date", variable.name = "Type", value.name = "Value")

jgb20_plot <- ggplot(shocks_long, aes(x = Date, y = Value, fill = Type)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("black", "blue"),
                    labels = c("Monetary policy shock", "Information effect")) +
  theme_minimal() + labs(x = "", y="Indicator: JGB20", fill = "") +
  theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(angle = 60, hjust = 1)) +
  coord_cartesian(ylim = c(-0.17, 0.17)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")
rm(shocks_data)
rm(shocks_long)

library(patchwork)
final_plot <- (jgb1_plot + jgb2_plot) / (jgb5_plot + jgb10_plot) / (jgb20_plot + plot_spacer()) +
  plot_layout(heights = c(1, 1, 1), widths = c(2, 2, 2))

final_plot

# ==== figure 1.8 extra series ====

#figure: plot extra series
source(here("pre_BVAR_part", "pre_data_processing_extra.R"))
library(reshape2)
extra_draw <- read.csv(here("data", "yield_daily.csv"))
extra_draw$Date <- as.Date(extra_draw$Date)
extra_draw <- left_join(extra_draw, yield_data, by="Date")



data_draw2 <- extra_draw %>% mutate(
  CB5 = CB1
) %>% filter(Date >= as.Date("2013-04-04") & Date <= as.Date("2022-12-19")) %>% dplyr::select(Date, JGB10, TSR10, TSR5, CB10, CB5)

yield_data_long <- melt(data_draw2, id.vars = "Date", variable.name = "Asset", value.name = "Yield")

policy_regimes <- data.frame(
  start_date = as.Date(c("2013-04-04", "2016-01-29", "2016-09-21", "2018-07-31", "2021-03-21")),
  end_date = as.Date(c("2016-01-28", "2016-09-20", "2018-07-30", "2021-03-20", "2022-12-19")),
  regime_name = c("QQE", "NIP", "YCC1", "YCC2", "YCC3")
)

## Plot using ggplot
# ggplot(yield_data_long, aes(x = Date, y = Yield, color = Asset)) +
#   # Add shaded regions for each policy regime
#   geom_rect(data = policy_regimes, aes(xmin = start_date, xmax = end_date, ymin = -Inf, ymax = Inf, fill = regime_name),
#             alpha = 0.2, inherit.aes = FALSE) +
#   geom_line() +
#   theme_minimal() +
#   labs(title = "",
#        x = "Date",
#        y = "Yield (%)",
#        color = "Asset",
#        fill = "Policy Regime") +
#   theme(legend.position = "bottom") +
#   guides(color = guide_legend(nrow = 2), fill = guide_legend(nrow = 2))

policy_regimes <- policy_regimes %>%
  mutate(mid_date = start_date + (end_date - start_date) / 2)

# Plot using ggplot
ggplot(yield_data_long, aes(x = Date, y = Yield, color = Asset)) +
  # Add shaded regions for each policy regime
  geom_rect(data = policy_regimes, aes(xmin = start_date, xmax = end_date, ymin = -Inf, ymax = Inf, fill = regime_name),
            alpha = 0.2, inherit.aes = FALSE) +

  # Add regime names directly on the graph
  geom_text(data = policy_regimes, aes(x = mid_date, y = 0.8, label = regime_name),
            color = "black", size = 4, fontface = "bold", vjust = -0.5) +
  geom_line(aes(linetype = Asset)) +
  scale_color_brewer(palette = "Set1") +
  
  #theme_minimal() +
  
  labs(title = "",
       x = "Date",
       y = "Yield (%)",
       color = "Asset") +
  theme(legend.position = "bottom", 
        legend.text = element_text(size = 15), 
        legend.title = element_text(size = 15)) +
  guides(color = guide_legend(nrow = 2, override.aes = list(size = 10)), fill = "none") # Remove fill legend


# ==== figure 1.9 MPS and INFO for extra data ====
# draw MPS and INFO for extra data

source(here("post_BVAR_part", "post_bvar_extradata_process.R"))
library(reshape2)
#graph: TSR10
shocks_data <- data.frame(
  Date = yt_select$Date,
  MPS = yt_select$mps_G_TSR10_JGB1,
  INFO = yt_select$info_G_TSR10_JGB1
)

shocks_long <- melt(shocks_data, id.var="Date", variable.name = "Type", value.name = "Value")

tsr10_plot <- ggplot(shocks_long, aes(x = Date, y = Value, fill = Type)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("black", "blue"),
                    labels = c("Monetary policy shock", "Information effect")) +
  theme_minimal() + labs(x = "", y="Indicator: TSR10", fill = "") +
  theme(legend.position = "none", legend.title = element_blank(), axis.text.x = element_text(angle = 60, hjust = 1)) +
  ylim(-0.12, 0.12) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")
rm(shocks_data)
rm(shocks_long)

#graph: TSR5
shocks_data <- data.frame(
  Date = yt_select$Date,
  MPS = yt_select$mps_G_TSR5_JGB1,
  INFO = yt_select$info_G_TSR5_JGB1
)

shocks_long <- melt(shocks_data, id.var="Date", variable.name = "Type", value.name = "Value")

tsr5_plot <- ggplot(shocks_long, aes(x = Date, y = Value, fill = Type)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("black", "blue"),
                    labels = c("Monetary policy shock", "Information effect")) +
  theme_minimal() + labs(x = "", y="Indicator: TSR5", fill = "") +
  theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(angle = 60, hjust = 1)) +
  ylim(-0.12, 0.12) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")
rm(shocks_data)
rm(shocks_long)

#graph: CB10
shocks_data <- data.frame(
  Date = yt_select$Date,
  MPS = yt_select$mps_G_CB10_JGB1,
  INFO = yt_select$info_G_CB10_JGB1
)

shocks_long <- melt(shocks_data, id.var="Date", variable.name = "Type", value.name = "Value")

cb10_plot <- ggplot(shocks_long, aes(x = Date, y = Value, fill = Type)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("black", "blue"),
                    labels = c("Monetary policy shock", "Information effect")) +
  theme_minimal() + labs(x = "", y="Indicator: CB10", fill = "") +
  theme(legend.position = "none", legend.title = element_blank(), axis.text.x = element_text(angle = 60, hjust = 1)) +
  ylim(-0.12, 0.12) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")
rm(shocks_data)
rm(shocks_long)


#graph: CB1
shocks_data <- data.frame(
  Date = yt_select$Date,
  MPS = yt_select$mps_G_CB1_JGB1,
  INFO = yt_select$info_G_CB1_JGB1
)

shocks_long <- melt(shocks_data, id.var="Date", variable.name = "Type", value.name = "Value")

cb1_plot <- ggplot(shocks_long, aes(x = Date, y = Value, fill = Type)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("black", "blue"),
                    labels = c("Monetary policy shock", "Information effect")) +
  theme_minimal() + labs(x = "", y="Indicator: CB5", fill = "") +
  theme(legend.position = "none", legend.title = element_blank(), axis.text.x = element_text(angle = 60, hjust = 1)) +
  ylim(-0.12, 0.12) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")
rm(shocks_data)
rm(shocks_long)

library(patchwork)

final_plot <- (tsr10_plot + cb10_plot) / (tsr5_plot + cb1_plot) +
  plot_layout(heights = c(1, 1), widths = c(2, 2))

# Display the final plot
final_plot
