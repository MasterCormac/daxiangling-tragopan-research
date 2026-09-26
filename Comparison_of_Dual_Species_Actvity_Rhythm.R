library(tidyverse)
library(readxl)
library(lubridate)
library(activity)
library(overlap)
library(showtext)

setwd("/Users/zz/Downloads/大相岭-生物活动")
file_paths <- c("全-红腹角雉.xlsx", "人.xlsx")

#---preprocess

# to numeric
combined_data <- file_paths %>%
  map_df(~ read_excel(.x) %>%
           mutate(across(contains("度"), as.numeric),
                  `水源距离（米）` = as.numeric(`水源距离（米）`)
           )
  )

data <- combined_data

# Parse time column (拍摄时间) to a datetime object
data <- data %>%
  mutate(拍摄时间 = ymd_hms(拍摄时间))

# Filter independent valid photos
data <- data %>%
  arrange(物种, 拍摄时间) %>%
  group_by(物种) %>%
  mutate(time_diff = difftime(拍摄时间, lag(拍摄时间), units = "mins")) %>%
  filter(is.na(time_diff) | time_diff > 30) %>%  # Filter out photos within 30 minutes of the last one
  ungroup()

data_store <- data

# Convert time to radians
data <- data %>%
  mutate(time_in_minutes = hour(拍摄时间) * 60 + minute(拍摄时间) + second(拍摄时间) / 60,
         time_in_radians = (time_in_minutes / 1440) * 2 * pi)

species_list <- unique(data$物种)

species1 <- species_list[1]
species2 <- species_list[2]
species1_data <- data %>% filter(物种 == species1)
species2_data <- data %>% filter(物种 == species2)

#--- daily activity rhythm
for (sp in species_list) {
  species_data <- data %>% filter(物种 == sp)
  
  densityPlot(species_data$time_in_radians, 
              main = paste("Daily Activity Rhythm of", sp), 
              xlab = "Time (radians)")
}

# 活动节律比较
overlap_estimate <- overlapEst(species1_data$time_in_radians, species2_data$time_in_radians, type="Dhat4")
print(paste("Daily Overlap coefficient between", species1, "and", species2, ":", overlap_estimate))


# Calculate the overlap and plot it
overlapPlot(species1_data$time_in_radians, species2_data$time_in_radians, xlab = "Time (radians)")
legend("topright", c("人", "红腹角雉"), lty=c(1,2), col=c(1,4), bty='n')

#---annual comparison
# Extract the month and convert to radians
data <- data_store
data <- data %>%
  mutate(month = month(拍摄时间))

data <- data %>%
  mutate(month_in_radians = (month - 1) * (2 * pi / 12))  # Adjust for 0-based indexing

species_list <- unique(data$物种)


species1 <- species_list[1]
species2 <- species_list[2]
species1_data <- data %>% filter(物种 == species1)
species2_data <- data %>% filter(物种 == species2)

# Adjust the custom x-axis
month_labels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec","")
month_positions <- seq(0, 2 * pi + ( 68/12 * pi ), length.out = 13) 

# Plot annual activity rhythm
for (sp in species_list) {
  species_data <- data %>% filter(物种 == sp)
  densityPlot(species_data$month_in_radians, main = paste("Annual Activity Rhythm of", sp), 
              xlab = "Month", xaxt = "n")
  
  # Add tilted labels
  axis(1, at = month_positions, labels = FALSE)  # Suppress default labels
  text(x = month_positions, y = par("usr")[3] - 0.02, labels = month_labels, 
       srt = 45, adj = 1, xpd = TRUE)
}

# Adjust month_positions
month_positions <- seq(0, 2 * pi + ( 68/12 * pi ), length.out = 13) 

# Calculate the overlap and plot it 
overlapPlot(species1_data$month_in_radians, species2_data$month_in_radians, xlab = "Month", 
            xaxt = "n", title = paste("Annual Activity Rhythm Comparison of 红腹角雉 and 人", sp))
# Add tilted labels
axis(1, at = month_positions, labels = FALSE)  # Suppress default labels
text(x = month_positions, y = par("usr")[3] - 0.02, labels = month_labels, 
     srt = 45, adj = 1, xpd = TRUE)

legend("topright", c("人", "红腹角雉"), lty=c(1,2), col=c(1,4), bty='n')
