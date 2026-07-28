##Abortion Tracking Radio Data## 
##K.Mercer 07/28/2026 ##

##Set WD## 
setwd("//HomeDrive/HDriveProd/kfmercer/Desktop/UIC/UIC")
getwd()


## packages## 
library(dplyr)
library(lubridate)

##Import Dataset##
uic_radio <- read.csv("//HomeDrive/HDriveProd/kfmercer/Desktop/UIC/Abortion Tracking/REDCap/AbortionTracking2024-UICCARLARates_DATA_2026-07-28_1210.csv")

##Generate Report Year variables ## 
august_counts <- uic_radio %>%
  mutate(enc_date = as.Date(enc_date),
         august_year= if_else(
           month(enc_date) >= 8, 
           year(enc_date), 
           year(enc_date) - 1
         )
  ) %>%
  filter(august_year %in%
           c(2023, 2024,2025,2026)) %>%
  group_by(august_year) %>%
  summarize(
    n_record_id = n_distinct(record_id), .groups = "drop")

print(august_counts)

## Total Abortion Patients ##
uic_radio %>% count()

##Total CARLA patients ## 
uic_radio %>% count(carla_yn)


## In and Out of State Breakdown## 

uic_radio$state_dist = if_else(uic_radio$dem_state_fullname == "Illinois", "IL", "OOS")
frequency <- table(uic_radio$state_dist)
pct_state_dist <- round(100*frequency/sum(frequency),1)

uic_state_dist <- barplot(
  table(uic_radio$state_dist),
  main = "Distribution of State of Residence (IL vs OOS)",
  xlab = "",
  ylab = "Patient Number",
  las = 2  # rotate labels if needed
)

table(uic_radio$state_dist[
  !is.na(uic_radio$state_dist)&
    uic_radio$state_dist != ""])


##CARLA IN and OOS## 
table <- table(uic_radio$state_dist,uic_radio$carla_yn)
table

