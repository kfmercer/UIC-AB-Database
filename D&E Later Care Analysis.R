##D&E complications Abstract## 
##09.30## 

# Upload packages # 
library(tidyverse)
library(ggplot2)
library(foreign)
library(dplyr)
library (lubridate)
library(readxl)
library(knitr)
library(readr)
library(magrittr)
library(rmarkdown)
library(diffdf)
library(tidyr)

# Designate working directory # 
setwd("//HomeDrive/HDriveProd/kfmercer/Desktop/UIC/Abortion Tracking/REDCap")
getwd()
de <- read_xlsx("//HomeDrive/HDriveProd/kfmercer/Desktop/UIC/Abortion Tracking/REDCap/D&E Later Care .xlsx")

#Complications by 3 month groups# 
## 6 Month breakdown ## 
class(de$enc_date)

library(dplyr)
library(lubridate)

three_month_counts <- de %>%
  mutate(
    enc_date = as.Date(enc_date),
    period = case_when(
      month(enc_date) <= 3 ~ paste0(year(enc_date), " Jan-Mar"),
      month(enc_date) <= 6 ~ paste0(year(enc_date), " Apr-Jun"),
      month(enc_date) <= 9 ~ paste0(year(enc_date), " Jul-Sep"),
      TRUE ~ paste0(year(enc_date), " Oct-Dec")
    )
  ) %>%
  filter(
    !is.na(enc_date),
    enc_date >= as.Date("2024-01-01")
  ) %>%
  group_by(period) %>%
  summarize(
    n_records = n_distinct(record_id),
    .groups = "drop"
  )

print(three_month_counts)


preg_quarter <- de %>%
  mutate(
    enc_date = as.Date(enc_date),
    quarter = quarter(enc_date),
    period = paste0(
      year(enc_date), " ",
      c("Jan-Mar", "Apr-Jun", "Jul-Sep", "Oct-Dec")[quarter]
    )
  ) %>%
  filter(
    !is.na(enc_date),
    enc_date >= as.Date("2024-01-01"),
    !is.na(preg_pro_comp_imm___clac)
  ) %>%
  group_by(year = year(enc_date), quarter, period) %>%
  summarize(
    pct_imm = mean(preg_pro_comp_imm___clac== 1, na.rm = TRUE) * 100,
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(year, quarter) %>%
  mutate(period = factor(period, levels = period))

ggplot(preg_quarter,
       aes(x = period,
           y = pct_imm)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Cervical Laceration Rate by Quarter",
    x = "Quarter",
    y = "Percent of Patients >23w6d"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

###Breakdown by GA## 
## Make new variable for later care patients ## 

de$twosix <- ifelse(de$ega_wks >= 26, 1, 0)

de %>% count(twosix)


##Visual 26w## 
preg_quarter <- de %>%
  mutate(
    enc_date = as.Date(enc_date),
    quarter = quarter(enc_date),
    period = paste0(
      year(enc_date), " ",
      c("Jan-Mar", "Apr-Jun", "Jul-Sep", "Oct-Dec")[quarter]
    )
  ) %>%
  filter(
    !is.na(enc_date),
    enc_date >= as.Date("2024-01-01"),
    !is.na(preg_pro_comp_imm___clac),
    twosix == 1
  ) %>%
  group_by(year = year(enc_date), quarter, period) %>%
  summarize(
    pct_imm = mean(preg_pro_comp_imm___clac== 1, na.rm = TRUE) * 100,
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(year, quarter) %>%
  mutate(period = factor(period, levels = period))

ggplot(preg_quarter,
       aes(x = period,
           y = pct_imm)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Cervical Laceration Rate by Quarter",
    x = "Quarter",
    y = "Percent of Patients >25w6d"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#Visual 24-26# 
##Visual## 
preg_quarter <- de %>%
  mutate(
    enc_date = as.Date(enc_date),
    quarter = quarter(enc_date),
    period = paste0(
      year(enc_date), " ",
      c("Jan-Mar", "Apr-Jun", "Jul-Sep", "Oct-Dec")[quarter]
    )
  ) %>%
  filter(
    !is.na(enc_date),
    enc_date >= as.Date("2024-01-01"),
    !is.na(preg_pro_comp_imm___clac),
    twosix == 0
  ) %>%
  group_by(year = year(enc_date), quarter, period) %>%
  summarize(
    pct_imm = mean(preg_pro_comp_imm___clac== 1, na.rm = TRUE) * 100,
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(year, quarter) %>%
  mutate(period = factor(period, levels = period))

ggplot(preg_quarter,
       aes(x = period,
           y = pct_imm)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Cervical Laceration Rate by Quarter",
    x = "Quarter",
    y = "Percent of Patients >25w6d"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
