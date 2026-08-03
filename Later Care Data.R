##Later Care Analysis at UIC ## 
##K.Mercer 07/28/26## 

# Designate working directory # 
setwd("//HomeDrive/HDriveProd/kfmercer/Desktop/UIC/Abortion Tracking/REDCap")
getwd()
later <- read.csv("//HomeDrive/HDriveProd/kfmercer/Desktop/UIC/Abortion Tracking/REDCap/AbortionTracking2024-LaterCarePatients_DATA_2026-07-31_0954.csv")

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

## Make new variable for later care patients ## 

later$later <- ifelse(later$ega_wks > 21, 1, 0)

later %>% count(later)

##Make 24W variable##
later$later24 <- ifelse(later$ega_wks > 23, 1, 0)

later %>% count(later24)

##Make 26W variable##
later$later26 <- ifelse(later$ega_wks > 25, 1, 0)

later %>% count(later26)

##Table for GA and procedure type ## 

procedure_type_ga <- table(later$later, later$preg_pro_primtype)
procedure_type_ga 

procedure_type_ga24 <- table(later$later24, later$preg_pro_primtype)
procedure_type_ga24 

procedure_type_ga26 <- table(later$later26, later$preg_pro_primtype)
procedure_type_ga26 

## 6 Month breakdown ## 
class(later$enc_date)

six_month_counts <- later %>%
  mutate(
    enc_date = as.Date(enc_date, format = "%m/%d/%Y"),
    period = paste0(
      year(enc_date),
      if_else(month(enc_date) <= 6, " Jan-Jun", " Jul-Dec")
    )
  ) %>%
  filter(
    !is.na(enc_date),
    enc_date >= as.Date("2024-01-01")
  ) %>%
  group_by(period) %>%
  summarize(
    n_names = n_distinct(dem_name),
    .groups = "drop"
  )

print(six_month_counts)

## 6 Month year breakdown## 
later <- later %>%
  mutate(
    enc_date = as.Date(enc_date, format = "%m/%d/%Y"),
    six_month_period = case_when(
      month(enc_date) <= 6 ~ paste0(year(enc_date), "_H1"),
      month(enc_date) >= 7 ~ paste0(year(enc_date), "_H2"),
      TRUE ~ NA_character_
    )
  )

#Table 6 month breakdown and GA ## 
six_month_ga <- table(later$six_month_period, later$later)
six_month_ga

six_month_ga24 <- table(later$six_month_period, later$later24)
six_month_ga24

six_month_ga26 <- table(later$six_month_period, later$later26)
six_month_ga26

#Table 6 month and procedure type ## 
six_month_proc <- table(later$six_month_period, later$preg_pro_primtype)
six_month_proc

##Chi Square Analysis ## 
chisq.test(six_month_ga)
ga_chi <- chisq.test(six_month_ga)
ga_chi$residuals
ga_chi$expected
###############Effect Size 
install.packages("rcompanion")
library(rcompanion)
cramerV(six_month_ga)
##########Weak association 

chisq.test(six_month_proc)
proc_chi <- chisq.test(six_month_proc)
proc_chi$residuals
proc_chi$expected

proc_chi_fixed <- chisq.test(six_month_proc, simulate.p.value = TRUE, B = 2000)
proc_chi_fixed$residuals

cramerV(six_month_proc)
######Weak association 

## Breakdown of later care patient procedure types ## 
chisq.test(procedure_type_ga)

####Create a hist for GA## 

hist(
  later$ega_wks[later$ega_wks != 0],
  breaks = 10,
  main = "Distribution of Gestational Age Weeks (Excluding Zeros)",
  xlab = "Weeks",
  col = "lightblue",
  border = "white"
)

summary(later$ega_wks)

### Proecural Type breakdown of later care patients ## 

later_ga_proc <- table(later$later[later$preg_pro_primtype %in% c(2,4)], 
                       later$six_month_period[later$preg_pro_primtype %in% c(2,4)])
later_ga_proc

####Later care procedural type 22/24/26###


later_ga <- table(later$preg_pro_primtype[later$later %in% c(1)], 
                       later$six_month_period[later$later %in% c(1)])
later_ga

later_ga24 <- table(later$preg_pro_primtype[later$later24 %in% c(1)], 
                  later$six_month_period[later$later24 %in% c(1)])
later_ga24

later_ga26 <- table(later$preg_pro_primtype[later$later26 %in% c(1)], 
                  later$six_month_period[later$later26 %in% c(1)])
later_ga26

##rename variable options for dates ## 

later <- later %>%
  mutate(six_month_period = case_when(
    six_month_period == "2024_H1" ~ "2024 Jan-Jun", 
    six_month_period == "2024_H2" ~ "2024 Jul-Dec", 
    six_month_period == "2025_H1" ~ "2025 Jan-Jun", 
    six_month_period == "2025_H2" ~ "2025 Jul-Dec", 
    six_month_period == "2026_H1" ~ "2026 Jan-Jun"))

##3 week GA intervals over time ## 

breaks3 <- seq(22,
               max(later$ega_wks,na.rm = TRUE)
               +3, by = 3)
labels3 <- paste(
  breaks3[-length(breaks3)],
  breaks3[-1] - 1, 
  sep = "-"
)
plot_later_3w <- later %>%
  mutate(ega_cat = cut(ega_wks, 
                       breaks = breaks3,
                       right = FALSE, 
                       include.lowest = TRUE,
                       labels = labels3))

ggplot(plot_later_3w %>%
         filter(!is.na(ega_cat)), aes(x=six_month_period, 
                          fill = ega_cat))+
  geom_bar() + geom_text(
    stat = "count",
    aes(label = after_stat(count)), 
    position = position_stack(vjust = 0.5), 
    size = 3, color = "black") +
    labs(
    x= "Six Month Period", 
    y = "Number of Patients", 
    fill = "Estimated GA (wks)", 
    title = "Distribution of Later Care GA by Six Month Period"
  )+ 
  theme_minimal()+ theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


##Drop all NA or empty rows## 
later <- later[!is.na(later$ega_wks) & later$ega_wks != "", ]


#####Export tables to .csv####
# Faster alternative for large tables (requires 'readr' package)
write_csv(later, "later.csv")

