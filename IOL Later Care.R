####IOL UIC Analysis ####
##K.Mercer 08/14/26 ## 

# Designate working directory # 
setwd("//HomeDrive/HDriveProd/kfmercer/Desktop/UIC/Abortion Tracking/REDCap")
getwd()
iol <- read.csv("//HomeDrive/HDriveProd/kfmercer/Desktop/UIC/Abortion Tracking/REDCap/IOL.csv")

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

##August - August year breakdowns ## 

class(iol$enc_date)

annual_iol <- iol %>%
  mutate(
    enc_date = as.Date(enc_date, format = "%m/%d/%Y"),
    fy_start = if_else(month(enc_date) >= 8,
                       year(enc_date),
                       year(enc_date) - 1),
    period = paste0(fy_start, "-", fy_start + 1)) %>%
  filter(
    !is.na(enc_date),
    enc_date >= as.Date("2024-08-01")
  )%>%
  group_by(period) %>%
  summarize(
    n_names = n_distinct(dem_name),
    .groups = "drop"
  )
print(annual_iol)

## Distribution of IOL ## 
## 3-week GA intervals
# Custom GA categories
breaks_ga <- c(-Inf,22, 25, 28, 31, 34, 36)

labels_ga <- c(
  "<22",
  "22-24",
  "25-27",
  "28-30",
  "31-33",
  "34-36"
)
plot_iol <- iol %>%
  mutate(
    enc_date = as.Date(enc_date, format = "%m/%d/%Y"),
    
    fy_start = if_else(
      month(enc_date) >= 8,
      year(enc_date),
      year(enc_date) - 1
    ),
    
    period = paste0(fy_start, "-", fy_start + 1),
    
    ega_cat = cut(
      ega_wks,
      breaks = breaks_ga,
      right = FALSE,
      include.lowest = TRUE,
      labels = labels_ga
    ),
    
    ega_cat = factor(
      ega_cat,
      levels = rev(labels_ga)
    )
  ) %>%
  filter(
    !is.na(enc_date),
    enc_date >= as.Date("2024-08-01")
  )


ga_colors <- c(
  "<22" = "#343333",
  "22-24"= "#A6C5D7",
  "25-27" = "#41B6E6",
  "28-30" = "#00966C",
  "31-33" = "#264a41",
  "34-36" = "#001E62"
)

##Stacked bar chart## 
ggplot(
  plot_iol %>% filter(!is.na(ega_cat)),
  aes(
    x = factor(period, levels = sort(unique(period))),
    fill = ega_cat
  )
) +
  geom_bar() +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    position = position_stack(vjust = 0.5),
    size = 3,
    color = "black"
  ) +
  scale_fill_manual(
    values = ga_colors,
    guide = guide_legend(reverse = TRUE)
  ) +
  labs(
    x = "Fiscal Years (08-01 to 07-31)",
    y = "Number of Patients",
    fill = "Estimated GA (weeks)",
    title = "Distribution of Later Care GA by Fiscal Year"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

##Grouped Bar Chart ## 

ggplot(
  plot_iol %>% filter(!is.na(ega_cat)),
  aes(
    x = factor(period, levels = sort(unique(period))),
    fill = ega_cat
  )
) +
  geom_bar(position = position_dodge(width = 0.9)) +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    position = position_dodge(width = 0.9),
    vjust = -0.25,
    size = 3,
    color = "black"
  ) +
  scale_fill_manual(
    values = ga_colors,
    guide = guide_legend(reverse = TRUE)
  ) +
  labs(
    x = "Fiscal Years (08/01 to 07/31)",
    y = "Number of Patients",
    fill = "Estimated GA (weeks)",
    title = "Distribution of IOL cases by GA & Fiscal Year"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

                      