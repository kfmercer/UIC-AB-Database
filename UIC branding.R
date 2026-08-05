##Creating UIC color theme## 
##K.Mercer 08/05/26##

library(ggplot2)

# UIC Brand Colors
uic_colors <- list(
  red         = "#D50032",  # Fire Engine Red
  navy        = "#001E62",  # Navy Pier Blue
  brick       = "#B10000",  # Chicago Brick
  slate       = "#A6C5D7",  # Second City Slate
  green       = "#00966C",  # Chicago River Green
  blue        = "#41B6E6",  # Chicago Blue
  maroon      = "#480002",  # Dark Maroon
  white       = "#FFFFFF",
  expo_white  = "#F6F3EC",
  beach       = "#FEF8E0",
  steel_gray  = "#343333",
  black       = "#000000"
)

theme_uic <- function(base_size = 12,
                      base_family = "sans") {
  
  theme_minimal(base_size = base_size,
                base_family = base_family) %+replace%
    
    theme(
      plot.background = element_rect(
        fill = uic_colors$white,
        color = NA
      ),
      
      panel.background = element_rect(
        fill = uic_colors$white,
        color = NA
      ),
      
      plot.title = element_text(
        size = rel(1.5),
        face = "bold",
        color = uic_colors$navy,
        margin = margin(b = 10)
      ),
      
      plot.subtitle = element_text(
        color = uic_colors$steel_gray,
        margin = margin(b = 12)
      ),
      
      plot.caption = element_text(
        color = uic_colors$steel_gray,
        size = rel(0.85)
      ),
      
      axis.title = element_text(
        color = uic_colors$navy,
        face = "bold"
      ),
      
      axis.text = element_text(
        color = uic_colors$steel_gray
      ),
      
      axis.line.x = element_line(
        color = uic_colors$steel_gray,
        linewidth = 0.4
      ),
      
      axis.line.y = element_line(
        color = uic_colors$steel_gray,
        linewidth = 0.4
      ),
      
      panel.grid.major = element_line(
        color = uic_colors$expo_white,
        linewidth = 0.5
      ),
      
      panel.grid.minor = element_blank(),
      
      legend.position = "bottom",
      
      legend.title = element_text(
        face = "bold",
        color = uic_colors$navy
      ),
      
      legend.text = element_text(
        color = uic_colors$steel_gray
      ),
      
      strip.background = element_rect(
        fill = uic_colors$navy,
        color = NA
      ),
      
      strip.text = element_text(
        color = "white",
        face = "bold"
      )
    )
}

# Discrete color scale
scale_color_uic <- function(...) {
  scale_color_manual(
    values = c(
      uic_colors$red,
      uic_colors$navy,
      uic_colors$blue,
      uic_colors$green,
      uic_colors$slate,
      uic_colors$brick
    ),
    ...
  )
}

scale_fill_uic <- function(...) {
  scale_fill_manual(
    values = c(
      uic_colors$red,
      uic_colors$navy,
      uic_colors$blue,
      uic_colors$green,
      uic_colors$slate,
      uic_colors$brick
    ),
    ...
  )
}
theme_uic()

#####Test in R #### 
ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) +
  geom_point(size = 3) +
  scale_color_uic() +
  labs(
    title = "Fuel Efficiency by Vehicle Weight",
    subtitle = "Example UIC-branded visualization",
    color = "Cylinders"
  ) +
  theme_uic()



uic_palette <- c(
  uic_colors$slate,
  uic_colors$blue,
  uic_colors$green,
  uic_colors$brick,
  uic_colors$red,
  uic_colors$navy,
  uic_colors$maroon
)

ggplot(
  plot_later_3w %>% filter(!is.na(ega_cat)),
  aes(
    x = six_month_period,
    fill = ega_cat
  )
) +
  geom_bar(position = position_stack(reverse = TRUE)) +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    position = position_stack(vjust = 0.5, reverse = TRUE),
    size = 3,
    color = "black"
  ) +
  scale_fill_manual(
    values = colorRampPalette(uic_palette)(
      nlevels(plot_later_3w$ega_cat)
    )
  ) +
  labs(
    x = "Six Month Period",
    y = "Number of Patients",
    fill = "Estimated GA (wks)",
    title = "Distribution of Later Care GA by Six Month Period"
  ) +
  theme_uic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )