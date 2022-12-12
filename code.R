library(readr)
library(tidyverse)

troponin <- read_csv("troponin.csv", col_types = cols(
  Age = col_factor(levels = c("40-49", "50-59", "60-69", "70-79", "80-89", "90-")),
  Sex = col_factor(levels = c("1","2")), 
  SBP = col_number(),
  DBP = col_number(), 
  B_hsTNT = col_number(), 
  F_hsTnT = col_number(),
  ACmortality = col_factor(levels = c("0", "1")), 
  MACE = col_factor(levels = c("0","1")), 
  Coronary = col_factor(levels = c("0","1")),
  Stroke = col_factor(levels = c("0","1"))))

# fix factor levels
troponin$Sex <- recode_factor(troponin$Sex, 
                              "1" = "Male",
                              "2" = "Female")
troponin$Age <- recode_factor(troponin$Age,
                              "40-49" = "40-49", 
                              "50-59" = "50-59", 
                              "60-69" = "60-69", 
                              "70-79" = "70-79", 
                              "80-89" = "80-89", 
                              "90-" = "90+")

# simple feature engineering
troponin <- troponin %>%
  mutate(MAP = round((1/3)*SBP + (2/3)*DBP, 2), # mean arterial BP
         D_hsTNT = F_hsTnT - B_hsTNT) # difference in trop levels

#### create plots
library(gridExtra)
library(wesanderson)
library(showtext)
showtext_auto()

# save some pretty colors
pal <- wes_palette("GrandBudapest2", 14, type = "continuous")

# define custom theme
theme_np_dark <- function() {
  theme(
    line = element_line(color = "#939393"),
    rect = element_rect(fill = "#212121",
                        linetype = 0, colour = NA),
    text = element_text(color = "#dbdbdb", family = "Lato"),
    axis.title = element_text(color = "#dbdbdb", size = rel(1)),
    axis.title.x = element_text(vjust = -2),
    axis.title.y = element_text(vjust = 4),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    axis.text.x = element_text(color = "#dbdbdb", size = rel(1.1)),
    axis.text.y = element_text(color = "#dbdbdb", size = rel(1.1)),
    legend.background = element_rect(fill = '#212121'),
    legend.key = element_rect(color = NULL, fill = "#212121"),
    legend.text = element_text(color = "#dbdbdb"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "vertical",
    panel.background = element_rect(color = '#212121', fill = '#212121'),
    panel.grid = element_line(color = NULL),
    panel.grid.major.y = element_line(color = "#565656"),
    panel.grid.minor.y = element_line(color = "#565656"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.title = element_text(hjust = 0.5, size = rel(1.6)),
    plot.subtitle = element_text(hjust = 0.5, size = rel(0.8)),
    plot.caption = element_text(hjust = 1, size = rel(0.85)),
    plot.margin = unit(c(1, 1, 1, 1), "lines")
    )
}

# build plots
plot_age <- troponin %>%
  ggplot(mapping = aes(Age)) + 
  geom_bar(aes(y = ..prop.., group = 1),
           fill = pal[9:14]) +
  scale_y_continuous(labels = scales::percent) +
  theme_np_dark() +
  labs(title = "Age",
       subtitle = "By ten-year categories",
       y = "", x = "")

plot_bp <- troponin %>%
  ggplot(mapping = aes(y = SBP, x = DBP)) + 
  geom_bin2d(show.legend = F) + 
  scale_fill_gradient(high = pal[3], low = "#1e2c47") +
  theme_np_dark() + 
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) + 
  labs(title = "Blood Pressure", 
       subtitle = "Brighter cells indicate more common values",
       y = "Systolic", x = "Diastolic")

plot_age_MAP <- troponin %>%
  ggplot(mapping = aes(y = MAP, x = Age)) +
  geom_boxplot(fill = pal[9:14], color = "#dbdbdb") + 
  theme_np_dark() +
  labs(title = "Mean Arterial Blood Pressure",
       subtitle = "Sorted by age group",
       caption = caption = "All plots by Nick Peckover
       Data courtesy Xiao, Wenkai, et. al. (2017) doi.org/10.5061/dryad.bq0rm",
       y = "MAP", x = "")

plot_ACM <- troponin %>%
  ggplot(mapping = aes(x = log(B_hsTNT), y = log(F_hsTnT),
                       color = Sex),
         position = "jitter") + 
  geom_point() + 
  scale_color_manual(values = c("#475d87", pal[3])) +
  #scale_color_manual(values = pal[c(14,9)]) +
  theme_np_dark() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
       legend.position = c(0.5, 0.98)) +
  labs(title = "High Sensitivity Troponin",
       subtitle = "Baseline vs. Follow-Up levels (log scale)",
       y = "Follow-Up hs-cTnT", x = "Baseline hs-cTnT")

# arrange all plots together
grid.arrange(plot_age, plot_ACM, plot_bp, plot_age_MAP, ncol = 2)
