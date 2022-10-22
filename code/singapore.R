# Setup
library(tidyverse)
library(scales)
library(glue)
library(ggsci)

# Load data
epidemic_curve <- read_csv("data/singapore/covid-19-case-numbers/epidemic-curve.csv")

# Visualize
epidemic_curve %>% 
  ggplot(aes(x = date, y = value, colour = type)) +
  geom_step() +
  scale_x_date(expand = c(0.01, 0),
               labels = label_date_short(),
               date_breaks = "3 months") +
  scale_y_continuous(expand = c(0.01, 0),
                     labels = label_number(big.mark = ","),
                     limits = c(0, 30000),
                     breaks = seq(0, 30000, 5000)) +
  scale_colour_jco() +
  theme_classic() +
  theme(legend.position = c(0.25, 0.5)) +
  labs(x = "", y = "",
       colour = "",
       title = glue("COVID-19 Epidemic Curve (updated {max(epidemic_curve$date)})"),
       subtitle = "Singapore is currently in a fourth major wave",
       caption = "Data: data.gov.sg | Graphic: @weiyuet")

ggsave("figures/covid-epidemic-curve-sg.png", width = 8, height = 5)