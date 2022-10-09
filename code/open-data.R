# Setup
library(tidyverse)
library(scales)
library(glue)
library(ggsci)

# Load data
open_data <- read_csv("https://storage.googleapis.com/covid19-open-data/v3/epidemiology.csv")

# Data to tidy format
tidy_data <- open_data %>%
  pivot_longer(cols = new_confirmed:cumulative_tested, names_to = "infection_type", values_to = "values")

# Covid in SG
sg_data <- tidy_data %>% 
  filter(location_key == "SG") %>% 
  filter(infection_type %in% c("cumulative_confirmed", "cumulative_deceased"))

sg_data %>% 
  ggplot(aes(x = date, y = values, colour = infection_type)) +
  geom_step() +
  scale_x_date(
    expand = c(0.01, 0),
    labels = label_date_short(),
    date_breaks = "3 months") +
  scale_y_log10(
    limits = c(1, 3000000),
    breaks = c(10, 1000, 100000, 1000000),
    labels = label_number(big.mark = ",")) +
  scale_colour_jco(labels = c("Cumulative confirmed", "Cumulative deceased")) +
  annotate(
    geom = "text",
    x = as.Date(glue("{max(sg_data$date) - 35}")),
    y = max(sg_data$values) + 950000,
    label = glue("{max(sg_data$values)}"),
    size = 3.5
  ) +
  theme_classic() +
  theme(legend.position = c(0.3, 0.9)) +
  guides(guide_legend(nrow = 1)) +
  labs(x = "", y = "(log scale)",
       colour = "",
       title = glue("COVID-19 in Singapore - Cumulative Cases as of {max(sg_data$date)}"),
       subtitle = "The first wave had the most number of exponential cases",
       caption = "Data: Google COVID-19 Open Data | Graphic: @weiyuet")

ggsave("figures/covid-cumulative-sg.png", width = 8, height = 5)