# Setup
library(tidyverse)
library(scales)
library(glue)

# Load data
open_data <- read_csv("https://storage.googleapis.com/covid19-open-data/v3/epidemiology.csv")

# Data to tidy format
tidy_data <- raw_data %>%
  pivot_longer(cols = new_confirmed:cumulative_tested, names_to = "infection_type", values_to = "values")

# covid in SG
sg_data <- tidy_data %>% 
  filter(location_key == "SG") %>% 
  filter(infection_type == "cumulative_confirmed")

sg_data %>% 
  ggplot(aes(x = date, y = values)) +
  geom_line() +
  scale_x_date(
    labels = label_date_short(),
    date_breaks = "3 months") +
  scale_y_log10(
    limits = c(1, 3000000),
    labels = label_number(big.mark = ",")) +
  annotate(
    geom = "text",
    x = as.Date(glue("{max(sg_data$date)}")),
    y = max(sg_data$values) + 1000000,
    label = glue("{max(sg_data$values)}"),
    size = 3.5
  ) +
  theme_classic() +
  labs(x = "", y = "# of cumulative confirmed cases (log scale)",
       title = glue("Covid-19 in Singapore - Cumulative Cases as of {max(sg_data$date)}"),
       subtitle = "The first wave had the most number of exponential cases",
       caption = "Data: Google Covid-19 Open Data")

ggsave("figures/covid-cumulative-sg.png", width = 8, height = 4.5)