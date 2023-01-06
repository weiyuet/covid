#################################
# COVID-19 Open Data Repository #
#################################

#### Setup ####
library(tidyverse)
library(scales)
library(glue)
library(paletteer)

#### Load data ####
open_data <- read_csv("https://storage.googleapis.com/covid19-open-data/v3/epidemiology.csv")

#### Wrangle ####
# Data to tidy format
tidy_data <- open_data %>%
  pivot_longer(cols = new_confirmed:cumulative_tested,
               names_to = "infection_type", values_to = "count")

# Covid in Singapore
sg_data <- tidy_data %>% 
  filter(location_key == "SG") %>% 
  filter(infection_type %in% c("cumulative_confirmed", "cumulative_deceased"))

#### Visualize ####
sg_data %>% 
  ggplot(aes(x = date,
             y = count,
             colour = infection_type)) +
  geom_line() +
  scale_x_date(
    expand = c(0.01, 0),
    labels = label_date_short(),
    date_breaks = "3 months") +
  scale_y_log10(
    limits = c(1, 3000000),
    breaks = c(10, 1000, 100000, 1000000),
    labels = label_number(big.mark = ",")) +
  scale_colour_paletteer_d("ggsci::default_jco") +
  annotate(
    geom = "text",
    x = as.Date(glue("{max(sg_data$date) - 40}")),
    y = max(sg_data$count) + 1000000,
    label = glue("{max(sg_data$count)}"),
    size = 3.5
  ) +
  theme_classic() +
  theme(legend.position = c(0.3, 0.9)) +
  guides(guide_legend(nrow = 1)) +
  labs(x = "",
       y = "",
       colour = "",
       title = glue("COVID-19 in Singapore - Cumulative Cases (updated {max(sg_data$date)})"),
       subtitle = "The first wave had the most number of exponential cases (y-axis log scale)",
       caption = "Data: Google COVID-19 Open Data | Graphic: @weiyuet")

#### Save image ####
ggsave("figures/covid-cumulative-sg.png", width = 8, height = 5)