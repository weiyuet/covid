# Setup
library(tidyverse)
library(janitor)
library(scales)
library(ggsci)
library(patchwork)

# Load data vaccination
vaccination_data <- read_csv("data/who/vaccination-data.csv")

# Wrangle
# Column names to lower case
vaccination_data <- vaccination_data %>% 
  clean_names()

# Which countries have the highest vaccination rates?
p1 <- vaccination_data %>%
  select(country, who_region, persons_fully_vaccinated_per100) %>% 
  mutate(country = fct_reorder(country, persons_fully_vaccinated_per100)) %>%
  slice_max(order_by = persons_fully_vaccinated_per100, n = 35) %>% 
  ggplot(aes(x = persons_fully_vaccinated_per100, y = country, fill = who_region)) +
  geom_col(colour = "gray10") +
  geom_label(aes(x = persons_fully_vaccinated_per100, y = country, label = country),
             hjust = 1,
             vjust = 0.5,
             colour = "black",
             fill = NA,
             label.size = NA,
             size = 3) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(0, 120, 20)) +
  scale_fill_tron() +
  theme_classic() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "bottom") +
  labs(x = "", y = "",
       fill = "",
       title = "Highest Vaccination Rates")

# Which countries have the lowest vaccination rates?
p2 <- vaccination_data %>%
  select(country, who_region, persons_fully_vaccinated_per100) %>% 
  mutate(country = fct_reorder(country, persons_fully_vaccinated_per100)) %>%
  slice_min(order_by = persons_fully_vaccinated_per100, n = 35) %>% 
  ggplot(aes(x = persons_fully_vaccinated_per100, y = country, fill = who_region)) +
  geom_col(colour = "gray10") +
  geom_label(aes(x = persons_fully_vaccinated_per100, y = country, label = country),
             hjust = 1,
             vjust = 0.5,
             colour = "black",
             fill = NA,
             label.size = NA,
             size = 3) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(0, 25, 5)) +
  scale_fill_tron() +
  theme_classic() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 2)) +
  labs(x = "", y = "",
       fill = "",
       title = "Lowest Vaccination Rates")

p <- p1 | p2

p + plot_annotation(title = "Persons Fully Vaccinated per 100 Population",
                    caption = "Data: WHO | Graphic: @weiyuet")

# Save image
ggsave("figures/country-vaccination-rates.png", width = 8, height = 8)

case_counts <- read_csv("data/who/WHO-COVID-19-global-table-data.csv")

# Wrangle
# Column names to lower case
case_counts <- case_counts %>% 
  clean_names()

# Data to tidy format
case_counts <- case_counts %>% 
  pivot_longer(cols = cases_cumulative_total:deaths_newly_reported_in_last_24_hours, names_to = "case_type", values_to = "count")

# Which countries have highest newly reported cases in the last 7 days?
case_counts %>% 
  filter(case_type == "cases_newly_reported_in_last_7_days_per_100000_population") %>% 
  mutate(name = case_when(name == "Micronesia (Federated States of)" ~ "Micronesia",
                          TRUE ~ name)) %>% 
  mutate(name = fct_reorder(name, count)) %>% 
  slice_max(order_by = count, n = 20) %>% 
  ggplot(aes(x = count, y = name, fill = who_region)) +
  geom_col(colour = "gray10") +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_fill_tron() +
  theme_classic() +
  theme(legend.position = c(0.8, 0.3)) +
  labs(x = "", y = "",
       fill = "",
       title = "Highest Reported Cases in the Last 7 Days",
       subtitle = "per 100,000 population",
       caption = "Data: WHO | Graphic: @weiyuet")

# Save image
ggsave("figures/new-cases-last-seven-days.png", width = 8, height = 8)