#######################
# COVID Data from WHO #
#######################

#### Setup ####
library(tidyverse)
library(janitor)
library(scales)
library(paletteer)
library(patchwork)
library(glue)

###############
# Vaccination #
###############

#### Load data vaccination ####
vaccination_data <- read_csv("data/who/vaccination-data.csv")

#### Wrangle ####
# Column names to lower case
vaccination_data <- vaccination_data %>% 
  clean_names()

#### Visualize ####
# Which countries have the highest vaccination rates?
p1 <- vaccination_data %>%
  select(country, who_region, persons_fully_vaccinated_per100) %>% 
  mutate(country = fct_reorder(country, persons_fully_vaccinated_per100)) %>%
  slice_max(order_by = persons_fully_vaccinated_per100, n = 30) %>% 
  ggplot(aes(x = persons_fully_vaccinated_per100,
             y = country,
             fill = who_region)) +
  geom_col(colour = "gray10") +
  geom_label(aes(x = persons_fully_vaccinated_per100,
                 y = country,
                 label = country),
             hjust = 1,
             vjust = 0.5,
             colour = "black",
             fill = NA,
             label.size = NA,
             size = 3) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(0, 120, 20)) +
  scale_fill_paletteer_d("feathers::rose_crowned_fruit_dove",
                         direction = -1) +
  theme_classic() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "bottom") +
  labs(x = "",
       y = "",
       fill = "",
       title = "Highest Vaccination Rates")

# Which countries have the lowest vaccination rates?
p2 <- vaccination_data %>%
  select(country, who_region, persons_fully_vaccinated_per100) %>% 
  mutate(country = fct_reorder(country, persons_fully_vaccinated_per100)) %>%
  slice_min(order_by = persons_fully_vaccinated_per100, n = 30) %>% 
  ggplot(aes(x = persons_fully_vaccinated_per100,
             y = country,
             fill = who_region)) +
  geom_col(colour = "gray10") +
  geom_label(aes(x = persons_fully_vaccinated_per100,
                 y = country,
                 label = country),
             hjust = 1,
             vjust = 0.5,
             colour = "black",
             fill = NA,
             label.size = NA,
             size = 3) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(0, 25, 5)) +
  scale_fill_paletteer_d("feathers::rose_crowned_fruit_dove", 
                         direction = -1) +
  theme_classic() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 2)) +
  labs(x = "", y = "",
       fill = "",
       title = "Lowest Vaccination Rates")

# Combine plots
p <- p1 | p2

p + plot_annotation(title = "Persons Fully Vaccinated per 100 Population",
                    caption = "Data: WHO | Graphic: @weiyuet")

#### Save image ####
ggsave("figures/country-vaccination-rates.png", width = 7, height = 7)

###############
# Case Counts #
###############

#### Load data case counts ####
case_counts <- read_csv("data/who/WHO-COVID-19-global-table-data.csv")

#### Wrangle ####
# Column names to lower case
case_counts <- case_counts %>% 
  clean_names()

# Convert deaths_newly_reported_in_last_24_hours to numeric
case_counts <- case_counts %>% 
  mutate(deaths_newly_reported_in_last_24_hours = as.numeric(deaths_newly_reported_in_last_24_hours))

# Data to tidy format
case_counts <- case_counts %>% 
  pivot_longer(cols = 3:12,
               names_to = "case_type",
               values_to = "count")

#### Visualize ####
# Which countries have highest reported cases in the last 7 days?
case_types <- c("cases_newly_reported_in_last_7_days_per_100000_population")

case_counts %>% 
  filter(case_type %in% case_types) %>% 
  mutate(name = case_when(name == "Micronesia (Federated States of)" ~ "Micronesia",
                          TRUE ~ name)) %>% 
  mutate(name = fct_reorder(name, count)) %>% 
  slice_max(order_by = count, n = 20) %>% 
  ggplot(aes(x = count,
             y = name,
             fill = who_region)) +
  geom_col(colour = "gray10") +
  geom_label(aes(x = count,
                 y = name,
                 label = round(count, 0)),
             hjust = 1, 
             vjust = 0.5,
             fill = NA,
             label.size = NA,
             size = 3) +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_fill_paletteer_d("ggsci::legacy_tron") +
  theme_classic() +
  theme(legend.position = c(0.8, 0.3)) +
  labs(x = "",
       y = "",
       fill = "WHO Region",
       title = "Newly Reported Cases in the Last 7 Days",
       subtitle = "per 100,000 population",
       caption = "Data: WHO | Graphic: @weiyuet")

#### Save image ####
ggsave("figures/new-cases-last-seven-days.png", width = 7, height = 7)

####################
# Case Time Series #
####################

#### Load data case time series ####
cases <- read_csv("data/who/WHO-COVID-19-global-data.csv")

#### Wrangle ####
# Column names to lower case
cases <- cases %>% 
  clean_names()

# Data to tidy format
cases <- cases %>% 
  pivot_longer(cols = 5:8,
               names_to = "case_type",
               values_to = "count")

#### Visualize ####
# Which regions are reporting the most number of new cases?
case_types <- c("new_cases")

cases %>%
  select(date_reported, who_region, case_type, count) %>%
  filter(case_type %in% case_types) %>% 
  filter(case_type > 0) %>% 
  ggplot(aes(x = date_reported,
             y = count)) +
  geom_line(show.legend = FALSE) +
  facet_wrap(vars(who_region),
             scales = "free_y") +
  scale_x_date(labels = label_date_short(),
               date_breaks = "6 months") +
  scale_y_continuous(labels = label_number(big.mark = ","),
                     expand = c(0.01, 0)) +
  theme_classic() +
  labs(x = "",
       y = "",
       colour = "WHO Region",
       title = glue("New COVID-19 Cases Reported (updated {max(cases$date_reported)})"),
       caption = "Data: WHO | Graphic: @weiyuet")

#### Save image ####
ggsave("figures/new-cases.png", width = 7, height = 6)

#### Visualize ####
# What is the cumulative toll of COVID-19 in Singapore? (Linear scale)
country_selected <- c("Singapore")
case_types <- c("cumulative_cases", "cumulative_deaths")

# Check latest numbers
cases %>% 
  filter(country %in% country_selected) %>%
  filter(case_type %in% case_types) %>% tail()

cases %>% 
  filter(country %in% country_selected) %>%
  filter(case_type %in% case_types) %>% 
  ggplot(aes(x = date_reported,
             y = count,
             colour = case_type)) +
  geom_step() +
  scale_x_date(labels = label_date_short(),
               date_breaks = "3 months") +
  scale_y_continuous(labels = label_number(big.mark = ","),
                     limits = c(0, 2500000)) +
  scale_colour_paletteer_d("ggsci::default_jco",
                           labels = c("Total Cases", "Total Deaths")) +
  annotate(geom = "text",
           x = as.Date(glue("{max(cases$date_reported) - 20}")),
           y = 2245348 + 90000,
           label = "2,245,348",
           size = 3.5) +
  annotate(geom = "text",
           x = as.Date(glue("{max(cases$date_reported) - 20}")),
           y = 1722 + 90000,
           label ="1,722",
           size = 3.5) +
  labs(x = "",
       y = "",
       colour = "",
       title = glue("The Cumulative Toll of COVID-19 in Singapore (Updated {max(cases$date_reported)})"),
       caption = "Data: WHO | Graphic: @weiyuet") +
  theme_classic() +
  theme(legend.position = "bottom")

#### Save Image ####
ggsave("figures/covid-cumulative-sg.png", width = 7, height = 5)