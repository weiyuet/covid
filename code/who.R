# Setup
library(tidyverse)
library(janitor)
library(scales)
library(ggsci)

# Load data
vaccination_data <- read_csv("data/who/vaccination-data.csv")

vaccination_data <- vaccination_data %>% clean_names()

# Which countries have the highest vaccination rates?
vaccination_data %>%
  select(country, who_region, persons_fully_vaccinated_per100) %>% 
  mutate(country = fct_reorder(country, persons_fully_vaccinated_per100)) %>%
  slice_max(n = 40, order_by = persons_fully_vaccinated_per100) %>% 
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
  scale_fill_nejm() +
  theme_classic() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(x = "", y = "",
       fill = "WHO Region",
       title = "Countries with Highest COVID-19 Vaccination Rates",
       subtitle = "Persons Fully Vaccinated per 100 Population",
       caption = "Data: WHO | Graphic: @weiyuet")

ggsave("figures/highest-vaccination-rates.png", width = 5.5, height = 7)

# Which countries have the lowest vaccination rates?
vaccination_data %>%
  select(country, who_region, persons_fully_vaccinated_per100) %>% 
  mutate(country = fct_reorder(country, persons_fully_vaccinated_per100)) %>%
  slice_min(n = 40, order_by = persons_fully_vaccinated_per100) %>% 
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
  scale_fill_nejm() +
  theme_classic() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(x = "", y = "",
       fill = "WHO Region",
       title = "Countries with Lowest COVID-19 Vaccination Rates",
       subtitle = "Persons Fully Vaccinated per 100 Population",
       caption = "Data: WHO | Graphic: @weiyuet")

ggsave("figures/lowest-vaccination-rates.png", width = 5.5, height = 7)