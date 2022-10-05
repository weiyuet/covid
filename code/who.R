# Setup
library(tidyverse)
library(janitor)
library(scales)
library(ggsci)
library(patchwork)

# Load data
vaccination_data <- read_csv("data/who/vaccination-data.csv")

vaccination_data <- vaccination_data %>% clean_names()

# Which countries have the highest vaccination rates?
p1 <- vaccination_data %>%
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
  scale_fill_npg() +
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
  scale_fill_npg() +
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

ggsave("figures/country-vaccination-rates.png", width = 8, height = 8)