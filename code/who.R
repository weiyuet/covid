# Setup
library(tidyverse)
library(skimr)
library(scales)

# Load data
vaccination_data <- read_csv("data/who/vaccination-data.csv")

# Which countries have the highest vaccination rates?
vaccination_data %>%
  select(COUNTRY, PERSONS_FULLY_VACCINATED_PER100) %>% 
  mutate(COUNTRY = fct_reorder(COUNTRY, PERSONS_FULLY_VACCINATED_PER100)) %>%
  slice_max(n = 40, order_by = PERSONS_FULLY_VACCINATED_PER100) %>% 
  ggplot(aes(x = PERSONS_FULLY_VACCINATED_PER100, y = COUNTRY)) +
  geom_col(colour = "gray10", fill = "gray35") +
  geom_label(aes(x = PERSONS_FULLY_VACCINATED_PER100, y = COUNTRY, label = COUNTRY),
             hjust = 1,
             vjust = 0.5,
             colour = "white",
             fill = NA,
             label.size = NA,
             size = 3) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(0, 120, 20)) +
  theme_classic() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(x = "", y = "",
       title = "Top 40 Countries With highest COVID-19 Vaccination Rates",
       subtitle = "Persons Fully Vaccinated per 100 Population",
       caption = "Data: WHO")

ggsave("figures/country-vaccination-rates.png", width = 6, height = 8)