##################################
# COVID Data from Singapore Govt #
##################################

#### Setup ####
library(tidyverse)
library(scales)
library(glue)
library(paletteer)

##################
# Epidemic Curve #
##################

#### Load data ####
epidemic_curve <- read_csv("data/singapore/covid-19-case-numbers/epidemic-curve.csv")

#### Visualize ####
# How many major waves of infection were there?
epidemic_curve %>% 
  ggplot(aes(x = date,
             y = value,
             colour = type)) +
  geom_step() +
  scale_x_date(labels = label_date_short(),
               date_breaks = "3 months") +
  scale_y_continuous(labels = label_number(big.mark = ","),
                     breaks = seq(0, 30000, 5000)) +
  scale_colour_paletteer_d("ggsci::springfield_simpsons",
                           guide = guide_legend(reverse = TRUE)) +
  annotate(geom = "text",
           x = as.Date("2020-05-01"),
           y = 4000,
           label = "Initial wave",
           size = 4) +
  annotate(geom = "text",
           x = as.Date("2021-05-01"),
           y = 2000,
           label = "Delta wave?",
           size = 4) +
  annotate(geom = "text",
           x = as.Date("2022-04-15"),
           y = 27000,
           label = "Omicron waves") +
  labs(x = "",
       y = "",
       colour = "",
       title = glue("COVID-19 Epidemic Curve (updated {max(epidemic_curve$date)})"),
       subtitle = "Six major waves of infection in Singapore",
       caption = "Data: Ministry of Health (data.gov.sg) | Graphic: @weiyuet") +
  theme_classic() +
  theme(legend.position = "bottom")

#### Save image ####
ggsave("figures/covid-epidemic-curve-sg.png", width = 7, height = 5)

################################
# Week-on-Week Infection Ratio #
################################

#### Load data ####
week_on_week_infection_ratio <- read_csv("data/singapore/covid-19-case-numbers/week-on-week-infection-ratio.csv")

#### Visualize ####
# Are infections increasing or decreasing?
week_on_week_infection_ratio %>% 
  ggplot(aes(x = pr_date,
             y = ratio_comm_cases_pw_over_wb)) +
  geom_step() +
  geom_hline(yintercept = 1,
             colour = "red",
             linetype = "dashed") +
  scale_x_date(labels = label_date_short(),
               date_breaks = "1 week") +
  labs(x = "",
       y = "",
       title = glue("Week-on-week Infection Ratio in Singapore (updated {max(week_on_week_infection_ratio$pr_date)})"),
       subtitle = "Ratio above 1.0 means infections are increasing",
       caption = "Data: Ministry of Health (data.gov.sg) | Graphic: @weiyuet") +
  theme_classic()

#### Save image ####
ggsave("figures/week-on-week-infection-ratio.png", width = 7, height = 5)

###############################
# Hospital admissions and ICU #
###############################

#### Load data ####
hospital_admissions <- read_csv("data/singapore/covid-19-hospital-admissions/new-covid-19-hospital-admissions.csv")
icu_admissions <- read_csv("data/singapore/covid-19-hospital-admissions/new-covid-19-icu-admissions.csv")

#### Wrangle ####
# Join hospital admissions and icu data sets by date
hospital_and_icu <- hospital_admissions %>% 
  left_join(icu_admissions, by = "date")

# Data to tidy format
hospital_and_icu_tidy <- hospital_and_icu %>% 
  pivot_longer(cols = 2:3,
               names_to = "type_of_admission",
               values_to = "value")

#### Visualize ####
# How many hospital admissions are related to COVID-19?
hospital_and_icu_tidy %>% 
  ggplot(aes(x = date,
             y = value,
             colour = type_of_admission)) +
  geom_step() +
  scale_x_date(labels = label_date_short(),
               date_breaks = "1 week") +
  scale_colour_paletteer_d("ggsci::springfield_simpsons",
                           labels = c("New hospital admissions", "New ICU admissions")) +
  labs(x = "",
       y = "",
       colour = "",
       title = glue("Number of Hospital Admissions for COVID-19 (Updated {max(hospital_and_icu_tidy$date)})"),
       caption = "Data: Ministry of Health (data.gov.sg) | Graphic: @weiyuet") +
  theme_classic() +
  theme(legend.position = "bottom")

#### Save image ####
ggsave("figures/hospital-admissions.png", width = 7, height = 5)