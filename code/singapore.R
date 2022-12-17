###########################
# COVID Data from SG Govt #
###########################

# Setup
library(tidyverse)
library(scales)
library(glue)
library(paletteer)

##################
# Epidemic Curve #
##################

#### Load data ####
epidemic_curve <- read_csv("data/singapore/covid-19-case-numbers/epidemic-curve.csv")

#### Epidemic Curve Plot ####
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
  scale_colour_paletteer_d("ggsci::springfield_simpsons",
                           guide = guide_legend(reverse = TRUE)) +
  theme_classic() +
  theme(legend.position = c(0.9, 0.9)) +
  labs(x = "", y = "",
       colour = "",
       title = glue("COVID-19 Epidemic Curve (updated {max(epidemic_curve$date)})"),
       caption = "Data: data.gov.sg | Graphic: @weiyuet")

#### Save image ####
ggsave("figures/covid-epidemic-curve-sg.png", width = 8, height = 5)

################################
# Week-on-Week Infection Ratio #
################################

#### Load data ####
week_on_week_infection_ratio <- read_csv("data/singapore/covid-19-case-numbers/week-on-week-infection-ratio.csv")

#### Week-on-week Infection Ratio ####
week_on_week_infection_ratio %>% 
  ggplot(aes(x = pr_date, y = ratio_comm_cases_pw_over_wb)) +
  geom_step() +
  geom_hline(yintercept = 1,
             colour = "red",
             linetype = "dashed") +
  scale_x_date(labels = label_date_short(),
               date_breaks = "1 week") +
  theme_classic() +
  labs(x = "", y = "",
       title = glue("Week-on-week Infection Ratio in Singapore (updated {max(week_on_week_infection_ratio$pr_date)})"),
       subtitle = "Ratio above 1.0 means infections are continuing to rise",
       caption = "Data: data.gov.sg | Graphic: @weiyuet")

#### Save image ####
ggsave("figures/week-on-week-infection-ratio.png", width = 7, height = 5)