library(tidyverse)
library(ggplot2)
library(viridis)

load("~/Desktop/R Studio Data/Household Data/d_HHP2020_241.Rdata")

# Create partnered variable
d_HHP2020_24$partnered <- (d_HHP2020_24$Mar_Stat == "Married") |
  (d_HHP2020_24$Mar_Stat == "widowed") |
  (d_HHP2020_24$Mar_Stat == "divorced") |
  (d_HHP2020_24$Mar_Stat == "separated")

# Ages 30-34
d_HHP_Age30_34 <- d_HHP2020_24 %>%
  filter((Age >= 30) & (Age < 35) & !is.na(partnered))

# Partnered by state
frac_MS_byState <- d_HHP_Age30_34 %>%
  group_by(State, partnered) %>%
  summarize(n = n()) %>%
  mutate(freq_in_group = n / sum(n))

# Partnered by region
frac_MS_byRegion <- d_HHP_Age30_34 %>%
  group_by(Region, partnered) %>%
  summarize(n = n()) %>%
  mutate(freq_in_group = n / sum(n))

# Partnered by census division
frac_MS_byDivision <- d_HHP_Age30_34 %>%
  group_by(Census_division, partnered) %>%
  summarize(n = n()) %>%
  mutate(freq_in_group = n / sum(n))

# Partnering rates by age
HHP_use <- d_HHP2020_24 %>%
  filter(Age < 88)

partner_rate_sum <- HHP_use %>%
  group_by(Age) %>%
  summarize(
    mar_rate = sum(Mar_Stat == "Married", na.rm = TRUE) / sum(!is.na(Mar_Stat)),
    divwidsep_rate = sum(
      (Mar_Stat == "divorced") |
        (Mar_Stat == "separated") |
        (Mar_Stat == "widowed"),
      na.rm = TRUE
    ) / sum(!is.na(Mar_Stat)),
    single_rate = sum(Mar_Stat == "never", na.rm = TRUE) / sum(!is.na(Mar_Stat)),
    number_obs = n()
  )

partner_for_graph1 <- partner_rate_sum %>%
  select(-number_obs) %>%
  pivot_longer(!Age, names_to = "partner", values_to = "rate")

ggplot(partner_for_graph1, aes(x = Age, y = rate, colour = partner)) +
  geom_line(linewidth = 1) +
  scale_color_viridis_d(option = "inferno", end = 0.75)

partner_for_graph2 <- partner_rate_sum %>%
  mutate(partner_rate = mar_rate + divwidsep_rate) %>%
  select(-c("mar_rate", "divwidsep_rate", "number_obs")) %>%
  pivot_longer(!Age, names_to = "partner", values_to = "rate")

ggplot(partner_for_graph2, aes(x = Age, y = rate, colour = partner)) +
  geom_line(linewidth = 1) +
  scale_color_viridis_d(option = "inferno", end = 0.75)

# Income and partnered rate by region
partner_region_income <- d_HHP2020_24 %>%
  filter(!is.na(partnered),
         !is.na(income_midpoint_factor),
         !is.na(Region)) %>%
  group_by(Region, income_midpoint_factor, partnered) %>%
  summarize(n = n()) %>%
  mutate(freq_in_group = n / sum(n))

partner_region_income_true <- partner_region_income %>%
  filter(partnered == TRUE)

ggplot(partner_region_income_true,
       aes(x = income_midpoint_factor,
           y = freq_in_group,
           colour = Region,
           group = Region)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(
    x = "Income Level",
    y = "Fraction Partnered",
    title = "Partnered Rate by Income and Region"
  )

# Education and partnered rate by region, ages 30-45
partner_edu_age30_45 <- d_HHP2020_24 %>%
  filter(Age >= 30,
         Age <= 45,
         !is.na(partnered),
         !is.na(Education),
         !is.na(Region)) %>%
  group_by(Region, Education, partnered) %>%
  summarize(n = n()) %>%
  mutate(freq_in_group = n / sum(n))

partner_edu_age30_45_true <- partner_edu_age30_45 %>%
  filter(partnered == TRUE)

ggplot(partner_edu_age30_45_true,
       aes(x = Education,
           y = freq_in_group,
           fill = Region)) +
  geom_col(position = "dodge") +
  labs(
    x = "Education Level",
    y = "Fraction Partnered",
    title = "Partnered Rate by Education and Region, Ages 30-45"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Work loss and partnered rate by region
partner_workloss <- d_HHP2020_24 %>%
  filter(!is.na(partnered),
         !is.na(workloss),
         !is.na(Region)) %>%
  group_by(Region, workloss, partnered) %>%
  summarize(n = n()) %>%
  mutate(freq_in_group = n / sum(n))

partner_workloss_true <- partner_workloss %>%
  filter(partnered == TRUE)

ggplot(partner_workloss_true,
       aes(x = workloss,
           y = freq_in_group,
           fill = Region)) +
  geom_col(position = "dodge") +
  labs(
    x = "Recent Household Work Loss",
    y = "Fraction Partnered",
    title = "Partnered Rate by Work Loss and Region"
  )