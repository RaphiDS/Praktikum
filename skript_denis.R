load("C:/Users/denis/OneDrive/Desktop/RStudio/locales/StatPrak-Overdose/NSDUH_2015.RData")
load("C:/Users/denis/OneDrive/Desktop/RStudio/locales/StatPrak-Overdose/NSDUH_2016.RData")
load("C:/Users/denis/OneDrive/Desktop/RStudio/locales/StatPrak-Overdose/NSDUH_2017.RData")
load("C:/Users/denis/OneDrive/Desktop/RStudio/locales/StatPrak-Overdose/NSDUH_2018.RData")
load("C:/Users/denis/OneDrive/Desktop/RStudio/locales/StatPrak-Overdose/NSDUH_2019.RData")

load("C:/Users/denis/OneDrive/Desktop/RStudio/locales/StatPrak-Overdose/combi_redu_data.Rdata")
drugusedata <- allfilterdata
library(tidyverse)
library(checkmate)
str(drugusedata)
summary(drugusedata)


# creating graph for days cigs used in the last 30 days
drugusedata %>%
  filter(CGR30USE >= 1 & CGR30USE <= 30) %>% 
  group_by(year) %>% 
  summarize(avg_days = mean(CGR30USE, na.rm = TRUE)) %>% 
  ggplot(aes(x = factor(year), y = avg_days)) + 
  geom_col(fill = "steelblue") + 
  labs(
    title = "average days of cigar use in the past 30 days",
    x = "Year", 
    y = "Average Days" 
  ) +
  theme_minimal() 
  
#NOCH NICHT WIRKLICH SINNVOLL
# Scatterplots for the number of days people smoked cigars in the past 30 days (2015-2019)
drugusedata %>%
  filter(CGR30USE >= 1 & CGR30USE <= 30) %>%
  ggplot(aes(x = year, y = CGR30USE)) +
  geom_point(position = position_jitter(width = 0.2, height = 0.2), 
             alpha = 0.5, color = "steelblue") +
  facet_wrap(~ year, nrow = 1, scales = "free_x") +
  labs(
    title = "Number of Days People Smoked Cigars in the Past 30 Days (2015-2019)",
    x = "Year",
    y = "Number of Days Smoked"
  ) +
  theme_minimal()




#smoked in the last 30 days in percent
drugusedata %>%
  group_by(year) %>%
  mutate(total_people = n()) %>%
  filter(CGR30USE >= 1 & CGR30USE <= 30) %>%
  summarize(
    total_people = first(total_people),
    smokers_count = n(),
    percentage = (smokers_count / total_people) * 100
  ) %>%
  ggplot(aes(x = factor(year), y = percentage)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Percentage of People Who Smoked Cigars in the Past 30 Days",
    x = "Year",
    y = "smoked in %"
  ) +
  theme_minimal()
