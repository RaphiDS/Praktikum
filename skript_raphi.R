# Skript Raphi
load("C:/Users/49177/Desktop/Praktikum/Praktikum GIthub/StatPrak-Overdose/combi_redu_data.Rdata")
drugdata <- allfilterdata
library(tidyverse)

# Column Graph for cigever
drugdata %>%
  group_by(year) %>%
  count(cigever) %>%
  mutate(relative = n / sum(n)) %>%
  filter(cigever == 1) %>%
  ggplot(aes(x = year, y = relative)) +
  geom_col() +
  xlab("Year") +
  ylab("Share of people that answered \"Yes\"") +
  ggtitle("Have you ever smoked part or all of a cigarette?") +
  theme_light()

# Column Graph for alcever
drugdata %>%
  group_by(year) %>%
  count(alcever) %>%
  mutate(relative = n / sum(n)) %>%
  filter(alcever == 1) %>%
  ggplot(aes(x = year, y = relative)) +
  geom_col() +
  xlab("Year") +
  ylab("Share of people that answered \"Yes\"") +
  ggtitle("Have you ever, even once, had a drink of any type of alcoholic beverage?
Please do not include times when you only had a sip or two from a drink.") +
  theme_light()

# Column Graph for herever
drugdata %>%
  group_by(year) %>%
  count(herever) %>%
  mutate(relative = n / sum(n)) %>%
  filter(herever == 1) %>%
  ggplot(aes(x = year, y = relative)) +
  geom_col() +
  xlab("Year") +
  ylab("Share of people that answered \"Yes\"") +
  ggtitle("Have you ever, even once, used heroin?") +
  theme_light()

# Column Graph for cocever
drugdata %>%
  group_by(year) %>%
  count(cocever) %>%
  mutate(relative = n / sum(n)) %>%
  filter(cocever == 1) %>%
  ggplot(aes(x = year, y = relative)) +
  geom_col() +
  xlab("Year") +
  ylab("Share of people that answered \"Yes\"") +
  ggtitle("Have you ever, even once, used any form of cocaine?") +
  theme_light()

# Column Graph for smklssevr
drugdata %>%
  group_by(year) %>%
  count(pipever) %>%
  mutate(relative = n / sum(n)) %>%
  filter(pipever == 1) %>%
  ggplot(aes(x = year, y = relative)) +
  geom_col() +
  xlab("Year") +
  ylab("Share of people that answered \"Yes\"") +
  ggtitle("Have you ever used \"smokeless\" tobacco, even once?") +
  theme_light()

drugdata %>%
  filter(aldaypwk < 10) %>%
  group_by(aldaypwk, year) %>%
  count() %>%
  ggplot(aes(x = year, y = n, fill = aldaypwk)) +
  geom_col(position = "dodge")

# ______________________________________________________________________________

# relative Line Diagram for cigever, connectign dots missing
drugdata %>%
  group_by(year) %>%
  count(cigever) %>%
  mutate(relative = n / sum(n)) %>%
  ggplot(aes(x = year, y = relative, color = cigever,)) +
  geom_point() +
  xlab("Ever smoked a cigarette?") +
  ylab("Relative share") +
  theme_light()


# Doesnt work ?
columngraph <- function(variable, title) {
  drugdata %>%
    group_by(year) %>%
    count(variable) %>%
    mutate(relative = n / sum(n)) %>%
    ggplot(aes(x = factor(variable, labels = c("Yes", "No")), y = relative, fill = variable)) +
    geom_col() +
    facet_grid(~year) +
    xlab(title) +
    ylab("relative share") +
    theme_light()
}
columngraph(drugdata$herever, "AB")


cigeverdata <-  drugdata %>%
    group_by(year) %>%
    count(cigever) %>%
    mutate(relative = n / sum(n)) %>%
    filter(cigever == 1) %>%
    select(year, relative) %>%
    mutate(drug = "cig")

alceverdata <-  drugdata %>%
  group_by(year) %>%
  count(alcever) %>%
  mutate(relative = n / sum(n)) %>%
  filter(alcever == 1) %>%
  select(year, relative) %>%
  mutate(drug = "alc")

hereverdata <-  drugdata %>%
  group_by(year) %>%
  count(herever) %>%
  mutate(relative = n / sum(n)) %>%
  filter(herever == 1) %>%
  select(year, relative) %>%
  mutate(drug = "her")

coceverdata <-  drugdata %>%
  group_by(year) %>%
  count(cocever) %>%
  mutate(relative = n / sum(n)) %>%
  filter(cocever == 1) %>%
  select(year, relative) %>%
  mutate(drug = "coc")
reldata <- matrix(0)
relative_data <- function(drug, text, index) {
  reldata[index] <- drugdata %>%
  group_by(year) %>%
  count(drugdata[,drug]) %>%
  mutate(relative = n / sum(n)) %>%
  filter(drugdata[,drug] == 1) %>%
  select(year, relative) %>%
  mutate(drugdata[,drug] = text)
}
  

everdata <- as.data.frame(rbind(alceverdata, cigeverdata, coceverdata, hereverdata))

ggplot(everdata, aes(x = year, y = relative, color = drug)) +
  geom_point() +
  geom_line() +
  theme_minimal()
