# Skript Raphi
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
