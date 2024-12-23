# Skript Raphi
load("C:/Users/49177/Desktop/Praktikum/Praktikum GIthub/StatPrak-Overdose/Daten bearbeitet/combi_redu_data.Rdata")
drugdata <- allfilterdata
library(tidyverse)

#extract only the people who answered Yes and cleaning up
everdatafun <- function(datacol, drug) {
  drugdata %>%
    group_by(year) %>%
    count(.data[[datacol]]) %>%
    mutate("Rel. share" = n / sum(n)) %>%
    filter(.data[[datacol]] == 1) %>%
    ungroup() %>%
    mutate(Drug = drug, Year = year) %>%
    select(Year, "Rel. share", Drug)
}
# small table for each drug over the years
cigeverdata <- everdatafun("cigever", "Cigarettes")
alceverdata <- everdatafun("alcever", "Alcohol")
hereverdata <- everdatafun("herever", "Heroin")
coceverdata <- everdatafun("cocever", "Cocaine")
smklsseverdata <- everdatafun("smklssevr", "Smokeless Tobacco")
cigareverdata <- everdatafun("cigarevr", "Cigar")
pipeeverdata <- everdatafun("pipever", "Pipe")
# creating combined df to plot in the same graph
fourdrugsever <- as.data.frame(rbind(alceverdata, cigeverdata, coceverdata, hereverdata))
tobaccoever <- as.data.frame(rbind(cigeverdata, smklsseverdata, pipeeverdata))
# Plot for Drug Use
ggplot(fourdrugsever, aes(x = Year, y = .data[["Rel. share"]], color = Drug)) +
  geom_point() + geom_line() +
  theme_light() +
  labs(title = "Relative share of people who have
ever tried certain drugs")
# Plot for Tobacco Use
ggplot(tobaccoever, aes(x = Year, y = .data[["Rel. share"]], color = Drug)) +
  geom_point() + geom_line() +
  theme_light() +
  labs(title = "Relative share of people who have
ever tried certain forms of tobacco")

# -----------------------------------------------------------------------------------
# Columngraph for each drug individually
graphfun1 <- function (drug, question, ymax) {
  drug %>%
    ggplot(aes(x = Year, y = .data[["Rel. share"]])) +
    geom_line() + geom_point() +
    ggtitle(question) +
    theme_light() +
    ylim(0, ymax)
}

graphfun1(cigeverdata, "Have you ever smoked part or all of a cigarette?", 0.6)
graphfun1(alceverdata, "Have you ever, even once, had a drink of any type of alcoholic beverage?
Please do not include times when you only had a sip or two from a drink.", 0.8)
graphfun1(hereverdata, "Have you ever, even once, used heroin?", 0.025)
graphfun1(coceverdata, "Have you ever, even once, used any form of cocaine?", 0.15)
graphfun1(smklsseverdata, "Have you ever used \"smokeless\" tobacco, even once?", 0.2)

#how often people drank over the years
drugdata %>%
  filter(aldaypwk < 10) %>%
  group_by(year, aldaypwk) %>%
  count() %>%
  ungroup(aldaypwk) %>%
  mutate(rel = n / sum(n)) %>%
  ggplot(aes(x = year, y = rel, color = factor(aldaypwk))) +
  geom_line() +
  geom_point() +
  theme_light() +
  labs(title = "On how many days per week people drink", x = "Year", y = "Rel. Share", color = "Days per week")

#Tabelle mit Pfeife und Cigaretten als Beispiel evtl. Korrelation Odds Ratio
data.frame(matrix(c(allfilterdata %>%
               filter(cigever == 1 & pipever == 1) %>%
               count(), allfilterdata %>%
               filter(cigever == 2 & pipever == 1) %>%
               count(), allfilterdata %>%
               filter(cigever == 1 & pipever == 2) %>%
               count(), allfilterdata %>%
               filter(cigever == 2 & pipever == 2) %>%
               count()), nrow = 2), row.names = c("Cig Yes", "Cig No")) %>%
  rename("Pipe Yes" = X1 , "Pipe No" = X2)

boxplotfun <- function(drug) {
  allfilterdata %>%
    select(.data[[drug]], year) %>%
    filter(.data[[drug]] < 200) %>%
    group_by(year, .data[[drug]]) %>%
    count() %>%
    ungroup() %>%
    ggplot(aes(y = .data[[drug]])) +
    geom_boxplot() +
    facet_grid(~year) +
    theme_light()
}

boxplotfun("cocage")
boxplotfun("cigage")
boxplotfun("herage")
boxplotfun("alctry")


# Bedingte Häufigkeit bei Drogenkonsum als tabelle machen, nicht nur Tabak

# Farbskalen