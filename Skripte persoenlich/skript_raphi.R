# Skript Raphi
load("C:/Users/49177/Desktop/Praktikum/Praktikum GIthub/StatPrak-Overdose/Daten bearbeitet/combi_redu_data.Rdata")
drugdata <- allfilterdata
library(tidyverse)

#extract only the people who answered Yes and cleaning up
everdatafun <- function(datacol, drug) {
  drugdata %>%
    group_by(year) %>%
    count(.data[[datacol]]) %>%
    mutate(relative = n / sum(n)) %>%
    filter(.data[[datacol]] == 1) %>%
    mutate(drug = drug) %>%
    select(year, relative, drug)
}

cigeverdata <- everdatafun("cigever", "Cigarettes")
alceverdata <- everdatafun("alcever", "Alcohol")
hereverdata <- everdatafun("herever", "Heroin")
coceverdata <- everdatafun("cocever", "Cocaine")
smklsseverdata <- everdatafun("smklssevr", "Smokeless Tobacco")
cigareverdata <- everdatafun("cigarevr", "Cigar")
pipeeverdata <- everdatafun("pipever", "Pipe")



#creating combined df to plot in the same graph
everdata <- as.data.frame(rbind(alceverdata, cigeverdata, coceverdata, hereverdata))
tobaccodata <- as.data.frame(rbind(cigeverdata, smklsseverdata, pipeeverdata))

#Plot for Drug Use
ggplot(everdata, aes(x = year, y = relative, color = drug)) +
  geom_point() + geom_line() +
  theme_minimal()

# Plot for Tobacco Use
ggplot(tobaccodata, aes(x = year, y = relative, color = drug)) +
  geom_point() + geom_line() +
  theme_minimal()

### Columngraph for each drug individually
graphfun1 <- function (drug, question) {
  drug %>%
    ggplot(aes(x = year, y = relative)) +
    geom_col(alpha = 0.6) + geom_line() + geom_point() +
    xlab("Year") +
    ylab("Share of people that answered \"Yes\"") +
    ggtitle(question) +
    theme_light()
}

graphfun1(cigeverdata, "Have you ever smoked part or all of a cigarette?")
graphfun1(alceverdata, "Have you ever, even once, had a drink of any type of alcoholic beverage?
Please do not include times when you only had a sip or two from a drink.")
graphfun1(hereverdata, "Have you ever, even once, used heroin?")
graphfun1(coceverdata, "Have you ever, even once, used any form of cocaine?")
graphfun1(smklsseverdata, "Have you ever used \"smokeless\" tobacco, even once?")

#_______________________________________________________________________

# to do: display how often people smoked over the years
drugdata %>%
  filter(aldaypwk < 10) %>%
  group_by(year, aldaypwk) %>%
  count() %>%
  ggplot(aes(x = factor(year), y = n, fill = factor(aldaypwk))) +
  geom_col(position = "fill", color = "black")