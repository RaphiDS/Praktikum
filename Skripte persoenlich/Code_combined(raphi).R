install.packages("tidyverse")
library(tidyverse)
load("Daten bearbeitet/combi_redu_data.Rdata")
drugdata <- allfilterdata

# erstellt kleine Tabelle nur mit Ja-Antworten für angegebene Drogen, jemals probiert
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
# erstellt kleine Tabelle nur mit Ja-Antworten für angegebene Drogen, in den letzten 30 Tagen konsumiert
datafun30 <- function(datacol, drug) {
  drugdata %>%
    group_by(year) %>%
    summarise(
      TotalPeople = n(),
      Users = sum(.data[[datacol]] >= 1 & .data[[datacol]] <= 30, na.rm = TRUE),
      "Rel. share" = Users / TotalPeople
    ) %>%
    mutate(Drug = drug) %>%
    select(Year = year, "Rel. share", Drug)
}

# kombinierter Dataframe für die 4 Hauptdrogen
fourdrugsever <- as.data.frame(rbind(everdatafun("alcever", "Alcohol"),
                                     everdatafun("cigever", "Cigarettes"),
                                     everdatafun("cocever", "Cocaine"),
                                     everdatafun("herever", "Heroin")))
# und für verschiedene Sorten von Tabak
tobaccoever <- as.data.frame(rbind(everdatafun("cigever", "Cigarettes"),
                                   everdatafun("smklssevr", "Smokeless Tobacco"),
                                   everdatafun("pipever", "Pipe"),
                                   everdatafun("cigarevr", "Cigar")))
# kombinierter Dataframe für 4 Huaptdrogen in den letzten 30 Tagen
fourdrugs30 <- as.data.frame(rbind(datafun30("alcdays", "Alcohol"),
                                   datafun30("CIG30USE", "Cigarettes"),
                                   datafun30("COCUS30A", "Cocaine"),
                                   datafun30("HER30USE", "Heroin")))
# kombinierter Dataframe für Arten von Tabak in den letzten 30 Tagen
tobacco30 <- as.data.frame(rbind(datafun30("CIG30USE", "Cigarettes"),
                                 datafun30("SMKLSS30N", "Smokeless Tobacco"),
                                 everdatafun("PIPE30DY", "Pipe"), # bei Pfeife andere Fragestellung, funktioniert mit Fkt von oben
                                 datafun30("CGR30USE", "Cigar")))

# Plot für Drogen jemals
ggplot(fourdrugsever, aes(x = Year, y = .data[["Rel. share"]], color = Drug)) +
  geom_point() +
  geom_line() +
  theme_light() +
  labs(title = "Relative share of people who have
ever tried certain drugs")
# Plot für Drogen letzte 30 Tage
ggplot(fourdrugs30, aes(x = Year, y = .data[["Rel. share"]], color = Drug)) +
  geom_point() +
  geom_line() +
  theme_light() +
  labs(title = "Relative share of people who have
done certain drugs in the last 30 days")

# Plot für Tabak jemals
ggplot(tobaccoever, aes(x = Year, y = .data[["Rel. share"]], color = Drug)) +
  geom_point() +
  geom_line() +
  theme_light() +
  labs(title = "Relative share of people who have
ever tried certain forms of tobacco")
# Plot für Tabak letzte 30 Tage
ggplot(tobacco30, aes(x = Year, y = .data[["Rel. share"]], color = Drug)) +
  geom_point() +
  geom_line() +
  theme_light() +
  labs(title = "Relative share of people who have
smoked forms of tobacco in the last 30 days")

# EVENTUELL SKALEN ANPASSEN BEI PLOTS?

# Balkendiagramm für die Drogen einzeln
graphfun1 <- function (drug, question, ymax) {
  drug %>%
    ggplot(aes(x = Year, y = .data[["Rel. share"]])) +
    geom_line() + geom_point() +
    ggtitle(question) +
    theme_light() +
    ylim(0, ymax)
}
graphfun1(everdatafun("cigever", "Cigarettes"), "Have you ever smoked part or all of a cigarette?", 0.6)
graphfun1(everdatafun("alcever", "Alcohol"), "Have you ever, even once, had a drink of any type of alcoholic beverage?
Please do not include times when you only had a sip or two from a drink.", 0.8)
graphfun1(everdatafun("herever", "Heroin"), "Have you ever, even once, used heroin?", 0.025)
graphfun1(everdatafun("cocever", "Cocaine"), "Have you ever, even once, used any form of cocaine?", 0.15)


# cigs relative Häufigkeit der gerauchten Tage 

cigarettesclean <- drugdata %>%
  mutate(
    CGR30USE = ifelse(CGR30USE %in% c(91,93,94,97,98), NA, CGR30USE)
  ) %>%
  filter(!is.na(CGR30USE)) %>%
  filter(CGR30USE > 0, CGR30USE <= 30)

cigarettesclean %>%
  ggplot(aes(x = CGR30USE)) +
  geom_histogram(
    aes(y = after_stat(count / sum(count))),
    breaks = seq(0.5, 30.5, by = 1),
    color = "black",
    na.rm = TRUE  # Entfernt dennoch eventuell auftretende NA-Werte beim Plotten
  ) +
  facet_wrap(~ year, nrow = 1, scales = "free_y") +
  scale_x_continuous(breaks = seq(5, 30, by = 5), limits = c(1, 30)) +
  ylim(0, 0.035) +
  labs(
    title = "Relative Häufigkeiten der Rauchtage (2015-2019)",
    x = "Anzahl der gerauchten Tage",
    y = "Relative Häufigkeit"
  ) +
  theme_light()

drugdata %>%
  filter(CGR30USE >= 1 & CGR30USE <= 30) %>%
  ggplot(aes(x = CGR30USE)) +
  geom_histogram(binwidth = 1, color = "black") +
  facet_wrap(~ year, nrow = 1, scales = "free_y") +
  labs(
    title = "Histograms of Number of Days Smoked Cigars (2015-2019)",
    x = "Number of Days Smoked",
    y = "Frequency"
  ) +
  theme_light()

prepare_data <- function(datacol, drug_name) {
  allfilterdata %>%
    mutate(
      UsageDays = case_when(
        .data[[datacol]] >= 1 & .data[[datacol]] <= 30 ~ .data[[datacol]],
        TRUE ~ 0
      ),
      Drug = drug_name
    ) %>%
    select(year, Drug, UsageDays)
}

# Funktion: Durchschnittliche Nutzungstage pro Jahr (inkl. 0-Tage-Nutzer)
avg_data_fun <- function(datacol, drug_name) {
  prepare_data(datacol, drug_name) %>%
    group_by(year, Drug) %>%
    summarize(avg_days = mean(UsageDays, na.rm = TRUE), .groups = "drop")
}

# Daten für jede Droge mit Durchschnitt berechnen
her30_avg_data <- avg_data_fun("HER30USE", "Heroin")
coc30_avg_data <- avg_data_fun("COCUS30A", "Cocaine")
alc30_avg_data <- avg_data_fun("alcdays", "Alcohol")
cig30_avg_data <- avg_data_fun("CIG30USE", "Cigarettes")

# Kombinierte Daten für das Liniendiagramm
combined_avg_usage <- bind_rows(her30_avg_data, coc30_avg_data, alc30_avg_data, cig30_avg_data)

# Liniendiagramm mit Durchschnittstagen (inkl. 0er)
ggplot(combined_avg_usage, aes(x = year, y = avg_days, color = Drug)) +
  geom_point() +
  geom_line() +
  theme_light() +
  labs(
    title = "Average number of days using substances (including non-users as 0)",
    x = "Year",
    y = "Average Usage Days"
  )


histogram_fun <- function(datacol, drug_name) {
  data <- drugdata %>%
    group_by(year) %>%
    count(day = .data[[datacol]]) %>%
    mutate("Relative share" = n / sum(n)) %>%
    # Nur Tage zwischen 1 und 30 filtern
    filter(day >= 1 & day <= 30) %>%
    ungroup() %>%
    mutate(Drug = drug_name)
  
  ggplot(data, aes(x = factor(day), y = `Relative share`)) +
    geom_col(fill = "darkgrey", color = "black", alpha = 0.7) +
    facet_wrap(~year) +
    theme_light() +
    labs(
      title = paste0("Distribution of usage days for ", drug_name),
      x = "Number of Days Used in Last 30 Days",
      y = "Relative Share"
    )
}

# Aufrufe der Plots für jede Droge:
histogram_fun("CIG30USE", "Cigarettes")
histogram_fun("alcdays", "Alcohol")
histogram_fun("COCUS30A", "Cocaine")
histogram_fun("HER30USE", "Heroin")

# Funktion für Mosaikplots für bivariate Daten mit Ausprägung 1 und 2
mosaicfun <- function(var1, var2, varname1, varname2) {
  data.frame(matrix(c(allfilterdata %>%
                        filter(.data[[var1]] == 1 & .data[[var2]] == 1) %>%
                        count(), allfilterdata %>%
                        filter(.data[[var1]] == 2 & .data[[var2]] == 1) %>%
                        count(), allfilterdata %>%
                        filter(.data[[var1]] == 1 & .data[[var2]] == 2) %>%
                        count(), allfilterdata %>%
                        filter(.data[[var1]] == 2 & .data[[var2]] == 2) %>%
                        count()), nrow = 2), row.names = c(paste(varname1, "Yes"), paste(varname1,"No"))) %>%
    rename(!!paste(varname2, "Yes") := X1 , !!paste(varname2, "No") := X2) %>%
    mosaicplot(color = TRUE, main = paste("Correlation between", varname1, "and", varname2))
}

mosaicfun("herever", "cocever", "Heroin", "Cocaine")
mosaicfun("cigever", "cocever", "Cigarettes", "Cocaine")
mosaicfun("alcever", "cocever", "Alcohol", "Cocaine")
mosaicfun("alcever", "cigever", "Alcohol", "Cigarettes")

# Mosaic Fun for 30 days instead of ever

mosaicfun30 <- function(var1, var2, varname1, varname2) {
  
  # 1) Erzeuge eine Hilfs-Tabelle, in der du var1 und var2 basierend auf 1–30 Tagen recodierst
  #    => 1 = Nutzung in den letzten 30 Tagen, 2 = keine Nutzung in den letzten 30 Tagen
  allfilterdata30 <- allfilterdata %>%
    mutate(
      usage1 = if_else(.data[[var1]] >= 1 & .data[[var1]] <= 30, 1, 2),
      usage2 = if_else(.data[[var2]] >= 1 & .data[[var2]] <= 30, 1, 2)
    )
  
  # 2) Erstelle eine 2x2-Matrix aus den 4 Zellen:
  #    (Yes/Yes, No/Yes, Yes/No, No/No)
  #    Die count()-Werte werden jeweils aus dem gefilterten Datensatz ermittelt.
  data.frame(
    matrix(
      c(
        allfilterdata30 %>% filter(usage1 == 1 & usage2 == 1) %>% count(),
        allfilterdata30 %>% filter(usage1 == 2 & usage2 == 1) %>% count(),
        allfilterdata30 %>% filter(usage1 == 1 & usage2 == 2) %>% count(),
        allfilterdata30 %>% filter(usage1 == 2 & usage2 == 2) %>% count()
      ),
      nrow = 2
    ),
    row.names = c(paste(varname1, "Yes"), paste(varname1, "No"))
  ) %>%
    # 3) Spalten dynamisch umbenennen, damit wir z.B. "Marijuana Yes"/"Marijuana No" o.Ä. bekommen
    rename(
      !!paste(varname2, "Yes") := X1,
      !!paste(varname2, "No")  := X2
    ) %>%
    # 4) Den Mosaikplot erzeugen
    mosaicplot(
      color = TRUE,
      main = paste("Correlation between", varname1, "and", varname2, "(Last 30 days)")
    )
}

mosaicfun30("CIG30USE", "COCUS30A", "Cigarettes", "Cocaine")
