##########################
# Installation & Laden der benötigten Pakete
##########################

install.packages("tidyverse")
library(tidyverse)

##########################
# Laden der Daten
##########################

# Laden einer vorbereiteten Rdata-Datei.
# Enthalten ist ein Datensatz "allfilterdata", den wir zusätzlich als "drugdata" abspeichern.
load("Daten bearbeitet/combi_redu_data.Rdata")
drugdata <- allfilterdata

##########################
# Funktionen zur Datenvorbereitung und Auswertung
##########################

# 1) Funktion: Erzeugt für eine angegebene (Drogen-)Variable eine
#    zusammengefasste Tabelle, in der nur "Ja"-Antworten (Wert 1) berücksichtigt werden.
#    Angewendet auf die Frage "jemals probiert".
everdatafun <- function(datacol, drug) {
  drugdata %>%
    group_by(year) %>%
    count(.data[[datacol]]) %>%                # Zählt, wie oft jeder Wert in datacol pro Jahr vorkommt
    mutate("Rel. share" = n / sum(n)) %>%       # Relativer Anteil pro Jahr
    filter(.data[[datacol]] == 1) %>%           # Nur "Ja"-Antworten (Wert 1)
    ungroup() %>%
    mutate(Drug = drug,                         # Name der Droge in eine neue Spalte
           Year = year) %>%
    select(Year, "Rel. share", Drug)            # Auswahl der Spalten in passender Reihenfolge
}

# 2) Funktion: Erzeugt für eine angegebene (Drogen-)Variable eine
#    zusammengefasste Tabelle für "Konsum in den letzten 30 Tagen".
#    Dabei werden die Werte 1–30 als "Ja" interpretiert, darüber/NA als "Nein".
datafun30 <- function(datacol, drug) {
  drugdata %>%
    group_by(year) %>%
    summarise(
      TotalPeople = n(),                                         # Gesamtzahl Beobachtungen
      Users = sum(.data[[datacol]] >= 1 & .data[[datacol]] <= 30, na.rm = TRUE),
      "Rel. share" = Users / TotalPeople
    ) %>%
    mutate(Drug = drug) %>%                                      # Droge in eine neue Spalte
    select(Year = year, "Rel. share", Drug)                      # Spalten umbenennen/anordnen
}

##########################
# Beispiel: Erstellung kombinierter Dataframes für mehrere Drogen
##########################

# 1) "Jemals" für 4 Hauptdrogen
fourdrugsever <- as.data.frame(
  rbind(
    everdatafun("alcever", "Alcohol"),
    everdatafun("cigever", "Cigarettes"),
    everdatafun("cocever", "Cocaine"),
    everdatafun("herever", "Heroin")
  )
)

# 2) "Jemals" für verschiedene Tabakprodukte
tobaccoever <- as.data.frame(
  rbind(
    everdatafun("cigever", "Cigarettes"),
    everdatafun("smklssevr", "Smokeless Tobacco"),
    everdatafun("pipever", "Pipe"),
    everdatafun("cigarevr", "Cigar")
  )
)

# 3) "In den letzten 30 Tagen" für 4 Hauptdrogen
fourdrugs30 <- as.data.frame(
  rbind(
    datafun30("alcdays", "Alcohol"),
    datafun30("CIG30USE", "Cigarettes"),
    datafun30("COCUS30A", "Cocaine"),
    datafun30("HER30USE", "Heroin")
  )
)

# 4) "In den letzten 30 Tagen" für verschiedene Tabakprodukte
tobacco30 <- as.data.frame(
  rbind(
    datafun30("CIG30USE", "Cigarettes"),
    datafun30("SMKLSS30N", "Smokeless Tobacco"),
    # Bei Pfeife werden hier scheinbar die "jemals"-Werte genutzt, da eine andere Fragestellung besteht
    everdatafun("PIPE30DY", "Pipe"),
    datafun30("CGR30USE", "Cigar")
  )
)

##########################
# Plot-Erstellungen mittels ggplot2
##########################

# 1) Plot: "Jemals probiert" – 4 Hauptdrogen
ggplot(fourdrugsever, aes(x = Year, y = .data[["Rel. share"]], color = Drug)) +
  geom_point() +
  geom_line() +
  scale_color_brewer(palette = "Dark2") +  # Farbskala
  theme_light() +
  labs(
    title = "Relativer Anteil von Personen, die jemals bestimmte Drogen konsumiert haben",
    x = "Jahr",
    y = "Relativer Anteil"
  )

# 2) Plot: "In den letzten 30 Tagen" – 4 Hauptdrogen
ggplot(fourdrugs30, aes(x = Year, y = .data[["Rel. share"]], color = Drug)) +
  geom_point() +
  geom_line() +
  scale_color_brewer(palette = "Dark2") +
  theme_light() +
  labs(
    title = "Relativer Anteil von Personen, die in den letzten 30 Tagen bestimmte Drogen konsumiert haben",
    x = "Jahr",
    y = "Relativer Anteil"
  )

# 3) Plot: "Jemals probiert" – Tabakprodukte
ggplot(tobaccoever, aes(x = Year, y = .data[["Rel. share"]], color = Drug)) +
  geom_point() +
  geom_line() +
  scale_color_brewer(palette = "Dark2") +
  theme_light() +
  labs(
    title = "Relativer Anteil von Personen, die jemals bestimmte Tabakprodukte konsumiert haben",
    x = "Jahr",
    y = "Relativer Anteil"
  )

# 4) Plot: "In den letzten 30 Tagen" – Tabakprodukte
ggplot(tobacco30, aes(x = Year, y = .data[["Rel. share"]], color = Drug)) +
  geom_point() +
  geom_line() +
  scale_color_brewer(palette = "Dark2") +
  theme_light() +
  labs(
    title = "Relativer Anteil von Personen, die in den letzten 30 Tagen bestimmte Tabakprodukte konsumiert haben",
    x = "Jahr",
    y = "Relativer Anteil"
  )

# Hinweis: Skalen für die Y-Achse könnten ggf. angepasst werden (ylim etc.).

##########################
# Balkendiagramme: Funktion zum Plotten einzelner Drogen
##########################

graphfun1 <- function(drug, question, ymax) {
  drug %>%
    ggplot(aes(x = Year, y = .data[["Rel. share"]])) +
    geom_line() +
    geom_point() +
    scale_color_brewer(palette = "Dark2", aesthetics = "color") +  # Farbskala, hier nur relevant, wenn ein 'color'-Mapping vorhanden wäre
    ggtitle(question) +                                            # Dieser Text kommt als Argument, hier also ggf. manuell anpassen
    theme_light() +
    ylim(0, ymax) +
    labs(
      x = "Jahr",
      y = "Relativer Anteil"
    )
}

# Beispielaufrufe für einzelne Drogen
graphfun1(
  everdatafun("cigever", "Cigarettes"),
  "Haben Sie jemals eine Zigarette (ganz oder teilweise) geraucht?",
  0.6
)
graphfun1(
  everdatafun("alcever", "Alcohol"),
  "Haben Sie jemals, auch nur einmal, irgendein alkoholisches Getränk zu sich genommen?",
  0.8
)
graphfun1(
  everdatafun("herever", "Heroin"),
  "Haben Sie jemals, auch nur einmal, Heroin konsumiert?",
  0.025
)
graphfun1(
  everdatafun("cocever", "Cocaine"),
  "Haben Sie jemals, auch nur einmal, irgendeine Form von Kokain konsumiert?",
  0.15
)

##########################
# Histogramme: Zigaretten (z.B. gerauchte Tage)
##########################

# Beispiel: Datensatz für Zigarren, bei dem Code-Werte wie 91,93,94,97,98 herausgefiltert werden
cigarettesclean <- drugdata %>%
  mutate(
    CGR30USE = ifelse(CGR30USE %in% c(91, 93, 94, 97, 98), NA, CGR30USE)
  ) %>%
  filter(!is.na(CGR30USE)) %>%
  filter(CGR30USE > 0, CGR30USE <= 30)

# Histogramm über gerauchte Tage, gruppiert nach Jahr
cigarettesclean %>%
  ggplot(aes(x = CGR30USE)) +
  geom_histogram(
    aes(y = after_stat(count / sum(count))),
    breaks = seq(0.5, 30.5, by = 1),
    color = "black",
    na.rm = TRUE
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

# Ähnliches Histogramm, etwas einfacher gehalten (Zigarren pro Jahr)
drugdata %>%
  filter(CGR30USE >= 1 & CGR30USE <= 30) %>%
  ggplot(aes(x = CGR30USE)) +
  geom_histogram(binwidth = 1, color = "black") +
  facet_wrap(~ year, nrow = 1, scales = "free_y") +
  labs(
    title = "Histogramme der Anzahl gerauchter Zigarren (2015-2019)",
    x = "Anzahl gerauchter Tage",
    y = "Häufigkeit"
  ) +
  theme_light()

##########################
# Funktion zur Berechnung: Durchschnittliche Nutzungstage pro Jahr (inklusive Nicht-Nutzern = 0)
##########################

# Hilfsfunktion: Datensatz vorbereiten
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

# Durchschnittliche Nutzungstage pro Droge
avg_data_fun <- function(datacol, drug_name) {
  prepare_data(datacol, drug_name) %>%
    group_by(year, Drug) %>%
    summarize(avg_days = mean(UsageDays, na.rm = TRUE), .groups = "drop")
}

# Beispiel: Drogen mit Durchschnittswerten pro Jahr
her30_avg_data <- avg_data_fun("HER30USE", "Heroin")
coc30_avg_data <- avg_data_fun("COCUS30A", "Cocaine")
alc30_avg_data <- avg_data_fun("alcdays", "Alcohol")
cig30_avg_data <- avg_data_fun("CIG30USE", "Cigarettes")

# Zusammenführen in einen Dataframe
combined_avg_usage <- bind_rows(
  her30_avg_data,
  coc30_avg_data,
  alc30_avg_data,
  cig30_avg_data
)

# Plot: Durchschnittliche Nutzungstage (inkl. 0er)
ggplot(combined_avg_usage, aes(x = year, y = avg_days, color = Drug)) +
  geom_point() +
  geom_line() +
  scale_color_brewer(palette = "Dark2") +
  theme_light() +
  labs(
    title = "Durchschnittliche Anzahl an Konsumtagen (inkl. Nicht-Nutzer als 0)",
    x = "Jahr",
    y = "Durchschnittliche Nutzungstage"
  )

##########################
# Funktion: Histogramm der Nutzungstage (1–30)
##########################

histogram_fun <- function(datacol, drug_name) {
  data <- drugdata %>%
    group_by(year) %>%
    count(day = .data[[datacol]]) %>%
    mutate("Relative share" = n / sum(n)) %>%
    filter(day >= 1 & day <= 30) %>%
    ungroup() %>%
    mutate(Drug = drug_name)
  
  ggplot(data, aes(x = factor(day), y = `Relative share`)) +
    geom_col(fill = "darkgrey", color = "black", alpha = 0.7) +
    facet_wrap(~ year) +
    theme_light() +
    labs(
      title = paste0("Verteilung der Nutzungstage für ", drug_name),
      x = "Anzahl der Nutzungstage in den letzten 30 Tagen",
      y = "Relativer Anteil"
    )
}

# Beispielaufrufe
histogram_fun("CIG30USE", "Cigarettes")
histogram_fun("alcdays", "Alcohol")
histogram_fun("COCUS30A", "Cocaine")
histogram_fun("HER30USE", "Heroin")

##########################
# Funktion: Mosaikplot für bivariate Daten (Werte 1 und 2)
##########################

mosaicfun <- function(var1, var2, varname1, varname2) {
  data.frame(
    matrix(
      c(
        allfilterdata %>%
          filter(.data[[var1]] == 1 & .data[[var2]] == 1) %>%
          count(),
        allfilterdata %>%
          filter(.data[[var1]] == 2 & .data[[var2]] == 1) %>%
          count(),
        allfilterdata %>%
          filter(.data[[var1]] == 1 & .data[[var2]] == 2) %>%
          count(),
        allfilterdata %>%
          filter(.data[[var1]] == 2 & .data[[var2]] == 2) %>%
          count()
      ),
      nrow = 2
    ),
    row.names = c(paste(varname1, "Yes"), paste(varname1, "No"))
  ) %>%
    rename(
      !!paste(varname2, "Yes") := X1,
      !!paste(varname2, "No")  := X2
    ) %>%
    mosaicplot(
      color = TRUE,
      main = paste("Korrelation zwischen", varname1, "und", varname2)
    )
}

# Beispielaufrufe für Mosaikplots
mosaicfun("herever", "cocever", "Heroin", "Cocaine")
mosaicfun("cigever", "cocever", "Cigarettes", "Cocaine")
mosaicfun("alcever", "cocever", "Alcohol", "Cocaine")
mosaicfun("alcever", "cigever", "Alcohol", "Cigarettes")

##########################
# Erweiterung: Mosaikplot für "letzte 30 Tage"
##########################

mosaicfun30 <- function(var1, var2, varname1, varname2) {
  
  # Datensatz, in dem var1 und var2 bei Werten 1–30 auf 1 recodiert werden, ansonsten auf 2
  allfilterdata30 <- allfilterdata %>%
    mutate(
      usage1 = if_else(.data[[var1]] >= 1 & .data[[var1]] <= 30, 1, 2),
      usage2 = if_else(.data[[var2]] >= 1 & .data[[var2]] <= 30, 1, 2)
    )
  
  # 2x2-Tabelle aus den Kombinationen (Yes/Yes, No/Yes, Yes/No, No/No)
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
    rename(
      !!paste(varname2, "Yes") := X1,
      !!paste(varname2, "No")  := X2
    ) %>%
    mosaicplot(
      color = TRUE,
      main = paste("Korrelation zwischen", varname1, "und", varname2, "(Letzte 30 Tage)")
    )
}

# Beispielaufruf: Mosaikplot für letzte 30 Tage
mosaicfun30("CIG30USE", "COCUS30A", "Cigarettes", "Cocaine")
