
library(tidyverse)
library(usmap)
library(usdata)

########################################################################################################################

load("data_edit/filtered_data.Rdata")

drugdata <- allfilterdata %>%
  mutate(NEWRACE2 =
           factor(NEWRACE2,
                  levels = c(1, 7, 2, 5, 6, 3, 4)
           ))

data2019 <- drugdata %>%
  filter(year == 2019)

########################################################################################################################

theme_custom <-
  theme_light() +
  theme(
    axis.title = element_text(size = 20),
    axis.text  = element_text(size = 20),
    axis.title.x = element_text(margin = margin(t = 20)),
    axis.title.y = element_text(margin = margin(r = 20)),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15)
    )

theme_set(theme_custom)

my_plot <- function(...) {
    ... +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(y = "Prozent", title = "")
}

newrace2_vector <-
  c(
    "1" = "Weiße",
    "2" = "Schwarze\nAfroamerikaner",
    "3" = "Am/Ak\nIndigene",
    "4" = "Indigene Hawaii\n/Paz. Inseln",
    "5" = "Asiaten",
    "6" = "Gemischte",
    "7" = "Hispanische"
    )

catag2_vector <-
  c(
    "1" = "12-17",
    "2" = "18-25",
    "3" = "26+"
    )

irsex_vector <-
  c(
    "1" = "Männer",
    "2"= "Frauen"
    )

colors_drugs <- c("#0072B2", "#009E73", "#E69F00", "#CC79A7")
labels_drugs <- c("Alkohol", "Zigarette", "Kokain", "Heroin")
shapes_drugs <- c(15:18)

colors_tobacco <- c("#009E73", "darkorchid4","lightblue", "darkgrey")
labels_tobacco <- c("Zigarette", "Zigarre", "Rauchfreier Tabak", "Pfeife")
shapes_tobacco <- c(c(16, 15, 17, 18))

########################################################################################################################

demo_distr_fun <- function(demovar, label_vec, xlab) {
  my_plot(data2019 %>%
  select(all_of(demovar)) %>%
  group_by(.data[[demovar]]) %>%
  summarize(count = n()) %>%
  mutate(count = count / sum(count)) %>%  
  ggplot(aes(
    x = factor(.data[[demovar]]),
    y = count)
    ) +
  geom_col() +
  scale_x_discrete(labels = label_vec) +
  labs(x = xlab))
}

Race.Distribution <-
  demo_distr_fun("NEWRACE2", newrace2_vector, "Race")

Age.Distribution <-
  demo_distr_fun("CATAG2", catag2_vector, "Altersgruppen")

Gender.Distribution <-
  demo_distr_fun("irsex", irsex_vector, "Geschlecht")

########################################################################################################################

everdatafun <- function(datacol, drug) {
  drugdata %>%
  group_by(year, .data[[datacol]]) %>%
  summarize(count = n()) %>%
  mutate(
    count = count / sum(count),
    drug = drug
    ) %>%
  filter(.data[[datacol]] == 1)
}

datafun30 <- function(datacol, drug) {
  drugdata %>%
  group_by(year, .data[[datacol]]) %>%
  summarise(count = n()) %>%
  mutate(
    count = count / sum(count),
    drug = drug
    ) %>%
  filter(
      .data[[datacol]] > 0 &
      .data[[datacol]] < 31
    ) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(count = sum(count)) %>%
  filter(.data[[datacol]] == 1)
}

########################################################################################################################

fourdrugsever <- as.data.frame(
  rbind(
    everdatafun("alcever", "Alkohol"),
    everdatafun("cigever", "Zigarette"),
    everdatafun("cocever", "Kokain"),
    everdatafun("herever", "Heroin")
  )
)

tobaccoever <- as.data.frame(
  rbind(
    everdatafun("cigever", "Zigarette"),
    everdatafun("smklssevr", "Rauchfreier Tabak"),
    everdatafun("pipever", "Pfeife"),
    everdatafun("cigarevr", "Zigarre")
  )
)

fourdrugs30 <- as.data.frame(
  rbind(
    datafun30("alcdays", "Alkohol"),
    datafun30("CIG30USE", "Zigarette"),
    datafun30("COCUS30A", "Kokain"),
    datafun30("HER30USE", "Heroin")
  )
)

tobacco30 <- as.data.frame(
  rbind(
    datafun30("CIG30USE", "Zigarette"),
    datafun30("SMKLSS30N", "Rauchfreier Tabak"),
    everdatafun("PIPE30DY", "Pfeife"),
    datafun30("CGR30USE", "Zigarre")
  )
)

fourdrugsdependency <- as.data.frame(
  rbind(
    everdatafun("depndalc", "Alkohol"),
    everdatafun("ndssdnsp", "Zigarette"),
    everdatafun("depndcoc", "Kokain"),
    everdatafun("depndher", "Heroin")
  )
)

########################################################################################################################

timeline_fun <- function(table, label_vec, colors, limit, shapes) {
  my_plot(
    table %>%
    ggplot(aes(
      x = year,
      y = count,
      color = factor(drug, levels = label_vec),
      shape = factor(drug, levels = label_vec)
    )) +
    geom_point(size = 3) +
    geom_line(linewidth = 1) +
    labs(
      color = "Droge",
      shape = "Droge",
      x = "Jahr",
      y = "Prozent"
    ) +
    theme(
      legend.title = element_text(size = 20),
      legend.text =  element_text(size = 20)
    ) +
    scale_color_manual(values = colors) +
    scale_shape_manual(values = shapes)
  )  +
    scale_y_continuous(limits = c(0, limit), labels = scales::percent_format())
}

########################################################################################################################

timeline_fun(fourdrugsever, labels_drugs, colors_drugs, 0.85, shapes_drugs)
timeline_fun(fourdrugs30, labels_drugs, colors_drugs, 0.85, shapes_drugs)
timeline_fun(fourdrugsdependency, labels_drugs, colors_drugs, 0.08, shapes_drugs)

timeline_fun(tobaccoever, labels_tobacco, colors_tobacco, 0.6, shapes_tobacco)
timeline_fun(tobacco30, labels_tobacco, colors_tobacco, 0.6, shapes_tobacco)

########################################################################################################################

histogram_fun <- function(datacol, drug_name, limit, colorcode, yearplot) {
  
  data <-
    drugdata %>%
    group_by(year) %>%
    count(day = .data[[datacol]]) %>%
    mutate(`Relative share` = n / sum(n)) %>%
    filter(
      day >= 1 &
        day <= 30
    ) %>%
    filter(year == yearplot) %>%
    ungroup() %>%
    mutate(
      Drug = drug_name,
      day  = factor(day, levels = as.character(1:30))
    )

  my_plot(
    ggplot(data, aes(x = day, y = `Relative share`)) +
    geom_col(fill = colorcode, color = "black") +
    labs(
      x =
        paste0(
        "Anzahl der Konsumtage in den letzten 30 Tagen (n = ",
        sum(data$n),
        ")"
      ),
      y = "Prozent"
    ) +
    scale_x_discrete(breaks = c("1", "5", "10", "15", "20", "25", "30"), drop = FALSE)
  ) +
  scale_y_continuous(limits = c(0, limit), labels = scales::percent_format())
}

########################################################################################################################

Histo_Alk_15 <- histogram_fun("alcdays", "Alcohol", 0.085, "#0072B2", "2015")

Histo_Alk_16 <- histogram_fun("alcdays", "Alcohol", 0.085, "#0072B2", "2016")

Histo_Alk_17 <- histogram_fun("alcdays", "Alcohol", 0.085, "#0072B2", "2017")

Histo_Alk_18 <- histogram_fun("alcdays", "Alcohol", 0.085, "#0072B2", "2018")

Histo_Alk_19 <- histogram_fun("alcdays", "Alcohol", 0.085, "#0072B2", "2019")

Histo_Zig_15 <- histogram_fun("CIG30USE", "Cigarettes", 0.12, "#009E73", "2015")

Histo_Zig_16 <- histogram_fun("CIG30USE", "Cigarettes", 0.12, "#009E73", "2016")

Histo_Zig_17 <- histogram_fun("CIG30USE", "Cigarettes", 0.12, "#009E73", "2017")

Histo_Zig_18 <- histogram_fun("CIG30USE", "Cigarettes", 0.12, "#009E73", "2018")

Histo_Zig_19 <- histogram_fun("CIG30USE", "Cigarettes", 0.12, "#009E73", "2019")

Histo_Koks_15 <- histogram_fun("COCUS30A", "Cocaine", 0.004, "#E69F00","2015")

Histo_Koks_16 <- histogram_fun("COCUS30A", "Cocaine", 0.004, "#E69F00","2016")

Histo_Koks_17 <- histogram_fun("COCUS30A", "Cocaine", 0.004, "#E69F00","2017")

Histo_Koks_18 <- histogram_fun("COCUS30A", "Cocaine", 0.004, "#E69F00","2018")

Histo_Koks_19 <- histogram_fun("COCUS30A", "Cocaine", 0.004, "#E69F00","2019")

Histo_Her_15 <- histogram_fun("HER30USE", "Heroin", 0.0006, "#CC79A7", "2015")

Histo_Her_16 <- histogram_fun("HER30USE", "Heroin", 0.0006, "#CC79A7", "2016")

Histo_Her_17 <- histogram_fun("HER30USE", "Heroin", 0.0006, "#CC79A7", "2017")

Histo_Her_18 <- histogram_fun("HER30USE", "Heroin", 0.0006, "#CC79A7", "2018")

Histo_Her_19 <- histogram_fun("HER30USE", "Heroin", 0.0006, "#CC79A7", "2019")

########################################################################################################################

nic_dependency_demo <- function(demog, label, labelvec) {
  data_sum <- data2019 %>%
    group_by(.data[[demog]]) %>%
    mutate(total = n()) %>%
    filter(ndssdnsp == 1) %>%
    summarise(count = n(), total = first(total))
  lvls <- levels(factor(data_sum[[demog]]))
  new_labels <- sapply(seq_along(lvls), function(i) {
    paste0(labelvec[i], " \n(n = ", data_sum$count[data_sum[[demog]] == lvls[i]], ")")
  })
  data_sum <- data_sum %>% mutate(count = count / total)
  my_plot(
    data_sum %>%
      ggplot(aes(
        x = factor(.data[[demog]]),
        y = count
      )) +
      geom_col(fill = "#009E73") +
      scale_x_discrete(
        name = label,
        labels = new_labels
      )
  )
}

########################################################################################################################

nic_dependency_demo("CATAG2", "Altersgruppen", catag2_vector)

nic_dependency_demo("NEWRACE2", "Race", newrace2_vector)

nic_dependency_demo("irsex", "Geschlecht", irsex_vector)

########################################################################################################################

drug_dep_color <- c(
  "Alkohol" = "#0072B2",
  "Kokain" = "#E69F00",
  "Heroin" = "#CC79A7",
  "Mehrfachabhängigkeit" = "grey20"
  )

########################################################################################################################

drug_dependency_demo <- function(demog, labelvec, xlabel) {
  data_filtered <- data2019 %>%
    mutate(Dependency = case_when(
      depndalc == 1 & depndcoc == 0 & depndher == 0 ~ "Alkohol",
      depndcoc == 1 & depndalc == 0 & depndher == 0 ~ "Kokain",
      depndher == 1 & depndalc == 0 & depndcoc == 0 ~ "Heroin",
      depndalc == 1 & depndcoc == 1 | depndalc == 1 & depndher == 1 | depndcoc == 1 & depndher == 1 ~ "Mehrfachabhängigkeit"
    )) %>%
    filter(!is.na(Dependency)) %>%
    mutate(Dependency = factor(Dependency, levels = c("Alkohol","Kokain","Heroin","Mehrfachabhängigkeit")))
  counts <- data_filtered %>% count(.data[[demog]])
  lvls <- levels(factor(data_filtered[[demog]]))
  new_labels <- sapply(seq_along(lvls), function(i) {
    paste0(labelvec[i]," \n(n = ", counts$n[counts[[demog]] == lvls[i]], ")")
  })
  my_plot(
    data_filtered %>%
      group_by(.data[[demog]], Dependency) %>%
      ggplot(aes(x = factor(.data[[demog]]), fill = Dependency)) +
      geom_bar(position = "fill") +
      scale_fill_manual(name = "Drogen", values = drug_dep_color) +
      scale_x_discrete(labels = new_labels) +
      labs(x = xlabel)
  )
}

########################################################################################################################

drug_dependency_demo("CATAG2", catag2_vector, "Altergruppen")

drug_dependency_demo("irsex", irsex_vector, "Geschlecht")

drug_dependency_demo("NEWRACE2", newrace2_vector, "Race")

########################################################################################################################



########################################################################################################################################

Drug.Dependency.Total <- data2019 %>%
  select(depndalc, depndcoc, depndher, MI_CAT_U) %>%
  mutate(
    Dependency = case_when(
      depndalc == 1 & depndcoc == 0 & depndher == 0 ~ "Alkohol",
      depndcoc == 1 & depndalc == 0 & depndher == 0 ~ "Kokain",
      depndher == 1 & depndalc == 0 & depndcoc == 0 ~ "Heroin",
      depndalc == 1 & depndcoc == 1 | depndalc == 1 & depndher == 1 | depndcoc == 1 & depndher == 1 ~ "Mehrfachabhängigkeit",
      TRUE ~ "Keine Abhängigkeit"
    )
  ) %>%
  filter(MI_CAT_U >= 0) %>%  # Mehrfachabhängigkeit wird jetzt nicht mehr ausgeschlossen
  group_by(Dependency, MI_CAT_U) %>%
  summarise(count = n(), .groups = "drop") 

# Plot
SubsAbhängig.Gesundheit <-ggplot(Drug.Dependency.Total, aes(x = factor(Dependency, levels = c("Keine Abhängigkeit", "Alkohol", "Kokain", "Heroin", "Mehrfachabhängigkeit")), 
                                                            fill = factor(MI_CAT_U))) +
  geom_bar(stat = "identity", position = "fill", aes(y = count)) +
  scale_x_discrete(labels = c("Keine Abhängigkeit" = "Keine \nAbhängigkeit", 
                              "Alkohol" = "Abhängigkeit \nvon Alkohol", 
                              "Kokain" = "Abhängigkeit \nvon Kokain", 
                              "Heroin" = "Abhängigkeit \nvon Heroin", 
                              "Mehrfachabhängigkeit" = "Abhängigkeit \nvon mehreren Drogen")) +
  scale_fill_manual(name = "Mentale Erkrankungen:",
                    labels = c("0" = "Keine", 
                               "1" = "Milde", 
                               "2" = "Moderate", 
                               "3" = "Schwere"),
                    values = c("grey80", "grey65", "grey45", "grey30")) + # Farben für Mental Health Kategorien
  scale_y_continuous(labels = scales::percent_format())+
  labs(x = "Kategorien", y = "Prozent") +
  theme_light() +
  theme(
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 20),
    legend.title = element_text(size = 17,5),
    legend.text = element_text(size = 17,5)
  )




############################
# 1) Daten definieren
############################
state_data <- data.frame(
  state = c(
    "California", "Texas", "Florida", "New York", "Pennsylvania",
    "Illinois", "Ohio", "Georgia", "North Carolina", "Michigan",
    "New Jersey", "Virginia", "Washington", "Arizona", "Massachusetts",
    "Tennessee", "Indiana", "Missouri", "Maryland", "Wisconsin",
    "Colorado", "Minnesota", "South Carolina", "Alabama", "Louisiana",
    "Kentucky", "Oregon", "Oklahoma", "Connecticut", "Utah", "Iowa", "Nevada", 
    "Arkansas", "Mississippi", "Kansas", "New Mexico", "Nebraska", "Idaho", 
    "West Virginia", "Hawaii", "New Hampshire", "Maine", "Montana", "Rhode Island", 
    "Delaware", "South Dakota", "North Dakota", "Alaska", "Vermont", "Wyoming"
  ),
  population = c(
    39512223, 28995881, 21477737, 19453561, 12801989,
    12671821, 11689100, 10617423, 10488084, 9986857,
    8882190, 8535519, 7614893, 7278717, 6892503,
    6829174, 6732219, 6137428, 6045680, 5822434,
    5758736, 5639632, 5148714, 4903185, 4648794,
    4467673, 4217737, 3956971, 3565287, 3205958, 3155070, 3080156,
    3017804, 2976149, 2913314, 2096829, 1934408, 1787065,
    1792147, 1415872, 1359711, 1344212, 1068778, 1059361,
    973764, 884659, 762062, 731545, 623989, 578759
  ),
  interviews = c(
    4560, 3300, 3300, 3300, 2400,
    2400, 2400, 1500, 1500, 2400,
    1500, 1500, 960, 960, 960,
    960, 960, 960, 960, 960,
    960, 960, 960, 960, 960,
    960, 960, 960, 960, 960, 960, 960,
    960, 960, 960, 960, 960, 960,
    960, 967, 960, 960, 960, 960,
    960, 960, 960, 960, 960, 960
  )
)

############################
# 2) Abkürzungen erzeugen
############################
# *Über* das Paket 'usdata':
# state2abbr("California") => "CA", etc.
state_data <- state_data %>%
  mutate(
    state_abbr = state2abbr(state),
    # Beispielmetrik: Interviews pro 100.000 Einwohner
    interviews_per_100k = (interviews / population) * 100000
  )

############################
# 3) Choroplethen-Karte zeichnen
############################
Karte.USA <- plot_usmap(
  data    = state_data,
  regions = "states",
  values  = "interviews_per_100k",
  include = state_data$state_abbr  # optional: nur die definierten Staaten
) +
  scale_fill_continuous(
    low   = "lightgrey",
    high  = "black",
    name  = "Umfragen\n(pro 100k)",
    label = scales::comma
  ) +
  theme(panel.background = element_blank(),
        
        legend.position = "right",
        legend.title = element_text(size = 20),   # Schriftgröße des Legendentitels
        legend.text  = element_text(size = 20),   # Schriftgröße der Legendenbeschriftungen
        legend.key.size = unit(1.0, "cm")         # Größe der Farbfelder
        
  )


## ODDS
# Erstellen der Kreuztabelle mit absoluten Häufigkeiten
Drug_Addprev_Crosstab <- data2019 %>%
  select(depndalc, depndcoc, depndher, addprev) %>%
  filter(addprev %in% c(1, 2)) %>%  # Nur gültige Werte behalten (Ja, Nein)
  mutate(
    Dependency = case_when(
      depndalc == 0 & depndcoc == 0 & depndher == 0 ~ "Keine Abhängigkeit",
      depndalc == 1 & depndcoc == 0 & depndher == 0 ~ "Alkohol",
      depndalc == 0 & depndcoc == 1 & depndher == 0 ~ "Kokain",
      depndalc == 0 & depndcoc == 0 & depndher == 1 ~ "Heroin",
      TRUE ~ "Mehrfache Abhängigkeit"  # Falls jemand mehrere Drogen konsumiert
    )
  ) %>%
  group_by(addprev, Dependency) %>%
  summarise(Frequency = n(), .groups = "drop") %>%
  pivot_wider(names_from = Dependency, values_from = Frequency, values_fill = 0) %>%
  mutate(addprev = recode(addprev, `1` = "Ja", `2` = "Nein")) %>%
  column_to_rownames(var = "addprev") %>%
  select("Keine Abhängigkeit", "Alkohol", "Kokain", "Heroin", "Mehrfache Abhängigkeit") 

# Kreuztabelle anzeigen
print(Drug_Addprev_Crosstab)

# Erstellen der Kreuztabelle mit absoluten Häufigkeiten
Drug_amdeyr_Crosstab <- data2019 %>%
  select(depndalc, depndcoc, depndher, amdeyr) %>%
  filter(amdeyr %in% c(1, 2)) %>%
  mutate(
    Dependency = case_when(
      depndalc == 0 & depndcoc == 0 & depndher == 0 ~ "Keine Abhängigkeit",
      depndalc == 1 & depndcoc == 0 & depndher == 0 ~ "Alkohol",
      depndalc == 0 & depndcoc == 1 & depndher == 0 ~ "Kokain",
      depndalc == 0 & depndcoc == 0 & depndher == 1 ~ "Heroin",
      TRUE ~ "Mehrfache Abhängigkeit"
    )
  ) %>%
  group_by(amdeyr, Dependency) %>%
  summarise(Frequency = n(), .groups = "drop") %>%
  pivot_wider(names_from = Dependency, values_from = Frequency, values_fill = 0) %>%
  mutate(amdeyr = recode(amdeyr, `1` = "Depression Ja", `2` = "Depression Nein")) %>%
  column_to_rownames(var = "amdeyr")

# Berechnung der Odds und Gruppengröße
Drug_Odds <- Drug_amdeyr_Crosstab %>%
  t() %>%
  as.data.frame() %>%
  mutate(Total = `Depression Ja` + `Depression Nein`,
         Odds_Yes = ifelse(`Depression Nein` > 0, `Depression Ja` / `Depression Nein`, NA)) %>%
  filter(!is.na(Odds_Yes))  # Entfernt Zeilen mit NA-Werten

# Gewichteter Durchschnitt der Odds berechnen
weighted_avg_odds <- sum(Drug_Odds$Odds_Yes * Drug_Odds$Total) / sum(Drug_Odds$Total)

# Berechnung der Odds Ratios relativ zum gewichteten Durchschnitt
Drug_Odds <- Drug_Odds %>%
  mutate(OR = Odds_Yes / weighted_avg_odds) %>%
  rownames_to_column(var = "Dependency") %>%
  mutate(Dependency = factor(Dependency, levels = c("Keine Abhängigkeit", "Alkohol", "Kokain", "Heroin", "Mehrfache Abhängigkeit")))  # Reihenfolge setzen

# Farben für die Kategorien definieren
drug_colors <- c("Keine Abhängigkeit" = "gray50",
                 "Alkohol" = "#0072B2",
                 "Kokain" = "#E69F00",
                 "Heroin" = "#CC79A7",
                 "Mehrfache Abhängigkeit" = "black")

# Plot der Odds Ratios mit Anpassungen
# Plot der Odds Ratios mit logarithmischer Skala
Odds.Abhängigkeit <- ggplot(Drug_Odds, aes(x = Dependency, y = OR, color = Dependency)) +
  geom_point(size = 5) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray60") +  # Gesamtdurchschnittslinie in Grau
  geom_text(aes(x = "Heroin", y = 1.05, label = "Gesamtdurchschnitt"), color = "gray60", vjust = -0.5, size = 5) +  
  scale_color_manual(values = drug_colors) +
  scale_y_log10(limits = c(0.5, max(Drug_Odds$OR) * 1.2), 
                breaks = c(0.1, 0.2, 0.5, 1, 2, 5, 10),  # Logarithmische Intervalle
                labels = scales::number_format(accuracy = 0.1)) +
  labs(
    x = "Abhängigkeitstyp",
    y = "Odds Ratio (log-Skala)",
    color = "Abhängigkeitstyp"
  ) +
  theme_light() +
  theme(
    axis.title = element_text(size = 22),  # Achsentitel
    axis.text  = element_text(size = 22),  # Achsbeschriftungen
    legend.position = "none"  # Legendentext
  )
