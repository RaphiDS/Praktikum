
# Datenaufbereitung für "Keine Abhängigkeit" vs. spezifische Substanzabhängigkeit & Mehrfachabhängigkeit
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
ggplot(Drug.Dependency.Total, aes(x = factor(Dependency, levels = c("Keine Abhängigkeit", "Alkohol", "Kokain", "Heroin", "Mehrfachabhängigkeit")), 
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
  labs(title = "Substanzabhängigkeit und Mentale Erkrankungen", x = "Kategorien", y = "Anteil") +
  theme_light() +
  theme(
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 15),
    legend.position = "bottom"
  )


library(dplyr)
library(tidyr)

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
ggplot(Drug_Odds, aes(x = Dependency, y = OR, color = Dependency)) +
  geom_point(size = 5) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray60") +  # Gesamtdurchschnittslinie in Grau
  geom_text(aes(x = "Heroin", y = 1.05, label = "Gesamtdurchschnitt"), color = "gray60", vjust = -0.5, size = 5) +  
  scale_color_manual(values = drug_colors) +
  scale_y_continuous(limits = c(0.5, max(Drug_Odds$OR) * 1.2), breaks = seq(0.5, max(Drug_Odds$OR) * 1.2, by = 0.5)) +
  labs(title = "Odds Ratios für amdeyr nach Abhängigkeitstyp (vs. gewichteter Durchschnitt)",
       x = "Abhängigkeitstyp",
       y = "Odds Ratio",
       color = "Abhängigkeitstyp") +
  theme_light() +
  theme(
    axis.title = element_text(size = 15),  # Achsentitel
    axis.text  = element_text(size = 15),  # Achsbeschriftungen
    legend.position = "none"  # Legendentext
  )