
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
  filter(MI_CAT_U >= 0) %>%
  mutate(FillGroup = paste(Dependency, MI_CAT_U, sep = "_")) %>%  # Sichere Labels für Farben
  group_by(Dependency, MI_CAT_U, FillGroup) %>%
  summarise(count = n(), .groups = "drop") 

# Graue Farbskala für Füllung
gray_palette <- c(
  "0" = "#E5E5E5",  # Hellgrau für keine mentale Erkrankung
  "1" = "#BFBFBF",  # Mittleres Grau für milde mentale Erkrankung
  "2" = "#808080",  # Dunkleres Grau für moderate mentale Erkrankung
  "3" = "#404040"   # Sehr dunkles Grau für schwere mentale Erkrankung
)

# Farbzuordnung für die Umrandungen der Balken nach Substanzabhängigkeit
outline_colors <- c(
  "Keine Abhängigkeit" = "#000000",  # Schwarz für keine Abhängigkeit
  "Alkohol" = "#0072B2",  # Blau für Alkohol
  "Kokain" = "#E69F00",  # Orange für Kokain
  "Heroin" = "#CC79A7",  # Rosa für Heroin
  "Mehrfachabhängigkeit" = "#4B0082"  # Dunkelviolett für Mehrfachabhängigkeit
)

# Plot mit grauer Füllung und farbigen Outlines
ggplot(Drug.Dependency.Total, aes(x = factor(Dependency, levels = c("Keine Abhängigkeit", "Alkohol", "Kokain", "Heroin", "Mehrfachabhängigkeit")), 
                                  fill = factor(MI_CAT_U), color = Dependency)) +
  geom_bar(stat = "identity", position = "fill", aes(y = count), size = 1.2) +  # Größere Outline für bessere Sichtbarkeit
  scale_x_discrete(labels = c("Keine Abhängigkeit" = "Keine \nAbhängigkeit", 
                              "Alkohol" = "Abhängigkeit \nvon Alkohol", 
                              "Kokain" = "Abhängigkeit \nvon Kokain", 
                              "Heroin" = "Abhängigkeit \nvon Heroin", 
                              "Mehrfachabhängigkeit" = "Abhängigkeit \nvon mehreren Drogen")) +
  scale_fill_manual(name = "Mentale Erkrankungen",
                    values = gray_palette,
                    labels = c("0" = "Keine Mentalen Gesundheitsprobleme", 
                               "1" = "Milde Mentale Erkrankung", 
                               "2" = "Moderate Mentale Erkrankung", 
                               "3" = "Ernste Mentale Erkrankung")) +
  scale_color_manual(name = "Substanzabhängigkeit", values = outline_colors) +
  labs(title = "Substanzabhängigkeit und Mentale Erkrankungen", x = "Kategorien", y = "Anteil") +
  theme_light() +
  theme(
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 15),
    legend.position = "bottom"
  )

library(ggplot2)
library(dplyr)
library(tidyr)

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
  filter(MI_CAT_U >= 0) %>%
  mutate(FillGroup = paste(Dependency, MI_CAT_U, sep = "_")) %>%  # Sichere Labels für Farben
  group_by(Dependency, MI_CAT_U, FillGroup) %>%
  summarise(count = n(), .groups = "drop") 

# Benutzerdefinierte Farbpalette
color_palette <- c(
  "Keine Abhängigkeit_0" = "#D3D3D3", "Keine Abhängigkeit_1" = "#B0B0B0", "Keine Abhängigkeit_2" = "#808080", "Keine Abhängigkeit_3" = "#505050",
  "Alkohol_0" = "#B3D7FF", "Alkohol_1" = "#66B2FF", "Alkohol_2" = "#1F78B4", "Alkohol_3" = "#004C99",
  "Kokain_0" = "#FFD699", "Kokain_1" = "#FFB347", "Kokain_2" = "#E69F00", "Kokain_3" = "#B36B00",
  "Heroin_0" = "#FFCCE5", "Heroin_1" = "#FF99CC", "Heroin_2" = "#CC79A7", "Heroin_3" = "#993366"
)

# Plot mit sequentiellen Farben
ggplot(Drug.Dependency.Total, aes(x = factor(Dependency, levels = c("Keine Abhängigkeit", "Alkohol", "Kokain", "Heroin", "Mehrfachabhängigkeit")), 
                                  fill = FillGroup)) +
  geom_bar(stat = "identity", position = "fill", aes(y = count)) +
  scale_x_discrete(labels = c("Keine Abhängigkeit" = "Keine \nAbhängigkeit", 
                              "Alkohol" = "Abhängigkeit \nvon Alkohol", 
                              "Kokain" = "Abhängigkeit \nvon Kokain", 
                              "Heroin" = "Abhängigkeit \nvon Heroin", 
                              "Mehrfachabhängigkeit" = "Abhängigkeit \nvon mehreren Drogen")) +
  scale_fill_manual(name = "Mentale Erkrankungen",
                    values = color_palette) +
  labs(title = "Substanzabhängigkeit und Mentale Erkrankungen", x = "Kategorien", y = "Anteil") +
  theme_light() +
  theme(
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 15),
    legend.position = "bottom"
  )




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
  scale_fill_manual(name = "Mentale Erkrankungen",
                    labels = c("0" = "Keine Mentalen Gesundheitsprobleme", 
                               "1" = "Milde Mentale Erkrankung", 
                               "2" = "Moderate Mentale Erkrankung", 
                               "3" = "Ernste Mentale Erkrankung"),
                    values = c("lightgray", "gray", "darkgrey", "azure4")) + # Farben für Mental Health Kategorien
  labs(title = "Substanzabhängigkeit und Mentale Erkrankungen", x = "Kategorien", y = "Anteil") +
  theme_light() +
  theme(
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 15),
    legend.position = "bottom"
  )



library(ggplot2)
library(dplyr)
library(tidyr)

# Datenaufbereitung für "Mentale Erkrankung vs. Substanzabhängigkeit"
Drug.Dependency.Total <- data2019 %>%
  select(depndalc, depndcoc, depndher, MI_CAT_U) %>%
  mutate(
    Dependency = case_when(
      depndalc == 1 & depndcoc == 0 & depndher == 0 ~ "Alkohol",
      depndcoc == 1 & depndalc == 0 & depndher == 0 ~ "Kokain",
      depndher == 1 & depndalc == 0 & depndcoc == 0 ~ "Heroin",
      depndalc == 0 & depndcoc == 0 & depndher == 0 ~ "Keine Abhängigkeit",
      TRUE ~ "Mehrfache Abhängigkeit"  # Falls jemand mehrere Drogen konsumiert
    )
  ) %>%
  filter(MI_CAT_U >= 0, Dependency != "Mehrfache Abhängigkeit") %>%  # Mehrfachabhängigkeit vorerst ausgeschlossen
  group_by(MI_CAT_U, Dependency) %>%
  summarise(count = n(), .groups = "drop") 

# Plot
ggplot(Drug.Dependency.Total, aes(x = factor(MI_CAT_U, levels = c("0", "1", "2", "3")), 
                                  fill = factor(Dependency, levels = c("Keine Abhängigkeit", "Alkohol", "Kokain", "Heroin")))) +
  geom_bar(stat = "identity", position = "fill", aes(y = count)) +
  scale_x_discrete(labels = c("0" = "Keine Mentalen \nGesundheitsprobleme", 
                              "1" = "Milde Mentale \nErkrankung", 
                              "2" = "Moderate Mentale \nErkrankung", 
                              "3" = "Ernste Mentale \nErkrankung")) +
  scale_fill_manual(name = "Substanzabhängigkeit",
                    labels = c("Keine Abhängigkeit" = "Keine Abhängigkeit",
                               "Alkohol" = "Alkohol",
                               "Kokain" = "Kokain",
                               "Heroin" = "Heroin"),
                    values = c("Keine Abhängigkeit" = "lightgray",  # Blau für keine Abhängigkeit
                               "Alkohol" = "#0072B2",  # Blau für Alkohol (wie vorher)
                               "Kokain" = "#E69F00",  # Orange für Kokain
                               "Heroin" = "#CC79A7")) +  # Rosa für Heroin
  labs(title = "Mentale Erkrankungen und Substanzabhängigkeit", x = "Mentale Erkrankung", y = "Anteil") +
  theme_light() +
  theme(
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 15),
    legend.position = "bottom"
  )



# Datenaufbereitung für "Mentale Erkrankung vs. Substanzabhängigkeit"
Drug.Dependency.Total <- data2019 %>%
  select(depndalc, depndcoc, depndher, MI_CAT_U) %>%
  mutate(
    Dependency = case_when(
      depndalc == 1 & depndcoc == 0 & depndher == 0 ~ "Alkohol",
      depndcoc == 1 & depndalc == 0 & depndher == 0 ~ "Kokain",
      depndher == 1 & depndalc == 0 & depndcoc == 0 ~ "Heroin",
      TRUE ~ "Mehrfache Abhängigkeit"  # Falls jemand mehrere Drogen konsumiert
    )
  ) %>%
  filter(MI_CAT_U >= 0, Dependency != "Mehrfache Abhängigkeit") %>%  # Mehrfachabhängigkeit vorerst ausgeschlossen
  group_by(MI_CAT_U, Dependency) %>%
  summarise(count = n(), .groups = "drop") 

# Plot
ggplot(Drug.Dependency.Total, aes(x = factor(MI_CAT_U, levels = c("0", "1", "2", "3")), 
                                  fill = factor(Dependency, levels = c("Alkohol", "Kokain", "Heroin")))) +
  geom_bar(stat = "identity", position ="stack", aes(y = count)) +
  scale_x_discrete(labels = c("0" = "Keine Mentalen \nGesundheitsprobleme", 
                              "1" = "Milde Mentale \nErkrankung", 
                              "2" = "Moderate Mentale \nErkrankung", 
                              "3" = "Ernste Mentale \nErkrankung")) +
  scale_fill_manual(name = "Substanzabhängigkeit",
                    labels = c("Keine Abhängigkeit" = "Keine Abhängigkeit",
                               "Alkohol" = "Alkohol",
                               "Kokain" = "Kokain",
                               "Heroin" = "Heroin"),
                    values = c(
                               "Alkohol" = "#0072B2",  # Blau für Alkohol (wie vorher)
                               "Kokain" = "#E69F00",  # Orange für Kokain
                               "Heroin" = "#CC79A7")) +  # Rosa für Heroin
  labs(title = "Mentale Erkrankungen und Substanzabhängigkeit", x = "Mentale Erkrankung", y = "Anteil") +
  theme_light() +
  theme(
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 15),
    legend.position = "bottom"
  )
