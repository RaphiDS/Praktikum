
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
