
gender <- data2019 %>%
  select(irsex) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "gender") %>%
  group_by(gender) %>%
  summarize(count = n()/56136) %>%
ggplot(aes(x = factor(gender, labels = c("1" = "Männer", "2"= "Frauen")), y = count))+
  geom_col()+
  scale_y_continuous(labels = scales::percent)+
  labs(y = "Prozent", fill = "Geschlecht", x = "") +
  theme_light() +
  theme(
    axis.title = element_text(size = 20),  # Achsentitel
    axis.text  = element_text(size = 20),  # Achsbeschriftungen
    legend.position = "none"  # Legendentex
  )

#ggsave("Presentation_files/Pres_plots/Verteilung_Geschlecht.png", plot = gender, width = 18, height = 10, dpi = 300)


Drogen.Einkommen <- data2019 %>%
  select(depndalc, depndcoc,depndher, income) %>%
  mutate(Dependency = case_when(
    depndalc == 1 & depndcoc == 0 & depndher == 0 ~ "Alkohol",
    depndcoc == 1 & depndalc == 0 & depndher == 0 ~ "Kokain",
    depndher == 1 & depndalc == 0 & depndcoc == 0 ~ "Heroin",
    depndalc == 1 & depndcoc == 1 | depndalc == 1 & depndher == 1 | depndcoc == 1 & depndher == 1 ~ "Mehrfachabhängigkeit"
  )) %>%
  filter(!is.na(Dependency)) %>%
  mutate(Dependency = factor(Dependency, levels = c("Alkohol", "Kokain", "Heroin", "Mehrfachabhängigkeit"))) %>%
  group_by(income, Dependency) %>%
  ggplot(aes(x = factor(income), fill = factor(Dependency)))+
  geom_bar(position = "fill")+
  geom_vline(xintercept = 3, linetype = "dotted", color = "black", size = 1 )+
  scale_fill_manual(
    name = "Drogen",
    values = c(
      "Alkohol" = "#0072B2",            # Blau
      "Kokain" = "#E69F00",             # Gelb
      "Heroin" = "#CC79A7",             # Rosa
      "Mehrfachabhängigkeit" = "grey30" # Grau
    ))+
  scale_x_discrete(labels = c("1" = "Weniger als 20.000", "2" = "20.000 - 49.999", "3" = "50.000 - 74.999", "4" = "75.000+"))+
  labs(x = "Familieneinkommen in $", y = "Anteil")+
  theme_light() +
  theme(
    axis.title = element_text(size = 20),  # Achsentitel
    axis.text  = element_text(size = 20),  # Achsbeschriftungen
    legend.title = element_text(size = 17,5),
    legend.text = element_text(size = 17,5),
    legend.position = "bottom"
  )

#ggsave("Presentation_files/Pres_plots/SubsAbhängig_Einkommen.png", plot = Drogen.Einkommen, width = 15, height = 8, dpi = 300)


### Mental health gender
Drug.Dependency.Gender <- data2019 %>%
  select(depndalc, depndcoc, depndher, MI_CAT_U, irsex) %>%
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
  group_by(Dependency,MI_CAT_U, irsex) %>%
ggplot(aes( x = factor(Dependency, 
                                               levels =  c("Keine Abhängigkeit", "Alkohol", "Kokain", "Heroin", "Mehrfachabhängigkeit"),
                                               labels = c("Keine Abhängigkeit", "Alkohol", "Kokain", "Heroin", "Mehrfachabhängigkeit")),  # Reihenfolge setzen
                                    fill = factor( MI_CAT_U)))+
  geom_bar(position = "fill")+
  theme_light() +
  facet_wrap(~ irsex, labeller = labeller(irsex = c("1" = "Mann", "2" = "Frauen")))+
  scale_fill_manual(name = "Mentale Erkrankungen",
                    labels = c("0" = "Keine", 
                               "1" = "Milde", 
                               "2" = "Moderate", 
                               "3" = "Schwere"),
                    values = c("grey80", "grey65", "grey45", "grey30")) +
  labs(x = "Abängigkeit", y = "Prozent") +
  scale_y_continuous(labels = scales::percent_format())+
  theme(
    axis.title = element_text(size = 20),  # Achsentitel
    axis.text  = element_text(size = 20),  # Achsbeschriftungen
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20),
    legend.position = "bottom"
  )

#ggsave("Presentation_files/Pres_plots/MI_Geschlecht.png", plot = Drug.Dependency.Gender, width = 18, height = 12, dpi = 300)
