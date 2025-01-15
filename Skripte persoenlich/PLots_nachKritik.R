#1) Substanzen- Jemals

Substanz.Probiert <- data2019 %>%
  select(alcever, herever, cocever, cigever) %>%
  mutate()
  pivot_longer(cols = everything(), names_to = "Drug", values_to = "Response")%>%
  filter(Response %in% c(1,2)) %>%
  group_by(Drug, Response) %>%
  summarise(count = n()) %>%
  mutate(count = count/56136)

ggplot(Substanz.Probiert, aes(x = Drug, y = count, fill = factor(Response)))+
  geom_col(position = "dodge")

## Drug Dependency and Race
Subs.Abhängig.Ethnie <- data2019 %>%
  select(NEWRACE2, depndcoc, depndalc, depndher) %>%
  mutate(Dependency = case_when(
    depndalc == 1 & depndcoc == 0 & depndher == 0 ~ "Alkohol",
    depndcoc == 1 & depndalc == 0 & depndher == 0 ~ "Kokain",
    depndher == 1 & depndalc == 0 & depndcoc == 0 ~ "Heroin",
    depndalc == 1 & depndcoc == 1 | depndalc == 1 & depndher == 1 | depndcoc == 1 & depndher == 1 ~ "Mehrfachabhängigkeit"
  )) %>%
  filter(!is.na(Dependency)) %>%
  mutate(Dependency = factor(Dependency, levels = c("Alkohol", "Kokain", "Heroin", "Mehrfachabhängigkeit"))) %>%
  group_by(NEWRACE2, Dependency) %>%
  ggplot(aes(x = factor(NEWRACE2, levels = c(1, 7, 2, 5, 6, 3, 4)), fill = Dependency)) +
  geom_bar(position = "fill") +
  scale_x_discrete(labels = c("1" = "Weisse",
                              "2" = "Afro \nAmerikaner",
                              "3" = "Am/Ak \nIndigene",
                              "4" = "Indigene Hawaii \n/Paz. Inseln",
                              "5" = "Asiaten",
                              "6" = "Gemischt",
                              "7" = "Hispanisch")) +
  scale_fill_manual(name = "Drogen",
                    values = c("Alkohol" = "#0072B2",  # Blau
                               "Kokain" = "#E69F00",  # Gelb
                               "Heroin" = "#CC79A7",  # Rosa
                               "Mehrfachabhängigkeit" = "grey30")) +  # Grau
  labs(x = "Gruppen", y = "Anteil") +
  theme_light() +
  theme(
    axis.title = element_text(size = 20),  
    axis.text  = element_text(size = 20),  
    legend.text = element_text(size = 17.5),
    legend.title = element_text(size = 17.5)
  )

Subs.Abhängig.Ethnie
