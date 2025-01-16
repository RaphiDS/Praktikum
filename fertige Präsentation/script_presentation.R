library(tidyverse)

load("Daten bearbeitet/combi_redu_data.Rdata")
drugdata <- allfilterdata

data2019 <- allfilterdata %>%
  filter(year == 2019)

Race.Distribution <- data2019 %>%
  select(NEWRACE2) %>%
  pivot_longer(cols = everything(), names_to = "Var", values_to = "Answer") %>%
  group_by(Answer) %>%
  summarize(count = n()) %>%
  mutate(count = count/56136) %>%
ggplot(aes(x = factor(Answer,
                              levels = c(1, 7, 2, 5, 6, 3, 4)),
                              y = count))+
  geom_col()+
  scale_x_discrete(labels = c("1" = "Weiße",
                              "2" = "Schwarze\nAfroamerikaner",
                              "3" = "Am/Ak\nIndigene",
                              "4" = "Indigene Hawaii\n/Paz. Inseln",
                              "5" = "Asiaten",
                              "6" = "Gemischte",
                              "7" = "Hispanische")) +
  labs(y = "Prozent", x = "Race", title = "") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_light() +
  theme(
    axis.title.x = element_text(margin = margin(t = 20)),
    axis.title = element_text(size = 30),  # Achsentitel
    axis.text  = element_text(size = 30),  # Achsbeschriftungen
    legend.position = "none"  # Legendentext
  )
ggsave("Presentation_files/Pres_plots/Ethnie_Verteilung_plot.png",
       plot = Race.Distribution, width = 18, height = 11, dpi = 300)

## ALtersverteilung
Altersverteilung <- data2019 %>%
  select(CATAG2) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "Group") %>%
  group_by(Group) %>%
  summarize(count =n()/56136)%>%
  ggplot(aes(x= factor(Group),y = count)) +
  geom_col() +
  labs(x = "Altersgruppen", y = "Prozent") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_light() +
  theme(
    axis.title = element_text(size = 30),  # Achsentitel
    axis.text  = element_text(size = 30),  # Achsbeschriftungen
    axis.title.x = element_text(margin = margin(t = 20)),
    axis.title.y = element_text(margin = margin(r = 20))
  ) +
  scale_x_discrete(labels = c("12-17", "18-25", "26+"))

ggsave ("Presentation_files/Pres_plots/Altersverteilung.png", plot = Altersverteilung, width = 18, height = 10, dpi = 300)
##### Drugs Denis Raphael

# 1) Function: Generates a summarized table for a specified (drug) variable,
#    focusing only on "Yes" answers (value == 1), referring to "ever used".
everdatafun <- function(datacol, drug) {
  drugdata %>%
    group_by(year) %>%
    count(.data[[datacol]]) %>%                # Counts how often each value appears in 'datacol' per year
    mutate("Anteil" = n / sum(n)) %>%       # Relative share per year
    filter(.data[[datacol]] == 1) %>%           # Keep only "Yes" answers
    ungroup() %>%
    mutate(
      Drug = drug,                              # Name of the drug in a new column
      Year = year
    ) %>%
    select(Year, "Anteil", Drug)            # Select columns in a consistent order
}
# 2) Function: Generates a summarized table for a specified (drug) variable,
#    focusing on consumption "in the last 30 days".
#    Values 1–30 are treated as "Yes," everything else as "No."
datafun30 <- function(datacol, drug) {
  drugdata %>%
    group_by(year) %>%
    summarise(
      TotalPeople = n(),                                         # Total number of observations
      Users = sum(.data[[datacol]] >= 1 & .data[[datacol]] <= 30, na.rm = TRUE),
      "Anteil" = Users / TotalPeople
    ) %>%
    mutate(Drug = drug) %>%                                      # Drug name in a new column
    select(Year = year, "Anteil", Drug)                      # Rename and reorder columns
}

# 1) "Ever used" data for 4 major drugs
fourdrugsever <- as.data.frame(
  rbind(
    everdatafun("alcever", "Alkohol"),
    everdatafun("cigever", "Zigarette"),
    everdatafun("cocever", "Kokain"),
    everdatafun("herever", "Heroin")
  )
)
# 2) "Ever used" for various tobacco products
tobaccoever <- as.data.frame(
  rbind(
    everdatafun("cigever", "Zigarette"),
    everdatafun("smklssevr", "Rauchfreier Tabak"),
    everdatafun("pipever", "Pfeife"),
    everdatafun("cigarevr", "Zigarre")
  )
)
# 3) "In the last 30 days" for 4 major drugs
fourdrugs30 <- as.data.frame(
  rbind(
    datafun30("alcdays", "Alkohol"),
    datafun30("CIG30USE", "Zigarette"),
    datafun30("COCUS30A", "Kokain"),
    datafun30("HER30USE", "Heroin")
  )
)
# 4) "In the last 30 days" for various tobacco products
tobacco30 <- as.data.frame(
  rbind(
    datafun30("CIG30USE", "Zigarette"),
    datafun30("SMKLSS30N", "Rauchfreier Tabak"),
    everdatafun("PIPE30DY", "Pfeife"),
    datafun30("CGR30USE", "Zigarre")
  )
)
 #5) Dependency

fourdrugsdependency <- as.data.frame(
  rbind(
    everdatafun("depndalc", "Alkohol"),
    everdatafun("ndssdnsp", "Zigarette"),
    everdatafun("depndcoc", "Kokain"),
    everdatafun("depndher", "Heroin")
  )
)

##########################
# Plot Creation using ggplot2
##########################

# 1) Plot: "Ever used" – 4 major drugs
Substanzen.Verlauf <- ggplot(fourdrugsever, aes(x = Year, y = .data[["Anteil"]],
                          color = factor(Drug, levels = c("Alkohol", "Zigarette", "Kokain", "Heroin")),
                          shape = factor(Drug, levels = c("Alkohol", "Zigarette", "Kokain", "Heroin")))) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  theme_light() +
  labs(
    color = "Droge",
    shape = "Droge",
    x = "Jahr",
    y = "Prozent"
  ) +
  theme(
    axis.title = element_text(size = 20),  # Achsentitel
    axis.text  = element_text(size = 20),  # Achsbeschriftungen
    legend.title = element_text(size = 20),  # Legendentitel
    legend.text =  element_text(size = 20),  # Legendentext
  ) + scale_y_continuous(limits = c(0, 0.85), labels = scales::percent_format()) +
  scale_color_manual(values = c("#0072B2", "#009E73", "#E69F00", "#CC79A7")) +
  scale_shape_manual(values = c(15:18))# beliebige Form-Codes


ggsave("Presentation_files/Pres_plots/Substanzen_Verlauf_plot.png",
       plot = Substanzen.Verlauf, width = 15, height = 8, dpi = 300)

# 2) Plot: "In the last 30 days" – 4 major drugs
Monatskonsum.Verlauf <- ggplot(fourdrugs30, aes(x = Year, y = .data[["Anteil"]],
                        color = factor(Drug, levels = c("Alkohol", "Zigarette", "Kokain", "Heroin")),
                        shape = factor(Drug, levels = c("Alkohol", "Zigarette", "Kokain", "Heroin")))) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  theme_light() +
  labs(
    color = "Droge",
    shape = "Droge",
    x = "Jahr",
    y = "Prozent"
  ) +
  theme(
    axis.title = element_text(size = 20),  # Achsentitel
    axis.text  = element_text(size = 20),  # Achsbeschriftungen
    legend.title = element_text(size = 20),  # Legendentitel
    legend.text =  element_text(size = 20),  # Legendentext
  )  +
  scale_y_continuous(limits = c(0, 0.85), labels = scales::percent_format()) +
  scale_color_manual(values = c("#0072B2", "#009E73", "#E69F00", "#CC79A7")) +
  scale_shape_manual(values = c(15:18))  # beliebige Form-Codes

ggsave("Presentation_files/Pres_plots/Monatskonsum_Verlauf_plot.png",
  plot = Monatskonsum.Verlauf, width = 15, height = 8, dpi = 300)

# 3) Plot: "Have ever used tobacco products"
Jemals.Tabbak <- ggplot(tobaccoever, aes(x = Year, y = .data[["Anteil"]],
                        color = factor(Drug, levels = c("Zigarette", "Zigarre", "Rauchfreier Tabak", "Pfeife")),
                        shape = factor(Drug, levels = c("Zigarette", "Zigarre", "Rauchfreier Tabak", "Pfeife")))) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  theme_light() +
  labs(
    fill = "Droge",
    color = "Droge",
    shape = "Droge",
    x = "Jahr",
    y = "Prozent"
  ) +
  theme(
    axis.title = element_text(size = 20),
    axis.text  = element_text(size = 20),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20)
  ) +
  scale_y_continuous(limits = c(0, 0.6), labels = scales::percent_format()) +
  scale_color_manual(values = c("#009E73", "red","lightblue", "darkgrey")) +
  scale_shape_manual(
    values = c(16, 15, 17, 18)
  )

ggsave("Presentation_files/Pres_plots/Jemals_Tabbak_plot.png",
  plot = Jemals.Tabbak, width = 15, height = 8, dpi = 300)

# 4) Plot: "In the last 30 days" – tobacco products
Monatskonsum.Tabbak <- ggplot(tobacco30, aes(x = Year, y = .data[["Anteil"]],
                      color = factor(Drug, levels = c("Zigarette", "Zigarre", "Rauchfreier Tabak", "Pfeife")),
                      shape = factor(Drug, levels = c("Zigarette", "Zigarre", "Rauchfreier Tabak", "Pfeife")))) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  theme_light() +
  labs(
    color = "Droge",
    shape = "Droge",
    x = "Jahr",
    y = "Prozent"
  ) +
  theme(
    axis.title = element_text(size = 20),
    axis.text  = element_text(size = 20),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20)
  ) +
  scale_y_continuous(limits = c(0, 0.6), labels = scales::percent_format()) +
  scale_color_manual(values = c("#009E73", "red","lightblue", "darkgrey")) +
  scale_shape_manual(
    values = c(16, 15, 17, 18)
  )

ggsave ("Presentation_files/Pres_plots/Monatskonsum_Tabbak_plot.png",
  plot = Monatskonsum.Tabbak, width = 15, height = 8, dpi = 300)

## Histogram Function
histogram_fun <- function(datacol, drug_name, limit, colorcode, yearplot) {
  data <- drugdata %>%
    group_by(year) %>%
    count(day = .data[[datacol]]) %>%
    mutate(`Relative share` = n / sum(n)) %>%
    filter(day >= 1 & day <= 30) %>%
    filter(year == yearplot) %>%
    ungroup() %>%
    mutate(
      Drug = drug_name,
      # Hier explizit alle Levels 1:30 setzen:
      day  = factor(day, levels = as.character(1:30))
    )
  
  # Berechnung der Stichprobengröße
  sample_size <- sum(data$n)
  
  ggplot(data, aes(x = day, y = `Relative share`)) +
    geom_col(fill = colorcode, color = "black") +
    theme_light() +
    labs(
      x = paste0("Anzahl der Konsumtage in den letzten 30 Tagen (n = ", sample_size, ")"),  # Stichprobengröße hinzufügen
      y = "Prozent"
    ) +
    theme(
      axis.title.x = element_text(margin = margin(t = 20)),
      axis.title.y = element_text(margin = margin(r = 20)),
      axis.title = element_text(size = 25),
      axis.text  = element_text(size = 25),
      legend.title = element_text(size = 25),
      legend.text = element_text(size = 25)
    ) +
    scale_x_discrete(breaks = c("1", "5", "10", "15", "20", "25", "30"), drop = FALSE) +
    scale_y_continuous(limits = c(0, limit), labels = scales::percent_format())
}

# 5) Abhängigkeit
Substanzen.Verlauf.Abh <- ggplot(fourdrugsdependency, aes(x = Year, y = .data[["Anteil"]],
                                                          color = factor(Drug, levels = c("Alkohol", "Zigarette", "Kokain", "Heroin")),
                                                          shape = factor(Drug, levels = c("Alkohol", "Zigarette", "Kokain", "Heroin")))) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  theme_light() +
  labs(
    color = "Droge",
    shape = "Droge",
    x = "Jahr",
    y = "Prozent"
  ) +
  theme(
    axis.title = element_text(size = 20),  # Achsentitel
    axis.text  = element_text(size = 20),  # Achsbeschriftungen
    legend.title = element_text(size = 20),  # Legendentitel
    legend.text =  element_text(size = 20),  # Legendentext
  ) + scale_y_continuous(limits = c(0, 0.08), labels = scales::percent_format()) +
  scale_color_manual(values = c("#0072B2", "#009E73", "#E69F00", "#CC79A7")) +
  scale_shape_manual(values = c(15:18))# beliebige Form-Codes


ggsave("Presentation_files/Pres_plots/Substanzen_Verlauf_Abhängigkeit_plot.png",
       plot = Substanzen.Verlauf.Abh, width = 15, height = 8, dpi = 300)


# Example calls
Histo_Alk_15 <- histogram_fun("alcdays", "Alcohol", 0.085, "#0072B2", "2015")
ggsave("Presentation_files/Pres_plots/Alk_15_plot.png",
       plot = Histo_Alk_15, width = 15, height = 7, dpi = 300)

Histo_Alk_16 <- histogram_fun("alcdays", "Alcohol", 0.085, "#0072B2", "2016")
ggsave("Presentation_files/Pres_plots/Alk_16_plot.png",
        plot = Histo_Alk_16, width = 15, height = 7, dpi = 300)

Histo_Alk_17 <- histogram_fun("alcdays", "Alcohol", 0.085, "#0072B2", "2017")
ggsave("Presentation_files/Pres_plots/Alk_17_plot.png",
       plot = Histo_Alk_17, width = 15, height = 7, dpi = 300)

Histo_Alk_18 <- histogram_fun("alcdays", "Alcohol", 0.085, "#0072B2", "2018")
ggsave("Presentation_files/Pres_plots/Alk_18_plot.png",
       plot = Histo_Alk_18, width = 15, height = 7, dpi = 300)

Histo_Alk_19 <- histogram_fun("alcdays", "Alcohol", 0.085, "#0072B2", "2019")
ggsave("Presentation_files/Pres_plots/Alk_19_plot.png",
       plot = Histo_Alk_19, width = 15, height = 7, dpi = 300)

# Zigaretten
Histo_Zig_15 <- histogram_fun("CIG30USE", "Cigarettes", 0.12, "#009E73", "2015")
ggsave("Presentation_files/Pres_plots/Zig_15_plot.png",
       plot = Histo_Zig_15, width = 15, height = 7, dpi = 300)

Histo_Zig_16 <- histogram_fun("CIG30USE", "Cigarettes", 0.12, "#009E73", "2016")
ggsave ("Presentation_files/Pres_plots/Zig_16_plot.png",
        plot = Histo_Zig_16, width = 15, height = 7, dpi = 300)

Histo_Zig_17 <- histogram_fun("CIG30USE", "Cigarettes", 0.12, "#009E73", "2017")
ggsave("Presentation_files/Pres_plots/Zig_17_plot.png",
       plot = Histo_Zig_17, width = 15, height = 7, dpi = 300)

Histo_Zig_18 <- histogram_fun("CIG30USE", "Cigarettes", 0.12, "#009E73", "2018")
ggsave("Presentation_files/Pres_plots/Zig_18_plot.png",
       plot = Histo_Zig_18, width = 15, height = 7, dpi = 300)

Histo_Zig_19 <- histogram_fun("CIG30USE", "Cigarettes", 0.12, "#009E73", "2019")
ggsave("Presentation_files/Pres_plots/Zig_19_plot.png",
       plot = Histo_Zig_19, width = 15, height = 7, dpi = 300)

# Kokain
Histo_Koks_15 <- histogram_fun("COCUS30A", "Cocaine", 0.004, "#E69F00","2015")
ggsave("Presentation_files/Pres_plots/Koks_15_plot.png",
       plot = Histo_Koks_15, width = 15, height = 7, dpi = 300)

Histo_Koks_16 <- histogram_fun("COCUS30A", "Cocaine", 0.004, "#E69F00","2016")
ggsave("Presentation_files/Pres_plots/Koks_16_plot.png",
       plot = Histo_Koks_16, width = 15, height = 7, dpi = 300)

Histo_Koks_17 <- histogram_fun("COCUS30A", "Cocaine", 0.004, "#E69F00","2017")
ggsave("Presentation_files/Pres_plots/Koks_17_plot.png",
       plot = Histo_Koks_17, width = 15, height = 7, dpi = 300)

Histo_Koks_18 <- histogram_fun("COCUS30A", "Cocaine", 0.004, "#E69F00","2018")
ggsave("Presentation_files/Pres_plots/Koks_18_plot.png",
       plot = Histo_Koks_18, width = 15, height = 7, dpi = 300)

Histo_Koks_19 <- histogram_fun("COCUS30A", "Cocaine", 0.004, "#E69F00","2019")
ggsave("Presentation_files/Pres_plots/Koks_19_plot.png",
       plot = Histo_Koks_19, width = 15, height = 7, dpi = 300)

##Heroin
Histo_Her_15 <- histogram_fun("HER30USE", "Heroin", 0.0006, "#CC79A7", "2015")
ggsave("Presentation_files/Pres_plots/Her_15_plot.png",
       plot = Histo_Her_15, width = 15, height = 7, dpi = 300)

Histo_Her_16 <- histogram_fun("HER30USE", "Heroin", 0.0006, "#CC79A7", "2016")
ggsave("Presentation_files/Pres_plots/Her_16_plot.png",
  plot = Histo_Her_16, width = 15, height = 7, dpi = 300)

Histo_Her_17 <- histogram_fun("HER30USE", "Heroin", 0.0006, "#CC79A7", "2017")
ggsave("Presentation_files/Pres_plots/Her_17_plot.png",
       plot = Histo_Her_17, width = 15, height = 7, dpi = 300)

Histo_Her_18 <- histogram_fun("HER30USE", "Heroin", 0.0006, "#CC79A7", "2018")
ggsave("Presentation_files/Pres_plots/Her_18_plot.png",
       plot = Histo_Her_18, width = 15, height = 7, dpi = 300)

Histo_Her_19 <- histogram_fun("HER30USE", "Heroin", 0.0006, "#CC79A7", "2019")
ggsave("Presentation_files/Pres_plots/Her_19_plot.png",
       plot = Histo_Her_19, width = 15, height = 7, dpi = 300)
###################################
#Demographics and Drugs / Nicotine
###################################

#-------------------------------------------------------------------------------
## Nicotine Dependency Age
Nik.Abhängig.Alter <- data2019 %>%
  select(CATAG2, ndssdnsp) %>%
  group_by(CATAG2) %>%
  mutate(total = n()) %>%  # Gesamtanzahl pro catage berechnen
  filter(ndssdnsp == 1) %>%
  summarise(count = n(), total = first(total))  %>%# count berechnen und total beibehalten
  mutate(count = count / total)%>%
  ggplot(aes(x = factor(CATAG2), y = count))+
  geom_col(fill = "#009E73")+
  scale_x_discrete(name = "Altersgruppen",labels = c("12-17", "18-25", "26+"))+
  scale_y_continuous(labels = scales::percent_format())+
  labs( x = " ", y = "Prozent")+
  theme_light() +
  theme(
    axis.title = element_text(size = 24),  # Achsentitel
    axis.text  = element_text(size = 24),  # Achsbeschriftungen
    legend.text = element_text(size = 17,5),
    legend.title = element_text(size = 17,5),
    legend.position = "none"  # Legendentext
  )
ggsave("Presentation_files/Pres_plots/NikAbhängig_Alter.png", 
       plot = Nik.Abhängig.Alter, width = 18, height = 9, dpi = 300)
#-------------------------------------------------------------------------------
## Nikotin Dependency (last month) and Race
Nik.Abhängig.Ethnie <- data2019 %>%
  select(NEWRACE2, ndssdnsp) %>%
  group_by(NEWRACE2) %>%
  mutate(total = n()) %>%
  filter (ndssdnsp == 1)%>%
  summarise(count = n(), total = first(total))  %>%# count berechnen und total beibehalten
  mutate(count = count /total)%>%
  ggplot(aes(x = factor(NEWRACE2,
                                               levels = c(1, 7, 2, 5, 6, 3, 4)),
                                    y = count))+
  geom_col(fill = "#009E73") +
  scale_x_discrete(labels = c("1" = "Weisse",
                              "2" = "Afro- \nAmerikaner",
                              "3" = "Am/Ak \nIndigene",
                              "4" = "Indigene \nHawaii \n/Paz. Inseln",
                              "5" = "Asiaten",
                              "6" = "Gemischt",
                              "7" = "Hispanisch"))+
  scale_y_continuous(labels = scales::percent_format())+
  labs(x = "Race", y = "Prozent")+
  theme_light() +
  theme(
    axis.title = element_text(size = 22),  # Achsentitel
    axis.text  = element_text(size = 22),  # Achsbeschriftungen
    legend.text = element_text(size = 17,5),
    legend.title = element_text(size = 17,5),
    legend.position = "none"  # Legendentext
  )

ggsave("Presentation_files/Pres_plots/NikAbhängig_Ethnie.png", 
       plot = Nik.Abhängig.Ethnie, width = 15, height = 8, dpi = 300)

## Nikotine und Geschlecht

Nik.Abhängig.Geschlecht <- data2019 %>%
  select(irsex, ndssdnsp) %>%
  group_by(irsex) %>%
  mutate(total = n()) %>%
  filter (ndssdnsp == 1)%>%
  summarise(count = n(), total = first(total))  %>%# count berechnen und total beibehalten
  mutate(count = count /total)%>%
  ggplot(aes(x = factor(irsex), y = count))+
  geom_col(fill = "#009E73")+
  scale_x_discrete(labels = c("1"="Männer", "2" = "Frauen"))+
  scale_y_continuous(labels = scales::percent_format())+
  labs(x = "Geschlecht", y = "Prozent")+
  theme_light() +
  theme(
    axis.title = element_text(size = 20),  # Achsentitel
    axis.text  = element_text(size = 20),  # Achsbeschriftungen
    legend.text = element_text(size = 17,5),
    legend.title = element_text(size = 17,5),
    legend.position = "none"  # Legendentext
  )

ggsave("Presentation_files/Pres_plots/NikAbhängig_Geschlecht.png",
       plot = Nik.Abhängig.Geschlecht, width = 15, height = 8, dpi = 300)
#-------------------------------------------------------------------------------
## Drug Dependency by age group
Subs.Abhängig.Alter <- data2019 %>%
  select(CATAG2,depndcoc,depndher, depndalc) %>%
  mutate(Dependency = case_when(
    depndalc == 1 & depndcoc == 0 & depndher == 0 ~ "Alkohol",
    depndcoc == 1 & depndalc == 0 & depndher == 0 ~ "Kokain",
    depndher == 1 & depndalc == 0 & depndcoc == 0 ~ "Heroin",
    depndalc == 1 & depndcoc == 1 | depndalc == 1 & depndher == 1 | depndcoc == 1 & depndher == 1 ~ "Mehrfachabhängigkeit"
  )) %>%
  filter(!is.na(Dependency)) %>%
  mutate(Dependency = factor(Dependency, levels = c("Alkohol", "Kokain", "Heroin", "Mehrfachabhängigkeit"))) %>%
  group_by(CATAG2, Dependency) %>%
  ggplot(aes(x = factor(CATAG2), fill = Dependency))+
  geom_bar(position = "dodge")+
  scale_fill_manual(name = "Drogen",
                    values = c("Alkohol" = "#0072B2",  # Blau
                               "Kokain" = "#E69F00",  # Gelb
                               "Heroin" = "#CC79A7",  # Rosa
                               "Mehrfachabhängigkeit" = "grey30")) +  # Grau
  scale_x_discrete(labels = c("1" = "12-17", "2" = "18-25", "3" = "26+"))+
  scale_y_continuous(labels = scales::percent_format())+
  labs(x = "Gruppierung", y = "Prozent")+
  theme_light() +
  theme(
    axis.title.x = element_text(margin = margin(t = 20)),
    axis.title = element_text(size = 20),  # Achsentitel
    axis.text  = element_text(size = 25),  # Achsbeschriftungen
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 20),
    legend.position = "bottom"
  )

ggsave("Presentation_files/Pres_plots/SubsAbhängig_Alter.png", 
       plot = Subs.Abhängig.Alter, width = 18, height = 9, dpi = 300)
Subs.Abhängig.Alter
#------------------------------------------------------------------------------
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
  ggplot(aes(x = factor(NEWRACE2, levels = c(1, 7, 2, 5, 6, 3, 4)), fill = Dependency)) +
  geom_bar(position = "fill") +
  scale_x_discrete(
    labels = function(x) {
      labels <- c(
        "1" = "Weisse",
        "2" = "Afro \nAmerikaner",
        "3" = "Am/Ak \nIndigene",
        "4" = "Indigene Hawaii \n/Paz. Inseln",
        "5" = "Asiaten",
        "6" = "Gemischt",
        "7" = "Hispanisch"
      )
      sample_sizes <- sapply(as.numeric(x), function(group) {
        sum(data2019$NEWRACE2 == group &
              (data2019$depndalc == 1 | data2019$depndcoc == 1 | data2019$depndher == 1),
            na.rm = TRUE)
      })
      mapply(function(label, n) paste0(label, "\n(n = ", n, ")"), labels[x], sample_sizes)
    }
  ) +
  scale_fill_manual(name = "Drogen",
                    values = c("Alkohol" = "#0072B2",  # Blau
                               "Kokain" = "#E69F00",  # Gelb
                               "Heroin" = "#CC79A7",  # Rosa
                               "Mehrfachabhängigkeit" = "grey30")) +  # Grau
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "Gruppen", y = "Prozent") +
  theme_light() +
  theme(
    axis.title = element_text(size = 20),  
    axis.text = element_text(size = 22),  
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 18),
    legend.position = "bottom"
  )


ggsave("Presentation_files/Pres_plots/SubsAbhängig_Ethnie.png", 
       plot = Subs.Abhängig.Ethnie, width = 16, height = 8, dpi = 300)

## Drug Dependency based on gender

SubsAbhängig.Geschlecht <- data2019 %>%
  select(irsex, depndcoc, depndalc, depndher) %>%
  mutate(Dependency = case_when(
    depndalc == 1 & depndcoc == 0 & depndher == 0 ~ "Alkohol",
    depndcoc == 1 & depndalc == 0 & depndher == 0 ~ "Kokain",
    depndher == 1 & depndalc == 0 & depndcoc == 0 ~ "Heroin",
    depndalc == 1 & depndcoc == 1 | depndalc == 1 & depndher == 1 | depndcoc == 1 & depndher == 1 ~ "Mehrfachabhängigkeit"
  )) %>%
  filter(!is.na(Dependency)) %>%
  mutate(Dependency = factor(Dependency, levels = c("Alkohol", "Kokain", "Heroin", "Mehrfachabhängigkeit"))) %>%
  group_by(irsex) %>%
  mutate(SampleSize = n()) %>%  # Berechnung der Stichprobengröße nach Geschlecht
  ungroup() %>%
  ggplot(aes(x = factor(irsex), fill = Dependency)) +
  geom_bar(position = "fill") +
  scale_x_discrete(
    labels = function(x) {
      ifelse(x == "1",
             paste0("Männer\n(n = ", sum(data2019$irsex == 1, na.rm = TRUE), ")"),
             paste0("Frauen\n(n = ", sum(data2019$irsex == 2, na.rm = TRUE), ")"))
    }
  ) +
  scale_fill_manual(name = "Drogen",
                    values = c("Alkohol" = "#0072B2",  # Blau
                               "Kokain" = "#E69F00",  # Gelb
                               "Heroin" = "#CC79A7",  # Rosa
                               "Mehrfachabhängigkeit" = "grey30")) +  # Grau
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "Geschlecht", y = "Prozent") +
  theme_light() +
  theme(
    axis.title = element_text(size = 20),  # Achsentitel
    axis.text = element_text(size = 20),   # Achsbeschriftungen
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 18),
    legend.position = "bottom"
  )


ggsave("Presentation_files/Pres_plots/SubsAbhängigkeit_Geschlecht.png", plot = SubsAbhängig.Geschlecht, width = 15, height = 8, dpi = 300)
########################################################################################################################################

## Mentale Gesundheit fertig
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
ggsave("Presentation_files/Pres_plots/SubsAbhängig_Gesundheit.png", plot = SubsAbhängig.Gesundheit, width = 18, height = 10, dpi = 300)


install.packages("usmap")
install.packages("usdata")
library(usmap)
library(usdata)


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

ggsave("Presentation_files/Pres_plots/Karte_Verteilung.png", plot = Karte.USA, width = 18, height = 10, dpi = 300)

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


ggsave("Presentation_files/Pres_plots/Odds_Abhängigkeit.png", plot = Odds.Abhängigkeit, width = 15, height = 10, dpi = 300)


