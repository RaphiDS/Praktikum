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
  scale_x_discrete(labels = c("1" = "Weisse",
                              "2" = "Schwarze \nAfroamerikaner",
                              "3" = "Am/Ak Indigene",
                              "4" = "Indigene Hawaii \n/Paz. Inseln",
                              "5" = "Asiaten",
                              "6" = "Gemischt",
                              "7" = "Hispanisch")) +
  labs(y = "Anteil", x = "Ethnische Zugehörigkeit") +
  theme_light() +
  theme(
    axis.title.x = element_text(margin = margin(t = 20)),
    axis.title = element_text(size = 20),  # Achsentitel
    axis.text  = element_text(size = 20),  # Achsbeschriftungen
    legend.position = "none"  # Legendentext
  )
ggsave("Presentation_files/Pres_plots/Ethnie_Verteilung_plot.png",
       plot = Race.Distribution, width = 18, height = 10, dpi = 300)
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
    y = "Anteil"
  ) +
  theme(
    axis.title = element_text(size = 20),  # Achsentitel
    axis.text  = element_text(size = 20),  # Achsbeschriftungen
    legend.title = element_text(size = 20),  # Legendentitel
    legend.text =  element_text(size = 20),  # Legendentext
  ) + scale_y_continuous(limits = c(0, NA)) +
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
    y = "Anteil"
  ) +
  theme(
    axis.title = element_text(size = 20),  # Achsentitel
    axis.text  = element_text(size = 20),  # Achsbeschriftungen
    legend.title = element_text(size = 20),  # Legendentitel
    legend.text =  element_text(size = 20),  # Legendentext
  )  +
  scale_y_continuous(limits = c(0, NA)) +
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
    y = "Anteil"
  ) +
  theme(
    axis.title = element_text(size = 20),
    axis.text  = element_text(size = 20),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20)
  ) +
  scale_y_continuous(limits = c(0, NA)) +
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
    y = "Anteil"
  ) +
  theme(
    axis.title = element_text(size = 20),
    axis.text  = element_text(size = 20),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20)
  ) +
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
  
  ggplot(data, aes(x = day, y = `Relative share`)) +
    geom_col(fill = colorcode, color = "black") +
    theme_light() +
    labs(
      x = paste("Anzahl der Konsumtage in den letzten 30 Tagen"),
      y = "Anteil"
    ) +
    theme(
      axis.title = element_text(size = 20),
      axis.text  = element_text(size = 20),
      legend.title = element_text(size = 20),
      legend.text = element_text(size = 20)
    ) +
    scale_x_discrete(breaks = c("1", "5", "10", "15", "20", "25", "30"), drop = FALSE) +
    scale_y_continuous(limits = c(0, limit), labels = scales::label_number())
}


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
  select(catage, ndssdnsp) %>%
  group_by(catage) %>%
  mutate(total = n()) %>%  # Gesamtanzahl pro catage berechnen
  filter(ndssdnsp == 1) %>%
  summarise(count = n(), total = first(total))  %>%# count berechnen und total beibehalten
  mutate(count = count / total)%>%
  ggplot(aes(x = factor(catage), y = count))+
  geom_col(fill = "#009E73")+
  scale_x_discrete(name = "Gruppierung",labels = c("12-17", "18-25", "26-34", "35+"))+
  labs( x = " ", y = "Anteil")+
  theme_light() +
  theme(
    axis.title = element_text(size = 20),  # Achsentitel
    axis.text  = element_text(size = 20),  # Achsbeschriftungen
    legend.text = element_text(size = 17,5),
    legend.title = element_text(size = 17,5),
    legend.position = "none"  # Legendentext
  )
ggsave("Presentation_files/Pres_plots/NikAbhängig_Alter.png", 
       plot = Nik.Abhängig.Alter, width = 15, height = 8, dpi = 300)
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
  labs(x = "Ethnie", y = "Anteil")+
  theme_light() +
  theme(
    axis.title = element_text(size = 20),  # Achsentitel
    axis.text  = element_text(size = 20),  # Achsbeschriftungen
    legend.text = element_text(size = 17,5),
    legend.title = element_text(size = 17,5),
    legend.position = "none"  # Legendentext
  )

ggsave("Presentation_files/Pres_plots/NikAbhängig_Ethnie.png", 
       plot = Nik.Abhängig.Ethnie, width = 15, height = 8, dpi = 300)
#-------------------------------------------------------------------------------
## Drug Dependency by age group
Subs.Abhängig.Alter <- data2019 %>%
  select(catage,depndcoc,depndher, depndalc) %>%
  pivot_longer(cols = c(depndcoc, depndher, depndalc), names_to = "Drug", values_to = "Usage") %>%
  filter (Usage == 1)%>%
  ggplot(aes(x = factor(catage), fill = factor(Drug)))+
  geom_bar(position = "fill")+
  scale_fill_manual(name = "Drogen",labels = c("depndalc" = "Alkohol","depndcoc" = "Kokain", "depndher" = "Heroin"),
                    values = c("#0072B2","#E69F00", "#CC79A7")) +
  scale_x_discrete(labels = c("1" = "12-17", "2" = "18-25", "3" = "26-34", "4" = "35+"))+
  labs(x = "Gruppierung", y = "Anteil")+
  theme_light() +
  theme(
    axis.title = element_text(size = 20),  # Achsentitel
    axis.text  = element_text(size = 25),  # Achsbeschriftungen
    legend.text = element_text(size = 17,5),
    legend.title = element_text(size = 17,5),
    legend.position = "bottom"  # Legendentext
  )
ggsave("Presentation_files/Pres_plots/SubsAbhängig_Alter.png", 
       plot = Subs.Abhängig.Alter, width = 15, height = 8, dpi = 300)
#------------------------------------------------------------------------------
## Drug Dependency and Race
Subs.Abhängig.Ethnie <- data2019 %>%
  select(NEWRACE2, depndcoc, depndalc, depndher) %>%
  pivot_longer(cols = c(depndcoc,depndher, depndalc), names_to = "Drug", values_to = "Usage") %>%
  filter(Usage == 1)%>%
  ggplot(aes(x = factor(NEWRACE2, levels = c(1, 7, 2, 5, 6, 3, 4)), fill = factor(Drug)))+
  geom_bar(position = "fill")+
  scale_x_discrete(labels = c("1" = "Weisse",
                              "2" = "Afro \nAmerikaner",
                              "3" = "Am/Ak \nIndigene",
                              "4" = "Indigene Hawaii \n/Paz. Inseln",
                              "5" = "Asiaten",
                              "6" = "Gemischt",
                              "7" = "Hispanisch")) +
  scale_fill_manual(name = "Drogen",
                    labels = c("depndalc" = "Alkohol","depndcoc" = "Kokain", "depndher" = "Heroin"),
                    values = c("#0072B2","#E69F00", "#CC79A7"))+
  labs(x = "Gruppen", y = "Anteil")+
  theme_light() +
  theme(
    axis.title = element_text(size = 20),  # Achsentitel
    axis.text  = element_text(size = 20),  # Achsbeschriftungen
    legend.text = element_text(size = 17,5),
    legend.title = element_text(size = 17,5),
    legend.position = "bottom"  # Legendentext
  )

ggsave("Presentation_files/Pres_plots/SubsAbhängig_Ethnie.png", 
       plot = Subs.Abhängig.Ethnie, width = 15, height = 8, dpi = 300)
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
  labs(x = "Kategorien", y = "Anteil") +
  theme_light() +
  theme(
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 20),
    legend.title = element_text(size = 17,5),
    legend.text = element_text(size = 17,5),
    legend.position = "bottom"
  )
ggsave("Presentation_files/Pres_plots/SubsAbhängig_Gesundheit.png", plot = SubsAbhängig.Gesundheit, width = 18, height = 10, dpi = 300)
