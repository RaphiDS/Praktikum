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
  mutate(count = count/56136)

ggplot(Race.Distribution, aes(x = factor(Answer,
                              levels = c(1, 7, 2, 5, 6, 3, 4)),
                              y = count, fill = factor(Answer)))+
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
    axis.title = element_text(size = 15),  # Achsentitel
    axis.text  = element_text(size = 15),  # Achsbeschriftungen
    legend.position = "none"  # Legendentext
  )

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
ggplot(fourdrugsever, aes(x = Year, y = .data[["Anteil"]],
                          color = factor(Drug, levels = c("Alkohol", "Zigarette", "Kokain", "Heroin")),
                          shape = factor(Drug, levels = c("Alkohol", "Zigarette", "Kokain", "Heroin")))) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  theme_light() +
  labs(
    color = "Droge",
    shape = "Droge",
    title = "Relative share of people who have ever used certain drugs",
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

# 2) Plot: "In the last 30 days" – 4 major drugs
ggplot(fourdrugs30, aes(x = Year, y = .data[["Anteil"]],
                        color = factor(Drug, levels = c("Alkohol", "Zigarette", "Kokain", "Heroin")),
                        shape = factor(Drug, levels = c("Alkohol", "Zigarette", "Kokain", "Heroin")))) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  theme_light() +
  labs(
    title = "Relative share of people who have used certain drugs in the last 30 days",
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

# 3) Plot: "Have ever used tobacco products"
ggplot(tobaccoever, aes(x = Year, y = .data[["Anteil"]],
                        color = factor(Drug, levels = c("Zigarette", "Zigarre", "Rauchfreier Tabak", "Pfeife")),
                        shape = factor(Drug, levels = c("Zigarette", "Zigarre", "Rauchfreier Tabak", "Pfeife")))) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  theme_light() +
  labs(
    fill = "Droge",
    color = "Droge",
    shape = "Droge",
    title = "Relative share of people who have ever used certain forms of tobacco",
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

# 4) Plot: "In the last 30 days" – tobacco products
ggplot(tobacco30, aes(x = Year, y = .data[["Anteil"]],
                      color = factor(Drug, levels = c("Zigarette", "Zigarre", "Rauchfreier Tabak", "Pfeife")),
                      shape = factor(Drug, levels = c("Zigarette", "Zigarre", "Rauchfreier Tabak", "Pfeife")))) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  theme_light() +
  labs(
    color = "Droge",
    shape = "Droge",
    title = "Relative share of people who have used certain forms of tobacco in the last 30 days",
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
      title = paste0("Distribution of usage days for ", drug_name),
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
    scale_y_continuous(limits = c(0, limit))
}


# Example calls
histogram_fun("alcdays", "Alcohol", 0.085, "#0072B2", "2015")
histogram_fun("alcdays", "Alcohol", 0.085, "#0072B2", "2016")
histogram_fun("alcdays", "Alcohol", 0.085, "#0072B2", "2017")
histogram_fun("alcdays", "Alcohol", 0.085, "#0072B2", "2018")
histogram_fun("alcdays", "Alcohol", 0.085, "#0072B2", "2019")

histogram_fun("CIG30USE", "Cigarettes", 0.12, "#009E73", "2015")
histogram_fun("CIG30USE", "Cigarettes", 0.12, "#009E73", "2016")
histogram_fun("CIG30USE", "Cigarettes", 0.12, "#009E73", "2017")
histogram_fun("CIG30USE", "Cigarettes", 0.12, "#009E73", "2018")
histogram_fun("CIG30USE", "Cigarettes", 0.12, "#009E73", "2019")

histogram_fun("COCUS30A", "Cocaine", 0.004, "#E69F00","2015")
histogram_fun("COCUS30A", "Cocaine", 0.004, "#E69F00","2016")
histogram_fun("COCUS30A", "Cocaine", 0.004, "#E69F00","2017")
histogram_fun("COCUS30A", "Cocaine", 0.004, "#E69F00","2018")
histogram_fun("COCUS30A", "Cocaine", 0.004, "#E69F00","2019")


histogram_fun("HER30USE", "Heroin", 0.0006, "#CC79A7", "2015")
histogram_fun("HER30USE", "Heroin", 0.0006, "#CC79A7", "2016")
histogram_fun("HER30USE", "Heroin", 0.0006, "#CC79A7", "2017")
histogram_fun("HER30USE", "Heroin", 0.0006, "#CC79A7", "2018")
histogram_fun("HER30USE", "Heroin", 0.0006, "#CC79A7", "2019")

###################################
#Demographics and Drugs / Nicotine
###################################
#-------------------------------------------------------------------------------
## Nicotine Dependency Gender
Nic.Dependency.Gender <- data2019 %>%
  select(ndssdnsp,irsex) %>%
  filter(ndssdnsp == 1) %>%
  group_by(irsex) %>%
  summarise(count = n()) %>%
  mutate (count = count/56136)

ggplot(Nic.Dependency.Gender, aes(x = factor(irsex), y = count))+
  geom_col()+
  scale_x_discrete(labels = c("1" = "Männer", "2" = "Frauen"))+
  labs(x = "Geschlecht", y = "Anteil")+
  theme_light() +
  theme(
    axis.title = element_text(size = 20),  # Achsentitel
    axis.text  = element_text(size = 20),  # Achsbeschriftungen
    legend.position = "none"  # Legendentext
  )
#-------------------------------------------------------------------------------
## Nicotine Dependency Age
Nic.Dependency.Age <- data2019 %>%
  select(catage, ndssdnsp) %>%
  filter(ndssdnsp == 1) %>%
  group_by(catage) %>%
  summarise(count = n()) %>%
  mutate(count = count/56136)

ggplot(Nic.Dependency.Age, aes(x = factor(catage), y = count))+
  geom_col()+
  scale_x_discrete(name = "Gruppierung",labels = c("12-17", "18-25", "26-34", "35+"))+
  labs( x = " ", y = "Anteil")+
  theme_light() +
  theme(
    axis.title = element_text(size = 20),  # Achsentitel
    axis.text  = element_text(size = 20),  # Achsbeschriftungen
    legend.position = "none"  # Legendentext
  )
#-------------------------------------------------------------------------------
## Nikotin Dependency (last month) and Race
Nikotin.Dependence.Race <- data2019 %>%
  select(NEWRACE2, ndssdnsp) %>%
  filter (ndssdnsp == 1)%>%
  group_by(NEWRACE2) %>%
  summarise(count = n()) %>%
  mutate(count = count /56136)

ggplot(Nikotin.Dependence.Race, aes(x = factor(NEWRACE2), y = count))+
  geom_col()+
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
    axis.title = element_text(size = 15),  # Achsentitel
    axis.text  = element_text(size = 15),  # Achsbeschriftungen
    legend.position = "none"  # Legendentext
  )
#-------------------------------------------------------------------------------
##Generel Drug Dependency / Abuse
Drug.Dependency.Abuse <- data2019 %>%
  select(alcyr, cocyr, heryr, depndalc, depndcoc, depndher, abusealc, abusecoc,abuseher) %>%
  mutate(ID = row_number()) %>%  # Add an ID column for pivoting
  pivot_longer(cols = -ID, names_to = "Variable", values_to = "Value") %>%
  mutate(
    Substance = case_when(
      str_detect(Variable, "alc") ~ "Alkohol",
      str_detect(Variable, "coc") ~ "Cokain",
      str_detect(Variable, "her") ~ "Heroin"
    ),
    Condition = case_when(
      str_detect(Variable, "depnd") ~ "Dependent",
      str_detect(Variable, "abuse") ~ "Abuse",
      str_detect(Variable, "yr") ~ "Use"
    )
  ) %>%
  filter(Value == 1)  # Keep only cases where the flag is 1 (indicating presence)

# Create stacked bar plot
ggplot(Drug.Dependency.Abuse, aes(x = Substance, fill = Condition)) +
  geom_bar(position = "fill") +
  labs(title = "Substanzkonsum, Abhängigkeit und Missbrauch im letzten Jahr",
       x = "Substanz")+
  theme_light() +
  theme(
    axis.title = element_text(size = 20),  # Achsentitel
    axis.text  = element_text(size = 15),  # Achsbeschriftungen
    legend.position = "none"  # Legendentext
  )

## Drug Dependency based on gender
Drug.Dependency.Gender <-data2019 %>%
  select(irsex, depndcoc, depndalc, depndher) %>%
  pivot_longer(cols = c(depndcoc,depndher, depndalc), names_to = "Drug", values_to = "Usage") %>%
  filter(Usage == 1)

ggplot(Drug.Dependency.Gender, aes(x = factor(Drug), fill = factor(irsex)))+
  geom_bar(position = "fill")+
  #geom_line(y = 0.5)+
  labs(title = "Drug Dependency by Gender")+
  scale_x_discrete(labels = c("depndalc" = "Alkohol", "depndcoc" = "Cokain", "depndher" = "Heroin"))+
  scale_fill_manual(labels = c("1" = "Männer", "2" = "Frauen"), values =c("1" = "darkblue", "2" = "maroon"))+
  labs(title = "Abhängigkeit von Männern und Frauen", x = "Substanz")+
  theme_light() +
  theme(
    axis.title = element_text(size = 15),  # Achsentitel
    axis.text  = element_text(size = 15),  # Achsbeschriftungen
  )

#-------------------------------------------------------------------------------
## Drug Dependency by age group
Drug.Dependency.Age <- data2019 %>%
  select(catage,depndcoc,depndher, depndalc) %>%
  pivot_longer(cols = c(depndcoc, depndher, depndalc), names_to = "Drug", values_to = "Usage") %>%
  filter (Usage == 1)

ggplot(Drug.Dependency.Age, aes(x = factor(catage), fill = factor(Drug)))+
  geom_bar(position = "fill")+
  scale_fill_discrete(name = "Drogen",labels = c("depndalc" = "Alkhol","depndcoc" = "Cokain", "depndher" = "Heroin"))+
  scale_x_discrete(labels = c("1" = "12-17", "2" = "18-25", "3" = "26-34", "4" = "35+"))+
  labs(title = "Abhängigkeit der Altersgruppen", x = "Gruppierung")+
  theme_light() +
  theme(
    axis.title = element_text(size = 20),  # Achsentitel
    axis.text  = element_text(size = 15),  # Achsbeschriftungen
    legend.position = "bottom"  # Legendentext
  )
#------------------------------------------------------------------------------
## Drug Dependency and Race
Dependent.Users.Race <- data2019 %>%
  select(NEWRACE2, depndcoc, depndalc, depndher) %>%
  pivot_longer(cols = c(depndcoc,depndher, depndalc), names_to = "Drug", values_to = "Usage") %>%
  filter(Usage == 1)

ggplot(Dependent.Users.Race , aes(x = factor(NEWRACE2), fill = factor(Drug)))+
  geom_bar(position = "fill")+
  scale_x_discrete(labels = c("1" = "Weisse",
                              "2" = "Afro \nAmerikaner",
                              "3" = "Am/Ak \nIndigene",
                              "4" = "Indigene Hawaii \n/Paz. Inseln",
                              "5" = "Asiaten",
                              "6" = "Gemischt",
                              "7" = "Hispanisch")) +
  scale_fill_discrete(name = "Drogen",labels = c("depndalc" = "Alkhol","depndcoc" = "Cokain", "depndher" = "Heroin"))+
  labs(title = "Abhängigkeit der Ethnien", x = "Gruppen")+
  theme_light() +
  theme(
    axis.title = element_text(size = 15),  # Achsentitel
    axis.text  = element_text(size = 15),  # Achsbeschriftungen
    legend.position = "bottom"  # Legendentext
  )