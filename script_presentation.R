library(tidyverse)

load("Daten bearbeitet/combi_redu_data.Rdata")
drugdata <- allfilterdata

data2019 <- allfilterdata %>%
  filter(year == 2019)

#################
### Demographics
#################
# Age Groups
age.grouped <- data2019 %>%
  select(catage) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "Group") %>%
  group_by(Group) %>%
  summarize(count =n()/56136) %>%
ggplot(aes(x= factor(Group),y = count, fill = factor(Group, labels = c("12-17", "18-25", "26-34", "35+")))) +
  geom_col() +
  labs(y = "Anteil", fill = "Age groups", x = "Altersgruppen") +
  theme_light() +
  theme(
    axis.title = element_text(size = 15),  # Achsentitel
    axis.text  = element_text(size = 15),  # Achsbeschriftungen
    legend.position = "none"  # Legendentext
  ) +
  scale_x_discrete(labels = c("12-17", "18-25", "26-34", "35+"))

age.grouped
#ggsave(filename = "age_grouped_plot.png",path = "" plot = age.grouped, width = 8, height = 6, dpi = 300)

#Race Destribution
Race.Destr <- data2019 %>%
  select(NEWRACE2) %>%
  pivot_longer(cols = everything(), names_to = "Var", values_to = "Answer") %>%
  group_by(Answer) %>%
  summarize(count = n()) %>%
  mutate(count = count/56136)%>%
ggplot(aes(x = factor(Answer), y = count))+
  geom_col()+
  scale_x_discrete(labels = c("1" = "Weisse",
                              "2" = "Afro- \nAmerikaner",
                              "3" = "Am/Ak \nIndigene",
                              "4" = "Indigene \nHawaii \n/Paz. Inseln",
                              "5" = "Asiaten",
                              "6" = "Gemischt",
                              "7" = "Hispanisch")) +
  labs(y = "Anteil", x = "Ethnische Zugehörigkeit") +
  theme_light() +
  theme(
    axis.title.x = element_text(margin = margin(t =25)),
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
    mutate("Rel. share" = n / sum(n)) %>%       # Relative share per year
    filter(.data[[datacol]] == 1) %>%           # Keep only "Yes" answers
    ungroup() %>%
    mutate(
      Drug = drug,                              # Name of the drug in a new column
      Year = year
    ) %>%
    select(Year, "Rel. share", Drug)            # Select columns in a consistent order
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
      "Rel. share" = Users / TotalPeople
    ) %>%
    mutate(Drug = drug) %>%                                      # Drug name in a new column
    select(Year = year, "Rel. share", Drug)                      # Rename and reorder columns
}


# 1) "Ever used" data for 4 major drugs
fourdrugsever <- as.data.frame(
  rbind(
    everdatafun("alcever", "Alcohol"),
    everdatafun("cigever", "Cigarettes"),
    everdatafun("cocever", "Cocaine"),
    everdatafun("herever", "Heroin")
  )
)

# 2) "Ever used" for various tobacco products
tobaccoever <- as.data.frame(
  rbind(
    everdatafun("cigever", "Cigarettes"),
    everdatafun("smklssevr", "Smokeless Tobacco"),
    everdatafun("pipever", "Pipe"),
    everdatafun("cigarevr", "Cigar")
  )
)

# 3) "In the last 30 days" for 4 major drugs
fourdrugs30 <- as.data.frame(
  rbind(
    datafun30("alcdays", "Alcohol"),
    datafun30("CIG30USE", "Cigarettes"),
    datafun30("COCUS30A", "Cocaine"),
    datafun30("HER30USE", "Heroin")
  )
)

# 4) "In the last 30 days" for various tobacco products
tobacco30 <- as.data.frame(
  rbind(
    datafun30("CIG30USE", "Cigarettes"),
    datafun30("SMKLSS30N", "Smokeless Tobacco"),
    # For pipes, the question seems to differ, so "ever used" is being used here
    everdatafun("PIPE30DY", "Pipe"),
    datafun30("CGR30USE", "Cigar")
  )
)

##########################
# Plot Creation using ggplot2
##########################

# 1) Plot: "Ever used" – 4 major drugs
ggplot(fourdrugsever, aes(x = Year, y = .data[["Rel. share"]], color = Drug, shape = Drug)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  theme_light() +
  labs(
    title = "Relative share of people who have ever used certain drugs",
    x = "Jahr",
    y = "Rel. Anteil"
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
ggplot(fourdrugs30, aes(x = Year, y = .data[["Rel. share"]], color = Drug, shape = Drug)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  theme_light() +
  labs(
    title = "Relative share of people who have used certain drugs in the last 30 days",
    x = "Jahr",
    y = "Rel. Anteil"
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
ggplot(tobaccoever, aes(x = Year, y = .data[["Rel. share"]], color = Drug, shape = Drug)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  theme_light() +
  labs(
    title = "Relative share of people who have ever used certain forms of tobacco",
    x = "Year",
    y = "Relative Share"
  ) +
  theme(
    axis.title = element_text(size = 20),
    axis.text  = element_text(size = 20),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20)
  ) +
  scale_y_continuous(limits = c(0, NA)) +
  scale_color_manual(values = c("#0072B2", "#009E73", "#E69F00", "#CC79A7")) +
  scale_shape_manual(
    values = c(15, 16, 17, 18)
  )

# 4) Plot: "In the last 30 days" – tobacco products
ggplot(tobacco30, aes(x = Year, y = .data[["Rel. share"]], color = Drug, shape = Drug)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  theme_light() +
  labs(
    title = "Relative share of people who have used certain forms of tobacco in the last 30 days",
    x = "Year",
    y = "Relative Share"
  ) +
  theme(
    axis.title = element_text(size = 20),
    axis.text  = element_text(size = 20),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20)
  ) +
  scale_y_continuous(limits = c(0, NA)) +
  scale_color_brewer(
    palette = "Dark2",
    breaks = c("Cigarettes", "Cigar", "Smokeless Tobacco", "Pipe")
  ) +
  scale_shape_manual(
    values = c(15, 16, 17, 18),
    breaks = c("Cigarettes", "Cigar", "Smokeless Tobacco", "Pipe")
  )

histogram_fun_2015 <- function(datacol, drug_name, limit, colorcode) {
  data <- drugdata %>%
    group_by(year) %>%
    count(day = .data[[datacol]]) %>%
    mutate(`Relative share` = n / sum(n)) %>%
    filter(day >= 1 & day <= 30) %>%
    filter(year == 2015) %>%
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
      x = "Number of Days Used in Last 30 Days",
      y = "Relative Share"
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

histogram_fun_2019 <- function(datacol, drug_name, limit, colorcode) {
  data <- drugdata %>%
    group_by(year) %>%
    count(day = .data[[datacol]]) %>%
    mutate(`Relative share` = n / sum(n)) %>%
    filter(day >= 1 & day <= 30) %>%
    filter(year == 2019) %>%
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
      x = "Number of Days Used in Last 30 Days",
      y = "Relative Share"
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
histogram_fun_2015("alcdays", "Alcohol", 0.085, "#0072B2")
histogram_fun_2019("alcdays", "Alcohol", 0.085, "#0072B2")

histogram_fun_2015("CIG30USE", "Cigarettes", 0.12, "#009E73")
histogram_fun_2019("CIG30USE", "Cigarettes", 0.12, "#009E73")

histogram_fun_2015("COCUS30A", "Cocaine", 0.003, "#E69F00")
histogram_fun_2019("COCUS30A", "Cocaine", 0.003, "#E69F00")

histogram_fun_2019("HER30USE", "Heroin", 0.0004, "#CC79A7")
histogram_fun_2015("HER30USE", "Heroin", 0.0004, "#CC79A7")

##########################
# Function: Mosaic Plot for Bivariate Data (values 1 and 2)
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
      main = paste("Correlation between", varname1, "and", varname2),
      cex.axis = 1.4
    )
}

# Example calls for mosaic plots
mosaicfun("herever", "cocever", "Heroin", "Cocaine")
mosaicfun("cigever", "cocever", "Cigarettes", "Cocaine")
mosaicfun("alcever", "cocever", "Alcohol", "Cocaine")
mosaicfun("alcever", "cigever", "Alcohol", "Cigarettes")

##########################
# Extension: Mosaic Plot for "Last 30 Days"
##########################

mosaicfun30 <- function(var1, var2, varname1, varname2) {
  
  # Dataset in which var1 and var2 are recoded to 1 if 1-30, otherwise 2
  allfilterdata30 <- allfilterdata %>%
    mutate(
      usage1 = if_else(.data[[var1]] >= 1 & .data[[var1]] <= 30, 1, 2),
      usage2 = if_else(.data[[var2]] >= 1 & .data[[var2]] <= 30, 1, 2)
    )
  
  # 2x2 table from the combinations (Yes/Yes, No/Yes, Yes/No, No/No)
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
      main = paste("Correlation between", varname1, "and", varname2, "(Last 30 Days)"),
      cex.axis = 1.4
    )
}

# Example call: Mosaic plot for the last 30 days
mosaicfun30("CIG30USE", "COCUS30A", "Cigarettes", "Cocaine")

####################
#Demographics Drugs
####################
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

## Drug Dependency and Gender
Drug.Dependency.Gender <-data2019 %>%
  select(irsex, depndcoc, depndalc, depndher) %>%
  pivot_longer(cols = c(depndcoc,depndher, depndalc), names_to = "Drug", values_to = "Usage") %>%
  filter(Usage == 1)

ggplot(Drug.Dependency.Gender, aes(x = factor(Drug), fill = factor(irsex)))+
  geom_bar(position = "fill")+
  labs(title = "Drug Dependency by Gender")+
  scale_x_discrete(labels = c("depndalc" = "Alkohol", "depndcoc" = "Cokain", "depndher" = "Heroin"))+
  scale_fill_manual(labels = c("1" = "Männer", "2" = "Frauen"), values =c("1" = "darkblue", "2" = "maroon"))+
  labs(title = "Abhängigkeit von Männern und Frauen", x = "Substanz")+
  theme_light() +
  theme(
    axis.title = element_text(size = 15),  # Achsentitel
    axis.text  = element_text(size = 15),  # Achsbeschriftungen
  )

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
  labs(title = "Abhängigkeit der Ethnien", x = "Gruppen")+theme_light() +
  theme(
    axis.title = element_text(size = 15),  # Achsentitel
    axis.text  = element_text(size = 15),  # Achsbeschriftungen
    legend.position = "bottom"  # Legendentext
  )

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
  labs( title = "Nikotionabhängigkeit der Geschlechter", x = "Geschlecht", y = "Anteil")+
  theme_light() +
  theme(
    axis.title = element_text(size = 20),  # Achsentitel
    axis.text  = element_text(size = 20),  # Achsbeschriftungen
    legend.position = "none"  # Legendentext
  )

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
  labs(title = "Nikotinabhängigkeit der Altersgruppen", x = " ")+
  theme_light() +
  theme(
    axis.title = element_text(size = 20),  # Achsentitel
    axis.text  = element_text(size = 20),  # Achsbeschriftungen
    legend.position = "none"  # Legendentext
  )

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
  labs(title = "Nikotin Abhängigkeit der verschieden Kulturen")+
  theme_light() +
  theme(
    axis.title = element_text(size = 15),  # Achsentitel
    axis.text  = element_text(size = 15),  # Achsbeschriftungen
    legend.position = "none"  # Legendentext
  )



imputed.employment18 <- data2019 %>%
  select (IRWRKSTAT18) %>%
  filter (IRWRKSTAT18 < 99) %>%
  pivot_longer(cols = everything(), names_to = "status", values_to = "number")%>%
  group_by(status, number) %>%
  summarise(count = n()/56136, .groups = 'drop')


ggplot(imputed.employment18, aes(x = factor(number), y = count, fill = factor(number))) +
  geom_bar(stat = "identity")+
  scale_x_discrete(labels = c("1" = "Vollzeit", "2" = "Teilzeit", "3" = "Arbeitslos", "4" = "Nicht arbeitsfähig"))+
  labs(title = "Anstellung Personen 18+", x = "Arbeitstatus", y = "Anteil")+
  theme_light() +
  theme(
    axis.title = element_text(size = 20),  # Achsentitel
    axis.text  = element_text(size = 20),  # Achsbeschriftungen
    legend.position = "none"  # Legendentext
  )


############
#Mental Health
#############
## Drug Dependency and 'Degree' of Mental illness
Drug.Dependency.MI <- data2019 %>%
  select(depndalc,depndcoc,depndher,MI_CAT_U) %>%
  filter (MI_CAT_U >= 0) %>%
  pivot_longer(cols = c(depndalc, depndcoc,depndher), names_to = "Drug", values_to = "Response") %>%
  filter( Response == 1) 

ggplot(Drug.Dependency.MI, aes( x = factor(MI_CAT_U), fill = factor(Drug)))+
  geom_bar(position = "fill")+
  scale_x_discrete(labels = c("0" = "Keine Mentalen \nGesundheitsprobleme" , "1" = "'Milde' Mentale \nErkrankung", "2" = " 'Moderate' Mentale \nErkrankung", "3" = "Ernste Mentale \nErkrankungen"),
                   guide = guide_axis(angle = 45))+
  labs(x = "Art der Erkrankung", y = "Anteil")

##GLeicher Plot, nur achsen vertauscht
ggplot(Drug.Dependency.MI, aes(x = factor(Drug), fill = factor(MI_CAT_U)))+
  geom_bar(position = "fill")+
  scale_x_discrete(labels = c("depndalc" = "Alkohol", "depndcoc" = "Cokain","depndher" = "Heroin"))+
  labs(title = "Substanzkonsum und Mentale Gesundheit", x = " Substanz")+
  scale_fill_discrete(name = "",labels = c("0" = "Keine Mentalen Gesundheitsprobleme" , "1" = "'Milde' Mentale Erkrankung", "2" = " 'Moderate' Mentale Erkrankung", "3" = "Ernste Mentale Erkrankungen"))+
  theme_light() +
  theme(
    axis.title = element_text(size = 15),  # Achsentitel
    axis.text  = element_text(size = 20),  # Achsbeschriftungen
    legend.position = "bottom"  # Legendentext
  )

#-------------------------------------------------------------------------------
## Adult Mental Heath / Substance Treatment
## Variable zur Rekodierung: leute die Mh Treatment bekommen haben, Treatment for Drug Use
Adult.Treatment <- data2019 %>%
  select(depndalc, depndcoc, depndher,rcvmhnsptx, rcvsptxnmh, rcvmhasptx) %>%
  pivot_longer(cols = c(depndalc, depndcoc, depndher), names_to = "Drug", values_to = "Answer") %>%
  pivot_longer(cols = c(rcvmhnsptx, rcvsptxnmh, rcvmhasptx), names_to = "Treatment", values_to = "Response") %>%
  filter(Answer == 1, Response == 1)

ggplot(Adult.Treatment, aes(x = factor(Drug), fill = factor(Treatment)))+
  geom_bar(position = "fill") +
  scale_x_discrete(labels = c("depndalc" = "Alkhol","depndcoc" = "Cokain", "depndher" = "Heroin"))+
  scale_fill_discrete(name = "", labels = c("rcvmhnsptx" = "Behandlung für MI", "rcvsptxnmh" = "Behandlung für Substanzkonsum", "rcvmhasptx" = "Behandlung beides"))+
  labs(title = "Substanzkonsum und Art der Behandlung", x = "SUbstanz")+
  theme_light() +
  theme(
    axis.title = element_text(size = 15),  # Achsentitel
    axis.text  = element_text(size = 20),  # Achsbeschriftungen
    legend.position = "bottom"  # Legendentext
  )


#######
#YOUTH
######
#Overall Substance and Mental Health

## SEVERE MDE with role impairment and ALcohol or (illicit) Substance Abuse
# Normal MDE nicht in ALlfilterdata!
Youth.MDE.Substance <- data2019 %>%
  select(ymdeimaud, ymdeimudpy) ## Daten fehlen!

#-------------------------------------------------------------------------------
## Mental Health Treatment and Substance Abuse Treatment
Youth.Treatment <-data2019 %>%
  select(ymhnsptx, ysptxnmh, ymhasptx, depndalc, depndcoc, depndher) %>%
  pivot_longer(cols = c(depndalc, depndcoc, depndher), names_to = "Drug", values_to = "Answer") %>%
  pivot_longer(cols = c(ymhnsptx, ysptxnmh, ymhasptx), names_to = "Treatment", values_to = "Response") %>%
  filter(Answer == 1, Response == 1)

ggplot(Youth.Treatment, aes(x = factor(Drug), fill = factor(Treatment)))+
  geom_bar(position = "fill")+
  scale_x_discrete(labels = c("depndalc" = "Alkhol","depndcoc" = "Cokain", "depndher" = "Heroin"))+
  scale_fill_discrete(labels = c("ymhnsptx" = "Behandlung für MI", "ysptxnmh" = "Behandlung für Substanzkonsum", "ymhasptx" = "Behandlung für beides"))
labs(title = "SUbstanzkonsum und Art der Behandlung YOUTH", x = "SUbstanz")+
  theme_light() +
  theme(
    axis.title = element_text(size = 15),  # Achsentitel
    axis.text  = element_text(size = 20),  # Achsbeschriftungen
    legend.position = "bottom"  # Legendentext
  )


##-----------------------------------------------------------------------------
## MDE and Drugs Youth
Youth.MDE.Drugs <- data2019 %>%
  select(ymdeyr, depndalc,depndcoc,depndher)%>%
  pivot_longer(cols =c(depndalc, depndcoc, depndher), names_to = "Drug", values_to = "Response") %>%
  filter(Response == 1, ymdeyr == 1) %>%
  group_by(Drug) %>%
  summarize(count = n())%>%
  mutate(count = count/56136)

ggplot(Youth.MDE.Drugs, aes(x = factor(Drug), y = count))+
  geom_col()+
  scale_x_discrete(labels = c("depndalc" = "Alkhol","depndcoc" = "Cokain", "depndher" = "Heroin"))+
  labs(title = "MDE und Substanzkonsum", x = "Substanz")
