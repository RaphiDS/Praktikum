
gender <- data2019 %>%
  select(irsex) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "gender") %>%
  group_by(gender) %>%
  summarize(count = n()/56136)

ggplot(gender,aes(x = factor(gender, levels = 1:2, labels = c("Male", "Female")), y = count, fill = factor(gender)))+
  geom_col()+
  scale_fill_manual(values = c("1" = "blue", "2" = "red"), labels = c("1" = "Male", "2" = "Female"))+
  labs(title = "Gender", y = "Percentage", fill = "Identity", x = "") +
  theme_light() +
  theme(
    axis.title = element_text(size = 20),  # Achsentitel
    axis.text  = element_text(size = 20),  # Achsbeschriftungen
    legend.position = "none"  # Legendentext
  )



Racial.Background <- data2019 %>%
  select (NEWRACE2, eduhighcat) %>% ## selected AI regions, racial background and education level
  filter(eduhighcat <5)       ## wert 5 streichen? --> Leute unter 17 kein Abschluss, zählen nicht


ggplot(Racial.Background, aes(x = NEWRACE2, fill = factor(eduhighcat)))+
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = c("1" = "White", "2" = "Afr.Am", "3" = "Am/AK Native", "4" ="Other Pac Isl", "5" = " Asian", "6" = "more than one Race", "7" = "Hispamic"))+
  scale_fill_discrete(labels = c("1" = "some High School", "2"= "HIgh School Grad", "3" ="Some coll/Assoc Dg", "4"= "College graduate"))+
  theme_light()+
  labs(title = "Education achieved by each Race")


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
    row.names = c(paste(varname1, "Ja"), paste(varname1, "Nein"))
  ) %>%
    rename(
      !!paste(varname2, "Ja") := X1,
      !!paste(varname2, "Nein")  := X2
    ) %>%
    mosaicplot(
      color = TRUE,
      main = paste("Häufigkeiten von", varname1, "und", varname2),
      cex.axis = 1.4
    )
}

# Example calls for mosaic plots
mosaicfun("herever", "cocever", "Heroin", "Kokain")
mosaicfun("cigever", "cocever", "Zigaretten", "Kokain")
mosaicfun("alcever", "cocever", "Alkohol", "Kokain")
mosaicfun("alcever", "cigever", "Alkohol", "Zigaretten")

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
    row.names = c(paste(varname1, "Ja"), paste(varname1, "Nein"))
  ) %>%
    rename(
      !!paste(varname2, "Ja") := X1,
      !!paste(varname2, "Nein")  := X2
    ) %>%
    mosaicplot(
      color = TRUE,
      main = paste("Häufigkeiten von", varname1, "und", varname2, "(Letzte 30 Tage)"),
      cex.axis = 1.4
    )
}

# Example call: Mosaic plot for the last 30 days
mosaicfun30("CIG30USE", "COCUS30A", "Zigaretten", "Kokain")


imputed.employment18 <- data2019 %>%
  select (IRWRKSTAT18) %>%
  filter (IRWRKSTAT18 < 99) %>%
  pivot_longer(cols = everything(), names_to = "status", values_to = "number")%>%
  group_by(status, number) %>%
  summarise(count = n()/56136, .groups = 'drop')


ggplot(imputed.employment18, aes(x = factor(number), y = count, fill = factor(number))) +
  geom_bar(stat = "identity")+
  scale_x_discrete(labels = c("1" = "Employed full-time", "2" = "Employed part-time", "3" = "Unemployed", "4" = "not in labour force"))+
  labs(title = "Employment status of People 18+", x = "Employment Status", y = "Percentage")+
  theme_light() +
  theme(
    axis.title = element_text(size = 20),  # Achsentitel
    axis.text  = element_text(size = 20),  # Achsbeschriftungen
    legend.position = "none"  # Legendentext
  )


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
##Generel Drug Dependency / Abuse
Drug.Dependency.Abuse <- data2019 %>%
  select(alcyr, cocyr, heryr, depndalc, depndcoc, depndher, abusealc, abusecoc,abuseher) %>%
  mutate(ID = row_number()) %>%  # Add an ID column for pivoting
  pivot_longer(cols = -ID, names_to = "Variable", values_to = "Value") %>%
  mutate(
    Substance = case_when(
      str_detect(Variable, "alc") ~ "Alkohol",
      str_detect(Variable, "coc") ~ "Kokain",
      str_detect(Variable, "her") ~ "Heroin"
    ),
    Condition = case_when(
      str_detect(Variable, "depnd") ~ "Abhängigkeit",
      str_detect(Variable, "abuse") ~ "Missbrauch",
      str_detect(Variable, "yr") ~ "Konsum im letzten Jahr"
    )
  ) %>%
  filter(Value == 1)  # Keep only cases where the flag is 1 (indicating presence)

# Create stacked bar plot
ggplot(Drug.Dependency.Abuse, aes(x = Condition, fill = Substance)) +
  geom_bar(position = "fill") +
  labs(title = "Substanzkonsum, Abhängigkeit und Missbrauch im letzten Jahr",
       x = "Substanz")+
  theme_light() +
  theme(
    axis.title = element_text(size = 20),  # Achsentitel
    axis.text  = element_text(size = 15),  # Achsbeschriftungen
  )


## SEVERE MDE with role impairment and ALcohol or (illicit) Substance Abuse
# Normal MDE nicht in ALlfilterdata!
Youth.MDE.Substance <- data2019 %>%
  select(ymdeimaud, ymdeimudpy) ## Daten fehlen!

# Youth MDE
## Youth MDE in the last year
Youth.MDE <- data2019 %>%
  select(ymdeyr) %>%
  filter(ymdeyr >= 0)%>%
  pivot_longer(cols = everything(), names_to = "Var", values_to = "Response")%>%
  group_by(Response)%>%
  summarise(count = n())#%>%
mutate(count = count/56136)

ggplot(Youth.MDE, aes(x = Response, y = count))+
  geom_col()+
  scale_x_discrete(labels = c("1" = "Yes", "2" = "No"))+
  labs(title = "Youth mith MDE in last Year")
#No Treatment but Drugs
TreatmentNo.Drugs <- data2019 %>%
  select

## Youth with MDE and Substance Abuse
Youth.MDE.Substance <- data2019 %>%
  select(YMDEAUDPY, ymdeimudpy, ymdeudpy) # Variable fehlt

## YOuth MDE an Dependency

## Spearman Rang versuch




#-------------------------------------------------------------------------
## Youth with MDE and Substance Abuse
Youth.MDE.Substance <- data2019 %>%
  select(YMDEAUDPY, ymdeimudpy, ymdeudpy) # Variable fehlt






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
  labs(title = "Substanzabhängigkeit und Mentale Gesundheit", x = " Substanz")+
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
  labs(title = "Substanzabhängigkeit und Mentale Gesundheit", x = " Substanz")+
  scale_fill_discrete(name = "",labels = c("0" = "Keine Mentalen Gesundheitsprobleme" , "1" = "'Milde' Mentale Erkrankung", "2" = " 'Moderate' Mentale Erkrankung", "3" = "Ernste Mentale Erkrankungen"))+
  theme_light() +
  theme(
    axis.title = element_text(size = 15),  # Achsentitel
    axis.text  = element_text(size = 20),  # Achsbeschriftungen
    legend.position = "bottom"  # Legendentext
  )

library(ggplot2)
library(dplyr)
library(tidyr)



# Erstelle eine Häufigkeitstabelle für jede Kombination aus Abhängigkeitskategorie und mentaler Erkrankung
Drug.Dependency.Table <- data2019 %>%
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
  filter(MI_CAT_U >= 0) %>%  # Nur gültige mentale Erkrankungen
  group_by(Dependency, MI_CAT_U) %>%
  summarise(Frequency = n(), .groups = "drop") %>%
  arrange(Dependency, MI_CAT_U)

# Anzeige der Tabelle in RStudio
print(Drug.Dependency.Table)
