
gender <- data2019 %>%
  select(irsex) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "gender") %>%
  group_by(gender) %>%
  summarize(count = n()/56136)

ggplot(gender,aes(x = factor(gender, levels = 1:2, labels = c("Male", "Female")), y = count, fill = factor(gender)))+
  geom_col()+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values = c("1" = "darkblue", "2" = "maroon"), labels = c("1" = "Male", "2" = "Female"))+
  labs(title = "", y = "Percentage", fill = "Identity", x = "") +
  theme_light() +
  theme(
    axis.title = element_text(size = 20),  # Achsentitel
    axis.text  = element_text(size = 20),  # Achsbeschriftungen
    legend.position = "none"  # Legendentext
  )



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
#mosaicfun("herever", "cocever", "Heroin", "Kokain")
#mosaicfun("cigever", "cocever", "Zigaretten", "Kokain")
#mosaicfun("alcever", "cocever", "Alkohol", "Kokain")
#mosaicfun("alcever", "cigever", "Alkohol", "Zigaretten")

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
#mosaicfun30("CIG30USE", "COCUS30A", "Zigaretten", "Kokain")

# ? noch mit drogen
imputed.employment18 <- data2019 %>%
  select (IRWRKSTAT18) %>%
  filter (IRWRKSTAT18 < 99) %>%
  pivot_longer(cols = everything(), names_to = "status", values_to = "number")%>%
  group_by(status, number) %>%
  summarise(count = n()/56136, .groups = 'drop')


ggplot(imputed.employment18, aes(x = factor(number), y = count, fill = factor(number))) +
  geom_bar(stat = "identity")+
  scale_y_continuous(labels = scales::percent)+
  scale_x_discrete(labels = c("1" = "Employed full-time", "2" = "Employed part-time", "3" = "Unemployed", "4" = "not in labour force"))+
  labs(title = "Employment status of People 18+", x = "Employment Status", y = "Percentage")+
  theme_light() +
  theme(
    axis.title = element_text(size = 20),  # Achsentitel
    axis.text  = element_text(size = 20),  # Achsbeschriftungen
    legend.position = "none"  # Legendentext
  )




#-------------------------------------------------------------------------------
##Generel Drug Dependency / Abuse
Drug.Dependency.Abuse <- data2019 %>%
  select(alcyr, cocyr, heryr, depndalc, depndcoc, depndher, abusealc, abusecoc,abuseher) %>%
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
  ) 

# Create stacked bar plot

#nezer versuch
GPT.Drug.Dependency.Abuse <- data2019 %>%
  select(alcyr, cocyr, heryr, depndalc, depndcoc, depndher, abusealc, abusecoc, abuseher) %>%
  mutate(ID = row_number()) %>%  # eine ID-Spalte anlegen
  pivot_longer(
    cols = -ID,                  # alle Spalten außer der ID
    names_to = "Variable",       # Spaltennamen gehen in "Variable"
    values_to = "Value"          # Werte gehen in "Value"
  ) %>%
  mutate(
    Substance = case_when(
      str_detect(Variable, "alc") ~ "Alkohol",
      str_detect(Variable, "coc") ~ "Kokain",
      str_detect(Variable, "her") ~ "Heroin"
    ),
    Condition = case_when(
      str_detect(Variable, "depnd") ~ "Abhängigkeit",
      str_detect(Variable, "abuse") ~ "Missbrauch",
      str_detect(Variable, "yr")    ~ "Konsum im letzten Jahr"
    )
  ) %>%
  filter(Value == 1)  # nur Zeilen behalten, in denen wirklich "1" steht

ggplot(
  Drug.Dependency.Abuse, 
  aes(
    x = Substance, 
    fill = factor(Condition, 
                  levels = c("Konsum im letzten Jahr", "Abhängigkeit", "Missbrauch"))
  )
) +
  geom_bar(position = "fill") +
  # Achte hier auf c(...) bei den Werten
  scale_fill_manual(name = "Art des Konsums",values = c("grey65", "grey45", "grey30")) +
  labs(
    title = "",
    x = "Substanz",
    y = "Prozent"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_light() +
  theme(
    axis.title = element_text(size = 22),
    axis.text  = element_text(size = 22),
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 20)
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
  scale_y_continuous(labels = scales::percent)+
  scale_x_discrete(labels = c("0" = "Keine Mentalen \nGesundheitsprobleme" , "1" = "'Milde' Mentale \nErkrankung", "2" = " 'Moderate' Mentale \nErkrankung", "3" = "Ernste Mentale \nErkrankungen"),
                   guide = guide_axis(angle = 45))+
  labs(x = "Art der Erkrankung", y = "Anteil")



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


### Demografisch


Drogen.Einkommen <- data2019 %>%
  select(depndalc, depndcoc, depndher, income) %>%
  mutate(
    Dependency = case_when(
      depndalc == 1 & depndcoc == 0 & depndher == 0 ~ "Alkohol",
      depndcoc == 1 & depndalc == 0 & depndher == 0 ~ "Kokain",
      depndher == 1 & depndalc == 0 & depndcoc == 0 ~ "Heroin",
      depndalc == 1 & depndcoc == 1 | 
        depndalc == 1 & depndher == 1 | 
        depndcoc == 1 & depndher == 1 ~ "Mehrfachabhängigkeit"
    )
  ) %>%
  filter(!is.na(Dependency)) %>%
  mutate(
    Dependency = factor(Dependency, 
                        levels = c("Alkohol", "Kokain", "Heroin", "Mehrfachabhängigkeit"))
  ) %>%
  group_by(income, Dependency) %>%
  ggplot(aes(x = factor(income), fill = factor(Dependency))) +
  geom_bar(position = "fill") +
  geom_vline(
    xintercept = 3, 
    linetype = "dotted", 
    color = "black", 
    size = 1
  ) +
  scale_fill_manual(
    name = "Art der Abhängigkeit",   # <- Titel der Legende
    values = c(
      "Alkohol" = "#0072B2",            # Blau
      "Kokain"  = "#E69F00",            # Orange/Gelb
      "Heroin"  = "#CC79A7",            # Rosa
      "Mehrfachabhängigkeit" = "grey30" # Grau
    )
  ) +
  scale_x_discrete(
    labels = c(
      "1" = "Weniger als 20.000", 
      "2" = "20.000 - 49.999", 
      "3" = "50.000 - 74.999", 
      "4" = "75.000+"
    )
  ) +
  labs(
    x = "Familieneinkommen in $",
    y = "Prozent"
    # Optional: falls du lieber hier die Legende benennen willst, ginge z. B.:
    # fill = "Abhängigkeit"
  ) +
  theme_light() +
  theme(
    axis.title      = element_text(size = 20),   
    axis.text       = element_text(size = 20),   
    legend.title    = element_text(size = 17.5), # auf Dezimalpunkt achten
    legend.text     = element_text(size = 17.5),
    legend.position = "bottom"
  )
Drogen.Einkommen




