#================================================
# Laden von Daten und nötigen Paketen
#================================================

load("C:/Users/denis/OneDrive/Desktop/RStudio/locales/StatPrak-Overdose/Daten bearbeitet/combi_redu_data.Rdata")

drugusedata <- allfilterdata
library(tidyverse)
library(checkmate)
str(drugusedata)
summary(drugusedata)


#==================================================================================
# Im Folgendem sind die ersten Versuche etwas herauszufinden.
# => vieles davon nicht so schön und einheitlich außerdem teilweise nicht sinnvoll
# @Raphi den Teil brauchst du nicht zu vereinheitlichen
#==================================================================================

# creating graph for days cigs used in the last 30 days
drugusedata %>%
  filter(CIG30USE >= 1 & CIG30USE <= 30) %>% 
  group_by(year) %>% 
  summarize(avg_days = mean(CIG30USE, na.rm = TRUE)) %>% 
  ggplot(aes(x = factor(year), y = avg_days)) + 
  geom_col(fill = "steelblue") + 
  labs(
    title = "average days of cigaretts use in the past 30 days",
    x = "Year", 
    y = "Average Days" 
  ) +
  theme_minimal() 
  

#smoked in the last 30 days in percent
drugusedata %>%
  group_by(year) %>%
  mutate(total_people = n()) %>%
  filter(CIG30USE >= 1 & CIG30USE <= 30) %>%
  summarize(
    total_people = first(total_people),
    smokers_count = n(),
    percentage = (smokers_count / total_people) * 100
  ) %>%
  ggplot(aes(x = factor(year), y = percentage)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Percentage of People who smoked cigaretts in the Past 30 Days",
    x = "Year",
    y = "smoked in %"
  ) +
  theme_minimal()

# Nur von denen die geraucht haben 
drugusedata %>%
  filter(CIG30USE >= 1 & CIG30USE <= 30) %>%
  ggplot(aes(x = factor(year), y = CIG30USE)) +
  geom_boxplot(fill = "steelblue", color = "black", alpha = 0.7, outlier.color = "red", outlier.shape = 16) +
  labs(
    title = "Boxplots der Anzahl gerauchter Tage (2015-2019)",
    x = "Jahr",
    y = "Anzahl der Tage (geraucht)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  )


# Boxplot für Anzahl tage cigs geraucht NICHT WIRKLICH SINNVOLL
drugusedata %>%
  mutate(CIG30USE = ifelse(CIG30USE %in% c(91, 93), 0, CIG30USE)) %>%  
  filter(CIG30USE >= 0 & CIG30USE <= 30) %>%  
  ggplot(aes(x = factor(year), y = CIG30USE)) +
  geom_boxplot(fill = "steelblue", color = "black", alpha = 0.7, outlier.color = "red", outlier.shape = 16) +
  labs(
    title = "Boxplots der Anzahl gerauchter Tage (inklusive Nichtraucher) 2015-2019",
    x = "Jahr",
    y = "Anzahl der Tage (geraucht)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  )


# Cigs Histogram Anzahl gerauchter Tage inkl. Nichtraucher Totale Häugigkeiten 
drugusedata %>%
  mutate(CIG30USE = ifelse(CIG30USE %in% c(91, 93), 0, CIG30USE)) %>%  
  filter(CIG30USE >= 0 & CIG30USE <= 30) %>%  
  ggplot(aes(x = CIG30USE)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black", alpha = 0.7) +
  facet_wrap(~ year, nrow = 1, scales = "free_y") +  
  labs(
    title = "Histogramme der Anzahl gerauchter Tage (inklusive Nichtraucher, 2015-2019)",
    x = "Anzahl der Tage (geraucht)",
    y = "Häufigkeit"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    strip.text = element_text(size = 12)  
  )


# anzahl gerauchter Tage in relativen Häufigkeiten inkl. Nichtraucher 
drugusedata %>%
  mutate(CGR30USE = ifelse(CGR30USE %in% c(91, 93), 0, CGR30USE)) %>%
  ggplot(aes(x = CGR30USE)) +
  geom_histogram(aes(y = after_stat(density)), 
                 binwidth = 1, 
                 fill = "steelblue", 
                 color = "black", 
                 alpha = 0.7) +
  facet_wrap(~ year, nrow = 1, scales = "free_y") +
  labs(
    title = "Relative Häufigkeiten der Rauchtage (2015-2019)",
    x = "Anzahl der gerauchten Tage",
    y = "Relative Häufigkeit"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    strip.text = element_text(size = 12)
  )


# Cigs relative Häufigkeiten NUR Raucher in Historgram
drugusedata %>%
  filter(CGR30USE >= 1 & CGR30USE <= 30) %>% 
  ggplot(aes(x = CGR30USE)) +
  geom_histogram(aes(y = after_stat(density)), 
                 binwidth = 1, 
                 fill = "steelblue", 
                 color = "black", 
                 alpha = 0.7) +
  facet_wrap(~ year, nrow = 1, scales = "free_y") +
  labs(
    title = "Relative Häufigkeiten der Rauchtage (2015-2019)",
    x = "Anzahl der gerauchten Tage",
    y = "Relative Häufigkeit"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    strip.text = element_text(size = 12)
  )


# cigs relative Häufigkeit der gerauchten Tage 

drugusedata_clean <- drugusedata %>%
  mutate(
    CGR30USE = ifelse(CGR30USE %in% c(91,93,94,97,98), NA, CGR30USE)
  ) %>%
  filter(!is.na(CGR30USE)) %>%
  filter(CGR30USE > 0, CGR30USE <= 30)


drugusedata_clean %>%
  ggplot(aes(x = CGR30USE)) +
  geom_histogram(
    aes(y = after_stat(count / sum(count))),
    breaks = seq(0.5, 30.5, by = 1),
    fill = "steelblue",
    color = "black",
    alpha = 0.7,
    na.rm = TRUE  # Entfernt dennoch eventuell auftretende NA-Werte beim Plotten
  ) +
  facet_wrap(~ year, nrow = 1, scales = "free_y") +
  scale_x_continuous(breaks = seq(5, 30, by = 5), limits = c(1, 30)) +
  labs(
    title = "Relative Häufigkeiten der Rauchtage (2015-2019)",
    x = "Anzahl der gerauchten Tage",
    y = "Relative Häufigkeit"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    strip.text = element_text(size = 12)
  )



# Cigars Versuche es in Histogrammen besser darzustellen  von Leuten die geraucht haben 
drugusedata %>%
  filter(CGR30USE >= 1 & CGR30USE <= 30) %>%
  ggplot(aes(x = CGR30USE)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black", alpha = 0.7) +
  facet_wrap(~ year, nrow = 1, scales = "free_y") +
  labs(
    title = "Histograms of Number of Days Smoked Cigars (2015-2019)",
    x = "Number of Days Smoked",
    y = "Frequency"
  ) +
  theme_minimal()


# Creating boxplot for the number of days cigars were smoked in the past 30 days (grouped by year) VON LEUTEN DIE GERAUCHT HABEN 
drugusedata %>%
  filter(CGR30USE >= 1 & CGR30USE <= 30) %>%
  ggplot(aes(x = factor(year), y = CGR30USE)) +
  geom_boxplot(fill = "steelblue", color = "black", alpha = 0.7, outlier.color = "red", outlier.shape = 16) +
  labs(
    title = "Boxplots of Number of Days Smoked Cigars (2015-2019)",
    x = "Year",
    y = "Anzahl Tage (geraucht)"
  ) +
  theme_minimal()


# Adjusting data to include non-smokers as 0 days smoked        ABER NICHT SO SINNVOLL
drugusedata %>%
  mutate(CGR30USE = ifelse(CGR30USE %in% c(91, 93), 0, CGR30USE)) %>%
  filter(CGR30USE %in% c(0:30)) %>%
  ggplot(aes(x = factor(year), y = CGR30USE)) +
  geom_boxplot(fill = "steelblue", color = "black", alpha = 0.7, outlier.color = "black", outlier.shape = 16) +
  labs(
    title = "Boxplots of Number of Days Smoked Cigars (2015-2019)",
    x = "Year",
    y = "Number of Days Smoked"
  ) +
  theme_minimal()


#============== nützliche Variablen ========================================================

# CIG30USE HOW MANY DAYS SMOKED CIG IN PAST 30 DAYS
# CGR30USE HOW MANY DAYS SMOKED CIGAR IN PAST 30 DAYS
# ALCDAYS heißt aber alcdays in drugusedata # DAYS HAD ONE OR MORE DRINKS PAST 30 DAYS
# ALCUS30D ist für #Drinks in den letzten 30 Tagen  
# AL30EST ist für #Tagen an denen min. ein Drink in den letzten 30 Tagen
# COCUS30A ist für #Tage an denen Kokain in den letzten 30 Tagen
# CC30EST BEST ESTIMATE # DAYS USED COCAINE PAST 30 DAYS
# HER30USE # DAYS USED HEROIN PAST 30 DAYS
# HR30EST BEST EST. # DAYS USED HEROIN PAST 30 DAYS

#============================================================================================
# Hier versucht es mit Funktionen und in Raphis Stil zu machen. Es klappt besser, aber noch nicht ganz.
# Probleme: Bei den Boxplots von Hero, Cigs und Coc sieht man nicht => evtl. noch reinzoomen  
# auch nach reinzoomen noch nicht sehr aussagekräfitg, da immer noch extrem nah an 0
# ==> Es funktioniert aber man sieht halt nicht viel
# @Raphi ab hier kannst du vereinheitlichen 
#============================================================================================

# Funktion: Daten für eine bestimmte Droge vorbereiten.
# Werte von 1–30 bleiben so, alle anderen werden auf 0 gesetzt.
prepare_data <- function(datacol, drug_name) {
  drugusedata %>%
    mutate(
      UsageDays = case_when(
        .data[[datacol]] >= 1 & .data[[datacol]] <= 30 ~ .data[[datacol]],
        TRUE ~ 0
      ),
      Drug = drug_name
    ) %>%
    select(year, Drug, UsageDays)
}

# Funktion: Durchschnittliche Nutzungstage pro Jahr (inkl. 0-Tage-Nutzer)
avg_data_fun <- function(datacol, drug_name) {
  prepare_data(datacol, drug_name) %>%
    group_by(year, Drug) %>%
    summarize(avg_days = mean(UsageDays, na.rm = TRUE), .groups = "drop")
}

# Daten für jede Droge mit Durchschnitt berechnen
her30_avg_data <- avg_data_fun("HER30USE", "Heroin")
coc30_avg_data <- avg_data_fun("COCUS30A", "Cocaine")
alc30_avg_data <- avg_data_fun("alcdays", "Alcohol")
cig30_avg_data <- avg_data_fun("CIG30USE", "Cigarettes")

# Kombinierte Daten für das Liniendiagramm
combined_avg_usage <- bind_rows(her30_avg_data, coc30_avg_data, alc30_avg_data, cig30_avg_data)

# Liniendiagramm mit Durchschnittstagen (inkl. 0er)
ggplot(combined_avg_usage, aes(x = year, y = avg_days, color = Drug)) +
  geom_point(size = 3) +
  geom_line() +
  theme_light() +
  labs(
    title = "Average number of days using substances (including non-users as 0)",
    x = "Year",
    y = "Average Usage Days"
  )

# Daten für Boxplots erstellen (inklusive 0-Werte)
her30_box_data <- prepare_data("HER30USE", "Heroin")
coc30_box_data <- prepare_data("COCUS30A", "Cocaine")
alc30_box_data <- prepare_data("alcdays", "Alcohol")
cig30_box_data <- prepare_data("CIG30USE", "Cigarettes")

# Boxplots (inkl. 0-Werte)
ggplot(her30_box_data, aes(x = factor(year), y = UsageDays)) +
  geom_boxplot() +
  theme_light() +
  coord_cartesian(ylim = c(0, 5)) +
  labs(title = "Heroin usage days including non-users (0)", x = "Year", y = "Usage Days")

ggplot(coc30_box_data, aes(x = factor(year), y = UsageDays)) +
  geom_boxplot() +
  theme_light() +
  coord_cartesian(ylim = c(0, 5)) +
  labs(title = "Cocaine usage days including non-users (0)", x = "Year", y = "Usage Days")

ggplot(alc30_box_data, aes(x = factor(year), y = UsageDays)) +
  geom_boxplot() +
  theme_light() +
  coord_cartesian(ylim = c(0, 15)) +
  labs(title = "Alcohol usage days including non-users (0)", x = "Year", y = "Usage Days")

ggplot(cig30_box_data, aes(x = factor(year), y = UsageDays)) +
  geom_boxplot() +
  theme_light() +
  coord_cartesian(ylim = c(0, 5)) + 
  labs(title = "Cigarettes usage days including non-users (0)", x = "Year", y = "Usage Days")

#=============================================================
# Versuch es mit histogrammen besser darzustellen
# => funktioniert besser, bin mir aber noch nicht sicher 
#    ob die Werte stimmen
# => evtl. absolute Häufigkeit über Histogram und rel. Häufigkeit über Kerndichteschätzer
#=============================================================

# Funktion zur Berechnung der relativen Anteile und Erstellen des Histogramms
histogram_fun <- function(datacol, drug_name) {
  data <- drugusedata %>%
    group_by(year) %>%
    count(day = .data[[datacol]]) %>%
    mutate("Relative share" = n / sum(n)) %>%
    # Nur Tage zwischen 1 und 30 filtern
    filter(day >= 1 & day <= 30) %>%
    ungroup() %>%
    mutate(Drug = drug_name)
  
  ggplot(data, aes(x = factor(day), y = `Relative share`)) +
    geom_col(fill = "darkgrey", color = "black", alpha = 0.7) +
    facet_wrap(~year) +
    theme_light() +
    labs(
      title = paste0("Distribution of usage days for ", drug_name),
      x = "Number of Days Used in Last 30 Days",
      y = "Relative Share"
    )
}

# Aufrufe der Plots für jede Droge:
histogram_fun("CIG30USE", "Cigarettes")
histogram_fun("alcdays", "Alcohol")
histogram_fun("COCUS30A", "Cocaine")
histogram_fun("HER30USE", "Heroin")

# Versuch es über Kerndichteschätzer zu machen
density_fun <- function(datacol, drug_name) {
  data <- drugusedata %>%
    # Filter auf 1-30 Tage
    filter(.data[[datacol]] >= 1 & .data[[datacol]] <= 30) %>%
    mutate(UsageDays = .data[[datacol]], Drug = drug_name)
  
  ggplot(data, aes(x = UsageDays)) +
    geom_density(fill = "darkgrey", alpha = 0.7) +
    facet_wrap(~year) +
    theme_light() +
    labs(
      title = paste0("Kernel Density of Usage Days for ", drug_name),
      x = "Number of Days Used in Last 30 Days",
      y = "Density"
    )
}

# Aufrufen der Plots der Kerndichteschätzer
density_fun("CIG30USE", "Cigarettes")
density_fun("alcdays", "Alcohol")
density_fun("COCUS30A", "Cocaine")
density_fun("HER30USE", "Heroin")



#===========Versuch Zusammenhang zwischen Drogen zu finden======================
# @Raphi der Teil ist noch in Arbeit 
# Am besten ist es wohl es mit den ever Daten zu machen, da in den letzten 30 Tagen
# wenig Heroin und Cocain genommen wurde und es deshalb nicht sehr aussgekräftig 
# ist.


#===============================================================================









