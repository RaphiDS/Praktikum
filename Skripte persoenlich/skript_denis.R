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

# Boxplot Nur von denen die cigs geraucht haben 
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
      title = paste0("Kerndichteschätzer für #Tage Konsum ", drug_name),
      x = "# Tagen in den letzten 30 Tagen",
      y = "Dichte"
    )
}

# Aufrufen der Plots der Kerndichteschätzer
density_fun("CIG30USE", "Zigaretten")
density_fun("alcdays", "Alkohol")
density_fun("COCUS30A", "Cocain")
density_fun("HER30USE", "Heroin")



#===========Versuch Zusammenhang zwischen Drogen zu finden======================
# @Raphi der Teil ist noch in Arbeit. Habe inzwischen hier was gemacht, ändere es
# aber vllt wieder.
# Am besten ist es wohl es mit den ever Daten zu machen, da in den letzten 30 Tagen
# wenig Heroin und Cocain genommen wurde und es deshalb nicht sehr aussgekräftig 
# ist.
# Wie? mit Streudiagrammen #tage droge1 eine achse #tage droge2 andere achse
# und nach Jahren gruppieren 
# => sieht noch nicht sehr sinnvoll aus 
# => auch mit heatmap noch nicht gut 
#===============================================================================

# Funktion zur Datenaufbereitung 
prepare_jointdata <- function() {
  drugusedata %>%
    mutate(
      CIG30USE_clean = ifelse(CIG30USE >= 1 & CIG30USE <= 30, CIG30USE, 0),
      alcdays_clean = ifelse(alcdays >= 1 & alcdays <= 30, alcdays, 0),
      COCUS30A_clean = ifelse(COCUS30A >= 1 & COCUS30A <= 30, COCUS30A, 0),
      HER30USE_clean = ifelse(HER30USE >= 1 & HER30USE <= 30, HER30USE, 0)
    ) %>%
    # Nur Personen behalten, die mindestens eine Droge konsumiert haben
    filter(
      (CIG30USE_clean > 0 | alcdays_clean > 0 | COCUS30A_clean > 0 | HER30USE_clean > 0),
      year >= 2015 & year <= 2019
    ) %>%
    select(year, CIG30USE_clean, alcdays_clean, COCUS30A_clean, HER30USE_clean)
}

# Funktion zum Plotten des Zusammenhangs zwischen zwei ausgewählten Drogen
plot_drug_correlation <- function(data, drug_x, drug_y) {
  ggplot(data, aes(x = .data[[drug_x]], y = .data[[drug_y]])) +
    geom_point(alpha = 0.7, color = "grey30") +
    facet_wrap(~year, nrow = 1) +
    theme_light() +
    labs(
      title = paste0("Zusammenhang zwischen ", drug_x, " und ", drug_y, " nach Jahren"),
      x = paste0("Tage Nutzung: ", drug_x),
      y = paste0("Tage Nutzung: ", drug_y)
    )
}


# Daten vorbereiten
joint_data <- prepare_jointdata()

# Beispielhafte Aufrufe für verschiedene Drogenpaare
plot_drug_correlation(joint_data, "alcdays_clean", "CIG30USE_clean")
plot_drug_correlation(joint_data, "alcdays_clean", "COCUS30A_clean")
plot_drug_correlation(joint_data, "alcdays_clean", "HER30USE_clean")
plot_drug_correlation(joint_data, "CIG30USE_clean", "COCUS30A_clean")
plot_drug_correlation(joint_data, "CIG30USE_clean", "HER30USE_clean")
plot_drug_correlation(joint_data, "COCUS30A_clean", "HER30USE_clean")



# Funktion zum Erstellen einer Heatmap für den Zusammenhang zwischen zwei Variablen
plot_heatmap_correlation <- function(data, drug_x, drug_y) {
  ggplot(data, aes(x = .data[[drug_x]], y = .data[[drug_y]])) +
    geom_bin2d(bins = 20) + # Aufteilung der Daten in Bins
    scale_fill_viridis_c(option = "magma", name = "Dichte") +
    facet_wrap(~year, nrow = 1) + # Facets für die Jahre
    theme_minimal() +
    labs(
      title = paste0("Heatmap: Zusammenhang zwischen ", drug_x, " und ", drug_y, " nach Jahren"),
      x = paste0("Tage Nutzung: ", drug_x),
      y = paste0("Tage Nutzung: ", drug_y)
    )
}


# Beispielhafte Aufrufe für verschiedene Drogenpaare
plot_heatmap_correlation(joint_data, "alcdays_clean", "CIG30USE_clean")
plot_heatmap_correlation(joint_data, "alcdays_clean", "COCUS30A_clean")
plot_heatmap_correlation(joint_data, "alcdays_clean", "HER30USE_clean")
plot_heatmap_correlation(joint_data, "CIG30USE_clean", "COCUS30A_clean")
plot_heatmap_correlation(joint_data, "CIG30USE_clean", "HER30USE_clean")
plot_heatmap_correlation(joint_data, "COCUS30A_clean", "HER30USE_clean")

# Funktion zum Erstellen eines Hexbin-Plots
plot_hexbin_correlation <- function(data, drug_x, drug_y) {
  ggplot(data, aes(x = .data[[drug_x]], y = .data[[drug_y]])) +
    geom_hex(bins = 15) + # Hexagonale Bins, Anzahl der Bins anpassen
    scale_fill_viridis_c(option = "plasma", name = "Dichte") +
    facet_wrap(~year, nrow = 1) +
    theme_minimal() +
    labs(
      title = paste0("Hexbin: Zusammenhang zwischen ", drug_x, " und ", drug_y, " nach Jahren"),
      x = paste0("Tage Nutzung: ", drug_x),
      y = paste0("Tage Nutzung: ", drug_y)
    )
}
plot_hexbin_correlation(joint_data, "alcdays_clean", "CIG30USE_clean")
plot_hexbin_correlation(joint_data, "alcdays_clean", "COCUS30A_clean")
plot_hexbin_correlation(joint_data, "alcdays_clean", "HER30USE_clean")
plot_hexbin_correlation(joint_data, "CIG30USE_clean", "COCUS30A_clean")
plot_hexbin_correlation(joint_data, "CIG30USE_clean", "HER30USE_clean")
plot_hexbin_correlation(joint_data, "COCUS30A_clean", "HER30USE_clean")

#======================weitere nützliche Variablen==============================
# ALCYRTOT TOTAL # OF DAYS USED ALCOHOL IN PAST 12 MOS
# COCYRTOT TOTAL # OF DAYS USED COCAINE IN PAST 12 MONTHS
# HRDAYPYR # DAYS USED HEROIN PAST 12 MONTHS
#===============================================================================
# Veranschaulichung mithilfe von boxplots möglicherweise sinnvoll
#===============================================================================

# Funktion: Daten für eine bestimmte Droge vorbereiten.
# Werte von 1–365 bleiben so, alle anderen werden auf 0 gesetzt.
prepare365_data <- function(datacol, drug_name) {
  drugusedata %>%
    mutate(
      UsageDays = case_when(
        .data[[datacol]] >= 1 & .data[[datacol]] <= 365 ~ .data[[datacol]],
        TRUE ~ 0
      ),
      Drug = drug_name
    ) %>%
    select(year, Drug, UsageDays)
}

# Daten für Boxplots erstellen (inklusive 0-Werte)
alc365_box_data <- prepare365_data("alcyrtot", "Alcohol") 
coc365_box_data <- prepare365_data("cocyrtot", "Cocaine")
her365_box_data <- prepare365_data("hrdaypyr", "Heroin")

# Boxplots (inkl. 0-Werte) für jede Droge
ggplot(alc365_box_data, aes(x = factor(year), y = UsageDays)) +
  geom_boxplot() +
  theme_light() +
  coord_cartesian(ylim = c(0, 365)) +
  labs(title = "Alcohol usage days including non-users (0)", x = "Year", y = "Usage Days")

ggplot(coc365_box_data, aes(x = factor(year), y = UsageDays)) +
  geom_boxplot() +
  theme_light() +
  coord_cartesian(ylim = c(0, 365)) +
  labs(title = "Cocaine usage days including non-users (0)", x = "Year", y = "Usage Days")

ggplot(her365_box_data, aes(x = factor(year), y = UsageDays)) +
  geom_boxplot() +
  theme_light() +
  coord_cartesian(ylim = c(0, 365)) +
  labs(title = "Heroin usage days including non-users (0)", x = "Year", y = "Usage Days")





#############################################################################################
# Zusammenhang Alter und Zigarettenkonsum

# IRSEX Len : 1    IMPUTATION REVISED GENDER  

#############################################################################################
load("Daten bearbeitet/combi_redu_data.Rdata")
data2019 <- allfilterdata %>%
  filter (year == 2019)


# Altersgruppen basierend auf AGE2 definieren
data2019 <- data2019 %>%
  mutate(Altersgruppe = case_when(
    AGE2 %in% 1:6 ~ "12-17 Jahre",
    AGE2 %in% 7:10 ~ "18-25 Jahre",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(Altersgruppe))  # Entferne unpassende Alterswerte

# Alle anderen Werte in CIG30AV als 0 interpretieren
data2019 <- data2019 %>%
  mutate(CIG30AV = ifelse(CIG30AV %in% 1:7, CIG30AV, 0))

# Erstellen einer hierarchischen Kategorie für das Rauchverhalten
data2019 <- data2019 %>%
  mutate(
    Rauchstatus = case_when(
      CIG30AV == 0 ~ "Nieraucher",
      CIG30AV >= 1 ~ "Raucher"
    ),
    Täglicher_Raucher = case_when(
      CIG30AV >= 2 ~ "Tägliches Zigarettenrauchen"
    ),
    Starkes_Rauchen_10 = case_when(
      CIG30AV >= 5 ~ "Starkes Rauchen (≥ 10 Zig./Tag)"
    ),
    Starkes_Rauchen_20 = case_when(
      CIG30AV == 7 ~ "Starkes Rauchen (≥ 20 Zig./Tag)"
    )
  )

# Datensatz für die Häufigkeitsberechnung transformieren (Pivot-Format für Hierarchie)
data_long <- data2019 %>%
  select(Altersgruppe, irsex, Rauchstatus, Täglicher_Raucher, Starkes_Rauchen_10, Starkes_Rauchen_20) %>%
  pivot_longer(cols = c(Rauchstatus, Täglicher_Raucher, Starkes_Rauchen_10, Starkes_Rauchen_20),
               names_to = "Kategorie", values_to = "Status") %>%
  filter(!is.na(Status))  # Entferne nicht definierte Werte

# Berechnung der Häufigkeiten
data_summary <- data_long %>%
  group_by(Altersgruppe, irsex, Status) %>%
  summarise(Anteil = n() / nrow(data2019) * 100) %>%
  ungroup()

# Plot erstellen mit Facetten nach Altersgruppen
ggplot(data_summary, aes(x = fct_reorder(Status, Anteil), y = Anteil, fill = factor(irsex))) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  facet_wrap(~Altersgruppe) +  # Facettierung nach Altersgruppen
  labs(title = "Rauchverhalten (Zigaretten) in den letzten 30 Tagen nach Altersgruppe und Geschlecht",
       x = "",
       y = "Anteil in %",
       fill = "Geschlecht") +
  scale_fill_manual(values = c("blue", "red"), labels = c("Männlich", "Weiblich")) +
  theme_minimal()


######## 
#        Anteile stimmen fast(noch nach geschlecht anpassen) jetzt ABER beschriftung noch nicht, da es nicht >=10 usw in der Variable ist
########



# Altersgruppen definieren
data2019 <- data2019 %>%
  mutate(Altersgruppe = case_when(
    AGE2 %in% 1:6 ~ "12-17 Jahre",
    AGE2 %in% 7:10 ~ "18-25 Jahre",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(Altersgruppe))  # Entferne unpassende Alterswerte

# Alle anderen Werte in CIG30AV als 0 interpretieren (Nieraucher)
data2019 <- data2019 %>%
  mutate(CIG30AV = ifelse(CIG30AV %in% 1:7, CIG30AV, 0))

# Hierarchische Kategorien für das Rauchverhalten erstellen
data2019 <- data2019 %>%
  mutate(
    Rauchstatus = case_when(
      CIG30AV == 0 ~ "Nieraucher",
      CIG30AV >= 1 ~ "Raucher"
    ),
    Täglicher_Raucher = case_when(
      CIG30AV >= 2 ~ "Tägliches Zigarettenrauchen"
    ),
    Starkes_Rauchen_10 = case_when(
      CIG30AV >= 5 ~ "Starkes Rauchen (≥ 10 Zig./Tag)"
    ),
    Starkes_Rauchen_20 = case_when(
      CIG30AV == 7 ~ "Starkes Rauchen (≥ 20 Zig./Tag)"
    )
  )

# Daten ins Long-Format bringen
data_long <- data2019 %>%
  select(Altersgruppe, irsex, Rauchstatus, Täglicher_Raucher, Starkes_Rauchen_10, Starkes_Rauchen_20) %>%
  pivot_longer(cols = c(Rauchstatus, Täglicher_Raucher, Starkes_Rauchen_10, Starkes_Rauchen_20),
               names_to = "Kategorie", values_to = "Status") %>%
  filter(!is.na(Status))  # Entferne nicht definierte Werte

# Berechnung der Anteile **innerhalb jeder Altersgruppe**
data_summary <- data_long %>%
  group_by(Altersgruppe, irsex) %>%
  mutate(Gesamt_n = n()) %>%  # Gesamtanzahl in der Altersgruppe
  group_by(Altersgruppe, Status, irsex) %>%
  summarise(Anzahl = n(), Gesamt_n = first(Gesamt_n), .groups = "drop") %>%
  mutate(Anteil = (Anzahl / Gesamt_n) * 100) %>%  # Berechnung der Anteile für 100 % innerhalb Altersgruppe
  ungroup()

# Kategorien in gewünschter Reihenfolge für bessere Lesbarkeit
data_summary$Status <- factor(data_summary$Status, levels = c(
  "Nieraucher", "Raucher", "Tägliches Zigarettenrauchen",
  "Starkes Rauchen (≥ 10 Zig./Tag)", "Starkes Rauchen (≥ 20 Zig./Tag)"
))

# Plot erstellen mit horizontalen Balken und nebeneinanderliegenden Geschlechtergruppen
ggplot(data_summary, aes(x = Status, y = Anteil, fill = factor(irsex))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Altersgruppe) +  # Separate Plots für Altersgruppen
  labs(title = "Rauchverhalten (Zigaretten) in den letzten 30 Tagen nach Altersgruppe und Geschlecht",
       x = "",
       y = "Anteil in %",
       fill = "Geschlecht") +
  scale_fill_manual(values = c("blue", "red"), labels = c("Männlich", "Weiblich")) +
  coord_flip() +  # Horizontale Balken
  theme_minimal()

##################USA-Karte Versuch########################


###########################################################

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
plot_usmap(
  data    = state_data,
  regions = "states",
  values  = "interviews_per_100k",
  include = state_data$state_abbr  # optional: nur die definierten Staaten
) +
  scale_fill_continuous(
    low   = "lightgrey",
    high  = "black",
    name  = "Interviews \n(pro 100k)",
    label = scales::comma
  ) +
  theme(panel.background = element_blank(),
    
    legend.position = "right",
    legend.title = element_text(size = 12),   # Schriftgröße des Legendentitels
    legend.text  = element_text(size = 10),   # Schriftgröße der Legendenbeschriftungen
    legend.key.size = unit(0.8, "cm")         # Größe der Farbfelder
    
  )











