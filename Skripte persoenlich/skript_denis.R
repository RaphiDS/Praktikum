load("C:/Users/denis/OneDrive/Desktop/RStudio/locales/StatPrak-Overdose/Daten bearbeitet/combi_redu_data.Rdata")

drugusedata <- allfilterdata
library(tidyverse)
library(checkmate)
str(drugusedata)
summary(drugusedata)


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
  
#NOCH NICHT WIRKLICH SINNVOLL Probiere es mit häufigkeit vllt noch über farbe 
# Scatterplots for the number of days people smoked cigarettes in the past 30 days (2015-2019)
drugusedata %>%
  filter(CGR30USE >= 1 & CIG30USE <= 30) %>%
  ggplot(aes(x = year, y = CIG30USE)) +
  geom_point(position = position_jitter(width = 0.2, height = 0.2), 
             alpha = 0.5, color = "steelblue") +
  facet_wrap(~ year, nrow = 1, scales = "free_x") +
  labs(
    title = "number of days people smoked cigaretts in the Past 30 Days (2015-2019)",
    x = "Year",
    y = "Number of Days Smoked"
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



# CIG30USE HOW MANY DAYS SMOKED CIG IN PAST 30 DAYS
# CGR30USE HOW MANY DAYS SMOKED CIGAR IN PAST 30 DAYS
# ALCUS30D ist für #Drinks in den letzten 30 Tagen  
# AL30EST ist für #Tagen an denen min. ein Drink in den letzten 30 Tagen
# COCUS30A ist für #Tage an denen Kokain in den letzten 30 Tagen
# CC30EST BEST ESTIMATE # DAYS USED COCAINE PAST 30 DAYS
# HER30USE # DAYS USED HEROIN PAST 30 DAYS
# HR30EST BEST EST. # DAYS USED HEROIN PAST 30 DAYS



