##########################
# Installation & Laden der benötigten Pakete
##########################

install.packages("tidyverse")
library(tidyverse)

##########################
# Laden der Daten
##########################

# Laden einer vorbereiteten Rdata-Datei.
# Enthalten ist ein Datensatz "allfilterdata", den wir zusätzlich als "drugdata" abspeichern.
load("Daten bearbeitet/combi_redu_data.Rdata")
drugdata <- allfilterdata

##########################
# Funktionen zur Datenvorbereitung und Auswertung
#############################
# Installation & Loading of Required Packages
##########################

install.packages("tidyverse")
library(tidyverse)

##########################
# Loading the Data
##########################

# Loads a prepared Rdata file.
# It contains a dataset called "allfilterdata", which we also store as "drugdata".
load("Daten bearbeitet/combi_redu_data.Rdata")
drugdata <- allfilterdata

##########################
# Functions for Data Preparation and Analysis
##########################

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

##########################
# Example: Creating Combined Data Frames for Several Drugs
##########################

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
ggplot(fourdrugsever, aes(x = Year, y = .data[["Rel. share"]], color = Drug)) +
  geom_point() +
  geom_line() +
  scale_color_brewer(palette = "Set1") +  # More vibrant color palette
  theme_light() +
  labs(
    title = "Relative share of people who have ever used certain drugs",
    x = "Year",
    y = "Relative Share"
  )

# 2) Plot: "In the last 30 days" – 4 major drugs
ggplot(fourdrugs30, aes(x = Year, y = .data[["Rel. share"]], color = Drug)) +
  geom_point() +
  geom_line() +
  scale_color_brewer(palette = "Set1") +
  theme_light() +
  labs(
    title = "Relative share of people who have used certain drugs in the last 30 days",
    x = "Year",
    y = "Relative Share"
  )

# 3) Plot: "Ever used" – tobacco products
ggplot(tobaccoever, aes(x = Year, y = .data[["Rel. share"]], color = Drug)) +
  geom_point() +
  geom_line() +
  scale_color_brewer(palette = "Set1") +
  theme_light() +
  labs(
    title = "Relative share of people who have ever used certain forms of tobacco",
    x = "Year",
    y = "Relative Share"
  )

# 4) Plot: "In the last 30 days" – tobacco products
ggplot(tobacco30, aes(x = Year, y = .data[["Rel. share"]], color = Drug)) +
  geom_point() +
  geom_line() +
  scale_color_brewer(palette = "Set1") +
  theme_light() +
  labs(
    title = "Relative share of people who have used certain forms of tobacco in the last 30 days",
    x = "Year",
    y = "Relative Share"
  )

# Note: You could adjust y-axis scales (ylim) if needed.

##########################
# Bar Charts: Function to Plot Single Drugs
##########################

graphfun1 <- function(drug, question, ymax) {
  drug %>%
    ggplot(aes(x = Year, y = .data[["Rel. share"]])) +
    geom_line() +
    geom_point() +
    scale_color_brewer(palette = "Set1", aesthetics = "color") +  # Vibrant color palette, relevant if color mapping is used
    ggtitle(question) +                                           # The question is passed as the title
    theme_light() +
    ylim(0, ymax) +
    labs(
      x = "Year",
      y = "Relative Share"
    )
}

# Example calls for single drugs
graphfun1(
  everdatafun("cigever", "Cigarettes"),
  "Have you ever smoked part or all of a cigarette?",
  0.6
)
graphfun1(
  everdatafun("alcever", "Alcohol"),
  "Have you ever, even once, had a drink of any type of alcoholic beverage?\nPlease do not include times when you only had a sip or two from a drink.",
  0.8
)
graphfun1(
  everdatafun("herever", "Heroin"),
  "Have you ever, even once, used heroin?",
  0.025
)
graphfun1(
  everdatafun("cocever", "Cocaine"),
  "Have you ever, even once, used any form of cocaine?",
  0.15
)

##########################
# Histograms: Example with Cigars (e.g., number of smoked days)
##########################

# Example: A dataset for cigars, filtering out code values like 91, 93, 94, 97, 98
cigarettesclean <- drugdata %>%
  mutate(
    CGR30USE = ifelse(CGR30USE %in% c(91, 93, 94, 97, 98), NA, CGR30USE)
  ) %>%
  filter(!is.na(CGR30USE)) %>%
  filter(CGR30USE > 0, CGR30USE <= 30)

# Histogram of smoked days, grouped by year
cigarettesclean %>%
  ggplot(aes(x = CGR30USE)) +
  geom_histogram(
    aes(y = after_stat(count / sum(count))),
    breaks = seq(0.5, 30.5, by = 1),
    color = "black",
    na.rm = TRUE
  ) +
  facet_wrap(~ year, nrow = 1, scales = "free_y") +
  scale_x_continuous(breaks = seq(5, 30, by = 5), limits = c(1, 30)) +
  ylim(0, 0.035) +
  labs(
    title = "Relative frequencies of smoking days (2015-2019)",
    x = "Number of smoking days",
    y = "Relative Frequency"
  ) +
  theme_light()

# A similar histogram, more straightforward (cigars per year)
drugdata %>%
  filter(CGR30USE >= 1 & CGR30USE <= 30) %>%
  ggplot(aes(x = CGR30USE)) +
  geom_histogram(binwidth = 1, color = "black") +
  facet_wrap(~ year, nrow = 1, scales = "free_y") +
  labs(
    title = "Histograms of the number of days cigars were smoked (2015-2019)",
    x = "Number of days smoked",
    y = "Frequency"
  ) +
  theme_light()

##########################
# Function: Calculate Average Usage Days per Year (including non-users = 0)
##########################

# Helper function: Prepares the dataset
prepare_data <- function(datacol, drug_name) {
  allfilterdata %>%
    mutate(
      UsageDays = case_when(
        .data[[datacol]] >= 1 & .data[[datacol]] <= 30 ~ .data[[datacol]],
        TRUE ~ 0
      ),
      Drug = drug_name
    ) %>%
    select(year, Drug, UsageDays)
}

# Average usage days per drug
avg_data_fun <- function(datacol, drug_name) {
  prepare_data(datacol, drug_name) %>%
    group_by(year, Drug) %>%
    summarize(avg_days = mean(UsageDays, na.rm = TRUE), .groups = "drop")
}

# Example: Drugs with average usage days per year
her30_avg_data <- avg_data_fun("HER30USE", "Heroin")
coc30_avg_data <- avg_data_fun("COCUS30A", "Cocaine")
alc30_avg_data <- avg_data_fun("alcdays", "Alcohol")
cig30_avg_data <- avg_data_fun("CIG30USE", "Cigarettes")

# Combine into one dataframe
combined_avg_usage <- bind_rows(
  her30_avg_data,
  coc30_avg_data,
  alc30_avg_data,
  cig30_avg_data
)

# Plot: Average usage days (including 0 for non-users)
ggplot(combined_avg_usage, aes(x = year, y = avg_days, color = Drug)) +
  geom_point() +
  geom_line() +
  scale_color_brewer(palette = "Set1") +
  theme_light() +
  labs(
    title = "Average number of usage days (including 0 for non-users)",
    x = "Year",
    y = "Average Usage Days"
  )

##########################
# Function: Histogram of Usage Days (1–30)
##########################

histogram_fun <- function(datacol, drug_name) {
  data <- drugdata %>%
    group_by(year) %>%
    count(day = .data[[datacol]]) %>%
    mutate("Relative share" = n / sum(n)) %>%
    filter(day >= 1 & day <= 30) %>%
    ungroup() %>%
    mutate(Drug = drug_name)
  
  ggplot(data, aes(x = factor(day), y = `Relative share`)) +
    geom_col(fill = "darkgrey", color = "black", alpha = 0.7) +
    facet_wrap(~ year) +
    theme_light() +
    labs(
      title = paste0("Distribution of usage days for ", drug_name),
      x = "Number of Days Used in Last 30 Days",
      y = "Relative Share"
    )
}

# Example calls
histogram_fun("CIG30USE", "Cigarettes")
histogram_fun("alcdays", "Alcohol")
histogram_fun("COCUS30A", "Cocaine")
histogram_fun("HER30USE", "Heroin")

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
      main = paste("Correlation between", varname1, "and", varname2)
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
      main = paste("Correlation between", varname1, "and", varname2, "(Last 30 Days)")
    )
}

# Example call: Mosaic plot for the last 30 days
mosaicfun30("CIG30USE", "COCUS30A", "Cigarettes", "Cocaine")
