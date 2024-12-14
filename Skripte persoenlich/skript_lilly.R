## create new, cleaned dataset for 2019 for everyone to use
data2019 <- allfilterdata %>%
  filter (year == 2019)
n <- 56136

library(dplyr)

# KARTE USA ABSOLUTE HÄUFIGKEIT VON ERHOBENEN INTERVIEWS PRO STAAT
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

# Calculate relative frequency
state_data <- state_data %>%
  mutate(relative_frequency = interviews / population)

# View the data
state_data

###US MAP ONLY INTERVIEWS ABSOLUT
install.packages(c("sf", "tigris", "ggplot2", "dplyr"))
library(sf)
library(tigris)
library(ggplot2)
library(dplyr)

interviews_data <- data.frame(
  state = c(
    "California", "Florida", "New York", "Texas",
    "Illinois", "Michigan", "Ohio", "Pennsylvania",
    "Georgia", "New Jersey", "North Carolina", "Virginia",
    "Hawaii", rep("Other", 37)  # 37 other states + DC
  ),
  interviews = c(
    4560, 3300, 3300, 3300,  # High-tier states
    2400, 2400, 2400, 2400,  # Medium-tier states
    1500, 1500, 1500, 1500,  # Low-tier states
    967, rep(960, 37)        # Hawaii and the rest
  )
)

# Expand the "Other" category into individual states
other_states <- setdiff(c(state.name, "District of Columbia"), interviews_data$state)
expanded_data <- data.frame(
  state = c(interviews_data$state[interviews_data$state != "Other"], other_states),
  interviews = c(
    interviews_data$interviews[interviews_data$state != "Other"], 
    rep(mean(interviews_data$interviews[interviews_data$state == "Other"]), length(other_states))
  )
)

#consistent formatting for state names
expanded_data$state <- tolower(expanded_data$state)  # Lowercase for joining

#state boundaries with simplified geometry
us_states <- states(cb = TRUE)

# Filter to the 48 contiguous states + DC
state_codes <- c(state.abb, "DC")
exclude_states <- c("HI", "AK")  # Exclude Hawaii and Alaska
us_states <- us_states %>% filter(STUSPS %in% setdiff(state_codes, exclude_states))

# Match state names to spatial data
us_states <- us_states %>%
  mutate(state = tolower(NAME))  # Create lowercase column for joining

# Join spatial data with interviews data
map_data <- us_states %>%
  left_join(expanded_data, by = "state")


ggplot(map_data) +
  geom_sf(aes(fill = interviews), color = "white", size = 0.2) +  # Fill states by interviews
  scale_fill_viridis_c(option = "plasma", direction = -1, name = "Total Interviews") +  # Color scale
  theme_void() +  # Remove axes and gridlines
  labs(title = "Number of Interviews by State") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20),
    plot.subtitle = element_text(hjust = 0.5, size = 14)
  )

ggsave("interviews_by_state.png", width = 10, height = 6, dpi = 300)




### ANTEIL ALLER BEFRAGTEN DIE 2019 IN BEHANDLUNG SIND ODER WAREN WEGEN DROGENMISSBRAUCH

# Function to calculate treatment ratio
treatment_ratio <- function(data, treatment_responses) {
  total_respondents <- sum(data$frequency)
  treatment_count <- sum(data$frequency[data$response %in% treatment_responses])
  ratio <- treatment_count / total_respondents
  return(list(total = total_respondents, count = treatment_count, ratio = ratio))
}

# Data for cocaine/crack
data_cocaine <- data.frame(
  response = c(1, 2, 3, 6, 85, 91, 94, 97, 98, 99),
  frequency = c(127, 367, 4, 375, 5, 13270, 4, 91, 848, 41045),
  percentage = c(0.23, 0.65, 0.01, 0.67, 0.01, 23.64, 0.01, 0.16, 1.51, 73.12)
)

# Data for heroin
data_heroin <- data.frame(
  response = c(1, 2, 3, 6, 85, 91, 94, 97, 98, 99),
  frequency = c(155, 80, 1, 665, 3, 13270, 1, 92, 824, 41045),
  percentage = c(0.28, 0.14, 0.00, 1.18, 0.01, 23.64, 0.00, 0.16, 1.47, 73.12)
)

# Data for alcohol
data_alcohol <- data.frame(
  response = c(1, 2, 3, 5, 6, 85, 89, 91, 94, 97, 98, 99),
  frequency = c(399, 336, 2, 48, 21, 8, 1, 13270, 5, 93, 909, 41044),
  percentage = c(0.71, 0.60, 0.00, 0.09, 0.04, 0.01, 0.00, 23.64, 0.01, 0.17, 1.62, 73.12)
)

# Define responses indicating receiving treatment
treatment_responses <- c(1, 3, 5)

# Calculate ratios for each dataset
ratio_cocaine <- treatment_ratio(data_cocaine, treatment_responses)
ratio_heroin <- treatment_ratio(data_heroin, treatment_responses)
ratio_alcohol <- treatment_ratio(data_alcohol, treatment_responses)

# Create a summary data frame for visualization
summary_data <- data.frame(
  Substanz = c("Kokain", "Heroin", "Alkohol"),
  total_respondents = c(ratio_cocaine$total, ratio_heroin$total, ratio_alcohol$total),
  receiving_treatment = c(ratio_cocaine$count, ratio_heroin$count, ratio_alcohol$count),
  ratio = c(ratio_cocaine$ratio, ratio_heroin$ratio, ratio_alcohol$ratio)
)

# Load ggplot2 library
library(ggplot2)

# Create histogram to visualize the ratios
ggplot(summary_data, aes(x = Substanz, y = ratio, fill = Substanz)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "In Behandlung",
    x = "Substanz",
    y = "Anteil der Behandelten"
  ) +
  theme_minimal()



### WO WIRD MAN BEHANDELT
library(ggplot2)
library(dplyr)


mental_health_data <- data.frame(
  treatment_any = c(528, 42034, 13397), # Yes, No, Legitimate Skip
  facility_type = c("Psychiatrische Klinik", "Psychiatrische Abteilung im Krankenhaus",
                    "Medizinische Abteilung", "Anderes Krankenhaus", "Stationäre Behandlung",
                    "Andere Einrichtung"),
  nights_psych_hospital = c(134, 6),
  nights_general_psych_unit = c(152, 1),
  nights_medical_unit = c(109),
  nights_other_hospital = c(46)
)

# Summarize treatment-seeking behavior
treatment_summary <- data.frame(
  Category = c("Übernacht wegen Behandlung",
               "Nicht übernachtet", "Skip"),
  Count = c(528, 42034, 13397)
)

# Facility-specific treatment data
facility_data <- data.frame(
  Facility = mental_health_data$facility_type,
  Count = c(141, 157, 114, 47, 44, 63)
)

# Plotting the distribution of facilities used
ggplot(facility_data, aes(x = reorder(Facility, -Count), y = Count, fill = Facility)) +
  geom_bar(stat = "identity", color = "black", show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Verteilung der Einrichtungen für Behandlung",
    x = "Art",
    y = "Anzahl Befragter",
    fill = "Art"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")


#### ART DER BEHANDLUNG BZW WO WIRD BEHANDELT
library(ggplot2)
library(dplyr)
library(scales)

# Original data including those who did not receive treatment
treatment_data <- data.frame(
  TreatmentType = c(
    "Stationär", 
    "Ambulant", 
    "verschreibungspflichtige Medikamente", 
    "Stationär und Ambulant", 
    "Stationär und verschreibungspflichtige Medikamente", 
    "Ambulant und verschreibungspflichtige Mediakmente", 
    "Stationär, Ambulant und verschreibungspflichtge Medikamente",
    "Did Not Receive Treatment"
  ),
  Frequency = c(132, 1366, 3212, 42, 82, 2375, 256, 34934)
)

# Exclude those who did not receive treatment
treatment_data_filtered <- subset(treatment_data, TreatmentType != "Did Not Receive Treatment")

# Calculate total frequency of those who received treatment
total_freq <- sum(treatment_data_filtered$Frequency)

# Calculate relative frequencies
treatment_data_filtered$RelativeFrequency <- treatment_data_filtered$Frequency / total_freq

# Create a bar plot of relative frequencies, sorted in ascending order
ggplot(treatment_data_filtered, aes(x = reorder(TreatmentType, RelativeFrequency),
                                    y = RelativeFrequency, fill = TreatmentType)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Behandlungsarten für psychische Gesundheit",
    x = "Behandlungsart",
    y = "Relative Häufigkeit"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

### WER IST IN BEHANDLUNG FÜR MENTALE GESUNDHEIT UND WARUM NACH ALTER
install.packages(c("sf", "tigris", "ggplot2", "dplyr", "cowplot"))
library(sf)
library(tigris)
library(ggplot2)
library(dplyr)
library(cowplot)

# Data for mental health treatment prompts
prompt_data <- data.frame(
  prompt = c(
    "Selbstentschieden", "Von anderen vorgeschlagen", "Gerichtlich veranlasst"
  ),
  frequency = c(8292, 1086, 384)
)

# Adjust frequencies to exclude irrelevant categories while maintaining ratios
valid_responses <- sum(prompt_data$frequency)
response_ratio <- valid_responses / 56136

# Add total interviews and age group proportions
total_interviewed <- 56136
age_group_proportions <- data.frame(
  age_group = c("Jugendlich (12-17)", "Junge Erwachsene (18-25)", 
                "Erwachsen (26-34)", "Erwachsen (35-49)", "Erwachsen (50+)"),
  proportion = c(0.25, 0.25, 0.15, 0.20, 0.15)
)

# Calculate the number of people in each age group
age_group_proportions <- age_group_proportions %>%
  mutate(total_people = proportion * total_interviewed * response_ratio)

# Calculate relative frequency of each prompt by age group
prompt_by_age <- expand.grid(prompt = prompt_data$prompt, age_group = age_group_proportions$age_group) %>%
  left_join(prompt_data, by = "prompt") %>%
  left_join(age_group_proportions, by = "age_group") %>%
  mutate(
    relative_frequency = frequency / total_people
  )

# Plot relative frequencies by age group
ggplot(prompt_by_age, aes(x = factor(age_group, levels = c("Jugendlich (12-17)", 
                                                           "Junge Erwachsene (18-25)", 
                                                           "Erwachsen (26-34)", 
                                                           "Erwachsen (35-49)", 
                                                           "Erwachsen (50+)")), 
                          y = relative_frequency, fill = prompt)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_viridis_d(name = "Grund") +
  labs(
    title = "In Behandlung für Mentale Gesundheit",
    x = "Altersgruppe",
    y = "Antwort"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 10),
    panel.background = element_rect(fill = "white", colour = "grey"),
    panel.grid.major = element_line(colour = "lightgrey"),
    panel.grid.minor = element_line(colour = "lightgrey", linetype = "dotted")
  )


