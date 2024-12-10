

###new code for layout 
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




### anteil aller befragten die 2019 in behandlung sind wegen drug abuse

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
  substance = c("Cocaine/Crack", "Heroin", "Alcohol"),
  total_respondents = c(ratio_cocaine$total, ratio_heroin$total, ratio_alcohol$total),
  receiving_treatment = c(ratio_cocaine$count, ratio_heroin$count, ratio_alcohol$count),
  ratio = c(ratio_cocaine$ratio, ratio_heroin$ratio, ratio_alcohol$ratio)
)

# Load ggplot2 library
library(ggplot2)

# Create histogram to visualize the ratios
ggplot(summary_data, aes(x = substance, y = ratio, fill = substance)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Proportion of Respondents Receiving Treatment",
    x = "Substance",
    y = "Proportion Receiving Treatment"
  ) +
  theme_minimal()



###
### mentale gesundheit adult mental health


library(ggplot2)
library(dplyr)


mental_health_data <- data.frame(
  treatment_any = c(528, 42034, 13397), # Yes, No, Legitimate Skip
  facility_type = c("Psychiatric Hospital", "General Hospital Psychiatric Unit", 
                    "Medical Unit", "Other Hospital", "Residential Treatment", "Other Facility"),
  nights_psych_hospital = c(134, 6),
  nights_general_psych_unit = c(152, 1),
  nights_medical_unit = c(109),
  nights_other_hospital = c(46)
)

# Summarize treatment-seeking behavior
treatment_summary <- data.frame(
  Category = c("Stayed Overnight for MH Treatment", 
               "Did Not Stay Overnight", "Legitimately Skipped"),
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
    title = "Distribution of Facilities Used for Mental Health Treatment",
    x = "Facility Type",
    y = "Number of Respondents",
    fill = "Facility Type"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")




####
#### ???
library(ggplot2)
library(dplyr)

# Create the data based on the user-provided information
mental_health_treatment_data <- data.frame(
  TreatmentType = c(
    "Inpatient Only", 
    "Outpatient Only", 
    "Prescription Medication Only", 
    "Inpatient and Outpatient Only", 
    "Inpatient and Prescription Medication Only", 
    "Outpatient and Prescription Medication Only", 
    "Inpatient, Outpatient, and Prescription Medication"
  ),
  Frequency = c(132, 1366, 3212, 42, 82, 2375, 256)
)

# Create a simple bar plot
ggplot(mental_health_treatment_data, aes(x = TreatmentType, y = Frequency, fill = TreatmentType)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(
    title = "Frequency of Mental Health Treatment Types (Excluding No Treatment)",
    x = "Type of Treatment",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
