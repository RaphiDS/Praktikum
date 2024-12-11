install.packages(c("sf", "tigris", "ggplot2", "dplyr"))
library(sf)
library(tigris)
library(ggplot2)
library(dplyr)

# Create a data frame with state populations and interviews
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

# State boundaries with simplified geometry
us_states <- states(cb = TRUE)

# Filter to the 48 contiguous states + DC
state_codes <- c(state.abb, "DC")
exclude_states <- c("HI", "AK")  # Exclude Hawaii and Alaska
us_states <- us_states %>% filter(STUSPS %in% setdiff(state_codes, exclude_states))

# Match state names to spatial data
us_states <- us_states %>%
  mutate(state = tolower(NAME))  # Create lowercase column for joining

# Prepare data for joining
state_data <- state_data %>%
  mutate(state = tolower(state))  # Lowercase for consistency

# Join spatial data with relative frequency data
map_data <- us_states %>%
  left_join(state_data, by = "state")

# Create a map showing relative frequency
ggplot(map_data) +
  geom_sf(aes(fill = relative_frequency), color = "white", size = 0.2) +  # Fill states by relative frequency
  scale_fill_viridis_c(option = "plasma", direction = -1, name = "Relative Frequency") +  # Color scale
  theme_void() +  # Remove axes and gridlines
  labs(title = "Relative Frequency of Interviews by State") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20),
    plot.subtitle = element_text(hjust = 0.5, size = 14)
  )
