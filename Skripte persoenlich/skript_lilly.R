install.packages(c("sf", "tigris", "ggplot2", "dplyr"))
library(sf)
library(tigris)
library(ggplot2)
library(dplyr)

# Create a data frame of interviews by state
interviews_data <- data.frame(
  state = c(
    "California", "Florida", "New York", "Texas",
    "Illinois", "Michigan", "Ohio", "Pennsylvania",
    "Georgia", "New Jersey", "North Carolina", "Virginia",
    "Hawaii", rep("Other", 38)  # 37 other states + DC
  ),
  interviews = c(
    4560, 3300, 3300, 3300,  # High-tier states
    2400, 2400, 2400, 2400,  # Medium-tier states
    1500, 1500, 1500, 1500,  # Low-tier states
    967, rep(960, 38)        # Hawaii and the rest
  )
)

# Expand the "Other" category into individual states
other_states <- setdiff(c(state.name, "District of Columbia"), interviews_data$state)
expanded_data <- data.frame(
  state = c(interviews_data$state, other_states),
  interviews = c(interviews_data$interviews, rep(960, length(other_states)))
)

# Ensure consistent formatting for state names
expanded_data$state <- tolower(expanded_data$state)  # Lowercase for joining

# Load state boundaries with simplified geometry
us_states <- states(cb = TRUE)

# Filter to the 50 states + DC
state_codes <- c(state.abb, "DC")
us_states <- us_states %>% filter(STUSPS %in% state_codes)

# Match state names to our data
us_states <- us_states %>%
  mutate(state = tolower(NAME))  # Create lowercase column for joining

# Join spatial data with interviews data
map_data <- us_states %>%
  left_join(expanded_data, by = "state")

# Plot the map
ggplot(map_data) +
  geom_sf(aes(fill = interviews), color = "white", size = 0.2) +  # Fill states by interviews
  scale_fill_viridis_c(option = "plasma", direction = -1, name = "total") +  # Color scale
  theme_void() +  # Remove axes and gridlines
  labs(title = "Number of Interviews by State") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20),
    plot.subtitle = element_text(hjust = 0.5, size = 14)
  )


ggsave("interviews_by_state.png", width = 10, height = 6, dpi = 300)




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



