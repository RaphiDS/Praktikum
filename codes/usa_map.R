########################################################################################################################
# Create a US states data frame with population and interview counts for mapping

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

# Calculate interviews per 100k population for mapping purposes
state_data <- state_data %>%
  mutate(interviews_per_100k = (interviews / population) * 100000)

# Generate a US map plot using the usmap package, coloring states by interviews per 100k
usa_map <- plot_usmap(
  data = state_data,
  regions = "states",
  values = "interviews_per_100k"
) +
  scale_fill_continuous(
    low = "grey90",
    high = "grey0",
    name = "Umfragen\n(pro 100k)"
  ) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 25),
    legend.text = element_text(size = 25),
    legend.key.height = unit(1.5, units = "cm"),
    legend.key.width = unit(0.8, units = "cm")
  )
ggsave("presentation_files/plots/usa_map.png", plot = usa_map, width = 18, height = 11, dpi = 300)
########################################################################################################################