########################################################################################################################
# Function to create histograms for consumption days over the last 30 days

histogram_fun <- function(data_col, drug_name, limit, colorcode, yearplot) {
  # Prepare data by counting consumption days and converting counts to proportions
  data <- drug_data %>%
    group_by(year) %>%
    count(day = .data[[data_col]]) %>%
    mutate(`Relative share` = n / sum(n)) %>%
    filter(day >= 1 & day <= 30) %>%         # Focus on days 1 to 30
    filter(year == yearplot) %>%              # Subset for the specified year
    ungroup() %>%
    mutate(
      Drug = drug_name,
      day = factor(day, levels = as.character(1:30))
    )
  
  # Create and return the histogram plot
  my_plot(
    ggplot(data, aes(x = day, y = `Relative share`)) +
      geom_col(fill = colorcode, color = "black") +
      labs(
        x = paste0("Anzahl der Konsumtage in den letzten 30 Tagen (n = ", sum(data$n), ")"),
        y = "Prozent"
      ) +
      scale_x_discrete(breaks = c("1", "5", "10", "15", "20", "25", "30"), drop = FALSE)
  ) +
    scale_y_continuous(limits = c(0, limit), labels = scales::percent_format())
}

# Generate histograms for Alcohol, Cigarettes, Cocaine, and Heroin for various years
hist_alc_15 <- histogram_fun("alcdays", "Alcohol", 0.085, "#0072B2", "2015")
hist_alc_16 <- histogram_fun("alcdays", "Alcohol", 0.085, "#0072B2", "2016")
hist_alc_17 <- histogram_fun("alcdays", "Alcohol", 0.085, "#0072B2", "2017")
hist_alc_18 <- histogram_fun("alcdays", "Alcohol", 0.085, "#0072B2", "2018")
hist_alc_19 <- histogram_fun("alcdays", "Alcohol", 0.085, "#0072B2", "2019")

hist_cig_15 <- histogram_fun("CIG30USE", "Cigarettes", 0.12, "#009E73", "2015")
hist_cig_16 <- histogram_fun("CIG30USE", "Cigarettes", 0.12, "#009E73", "2016")
hist_cig_17 <- histogram_fun("CIG30USE", "Cigarettes", 0.12, "#009E73", "2017")
hist_cig_18 <- histogram_fun("CIG30USE", "Cigarettes", 0.12, "#009E73", "2018")
hist_cig_19 <- histogram_fun("CIG30USE", "Cigarettes", 0.12, "#009E73", "2019")

hist_coc_15 <- histogram_fun("COCUS30A", "Cocaine", 0.004, "#E69F00", "2015")
hist_coc_16 <- histogram_fun("COCUS30A", "Cocaine", 0.004, "#E69F00", "2016")
hist_coc_17 <- histogram_fun("COCUS30A", "Cocaine", 0.004, "#E69F00", "2017")
hist_coc_18 <- histogram_fun("COCUS30A", "Cocaine", 0.004, "#E69F00", "2018")
hist_coc_19 <- histogram_fun("COCUS30A", "Cocaine", 0.004, "#E69F00", "2019")

hist_her_15 <- histogram_fun("HER30USE", "Heroin", 0.0006, "#CC79A7", "2015")
hist_her_16 <- histogram_fun("HER30USE", "Heroin", 0.0006, "#CC79A7", "2016")
hist_her_17 <- histogram_fun("HER30USE", "Heroin", 0.0006, "#CC79A7", "2017")
hist_her_18 <- histogram_fun("HER30USE", "Heroin", 0.0006, "#CC79A7", "2018")
hist_her_19 <- histogram_fun("HER30USE", "Heroin", 0.0006, "#CC79A7", "2019")

########################################################################################################################