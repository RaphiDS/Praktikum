
# Plot ABhängigkeit Drogen

fourdrugsdependency <- as.data.frame(
  rbind(
    everdatafun("depndalc", "Alkohol"),
    everdatafun("ndssdnsp", "Zigarette"),
    everdatafun("depndcoc", "Kokain"),
    everdatafun("depndher", "Heroin")
  )
)

Substanzen.Verlauf.Abh <- ggplot(fourdrugsdependency, aes(x = Year, y = .data[["Anteil"]],
                                                color = factor(Drug, levels = c("Alkohol", "Zigarette", "Kokain", "Heroin")),
                                                shape = factor(Drug, levels = c("Alkohol", "Zigarette", "Kokain", "Heroin")))) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  theme_light() +
  labs(
    color = "Droge",
    shape = "Droge",
    x = "Jahr",
    y = "Anteil"
  ) +
  theme(
    axis.title = element_text(size = 20),  # Achsentitel
    axis.text  = element_text(size = 20),  # Achsbeschriftungen
    legend.title = element_text(size = 20),  # Legendentitel
    legend.text =  element_text(size = 20),  # Legendentext
  ) + scale_y_continuous(limits = c(0, NA)) +
  scale_color_manual(values = c("#0072B2", "#009E73", "#E69F00", "#CC79A7")) +
  scale_shape_manual(values = c(15:18))# beliebige Form-Codes


ggsave("Presentation_files/Pres_plots/Substanzen_Verlauf_Abhängigkeit_plot.png",
       plot = Substanzen.Verlauf.Abh, width = 15, height = 8, dpi = 300)


####################################

ex.year = c(2015, 2016, 2015, 2019, 2018)
ex.cocever = c(1, 2, 1, 94, 2)
ex.COCUS30A = c(2, 94, 30, 1, 97)
ex.CATAG2 = c(1, 3, 1, 2, 2)
ex.ndssdnsp = c(0, 1, 1, 0, 0)

# Erstelle einen DataFrame
ex.data.sample <- data.frame(
  year = ex.year,
  cocever = ex.cocever,
  COCUS30A = ex.COCUS30A,
  CATAG2 = ex.CATAG2,
  ndssdnsp = ex.ndssdnsp
)

 kable(ex.data.sample)
 