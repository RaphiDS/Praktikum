
# themenfrage: -	Wie hängt der Konsum im Jahr 2019 mit demographischen Merkmalen zusammen?
## datensatz: demografische Faktoren ahnschauen
load("C:/Users/sarah/OneDrive/UNI/WS 24_25/Praktikum/Datensätze/NSDUH-2019-DS0001-bndl-data-r/NSDUH_2019.RData")
data <- PUF2019_100920

library(ggplot2)
library(tidyverse)
## combine Variables to find out who was employed the last 12 Months (or self employed)
### categorized into yes,no or no answer/NA
 


general.employment <- data%>%
  select(wrkdpstyr, wrkselfem) %>%
  filter(wrkdpstyr %in% c(1,2) | wrkselfem %in% c(1,2)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  group_by(variable,value) %>%
  summarise(count = n(), .groups = 'drop')
  
general.employment

  selfemployment <- ggplot(general.employment, aes(x = as.factor(value), y = count, fill = as.factor(value)))+
  geom_bar(stat = "identity") +
    facet_wrap(~ variable)
selfemployment


issue
