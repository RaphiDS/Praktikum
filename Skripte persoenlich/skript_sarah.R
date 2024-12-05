
# themenfrage: -	Wie hängt der Konsum im Jahr 2019 mit demographischen Merkmalen zusammen?
## datensatz: demografische Faktoren ahnschauen

load("C:/Users/sarah/OneDrive/UNI/WS 24_25/Praktikum/Datensätze/NSDUH-2019-DS0001-bndl-data-r/NSDUH_2019.RData")
data2019 <- PUF2019_100920

library(ggplot2)
library(tidyverse)


## combine Variables to find out who was employed the last 12 Months (or self employed)
### categorized into yes,no or no answer/NA

general.employment <- data2019%>%
  select(wrkdpstyr, wrkselfem) %>%
  filter(wrkdpstyr %in% c(1,2) | wrkselfem %in% c(1,2)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  group_by(variable,value) %>%
  summarise(count = n(), .groups = 'drop')
  
general.employment

# graph
employment.bar <- ggplot(general.employment, aes(x = as.factor(value), y = count, fill = as.factor(value)))+
  geom_bar(stat = "identity") +
    facet_wrap(~ variable)
employment.bar

## represent number of people who get health care for substance abuse
# categorize into yes(1), no(2), don't know (94), skip (prvhlthin = 2 --> no private insurance!, 97/98)
insurance.substance <- data2019 %>%
  select(hltinalc,hltindrg)%>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "values")%>%
  group_by(variable)
 ## plot comparing the values
ggplot(insurance.substance, aes(x = as.factor (values), fill = as.factor(values)))+
  geom_bar()+
  facet_wrap( ~ variable)+
  ggtitle("Drug and alcohol abuse covered by private Insurance")+ 
  labs( fill = " Insurance status", x = "Status")

## college enrollemnt(people aged 18-22, enrolled in School and College)
college.enrollment <- data2019 %>%
  select(collenrlst) %>%
  pivot_longer(cols = everything(), names_to = "status", values_to = "value") %>%
  group_by(status)

college.enrollment
#graph
ggplot(college.enrollment, aes( x = value, fill = as.factor(value)))+
  geom_bar()+
  ggtitle ("college enrollment")+
  labs(fill = "status")+
  theme_minimal()

## write function to filter for data --> type of yes/no/missing/no response
filter_columns_dplyr <- function(data.set, columns_to_keep) {
  data.set %>%
    select(all_of(columns_to_keep)) %>%
    pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
    group_by(variable)
}
filter_columns_dplyr(data2019, "wrkselfem")

create_barplot <- function(data, x_var, y_var = NULL, fill_var = NULL, title = "Bar Plot") {
  # Assertions for input validation
  assert(is.data.frame(data), "data must be a data frame")
  assert(x_var %in% names(data), paste0("x_var '", x_var, "' must be a column in the data frame"))
  if (!is.null(y_var)) assert(y_var %in% names(data), paste0("y_var '", y_var, "' must be a column in the data frame"))
  if (!is.null(fill_var)) assert(fill_var %in% names(data), paste0("fill_var '", fill_var, "' must be a column in the data frame"))
  
  # Base ggplot object
  if (is.null(y_var)) {
    # Bar plot for counts
    p <- ggplot(data, aes_string(x = x_var, fill = fill_var)) +
      geom_bar()
  } else {
    # Bar plot for specific y values
    p <- ggplot(data, aes_string(x = x_var, y = y_var, fill = fill_var)) +
      geom_bar(stat = "identity")
  }
  
  # Add title and theme
  p <- p + labs(title = title, x = x_var, y = ifelse(is.null(y_var), "Count", y_var)) +
    theme_minimal()
  
  # Return the plot
  return(p)
}


