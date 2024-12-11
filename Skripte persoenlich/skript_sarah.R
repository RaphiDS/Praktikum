
# themenfrage: -	Wie hängt der Konsum im Jahr 2019 mit demographischen Merkmalen zusammen?
## datensatz: demografische Faktoren ahnschauen

load("C:/Users/sarah/OneDrive/UNI/WS 24_25/Praktikum/Datensätze/NSDUH-2019-DS0001-bndl-data-r/NSDUH_2019.RData")
data2019 <- PUF2019_100920                   # loading data set

## installing rquired packages

library(ggplot2)
library(tidyverse)

## write function to filter for data --> type of yes/no/missing/no response
filter_columns_dplyr <- function(data.set, columns_to_keep) {
  data.set %>%  
    #select the variable
    select(all_of(columns_to_keep)) %>%  
    
    # shift the table to adjust values to fit a barplot
    pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>% 
    
    # group values together
    group_by(variable) 
}
filter_columns_dplyr(data2019, "wrkselfem")

## function to create barplot

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

## combine Variables to find out who was employed the last 12 Months (or self employed)
### categorized into yes,no or no answer/NA

general.employment <- data2019%>%
  select(wrkdpstyr, wrkselfem) %>%                          #select needed variables
  filter(wrkdpstyr %in% c(1,2) | wrkselfem %in% c(1,2)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>% #rearange table to fit values for barplot
  group_by(variable,value) %>%
  summarise(count = n(), .groups = 'drop')            # group to find summarized values for the bar
  
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

## college enrollment(people aged 18-22, enrolled in School and College)
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



## AIA (indian) segments, general racial background, alcohol in these regions
Racial.Background <- data2019 %>%
  select (MAIIN102,NEWRACE2, sexrace, eduhighcat) ## selected AI regions, racial background and education level


Racial.Background$labels <- paste(as.character(Racial.Background$NEWRACE2)) ## trying to change label of plot

## Plot for Eudcation level dependend on race

ggplot(Racial.Background, aes(x = eduhighcat))+
  geom_bar()+
  facet_wrap(~NEWRACE2)+
  theme_minimal()+
  labs( x = Racial.Background$labels)

## Better: Bocplot
ggplot(Racial.Background, aes(x = NEWRACE2, y = eduhighcat))+
  geom_boxplot(aes(group = NEWRACE2, fill = as.factor(NEWRACE2)))



## kovarianz: filter arbeitslos und kokain/ mariuana/alcohol /cigarette

substanceUse.Work <- data2019 %>%
  select(wrkdpstyr, wrkselfem, cocrec, crakrec, herrec) %>%
  filter(
    wrkdpstyr %in% c(1, 2) | wrkselfem %in% c(1, 2),          # Check if wrkdpstyr or wrkselfem equals 1 or 2
    cocrec %in% c(1, 2,91) & herrec %in% c(1, 2, 91) # Check if cocrec, crakrec, and herrec have valid values
  ) %>%
  mutate(
    employed = if_else(wrkdpstyr == 1 | wrkselfem == 1, 1, 2),  # Create "employed" column
    drug = case_when(                                         # Create "drug" column with multiple conditions
      cocrec == 91 & herrec == 1 ~ "heroin",
      cocrec %in% c(1, 2) & herrec %in% c(1, 2) ~ "both",
      cocrec %in% c(1, 2) ~ "cocain",
      TRUE ~ NA_character_                                    # Assign NA for rows that do not match any condition
    )
  ) %>%
  filter(is.na(drug) == FALSE) %>%
  group_by(drug)

  substanceUse.Work
  
  ggplot(substanceUse.Work, aes(x = employed)) +
    geom_bar(aes(fill = as.factor(drug)),position = "dodge")
    
