
# themenfrage: -	Wie hängt der Konsum im Jahr 2019 mit demographischen Merkmalen zusammen?
## datensatz: demografische Faktoren ahnschauen

## installing rquired packages
library(ggplot2)
library(tidyverse)

## create new, cleaned dataset for 2019 for everyone to use
data2019 <- allfilterdata %>%
  filter (year == 2019)
n <- 56136

#--------------------------------------------------------------------------------------------------
## Age of Survey People
age <- data2019 %>%
  select(AGE2)


#------------------------------------------------------------------------------------------------------
## combine Variables to find out who was employed the last 12 Months (or self employed)
### categorized into yes,no or no answer/NA

general.employment <- data2019%>%
  select(wrkdpstyr, wrkselfem) %>%                          #select needed variables
  filter(wrkdpstyr %in% c(1,2) | wrkselfem %in% c(1,2)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>% #rearange table to fit values for barplot
  group_by(variable,value) %>%
  summarise(count = n(), .groups = 'drop') %>%          # group to find summarized values for the bar
  filter (value < 99)                                # getting rid of NAs --> NOT working
  
general.employment

# graph
employment.bar <- ggplot(general.employment, aes(x = as.factor(value), y = count, fill = as.factor(value)))+
  geom_bar(stat = "identity") +
  facet_wrap(~ variable)

employment.bar

## BUT in the Skript they use antoher Variable

imputed.employment18 <- data2019 %>%
  select (IRWRKSTAT18) %>%
  filter (IRWRKSTAT18 < 99)
ggplot(imputed.employment18, aes(x = IRWRKSTAT18, fill = factor(IRWRKSTAT18))) +
  geom_bar()+
  labs(title = "Employment status of People 18+")

#----------------------------------------------------------------------------------------------------
## represent number of people who get health care for substance abuse
# categorize into yes(1), no(2), don't know (94), skip (prvhlthin = 2 --> no private insurance!, 97/98)
insurance.substance <- data2019 %>%
  select(hltinalc,hltindrg)%>%
  filter(hltinalc < 94 | hltindrg < 94) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "values") 

## plot comparing the values
ggplot(insurance.substance, aes(x = as.factor (values), fill = as.factor(values)))+
  geom_bar()+
  facet_wrap( ~ variable)+
  ggtitle("Drug and alcohol abuse covered by private Insurance")+ 
  labs( fill = " Insurance status", x = "Status")


#------------------------------------------------------------------------------------------------------
## college enrollment(people aged 18-2count## college enrollment(people aged 18-22, enrolled in School and College)
college.enrollment <- data2019 %>%
  select(collenrlst) %>%
  pivot_longer(cols = everything(), names_to = "status", values_to = "value") %>%
  group_by(status)

#create label for the scale

college.enrollment
#graph
ggplot(college.enrollment, aes( x = value, fill = as.factor(value)))+
  geom_bar()+
  ggtitle ("college enrollment status")+
  labs(fill = "status")+
  theme_minimal()


#------------------------------------------------------------------------------------------------------
## AIA (indian) segments, general racial background, alcohol in these regions
Racial.Background <- data2019 %>%
  select (NEWRACE2, eduhighcat) ## selected AI regions, racial background and education level
  

## Plot for Eudcation level dependend on race

ggplot(Racial.Background, aes(x = NEWRACE2, fill = factor(eduhighcat)))+
  geom_bar(position = "fill")+
  scale_y_continuous(labels = scales::percent) +
  theme_light()+
  labs(title = "Education achieved by each Race")
## Verhältnisse -- und wert 5 streichen? --> Leute unter 17 also kein Abschluss, zählen nicht



## maybe better: Ireduhighst2: genauer Klassenabschluss also 5,6,7 ... college und weiter
Racial.Background2 <- data2019 %>%
  select(NEWRACE2, IREDUHIGHST2) 
ggplot(Racial.Background2, aes(x = NEWRACE2, fill = factor(IREDUHIGHST2)))+
  geom_bar(position = "fill")+
  theme_light()+
  scale_y_continuous(labels = scales::percent)+ 
  labs(title = "Class finished by each Race")
