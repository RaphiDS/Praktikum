
# themenfrage: -	Wie hängt der Konsum im Jahr 2019 mit demographischen Merkmalen zusammen?
## datensatz: demografische Faktoren ahnschauen

## installing rquired packages
library(ggplot2)
library(tidyverse)

## create new, cleaned dataset for 2019 for everyone to use
data2019 <- allfilterdata %>%
  filter (year == 2019)


allfilterdata[COLLENRLST]

#------------------------------------------------------------------------------------------------------
## AGE grouped into allocated Categories of NSDUH
age.grouped <- data2019 %>%
  select(catage) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "group") %>%
  group_by(group) %>%
  summarize(count =n()/56136)

  
ggplot(age.grouped, aes(x= group,y = count, fill = factor(group)))+
  geom_col()+
  scale_y_continuous(labels = scales::percent)

# age in propertion
age.ratio.levels <- data2019 %>%
  select(CATAG2)%>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "group") %>%
  group_by(group) %>%
  summarize(count =n()/56136)

ggplot(age.ratio.levels, aes(x = group, y = count, fill = factor(group)))+
  geom_col()+
  scale_y_continuous(labels = scales::percent)

## ages properly distinct
true.age <- data2019 %>%
  select(CATAG7) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "group") %>%
  group_by(group) %>%
  summarize(count =n()/56136)

ggplot(true.age, aes(x = group, y = count, fill = factor(group)))+
  geom_col()+
  scale_y_continuous(labels = scales::percent) ## improvemnt: create stacked bar plots of level 3?
#------------------------------------------------------------------------------------------------------
## combine Variables to find out who was employed the last 12 Months (or self employed)
### categorized into yes,no or no answer/NA

general.employment <- data2019%>%
  select(wrkdpstyr, wrkselfem) %>%                          #select needed variables
  filter(wrkdpstyr %in% c(1,2) | wrkselfem %in% c(1,2)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>% #rearange table to fit values for barplot
  group_by(variable,value) %>%
  summarise(count = n()/56136, .groups = 'drop') %>%          # group to find summarized values for the bar
  filter (value < 94)                                # getting rid of NAs --> NOT working

general.employment

## graph

#manual scale name
employment.scale.manual <- c("1" = "Yes",
                             "2" = "No")

employment.bar <- ggplot(general.employment, aes(x = as.factor(value), y = count, fill = factor(value)))+
  geom_bar(stat = "identity") +
  facet_wrap(~ variable)+
  scale_x_discrete(labels = employment.scale.manual)+
  scale_fill_manual(values = c("1" = "blue", "2" = "red"), labels = employment.scale.manual)+
  scale_y_continuous(labels = scales::percent)+
  labs(title = "Employed either at a business or selfemployed", x = " ")

employment.bar

## BUT in the Skript they use antoher Variable

imputed.employment18 <- data2019 %>%
  select (IRWRKSTAT18) %>%
  filter (IRWRKSTAT18 < 99) %>%
  pivot_longer(cols = everything(), names_to = "status", values_to = "number")%>%
  group_by(status, number) %>%
  summarise(count = n()/56136, .groups = 'drop')
  

ggplot(imputed.employment18, aes(x = factor(number), y = count, fill = factor(number))) +
  geom_bar(stat = "identity")+
  scale_x_discrete(labels = c("1" = "EMployed full-time", "2" = "Employed part-time", "3" = "Unemployed", "4" = "not in labour force"))+
  scale_y_continuous(labels = scales::percent)+
  labs(title = "Employment status of People 18+", x = "Employment Status", y = "Percentage")+
  theme_minimal()

#----------------------------------------------------------------------------------------------------
## sexual Identity
gender <- data2019 %>%
  select(irsex) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "gender") %>%
  group_by(gender) %>%
  summarize(count = n()/56136)
  
ggplot(gender,aes(x = gender, y = count, fill = factor(gender)))+
  geom_col()+
  scale_fill_manual(values = c("1" = "blue", "2" = "red"), labels = c("1" = "Male", "2" = "Female"))+
  scale_y_continuous(labels = scales::percent)+
  labs(title = "Gender", y = "Percentage", fill = "Identity")+
  theme_minimal()

#------------------------------------------------------------------------------------------------------
## MArital Stauts 
## ISSUE not in allfilterdata
marital.status <- data2019 %>%
  select (IRMARIT) %>%
  filter (IRMARIT < 99)

ggplot(marital.status, aes(x = IRMARIT, fill = factor(IRMARIT)))+
  geom_bar()+
  scale_fill_manual(labels = c("1" = "Married", "2" = "Widowed", "3" = "Divorced or Seperated", "4" = "Never been MArried"))+
  labs(title = "Marital satus", fill = "Status")
#------------------------------------------------------------------------------------------------------
## college enrollment(people aged 18-2count## college enrollment(people aged 18-22, enrolled in School and College)
college.enrollment <- data2019 %>%
## ISSUE. not in allfilterdata --> Graph is wrong
  select(collenrlst) %>%
  filter(collenrlst < 5) %>%
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
## Plot for Eudcation level dependend on race
## --> diskrete Farbskala besser
Racial.Background <- data2019 %>%
  select (NEWRACE2, eduhighcat) %>% ## selected AI regions, racial background and education level
  filter(eduhighcat <5)       ## wert 5 streichen? --> Leute unter 17 kein Abschluss, zählen nicht


ggplot(Racial.Background, aes(x = NEWRACE2, fill = factor(eduhighcat)))+
  geom_bar(position = "fill")+
  scale_y_continuous(labels = scales::percent) +
  theme_light()+
  labs(title = "Education achieved by each Race")+
  scale_fill_manual(labels = c())

## maybe better: Ireduhighst2: genauer Klassenabschluss also 5,6,7 ... college und weiter
Racial.Background2 <- data2019 %>%
  select(NEWRACE2, IREDUHIGHST2) 

ggplot(Racial.Background2, aes(x = NEWRACE2, fill = factor(IREDUHIGHST2)))+
  geom_bar(position = "fill")+
  theme_light()+
 # scale_fill_manual(labels = c("1" = "5th", "2"= "6th", "3" ="7th", "4"= "8th", "5" ="9th", "6" = "10th", "7" = "11/12th grade", "8" = "GED", "9" = "some college credit", "10" = "Associate's degree", "11" = "college graduate or higher"))+
  scale_y_continuous(labels = scales::percent)+ 
  labs(title = "Class finished by each Race")
