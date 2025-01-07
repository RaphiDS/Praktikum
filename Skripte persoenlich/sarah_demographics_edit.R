
# themenfrage: -	Wie hängt der Konsum im Jahr 2019 mit demographischen Merkmalen zusammen?
## datensatz: demografische Faktoren ahnschauen

## installing rquired packages
library(tidyverse)

## create new, cleaned dataset for 2019 for everyone to use
data2019 <- allfilterdata %>%
  filter (year == 2019)


#------------------------------------------------------------------------------------------------------
## AGE grouped into allocated Categories of NSDUH
age.grouped <- data2019 %>%
  select(catage) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "Group") %>%
  group_by(Group) %>%
  summarize(count =n()/56136)

ggplot(age.grouped, aes(x= factor(Group),y = count, fill = factor(Group, labels = c("12-17", "18-25", "26-34", "35+")))) +
  geom_col() +
  labs(title = "Age in allocated Groups", y = "Percentage", fill = "Age groups", x = "Group") +
  theme_light() +
  theme(
    axis.title = element_text(size = 20),  # Achsentitel
    axis.text  = element_text(size = 20),  # Achsbeschriftungen
    legend.position = "none"  # Legendentext
  ) +
  scale_x_discrete(labels = c("12-17", "18-25", "26-34", "35+"))
#---------------------
# age in propertion
age.ratio.levels <- data2019 %>%
  select(CATAG2)%>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "group") %>%
  group_by(group) %>%
  summarize(count =n()/56136)

ggplot(age.ratio.levels, aes(x = factor(group), y = count, fill = factor(group)))+
  geom_col()+
  scale_fill_discrete(labels = c("1" = "12-17", "2" = "18-25", "3" ="26+")) +
  labs(title = "age in levels of three", x = "Group", y = "Percentage") +
  theme_light() +
  theme(
    axis.title = element_text(size = 20),  # Achsentitel
    axis.text  = element_text(size = 20),  # Achsbeschriftungen
    legend.position = "none"  # Legendentext
  ) +
  scale_x_discrete(labels = c("12-17", "18-25", "26+"))


#------------------------

#------------------------------------------------------------------------------------------------------
## combine Variables to find out who was employed the last 12 Months (or self employed)
### categorized into yes,no or no answer/NA


## BUT in the Skript they use antoher Variable

imputed.employment18 <- data2019 %>%
  select (IRWRKSTAT18) %>%
  filter (IRWRKSTAT18 < 99) %>%
  pivot_longer(cols = everything(), names_to = "status", values_to = "number")%>%
  group_by(status, number) %>%
  summarise(count = n()/56136, .groups = 'drop')


ggplot(imputed.employment18, aes(x = factor(number), y = count, fill = factor(number))) +
  geom_bar(stat = "identity")+
  scale_x_discrete(labels = c("1" = "Employed full-time", "2" = "Employed part-time", "3" = "Unemployed", "4" = "not in labour force"))+
  labs(title = "Employment status of People 18+", x = "Employment Status", y = "Percentage")+
  theme_light() +
  theme(
    axis.title = element_text(size = 20),  # Achsentitel
    axis.text  = element_text(size = 20),  # Achsbeschriftungen
    legend.position = "none"  # Legendentext
  )

#----------------------------------------------------------------------------------------------------
## sexual Identity
gender <- data2019 %>%
  select(irsex) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "gender") %>%
  group_by(gender) %>%
  summarize(count = n()/56136)

ggplot(gender,aes(x = factor(gender, levels = 1:2, labels = c("Male", "Female")), y = count, fill = factor(gender)))+
  geom_col()+
  scale_fill_manual(values = c("1" = "blue", "2" = "red"), labels = c("1" = "Male", "2" = "Female"))+
  labs(title = "Gender", y = "Percentage", fill = "Identity", x = "") +
  theme_light() +
  theme(
    axis.title = element_text(size = 20),  # Achsentitel
    axis.text  = element_text(size = 20),  # Achsbeschriftungen
    legend.position = "none"  # Legendentext
  )

#---------------------------------#---------------------------------#------------------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------------------
## Destribution Race
Race.Destr <- data2019 %>%
  select(NEWRACE2) %>%
  pivot_longer(cols = everything(), names_to = "Var", values_to = "Answer") %>%
  group_by(Answer) %>%
  summarize(count = n()) %>%
  mutate(count = count/56136)

ggplot(Race.Destr, aes(x = factor(Answer), y = count, fill = factor(Answer)))+
  geom_col()+
  scale_x_discrete(labels = c("1" = "White", "2" = "Afr.Am", "3" = "Am/AK Native", "4" ="Other Pac Isl", "5" = " Asian", "6" = "more than one Race", "7" = "Hispamic"),
                   guide = guide_axis(angle = 45)) +
  labs(title = "Race ", y = "Percentage", x = "Background") +
  theme_light() +
  theme(
    axis.title = element_text(size = 20),  # Achsentitel
    axis.text  = element_text(size = 20),  # Achsbeschriftungen
    legend.position = "none"  # Legendentext
  )


## Race and Gender

#-----------------------------------------------------------------------------------------------------

## Plot for Eudcation level dependend on race
## --> diskrete Farbskala besser
Racial.Background <- data2019 %>%
  select (NEWRACE2, eduhighcat) %>% ## selected AI regions, racial background and education level
  filter(eduhighcat <5)       ## wert 5 streichen? --> Leute unter 17 kein Abschluss, zählen nicht


ggplot(Racial.Background, aes(x = NEWRACE2, fill = factor(eduhighcat)))+
  geom_bar(position = "fill")+
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = c("1" = "White", "2" = "Afr.Am", "3" = "Am/AK Native", "4" ="Other Pac Isl", "5" = " Asian", "6" = "more than one Race", "7" = "Hispamic"))+
  scale_fill_discrete(labels = c("1" = "some High School", "2"= "HIgh School Grad", "3" ="Some coll/Assoc Dg", "4"= "College graduate"))+
  theme_light()+
  labs(title = "Education achieved by each Race")
