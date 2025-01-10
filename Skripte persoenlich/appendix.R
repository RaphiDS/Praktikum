
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


ggplot(Racial.Background, aes(x = NEWRACE2, fill = factor(eduhighcat)))+
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = c("1" = "White", "2" = "Afr.Am", "3" = "Am/AK Native", "4" ="Other Pac Isl", "5" = " Asian", "6" = "more than one Race", "7" = "Hispamic"))+
  scale_fill_discrete(labels = c("1" = "some High School", "2"= "HIgh School Grad", "3" ="Some coll/Assoc Dg", "4"= "College graduate"))+
  theme_light()+
  labs(title = "Education achieved by each Race")