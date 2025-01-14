#1) Substanzen- Jemals

Substanz.Probiert <- data2019 %>%
  select(alcever, herever, cocever, cigever) %>%
  pivot_longer(cols = everything(), names_to = "Drug", values_to = "Response")%>%
  filter(Response %in% c(1,2)) %>%
  group_by(Drug, Response) %>%
  summarise(count = n()) %>%
  mutate(count = count/56136)

ggplot(Substanz.Probiert, aes(x = Drug, y = count, fill = factor(Response)))+
  geom_col(position = "dodge")
