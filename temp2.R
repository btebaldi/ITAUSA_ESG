PIB2 <- read_excel("database/emissoes_gases.xlsx", 
                  sheet = "Sheet7", range = "BB1:BC33")
colnames(PIB2) <- c("CO2", "PIB")
PIB2$ANO <- 1990:2021


PIB2 %>%
  ggplot() +
  geom_line(aes(x=ANO, y=PIB, colour = "PIB")) + 
  geom_line(aes(x=ANO, y=PIB-CO2, colour = "PIB Ajus.")) + 
  theme_bw()+
  theme(legend.position = "bottom")+
labs(colour = NULL)


PIB2 %>%
  mutate(Dl.PIB = log(PIB) - log(lag(PIB)), 
         Dl.PIB2 = log(PIB-CO2) - log(lag(PIB-CO2)) ) %>% 
  ggplot() +
  geom_line(aes(x=ANO, y=Dl.PIB), colour = "red") + 
  geom_line(aes(x=ANO, y=Dl.PIB2)) + 
  labs()
