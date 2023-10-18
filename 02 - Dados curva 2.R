rm(list = ls())
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
GDP <- read_excel("./database/total_gdp_us_inflation_adjusted.xlsx")
head(GDP)

CO2 <- read_excel("./database/yearly_co2_emissions_1000_tonnes.xlsx")
head(CO2)

GDP2 <- GDP %>% filter(country %in% c("UK", "USA")) %>% 
  pivot_longer(cols = -country)

  
GDP2$type <- str_match(string = GDP2$value, pattern = "B")
GDP2$value2 <- str_replace(string = GDP2$value, pattern = "B", replacement = "")
GDP2$value2 <- str_replace(string = GDP2$value2, pattern = "TR", replacement = "")
GDP2$value2 <- as.numeric(GDP2$value2)
GDP2$value2[is.na(GDP2$type)] <- GDP2$value2[is.na(GDP2$type)] *1000

GDP2 <- GDP2 %>% select(country, name, GDP = value2)

CO2_2 <- CO2 %>% filter(country %in% c("UK", "USA")) %>% 
  pivot_longer(cols = -country, values_transform = as.character)

CO2_2$type <- str_match(string = CO2_2$value, pattern = "k")

CO2_2$value2 <- str_replace(string = CO2_2$value, pattern = "k", replacement = "")
CO2_2$value2 <- str_replace(string = CO2_2$value2, pattern = "M", replacement = "")
CO2_2$value2 <- as.numeric(CO2_2$value2)
CO2_2$value2[is.na(CO2_2$type)] <- CO2_2$value2[is.na(CO2_2$type)] *1000

CO2_2 <- CO2_2 %>% select(country, name, CO2 = value2)


tbl <- inner_join(GDP2, CO2_2, by = c("country"="country", "name"="name"))


library(ggplot2)

tbl %>% 
  filter(country == "USA") %>% 
ggplot(aes(x=GDP, y = CO2)) + 
  geom_point() + 
  geom_smooth(formula = y~poly(x, degree = 2) , method = "lm") + 
  theme_bw() + 
labs(title = "Estados Unidos da América", 
     x = "PIB", y= "CO2", caption = "Fonte: https://www.gapminder.org")  

ggsave(filename = "./curva1.png", width = 8, height = 6, dpi = 100, units = "in", scale = 1)

tbl %>% 
  filter(country == "UK") %>% 
  ggplot(aes(x=GDP, y = CO2)) + 
  geom_point() + 
  geom_smooth(formula = y~poly(x, degree = 2) , method = "lm") + 
  theme_bw() + 
  labs(title = "Reino Unido", 
       x = "PIB", y= "CO2", caption = "Fonte: https://www.gapminder.org")  

ggsave(filename = "./curva2.png", width = 8, height = 6, dpi = 100, units = "in", scale = 1)
