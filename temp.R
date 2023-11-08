rm(list = ls())
library(data.table)


# tbl <- fread("./database/tabelao/tabelao.csv")
# tbl[]

library(readr)
tbl2 <- read_delim("database/1-SEEG10_GERAL-BR_UF_2022 GEE Brasil.csv", 
                   delim = ";", escape_double = FALSE,
                   locale = locale(decimal_mark = ",",
                                   grouping_mark = "."), na = "-",
                   trim_ws = TRUE)
tbl2


library(readxl)
PIB <- read_excel("database/emissoes_gases.xlsx", 
                               sheet = "Sheet7", range = "M1:M33")
colnames(PIB) <- "PIB_REAL"
PIB$ANO <- 1990:2021


# tbl2 <- fread("./database/1-SEEG10_GERAL-BR_UF_2022 GEE Brasil.csv",
#               sep = ";", dec = ",", 
#               na.strings = "-",
#               header = TRUE)
str(tbl2)

colnames(tbl2) <- c("N1", "N2", "N3",
                    "N4", "N5", "N6",
                    "ERB", "Gas", "Territorio", "AE", "Produto", "YY_1970",
                    "YY_1971", "YY_1972", "YY_1973",
                    "YY_1974", "YY_1975", "YY_1976",
                    "YY_1977", "YY_1978", "YY_1979",
                    "YY_1980", "YY_1981", "YY_1982",
                    "YY_1983", "YY_1984", "YY_1985",
                    "YY_1986", "YY_1987", "YY_1988",
                    "YY_1989", "YY_1990", "YY_1991",
                    "YY_1992", "YY_1993", "YY_1994",
                    "YY_1995", "YY_1996", "YY_1997",
                    "YY_1998", "YY_1999", "YY_2000",
                    "YY_2001", "YY_2002", "YY_2003",
                    "YY_2004", "YY_2005", "YY_2006",
                    "YY_2007", "YY_2008", "YY_2009",
                    "YY_2010", "YY_2011", "YY_2012",
                    "YY_2013", "YY_2014", "YY_2015",
                    "YY_2016", "YY_2017", "YY_2018",
                    "YY_2019", "YY_2020", "YY_2021")

str(tbl2)
library(tidyr)
library(dplyr)

unique(tbl2$N1)





tbl3 <- tbl2 %>%
  filter(Gas %in% c("CO2e (t) GWP-AR6")) %>%
  # filter(!(N1 %in% c("Mudança de Uso da Terra e Floresta"))) %>%
  # pull(N1) %>% unique()
  # filter(N1 %in% c("Processos Industriais",
  #                  "Agropecuária",
  #                  # "Energia",
  #                  # "Mudança de Uso da Terra e Floresta",
  #                  # "Resíduos",
  #                  "SEMCOLUNA")) %>%
  select(ERB, starts_with("YY_")) %>% 
  pivot_longer(cols = starts_with("YY_"), names_prefix = "YY_", names_transform = as.integer) %>% 
  group_by(ERB, name) %>% 
  summarise(value = sum(value, na.rm = TRUE)) %>% 
  filter(name >= 1990) %>% 
  pivot_wider(id_cols = c("name"), names_from = "ERB", values_from = "value")
# mutate(value2 = as.numeric(value)) %>%
# filter(is.na(value2))

tbl2 %>%
  filter(Gas %in% c("CO2e (t) GWP-AR6")) %>%
  select(N1, ERB, starts_with("YY_")) %>% 
  pivot_longer(cols = starts_with("YY_"), names_prefix = "YY_", names_transform = as.integer) %>% 
  group_by(N1, ERB, name) %>% 
  summarise(value = sum(value, na.rm = TRUE)) %>% 
  filter(name >= 1990) %>% 
  pivot_wider(id_cols = c("N1", "name"), names_from = "ERB", values_from = "value") %>% 
  arrange(name)






colnames(tbl3) <- c("name", "Bunker", "Emissao", "Emissao_NCI", "Remocao", "Remocao_NCI")

tbl4 <- inner_join(PIB, tbl3, by=c("ANO"="name"))

tbl4.1 <- tbl4 %>% 
  mutate(factor = 31.62*6.3951/2.1441,
         Emissao = Emissao*factor,
         Emissao_NCI = Emissao_NCI*factor,
         Remocao = Remocao*factor,
         # Remocao_NCI = Remocao_NCI*factor,
         n=100*Bunker/Emissao  )

library(ggplot2)
tbl4.1 %>% 
  mutate(P1 = PIB_REAL*1e6,
         P2 = P1 - Emissao,
         P3 = P2 - Remocao,
         P22 = P2 - Emissao_NCI,
         # P32 = P22 - Remocao_NCI
         ) %>% 
  ggplot() + 
  geom_line(aes(x=ANO, y=P1, colour = "P1")) +
  # geom_line(aes(x=ANO, y=P2, colour = "P2")) +
  # geom_line(aes(x=ANO, y=P3, colour = "P3")) +
  geom_line(aes(x=ANO, y=P22, colour = "P22")) +
  # geom_line(aes(x=ANO, y=P32, colour = "P32")) +
  theme_bw()+
  labs()  
  

tbl4 %>% 
  mutate(P1 = PIB_REAL*1e6,
         P2 = P1 - Emissao,
         P3 = P2 - Remocao,
         P22 = P2 - Emissao_NCI,
         P32 = P22 - Remocao_NCI ) %>% 
  ggplot() + 
  geom_col(aes(x=ANO, y=P1/1e12, fill = "P1")) +
  geom_col(aes(x=ANO, y=P2/1e12, colour = "P2")) +
  # geom_line(aes(x=ANO, y=P3, colour = "P3")) +
  # geom_line(aes(x=ANO, y=P22, colour = "P22")) +
  # geom_line(aes(x=ANO, y=P32, colour = "P32")) +
  # ylim(2,3) +
  labs()



