library(readxl)
library(tidyverse)
tbl <- read_excel("database/tabelao-setores-SEEG-Municipios-2_0-GWP-AR5-FINAL-SITE (1).xlsx", 
                  sheet = "Ranking Municipi & Setores 2022", 
                  range = "c4:K575", col_types = c("numeric", 
                                                   "text", "text", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", 
                                                   "numeric"))

colnames(tbl) <- c("ibge_cod", "municipio", "uf",                                 
                   "Agropecuária",
                   "Energia",
                   "Mudança de Uso da Terra e Florestas",
                   "Processos Industriais",
                   "Resíduos",
                   "Grand_Total")

tbl %>%
  ggplot() + 
  geom_boxplot(aes(x = uf, y = log(Grand_Total), colour = uf) ) +
  geom_jitter(aes(x = uf, y = log(Grand_Total), colour = uf), width = 0.2, alpha=0.3) +
labs(title = "Ranking Municipios & Setores 2022",
     y= "Log(Grand Total)",
     x = "Estado" ) + 
  theme(legend.position = "none")


