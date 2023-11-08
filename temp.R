library(data.table)


tbl <- fread("./database/tabelao/tabelao.csv")
tbl[]

tbl2 <- fread("./database/1-SEEG10_GERAL-BR_UF_2022 GEE Brasil.csv",
              # sep = ";", dec=",",
              na.strings = "-",
              header = TRUE)

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

tbl2 %>%
  filter(N1 %in% c("Processos Industriais",
                 "Agropecuária",
                 # "Energia",
                 # "Mudança de Uso da Terra e Floresta",
                 # "Resíduos",
                 "joe")) %>% 
  select(ERB, starts_with("YY_")) %>% 
  pivot_longer(cols = starts_with("YY_")) %>% 
  group_by(ERB, name) %>% 
  summarise(v = sum(value, na.rm = TRUE)) %>% 
  pivot_wider(id_cols = c("name"), names_from = "ERB", values_from = "v") -> b
# mutate(value2 = as.numeric(value)) %>%
# filter(is.na(value2))

1488686061
 121146405+1212480692
2051977036


 
tbl2[, .(ERB)]

tbl2[, unique(N1)]

tbl2[, unique(N2)]

tbl2[, unique(N5)]


