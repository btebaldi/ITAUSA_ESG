#' ---
#' title: "Analise Econometrica"
#' author: "Bruno Tebaldi Q. Barbosa"
#' date: "2023-11-24"
#' output: html_document
#' ---

# Setup -------------------------------------------------------------------

rm(list = ls())

library(data.table)
library(dplyr)
library(ggplot2)
library(ggcorrplot)
library(plm)
library(broom)
library(writexl)
library(readxl)
library(tidyr)

# User defined functions --------------------------------------------------

d.ln <- function(x){
  ret <- log(x) - log(dplyr::lag(x))
  return(ret)
}


# Data Load ---------------------------------------------------------------

# leitura dos dados
tbl <- fread("./database/dt_panel_v1.csv", dec = ",")

# convertendo dados numericos
tbl$pwt_pop <- as.numeric(tbl$pwt_pop)
tbl$pwt_hc <- as.numeric(tbl$pwt_hc)
tbl$pwt_ctfp <- as.numeric(tbl$pwt_ctfp)

# leitura do dicionario de dados
dic <- fread("./database/wdi_vars_dic_v1.csv")
dic[,1]

# Renaming columns
tbl <- rename(tbl,
              Bird = wdi_EN.BIR.THRD.NO,
              Fish = wdi_EN.FSH.THRD.NO, 
              Plant = wdi_EN.HPT.THRD.NO, 
              Mammal = wdi_EN.MAM.THRD.NO, 
              Land_protected = wdi_ER.LND.PTLD.ZS, 
              Marine_protected = wdi_ER.MRN.PTMR.ZS,
              Forest = wdi_AG.LND.FRST.K2,
              Forest_perc = wdi_AG.LND.FRST.ZS,
              CO2 = wdi_EN.ATM.CO2E.KT,
              Greenhouse = wdi_EN.ATM.GHGT.KT.CE,
              Natural_resources_rents = wdi_NY.GDP.TOTL.RT.ZS, 
              Gini_1 = wdi_SI.POV.GINI,
              Idh = hdi,
              Forest = wdi_AG.LND.FRST.K2,
              Forest_perc = wdi_AG.LND.FRST.ZS,
              Ich = wdi_HD.HCI.OVRL,
              GDP = wdi_NY.GDP.PCAP.KD,
              GDP_r = wdi_NY.GDP.PCAP.PP.KD,
              RuleOfLaw = wdi_RL.EST,
              Land_n_Marine = wdi_ER.PTD.TOTL.ZS)


# summary(tbl)

# Data Wrangler  ----------------------------------------------------------

# Completanto as informacoes de Bird, Fish, e Plant
tbl[, Bird := mean(Bird, na.rm=TRUE), iso3]
tbl[, Fish := mean(Fish, na.rm=TRUE), iso3]
tbl[, Plant := mean(Plant, na.rm=TRUE), iso3]
tbl[, Mammal := mean(Mammal, na.rm=TRUE), iso3]


# Completanto as informacoes de Bird, Fish, e Plant
tbl[, Land_n_Marine := mean(Land_n_Marine, na.rm=TRUE), iso3]


# Cria a variavel de crescimento
tbl[,  g := d.ln(GDP), iso3]
tbl[,  GDP_1 := dplyr::lag(log(GDP)), iso3]

# Calculo a diff de log do CO2 
tbl[,  dlnCO2 := d.ln(CO2), iso3]


# Data completion of Gini -------------------------------------------------

gini_1 <- read_excel("./database/gini.xlsx")

gini_1$country[gini_1$country == "Czech Republic"] <- "Czechia"
gini_1$country[gini_1$country == "South Korea"] <- "Korea, Rep."
gini_1$country[gini_1$country == "Turkey"] <- "Turkiye"
gini_1$country[gini_1$country == "UK"] <- "United Kingdom"
gini_1$country[gini_1$country == "Yemen"] <- "Yemen, Rep."
gini_1$country[gini_1$country == "USA"] <- "United States"
gini_1$country[gini_1$country == "Cape Verde"] <- "Cabo Verde"
gini_1$country[gini_1$country == "Russia"] <- "Russian Federation"
gini_1$country[gini_1$country == "Hong Kong, China"] <- "Hong Kong SAR, China"
gini_1$country[gini_1$country == "Venezuela"] <- "Venezuela, RB"
gini_1$country[gini_1$country == "Vietnam"] <- "Viet Nam"
gini_1$country[gini_1$country == "UAE"] <- "United Arab Emirates"
gini_1$country[gini_1$country == "Lao"] <- "Lao PDR"
gini_1$country[gini_1$country == "Iran"] <- "Iran, Islamic Rep."
gini_1$country[gini_1$country == "Brunei"] <- "Brunei Darussalam"
gini_1$country[gini_1$country == "Bahamas"] <- "Bahamas, The"
gini_1$country[gini_1$country == "Egypt"] <- "Egypt, Arab Rep."
gini_1$country[gini_1$country == "Gambia"] <- "Gambia, The"
gini_1$country[gini_1$country == "North Korea"] <- "Korea, Dem. People's Rep."
gini_1$country[gini_1$country == "Syria"] <- "Syrian Arab Republic"
gini_1$country[gini_1$country == "Palestine"] <- "West Bank and Gaza"


gini_1 <- gini_1 %>%
  pivot_longer(cols = -country, names_to = "year",
               names_transform = as.integer,
               values_to = "Gini_2")

tbl <- left_join(tbl, gini_1, by=c("country_name"="country", "year"="year"))

tbl <- mutate(tbl, Gini = Gini_2)


# Data completion of ctfp -------------------------------------------------

# Complete with pwt
my_pwt10 <- pwt10::pwt10.01 %>% select(isocode, year, ctfp)
tbl <- left_join(tbl, my_pwt10, by = c("iso3"="isocode", "year"="year"))


# Estatisticas Descritiva -------------------------------------------------

tbl <- tbl[, .(iso3, year, GDP,
               GDP_1,
               ctfp,
               dlnCO2,
               pwt_hc,
               RuleOfLaw,
               Gini,
               Idh,
               Forest_perc, 
               g)]

tbl %>% summary()


# Grafico correlacao ------------------------------------------------------

# Correlation matrix
corr <- cor(tbl[, .(GDP, GDP_1, PTF = ctfp, CO2 = dlnCO2, Ich = pwt_hc, Instituicao = RuleOfLaw, Gini, Idh, Floresta = Forest_perc, Crescimento = g)],
            use = "pairwise.complete.obs")


ggcorrplot(corr,
           hc.order = TRUE, 
           type = "full",
           lab = TRUE, 
           lab_size = 3, 
           show.diag = TRUE,
           method="square",
           colors = c("tomato2", "white", "springgreen3"), 
           title="Environment: Biodiversity & protected areas - correlation", 
           ggtheme=theme_bw, 
           tl.srt = 90)



ggsave(filename = "./graficos/correlation.png",
       plot = last_plot(),
       units = "in",
       scale = 1,
       width = 8, height = 6,
       dpi = 100)


# Panel estimation --------------------------------------------------------


pmdl_01.1 <- plm(g ~ GDP_1 +
                   ctfp +
                   dlnCO2 + 
                   dlnCO2*ctfp +
                   pwt_hc + 
                   RuleOfLaw +
                   Gini + 
                   Idh +
                   Forest_perc,
                 data=tbl,
                 effect = "twoways",
                 index=c("iso3", "year"),
                 model="within")

summary(pmdl_01.1)

length(unique(tbl$iso3))
length(unique(tbl$year))
# 195 paises com 6 anos cada (total de 1170 pontos)

# estimativa feita com 94 paises considerando-se 5 anos, gerando um total de 470 pontos


