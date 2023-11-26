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

tbl[,  dlnCO2 := d.ln(CO2), iso3]

# summary(tbl)

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

# Tratamento especifico para AUS, IND
tbl[iso3 %chin% c("AUS", "IND"), .(iso3, Gini, Gini_1, Gini_2)]
tbl[iso3 %chin% c("AUS", "IND"), Gini := ifelse(test = is.na(Gini_1), yes = Gini_2, no = Gini_1)]
tbl[iso3 %chin% c("AUS", "IND"), .(iso3, Gini, Gini_1, Gini_2)]

# Data completion of ctfp -------------------------------------------------

# Complete with pwt
my_pwt10 <- pwt10::pwt10.01 %>% select(isocode, year, ctfp)
tbl <- left_join(tbl, my_pwt10, by = c("iso3"="isocode", "year"="year"))


# Estatisticas Descritiva -------------------------------------------------

tbl[, .(year, 
        Forest, 
        Forest_perc, 
        CO2,
        Greenhouse, 
        Bird, Fish, Plant, Mammal, 
        Land_protected, Marine_protected, Land_n_Marine,
        Ich,
        GDP,
        GDP_r,
        Natural_resources_rents,
        RuleOfLaw,
        Gini,
        Idh, 
        pwt_pop, 
        pwt_hc, 
        pwt_ctfp, 
        g)] %>% summary()


# Resume nature variables in a single factor
tbl <- tbl[!is.na(Forest),]
pca <- prcomp(tbl[, .(Bird, Fish, Plant, Mammal)], scale = TRUE)
summary(pca)
tbl$pca1_nature <- pca$x[, 1]


# Grafico correlacao ------------------------------------------------------

# Correlation matrix
corr <- cor(tbl[, .(Forest, Forest_perc, CO2,
                    Greenhouse, Bird, Fish, Plant, Mammal, Land_protected, Marine_protected,
                    Land_n_Marine, Ich, GDP, GDP_r, Natural_resources_rents, RuleOfLaw, Gini,
                    Idh, pwt_pop, pwt_hc, pwt_ctfp, g, GDP_1)],
            use = "pairwise.complete.obs")


ggcorrplot(corr,
           hc.order = TRUE, 
           type = "full",
           lab = TRUE, 
           lab_size = 3, 
           show.diag = TRUE,
           method="square",
           colors = c("tomato2", "white", "springgreen3"), 
           title="Environment: Biodiversity & protected areas", 
           ggtheme=theme_bw, 
           tl.srt = 90)




# Panel estimation --------------------------------------------------------


#  Run a Panel Estimation
pmdl_01 <- plm(g ~ Gini + Idh + pca1_nature + dlnCO2*ctfp + GDP_1 + pwt_hc + Forest_perc + Natural_resources_rents + RuleOfLaw,
               data=tbl,
               index=c("iso3", "year"),
               model="within")

summary(pmdl_01)


# Teste de Hausman (Conclusao do teste: usar modelo de efeito fixo)
# pmdl_01.r <- plm(g ~ Gini + Idh + pca1_nature + dlnCO2*ctfp + GDP_1 + pwt_hc + Forest_perc + Natural_resources_rents + RuleOfLaw,
#                data=tbl,
#                index=c("iso3", "year"),
#                model="random")
# phtest(pmdl_01, pmdl_01.r)



#  Run a Panel Estimation
pmdl_02 <- plm(g ~ Gini + Idh + pca1_nature + dlnCO2*ctfp + GDP_1 + pwt_hc + Forest_perc + Natural_resources_rents,
               data=tbl,
               index=c("iso3", "year"),
               model="within")

summary(pmdl_02)



#  Run a Panel Estimation (retirada do  GDP_1)
pmdl_03 <- plm(g ~ Gini + Idh + pca1_nature + dlnCO2*ctfp + pwt_hc + Forest_perc + Natural_resources_rents + RuleOfLaw,
               data=tbl,
               index=c("iso3", "year"),
               model="within")

summary(pmdl_03)


#  Run a Panel Estimation (retirada do  GDP_1, Forest_perc,
#  Natural_resources_rents, RuleOfLaw)
pmdl_04 <- plm(g ~ Gini + Idh + pca1_nature + dlnCO2*ctfp + pwt_hc,
               data=tbl,
               index=c("iso3", "year"),
               model="within")

summary(pmdl_04)


# OLS Average Estimation --------------------------------------------------

# Compute the averages for every country
tbl_ols <- tbl %>% 
  group_by(iso3) %>% 
  summarise(g = mean(g, na.rm = TRUE),
            Gini = mean(Gini, na.rm=TRUE),
            Idh = mean(Idh, na.rm=TRUE),
            pca1_nature = mean(pca1_nature, na.rm=TRUE),
            dlnCO2 = mean(dlnCO2, na.rm=TRUE),
            ctfp = mean(ctfp, na.rm=TRUE),
            GDP_1 = mean(GDP_1, na.rm=TRUE),
            pwt_hc = mean(pwt_hc, na.rm=TRUE),
            Forest_perc = mean(Forest_perc, na.rm=TRUE),
            Natural_resources_rents = mean(Natural_resources_rents, na.rm=TRUE),
            RuleOfLaw = mean(RuleOfLaw, na.rm=TRUE))

tbl_ols


#  Run a OLS Estimation on the averages
pmdl_05 <- lm(g ~ Gini + Idh + pca1_nature + dlnCO2*ctfp + GDP_1 + pwt_hc + Forest_perc + Natural_resources_rents + RuleOfLaw,
               data=tbl_ols)

summary(pmdl_05)


#  Run a OLS Estimation on the averages
pmdl_06 <- lm(g ~ Gini + Idh + pca1_nature + dlnCO2*ctfp + GDP_1 + pwt_hc + Forest_perc, data=tbl_ols)

summary(pmdl_06)


#  Run a OLS Estimation on the averages
pmdl_07 <- lm(g ~ Gini + Idh + pca1_nature + dlnCO2*ctfp + pwt_hc, data=tbl_ols)

summary(pmdl_07)



# Arellano–Bond estimator -------------------------------------------------

#  Run a Panel Estimation
pmdl_ab <- pgmm(g ~ Gini + Idh + dlnCO2*ctfp + GDP_1 + pwt_hc + Forest_perc + Natural_resources_rents + RuleOfLaw |  lag(GDP_1, 2:4),
               data=tbl,
               effect = "individual",
               model = "onestep",
               index=c("iso3", "year"))

summary(pmdl_ab)


#  Run a Panel Estimation
pmdl_ab2 <- pgmm(g ~ Gini + Idh + dlnCO2*ctfp + GDP_1 + pwt_hc + Forest_perc |  lag(GDP_1, 2:4),
                data=tbl,
                effect = "individual",
                model = "onestep",
                index=c("iso3", "year"))

summary(pmdl_ab2)


write_xlsx(x = list("pmdl_01" = broom::tidy(pmdl_01),
                    "pmdl_02" = broom::tidy(pmdl_02),
                    "pmdl_03" = broom::tidy(pmdl_03),
                    "pmdl_04" = broom::tidy(pmdl_04),
                    "pmdl_05" = broom::tidy(pmdl_05),
                    "pmdl_06" = broom::tidy(pmdl_06),
                    "pmdl_07" = broom::tidy(pmdl_07)),
           path = "Resultado_Regressoes.xlsx")
