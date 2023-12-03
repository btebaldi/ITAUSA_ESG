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

# Data completion of human capital index ----------------------------------

my_pwt10_hc <- pwt10::pwt10.01 %>% select(isocode, year, hc)
tbl <- left_join(tbl, my_pwt10_hc, by = c("iso3"="isocode", "year"="year"))

tbl[, .(iso3, Ich, hc, pwt_hc)] %>% summary()
tbl[is.na(hc) & !is.na(pwt_hc),  .(iso3, Ich, hc, pwt_hc)]

# sobrescrevo informacao original
tbl[, pwt_hc := hc]

# crio modelo linear
tbl[, .(iso3, Ich, hc, pwt_hc)] %>% 
  lm(hc~Ich, data = .) -> ich_mdl

#  Roda modelo linear para completar Hc
tbl[is.na(hc) & !is.na(Ich), hc := (ich_mdl$coefficients[1] + ich_mdl$coefficients[2]*Ich)]


# Data completion of ctfp -------------------------------------------------

# Complete with pwt
my_pwt10 <- pwt10::pwt10.01 %>% select(isocode, year, ctfp)
tbl <- left_join(tbl, my_pwt10, by = c("iso3"="isocode", "year"="year"))
# tbl[is.na(ctfp), .N, iso3]

# Data completion of Education --------------------------------------------

Edu_tbl <- fread("./database/IHME_EDUC_DISTRIBUTIONS_1970_2030_Y2020M04D15.CSV")
Edu_tbl <- Edu_tbl[year >=2014 & year <2020 & measure == "mean_years",  .(Education = mean(mean)),  .(location_name, year)]
# Edu_tbl[location_name %like% "w", unique(location_name)]
# sort(Edu_tbl[,unique(location_name)])
# Edu_tbl[location_name %like% "Es", ]

Edu_tbl$location_name[Edu_tbl$location_name == "Czech Republic"] <- "Czechia"
Edu_tbl$location_name[Edu_tbl$location_name == "South Korea"] <- "Korea, Rep."
Edu_tbl$location_name[Edu_tbl$location_name == "North Korea"] <- "Korea, Dem. People's Rep."
Edu_tbl$location_name[Edu_tbl$location_name == "Turkey"] <- "Turkiye"

Edu_tbl$location_name[Edu_tbl$location_name == "Yemen"] <- "Yemen, Rep."
Edu_tbl$location_name[Edu_tbl$location_name == "Cape Verde"] <- "Cabo Verde"
Edu_tbl$location_name[Edu_tbl$location_name == "Russia"] <- "Russian Federation"
# Edu_tbl$location_name[Edu_tbl$location_name == "Hong Kong, China"] <- "Hong Kong SAR, China"
Edu_tbl$location_name[Edu_tbl$location_name == "Venezuela"] <- "Venezuela, RB"
Edu_tbl$location_name[Edu_tbl$location_name == "Vietnam"] <- "Viet Nam"
# Edu_tbl$location_name[Edu_tbl$location_name == "UAE"] <- "United Arab Emirates"
Edu_tbl$location_name[Edu_tbl$location_name == "Laos"] <- "Lao PDR"
Edu_tbl$location_name[Edu_tbl$location_name == "Iran"] <- "Iran, Islamic Rep."
Edu_tbl$location_name[Edu_tbl$location_name == "Brunei"] <- "Brunei Darussalam"
Edu_tbl$location_name[Edu_tbl$location_name == "The Bahamas"] <- "Bahamas, The"
Edu_tbl$location_name[Edu_tbl$location_name == "Egypt"] <- "Egypt, Arab Rep."
Edu_tbl$location_name[Edu_tbl$location_name == "The Gambia"] <- "Gambia, The"
Edu_tbl$location_name[Edu_tbl$location_name == "Syria"] <- "Syrian Arab Republic"
Edu_tbl$location_name[Edu_tbl$location_name == "Palestine"] <- "West Bank and Gaza"
Edu_tbl$location_name[Edu_tbl$location_name == "Slovakia"] <- "Slovak Republic"
Edu_tbl$location_name[Edu_tbl$location_name == "Democratic Republic of the Congo"] <- "Congo, Dem. Rep."
Edu_tbl$location_name[Edu_tbl$location_name == "Congo"] <- "Congo, Rep."
Edu_tbl$location_name[Edu_tbl$location_name == "Saint Vincent and the Grenadines"] <- "St. Vincent and the Grenadines"
Edu_tbl$location_name[Edu_tbl$location_name == "Kyrgyzstan"] <- "Kyrgyz Republic"
Edu_tbl$location_name[Edu_tbl$location_name == "Saint Lucia"] <- "St. Lucia"
Edu_tbl$location_name[Edu_tbl$location_name == "Macedonia"] <- "North Macedonia"

tbl <- left_join(tbl, Edu_tbl, by=c("country_name"="location_name", "year"="year"))


Education_mdl <- lm(Education~pwt_hc + Idh + GDP + ctfp, data = tbl)
tbl$Education2 <- as.numeric(NA)
tbl[is.na(Education), Education := (Education_mdl$coefficients[1] + Education_mdl$coefficients[2] * pwt_hc + Education_mdl$coefficients[3]*Idh + Education_mdl$coefficients[4]*GDP + Education_mdl$coefficients[5]*ctfp)]

tbl[] %>% ggplot() + geom_point(aes(pwt_hc, Education, colour = iso3), alpha=0.5) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(title = "correlation by country between Human capital and Education", subtitle = "142 countries with 6 datapoints",
       x="Human capital")

# tbl[] %>% select(pwt_hc, Education, iso3) %>% na.omit() %>% group_by(iso3) %>% count(iso3) %>% arrange(-n)

ggsave(filename = "./graficos/HumanCapital_Education.png",
       plot = last_plot(),
       units = "in",
       scale = 1.5,
       width = 8, height = 6,
       dpi = 100)


# Estatisticas Descritiva -------------------------------------------------

tbl[, .(year, 
        # Forest, 
        Forest_perc, 
        CO2,
        # Greenhouse, 
        Bird, Fish, Plant, Mammal, 
        Land_protected, Marine_protected, Land_n_Marine,
        Ich,
        GDP,
        # GDP_r,
        Natural_resources_rents,
        RuleOfLaw,
        Gini,
        Idh, 
        hc,
        Education,
        ctfp, 
        g)] %>% summary()

# tbl %>% count(iso3) %>% arrange(n)

# 194 countries with 6 datapoints

#  carbon total factor productivity
# PWT provides a ‘current PPP’ TFP series (CTFP),

# Resume nature variables in a single factor ------------------------------

tbl <- tbl[!is.na(Forest),]
pca <- prcomp(tbl[, .(Bird, Fish, Plant, Mammal)], scale = TRUE)
summary(pca)
tbl$pca1_nature <- pca$x[, 1]


# Grafico correlacao ------------------------------------------------------

# Correlation matrix
corr <- cor(tbl[, .(Forest, Forest_perc, CO2,
                    Greenhouse, Bird, Fish, Plant, Mammal, Land_protected, Marine_protected,
                    Land_n_Marine, Ich, GDP, GDP_r, Natural_resources_rents, RuleOfLaw, Gini,
                    Education,
                    Idh, pwt_pop, hc, ctfp, g, GDP_1)],
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
       scale = 2,
       width = 8, height = 6,
       dpi = 100)


# Panel estimation --------------------------------------------------------


#  Run a Panel Estimation
pmdl_01 <- plm(g ~ Gini + Idh + pca1_nature + dlnCO2*ctfp + GDP_1 + Education + Forest_perc + Natural_resources_rents + RuleOfLaw,
               data=tbl,
               # effect = "individual",
               # effect = "time",
               effect = "twoways",
               index=c("iso3", "year"),
               model="within")

summary(pmdl_01)

# 114 paises com 5 anos cada (total de 570 pontos)

# List of economic crises
# 2014 Russian financial crisis
# 2014–2017 Brazilian economic crisis
# 2015 Chinese stock market crash
# Turkish currency and debt crisis, 2018
# European sovereign debt crisis (EU) (2009–2019)
# Greek government-debt crisis (2009–2019)


# Teste de Hausman (Conclusao do teste: usar modelo de efeito fixo)
# pmdl_01.r <- plm(g ~ Gini + Idh + pca1_nature + dlnCO2*ctfp + GDP_1 + hc + Forest_perc + Natural_resources_rents + RuleOfLaw,
#                data=tbl,
#                index=c("iso3", "year"),
#                model="random")
# phtest(pmdl_01, pmdl_01.r)



#  Run a Panel Estimation
pmdl_02 <- plm(g ~ Gini + Idh + dlnCO2*ctfp + GDP_1 + Natural_resources_rents,
               data=tbl,
               # effect = "individual",
               # effect = "time",
               effect = "twoways",
               index=c("iso3", "year"),
               model="within")

summary(pmdl_02)

# 114 paises com 5 anos cada (total de 570 pontos)


# ESTIMACAO DESLIGADA
#  Run a Panel Estimation (retirada do  GDP_1)
# pmdl_03 <- plm(g ~ Gini + Idh + pca1_nature + dlnCO2*ctfp + hc + Forest_perc + Natural_resources_rents + RuleOfLaw,
#                data=tbl,
#                index=c("iso3", "year"),
#                model="within")
# 
# summary(pmdl_03)

# ESTIMACAO DESLIGADA
#  Run a Panel Estimation (retirada do  GDP_1, Forest_perc,
#  Natural_resources_rents, RuleOfLaw)
# pmdl_04 <- plm(g ~ Gini + Idh + pca1_nature + dlnCO2*ctfp + hc,
#                data=tbl,
#                index=c("iso3", "year"),
#                model="within")
# 
# summary(pmdl_04)


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
            hc = mean(hc, na.rm=TRUE),
            Education = mean(hc, na.rm=TRUE),
            Forest_perc = mean(Forest_perc, na.rm=TRUE),
            Natural_resources_rents = mean(Natural_resources_rents, na.rm=TRUE),
            RuleOfLaw = mean(RuleOfLaw, na.rm=TRUE))

tbl_ols


#  Run a OLS Estimation on the averages
pmdl_05 <- lm(g ~ Gini + Idh + pca1_nature + dlnCO2*ctfp + GDP_1 + Education + Forest_perc + Natural_resources_rents + RuleOfLaw,
              data=tbl_ols)

summary(pmdl_05)

# tbl_ols %>% select(g, Gini, Idh, pca1_nature, dlnCO2, ctfp, GDP_1, Education, Forest_perc, Natural_resources_rents, RuleOfLaw) %>% 
#   na.omit()
# 114 paises com informacoes completas


#  Run a OLS Estimation on the averages
pmdl_06 <- lm(g ~ Gini + Idh + dlnCO2*ctfp + GDP_1 + Natural_resources_rents + pca1_nature, data=tbl_ols)

summary(pmdl_06)
# 114 paises com informacoes completas


# ESTIMACAO DESLIGADA
# #  Run a OLS Estimation on the averages
# pmdl_07 <- lm(g ~ Gini + Idh + pca1_nature + dlnCO2*ctfp + hc, data=tbl_ols)
# 
# summary(pmdl_07)



# Arellano–Bond estimator -------------------------------------------------


#  Run a Panel Estimation
pmdl_ab <- pgmm(g ~ Gini + Idh + dlnCO2*ctfp + GDP_1 + Education + Forest_perc + Natural_resources_rents + RuleOfLaw |  lag(GDP_1, 2:4),
                data=tbl,
                effect = "individual",
                model = "onestep",
                index=c("iso3", "year"))

summary(pmdl_ab)


#  Run a Panel Estimation
pmdl_ab2 <- pgmm(g ~ Gini + Idh + dlnCO2*ctfp + GDP_1 + Natural_resources_rents|  lag(GDP_1, 2:4),
                 data=tbl,
                 effect = "individual",
                 model = "onestep",
                 index=c("iso3", "year"))

summary(pmdl_ab2)


write_xlsx(x = list("pmdl_01" = broom::tidy(pmdl_01),
                    "pmdl_02" = broom::tidy(pmdl_02),
                    # "pmdl_03" = broom::tidy(pmdl_03),
                    # "pmdl_04" = broom::tidy(pmdl_04),
                    "pmdl_05" = broom::tidy(pmdl_05),
                    "pmdl_06" = broom::tidy(pmdl_06)),
           # "pmdl_07" = broom::tidy(pmdl_07)),
           path = "Resultado_Regressoes.xlsx")

# PARA AMBOS OS PAINEIS EM AB TOTAL DE AMOSTRA UTILIZADO 456



# Analise grafica ---------------------------------------------------------


tbl %>% 
  colnames()

tbl %>%
  select(iso3, year, Bird, Fish, Mammal, Plant, Land_protected, Marine_protected, Gini, Idh, Education) %>%
  group_by(iso3) %>% 
  summarise(Bird = mean(Bird, na.rm = TRUE),
            Fish = mean(Fish, na.rm = TRUE),
            Mammal = mean(Mammal, na.rm = TRUE),
            Plant = mean(Plant, na.rm = TRUE),
            Gini = mean(Gini), 
            Idh = mean(Idh),
            Education = mean(Education),
            Land_protected = mean(Land_protected, na.rm = TRUE),
            Marine_protected = mean(Marine_protected, na.rm = TRUE)) %>% 
  mutate_at(.vars = c("Bird", "Fish", "Mammal", "Plant", "Land_protected", "Marine_protected",
                      "Gini", "Idh", "Education"),
            .funs = scale) %>%
  pivot_longer(cols = -c(iso3)) %>% 
  ggplot() +
  geom_boxplot(mapping = aes(y = name, colour = name, x = value), show.legend = FALSE) +
  theme_bw() + 
  labs(x = "Z Score", y = "Variable")

mscale <- function(c){as.numeric(scale(c))}

tbl %>%
  select(iso3, Gini, Idh, Education) %>% 
  # group_by(iso3) %>% 
  # summarise(Bird = mean(Bird, na.rm = TRUE),
  #           Fish = mean(Fish, na.rm = TRUE),
  #           Mammal = mean(Mammal, na.rm = TRUE),
  #           Plant = mean(Plant, na.rm = TRUE),
  #           Land_protected = mean(Land_protected, na.rm = TRUE),
  #           Marine_protected = mean(Marine_protected, na.rm = TRUE)) %>% 
  mutate_at(.vars = c("Gini", "Idh", "Education"),
            .funs = mscale) %>%
  pivot_longer(cols = -c(iso3)) %>%
  ggplot() +
  geom_boxplot(mapping = aes(y = name, x = value)) +
  labs(x = NULL, y =NULL) +
  theme_bw()
