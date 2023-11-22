
# Setup -------------------------------------------------------------------

rm(list = ls())

library(data.table)
library(dplyr)
library(ggplot2)
library(ggcorrplot)
library(plm)

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
              Gini = wdi_SI.POV.GINI,
              Idh = hdi,
              Forest = wdi_AG.LND.FRST.K2,
              Forest_perc = wdi_AG.LND.FRST.ZS,
              Ich = wdi_HD.HCI.OVRL,
              GDP = wdi_NY.GDP.PCAP.KD,
              GDP_r = wdi_NY.GDP.PCAP.PP.KD,
              RuleOfLaw = wdi_RL.EST,
              Land_n_Marine = wdi_ER.PTD.TOTL.ZS)




summary(tbl)

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
summary(tbl)


# Correlation matrix
corr <- cor(tbl[, .(Forest, Forest_perc, CO2,
                    Greenhouse, Bird, Fish, Plant, Mammal, Land_protected, Marine_protected,
                    Land_n_Marine, Ich, GDP, GDP_r, Natural_resources_rents, RuleOfLaw, Gini,
                    Idh, pwt_pop, pwt_hc, pwt_ctfp, g, GDP_1)],
            use = "pairwise.complete.obs")

# Grafico -----------------------------------------------------------------
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



# Correlation matrix
corr <- cor(tbl[, .(Gini, Idh)],
            use = "pairwise.complete.obs")

tbl[, .(x = sum(is.na(Land_protected))), iso3] |> dplyr::arrange(desc(x))
tbl[, .(x = sum(is.na(Marine_protected))), iso3] |> dplyr::arrange(desc(x))
tbl[, .(x = sum(is.na(Land_n_Marine))), iso3] |> dplyr::arrange(desc(x))

tbl[, .(x = sum(is.na(pwt_ctfp))), iso3] |> dplyr::arrange(desc(x))->bb


tbl[is.na(Land_n_Marine)]
my_pwt10 <- pwt10::pwt10.01 %>% select(isocode, year, ctfp)

tbl <- left_join(tbl, my_pwt10, by = c("iso3"="isocode", "year"="year"))

pca <- prcomp(tbl[, .(Bird, Fish, Plant, Mammal)], scale = TRUE)
summary(pca)
tbl$pca1_nature <- pca$x[, 1]

summary(tbl)
colnames(tbl)
plm(g ~ Gini + Idh + pca1_nature + dlnCO2*ctfp + GDP_1 + pwt_hc + Forest_perc,
    data=tbl,
    index=c("iso3", "year"),
    model="within") %>% summary()

lm(g ~ Gini + Idh + pca1_nature + dlnCO2*ctfp + GDP_1 + pwt_hc - 1,
    data=tbl) %>% summary()


plm(y ~ x1, data=Panel, index=c("country", "year"), model="within")

lm(RuleOfLaw~Forest_perc, data = tbl) %>% summary()





tbl[iso3 %chin% c("AUS"), wdi_EN.MAM.THRD.NO]
tbl[iso3 %chin% c("AUS"), wdi_EN.MAM.THRD.NO]

range(tbl$wdi_HD.HCI.OVRL, na.rm = TRUE)

tbl |>
  ggplot(aes(x = wdi_HD.HCI.OVRL, y = wdi_SI.POV.GINI)) + 
  geom_point(mapping = aes(colour = iso3)) + 
  geom_smooth(method = "lm", show.legend = FALSE) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(y = "Gini", x = "IDH") #+ xlim(0.28, 0.89)

tbl |>
  ggplot(aes(x = hdi, y = wdi_SI.POV.GINI)) + 
  geom_point( alpha = 0.5) + 
  geom_smooth(method = "lm", show.legend = FALSE) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(y = "Gini", x = "IDH")



lm(wdi_SI.POV.GINI ~ hdi, data = tbl) |> summary()
lm(wdi_HD.HCI.OVRL ~ hdi, data = tbl) |> summary()

hist(tbl$wdi_NY.GDP.PCAP.KD, breaks = "FD")
hist(log(tbl$wdi_NY.GDP.PCAP.KD), breaks = "FD")

setorderv(tbl, c("iso3","year"))










# Load database -----------------------------------------------------------

colnames(tbl)

# Correlation matrix
corr <- cor(tbl[, .("Bird species, threatened" = Bird,
                    "Fish species, threatened" = Fish, 
                    "Plant species (higher), threatened" = Plant, 
                    "Mammal species, threatened" = Mammal, 
                    "Terrestrial protected areas" = wdi_ER.LND.PTLD.ZS, 
                    "Marine protected areas" = wdi_ER.MRN.PTMR.ZS,
                    "Forest area (sq. km)" = wdi_AG.LND.FRST.K2,
                    "Forest area (% of land area)" = wdi_AG.LND.FRST.ZS,
                    "CO2 emissions (kt)" = wdi_EN.ATM.CO2E.KT,
                    "Total greenhouse gas emissions" = wdi_EN.ATM.GHGT.KT.CE,
                    "Total natural resources rents (% of GDP)" = wdi_NY.GDP.TOTL.RT.ZS)],
            use = "pairwise.complete.obs")

# Grafico -----------------------------------------------------------------
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


ggsave(filename = "./Graficos/Aula 1/15_Correlograma.png",units = "in",
       width = 8, height = 6,dpi = 100)


