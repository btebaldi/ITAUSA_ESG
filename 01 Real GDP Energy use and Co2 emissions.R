
# Setup -------------------------------------------------------------------

rm(list = ls())

library(readxl)
library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)


# Dataload ----------------------------------------------------------------

# Co2 Data
DT <- fread("./database/owid-co2-data.csv")


PIB <- read_excel("database/tabela1620 - PIB.xlsx", range = cell_limits(ul = c(5, 1), lr = c(NA, 2)))
colnames(PIB) <- c("Data", "PIB")
PIB$Data <- seq(from = as.Date("1996-01-01"), to = as.Date("2023-07-01"), by="quarter")

Luz <- read_excel("database/Energia ipeadata [33442].xls", range = cell_limits(ul = c(1, 1), lr = c(NA, 2)))
colnames(Luz) <- c("Ano", "Energia")
Luz <- Luz %>% mutate(date = ymd(Ano, truncated = 2))


# Data Regularization -----------------------------------------------------


DT <- DT[iso_code == "BRA", .(year, co2, date = ymd(year, truncated = 2))] |>
  merge(PIB, by.x = c("date"), by.y=c("Data")) |>
  merge(Luz, by.x = c("date"), by.y=c("date"))


DT[, co2 := 100*co2/co2[1]]
DT[, GDP := 100*PIB/PIB[1]]
DT[, KWH := 100*Energia/Energia[1]]


DT |>
  ggplot() + 
  geom_line(aes(x = year, y = GDP, linetype = "Real GDP"), size = 1) + 
  geom_line(aes(x = year, y = co2, linetype = "CO2"), size = 1, colour = "red") + 
  geom_line(aes(x = year, y = KWH, linetype = "Energy"), size = 1) + 
  geom_ribbon(aes(x = year, ymax = GDP, ymin = pmin(KWH, GDP), fill = "A"), alpha = 0.8) +
  geom_ribbon(aes(x = year, ymax = KWH, ymin = pmin(co2, KWH), fill = "B"), alpha = 0.8) +
  
  geom_ribbon(aes(x = year, ymax = GDP, ymin = pmax(KWH, GDP), fill = "C"), alpha = 0.8) +
  geom_ribbon(aes(x = year, ymax = KWH, ymin = pmax(co2, KWH), fill = "D"), alpha = 0.8) +
  
  scale_linetype_manual(values=c("solid", "dashed", "dotted"),
                        breaks = c("Real GDP", "CO2", "Energy")) +
  scale_fill_manual(values=c("#339966", "#006600", "#FF9966", "#FF6600"),
                    breaks = c("B", "A", "D", "C"),
                    labels = c("Additional gain from greening", "Energy-productivuty gains",
                               "Additional loss from greening", "Energy-productivuty loss")) +
  theme_bw()+ylim(100,200)+
  theme(legend.position = "bottom") +
  labs(title = "Real GDP, Energy use and Co2 emissions",
       linetype = NULL, fill = NULL,
       y = NULL,
       x = NULL)


ggsave(filename = "Todos.png", scale = 1, width = 10.24, path ="Graficos", dpi = 100)


# Real GDP ----------------------------------------------------------------

DT |>
  ggplot() + 
  geom_line(aes(x = year, y = GDP, linetype = "Real GDP"), size = 1) + 
  # geom_line(aes(x = year, y = co2, linetype = "CO2"), size = 1, colour = "red") + 
  # geom_line(aes(x = year, y = KWH, linetype = "Energy"), size = 1) + 
  # geom_ribbon(aes(x = year, ymax = GDP, ymin = pmin(KWH, GDP), fill = "A"), alpha = 0.8) +
  # geom_ribbon(aes(x = year, ymax = KWH, ymin = pmin(co2, KWH), fill = "B"), alpha = 0.8) +
  # 
  # geom_ribbon(aes(x = year, ymax = GDP, ymin = pmax(KWH, GDP), fill = "C"), alpha = 0.8) +
  # geom_ribbon(aes(x = year, ymax = KWH, ymin = pmax(co2, KWH), fill = "D"), alpha = 0.8) +
  
  scale_linetype_manual(values=c("solid", "dashed", "dotted"),
                        breaks = c("Real GDP", "CO2", "Energy")) +
  scale_fill_manual(values=c("#339966", "#006600", "#FF9966", "#FF6600"),
                    breaks = c("B", "A", "D", "C"),
                    labels = c("Additional gain from greening", "Energy-productivuty gains",
                               "Additional loss from greening", "Energy-productivuty loss")) +
  theme_bw()+ylim(100,200)+
  theme(legend.position = "bottom") +
  labs(title = "Real GDP",
       linetype = NULL, fill = NULL,
       y = NULL,
       x = NULL)

ggsave(filename = "PIB.png", scale = 1, width = 10.24, path ="Graficos", dpi = 100)

# CO2 Emissions -----------------------------------------------------------

DT |>
  ggplot() + 
  # geom_line(aes(x = year, y = GDP, linetype = "Real GDP"), size = 1) + 
  geom_line(aes(x = year, y = co2, linetype = "CO2"), size = 1, colour = "red") +
  # geom_line(aes(x = year, y = KWH, linetype = "Energy"), size = 1) + 
  # geom_ribbon(aes(x = year, ymax = GDP, ymin = pmin(KWH, GDP), fill = "A"), alpha = 0.8) +
  # geom_ribbon(aes(x = year, ymax = KWH, ymin = pmin(co2, KWH), fill = "B"), alpha = 0.8) +
  # 
  # geom_ribbon(aes(x = year, ymax = GDP, ymin = pmax(KWH, GDP), fill = "C"), alpha = 0.8) +
  # geom_ribbon(aes(x = year, ymax = KWH, ymin = pmax(co2, KWH), fill = "D"), alpha = 0.8) +
  
  scale_linetype_manual(values=c("solid", "dashed", "dotted"),
                        breaks = c("Real GDP", "CO2", "Energy")) +
  scale_fill_manual(values=c("#339966", "#006600", "#FF9966", "#FF6600"),
                    breaks = c("B", "A", "D", "C"),
                    labels = c("Additional gain from greening", "Energy-productivuty gains",
                               "Additional loss from greening", "Energy-productivuty loss")) +
  theme_bw()+ylim(100,200)+
  theme(legend.position = "bottom") +
  labs(title = "CO2 Emissions",
       linetype = NULL, fill = NULL,
       y = NULL,
       x = NULL)

ggsave(filename = "CO2.png", scale = 1, width = 10.24, path ="Graficos", dpi = 100)

# Energy ------------------------------------------------------------------

DT |>
  ggplot() + 
  # geom_line(aes(x = year, y = GDP, linetype = "Real GDP"), size = 1) + 
  # geom_line(aes(x = year, y = co2, linetype = "CO2"), size = 1, colour = "red") + 
  geom_line(aes(x = year, y = KWH, linetype = "Energy"), size = 1) +
  # geom_ribbon(aes(x = year, ymax = GDP, ymin = pmin(KWH, GDP), fill = "A"), alpha = 0.8) +
  # geom_ribbon(aes(x = year, ymax = KWH, ymin = pmin(co2, KWH), fill = "B"), alpha = 0.8) +
  # 
  # geom_ribbon(aes(x = year, ymax = GDP, ymin = pmax(KWH, GDP), fill = "C"), alpha = 0.8) +
  # geom_ribbon(aes(x = year, ymax = KWH, ymin = pmax(co2, KWH), fill = "D"), alpha = 0.8) +
  
  scale_linetype_manual(values=c("solid", "dashed", "dotted"),
                        breaks = c("Real GDP", "CO2", "Energy")) +
  scale_fill_manual(values=c("#339966", "#006600", "#FF9966", "#FF6600"),
                    breaks = c("B", "A", "D", "C"),
                    labels = c("Additional gain from greening", "Energy-productivuty gains",
                               "Additional loss from greening", "Energy-productivuty loss")) +
  theme_bw()+ylim(100,200)+
  theme(legend.position = "bottom") +
  labs(title = "Energy",
       linetype = NULL, fill = NULL,
       y = NULL,
       x = NULL)

ggsave(filename = "energy.png", scale = 1, width = 10.24, path ="Graficos", dpi = 100)

# Real GDP vs Energy ------------------------------------------------------

DT |>
  ggplot() + 
  geom_line(aes(x = year, y = GDP, linetype = "Real GDP"), size = 1) + 
  # geom_line(aes(x = year, y = co2, linetype = "CO2"), size = 1, colour = "red") +
  geom_line(aes(x = year, y = KWH, linetype = "Energy"), size = 1) +
  
  geom_ribbon(aes(x = year, ymax = GDP, ymin = pmin(KWH, GDP), fill = "A"), alpha = 0.8) +
  geom_ribbon(aes(x = year, ymax = GDP, ymin = pmax(KWH, GDP), fill = "C"), alpha = 0.8) +
  
  # geom_ribbon(aes(x = year, ymax = KWH, ymin = pmin(co2, KWH), fill = "B"), alpha = 0.8) +
  
  # geom_ribbon(aes(x = year, ymax = KWH, ymin = pmax(co2, KWH), fill = "D"), alpha = 0.8) +
  
  scale_linetype_manual(values=c("solid", "dashed", "dotted"),
                        breaks = c("Real GDP", "CO2", "Energy")) +
  scale_fill_manual(values=c("#339966", "#006600", "#FF9966", "#FF6600"),
                    breaks = c("B", "A", "D", "C"),
                    labels = c("Additional gain from greening", "Energy-productivuty gains",
                               "Additional loss from greening", "Energy-productivuty loss")) +
  theme_bw()+ylim(100,200)+
  theme(legend.position = "bottom") +
  labs(title = "Real GDP vs Energy",
       linetype = NULL, fill = NULL,
       y = NULL,
       x = NULL)

ggsave(filename = "PIB-Energy.png", scale = 1, width = 10.24, path ="Graficos", dpi = 100)

# Energy vs CO2 -----------------------------------------------------------

DT |>
  ggplot() + 
  # geom_line(aes(x = year, y = GDP, linetype = "Real GDP"), size = 1) + 
  geom_line(aes(x = year, y = co2, linetype = "CO2"), size = 1, colour = "red") +
  geom_line(aes(x = year, y = KWH, linetype = "Energy"), size = 1) +
  
  # geom_ribbon(aes(x = year, ymax = GDP, ymin = pmin(KWH, GDP), fill = "A"), alpha = 0.8) +
  # geom_ribbon(aes(x = year, ymax = GDP, ymin = pmax(KWH, GDP), fill = "C"), alpha = 0.8) +
  
  geom_ribbon(aes(x = year, ymax = KWH, ymin = pmin(co2, KWH), fill = "B"), alpha = 0.8) +
  geom_ribbon(aes(x = year, ymax = KWH, ymin = pmax(co2, KWH), fill = "D"), alpha = 0.8) +
  
  scale_linetype_manual(values=c("solid", "twodash", "dotted"),
                        breaks = c("Real GDP", "CO2", "Energy")) +
  scale_fill_manual(values=c("#339966", "#006600", "#FF9966", "#FF6600"),
                    breaks = c("B", "A", "D", "C"),
                    labels = c("Additional gain from greening", "Energy-productivuty gains",
                               "Additional loss from greening", "Energy-productivuty loss")) +
  theme_bw()+ylim(100,200)+
  theme(legend.position = "bottom") +
  labs(title = "Energy vs CO2",
       linetype = NULL, fill = NULL,
       y = NULL,
       x = NULL)

ggsave(filename = "Co2-Energy.png", scale = 1, width = 10.24, path ="Graficos", dpi = 100)

