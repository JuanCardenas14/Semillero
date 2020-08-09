library(ggplot2)
library(tidyr)
library(dplyr)
library(readxl)
library(plyr)
library(lubridate)
library(segmented)
library(plotly)

prueba <- read_excel("E:/Informaciòn/Pictures/Desktop/pruebas máximas/Hojas limpias/Base 5b/Excel/3203.xlsx")

View(prueba)

OUES<- prueba %>%
  mutate(Time_min = substr(Fecha, 15,20), VE_log = log(VE)) %>%
  filter(Time_min > "08:00", Load != 0)

model_OUES_100<- OUES %>% lm(data = ., VO2 ~ VE_log)

OUES %>%
  ggplot(., aes(VE_log, VO2)) + geom_point() +
  geom_abline(slope = model_OUES_100$coefficients[[2]], intercept = model_OUES_100[[1]])

