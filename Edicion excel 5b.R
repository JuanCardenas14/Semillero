library(openxlsx)
library(ggplot2)
library(tidyr)
library(dplyr)
library(readxl)
library(plyr)
library(lubridate)

path = "/Users/Manuela/Downloads/Datos/"

filenames<- list.files(path, pattern = ".xlsx")
Time_min <- tibble()

for (i in (1:length(filenames))) {
  a <- read_excel(paste(path,filenames[[i]], sep = ""))
  Fecha <- a$Fecha
  p <- a %>%
    mutate(Time_min = substr(Fecha, 15,20))
  sheetname <- substr(filenames[[i]], 1,4)
  write.xlsx(p, file = paste(path,filenames[[i]], sep = ""),
             sheetName = sheetname)
}
