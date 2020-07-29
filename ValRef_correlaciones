# 1. Cargar paquetes

library(ggplot2)
library(tidyr)
library(dplyr)
library(readxl)
library(plyr)
library(lubridate)

# 2. Abrir archivo - ojo con el path

ValRef222 <- read_excel("E:/Informaciòn/Pictures/Downloads/ValRef Concordancia.xlsx", 
                        skip = 1)

# 3. Pasar el tiempo a segundos
ValRef222<- ValRef222 %>%
  mutate(time_bxb_i_exe_dx = as.numeric(ms(time_bxb_i_exe_dx)), 
         lac_5b_x_pkr_dx= as.numeric(lac_5b_x_pkr_dx))

# 4. Crear el data frame con las columnas de interés

ValRef_VO2 <- ValRef222 %>%
  select("vo2_5b_x_pkr_dx", "vo2_mx_b_c_myl_19_2":"vo2_mx_b_c_jon_85_1")

nombres_ecuaciones <- ValRef_VO2 %>%
  names(.)

# 5. Crear el data frame al que se van a exportar los datos

Equation <- c("p", "r")
pearsons <- tibble(Equation)

# 6. Crear el path donde se va a guardar todo

el_path <- "E:/Informaciòn/Pictures/Desktop/Manuela/Pearson/"

# 7. Correr el loop

for (i in (1:length(ValRef_VO2))) {
  ecuacion <- nombres_ecuaciones[i]
  pearson <-cor.test(ValRef_VO2$vo2_5b_x_pkr_dx,ValRef_VO2[[i]], 
                     method = "pearson")
  a <- ggplot(ValRef_VO2, aes(vo2_5b_x_pkr_dx, ValRef_VO2[[i]]))+ 
    geom_point() + labs(y = nombres_ecuaciones[i])
  path <- paste(el_path, 
                nombres_ecuaciones[i], ".jpeg")
  ggsave(a, file = path)
  nuevacolumna <- c(pearson$p.value, pearson$estimate)
  nuevacolumna <- tibble(nuevacolumna)
  pearsons <-cbind(pearsons, nuevacolumna)
  pearsons <- pearsons %>%
    plyr::rename(list("nuevacolumna" = paste0(ecuacion)))
}

# 8. Revisar que el data frame haya quedado bien
View(pearsons)

# 9. Exportar a un csv
write.csv(pearsons, paste(el_path, "pearsons.csv"))

# 10. Deberle un helado a Juan

