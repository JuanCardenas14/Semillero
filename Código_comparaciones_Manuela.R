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

# 4. Crear los dataframe con las columnas de interés

ValRef_sujetos<- ValRef222 %>% 
  select("participant_id" : "lac_5b_x_pkr_dx")

ValRef_male<- ValRef_sujetos %>%
  filter(sex_sub_e1 == "1")

ValRef_female <- ValRef_sujetos %>%
  filter(sex_sub_e1 == "0")

# 5. Crear el data frame para exportar los datos

Comparaciones <- c("shapiro_result_1", 
                   "shapiro_result_2", 
                   "normalidad", 
                   "varianzas", 
                   "el_pe_valor")

Comparaciones<- tibble(Comparaciones)

column_names <- names(ValRef_sujetos)

# 6. Crear el path donde se van a guardar los datos

el_path <- "E:/Informaciòn/Pictures/Desktop/Manuela/Comparaciones/"

# 7. Correr el loop

for (i in (5:length(ValRef_sujetos))) {
  nombres <- column_names[[i]]
  variable_1 <- ValRef_female[[i]]
  variable_2 <- ValRef_male[[i]]
  if (shapiro.test(variable_1)$p.value > 0.05 && 
      shapiro.test(variable_2)$p.value > 0.05) {
    normalidad <- c("Both normal")
    shapiro_result_1 <- shapiro.test(variable_1)$p.value
    shapiro_result_2 <- shapiro.test(variable_2)$p.value
    if (var.test(variable_1, variable_2, 
                 alternative = "two.sided")$p.value > 0.05) {
      varianzas <- c("equal variances")
      the_test <- t.test(variable_1, variable_2, paired = FALSE,
                         alternative = "two.sided", var.equal = TRUE)
      el_pe_valor <- the_test$p.value}
    else {
      varianzas <- c("unequal variances")
      the_test <- t.test(variable_1, variable_2, paired = FALSE,
                         alternative = "two.sided", var.equal = FALSE)
      el_pe_valor <- the_test$p.value} 
  } else {
    normalidad <- c("Either non-normal")
    varianzas <- c("NA")
    shapiro_result_1 <- shapiro.test(variable_1)$p.value
    shapiro_result_2 <- shapiro.test(variable_2)$p.value
    the_test <-wilcox.test(variable_1, variable_2, paired = FALSE)
    el_pe_valor<- the_test$p.value
  }
  nuevacolumna <- c(shapiro_result_1, 
                    shapiro_result_2, 
                    normalidad, 
                    varianzas,
                    el_pe_valor)
  nuevacolumna <- tibble(nuevacolumna)
  Comparaciones <-cbind(Comparaciones, nuevacolumna)
  Comparaciones <- Comparaciones %>%
    plyr::rename(list("nuevacolumna" = paste0(nombres)))
}

# 8. Guardar csv

write.csv(Comparaciones, paste(el_path, "Comparaciones.csv"))

# 9. Deberle un chocolate a Juan