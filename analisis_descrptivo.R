#ANALISIS DESCRIPTIVO SINALOA

#abrimos el csv de datos limpios
datos <- read.csv("~/COLMEX/proyecto_sinaloa/datos_limpios.csv", stringsAsFactors = FALSE)

#graficamos porcentaje sexo femenino y masculino
library(ggplot2)

ggplot(datos, aes(x = "", fill = sexo)) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Sexo de desaparecidos", x = NULL, y = NULL) +
  scale_fill_manual(values = c("pink", "lightblue")) +
  #mostrar porcentaje en la leyenda
  geom_text(stat = "count", aes(label = scales::percent(..count../sum(..count..))), position = position_stack(vjust = 0.5)) +
  theme_void() +
  theme(legend.title = element_blank())

#histograma edad
ggplot(datos, aes(x = edad)) +
  geom_histogram(binwidth = 5, fill = "grey", color = "black") +
  labs(title = "Edad de desaparecidos", x = "Edad", y = "Frecuencia") +
  theme_minimal()

#boxplot edad y genero
ggplot(datos, aes(x = sexo, y = edad)) +
  geom_boxplot(fill = c("masculino" ="pink", "femenino"="lightblue"), color = "black") +
  labs(title = "Boxplot de edad de desaparecidos por género", x = NULL, y = "Edad") +
  theme_minimal()


################COMPARACIÓN DATOS LANTIA#############

library(tidyverse)
library(lubridate)

# Cargar datos
df <- read_csv("~/COLMEX/proyecto_sinaloa/victimas_14_2025-07-18.csv")

# Filtrar solo Sinaloa
df_sin <- df %>% filter(ESTADO == "Sin")

# Pivotar a formato largo
df_long <- df_sin %>%
  pivot_longer(cols = matches("\\w{3}/\\d{4}"), names_to = "fecha", values_to = "casos") %>%
  filter(!is.na(casos)) %>%
  mutate(
    fecha = str_replace_all(fecha, c(
      "ene" = "01", "feb" = "02", "mar" = "03", "abr" = "04",
      "may" = "05", "jun" = "06", "jul" = "07", "ago" = "08",
      "sep" = "09", "oct" = "10", "nov" = "11", "dic" = "12"
    )),
    fecha = dmy(paste0("01/", fecha))
  )

#suma todos los casos  filter(fecha >= ymd("2024-08-01") & fecha <= ymd("2025-07-31")) %>%
df_cota <- df_long %>%
  group_by(fecha) %>%filter(fecha >= ymd("2024-08-01") & fecha <= ymd("2025-07-31"))%>%
  summarise(casos = sum(casos, na.rm = TRUE), .groups = 'drop')

suma_total <- sum(df_cota$casos, na.rm = TRUE)

# graficamos los casos por mes
library(ggplot2)
ggplot(df_cota, aes(x = fecha, y = casos)) +
  geom_line(color = "black") +
  labs(title = "Víctimas letales en Sinaloa (Ago 2024 - Jul 2025)", x = "Fecha", y = "Número de víctimas") +
  theme_minimal()

# Paleta de colores por género
colores_genero <- c("Femenino" = "pink", "Masculino" = "lightblue", "No identificado" = "grey")

# --------- GRÁFICO 1: Todo el periodo ---------
df_total <- df_long %>%
  group_by(GÉNERO) %>%
  summarise(total = sum(casos, na.rm = TRUE)) %>%
  mutate(
    porcentaje = total / sum(total),
    etiqueta = paste0(round(porcentaje * 100, 1), "%")
  )

ggplot(df_total, aes(x = "", y = total, fill = GÉNERO)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = colores_genero) +
  geom_text(aes(label = etiqueta), position = position_stack(vjust = 0.5)) +
  labs(title = "Sexo de las víctimas (ene 2018 - jun 2025)", fill = "Género") +
  theme_void()

# --------- GRÁFICO 2: Ago 2024 - Jul 2025 ---------
df_rango <- df_long %>%
  filter(fecha >= ymd("2024-08-01") & fecha <= ymd("2025-07-31")) %>%
  group_by(GÉNERO) %>%
  summarise(total = sum(casos, na.rm = TRUE)) %>%
  mutate(
    porcentaje = total / sum(total),
    etiqueta = paste0(round(porcentaje * 100, 1), "%")
  )

ggplot(df_rango, aes(x = "", y = total, fill = GÉNERO)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = colores_genero) +
  geom_text(aes(label = etiqueta), position = position_stack(vjust = 0.5)) +
  labs(title = "Sexo de las víctimas (ago 2024 - jun 2025)", fill = "Género") +
  theme_void()




#graficos por municipio lantia
# Gráfica de barras - TODO el periodo
df_muni_total <- df_long %>%
  group_by(MUNICIPIO) %>%
  summarise(total = sum(casos, na.rm = TRUE)) %>%
  arrange(desc(total))

ggplot(df_muni_total, aes(x = reorder(MUNICIPIO, total), y = total)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Víctimas por municipio en Sinaloa (ene 2018 - jun 2025)",
       x = "Municipio", y = "Total de víctimas") +
  theme_minimal()

# Gráfica de barras - AGO 2024 a JUL 2025
df_muni_rango <- df_long %>%
  filter(fecha >= ymd("2024-08-01") & fecha <= ymd("2025-07-31")) %>%
  group_by(MUNICIPIO) %>%
  summarise(total = sum(casos, na.rm = TRUE)) %>%
  arrange(desc(total))

ggplot(df_muni_rango, aes(x = reorder(MUNICIPIO, total), y = total)) +
  geom_col(fill = "darkorange") +
  coord_flip() +
  labs(title = "Víctimas por municipio en Sinaloa (ago 2024 - jul 2025)",
       x = "Municipio", y = "Total de víctimas") +
  theme_minimal()



#EDADES LANTIA 
library(tidyverse)

# Datos de porcentaje por grupo de edad
edad_df <- tibble(
  grupo_edad = factor(c("< 20", "20s", "30s", "40s", "50 +"),
                      levels = c("< 20", "20s", "30s", "40s", "50 +")),
  porcentaje = c(6.4, 27.5, 31.2, 20.1, 14.8)
)

# Gráfico de barras verticales con etiquetas
ggplot(edad_df, aes(x = grupo_edad, y = porcentaje)) +
  geom_col(fill = "gray80", width = 0.7) +
  geom_text(aes(label = paste0(porcentaje, "%")), vjust = -0.5, size = 5) +
  labs(title = "Edad de las víctimas en Sinaloa (ene 2018 - jun 2025)", x = NULL, y = NULL) +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    #plot.title = element_text(face = "bold")
  ) +
  ylim(0, 35)
