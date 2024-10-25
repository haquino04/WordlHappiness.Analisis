
#World Happiness Report 
#----------------------------------------------------------------

#install.packages(c("readr", "tidyverse"))

#Cargamos librerias
library(tidyverse)
library(readr)




#Limpiamos el work space, Cambiamos el directorio de trabajo y fijamos UTF-8 para manipulación de caracteres
rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
options(encoding = "utf-8")


# Márgenes de visualización
par(mar=c(5,5,5,5))
felicidad <- read_csv("World Happiness Report.csv")
felicidad

# Gráfico 1
p1 <- felicidad %>%
  # Filtrar los datos, excluyendo Oceanía y Antártica
  filter(!continent %in% c("Oceania", "Antarctica")) %>%
  ggplot(aes(x = corruption_percep, y = life_sat)) +
  
  # Pintamos todos los puntos en gris claro como referencia
  geom_point(aes(size = corruption_percep), alpha = 0.6, color = "lightgrey") +
  
  # Resaltamos los datos de Asia en color rojo
  geom_point(data = filter(felicidad, continent == "Asia"), 
             aes(size = corruption_percep), color = "red", alpha = 0.8) +
  
  # Resaltamos a Perú en color verde con mayor tamaño
  geom_point(data = filter(felicidad, country == "Peru"), 
             aes(size = corruption_percep), color = "green", size = 5, alpha = 1) +
  
  # Ajustar el rango de tamaños de los puntos
  scale_size(range = c(2, 10), name = "Percepción de Corrupción") +
  
  # Escala de colores más suave para evitar el gris sobre gris
  scale_color_brewer(palette = "Set1") +
  
  # Etiquetas más descriptivas para los ejes
  labs(
    title = "Relación entre Percepción de Corrupción y Satisfacción con la Vida",
    subtitle = "Diferentes continentes resaltando Asia y Perú",
    x = "Percepción de Corrupción",
    y = "Satisfacción con la Vida",
    size = "Percepción de Corrupción"
  ) +
  
  # Mejoramos la escala del eje Y con divisiones más claras
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2), minor_breaks = NULL) +
  
  # Temas más claros y atractivos visualmente
  theme_minimal() + 
  
  # Mejorar el diseño general
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  )

# Mostrar el gráfico
p1



p2 <- felicidad %>%
  # Filtrar datos, excluyendo Oceanía y Antártica
  filter(!continent %in% c("Oceania", "Antarctica")) %>%
  
  ggplot(aes(x = gdp_cap, y = life_sat)) +
  
  # Pintar puntos grises para todos los datos
  geom_point(aes(size = pop), alpha = 0.6, color = "lightgrey") +
  
  # Resaltar los países de Europa en color azul
  geom_point(data = filter(felicidad, continent == "South America"), 
             aes(size = pop), color = "blue", alpha = 0.8) +
  
  # Ajustar el rango de los tamaños de los puntos basados en la población
  scale_size(range = c(2, 12), name = "Población") +
  
  # Aplicar una escala logarítmica al eje X para el PIB per cápita
  scale_x_log10(labels = scales::dollar_format(scale = 1e-3, suffix = "k"),
                breaks = scales::log_breaks(n = 10)) +
  
  # Ajustar la escala del eje Y para la satisfacción con la vida
  scale_y_continuous(breaks = seq(0, 10, by = 2), limits = c(0, 10), minor_breaks = NULL) +
  
  # Añadir etiquetas claras
  labs(
    title = "Relación entre PIB per cápita y Satisfacción con la Vida",
    subtitle = "Países de South America destacados en azul",
    x = "PIB per cápita (log)", 
    y = "Satisfacción con la Vida",
    size = "Población"
  ) +
  
  # Aplicar tema minimalista
  theme_minimal() +
  
  # Mejorar el diseño del gráfico con ajustes de tema
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  )

# Mostrar el gráfico mejorado
p2

library(ggrepel)


p3 <- felicidad %>%
  # Filtrar datos, excluyendo Oceanía y Antártica
  filter(!continent %in% c("Oceania", "Antarctica")) %>%
  
  ggplot(aes(x = gdp_cap, y = life_sat)) +
  
  # Pintar puntos grises para todos los datos
  geom_point(aes(size = pop), alpha = 0.6, color = "lightgrey") +
  
  # Resaltar los países de Europa en color azul
  geom_point(data = filter(felicidad, continent == "Europe"), 
             aes(size = pop), color = "blue", alpha = 0.8) +
  
  # Agregar las etiquetas de los códigos de los países en cada punto
  geom_text_repel(data = felicidad, aes(label = code), size = 3) +
  
  # Ajustar el rango de los tamaños de los puntos basados en la población
  scale_size(range = c(2, 12), name = "Población") +
  
  # Aplicar una escala logarítmica al eje X para el PIB per cápita
  scale_x_log10(labels = scales::dollar_format(scale = 1e-3, suffix = "k"),
                breaks = scales::log_breaks(n = 10)) +
  
  # Ajustar la escala del eje Y para la satisfacción con la vida
  scale_y_continuous(breaks = seq(0, 10, by = 2), limits = c(0, 10), minor_breaks = NULL) +
  
  # Añadir etiquetas claras
  labs(
    title = "Relación entre PIB per cápita y Satisfacción con la Vida",
    subtitle = "Países de Europa destacados en azul",
    x = "PIB per cápita (log)", 
    y = "Satisfacción con la Vida",
    size = "Población"
  ) +
  
  # Aplicar tema minimalista
  theme_minimal() +
  
  # Mejorar el diseño del gráfico con ajustes de tema
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  )

# Mostrar el gráfico con etiquetas
p3

p4 <- felicidad %>%
  # Filtrar datos, excluyendo Oceanía y Antártica
  filter(!continent %in% c("Oceania", "Antarctica")) %>%
  
  ggplot(aes(x = gdp_cap, y = life_sat)) +
  
  # Pintar puntos grises para todos los datos
  geom_point(aes(size = pop), alpha = 0.6, color = "lightgrey") +
  
  # Resaltar los países de Europa en color azul
  geom_point(data = filter(felicidad, continent == "South America"), 
             aes(size = pop), color = "blue", alpha = 0.8) +
  
  # Agregar etiquetas solo para países del continente "Europe"
  geom_text_repel(data = filter(felicidad, continent == "South America"), 
                  aes(label = code), size = 3, color = "black") +
  
  # Ajustar el rango de los tamaños de los puntos basados en la población
  scale_size(range = c(2, 12), name = "Población") +
  
  # Aplicar una escala logarítmica al eje X para el PIB per cápita
  scale_x_log10(labels = scales::dollar_format(scale = 1e-3, suffix = "k"),
                breaks = scales::log_breaks(n = 10)) +
  
  # Ajustar la escala del eje Y para la satisfacción con la vida
  scale_y_continuous(breaks = seq(0, 10, by = 2), limits = c(0, 10), minor_breaks = NULL) +
  
  # Añadir etiquetas claras
  labs(
    title = "Relación entre PIB per cápita y Satisfacción con la Vida",
    subtitle = "Países de South America destacados con etiquetas",
    x = "PIB per cápita (log)", 
    y = "Satisfacción con la Vida",
    size = "Población"
  ) +
  
  # Aplicar tema minimalista
  theme_minimal() +
  
  # Mejorar el diseño del gráfico con ajustes de tema
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  )

# Mostrar el gráfico con etiquetas para Europa
p4

#install.packages("gridExtra")
library(gridExtra)
gridExtra::grid.arrange(p1, p2, ncol=2)


# Otros ejemplos ordenados
felicidad %>%
  filter(!continent %in% c("Oceania", "Antarctica")) %>%
  
  ggplot(mapping = aes(x = gdp_cap, y = life_sat, color = continent)) + 
  
  # Puntos que varían de tamaño según la población
  geom_point(mapping = aes(size = pop), alpha = 0.7) +
  
  # Línea de suavización con nivel de confianza por continente
  geom_smooth(method = "loess", se = TRUE, aes(fill = continent), alpha = 0.2) +
  
  # Escala logarítmica en el eje X para el PIB per cápita
  scale_x_log10(labels = scales::dollar_format(scale = 1e-3, suffix = "k")) +
  
  # Escala continua en el eje Y para la satisfacción con la vida
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 2)) +
  
  # Paleta de colores definida para cada continente
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  
  # Añadir etiquetas a los ejes y un título al gráfico
  labs(
    title = "Relación entre PIB per cápita y Satisfacción con la Vida",
    subtitle = "Datos por continente con suavización local",
    x = "PIB per cápita (log)", 
    y = "Satisfacción con la Vida",
    size = "Población",
    color = "Continente"
  ) +
  
  # Aplicar un tema minimalista para mejorar la visualización
  theme_minimal() +
  
  # Ajustar el tema, centrando títulos y mejorando la legibilidad
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    legend.position = "right"
  )


felicidad %>%
  filter(!continent %in% c("Oceania", "Antarctica")) %>%
  
  ggplot(mapping = aes(x = gdp_cap, y = life_sat, color = continent)) +
  
  # Dividir el gráfico en facetas por continente
  facet_wrap(~ continent, scales = "free_x", ncol = 3) +
  
  # Dibujar puntos con tamaño basado en la población
  geom_point(mapping = aes(size = pop), alpha = 0.7) +
  
  # Añadir una línea de suavización (diferenciada por continente)
  geom_smooth(method = "loess", se = TRUE, aes(fill = continent), alpha = 0.2, color = "black") +
  
  # Escala logarítmica para el PIB per cápita (gdp_cap)
  scale_x_log10(labels = scales::dollar_format(scale = 1e-3, suffix = "k")) +
  
  # Ajustes en la escala de satisfacción con la vida
  scale_y_continuous(breaks = seq(0, 10, by = 2), limits = c(0, 10)) +
  
  # Paleta de colores definida para cada continente
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  
  # Añadir títulos y etiquetas a los ejes
  labs(
    title = "Relación entre PIB per cápita y Satisfacción con la Vida",
    subtitle = "Facetas por continente con suavización local",
    x = "PIB per cápita (log)",
    y = "Satisfacción con la Vida",
    size = "Población",
    color = "Continente"
  ) +
  
  # Aplicar un tema minimalista para mejorar la visualización
  theme_minimal() +
  
  # Mejorar el tema del gráfico para que sea más claro
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    strip.text = element_text(size = 12, face = "bold"),  # Mejora el texto de las facetas
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    legend.position = "right"
  )


library(scales)  # Para las escalas

felicidad %>%
  filter(!continent %in% c("Oceania", "Antarctica")) %>%
  
  ggplot(mapping = aes(x = gdp_cap, y = life_sat)) +
  
  # Dibujar los puntos con tamaño basado en la población
  geom_point(mapping = aes(size = pop, color = continent), alpha = 0.8) +
  
  # Añadir líneas de suavización diferenciadas por continente
  geom_smooth(mapping = aes(color = continent), method = "loess", se = TRUE, size = 1) +
  
  # Escala logarítmica para PIB per cápita
  scale_x_log10(labels = dollar_format(scale = 1e-3, suffix = "k")) +
  
  # Ajustar la escala del eje Y (Satisfacción con la vida)
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2), labels = seq(0, 10, by = 2)) +
  
  # Ajustar el tamaño de los puntos (población)
  scale_size_continuous(range = c(2, 12)) +
  
  # Añadir colores diferenciados por continente
  scale_color_brewer(palette = "Set1") +
  
  # Añadir etiquetas, títulos y leyendas
  labs(
    title = "Relación entre PIB per cápita y Satisfacción con la Vida",
    subtitle = "Datos por Continente con Suavización",
    x = "PIB per cápita (logarítmico)",
    y = "Satisfacción con la Vida",
    size = "Población",
    color = "Continente"
  ) +
  
  # Tema limpio y minimalista
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )


# Facets (small multiples)
library(scales)  # Para formatear los números

# Mejorar el gráfico con modificaciones adicionales
ggplot(data = felicidad) + 
  
  # Dividir por continentes
  facet_wrap(~continent) +
  
  # Gráfico de dispersión, color por continente y tamaño por población
  geom_point(mapping = aes(x = gdp_cap, y = life_sat, color = continent, size = pop), alpha = 0.8) +
  
  # Añadir escala logarítmica para PIB per cápita
  scale_x_log10(labels = dollar_format(scale = 1e-3, suffix = "k")) +
  
  # Escala para los tamaños de los puntos (población)
  scale_size_continuous(range = c(2, 12)) +
  
  # Usar una paleta de colores para diferenciar continentes
  scale_color_brewer(palette = "Set1") +
  
  # Títulos y etiquetas
  labs(
    title = "Relación entre PIB per cápita y Satisfacción con la Vida",
    subtitle = "Datos por Continente",
    x = "PIB per cápita (logarítmico)",
    y = "Satisfacción con la Vida",
    size = "Población",
    color = "Continente"
  ) +
  
  # Tema minimalista
  theme_minimal() +
  
  # Ajustar el tamaño y estilo del texto
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )


library(dplyr)
#install.packages("forcats")
library(forcats)
# Gráficos de barras (transformación)
totales <- felicidad %>%
  mutate(continent = fct_relevel(continent, "Europe", "Africa") ) %>%
  group_by(continent) %>%
  summarise( media = mean(life_sat, na.rm=T) ) %>%
  mutate(continent = fct_reorder(continent, -media ) )
totales

totales %>%
  ggplot(aes(x = continent, fill=continent, color=continent) ) + 
  geom_col(aes(y=media), alpha=0.2, color=NA ) +
  geom_jitter(data=felicidad, aes(y=life_sat), alpha=0.25, width=0.1 ) +
  theme_minimal()



############################
# Gráfico de dispersión (Scatter Plot): Relación entre PIB per cápita y satisfacción con la vida
# Este gráfico te permitirá ver si existe una correlación entre el PIB per cápita (gdp_cap) 
# y el nivel de satisfacción con la vida (life_sat).

ggplot(data = felicidad, aes(x = gdp_cap, y = life_sat)) +
  geom_point(aes(color = continent, size = pop), alpha = 0.7) +
  scale_x_continuous(trans = 'log10') +
  labs(title = "Relación entre PIB per cápita y Satisfacción con la vida",
       x = "PIB per cápita (escala log)", y = "Satisfacción con la vida",
       color = "Continente", size = "Población") +
  theme_minimal()

# Gráfico de barras apiladas: Comparación del nivel de corrupción percibida entre continentes
# Puedes comparar la percepción de corrupción en cada continente mediante un gráfico de barras apiladas.
ggplot(data = felicidad, aes(x = continent, y = corruption_percep, fill = continent)) +
  geom_bar(stat = "identity") +
  labs(title = "Percepción de la corrupción por continente",
       x = "Continente", y = "Percepción de la corrupción") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")


# Gráfico de densidad: Distribución de la felicidad en diferentes continentes
# Este gráfico te permite ver cómo varía la satisfacción con la vida entre diferentes continentes.
ggplot(data = felicidad, aes(x = life_sat, fill = continent)) +
  geom_density(alpha = 0.6) +
  labs(title = "Distribución de la satisfacción con la vida por continente",
       x = "Satisfacción con la vida", y = "Densidad") +
  theme_minimal()

