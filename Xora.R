#install.packages("tidyverse")
library(tidyverse)
provincias <- read_csv(destino)
View(provincias)

provincias <- provincias %>%
+     rename(Viviendas_particulares = Viviendas particulares (2022))

# Calculamos la media de personas de Viviendas particulares por provincia
media_Viviendas_particulares <- provincias %>%
+     group_by(Nombre de provincia) %>% #Agrupamos los datos por la columna Nombre.de.provincia.
+     summarise(media_Viviendas_particulares = mean(Viviendas_particulares, na.rm = TRUE))

# Creamos el gráfico de Viviendas particulares por provincias
ggplot(provincias, aes(x = reorder(Nombre de provincia, Viviendas_particulares), y = Viviendas_particulares)) + #indicamos que datos asignamos a cada eje
+     geom_bar(stat = "identity", na.rm = TRUE) + #el tipo de gráfico que realizaremos, podría 
+     coord_flip() +  # Giramos el gráfico para mayor legibilidad
+     labs(title = "Viviendas particulares por Provincias argentinas", #indicamos las etiquetas
+          x = "Provincias",
+          y = "Viviendas particulares") +
+     theme_light() #indicamos el tema, ggplot tiene 8 temas, el genérico es minimal

# Graficamos con barras apiladas la media
ggplot(subset(media_Viviendas_particulares, media_Viviendas_particulares > 0), 
+        aes(x = reorder(Nombre de provincia, media_Viviendas_particulares), y = media_Viviendas_particulares, fill = media_Viviendas_particulares)) +
+     geom_bar(stat = "identity") +
+     labs(title = "Valor medio de viviendas particulares por provincia",
+          x = "Provincia",
+          y = "Valor medio de viviendas particulares") +
+     theme_minimal() +
+     theme(axis.text.x = element_text(angle = 45, hjust = 1),
+           legend.position = "none") #Eliminamos la leyenda

# Gráfico comparativo
ggplot(provincias, aes(x = Nombre de provincia, y = Viviendas_particulares, fill = Viviendas_particulares)) +
+     geom_bar(stat = "identity", position = "dodge", width = 0.9) +
+     scale_fill_viridis_c() + 
+     labs(title = "Viviendas particulares por provincia",
+          x = "Provincia",
+          y = "Viviendas particulares",
+          fill = "Viviendas particulares") +
+     theme_minimal() +
+     theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotamos a 90º las etiquetas del eje x
