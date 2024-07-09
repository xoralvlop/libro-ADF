url<- "https://raw.githubusercontent.com/rominicky/materiales/main/assets/Provincias.csv"
destino <- "./provincias.csv"
download.file(url, destino)
library("readr")
provincias <- read_csv(destino)
install.packages("tidyverse")
library(tidyverse)
provincias <- read.csv('C:/Users/xoral/OneDrive/Documentos/libro-ADF/Provincias.csv')
View(provincias)
provincias <- provincias %>%
   rename(Viviendas_particulares = Viviendas.particulares..2022.)

# Calculamos la media de viviendas particulares por provincia
media_Viviendas_particulares <- provincias %>%
       group_by(Nombre.de.provincia) %>% #Agrupamos los datos por la columna Nombre.de.provincia.
       summarise(media_Viviendas_particulares = mean(Viviendas_particulares, na.rm = TRUE)) #Calculamos la media de la columna Viviendas_particulares por cada provincia agrupada, generamos una nueva columna 'media_Viviendas_particulares

# Creamos el gráfico de viviendas particulares por provincias
ggplot(provincias, aes(x = reorder(Nombre.de.provincia, Viviendas_particulares), y = Viviendas_particulares)) + #indicamos que datos asignamos a cada eje
       geom_bar(stat = "identity", na.rm = TRUE) + #el tipo de gráfico que realizaremos, podría 
       coord_flip() +  # Giramos el gráfico para mayor legibilidad
       labs(title = "Viviendas particulares por Provincias argentinas", #indicamos las etiquetas
            x = "Provincias",
            y = "Viviendas particulares") +
       theme_light() 

 # Graficamos con barras apiladas la media
ggplot(subset(media_Viviendas_particulares, media_Viviendas_particulares > 0), 
          aes(x = reorder(Nombre.de.provincia, media_Viviendas_particulares), y = media_Viviendas_particulares, fill = media_Viviendas_particulares)) +
       geom_bar(stat = "identity") +
       labs(title = "Valor medio de viviendas particulares por provincia",
            x = "Provincia",
            y = "Valor medio de viviendas particulares") +
       theme_minimal() +
       theme(axis.text.x = element_text(angle = 80, hjust = 1),
             legend.position = "none") 

# Gráfico comparativo
ggplot(provincias, aes(x = Nombre.de.provincia, y = Población..2022., fill = Viviendas_particulares)) +
       geom_bar(stat = "identity", position = "dodge", width = 0.9) +
       scale_fill_viridis_c() +  # Usaremos una paleta con colores contrastantes
       labs(title = "Comparación de Población 2022 y Viviendas particulares por provincia",
                       x = "Provincia",
                       y = "Población 2022",
                       fill = "Viviendas particulares") +
       theme_minimal() +
       theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotamos a 90º las etiquetas del eje x
