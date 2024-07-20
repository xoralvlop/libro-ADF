#entorno limpio
rm(list = ls()) #limpiamos el entorno

getwd() #obtenemos dirección de la carpeta de trabajo, acá puedo cambiarla con setwd("direccion")
#ubico en carpeta deseada
setwd("C:/carpeta")

#instalo librerías necesarias
install.packages("tidyverse") #tidyverse y tidytext 
#Estas librerías sirven para limpiar y transformar datos en R.
install.packages("tidytext") #librería Tidytext permite convertir datos textuales a estructurados.
install.packages("ggplot2")
install.packages("tm") #librería que permite realizar minería de textos
install.packages("wordcloud") #creará nube de palabras
install.packages("RColorBrewer") #añado una paleta de colores para la nube


# Cargar las librerías necesarias
library(tidyverse)
library(tidytext)
library(ggplot2)
library(tm)
library(wordcloud)
library(RColorBrewer)

#descargar el texto
url <- "https://www.url.txt"
destino <- "./libro-gutengerg.txt"
download.file(url, destino, mode="wb")

# Cargar el texto desde el archivo descargado
text_data <- readLines(destino, encoding = "UTF-8")


# Convertir el texto a un data frame
text_df <- data.frame(line = 1:length(text_data), text = text_data)

#Preprocesar el texto
text_df <- text_df %>% #token se llama a la parte más pequeña de una secuencia de texto
  unnest_tokens(word, text) #unnest_tokens, se encuentra dentro del paquete tidytext, sirve para dividir el texto en palabras individuales

#Calcular frecuencias de palabras
word_counts <- text_df %>%
  count(word, sort = TRUE) #count cuenta la frecuencia de cada palabra 

#Calcular la densidad de vocabulario
total_words <- nrow(text_df)
unique_words <- n_distinct(text_df$word) #número de palabras únicas dividido por el total de palabras
vocab_density <- unique_words / total_words #densidad de vocabulario, recuerden la definición que vimos en Voyant

#Calcular frecuencias relativas
word_counts <- word_counts %>%
  mutate(relative_frequency = n / total_words) 

#descargamos una lista de palabras vacías en español, si utiliza en otro idioma buscar en Help el código de descarga
stopwords1 <- get_stopwords("es") # get_stopwords forma parte del paquete tidytext

# Eliminar palabras vacías de nuestro dataframe
word_counts <- word_counts %>% 
 anti_join(stopwords1) #si se realiza correctamente nos aparecerá en consola: # Joining with `by = join_by(palabra)`

#elimino del df palabras con menos de 3 caracteres con nchar que cuenta el número de caracteres
word_counts1 <- word_counts %>%
  filter(nchar(word) > 3) %>% 
  filter(!is.na(word) & word != "") %>% #filtro NA de mi dataframe y palabras que no son útiles
  filter(!(word %in% c("dijo", "hacia", "entonces", "modo", "this", "luego", "aquí", "toda", "gran", "the", "gutenberg", "project", "después", "pues"))) 


# Visualizar los resultados

# Frecuencia de las 25 palabras más comunes
top_words <- word_counts1 %>%
  top_n(25, n) #indico la cantidad que deseo

# Gráfico las palabras más frecuentes, pueden cambiar los colores
ggplot(top_words, aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Frecuencia de Palabras", x = "Palabras", y = "Frecuencia")

#nube de palabras
wordcloud(words = word_counts1$word, 
          freq = word_counts1$n, 
          min.freq = 1, 
          max.words = 300, 
          random.order = FALSE, 
          rot.per = 0.35,  # Reducir la proporción de palabras rotadas
          colors = brewer.pal(8, "Dark2"))
#comparar con la de Voyant

# Mostrar la densidad de vocabulario y las frecuencias relativas
print(paste("Densidad de Vocabulario:", vocab_density))
#comparar con el de Voyant

# Opcional, Guardar resultados en un archivo CSV
#write.csv(word_counts1, "word_counts.csv", row.names = FALSE)
