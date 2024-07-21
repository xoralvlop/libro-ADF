# Establecer el directorio de trabajo (opcional)
output_dir <- "C:/Users/Paulina/OneDrive/Desktop/UNTREF Master Humanidades Digitales/Revista Chuy/Graficos"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}
setwd(output_dir)

# Instalar las librerías necesarias sólo si no están ya instaladas
required_packages <- c("tm", "wordcloud", "RColorBrewer", "dplyr", "textclean", "ggplot2", "reshape2")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Cargar las librerías necesarias
library(tm)
library(wordcloud)
library(RColorBrewer)
library(dplyr)
library(textclean)
library(ggplot2)
library(reshape2)

# Función para cargar stopwords adicionales en español y portugués
additional_stopwords <- function() {
  c("yo", "tú", "él", "ella", "nosotros", "vosotros", "ellos", "ellas",
    "me", "te", "se", "nos", "os", "le", "les", "lo", "la", "los", "las",
    "un", "una", "unos", "unas", "mi", "mis", "tu", "tus", "su", "sus",
    "nuestro", "nuestra", "nuestros", "nuestras", "vuestro", "vuestra",
    "vuestros", "vuestras", "ese", "esa", "esos", "esas", "este", "esta",
    "estos", "estas", "aquel", "aquella", "aquellos", "aquellas", "el", 
    "y", "o", "pero", "ni", "aunque", "porque", "sino", "que", "como",
    "yo", "tu", "ele", "ela", "nós", "vós", "eles", "elas", "me", "te",
    "se", "nos", "vos", "lhe", "lhes", "lo", "la", "os", "as", "um", "uma",
    "uns", "umas", "meu", "minha", "meus", "minhas", "teu", "tua", "teus",
    "tuas", "seu", "sua", "seus", "suas", "nosso", "nossa", "nossos", 
    "nossas", "vosso", "vossa", "vossos", "vossas", "esse", "essa", "esses",
    "essas", "este", "esta", "estes", "estas", "aquele", "aquela", "aqueles",
    "aquelas", "o", "e", "ou", "mas", "nem", "embora", "porque", "pois", "que",
    "como", "puede", "vez", "menos")
}

# Función para manejar caracteres especiales
fix_encoding <- function(text) {
  iconv(text, from = "latin1", to = "UTF-8")
}

# Función para limpiar nombres de archivos
clean_filename <- function(filename) {
  gsub("[^[:alnum:]_]", "_", filename)
}

# Función para reemplazar frases específicas con un solo término
replace_phrases <- function(text, phrases) {
  for (phrase in phrases) {
    replacement <- gsub(" ", "_", phrase)
    text <- gsub(phrase, replacement, text, fixed = TRUE)
  }
  return(text)
}

# Función para limpiar el texto
clean_text <- function(text) {
  text <- tolower(text) # Convertir a minúsculas
  text <- removePunctuation(text) # Eliminar puntuación
  text <- removeNumbers(text) # Eliminar números
  text <- removeWords(text, c(stopwords("spanish"), stopwords("portuguese"), additional_stopwords())) # Eliminar palabras vacías en español y portugués
  text <- stripWhitespace(text) # Eliminar espacios en blanco extras
  return(text)
}

# Listado de archivos a procesar
files <- list(
  "C:/Users/Paulina/OneDrive/Desktop/UNTREF Master Humanidades Digitales/Revista Chuy/Revista Chuy Data/A contraluz_Miguel_Rosetti.csv",
  "C:/Users/Paulina/OneDrive/Desktop/UNTREF Master Humanidades Digitales/Revista Chuy/Revista Chuy Data/El comparatismo latinoamericano ciencia menor_Comite_Redactor.csv",
  "C:/Users/Paulina/OneDrive/Desktop/UNTREF Master Humanidades Digitales/Revista Chuy/Revista Chuy Data/Encuesta latinoamericana_Daniel_Link.csv",
  "C:/Users/Paulina/OneDrive/Desktop/UNTREF Master Humanidades Digitales/Revista Chuy/Revista Chuy Data/Figuras e Gestos da Delicadeza_Danilson_Lopes.csv",
  "C:/Users/Paulina/OneDrive/Desktop/UNTREF Master Humanidades Digitales/Revista Chuy/Revista Chuy Data/Haroldo de Campos_Mario_Camara.csv",
  "C:/Users/Paulina/OneDrive/Desktop/UNTREF Master Humanidades Digitales/Revista Chuy/Revista Chuy Data/La literatura mundial como provocacion_Marcelo_Topuzian.csv",
  "C:/Users/Paulina/OneDrive/Desktop/UNTREF Master Humanidades Digitales/Revista Chuy/Revista Chuy Data/La literatura brasileña_Silviano_Santiago.csv",
  "C:/Users/Paulina/OneDrive/Desktop/UNTREF Master Humanidades Digitales/Revista Chuy/Revista Chuy Data/Recomposición de la literatura comparada_Jean_Bessiere.csv",
  "C:/Users/Paulina/OneDrive/Desktop/UNTREF Master Humanidades Digitales/Revista Chuy/Revista Chuy Data/Só centros_Raul_Antelo.csv",
  "C:/Users/Paulina/OneDrive/Desktop/UNTREF Master Humanidades Digitales/Revista Chuy/Revista Chuy Data/Tres Negritos_Daniel_Link.csv"
)

# Frases específicas a reemplazar
phrases_to_replace <- c("nacional", "mundial", "latinoamerica*")

# Inicializar listas para almacenar datos globales
global_texts <- list()
global_word_freqs <- list()
processed_files <- list()  # Lista para archivos que se han procesado correctamente

# Función para procesar cada archivo
process_file <- function(file) {
  text <- readLines(file, encoding = "latin1")
  text <- fix_encoding(text)
  text <- replace_phrases(text, phrases_to_replace)
  global_texts <<- append(global_texts, text)
  
  # Separar el texto por autores
  author_texts <- split(text, cumsum(text == ""))
  
  author_word_freqs <- lapply(author_texts, function(author_text) {
    author_name <- author_text[1]
    if (length(author_text) > 1) {
      author_text_cleaned <- sapply(author_text[-1], clean_text)
      author_corpus <- Corpus(VectorSource(author_text_cleaned))
      
      # Crear la DTM y calcular frecuencias
      author_dtm <- TermDocumentMatrix(author_corpus)
      author_matrix <- as.matrix(author_dtm)
      author_word_freqs <- sort(rowSums(author_matrix), decreasing=TRUE)
      author_df <- data.frame(word=names(author_word_freqs), freq=author_word_freqs)
      
      # Generar la nube de palabras por autor
      set.seed(1234)
      tryCatch({
        png(file.path(output_dir, paste0("nube_de_palabras_", clean_filename(basename(file)), "_", clean_filename(author_name), ".png")), width=800, height=600)
        wordcloud(words = author_df$word, freq = author_df$freq, min.freq = 2,
                  max.words=100, random.order=FALSE, rot.per=0.35, 
                  colors=brewer.pal(8, "Dark2"), scale = c(3, 0.5))
        dev.off()
        cat("Nube de palabras guardada para el autor:", author_name, "\n")
      }, error = function(e) {
        cat("Error generando la nube de palabras para el autor:", author_name, "\n")
      })
      
      # Guardar la frecuencia de palabras por autor
      write.csv(author_df, file.path(output_dir, paste0("frecuencia_palabras_", clean_filename(basename(file)), "_", clean_filename(author_name), ".csv")), row.names = FALSE)
      
      # Gráfico de barras de las palabras más frecuentes por autor
      top_words_author <- head(author_df, 20)
      if(nrow(top_words_author) > 0) {
        p <- ggplot(top_words_author, aes(x = reorder(word, freq), y = freq)) +
          geom_bar(stat = "identity", fill = "skyblue") +
          coord_flip() +
          labs(title = paste("Top 20 palabras más frecuentes de", author_name),
               x = "Palabras",
               y = "Frecuencia") +
          theme_minimal()
        ggsave(file.path(output_dir, paste0("grafico_barras_", clean_filename(basename(file)), "_", clean_filename(author_name), ".png")), plot = p, width = 10, height = 6)
      }
      
      return(author_df)
    } else {
      return(NULL)
    }
  })
  
  return(author_word_freqs)
}

# Procesar cada archivo y guardar los resultados
for (file in files) {
  tryCatch({
    word_freqs <- process_file(file)
    global_word_freqs[[basename(file)]] <- word_freqs
    processed_files[[basename(file)]] <- TRUE
  }, error = function(e) {
    cat("Error procesando el archivo:", file, "\n")
    processed_files[[basename(file)]] <- FALSE
  })
}

# Filtrar las palabras "nacional", "mundial" y "latinoamerica*" en todos los textos
all_text <- unlist(global_texts)
all_text_cleaned <- clean_text(all_text)
all_corpus <- Corpus(VectorSource(all_text_cleaned))
all_dtm <- TermDocumentMatrix(all_corpus)
all_matrix <- as.matrix(all_dtm)
word_freqs_global <- sort(rowSums(all_matrix), decreasing=TRUE)
word_freqs_df_global <- data.frame(word=names(word_freqs_global), freq=word_freqs_global)

# Filtrar las frecuencias de "nacional", "mundial" y "latinoamerica*"
freq_nacional <- word_freqs_df_global[word_freqs_df_global$word == "nacional", ]
freq_mundial <- word_freqs_df_global[word_freqs_df_global$word == "mundial", ]
freq_latinoamerica <- word_freqs_df_global[grep("latinoamerica", word_freqs_df_global$word), ]

# Combinar las frecuencias en un solo data frame
freq_combined <- rbind(freq_nacional, freq_mundial, freq_latinoamerica)

# Graficar las frecuencias comparativas
p_global <- ggplot(freq_combined, aes(x = word, y = freq, fill = word)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Frecuencia comparativa de 'nacional', 'mundial' y 'latinoamerica*'",
       x = "Palabra",
       y = "Frecuencia") +
  theme_minimal() +
  scale_fill_manual(values = c("nacional" = "red", "mundial" = "blue", "latinoamerica*" = "green"))

# Guardar el gráfico comparativo
ggsave(file.path(output_dir, "frecuencia_comparativa_nacional_mundial_latinoamerica.png"), plot = p_global, width = 10, height = 6)

# Crear un data frame con frecuencias para el heatmap
freq_heatmap <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(freq_heatmap) <- c("file", "word", "freq")

for (file in names(global_word_freqs)) {
  for (author_df in global_word_freqs[[file]]) {
    if (!is.null(author_df)) {
      for (word in c("nacional", "mundial", "latinoamerica*")) {
        word_freq <- author_df[author_df$word == word, "freq"]
        if (length(word_freq) == 0) word_freq <- 0
        freq_heatmap <- rbind(freq_heatmap, data.frame(file=file, word=word, freq=word_freq))
      }
    }
  }
}

# Graficar el heatmap
freq_heatmap_matrix <- acast(freq_heatmap, file ~ word, value.var="freq", fill=0)
heatmap_colors <- colorRampPalette(brewer.pal(9, "Blues"))(255)

png(file.path(output_dir, "heatmap_frecuencias.png"), width=800, height=600)
heatmap(freq_heatmap_matrix, col=heatmap_colors, scale="column", margins=c(10,10), cexRow=0.8, cexCol=0.8)
dev.off()

cat("Análisis completado. Resultados guardados en:", output_dir, "\n")

