###############################################################################
# Este código permite conectar con un grupo de Facebook y hacer algo de
# minería de textos.
# Antes de empezar hay que crear una App en la plataforma de Facebook para
# poder conectar con la API de Facebook. Crear esta App no es complicado, y
# puede encontrar una muy buena guía, paso a paso, en la siguiente dirección:
# http://thinktostart.com/analyzing-facebook-with-r/
# Para todo esto por supuesto es necesario tener una cuenta de Facebook.
###############################################################################

# Instalar paquetes requeridos desde CRAN

install.packages("devtools", "tm", "wordcloud")

# Instalar paquete Rfacebook desde GitHub

devtools::install_github("pablobarbera/Rfacebook/Rfacebook")

# Cargar paquetes

library("Rfacebook")
library("tm")
library("wordcloud")

# Autenticar. Sobre cómo obtener los valores de app_id y app_secret visite
# http://thinktostart.com/analyzing-facebook-with-r/

fba <- fbOAuth(app_id = "XXXXX", app_secret = "XXXXX")

# Buscar grupo

searchGroup("bionacle", fba)

# Obtener mensajes

mensajes <- getGroup("209159462430576", fba, n = 5000)

# Identificar mensaje con más comentarios y con más me gusta

mensajes[which.max(mensajes$comments_count), ]
mensajes[which.max(mensajes$likes_count), ]

# Convertir todos los textos de UTF-8 a ASCII
# (Necesario cuando el idioma no es inglés y hay caracteres especiales)

textos <- sapply(mensajes$message, function(x) iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT"))

# Crear corpus (https://es.wikipedia.org/wiki/Corpus_lingüístico)

texcorpus <- Corpus(VectorSource(textos))

# Limpiar el corpus:
# - Transformar todo a minúsculas
# - Remover puntuación
# - Remover palabras que no son importantes

texcorpus <- tm_map(texcorpus, content_transformer(tolower)) 
texcorpus <- tm_map(texcorpus, removePunctuation)
texcorpus <- tm_map(texcorpus, function(x) removeWords(x, stopwords("spanish")))

# Definir paleta de colores para el gráfico

pal2 <- brewer.pal(8, "Dark2")

# Crear nube de palabras

wordcloud(texcorpus, scale = c(3.5, 0.2), colors = pal2)
