
# bibliotecas -------------------------------------------------------------

#dados
library(tidyverse) 
#api vagalume
library(vagalumeR)
#mineracao de texto
library(tidytext) 
library(stopwords)
#nuvem
library(wordcloud) 


# chave api ---------------------------------------------------------------

usethis::edit_r_environ(scope = "project")

api_key <- Sys.getenv("API_VAGALUME")


# artista -----------------------------------------------------------------

artist <- "mukeka-di-rato"


# informacoes do artista --------------------------------------------------

# informacoes no vagalume

artist_info <- artist |>  
  map_dfr(artistInfo)

# musicas

songs_artist <- artist |> 
  map_dfr(songNames)


# amostra de musicas do artista -------------------------------------------

set.seed(1)

songs_sample <- songs_artist |> 
  sample_n(60) |> #amostra de 60 musicas
  pull(song.id) |> #coluna de ids das musicas
  map_dfr(
    vagalumeR::lyrics, #retorna as letras das musicas
    artist = "Mukeka di Rato", #nome como aparece no site vagalume
    type = "id", key = api_key,
    message = FALSE
  )


# mineracao de texto ------------------------------------------------------

# stopwords

sw <- c(
  stopwords::stopwords("pt"),
  c("é", "ta", "la", "tá", "lá", "to", "el", "in", "pra", "and", 
    "il",  "in", "i", "3x", "2x", "of", "x", "the", "is")
)

# letras das musicas

lyrics <- songs_sample |>
  select(text)

# contagem das palavras

count_words <- tibble(text = lyrics$text) |> #transforma cada musica em uma observacao
  tidytext::unnest_tokens(word, text, to_lower = TRUE) |> #transforma cada palavra em uma observacao
  count(word, sort = TRUE) |> #frequencia de cada palavra
  anti_join(tibble(word = sw)) #remove as palavras stopwords
  

# nuvem de palavras -------------------------------------------------------

wordcloud(
  count_words$word, count_words$n,
  max.words = 100, 
  colors = c("red", "blue", "black"),
  random.order = FALSE, 
  scale = c(2, .5), rot.per = 0.35, 
)





