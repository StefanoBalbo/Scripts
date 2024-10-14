rm(list=ls())

#library(Rcrawler)
#library(installr)

library(magrittr)
library(tidyverse)
library(conflicted)
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("set_names", "purrr")
library(ggplot2)
library(rvest) # web scraping
library(polite) # verificación de robots.txt
library(lubridate) # tabajar con datos tipo fecha
library(wordcloud) # crear nubes de palabras
library(tidytext)  # manejo de datos tipo texto 
library(tm)        # manejo de texto StopWords
library(RColorBrewer)

######################################################################

url <- "https://www.bcra.gob.ar/"

url %>% 
  bow()

titulo <- url %>%
  read_html() %>%
  html_nodes("p") %>%
  html_text()

titulo %>% head(n = 10)


# nube de palabras

#stop_words_spanish <- data.frame(word = stopwords("spanish"))

 #ngrama <-
# titulo %>%
# select(titulo) %>%
  # desanida el texto completo por palabra n = 1
 # unnest_tokens(output = "word", titulo, token = "ngrams", n = 1) %>%
  # elimina las stopwords (palabras NO informativas)
 # anti_join(stop_words_spanish) %>%
  # conteo del número de veces que aparece cada palabra
 # count(word)


################################################################################################

rm(list=ls())
library(rvest)
library(xml2)

#webpage <- read_html("http://www.bcv.org.ve")
#results <- webpage %>% html_nodes("div")
#b <- as.character(xml_child(xml_child(xml_child(results[[66]], 7), 1), 1))
#b

bna <- read_html("https://www.bna.com.ar/Personas")
bna.html <- bna %>% html_nodes("div")


#url2 <- "https://www.bna.com.ar/Personas"

#url2 %>% 
#  bow()

#titulo <- url2 %>%
#  read_html() %>%
#  html_nodes("p") %>%
#  html_text()

#titulo %>% head(n = 10)









