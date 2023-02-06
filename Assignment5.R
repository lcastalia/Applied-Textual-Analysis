install.packages("rvest")
require(rvest)
require(textrank)
require(udpipe)
require(dplyr)
require(readr)
require(stringr)

# Reading the file
appl <- 
  read_html("aapl_q1_2016.xml") %>% 
  html_nodes(c("intro","text")) %>% 
  html_text2()

# Preprocessing
tagger <- udpipe_load_model("english-ewt-ud-2.5-191206.udpipe")

appl_text <- 
  udpipe_annotate(object = tagger, x = appl) %>%  
  as_tibble()

# Creating the data frames sentences and terminology
sentences <- 
  appl_text %>% 
  select("sentence_id","sentence") %>%
  unique()

terminology <- 
  appl_text %>%
  filter(upos %in% c("NOUN", "ADJ")) %>%
  select("sentence_id", "lemma")

# Run textrank_sentences and evaluate the output
tr <- textrank_sentences(data = sentences,
                         terminology = terminology)

# Extract the top 10 most central sentences
top10_sentence <- summary(tr, n = 10)
