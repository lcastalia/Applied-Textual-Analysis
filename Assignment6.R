require(tidytext)
require(readr)
require(sentimentr)
require(dplyr)
require(lexicon)
require(stringr)
require(rvest)

# load data
load("firm_dataset.Rdata")

# Calculate the average sentiment of all insurance companies 
insur_rownum <- which(grepl("46 Insur",raw.data$industry.fama.french.49)) 

insur_company <- tibble(text = section.1.business[insur_rownum])

insur_company$text %>%
  get_sentences() %>%
  sentiment_by(polarity_dt = lexicon::hash_sentiment_loughran_mcdonald) %>%
  .$ave_sentiment -> insur_company$sentimentLM

mean(insur_company$sentimentLM)

# Calculate the average sentiment of all other companies 
other_company <- tibble(text = section.1.business[-insur_rownum])

other_company$text %>% 
  get_sentences() %>%
  sentiment_by(polarity_dt = lexicon::hash_sentiment_loughran_mcdonald) %>%
  .$ave_sentiment -> other_company$sentimentLM

mean(other_company$sentimentLM)

# the most frequent words of insurance companies and other companies 
insur_company %>% 
  unnest_tokens(word,text) %>% 
  filter(! word %in% stopwords()) %>% 
  group_by(word) %>% 
  summarise(freq = n()) %>% 
  arrange(-freq) %>%  
  head(50) %>% 
  as.data.frame()

other_company %>% 
  filter(! word %in% stopwords()) %>% 
  group_by(word) %>% 
  summarise(freq = n()) %>% 
  arrange(-freq) %>%  
  head(50) %>% 
  as.data.frame()


