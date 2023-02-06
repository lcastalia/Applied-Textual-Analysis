# 0. load packages------
require(tidyverse)
require(tidyr)
require(tidytext)
require(readtext)
require(dplyr)
require(readr)
require(stopwords)
require(tm)
require(slam)
require(topicmodels)
require(tokenizers)
require(purrr)
require(naniar)
require(udpipe)
require(usmap)
require(ggplot2)

# 1. data cleaning ---------
speech_data <- readtext::readtext("TrumpSpeeches_v1/*.txt") %>%
  filter(grepl("Speech", .$doc_id)) %>% 
  mutate(date = str_sub(doc_id, 1, 10)) %>%
  mutate(date = as.Date(date)) %>% 
  filter(date > '2015-12-31') %>% 
  mutate(state = stringr::str_extract(doc_id, paste0(state.abb, collapse = "|"))) %>% 
  select(date, state, text) %>% 
  select(date, state, text) %>% 
  filter(!is.na(state)) %>% 
  group_by(state) %>% 
  summarise(Speeches = paste(text, collapse = " | "))

# 2. create corpus and dtm------
# (1) corpus and dtm------
corpus <- Corpus(VectorSource(speech_data$Speeches))

dtm <- DocumentTermMatrix(corpus,
                          control = list( 
                            removePunctuation = T,
                            stopwords = T,
                            stemming = F,
                            removeNumbers = T,
                            lemma = c('NOUN','ADJ','VERB','ADV'),
                            wordLengths = c(4, 20)))

# (2) removing a specific list of terms------
state.name <- tolower(state.name)
dtm <- dtm_remove_terms(dtm, state.name, remove_emptydocs = FALSE)
dtm <- dtm_remove_terms(dtm, 
                        c("donald", "trump","cheering","cheers","thank","inaudible",
                          "crowd","reacts","york","booing","boos","like","applause",
                          "north","carolina","dont","cant","know","gonna","hampshire",
                          "heres","will","youre","theyre","thats","said","audience",
                          "shes","going","just","people","weve"), 
                        remove_emptydocs = FALSE)

# (3) exclude empty/sparse documents------
dtm <- dtm[row_sums(dtm) > 10,]

# 3. estimate topic model---------
topic <- LDA(dtm,  
             k = 8, 
             method = "Gibbs",
             control = list(
               seed = 1234, 
               burnin = 100,  
               iter = 300,  
               keep = 1,  
               save = F,    
               verbose = 10  
             ))

# 4.terms that are most common within each topic---------
apply(topic@beta, 1, function(x) head(topic@terms[order(x, decreasing = T)],5))

# 5. calculate beta value of top terms in each topic -------
state_topics <- 
  tidy(topic, matrix = "beta")

state_top_terms <- 
  state_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 5) %>% 
  ungroup() %>%
  arrange(topic, -beta)

state_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

beta_wide <- 
  state_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  pivot_wider(names_from = topic, values_from = beta) %>% 
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

# 6. calculate gamma probabilities for each state--------
# The higher the possibility is, the more unique the single topic is.
state_gamma <- tidy(topic, matrix = "gamma")

state_gamma <- 
  state_gamma %>%
  separate(document, c("title", "chapter"), sep = "_", convert = TRUE)

state_gamma %>%
  mutate(title = reorder(title, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ title) +
  labs(x = "topic", y = expression(gamma))

# 7. map of topic 8 (Hillary Clinton) --------
topic_gamma <- 
  tidy(topic, matrix = "gamma") %>% 
  filter(topic == 8) 

speech_data %>% 
  rownames_to_column() -> speech_data

names(speech_data)[names(speech_data) == 'rowname'] <- 'document'

raw_gamma <- left_join(speech_data, topic_gamma, by = 'document' )

state_info <- 
  tibble(state_name = state.name,
         state = state.abb)

state_gamma <- 
  left_join(state_info, raw_gamma, by = 'state' ) %>% 
  select(state_name, state, gamma)
  
plot_usmap(data = state_gamma,
           values = "gamma",
           color = "blue") +
  scale_fill_continuous(low = "white", high = "blue",
                        name = "Gamma", label = scales::comma) +
  theme(legend.position = "right")

