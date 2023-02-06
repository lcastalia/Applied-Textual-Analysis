# Assignment 3
require(tidytext)
require(readr)
require(dplyr)
require(udpipe)
require(wordcloud)
require(tm)

# load data
load("firm_dataset.Rdata")

# get row numbers of firms from industries "30 Oil" and "31 Util"
oil_rownum <- which(grepl("30 Oil",raw.data$industry.fama.french.49)) 
util_rownum <- which(grepl("31 Util",raw.data$industry.fama.french.49)) 

# load the model and tagger
udpipe_download_model("english")
tagger <- udpipe_load_model("english-ewt-ud-2.5-191206.udpipe")

# tag firms from industry "30 Oil"
oil_tag <- 
  udpipe_annotate(tagger,
                  x = section.1.business[oil_rownum]) %>% 
  as_tibble() %>% 
  select(doc_id, token, lemma, xpos, upos)

# tag firms from industry "31 Util"
util_tag <- 
  udpipe_annotate(tagger,
                  x = section.1.business[util_rownum]) %>% 
  as_tibble() %>% 
  select(doc_id, token, lemma, xpos, upos)

# extract nouns and adjectives
oil_tag %>%
  filter(grepl(c("^N.+","RB"), xpos)) %>% 
  count(lemma, sort = T) -> oil.df 

util_tag %>%
  filter(grepl(c("^N.+","RB"), xpos)) %>% 
  count(lemma, sort = T) -> util.df 

# create wordcloud for each industry
png(filename = "30 oil.png",
    units = "cm",
    width = 20,
    height = 20,
    res = 300)
wordcloud(word = oil.df$lemma[1:100],
          freq = oil.df$n[1:100])
dev.off()

png(filename = "31 util.png",
    units = "cm",
    width = 20,
    height = 20,
    res = 300)
wordcloud(word = util.df$lemma[1:100],
          freq = util.df$n[1:100])
dev.off()