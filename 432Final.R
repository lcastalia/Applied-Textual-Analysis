# Load packages-------
require(xml2)
require(dplyr)
require(tidytext)
require(tidyverse)
require(rvest)
require(tm)
require(tm.plugin.factiva)
require(stringr)
require(scales)
require(tidymodels)
require(sentimentr)
require(slam)
require(udpipe)
require(textir)

# Load data--------
load("earningsCallsTranscripts.RData")
dailyReturn <- read_csv("dailyReturn.csv")

# Task 1 Explorative analysis----------
## 1. Import WSJ Articles------
newsfiles <- dir(path="WSJdata", all.files=TRUE, recursive=TRUE)
newsfiles <- newsfiles[! newsfiles %in% ".DS_Store"]
newsfiles <- as_tibble(newsfiles)
colnames(newsfiles) <- "filename"

## 2. Load one(first) html file to get the first corpus-----------
# Load one file
firstfile <- Corpus(
  FactivaSource(paste0("WSJdata/",newsfiles$filename[1])),
  list(language=NA)) 

# Build up a data frame
totaldata <- as.data.frame(
  matrix(nrow=length(firstfile), ncol=4))

# Give names to each column
colnames(totaldata) <- c("date","WC","news","ticker")

# Extract all information from files 
for (i in 1:length(firstfile)){
  paste0(substr(firstfile[[i]][["meta"]][["id"]],9,12),"-",
         substr(firstfile[[i]][["meta"]][["id"]],13,14),"-",
         substr(firstfile[[i]][["meta"]][["id"]],15,16)) -> totaldata[[i,1]]
  
  firstfile[[i]][["meta"]][["wordcount"]]-> totaldata[[i,2]]
  
  toString(firstfile[[i]][["content"]])-> totaldata[[i,3]]
  
  strsplit("AET/AET01.html", "/", fixed= T)[[1]][1]-> totaldata[[i,4]]
}

## 3. Generalize former steps, merge all WSJ Article files into one dataframe--------
for (w in 2:length(newsfiles$filename)){
  abc <- Corpus(FactivaSource(paste0("WSJdata/",newsfiles$filename[w])),
         list(language=NA))
  
  df <- as.data.frame(matrix(nrow=length(abc),ncol=4))
  colnames(df) <- c("date","WC","news","ticker")
  
  for (i in 1:length(abc)){
    strsplit(newsfiles$filename[w], "/", fixed= T)[[1]][1] -> df[[i,4]]
    as.String(abc[[i]][["content"]]) -> df[[i,3]]
    abc[[i]][["meta"]][["wordcount"]] -> df[[i,2]]
    paste0(substr(abc[[i]][["meta"]][["id"]],9,12),"-",
           substr(abc[[i]][["meta"]][["id"]],13,14),"-",
           substr(abc[[i]][["meta"]][["id"]],15,16)) -> df[[i,1]]
  }
  
  df[!duplicated(df), ] -> df
  rbind(df,totaldata)-> totaldata
}

# Clean data
totaldata$date <- as.Date(totaldata$date)
totaldata1 <- 
  totaldata %>% 
  group_by(date, ticker) %>% 
  summarise(WC = sum(WC),
            text = paste0(news, sep =". ", collapse = '. ')) 

# Merge 'WSJ Articles' with 'dailyReturn'
finaldata <- 
  left_join(totaldata1, dailyReturn, by =c("date","ticker")) %>% 
  select(date, vol, ret, WC, ticker) %>% 
  na.omit()

finaldata <- 
  left_join(dailyReturn, totaldata1, by =c("date","ticker")) %>% 
  select(date, vol, ret, WC, ticker) %>% 
  na.omit()

## 4. Regression of news coverage on daily return and stock market volume------
# regarding a given firm
m1 <- lm(log(WC) ~  abs(ret) + abs(vol), 
          data = finaldata)
summary(m1)

# Get daily returns of 3 days before and after the article was published
return_loglead <- 
  dailyReturn %>% 
  group_by(ticker) %>% 
  mutate(ret3_before = lag(ret,3),
         ret2_before = lag(ret,2),
         ret1_before = lag(ret,1),
         ret1_after = lead(ret,1),
         ret2_after = lead(ret,2),
         ret3_after = lead(ret,3),
         ret4_after = lead(ret,4),
         ret5_after = lead(ret,5))

# Merge 'WSJ Articles' with new 'dailyReturn'
return_reaction_data <- 
  left_join(totaldata1, return_loglead, by =c("date","ticker")) %>% 
  select(date, vol, ret, WC, ticker,
         ret3_before, ret2_before, ret1_before,
         ret1_after, ret2_after, ret3_after, ret4_after, ret5_after) %>% 
  na.omit()           

## 5. Regression of news coverage on return reaction regarding a given firm-----
m2 <- lm(log(WC) ~ abs(ret3_before) + abs(ret2_before) + abs(ret1_before) + abs(ret)
         + abs(ret1_after) + abs(ret2_after) + abs(ret3_after) + abs(ret4_after) + abs(ret5_after), 
         data = return_reaction_data)
summary(m2)

# Task 2 Construct a sentiment dictionary----------
## 1. Randomly select 50% of the firms (Group A)------
companies <- list.files("WSJdata/")
group_a <- sample(companies, 53, replace = FALSE, prob = NULL)
group_b <- companies[ !companies %in%  group_a]

group_a_data <- 
  totaldata1 %>% 
  filter(ticker %in% group_a)

group_a_data <- 
  left_join(group_a_data, return_loglead, by =c("date","ticker")) %>% 
  select(date, ticker, WC, ret1_before, text) %>% 
  na.omit() 

## 2. Separate texts into one positive group and one negative group-----
pos_a <- 
  group_a_data %>% 
  filter(ret1_before >= 0) %>% 
  arrange(-ret1_before) %>% 
  head(1500)

neg_a <- 
  group_a_data %>% 
  filter(ret1_before < 0) %>% 
  arrange(-ret1_before) %>% 
  head(1500)

## 3. Create dtm for each group------
pos_text_source <- VectorSource(pos_a$text)
pos_corpus <- Corpus(pos_text_source)
removeN <- function(x) gsub('\n', ' ', x)
pos_corpus <- tm_map(pos_corpus, content_transformer(removeN))

neg_text_source <- VectorSource(neg_a$text)
neg_corpus <- Corpus(neg_text_source)
removeN <- function(x) gsub('\n', ' ', x)
neg_corpus <- tm_map(neg_corpus, content_transformer(removeN))

pos_dtm <- DocumentTermMatrix(pos_corpus,
                          control = list(removeNumbers = T,
                                         stopwords = T,
                                         tolower = T,
                                         stemming = T,
                                         removePunctuation = T,
                                         lemma = c('NOUN','ADJ','VERB','ADV'),
                                         wordLengths = c(4, 20)))

neg_dtm <- DocumentTermMatrix(neg_corpus,
                              control = list(removeNumbers = T,
                                             stopwords = T,
                                             tolower = T,
                                             stemming = T,
                                             removePunctuation = T,
                                             lemma = c('NOUN','ADJ','VERB','ADV'),
                                             wordLengths = c(4, 20)))

## 4. Get a word list for each group with word frequencies > 100----
pos_WC <- tibble(word = colnames(pos_dtm), freq = col_sums(pos_dtm)) %>% 
  arrange(-freq) %>% 
  filter(freq > 100)

neg_WC <- tibble(word = colnames(neg_dtm), freq = col_sums(neg_dtm)) %>% 
  arrange(-freq) %>% 
  filter(freq > 100)

## 5. Merge two word lists into one0-----
total_WC <- full_join(pos_WC, neg_WC, by = "word") 
colnames(total_WC) <- c("word", "pos_freq","neg_freq")
total_WC$pos_freq <- as.numeric(total_WC$pos_freq)
total_WC$neg_freq <- as.numeric(total_WC$neg_freq)

## 6. Assign scores to each word-----
total_WC <- 
  total_WC %>% 
  replace(is.na(.), 0) %>% 
  mutate(diff_freq = pos_freq - neg_freq,
         score = 100*diff_freq / sum(pos_freq,neg_freq))

## 7. Get a sentiment dictionary------
sentiment_dict <- 
  total_WC %>% 
  select(word, score) %>% 
  arrange(-score) 
colnames(sentiment_dict) <- c("x", "y")

# Task 3 Internal validity-----------
## 1. The relationship between sentiment and return-------
# (1) Set our sentiment dictionary-----
as_key(sentiment_dict) -> sentiment_dict

# (2) Generate sentiment score for texts in Group A------
group_a_data <- 
  group_a_data %>% 
  mutate(senti_score = NA)

for (i in 1:nrow(group_a_data)) {
  group_a_data$senti_score[i] <- 
    sentiment_by(group_a_data$text[i] %>% get_sentences(),
                 polarity_dt =sentiment_dict)$ave_sentiment
}

# Inspect the relation between sentiment and return in Group A
m3 <- lm(abs(senti_score) ~ abs(ret1_before),
         data = group_a_data)
summary(m3)

# Plot
ggplot(group_a_data, aes(abs(ret1_before), abs(senti_score))) +
  geom_point() +
  geom_smooth(method='lm') +
  labs(title = "Relationship between sentiment and return for Group A",
       x = "Return one day before news coverage",
       y = "Sentiment score") +
  theme_minimal()

# (3) Do the same thing with Group B-------
group_b <- companies[ !companies %in%  group_a]
group_b_data <- 
  totaldata1 %>% 
  filter(ticker %in% group_b)

group_b_data <- 
  left_join(group_b_data, return_loglead, by =c("date","ticker")) %>% 
  select(date, ticker, WC, ret1_before, text) %>% 
  na.omit() 

group_b_data <- 
  group_b_data %>% 
  mutate(senti_score = NA)

for (i in 1:nrow(group_b_data)) {
  group_b_data$senti_score[i] <- 
    sentiment_by(group_b_data$text[i] %>% get_sentences(),
                 polarity_dt =sentiment_dict)$ave_sentiment
}

# Regression
m4 <- lm(abs(senti_score) ~ abs(ret1_before),
         data = group_b_data)
summary(m4)

# Plot
ggplot(group_b_data, aes(abs(ret1_before), abs(senti_score))) +
  geom_point() +
  geom_smooth(method='lm') +
  labs(title = "Relationship between sentiment and return for Group B",
       x = "Return one day before news coverage",
       y = "Sentiment score") +
  theme_minimal()

## 2. Split the remaining firms in Group B----------
# (1) Split firms in Group B according to the number of articles------
group_b_n <- 
  group_b_data %>%
  group_by(ticker)%>%
  mutate(number=n()) %>% 
  select(ticker,number) %>%
  unique() %>%
  arrange(-number) 

group_b_big <- head(group_b_n, 25)
group_b_small <- tail(group_b_n, 26)

# (2) Regression of firms with a lot of news articles-------
group_b_big_data <- 
  group_b_data %>% 
  filter(ticker %in% group_b_big$ticker) %>% 
  select(date, ticker, ret1_before, senti_score)

m5 <- lm(abs(senti_score) ~ abs(ret1_before),
         data = group_b_big_data)
summary(m5)

ggplot(group_b_big_data, aes(abs(ret1_before), abs(senti_score))) +
  geom_point() +
  geom_smooth(method='lm') +
  labs(title = "Relationship between sentiment and return for firms has many articles in Group B",
       x = "Return one day before news coverage",
       y = "Sentiment score") +
  theme_minimal()

# (3) Regression of firms with few of news articles-------
group_b_small_data <- 
  group_b_data %>% 
  filter(ticker %in% group_b_small$ticker) %>% 
  select(date, ticker, ret1_before, senti_score) 

m6 <- lm(abs(senti_score) ~ abs(ret1_before),
         data = group_b_small_data)
summary(m6)

ggplot(group_b_small_data, aes(abs(ret1_before), abs(senti_score))) +
  geom_point() +
  geom_smooth(method='lm') +
  labs(title = "Relationship between sentiment and return for firms has few articles in Group B",
       x = "Return one day before news coverage",
       y = "Sentiment score") +
  theme_minimal()

## 3. Change parameters--------
# (1) Change: Only select top 1000 words with highest frequency---- 
# instead of filteringout words with frequency lower than 100
pos_WC <- tibble(word = colnames(pos_dtm), freq = col_sums(pos_dtm)) %>% 
  arrange(-freq) %>% 
  head(n=1000)

neg_WC <- tibble(word = colnames(neg_dtm), freq = col_sums(neg_dtm)) %>% 
  arrange(-freq) %>% 
  head(n=1000)

total_WC <- full_join(pos_WC, neg_WC, by = "word") 
colnames(total_WC) <- c("word", "pos_freq","neg_freq")
total_WC$pos_freq <- as.numeric(total_WC$pos_freq)
total_WC$neg_freq <- as.numeric(total_WC$neg_freq)

total_WC <- 
  total_WC %>% 
  replace(is.na(.), 0) %>% 
  mutate(diff_freq = pos_freq - neg_freq,
         score = 100*diff_freq / sum(pos_freq,neg_freq))

sentiment_dict <- 
  total_WC %>% 
  select(word, score) %>% 
  arrange(-score) 
colnames(sentiment_dict) <- c("x", "y")
as_key(sentiment_dict) -> sentiment_dict

# (2) Regression of Group A------
group_a_data1 <- 
  group_a_data %>% 
  mutate(senti_score = NA)

for (i in 1:nrow(group_a_data1)) {
  group_a_data1$senti_score[i] <- 
    sentiment_by(group_a_data1$text[i] %>% get_sentences(),
                 polarity_dt = sentiment_dict)$ave_sentiment
}

m7 <- lm(abs(senti_score) ~ abs(ret1_before),
         data = group_a_data1)
summary(m7)

ggplot(group_a_data1, aes(abs(ret1_before), abs(senti_score))) +
  geom_point() +
  geom_smooth(method='lm') +
  labs(title = "Relationship between sentiment and return for Group A",
       x = "Return one day before news coverage",
       y = "Sentiment score") +
  theme_minimal()

# (3) Regression of Group B------
group_b_data1 <- 
  group_b_data %>% 
  mutate(senti_score = NA)

for (i in 1:nrow(group_b_data1)) {
  group_b_data1$senti_score[i] <- 
    sentiment_by(group_b_data1$text[i] %>% get_sentences(),
                 polarity_dt =sentiment_dict)$ave_sentiment
}

m8 <- lm(abs(senti_score) ~ abs(ret1_before),
         data = group_b_data1)
summary(m8)

ggplot(group_b_data1, aes(abs(ret1_before), abs(senti_score))) +
  geom_point() +
  geom_smooth(method='lm') +
  labs(title = "Relationship between sentiment and return for Group B",
       x = "Return one day before news coverage",
       y = "Sentiment score") +
  theme_minimal()

# Task 4 External validity-------------
## 1. Data wrangling------
transcripts <- 
  transcripts %>%
  subset(select = -intro) %>% 
  mutate(WC=str_count(qa,"\\W+")) %>% 
  filter(WC > 100)
colnames(transcripts)[1] = "date"

## 2. Assign sentiment scores-------
earning_call_total <- 
  left_join(transcripts, return_loglead, by =c("date","permno")) %>%
  select(date, ticker, permno, qa, WC, ret1_before) %>% 
  na.omit() %>% 
  mutate(senti_score = NA)

for (i in 1:nrow(earning_call_total)) {
  earning_call_total$senti_score[i] <- 
    sentiment_by(earning_call_total$qa[i] %>% get_sentences(),
                 polarity_dt =sentiment_dict)$ave_sentiment
}


## 3. Separate companies into 2 groups-------
earning_call_companies <- 
  earning_call_total$permno %>%
  unique()

group_a_earning <- sample(earning_call_companies, 15, replace = FALSE, prob = NULL)
group_b_earning <- earning_call_companies[ !earning_call_companies %in%  group_a_earning]

group_a_earning_data <- 
  earning_call_total %>% 
  filter(permno %in% group_a_earning)

group_b_earning_data <- 
  earning_call_total %>% 
  filter(permno %in% group_b_earning)

## 4. Separate Group B into 2 groups-----
group_b_n <- 
  group_b_earning_data %>%
  group_by(permno) %>%
  mutate(number=n()) %>% 
  select(permno,number) %>%
  unique() %>%
  arrange(-number)

group_b_earning_big <- head(group_b_n,7)
group_b_earning_small <- tail(group_b_n,8)

group_b_earning_big_data <- 
  earning_call_total %>% 
  filter(permno %in% group_b_earning_big$permno)

group_b_earning_small_data <- 
  earning_call_total %>% 
  filter(permno %in% group_b_earning_small$permno)

## 5. All the regressions --------
# earning call full sample 
m9 <- lm(abs(senti_score) ~ abs(ret1_before),
         data = earning_call_total)
summary(m9)

# earning call group A
m10 <- lm(abs(senti_score) ~ abs(ret1_before),
         data = group_a_earning_data)
summary(m10)

# earning call group B
m11 <- lm(abs(senti_score) ~ abs(ret1_before),
          data = group_b_earning_data)
summary(m11)

# earning call group B firms with many articles 
m12 <- lm(abs(senti_score) ~ abs(ret1_before),
          data = group_b_earning_big_data)
summary(m12)

# earning call group B firms with few articles 
m13 <- lm(abs(senti_score) ~ abs(ret1_before),
          data = group_b_earning_small_data)
summary(m13)

