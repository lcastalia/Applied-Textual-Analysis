# Packages 
install.packages("Metrics")
require(tibble)
require(dplyr)
require(tidytext)
require(stopwords)
require(tm)
require(stringr)
require(tokenizers)
require(tidyr)
require(slam)
require(tidyverse)
require(Metrics)

# Load data
load("firm_dataset.Rdata")

#1. Using bigram---------------------
# (1)Clean the data -----------
section.1.business <- 
  section.1.business %>% 
  removePunctuation() %>% 
  removeNumbers() %>% 
  tolower() %>% 
  removeWords(stopwords()) %>% 
  gsub('\\b\\w{1,2}\\b','',.) %>% 
  gsub('\\b\\w{20,}\\b','',.) %>% 
  stripWhitespace()

# (2)Transform the data into bigrams ----------
s1.bigram <- tokenize_ngrams(section.1.business, n = 2, ngram_delim = "_")

# (3)Generate DTM --------
corpus <- Corpus(VectorSource(s1.bigram))

dtm <- DocumentTermMatrix(corpus,
                          control = list(bounds = list(global = c(5,100))))

# (4)Identify oil and non-oil industries --------
oil.dtm <- dtm[raw.data$industry.fama.french.49 == "30 Oil",]
nonoil.dtm <- dtm[raw.data$industry.fama.french.49 != "30 Oil",]

# (5)Compute the Log-Likelihood-------
calculate.ll <- function(a, b, c, d){
  e1 <- c*(a+b)/(c+d)
  e2 <- d*(a+b)/(c+d)
  ll <- 2*((a*log(a/e1)) + (b*log(b/e2)))
  return(ll)
}

LogLikelihood <- calculate.ll( a <- colSums(as.matrix(oil.dtm)),
                             b <- colSums(as.matrix(nonoil.dtm)),
                             c <- sum(oil.dtm),
                             d <- sum(nonoil.dtm))

# (6)Display the 500 top tokens---------
LogLikelihood %>%
  sort(. , decreasing = TRUE)%>%
  head(500) %>% 
  as.data.frame(col.names = c("token","loglikelihood")) %>% 
  rownames_to_column() -> top_tokens

# (7)Limit the tokens within 500 top tokens-------- 
dtm <- DocumentTermMatrix(corpus,
                          control = list(bounds = list(global = c(5,100)),
                                         dictionary = top_tokens$rowname))

# (8)Identify oil and non-oil industries --------
oil.dtm <- dtm[raw.data$industry.fama.french.49 == "30 Oil",]
nonoil.dtm <- dtm[raw.data$industry.fama.french.49 != "30 Oil",]

# (9)Compute cosine similarity--------
CosineSimilarity <- function(A, B) {  
  sum(A * B) / sqrt(sum(A^2)*sum(B^2)) 
  }

CS <- data.frame()

for (i in 1:18) {
  for (j in 1:482) {
    CosineSimilarity(oil.dtm[i,],
                     nonoil.dtm[j,]) -> CS[i,j]
  }
}

# (10)Make the results more readable------
CS <- as.data.frame(t(CS))

raw.data[raw.data$industry.fama.french.49 != "30 Oil",] %>% 
  select(cik) %>% 
  as.vector() %>% 
  unlist() -> nonoilfirms

raw.data[raw.data$industry.fama.french.49 == "30 Oil",] %>% 
  select(cik) %>% 
  as.vector() %>% 
  unlist() -> oilfirms

rownames(CS) <- nonoilfirms
colnames(CS) <- oilfirms
  
# (11)Find 5 closest non-oil firms for each oil-firm --------
Peer_CS <- 
  lapply(seq(CS), function(x) 
    data.frame(oilfirms=names(CS)[x], nonoilfirms=rownames(CS), CS=CS[, x])[order(-CS[, x]), ][1:5, ])

Peer_CS <- do.call(rbind.data.frame, Peer_CS)

names(Peer_CS)[names(Peer_CS) == 'nonoilfirms'] <- 'cik'

Peer_cik <- left_join(Peer_CS, raw.data, by = 'cik')

# (12)Average return of peer group firms----------- 
Peer_avg_return <- 
  Peer_cik %>%
  mutate(sum_return = 
           return.monthly.NY.m01+
           return.monthly.NY.m02+
           return.monthly.NY.m03+
           return.monthly.NY.m04+
           return.monthly.NY.m05+
           return.monthly.NY.m06+
           return.monthly.NY.m07+
           return.monthly.NY.m08+
           return.monthly.NY.m09+
           return.monthly.NY.m10+
           return.monthly.NY.m11+
           return.monthly.NY.m12) %>% 
  mutate(avg_return = sum_return/12) %>% 
  select(oilfirms,cik,avg_return)

# (13)Average return of peer group of each oil firm --------
Total_peer_avg_return <- 
  Peer_avg_return %>% 
  group_by(oilfirms) %>% 
  summarise(peer_avg_return = mean(avg_return))

# (14)Average return of each oil firm -------
oil_avg_return <- 
  raw.data %>% 
  filter(.$industry.fama.french.49 == "30 Oil") %>%
  mutate(sum_return = 
           return.monthly.NY.m01+
         return.monthly.NY.m02+
         return.monthly.NY.m03+
         return.monthly.NY.m04+
         return.monthly.NY.m05+
         return.monthly.NY.m06+
         return.monthly.NY.m07+
         return.monthly.NY.m08+
         return.monthly.NY.m09+
         return.monthly.NY.m10+
         return.monthly.NY.m11+
         return.monthly.NY.m12) %>% 
  mutate(avg_return = sum_return/12) %>% 
  select(cik,avg_return)

# (15)Final results: combine each oil firm with its peer group average ----------
names(oil_avg_return)[names(oil_avg_return) == 'cik'] <- 'oilfirms'

Total_avg_return <- left_join(Total_peer_avg_return, oil_avg_return, by = 'oilfirms')

# (16)Compute the Root Mean Squared Error -------
rmse(Total_avg_return$avg_return, Total_avg_return$peer_avg_return)


# 2. Using uni-grams---------------------
# (1)Clean the data -----------
section.1.business <- 
  section.1.business %>% 
  removePunctuation() %>% 
  removeNumbers() %>% 
  tolower() %>% 
  removeWords(stopwords()) %>% 
  gsub('\\b\\w{1,2}\\b','',.) %>% 
  gsub('\\b\\w{20,}\\b','',.) %>% 
  stripWhitespace()

# (2)Transform the data into bigrams ----------
s1.bigram <- tokenize_ngrams(section.1.business, n = 2, ngram_delim = "_")

# (3)Generate DTM --------
corpus <- Corpus(VectorSource(s1.bigram))

dtm <- DocumentTermMatrix(corpus,
                          control = list(bounds = list(global = c(5,100))))

# (4)Identify oil and non-oil industries --------
oil.dtm <- dtm[raw.data$industry.fama.french.49 == "30 Oil",]
nonoil.dtm <- dtm[raw.data$industry.fama.french.49 != "30 Oil",]

# (5)Compute the Log-Likelihood-------
calculate.ll <- function(a, b, c, d){
  e1 <- c*(a+b)/(c+d)
  e2 <- d*(a+b)/(c+d)
  ll <- 2*((a*log(a/e1)) + (b*log(b/e2)))
  return(ll)
}

LogLikelihood <- calculate.ll( a <- colSums(as.matrix(oil.dtm)),
                               b <- colSums(as.matrix(nonoil.dtm)),
                               c <- sum(oil.dtm),
                               d <- sum(nonoil.dtm))

# (6)Display the 500 top tokens---------
LogLikelihood %>%
  sort(. , decreasing = TRUE)%>%
  head(500) %>% 
  as.data.frame(col.names = c("token","loglikelihood")) %>% 
  rownames_to_column() -> top_tokens

# (7)Limit the tokens within 500 top tokens-------- 
dtm <- DocumentTermMatrix(corpus,
                          control = list(bounds = list(global = c(5,100)),
                                         dictionary = top_tokens$rowname))

# (8)Identify oil and non-oil industries --------
oil.dtm <- dtm[raw.data$industry.fama.french.49 == "30 Oil",]
nonoil.dtm <- dtm[raw.data$industry.fama.french.49 != "30 Oil",]

# (9)Compute cosine similarity--------
CosineSimilarity <- function(A, B) {  
  sum(A * B) / sqrt(sum(A^2)*sum(B^2)) 
}

CS <- data.frame()

for (i in 1:18) {
  for (j in 1:482) {
    CosineSimilarity(oil.dtm[i,],
                     nonoil.dtm[j,]) -> CS[i,j]
  }
}

# (10)Make the results more readable------
CS <- as.data.frame(t(CS))

raw.data[raw.data$industry.fama.french.49 != "30 Oil",] %>% 
  select(cik) %>% 
  as.vector() %>% 
  unlist() -> nonoilfirms

raw.data[raw.data$industry.fama.french.49 == "30 Oil",] %>% 
  select(cik) %>% 
  as.vector() %>% 
  unlist() -> oilfirms

rownames(CS) <- nonoilfirms
colnames(CS) <- oilfirms

# (11)Find 5 closest non-oil firms for each oil-firm --------
Peer_CS <- 
  lapply(seq(CS), function(x) 
    data.frame(oilfirms=names(CS)[x], nonoilfirms=rownames(CS), CS=CS[, x])[order(-CS[, x]), ][1:5, ])

Peer_CS <- do.call(rbind.data.frame, Peer_CS)

names(Peer_CS)[names(Peer_CS) == 'nonoilfirms'] <- 'cik'

Peer_cik <- left_join(Peer_CS, raw.data, by = 'cik')

# (12)Average return of peer group firms----------- 
Peer_avg_return <- 
  Peer_cik %>%
  mutate(sum_return = 
           return.monthly.NY.m01+
           return.monthly.NY.m02+
           return.monthly.NY.m03+
           return.monthly.NY.m04+
           return.monthly.NY.m05+
           return.monthly.NY.m06+
           return.monthly.NY.m07+
           return.monthly.NY.m08+
           return.monthly.NY.m09+
           return.monthly.NY.m10+
           return.monthly.NY.m11+
           return.monthly.NY.m12) %>% 
  mutate(avg_return = sum_return/12) %>% 
  select(oilfirms,cik,avg_return)

# (13)Average return of peer group of each oil firm --------
Total_peer_avg_return <- 
  Peer_avg_return %>% 
  group_by(oilfirms) %>% 
  summarise(peer_avg_return = mean(avg_return))

# (14)Average return of each oil firm -------
oil_avg_return <- 
  raw.data %>% 
  filter(.$industry.fama.french.49 == "30 Oil") %>%
  mutate(sum_return = 
           return.monthly.NY.m01+
           return.monthly.NY.m02+
           return.monthly.NY.m03+
           return.monthly.NY.m04+
           return.monthly.NY.m05+
           return.monthly.NY.m06+
           return.monthly.NY.m07+
           return.monthly.NY.m08+
           return.monthly.NY.m09+
           return.monthly.NY.m10+
           return.monthly.NY.m11+
           return.monthly.NY.m12) %>% 
  mutate(avg_return = sum_return/12) %>% 
  select(cik,avg_return)

# (15)Final results: combine each oil firm with its peer group average ----------
names(oil_avg_return)[names(oil_avg_return) == 'cik'] <- 'oilfirms'

Total_avg_return <- left_join(Total_peer_avg_return, oil_avg_return, by = 'oilfirms')

# (16)Compute the Root Mean Squared Error -------
rmse(Total_avg_return$avg_return, Total_avg_return$peer_avg_return)