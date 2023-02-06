require(jsonlite)
require(tidyverse)
require(tidytext)
require(dplyr)
require(readr)
require(stopwords)
require(tm)
require(topicmodels)
require(slam)
require(wordcloud)
library(lubridate)
require(tokenizers)

# 2016-01-05 21.00.00 (Complete, As spoken) Interview with Paul Steinhauser, NH, NH1
# 2016-01-14 21.00.00 (Complete, As spoken) Debate with Maria Bartiromo, Neil Cavuto, Ted Cruz, Jeb Bush, Marco Rubio, Chris Christie, Ben Carson, John Kasi, Republican Candidates, North Charleston, SC, Fox
# 2016-01-14 23.00.00 (Complete, As spoken) Interview with Dana Bash, North Charleston, SC, CNN
# 2016-01-29 10.00.00 (Complete, As spoken) Interview with Paul Steinhauser, Nashua, NH, NH1
# 2016-02-02 00.00.00 (Partial, As spoken) Interview with Paul Steinhauser, NH, NH!
# 2016-02-03 19.00.00 (Complete, As spoken) Interview with Greta Van Susteren, New York, NY, 'On the Record', Fox
# 2016-02-04 20.00.00 (Complete, As spoken) Town Hall with Anderson Cooper, Andre Wassouf, Timothy Baines, Joe Manzoli, Ryan Bernier, Susan Theirrien, Brian Ti, Manchester, NH, 'Anderson Cooper 360', CNN
# 2016-02-05 20.00.00 (Complete, As spoken) Interview with Bill O'Reilly, New York, NY, 'The O'Reilly Factor', Fox
# 2016-02-06 20.00.00 (Complete, As spoken) Debate with David Muir, Martha Raddatz, Marco Rubio, Jeb Bush, Chris Christie, Ted Cruz, John Kasich, Ben Carson, Republican Candidates, Goffstown, NH, ABC
# 2016-02-06 23.15.00 (Complete, As spoken) Interview with Sarah Murray, Goffstown, NH, CNN
# 2016-02-13 21.00.00 (Complete, As spoken) Debate with John Dickerson, Jeb Bush, Ted Cruz, Major Garrett, Marco Rubio, Kimberley Strassel, John Kasich, Ben, Republican Candidates, Greenville, SC, CBS
# 2016-02-17 12.15.00 (Complete, As spoken) Interview with Thomas Roberts, SC, 'MSNBC Live', MSNBC
# 2016-02-22 22.00.00 (Complete, As spoken) Interview with Sean Hannity, Las Vegas, NV, 'The Sean Hannity Show', Fox
# 2016-02-23 21.00.00 (Complete, As spoken) Interview with Jacob Soboroff, Las Vegas, NV, 'The Rachel Maddow Show', MSNBC
# 2016-02-25 20.00.00 (Complete, As spoken) Debate with Wolf Blitzer, Marco Rubio, Ted Cruz, Dana Bash, John Kasich, Maria Celeste Arraras, Hugh Hewitt, Ben, Republican Candidates, Houston, TX, CNN
# 2016-02-25 22.00.00 (Complete, As spoken) Interview with Chris Cuomo, Houston, TX, 'Anderson Cooper 360', CNN
# 2016-02-29 22.00.00 (Complete, As spoken) Interview with Sean Hannity, Valdosta, GA, 'The Hannity Show', Fox
# 2016-03-11 19.00.00 (Complete, As spoken) Interview with Greta Van Susteren, Chicago, IL, 'On the Record', Fox
# 2016-03-11 19.45.00 (Complete, As spoken) Interview with Chris Matthews, Chicago, IL, 'Hardball', MSNBC
# 2016-03-13 10.30.00 (Complete, As spoken) Interview with John Dickerson, Chicago, IL, 'Face the Nation', CBS.txt
# 2016-03-16 20.00.00 (Complete, As spoken) Interview with Bill O'Reilly, New York, NY, 'The O'Reilly Factor', Fox
# 2016-05-04 17.00.00 (Complete, As spoken) Interview with Wolf Blitzer, New York, NY, 'The Situation Room', CNN
# 2016-05-04 20.00.00 (Complete, As spoken) Interview with Bill O'Reilly, New York, NY, 'The O'Reilly Factor', Fox
# 2016-06-21 00.00.00 (Complete, As spoken) Interview with Lou Dobbs, New York, NY, 'Lou Dobbs Tonight', Fox
# 2016-06-23 19.00.00 (Complete, As spoken) Interview with Lester Holt, New York, NY, NBC
# 2016-07-14 20.00.00 (Complete, As spoken) Interview with Bill O'Reilly, New York, NY, 'The O'Reilly Factor', Fox
# 2016-08-16 21.00.00 (Complete, As spoken) Town Hall with Sean Hannity, Kris Paronto, Sebastian Gorka, Karen McWatters, Kimberly Munley, Howard Ray, Milwaukee, WI, Fox
# 2016-08-22 20.10.00 (Complete, As spoken) Interview with Bill O'Reilly, Akron, OH, 'The O'Reilly Factor', Fox
# 2016-08-23 20.00.00 (Complete, As spoken) Interview with Sean Hannity, Michelle Root, Laura Wilkerson, Mary Ann Mendoza, Sabine Durden, Julie Golvach, Austin, TX, 'The Sean Hannity Show', Fox
# 2016-10-20 13.00.00 (Complete, As spoken) Interview with Colleen Marshall, Delaware, OH, NBC4i


# Load the Trump corpus into a tibble with just one column and one row.
raw.text <- tibble(text = read_file("trump_campaign_corpus.json"))
str(raw.text)


  
corpus <- Corpus(VectorSource(raw.text))
  
dtm <- DocumentTermMatrix(corpus,
                            control = list( 
                              removePunctuation = T,
                              stopwords = T,
                              stemming = F,
                              removeNumbers = T,
                              lemma = c('NOUN','ADJ','VERB','ADV'),
                              wordLengths = c(4, 20)))
  
  
  dtm <- dtm[row_sums(dtm) > 10,]
  
  topic <- LDA(dtm,  
               k = 25, 
               method = "Gibbs",
               control = list(
                 seed = 1234, 
                 burnin = 100,  
                 iter = 300,  
                 keep = 1,  
                 save = F,    
                 verbose = 10  
               ))
  
  t <- apply(topic@beta, 1, function(x) head(topic@terms[order(x, decreasing = T)],5))
  