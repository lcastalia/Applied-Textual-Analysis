#install packages
library(readr)
library(dplyr)

# (1) Load the data
consumer_complaints <- read_csv("consumer_complaints.csv")

# (2) Top 10 companies that received most complaints
consumer_complaints %>% 
  count(Company, sort = T) 

# (3) the 10 most frequent issues the customers complain about
consumer_complaints %>% 
  filter(Company == "EQUIFAX, INC.") %>% 
  count(Issue, sort = T)

# (4) the time range of the complaints
consumer_complaints %>% 
  mutate(date = as.Date(`Date received`)) %>% 
  summarise(min_date = min(`Date received`),
            max_date = max(`Date received`))
