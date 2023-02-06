library(dplyr)
library(tidyverse)

# Download files
year <- 2021
for (quarter in 1:4) { 
  web.url <- paste0("https://www.sec.gov/Archives/edgar/full-index/",
                    year,
                    "/QTR", 
                    quarter, 
                    "/master.idx")
  download.file(url = web.url,
                destfile = paste0("EdgarIndexFile/Year",
                                  year,
                                  "Quarter", 
                                  quarter,
                                  ".txt"),
                headers = c("User-Agent" = "Sicen.LIU@student.nhh.no"))
}

# Data Wrangling
# Q1
q1 <- read_delim(file = paste0("EdgarIndexFile/Year",
                                       year,
                                       "Quarter1",
                                       ".txt"),
                         delim = "|",
                         skip = 9)
q1 <- q1[-1,]
q1 %>% 
  filter(`Form Type` == "S-1") -> q1

# Q2
q2 <- read_delim(file = paste0("EdgarIndexFile/Year",
                               year,
                               "Quarter2",
                               ".txt"),
                 delim = "|",
                 skip = 9)
q2 <- q2[-1,]
q2 %>% 
  filter(`Form Type` == "S-1") -> q2

# Q3
q3 <- read_delim(file = paste0("EdgarIndexFile/Year",
                               year,
                               "Quarter3",
                               ".txt"),
                 delim = "|",
                 skip = 9)
q3 <- q3[-1,]
q3 %>% 
  filter(`Form Type` == "S-1") -> q3

# Q4
q4 <- read_delim(file = paste0("EdgarIndexFile/Year",
                               year,
                               "Quarter4",
                               ".txt"),
                 delim = "|",
                 skip = 9)
q4 <- q4[-1,]
q4 %>% 
  filter(`Form Type` == "S-1") -> q4

# Combining data
newdata <-  rbind(q1, q2, q3, q4)
newdata %>% 
  mutate(qrt = quarters(`Date Filed`)) %>% 
  group_by(qrt) %>% 
  summarise(n = n())