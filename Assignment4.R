require(pdftools)
require(tidytext)
require(tidyverse)
require(quanteda)
require(dplyr)
require(tidytext)
require(stopwords)
require(tm)

#Load data
load("ipcc.RData")

# Download report
urlIPCC6 <- "https://www.ipcc.ch/report/ar6/wg1/downloads/report/IPCC_AR6_WGI_Full_Report.pdf"
ipcc6 <- pdf_text(urlIPCC6)

urlIPCC5 <- "https://www.ipcc.ch/site/assets/uploads/2018/02/WG1AR5_all_final.pdf"
ipcc5 <- pdf_text(urlIPCC5)

# Construct Wordlists based on resources 
id <- "1moS1tkh_AJafpcIFpktaSvZwrzv5d4ix"
sURL <- sprintf("https://docs.google.com/uc?id=%s&export=download", id)
con <- curl::curl(sURL)
LM_freq_list <- read_csv(con)

termsModal <- c(LM_freq_list$Word[LM_freq_list$Strong_Modal != 0],
                LM_freq_list$Word[LM_freq_list$Weak_Modal != 0])
termsUncertainty <- LM_freq_list$Word[LM_freq_list$Uncertainty != 0]

# Limit the reports to first 100 pages
ipcc5 %>% 
  gsub("\n", "", .) %>%                            # Remove "\n" 
  as_tibble() %>% 
  unnest_tokens(input  = "value",      
              output = "tokens",   
              token  = "words") %>% 
  filter(! tokens %in% stopwords()) %>%            # Remove stopwords
  mutate(tokens_length = str_length(tokens)) %>% 
  filter(tokens_length > 2) %>%                    # Exclude terms less than 2 words
  count(tokens, sort = T) -> i5_tokens

ipcc6 %>% 
  gsub("\n", "", .) %>%                            # Remove "\n" 
  as_tibble() %>% 
  unnest_tokens(input  = "value",      
                output = "tokens",   
                token  = "words") %>% 
  filter(! tokens %in% stopwords()) %>%            # Remove stopwords
  mutate(tokens_length = str_length(tokens)) %>% 
  filter(tokens_length > 2) %>%                    # Exclude terms less than 2 words
  count(tokens, sort = T) -> i6_tokens

# Lowercase modal terms and uncertainty terms
termsModal <- tolower(termsModal)
termsUncertainty <- tolower(termsUncertainty)

# Filter out the relative word count of modal and uncertainty related words
i5_modal <- 
  i5_tokens %>% 
  filter(tokens %in% termsModal)

i5_uncerntainty <- 
  i5_tokens %>% 
  filter(tokens %in% termsUncertainty) 

i6_modal <- 
  i6_tokens %>% 
  filter(tokens %in% termsModal)

i6_uncerntainty <- 
  i6_tokens %>% 
  filter(tokens %in% termsUncertainty)

# Make omparisons
modal_diff_pct <- (sum(i5_modal$n) - sum(i6_modal$n)) / sum(i6_modal$n)
uncertain_diff_pct <- (sum(i6_uncerntainty$n) - sum(i5_uncerntainty$n)) / sum(i5_uncerntainty$n)

modal_comparison <- 
  full_join(i5_modal, i6_modal, by="tokens") %>% 
  rename("tokens_modal"= tokens, "i5"=n.x, "i6"=n.y) %>% 
  mutate(diff_modal = i6 - i5) 

uncertain_comparison <- 
  full_join(i5_uncerntainty, i6_uncerntainty, by="tokens") %>% 
  rename("tokens_uncertain" = tokens, "i5"=n.x, "i6"=n.y) %>% 
  mutate(diff_uncertain = i6 - i5)