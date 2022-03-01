############################
##### Created by Alexander Beveridge
##### MBAN1 HULT 2021 - Text Analytics Final Exam
##### Date: 12.7.2021
##### Version 1.0
###########################

## -- Loading all necessary libraries
library(tidytext)
library(tidyverse)
library(tidyr)
library(tidytuesdayR)
library(stringr)
library(textreadr)
library(pdftools)
library(textshape)
library(twitteR)
library(tm)
library(ggplot2)
library(scales)
library(magrittr)
library(dplyr)
library(gutenbergr) 
library(Matrix) 
library(textdata) 
library(igraph) 
library(ggraph) 
library(widyr) 
library(topicmodels)
library(quanteda) 
library(quanteda.textmodels) 
library(RColorBrewer) 
library(qdap)
data(stop_words)

## -- Type out text into a vector
my_txt <- c("Well, we need to go back a bit.",
            "In the 1580s and 1590s England is at war with Spain and there were lots of privateers",
            "Privateers are people working as mercenaries who have letters of commission which allow them to attack the merchant shipping of a hostile nation.",
            "In those days they didn't have a very big navy so it was of getting adventurers to fight your war for you.",
            "People who do that aren't pirates, they are privateers, and that means they've got a quasi-legal status and these were the buccaneers.",
            "There's a great story in the book about a famous pirate, Henry Morgan, who was so outraged when a London pamphlet described him as a pirate that he sued them.",
            "He ended up winning the pricely sum of £200.",
            "I find it incredible that he had a lawyer in London who could actually take out an action for libel.",
            "The distinction between privateer and pirate is one that the privateers clung to because pirates were criminals but in everyday life there really wasn't much difference between them.")

## -- Create data frame with text data
library(dplyr)
my_df <- data.frame(line=1:9, text=my_txt)

## -- unnest tokens and remove stopwords; count to recieve the top frequented words
library(tidytext)
library(tidyverse)
my_tokens <- my_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort=TRUE)

## Question 2 ##

## -- Load in the PDFs from local file
library(pdftools) 
setwd("/Users/alexbeveridge/Desktop/HULT Essays/MBAN/R/PDF")
nm <- list.files(path="/Users/alexbeveridge/Desktop/HULT Essays/MBAN/R/PDF")
my_pdf_text <- do.call(rbind, lapply(nm, function(x) pdf_text(x)))

## -- Create opinions to enable reading in the PDFs 
Rpdf <- readPDF(control = list(text = "-layout"))
opinions <- Corpus(URISource(nm), 
                   readerControl = list(reader = Rpdf))

## -- AcorXFA_BasicToggle.pdf
Answer_A <- opinions[[1]]$meta$id

## -- en
Answer_B <- opinions[[7]]$meta$language

## Question 3 ##

library(topicmodels)
library(tidytext)
library(ggplot2)
library(dplyr)
library(tidyr)
data("AssociatedPress")

## -- Creat LDA with AP data
Press_lda <- LDA(AssociatedPress, k=2, control=list(seed=123))

## -- Create beta matrix of the LDA 
Press_topics <- tidy(Press_lda, matrix="beta")
Press_topics

## -- Adding two topics to beta matrix for comparison
beta_spread <- Press_topics %>%
  mutate(topic=paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1>.0000001 | topic2 >0.000001) %>%
  mutate(log_rate = log2(topic2/topic1))

beta_spread

## -- Find the answer by using the find tab when searching the matrix
Answer_q3.5 <- round(0.4251486, digits = 4)

## Question 4 ## 

library(gutenbergr)
library(widyr)
library(tidyr)
library(dplyr)
library(ggraph)
library(igraph)
library(tidytuesdayR)
data(stop_words)

## -- Create 3 groups as according to the question
Group1 <- gutenberg_download(c(100, 101), mirror = "http://mirrors.xmission.com/gutenberg/")
Group2 <- gutenberg_download(c(110, 111), mirror = "http://mirrors.xmission.com/gutenberg/")
Group3 <- gutenberg_download(c(120, 121), mirror = "http://mirrors.xmission.com/gutenberg/")

## -- Tidy each group by unnesting tokens and removing stop words
tidy_group1 <- Group1 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) 

tidy_group2 <- Group2 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_group3 <- Group3 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

## -- Creating correlation of Group 1 and Group 3
library(tidyr)
Answer_A_Frequency <- bind_rows(mutate(tidy_group1, author="Group1"),
                                   mutate(tidy_group2, author= "Group2"),
                                   mutate(tidy_group3, author="Group3")) %>% 
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(author, proportion) %>%
  gather(author, proportion, `Group3`)

Answer_A_Corr <- cor.test(data=Answer_A_Frequency[Answer_A_Frequency$author == "Group3",],
         ~proportion + `Group1`)

Answer_A_Corr

## -- Rounding answer to 2 decimals
round(0.3840855, digits = 2)

## -- Creating correlation of Group 2 and Group 3
library(tidyr)
Answer_B_Frequency <- bind_rows(mutate(tidy_group1, author="Group1"),
                                   mutate(tidy_group2, author= "Group2"),
                                   mutate(tidy_group3, author="Group3")) %>% 
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(author, proportion) %>%
  gather(author, proportion, `Group2`)

Answer_B_Corr <- cor.test(data=Answer_B_Frequency[Answer_B_Frequency$author == "Group2",],
         ~proportion + `Group3`)

Answer_B_Corr

## -- Rounding answer to 2 decimals
round(0.5591232, digits = 2)
