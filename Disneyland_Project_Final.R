############################
##### Created by Alexander Beveridge
##### MBAN1 HULT 2021 - Text Analytics Final Project
##### Date: 12.1.2021
##### Version 1.0
###########################

### ANALYSIS ON RESORTS AS A WHOLE ###
# -- Installing and loading required packages: 
library(infer)
library(dplyr)
library(ggplot2)
library(viridis)
library(wordcloud2)
library(dunn.test)
library(qdap)
library(tidytext)
library(tidyverse)
library(textdata)

# -- Loading CSV file with data on Disneyland Reviews 
Disneyland_df <- read.csv(file = '/Users/alexbeveridge/Desktop/HULT Essays/MBAN/R/DisneylandReviews.csv')

# -- Renaming the unstructured text variable as "text", as it is good real-world practice 
colnames(Disneyland_df)[5] <- "text"

# -- Changing variables to numeric which are integer to allow for frequency analysis
as.numeric(Disneyland_df$Review_ID)
as.numeric(Disneyland_df$Rating)

# -- Changing "Branch" to categorical
as.factor(Disneyland_df$Branch)

# -- Creating clean variable of "Branch" // creating "Resort"
Disneyland_df$Resort <- gsub("Disneyland_HongKong","Hong Kong",Disneyland_df$Branch)
Disneyland_df$Resort <- gsub("Disneyland_California","California",Disneyland_df$Resort)
Disneyland_df$Resort <- gsub("Disneyland_Paris","Paris",Disneyland_df$Resort)


# # -- Structuring the unstructured text -- # #

# -- Creating individual tokens with "unnest_tokens" function
tidy_disney <- Disneyland_df %>% unnest_tokens(word, text)

# -- Creating custom stop words
stop_words_custom <- bind_rows(tibble(word = c("disneyland","park","hongkong", "california", "resort", "hk", 
                                               "disney"),
                                      lexicon = c("custom", "custom","custom", "custom")),
                               stop_words)

# -- Removing any stop words
data(stop_words)
data(stop_words_custom)
disney_no_stop <- tidy_disney %>% anti_join(stop_words) %>% anti_join(stop_words_custom)


# # -- Creating frequency analysis on each DisneyLand Resort -- # #


# -- Creating a tidy format for Paris Resort
Paris <- Disneyland_df %>%
  filter(Resort == "Paris")

tidy_Paris <- Paris %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(stop_words_custom) 

# -- Creating a tidy format for Hong Kong Resort
HongKong <- Disneyland_df %>%
  filter(Resort == "Hong Kong")

tidy_HongKong <- HongKong %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(stop_words_custom)

# -- Creating a tidy format for California Resort
California <- Disneyland_df %>%
  filter(Resort == "California")

tidy_California <- California %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(stop_words_custom) 

# -- Combining and calculating frequencies
library(tidyr)
frequency <- bind_rows(mutate(tidy_HongKong, author="Hong Kong"),
                       mutate(tidy_California, author= "California"),
                       mutate(tidy_Paris, author="Paris")
)%>%
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(author, proportion) %>%
  gather(author, proportion, `California`, `Paris`)

# -- Plotting the correlograms with Hong Kong as the benchmark
library(scales)
ggplot(frequency, aes(x=proportion, y=`Hong Kong`, 
                      color = abs(`Hong Kong`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~author, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "Hong Kong", x=NULL) # The diagonal identifies correlation // things on the diagonal are similar, further away are more country specific

# -- Conducting a correlation test 
library(textreadr)
cor.test(data=frequency[frequency$author == "California",],
         ~proportion + `Hong Kong`)

cor.test(data=frequency[frequency$author == "Paris",],
         ~proportion + `Hong Kong`)

# -- Creating count frequencies for each resort
count_tidy_HongKong <- tidy_HongKong %>%
  count(word, sort=TRUE)

count_tidy_HongKong

count_tidy_California <- tidy_California %>%
  count(word, sort=TRUE)

count_tidy_Paris <- tidy_California %>%
  count(word, sort=TRUE)

# -- Plotting resort token frequencies

freq_hist_HongKong <- tidy_HongKong %>% 
  count(word, sort=TRUE) %>% 
  filter(n>1000) %>%
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n)) + geom_col() + xlab(NULL) + coord_flip()

freq_hist_HongKong

freq_hist_California <- tidy_California %>% 
  count(word, sort=TRUE) %>% 
  filter(n>2500) %>% # Higher number of tokens, therefore increased this value
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n)) + geom_col() + xlab(NULL) + coord_flip()

freq_hist_California

freq_hist_Paris <- tidy_Paris %>% 
  count(word, sort=TRUE) %>% 
  filter(n>2500) %>% # Higher number of tokens, therefore increased this value
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n)) + geom_col() + xlab(NULL) + coord_flip()

freq_hist_Paris

# # -- Sentiment analysis on tokens for each resort -- # #

# -- Installing nrc another way because it did not work on tidytext
textdata::lexicon_nrc(delete = TRUE)
textdata::lexicon_nrc()

# -- Installing nrc 
nrc <- get_sentiments("nrc")

# -- Installing afinn 
afinn <- get_sentiments("afinn")

# -- Seeing what emotions are the most frequent // Hong Kong
tidy_HongKong %>% 
  count(word) %>% 
  mutate(word2 = fct_reorder(word, n)) %>% 
  inner_join(nrc) %>% 
  count(sentiment, sort = TRUE)

# -- NRC lexicon // Hong Kong
sentiment_HK <- tidy_HongKong %>% 
  count(word) %>% 
  mutate(word2 = fct_reorder(word, n)) %>% 
  inner_join(nrc)

sentiment_HK_types <- tidy_HongKong %>% 
  inner_join(nrc) %>% 
  count(word, sentiment) %>% 
  arrange(desc(n)) %>% 
  filter(sentiment %in% c("positive", "negative", "trust", "fear", "anticipation", "sadness"))

HK_word_counts <- sentiment_HK_types %>% 
  group_by(sentiment) %>% 
  top_n(10, n) %>% 
  ungroup() %>% 
  mutate(word2 = fct_reorder(word, n))

ggplot(HK_word_counts, aes(x = word2, y = n, fill = sentiment))+
  geom_col()+
  facet_wrap(~sentiment, scales = "free")+
  coord_flip()+
  xlab(NULL)+
  ylab(NULL)

# -- Seeing what emotions are the most frequent // California
tidy_California %>% 
  count(word) %>% 
  mutate(word2 = fct_reorder(word, n)) %>% 
  inner_join(nrc) %>% 
  count(sentiment, sort = TRUE)

#-- NRC lexicon // California
sentiment_California <- tidy_California %>%
  count(word) %>% 
  mutate(word2 = fct_reorder(word, n)) %>% 
  inner_join(nrc)

sentiment_California_types <- tidy_California %>% 
  inner_join(nrc) %>% 
  count(word, sentiment) %>% 
  arrange(desc(n)) %>% 
  filter(sentiment %in% c("negative", "positive", "trust", "fear", "anger", "sadness"))

California_word_counts <- sentiment_California_types %>% 
  group_by(sentiment) %>% 
  top_n(10, n) %>% 
  ungroup() %>% 
  mutate(word2 = fct_reorder(word, n))

ggplot(California_word_counts, aes(x = word2, y = n, fill = sentiment))+
  geom_col()+
  facet_wrap(~sentiment, scales = "free")+
  coord_flip()+
  xlab(NULL)+
  ylab(NULL)

# -- Seeing what emotions are the most frequent // Paris
tidy_Paris %>% 
  count(word) %>% 
  mutate(word2 = fct_reorder(word, n)) %>% 
  inner_join(nrc) %>% 
  count(sentiment, sort = TRUE)

#-- NRC lexicon // Paris
sentiment_Paris <- tidy_Paris %>%
  count(word) %>% 
  mutate(word2 = fct_reorder(word, n)) %>% 
  inner_join(nrc)

sentiment_Paris_types <- tidy_Paris %>% 
  inner_join(nrc) %>% 
  count(word, sentiment) %>% 
  arrange(desc(n)) %>% 
  filter(sentiment %in% c("negative", "positive", "fear", "trust", "anger", "sadness"))

Paris_word_counts <- sentiment_Paris_types %>% 
  group_by(sentiment) %>% 
  top_n(10, n) %>% 
  ungroup() %>% 
  mutate(word2 = fct_reorder(word, n))

ggplot(Paris_word_counts, aes(x = word2, y = n, fill = sentiment))+
  geom_col()+
  facet_wrap(~sentiment, scales = "free")+
  coord_flip()+
  xlab(NULL)+
  ylab(NULL)

# # -- afinn analysis -- # #

## PARIS AFINN

Paris_afinn <- tidy_Paris %>%
  inner_join(afinn, by = "word") %>%
  group_by(Review_ID) %>%
  summarize(rating = mean(Rating), sentiment = sum(value))

## - Filtering for tokens with only negative sentiment 
Paris_negsent <- tidy_Paris %>%
  inner_join(Paris_afinn, by = c("Review_ID")) %>%
  filter(sentiment < 0)

Paris_negsent %>%
  count(word)  %>%
  top_n(10, n) %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(n,word)) +
  geom_col()+ labs(y = NULL, x = "word counts")+
  ggtitle("Paris: Top 10 frequent words in reviews with overall neg-sentiment scores")

## California AFINN

Cali_afinn <- tidy_California %>%
  inner_join(afinn, by = "word") %>%
  group_by(Review_ID) %>%
  summarize(rating = mean(Rating), sentiment = sum(value))

## - Filtering for tokens with only negative sentiment 
Cali_negsent <- tidy_California %>%
  inner_join(Cali_afinn, by = c("Review_ID")) %>%
  filter(sentiment < 0)

Cali_negsent %>%
  count(word)  %>%
  top_n(10, n) %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(n,word)) +
  geom_col()+ labs(y = NULL, x = "word counts")+
  ggtitle("Cali: Top 10 frequent words in reviews with overall neg-sentiment scores")

## Hong Kong AFINN

HK_afinn <- tidy_HongKong %>%
  inner_join(afinn, by = "word") %>%
  group_by(Review_ID) %>%
  summarize(rating = mean(Rating), sentiment = sum(value))

## - Filtering for tokens with only negative sentiment 
HK_negsent <- tidy_HongKong %>%
  inner_join(HK_afinn, by = c("Review_ID")) %>%
  filter(sentiment < 0)

HK_negsent %>%
  count(word)  %>%
  top_n(4, n) %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(n,word)) +
  geom_col()+ labs(y = NULL, x = "word counts")+
  ggtitle("HK: Top 4 frequent words in reviews with overall neg-sentiment scores")

library(dplyr)
library(tidytext)
library(tidyr)
library(tidytuesdayR)

# # -- HONG KONG BIGRAMS -- # #

# -- Creating tokens that allow us to create the bigrams; we do this in pairs 
HK_bigrams <- HongKong %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)

# -- We want to see the bigrams 
HK_bigrams 

# -- Remove stop words
HK_bigrams %>%
  count(bigram, sort = TRUE) 

# -- To remove stop words from the bigram data, we need to use the separate and filter functions
library(tidyr)
HK_bigrams_separated <- HK_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

HK_bigrams_filtered <- HK_bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# -- creating the new bigram 
HK_bigram_counts <- HK_bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

# -- We want to see the new bigrams 
HK_bigram_counts

# # -- CALIFORNIA BIGRAMS -- # #

# -- Repeat process from Hong Kong Bigrams

data(stop_words)

Cali_bigrams <- California %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2) 

Cali_bigrams %>%
  count(bigram, sort = TRUE) 

library(tidyr)
Cali_bigrams_separated <- Cali_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# -- Uniting the words to create the bigram without stopwords
Cali_bigrams_filtered <- Cali_bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  unite(word, word1, word2, sep = " ")

Cali_bigram_counts <- Cali_bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

# - Creation of bigram chart by rating
Cali_Bigram_Final <- Cali_bigrams_filtered %>%
  group_by(Branch, Rating) %>%
  dplyr::count(word, sort = TRUE)%>%
  filter(row_number() <= 10)

# - Creation of bigram chart
Cali_Bigram_Final %>% 
  group_by(Rating) %>% 
  ggplot(mapping = aes(reorder_within(word, n, Rating), y=n))+ 
  geom_col(aes(fill=Rating)) + 
  coord_flip()+ scale_x_reordered()+
  facet_wrap(~Rating, scales = "free")+ 
  theme_minimal()+ 
  xlab("Bigrams")+
  ylab("Count") + 
  ggtitle("Disneyland California Bigram By Rating")+
  theme(legend.position = "null", plot.title = element_text(hjust = .5))


# # -- PARIS BIGRAMS -- # #

Paris_bigrams <- Paris %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)

Paris_bigrams 

Paris_bigrams %>%
  count(bigram, sort = TRUE) 

library(tidyr)
Paris_bigrams_separated <- Paris_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

Paris_bigrams_filtered <- Paris_bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

Paris_bigram_counts <- Paris_bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

Paris_bigram_counts


# # -- CREATE QUAD FOR PARIS AND CALI DUE TO FAST PASS -- # #

Cali_quadrogram <- California %>%
  unnest_tokens(quadrogram, text, token = "ngrams", n=4) %>%
  separate(quadrogram, c("word1", "word2", "word3", "word4"), sep=" ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  filter(!word4 %in% stop_words$word) %>%
  count(word1, word2, word3, word4, sort = TRUE)

Cali_quadrogram

Paris_quadrogram <- Paris %>%
  unnest_tokens(quadrogram, text, token = "ngrams", n=4) %>%
  separate(quadrogram, c("word1", "word2", "word3", "word4"), sep=" ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  filter(!word4 %in% stop_words$word) %>%
  count(word1, word2, word3, word4, sort = TRUE)

Paris_quadrogram

# # -- CREATING TOKEN CORRELATION NETWORK -- # #

## -- California 

# -- Installing igraph
library(igraph)
Cali_bigram_graph <- Cali_bigram_counts %>%
  filter(n>100) %>%
  graph_from_data_frame()

# -- Direction of semantic (->) structure is important 
Cali_bigram_graph 

# -- Installing ggraph
library(ggraph)
ggraph(Cali_bigram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1) # this is a semantic structure 

## -- Paris 

library(igraph)
Paris_bigram_graph <- Paris_bigram_counts %>%
  filter(n>100) %>%
  graph_from_data_frame()

Paris_bigram_graph 

library(ggraph)
ggraph(Paris_bigram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)


