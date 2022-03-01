#############################################################################
############################### TEAM 9
############################### Airbnb 
############################### COMBINED ASSIGNMENT - Text analytics


#### Loading all libraries ####

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
library(mongolite)

#### Data Massaging ####

## Creating reviews dataframe

reviews <- do.call(rbind.data.frame, airbnb_all$reviews)

colnames(reviews)[6] = "text"

## -- Filtering Language for summary text data with textcat (SUMMARY ANALYSIS)
airbnb_all <- airbnb_all %>%
  unite('summary_text', summary, description, remove = TRUE)

airbnb_all$summary_language <- textcat(airbnb_all$summary_text)

airbnb_all_en <- airbnb_all %>%
  filter(summary_language == "english") 

## -- Renaming country variable

airbnb_all_en$country <- airbnb_all_en$address$country

## -- Putting review dataset in one dataset with original Airbnb dataset

airbnb_all_en <- cSplit(airbnb_all_en, "listing_url", "/") %>%
  select(-listing_url_1, -listing_url_2, -listing_url_3, -listing_url_4) 

colnames(airbnb_all_en)[77] = "listing_id"

reviews$listing_id <- as.numeric(reviews$listing_id)

airbnb_all_en_r <- airbnb_all_en %>%
  inner_join(reviews, by = "listing_id")

## -- Sub-setting data we are not interested in (to reduce memory)

airbnb_all_final <- subset(airbnb_all_en_r, select = -c(1, 3, 4:9, 12:24, 29:34, 37:44, 46, 48:50, 52, 53, 56, 57, 59:63, 71:73, 78:80) )

## -- Filtering Language for summary text data with textcat (REVIEW ANALYSIS)

colnames(airbnb_all_final)[28] = "review_text"


# Removing amenities and location column

airbnb_specific <- airbnb_all_final %>% 
  select(-amenities, -address.location.coordinates)
airbnb_specific <- as.data.frame(airbnb_specific)

# Removing other languages
airbnb_specific$review_language <- textcat(airbnb_specific$review_text)

airbnb_en <- airbnb_specific %>%
  filter(review_language == "english")

### creating a tidy format for Description
airbnb_desp_tokens <- airbnb_en %>%
  unnest_tokens(word, summary_text) %>% 
  anti_join(stop_words) 

airbnb_review_tokens <- airbnb_en %>%
  unnest_tokens(word, review_text) 

################################################################################
#### Correlation and related plots####


# Correlograms

frequency <- bind_rows(mutate(airbnb_desp_tokens, author="desp"), 
                       mutate(airbnb_review_tokens, author= "review")
)%>%#closing bind_rows
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(author, proportion) %>%
  gather(author, proportion, `review`)

ggplot(frequency, aes(x=proportion, y=`desp`, 
                      color = abs(`desp`- proportion)))+ 
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~author, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "desp", x=NULL)

# Pairwise correlation

word_cors_prop <- airbnb_review_tokens %>%  
  group_by(word) %>%
  mutate(word=str_extract(word, "[a-z']+")) %>%
  filter(n() >= 100) %>%
  pairwise_cor(word, property_type, sort=TRUE)

# Specific Words with negative connotation

# No correlation
no_word_corr <- word_cors_prop %>%
  filter(item1 %in% c("no")) %>%
  filter(!item2 %in% stop_words$word) %>%  
  group_by(item1) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) 

no_word_corr %>% 
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity", fill = "#0EA8B9")+
  facet_wrap(~item1, scales = "free")+
  coord_flip()

# Negative words used
negative_corr <- word_cors_prop %>%
  filter(item1 %in% c("annoying",  "disappointing", "bad")) %>%
  group_by(item1) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) 


negative_corr %>% 
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity", fill = "#0EA8B9")+
  facet_wrap(~item1, scales = "free")+
  coord_flip()


issue_corr <- word_cors_prop %>%
  filter(item1 %in% c( "dirty","noise", "noisy", "broken")) %>%
  group_by(item1) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) 

issue_corr %>% 
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity", fill = "#0EA8B9")+
  facet_wrap(~item1, scales = "free")+
  coord_flip()

# Rent and related
rent_corr <- word_cors_prop %>%
  filter(item2 %in% c("rent", "neighborhood")) %>%
  filter(!item1 %in% stop_words$word) %>%  
  group_by(item2) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(item1 = reorder(item1, correlation)) 


rent_corr %>% 
  ggplot(aes(item1, correlation)) +
  geom_bar(stat = "identity", fill = "#0EA8B9")+
  facet_wrap(~item2, scales = "free")+
  coord_flip()


################################################################################
#### Bigram analysis ####

# Bigram with sentiment analysis

afinn <- get_sentiments("afinn")

airbnb_bigram <- airbnb_en %>% 
  unnest_tokens(bigram, review_text, token = "ngrams", n=2) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(word1 == "not") %>%
  inner_join(afinn, by=c(word2 = "word")) %>% 
  count(word1, word2, value, sort = TRUE) %>% 
  ungroup() %>% 
  mutate(contribution = n * value,
         sign = if_else(value > 0, "postive", "negative")) %>%
  group_by(word1) %>% 
  top_n(20, abs(contribution)) %>%
  ungroup()


airbnb_bigram %>%
  ggplot(aes(y = reorder_within(word2, contribution, word1), 
             x = contribution, 
             fill = sign)) +
  geom_col() + 
  scale_y_reordered() + 
  facet_wrap(~ word1, scales = "free") + 
  labs(y = 'Words proceeded by not',
       x = "Sentiment value * number of occurrences")


################################################################################
#### Country Specific Sentiment Analysis ####
## United States
US_air <- airbnb_en%>%
  filter(country== "United States")


data(stop_words)

tidy_USair <- US_air%>%
  unnest_tokens(word,review_text)%>%
  anti_join(stop_words) 

word_countUS<- tidy_USair%>%
  count(word, sort = TRUE) %>% 
  top_n(15)%>%
  arrange(desc(n))

ggplot(word_countUS,aes(x=word,y=n))+
  geom_col()+coord_flip()

##counting sentiments
bing <- get_sentiments("bing")

sentiment_review1_us_positive<- tidy_USair%>%
  inner_join(bing) %>% 
  count(word,sentiment) %>%
  filter(sentiment == "positive") %>% 
  top_n(15, n)

sentiment_review1_us_negative<- tidy_USair%>%
  inner_join(bing) %>% 
  filter(sentiment == "negative") %>%
  count(word,sentiment, sort = TRUE) %>%
  top_n(15, n)


ggplot(sentiment_review1_us_positive,aes(x=word,y=n))+
  geom_col(show.legend = FALSE)+
  coord_flip()+
  labs(title = "US positve word counts",x="words")

ggplot(sentiment_review1_us_negative,aes(x=word,y=n))+
  geom_col(show.legend = FALSE)+
  coord_flip()+
  labs(title = "US negative word counts",x="words")

## Australia
Aus_air <- airbnb_en%>%
  filter(country== "Australia")

data(stop_words)

tidy_Ausair <- Aus_air%>%
  unnest_tokens(word,review_text)%>%
  anti_join(stop_words) 

word_countAus<- tidy_Ausair%>%
  count(word, sort = TRUE)%>%
  top_n(15)%>%
  arrange(desc(n))

ggplot(word_countAus,aes(x=word,y=n))+
  geom_col()+coord_flip()

##counting sentiments
bing <- get_sentiments("bing")

sentiment_review_aus_positive<- tidy_Ausair%>%
  inner_join(bing) %>% 
  count(word,sentiment) %>%
  filter(sentiment == "positive") %>% 
  top_n(15, n)

sentiment_review_aus_negative<- tidy_USair%>%
  inner_join(bing) %>% 
  filter(sentiment == "negative") %>%
  count(word,sentiment, sort = TRUE) %>%
  top_n(15, n)

ggplot(sentiment_review_aus_positive,aes(x=word,y=n))+
  geom_col(show.legend = FALSE)+
  coord_flip()+
  labs(title = "AUSTRALIA positive word counts",x="words")

ggplot(sentiment_review_aus_negative,aes(x=word,y=n))+
  geom_col(show.legend = FALSE)+
  coord_flip()+
  labs(title = "AUSTRALIA negative word counts",x="words")


## Portugal
Portugal_air <- airbnb_en%>%
  filter(country== "Portugal")

data(stop_words)

tidy_Portugalair <- Portugal_air%>%
  unnest_tokens(word,review_text)%>%
  anti_join(stop_words)

word_countPor<- tidy_Portugalair%>%
  count(word, sort = TRUE)%>%
  top_n(15)%>%
  arrange(desc(n))

ggplot(word_countPor,aes(x=word,y=n))+
  geom_col()+coord_flip()

##counting sentiments
bing <- get_sentiments("bing")

sentiment_review_Por_positive<- tidy_Portugalair%>%
  inner_join(bing) %>% 
  count(word,sentiment) %>%
  filter(sentiment == "positive") %>% 
  top_n(15, n)

sentiment_review_Por_negative<- tidy_Portugalair%>%
  inner_join(bing) %>% 
  filter(sentiment == "negative") %>%
  count(word,sentiment, sort = TRUE) %>%
  top_n(15, n)

ggplot(sentiment_review_Por_positive,aes(x=word,y=n))+
  geom_col(show.legend = FALSE)+
  coord_flip()+
  labs(title = "Portugal positive word counts",x="words")

ggplot(sentiment_review_Por_negative,aes(x=word,y=n))+
  geom_col(show.legend = FALSE)+
  coord_flip()+
  labs(title = "Portugal negative word counts",x="words")

## Spain
Spain_air <- airbnb_en%>%
  filter(country== "Spain")

data(stop_words)

tidy_Spainair <- Spain_air%>%
  unnest_tokens(word,review_text)%>%
  anti_join(stop_words) 

word_countSpain<- tidy_Spainair%>%
  count(word, sort = TRUE)%>%
  top_n(15)%>%
  arrange(desc(n))

ggplot(word_countSpain,aes(x=word,y=n))+
  geom_col()+coord_flip()

##counting sentiments
bing <- get_sentiments("bing")

sentiment_review_Spain_positive<- tidy_Spainair%>%
  inner_join(bing) %>% 
  count(word,sentiment) %>%
  filter(sentiment == "positive") %>% 
  top_n(15, n)

sentiment_review_Spain_negative<- tidy_Spainair%>%
  inner_join(bing) %>% 
  filter(sentiment == "negative") %>%
  count(word,sentiment, sort = TRUE) %>%
  top_n(15, n)

ggplot(sentiment_review_Spain_positive,aes(x=word,y=n))+
  geom_col(show.legend = FALSE)+
  coord_flip()+
  labs(title = "Spain positive word counts",x="words")

ggplot(sentiment_review_Spain_negative,aes(x=word,y=n))+
  geom_col(show.legend = FALSE)+
  coord_flip()+
  labs(title = "Spain negative word counts",x="words")


## Canada
Canada_air <- airbnb_en%>%
  filter(country== "Canada")

data(stop_words)

tidy_Canadaair <- Canada_air%>%
  unnest_tokens(word,review_text)%>%
  anti_join(stop_words)

word_countCanada<- tidy_Canadaair%>%
  count(word,sort = TRUE)%>%
  top_n(10)%>%
  arrange(desc(n))

ggplot(word_countCanada,aes(x=word,y=n))+
  geom_col()+coord_flip()

##counting sentiments
bing <- get_sentiments("bing")

sentiment_review_Canada_positive<- tidy_Canadaair%>%
  inner_join(bing) %>% 
  count(word,sentiment) %>%
  filter(sentiment == "positive") %>% 
  top_n(15, n)

sentiment_review_Canada_negative<- tidy_Canadaair%>%
  inner_join(bing) %>% 
  filter(sentiment == "negative") %>%
  count(word,sentiment, sort = TRUE) %>%
  top_n(15, n)

ggplot(sentiment_review_Canada_positive,aes(x=word,y=n))+
  geom_col(show.legend = FALSE)+
  coord_flip()+
  labs(title = "Canada positive word counts",x="words")

ggplot(sentiment_review_Canada_negative,aes(x=word,y=n))+
  geom_col(show.legend = FALSE)+
  coord_flip()+
  labs(title = "Canada negative word counts",x="words")

################################################################################
#### Country Specific TF-IDF Analysis ####

airbnb_all_final$review_language <- textcat(airbnb_all_final$review_text)

airbnb_all_en_tfidf <- airbnb_all_final %>%
  filter(review_language == "english") 

colnames(airbnb_all_en_tfidf)[28] = "review_text"
colnames(airbnb_all_en_tfidf)[1] = "text"

# Creating United States tokens
USA_words <- airbnb_all_en_tfidf %>%
  filter(country == "United States") %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word) 

# Creating Australia tokens
Australia_words <- airbnb_all_en_tfidf %>%
  filter(country == "Australia") %>% 
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word)

# Creating Canada tokens
Canada_words <- airbnb_all_en_tfidf %>%
  filter(country == "Canada") %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word)

# Creating Portugal tokens
Portugal_words <- airbnb_all_en_tfidf %>%
  filter(country == "Portugal") %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word)

# Creating Spain tokens
Spain_words <- airbnb_all_en_tfidf %>%
  filter(country == "Spain") %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word) 

## -- SUMMARY ANALYSIS -- ##

# -- Creating a summary database with relevant variables
summary_an <- subset(airbnb_all_en_tfidf, select = -c(2:24, 26:29))

# -- Tokenizing data
summary_words <- summary_an %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(country, word, sort = TRUE)

# -- Grouping and summarizing
total_summmary <- summary_words %>% 
  group_by(country) %>% 
  summarize(total = sum(n))

summary_words <- left_join(summary_words, total_summmary)

# -- Creating a Term Frequency Distribution
library(ggplot2)

ggplot(summary_words, aes(n/total, fill = country)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~country, ncol = 2, scales = "free_y")

summary_tf_idf <- summary_words %>%
  bind_tf_idf(word, country, n) %>%
  arrange(desc(tf_idf))

# -- Creating TF-IDF plot
library(forcats)

summary_tf_idf %>%
  group_by(country) %>%
  slice_max(tf_idf, n = 10) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = country)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~country, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)

## -- REVIEWS ANALYSIS -- ##

colnames(airbnb_all_en_tfidf)[1] = "review_text"
colnames(airbnb_all_en_tfidf)[28] = "text"

# -- Creating a reviews database with relevant variables
review_an <- subset(airbnb_all_en_tfidf, select = -c(1:24, 26, 27, 29))

# -- Tokenizing data
review_words <- review_an %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(country, word, sort = TRUE)

# -- Grouping and summarizing
total_review <- review_words %>% 
  group_by(country) %>% 
  summarize(total = sum(n))

review_words <- left_join(review_words, total_review)

# -- Creating a Term Frequency Distribution
library(ggplot2)

ggplot(review_words, aes(n/total, fill = country)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~country, ncol = 2, scales = "free_y")

review_tf_idf <- review_words %>%
  bind_tf_idf(word, country, n) %>%
  arrange(desc(tf_idf))

# -- Creating TF-IDF plot
library(forcats)

review_tf_idf %>%
  group_by(country) %>%
  slice_max(tf_idf, n = 10) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = country)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~country, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)

