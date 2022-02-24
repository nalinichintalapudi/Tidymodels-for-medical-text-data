library(tidyverse)
library(tidytext)
library(wordcloud2)

## Data
library(readxl)
X2021data <- read_excel("C:/Users/Utente/Desktop/2021data.xlsx")
View(X2021data)


## Data Cleaning

Text mining typically requires a lot of data cleaning.  

## Identify line numbers

original_data1 <- X2021data %>%
  group_by(`WHO CODE`) %>%
  mutate(line = row_number()) %>%  
  ungroup()

original_data1

## Tokenization

tidy_data <- original_data1  %>%
  unnest_tokens(word, `TEXT TX`)

tidy_data


# Now that the data is in the one-word-per-row format, we can manipulate it with tidy tools like dplyr.


## Stop Words

`tidytext::get_stopwords()`

data(stop_words)

tidy_data <- tidy_data %>%
  anti_join(stop_words)


### Calculate word frequency

How many Austen countable words are there if we remove _snowball_ stop-words?  There are `r   nrow(dplyr::distinct(matchwords_books, word))` countable words. 


tidy_data  %>% 
  count(word, sort = TRUE) 


positive <- get_sentiments("bing") %>%
  filter(sentiment == "positive")  


tidy_data %>%
  filter(`WHO CODE` == "22 OTHER (Codes for special purposes)") %>%                       
  semi_join(positive) %>%                           
  count(word, sort = TRUE)

library(tidyr)
bing <- get_sentiments("bing")

diseasessentiment <- tidy_data %>% 
  inner_join(bing) %>% 
  count(`WHO CODE`, index = line %/% 80, sentiment) %>%                          
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%    
  mutate(sentiment = positive - negative)                                      



library(ggplot2)

ggplot(diseasessentiment, aes(index, sentiment, fill = `WHO CODE`)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~`WHO CODE`, ncol = 2, scales = "free_x")  


bing_word_counts <- tidy_data %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE)

bing_word_counts


bing_word_counts %>%
  filter(n > 1000) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col() +
  coord_flip() +
  labs(y = "Contribution to sentiment")


library(wordcloud)

tidy_data %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 150))


library(reshape2)

tidy_data %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 150)  



















