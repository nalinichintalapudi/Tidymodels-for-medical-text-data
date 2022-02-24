library(readxl)
X2021data <- read_excel("C:/Users/Utente/Desktop/2021data.xlsx")
View(X2021data)


library(dplyr)
library(tidytext)


data_words <- X2021data %>%
  unnest_tokens(word,`TEXT TX`) %>%
  count(`WHO CODE`, word, sort = TRUE)

total_words <- data_words %>% 
  group_by(`WHO CODE`) %>% 
  summarize(total = sum(n))

data1_words <- left_join(data_words, total_words)

data1_words


library(ggplot2)

ggplot(data1_words, aes(n/total, fill = `WHO CODE`)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~`WHO CODE`, ncol = 2, scales = "free_y")



freq_by_rank <- data1_words %>% 
  group_by(`WHO CODE`) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n/total) %>%
  ungroup()

freq_by_rank



freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = `WHO CODE`)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()



rank_subset <- freq_by_rank %>% 
  filter(rank < 500,
         rank > 10)

lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)


freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = `WHO CODE`)) + 
  geom_abline(intercept = -0.62, slope = -1.1, 
              color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()


data1_tf_idf <- data1_words %>%
  bind_tf_idf(word, `WHO CODE`, n)

data1_tf_idf



data1_tf_idf %>%
  select(-total) %>%
  arrange(desc(tf_idf))


library(forcats)

data1_tf_idf %>%
  group_by(`WHO CODE`) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = `WHO CODE`)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~`WHO CODE`, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)









