library(tidyverse)
library(tidymodels)

## Data
library(readxl)
dat_new<- read_excel(file.choose())
data_new2<-na.omit(dat_new)


## Data Cleaning

##Text mining typically requires a lot of data cleaning.  

## Identify line numbers

data_3 <- data_new2 %>% 
  mutate(document = row_number())  
 

data_3

## Tokenization
library(tidytext)

tidy_data <- data_3 %>%
  unnest_tokens(word, text) %>%
  group_by(word) %>%
  filter(n() > 10) %>%
  ungroup()

tidy_data


tidy_data %>%
  count(who_code, word, sort = TRUE) %>%
  anti_join(get_stopwords()) %>%
  group_by(who_code) %>%
  top_n(20) %>%
  ungroup() %>%
  ggplot(aes(reorder_within(word, n, who_code), n,
             fill = who_code
  )) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  scale_x_reordered() +
  coord_flip() +
  facet_wrap(~who_code, scales = "free") +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    x = NULL, y = "Word count",
    title = "Most frequent words after removing stop words",
    subtitle = "Words like 'said' occupy similar ranks but other words are quite different"
  )

library(rsample)

data_split <- data_3 %>%
  select(document) %>%
  initial_split()
train_data <- training(data_split)
test_data <- testing(data_split)


sparse_words <- tidy_data %>%
  count(document, word) %>%
  inner_join(train_data) %>%
  cast_sparse(document, word, n)

class(sparse_words)

dim(sparse_words)

word_rownames <- as.integer(rownames(sparse_words))

data_joined <- data.frame(document = word_rownames) %>%
  left_join(data_3 %>%
              select(document, who_code))

library(glmnet)
library(doMC)
registerDoMC(cores = 8)

data_189 <- data_joined$who_code=="Diseases of the circulatory system"
model <- cv.glmnet(sparse_words,data_189,
                   family = "binomial",
                   parallel = TRUE, keep = TRUE
)
plot(model)

plot(model$glmnet.fit)

library(broom)

coefs <- model$glmnet.fit %>%
  tidy() %>%
  filter(lambda == model$lambda.1se)

coefs %>%
  group_by(estimate > 0) %>%
  top_n(10, abs(estimate)) %>%
  ungroup() %>%
  ggplot(aes(fct_reorder(term, estimate), estimate, fill = estimate > 0)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  coord_flip() +
  labs(
    x = NULL,
    title = "Coefficients that increase/decrease probability the most",
    subtitle = "A document mentioning Martians is unlikely to be written by Jane Austen"
  )

intercept <- coefs %>%
  filter(term == "(Intercept)") %>%
  pull(estimate)

classifications <- tidy_data %>%
  inner_join(test_data) %>%
  inner_join(coefs, by = c("word" = "term")) %>%
  group_by(document) %>%
  summarize(score = sum(estimate)) %>%
  mutate(probability = plogis(intercept + score))

classifications

library(yardstick)

comment_classes <- classifications %>%
  left_join(data_3 %>%
              select(who_code, document), by = "document") %>%
  mutate(who_code = as.factor(who_code))

comment_classes %>%
  roc_curve(who_code, probability) %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(
    color = "midnightblue",
    size = 1.5
  ) +
  geom_abline(
    lty = 2, alpha = 0.5,
    color = "gray50",
    size = 1.2
  ) +
  labs(
    title = "ROC curve for text classification using regularized regression",
    subtitle = "Predicting whether text was written by Nalini chindelepudi"
  )



comment_classes %>%
  roc_auc(who_code, probability)

comment_classes %>%
  mutate(
    prediction = case_when(
      probability > 0.5 ~ "Diseases of the circulatory system",
      TRUE ~ "Diseases of the digestive system"
    ),
    prediction = as.factor(prediction)
  ) %>%
  conf_mat(who_code, prediction)



comment_classes %>%
  filter(
    probability > .8,
    title == "Diseases of the digestive system"
  ) %>%
  sample_n(10) %>%
  inner_join(tidy_data %>%
               select(document, text)) %>%
  select(probability, text)
